# The optional GUI's launcher and continuation logic. The Shiny machinery is a
# Suggests concern; the continuation experiment is live, so its logic is tested
# with a fake echo agent that proves the control and perturbed branches diverge
# without any model call.

fake_fg_agent <- function() {
  env <- new.env()
  env$clone <- function(deep = TRUE) env
  env$generate_utterance <- function(topic, conversation_history_string, ...) {
    list(text = paste0("ECHO> ", conversation_history_string))
  }
  class(env) <- c("R6", "environment")
  env
}

fake_fg <- function() {
  list(topic = "transit", agents = list(P1 = fake_fg_agent()),
       prompt_templates = list(participant_utterance_subtle_persona = "{{conversation_history}}"),
       max_tokens_utterance = 160L, message_mode = "roleflip")
}

sample_fg_log <- function() {
  list(list(message_id = 1L, round = 1L, speaker_id = "Moderator", text = "What about transit?"),
       list(message_id = 2L, round = 1L, speaker_id = "P1", text = "Buses are unreliable."),
       list(message_id = 3L, round = 1L, speaker_id = "P2", text = "Cost is my issue."))
}

test_that("run_focus_studio errors helpfully when GUI packages are missing", {
  need <- c("shiny", "bslib", "DT", "LLMR.shiny")
  if (all(vapply(need, requireNamespace, logical(1), quietly = TRUE))) {
    expect_true(isTRUE(FocusGroup:::.fg_gui_require()))
  } else {
    expect_error(FocusGroup:::.fg_gui_require(), "GUI needs these packages")
  }
})

test_that("perturbing a message changes only that message", {
  log <- sample_fg_log()
  out <- FocusGroup:::.fg_perturb_log(log, 2, "I LOVE THE BUS")
  expect_equal(out[[2]]$text, "I LOVE THE BUS")
  expect_equal(out[[1]]$text, log[[1]]$text)
  expect_equal(out[[3]]$text, log[[3]]$text)
})

test_that("the paired experiment produces diverging control and perturbed messages", {
  progress <- character()
  res <- FocusGroup:::.fg_run_experiment(
    fake_fg(), sample_fg_log(), "P1", perturb_message_id = 2,
    perturb_text = "I LOVE THE BUS NOW",
    .progress = function(detail) progress <<- c(progress, detail))
  expect_false(identical(res$control, res$perturbed))
  expect_true(grepl("unreliable", res$control))
  expect_true(grepl("I LOVE THE BUS NOW", res$perturbed))
  expect_identical(
    progress,
    c("Control response complete", "Perturbed response complete")
  )
})

test_that("the multi-arm experiment retains every arm and run", {
  progress <- character()
  arms <- data.frame(
    label = c("Reliable buses", "Free fares"),
    text = c("The buses are reliable now.", "The buses should be free."),
    stringsAsFactors = FALSE
  )
  runs <- 3L
  res <- FocusGroup:::.fg_run_experiment_many(
    fake_fg(), sample_fg_log(), "P1", perturb_message_id = 2L,
    arms = arms, runs_per_arm = runs,
    .progress = function(detail) progress <<- c(progress, detail)
  )

  expect_equal(nrow(res), (1L + nrow(arms)) * runs)
  expect_identical(
    unique(res$arm_label),
    c("Control", "Reliable buses", "Free fares")
  )
  expect_equal(
    as.integer(table(res$arm_label)),
    rep(runs, 1L + nrow(arms))
  )
  expect_equal(
    split(res$run_id, res$arm_label),
    list(
      Control = 1:3,
      `Free fares` = 1:3,
      `Reliable buses` = 1:3
    )
  )
  expect_true(all(grepl("reliable now", res$response_text[
    res$arm_label == "Reliable buses"
  ])))
  expect_true(all(grepl("should be free", res$response_text[
    res$arm_label == "Free fares"
  ])))
  expect_length(progress, nrow(res))
  expect_identical(progress[[1]], "Control response, run 1, complete")
  expect_identical(
    progress[[length(progress)]],
    "Free fares response, run 3, complete"
  )

  summary <- FocusGroup:::.fg_experiment_summary(res)
  expect_identical(summary$arm, unique(res$arm_label))
  expect_equal(summary$runs, rep(runs, 1L + nrow(arms)))
})

test_that("continuation call counts include control, treatments, and runs", {
  expect_identical(FocusGroup:::.fg_experiment_call_count(1L, 1L), 2L)
  expect_identical(FocusGroup:::.fg_experiment_call_count(2L, 4L), 12L)
  expect_identical(FocusGroup:::.fg_experiment_call_count(3L, 17L), 68L)
})

test_that("a CSV transcript coerces to a conversation log", {
  log <- FocusGroup:::.fg_as_log(FocusGroup:::.fg_demo_transcript())
  expect_true(length(log) >= 6)
  expect_true(all(vapply(log, function(e) !is.null(e$text), logical(1))))
  expect_identical(vapply(log, `[[`, integer(1), "message_id"),
                   seq_along(log))
})

test_that("the transcript CSV export contains scalar transcript columns only", {
  transcript <- data.frame(
    message_id = 1:2,
    speaker_id = c("MOD", "P1"),
    text = c("Welcome.", "Hello."),
    duration_s = c(0.4, 1.2),
    timestamp = as.POSIXct(
      c("2026-01-01 12:00:00", "2026-01-01 12:00:02"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  transcript$metadata <- I(list(list(source = "moderator"), list(source = "model")))

  csv <- FocusGroup:::.fg_transcript_csv(transcript)
  expect_identical(
    names(csv),
    c("message_id", "speaker_id", "text", "duration_s", "timestamp")
  )
  expect_equal(nrow(csv), nrow(transcript))
  expect_identical(csv$text, transcript$text)

  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(csv, path, row.names = FALSE, na = "")
  roundtrip <- utils::read.csv(path, stringsAsFactors = FALSE)
  expect_identical(roundtrip$text, transcript$text)
  expect_false("metadata" %in% names(roundtrip))
})

test_that("persona columns default to all fields and support a subset", {
  personas <- data.frame(
    age = c("42", "67"),
    transit_priority = c("Frequent service", "Lower fares"),
    ideology_score = c(-0.3, 0.4),
    stringsAsFactors = FALSE
  )
  attr(personas, "demographic_fields") <- "age"
  attr(personas, "dictionary") <- data.frame(
    handle = c("age", "transit_priority"),
    question = c("Age in years", "Transit priority"),
    stringsAsFactors = FALSE
  )
  attr(personas, "source") <- "test personas"

  expect_identical(FocusGroup:::.fg_persona_columns(personas), personas)

  subset <- FocusGroup:::.fg_persona_columns(
    personas,
    c("age", "transit_priority")
  )
  expect_identical(names(subset), c("age", "transit_priority"))
  expect_identical(attr(subset, "demographic_fields"), "age")
  expect_identical(
    attr(subset, "dictionary")$handle,
    c("age", "transit_priority")
  )

  full_text <- FocusGroup:::.fg_persona_text(
    personas,
    row = 1L,
    columns = c("age", "transit_priority")
  )
  demographic_only <- FocusGroup:::.fg_persona_text(
    personas,
    row = 1L,
    columns = "age"
  )
  expect_match(full_text, "42", fixed = TRUE)
  expect_match(full_text, "Frequent service", fixed = TRUE)
  expect_false(grepl("Frequent service", demographic_only, fixed = TRUE))
  expect_match(
    FocusGroup:::.fg_persona_text(personas, row = integer()),
    "Select a persona row"
  )
})

test_that("timing statistics report wall time and per-speaker durations", {
  started <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  transcript <- data.frame(
    speaker_id = c("System", "P1", "P1", "P2"),
    duration_s = c(99, 2, 4, NA),
    timestamp = started + c(0, 5, 10, 20),
    stringsAsFactors = FALSE
  )
  timing <- FocusGroup:::.fg_timing_stats(transcript)

  expect_true(timing$has_wall_time)
  expect_equal(timing$wall_seconds, 20)
  expect_true(timing$has_durations)
  expect_identical(timing$speaker_stats$speaker_id, "P1")
  expect_identical(timing$speaker_stats$messages_timed, 2L)
  expect_equal(timing$speaker_stats$total_seconds, 6)
  expect_equal(timing$speaker_stats$mean_seconds, 3)

  unavailable <- FocusGroup:::.fg_timing_stats(
    data.frame(speaker_id = "P1", text = "No timing metadata")
  )
  expect_false(unavailable$has_wall_time)
  expect_false(unavailable$has_durations)
})

test_that("the GUI assembles when its suggested packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("LLMR.shiny")
  expect_s3_class(FocusGroup:::.fg_gui_ui(), "bslib_page")
})

test_that("the navbar and long text outputs do not use fill layout", {
  ui_code <- paste(deparse(body(FocusGroup:::.fg_gui_ui)), collapse = "\n")
  server_code <- paste(
    deparse(body(FocusGroup:::.fg_run_server)),
    deparse(body(FocusGroup:::.fg_analyze_server)),
    deparse(body(FocusGroup:::.fg_experiment_server)),
    collapse = "\n"
  )
  expect_match(ui_code, "fillable = FALSE", fixed = TRUE)
  expect_match(server_code, "text_block_output", fixed = TRUE)
  expect_false(grepl("verbatimTextOutput", server_code, fixed = TRUE))
})

test_that(".fg_display_messages drops System and numbers real messages", {
  log <- c(list(list(message_id = 1L, round = 0L, speaker_id = "System",
                     text = "Participants: P1, P2")),
           sample_fg_log())
  dt <- FocusGroup:::.fg_display_messages(log)
  expect_equal(nrow(dt), 3L)                       # System row dropped
  expect_equal(dt$display_message_id, 1:3)         # clean 1..N numbering
  expect_equal(dt$log_index, 2:4)                  # maps back into the full log
  expect_false(any(dt$speaker_id == "System"))
  expect_equal(dt$text[1], "What about transit?")
  # an empty log yields an empty frame, not an error
  expect_equal(nrow(FocusGroup:::.fg_display_messages(list())), 0L)
})

# A minimal fake `shared` context (the shape shell_context returns: reactives
# plus add_usage/can_run). Lets us drive the run module offline.
fake_shared <- function(mode = "live", can_run = TRUE, active_tab = "run") {
  usage <- new.env(); usage$last <- NULL
  tab <- active_tab
  finished_session <- shiny::reactiveVal(NULL)
  analysis_handoff <- shiny::reactiveVal(NULL)
  list(
    provider = function() "groq",
    model = function() "openai/gpt-oss-20b",
    mode = function() mode,
    temperature = function() 0.4,
    max_tokens = function() 480L,
    reasoning_effort = function() "low",
    active_tab = function() tab,
    key = function() list(found = can_run, var = "GROQ_API_KEY"),
    can_run = function() can_run,
    finished_session = finished_session,
    analysis_handoff = analysis_handoff,
    set_plan = function(calls, label = "Next run") {
      usage$plan <- list(calls = calls, label = label)
    },
    add_usage = function(tokens) usage$last <- tokens,
    .usage = usage)
}

test_that(".fg_live_ok requires live mode AND a key (demo never runs)", {
  expect_true(FocusGroup:::.fg_live_ok(fake_shared("live", TRUE)))
  expect_false(FocusGroup:::.fg_live_ok(fake_shared("live", FALSE)))
  expect_false(FocusGroup:::.fg_live_ok(fake_shared("demo", TRUE)))  # demo: no live call
})

test_that("the run module runs via an injected fake and stores the focus group", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("live", TRUE)
  seen <- new.env()
  fake_run <- function(topic, n_participants, flow, config, seed,
                       message_mode = "roleflip", verbose,
                       max_participant_responses, ...) {
    seen$config <- config
    structure(list(
      focus_group = structure(list(topic = topic, message_mode = message_mode),
                              class = "FocusGroup"),
      transcript = data.frame(message_id = 1:2, round = c(1L, 1L),
                              speaker_id = c("MOD", "P1"),
                              text = c("Welcome.", "Hi."), stringsAsFactors = FALSE),
      summary = "A short session.",
      participants = data.frame(id = c("MOD", "P1")),
      usage = list(sent = 100L, rec = 20L, total = 120L),
      metadata = list(message_mode = message_mode)
    ), class = c("focus_group_result", "list"))
  }
  shiny::testServer(
    FocusGroup:::.fg_run_server, args = list(shared = shared, run_fun = fake_run),
    {
      session$setInputs(topic = "public libraries", n_participants = 3,
                        flow = "round_robin", message_mode = "roleflip",
                        max_resp = 1, seed = 110)
      session$setInputs(run = 1)
      expect_true(inherits(result()$focus_group, "FocusGroup"))
      # real token totals propagate to usage; calls is the estimate
      expect_equal(shared$.usage$last$sent, 100L)
      expect_equal(shared$.usage$last$received, 20L)
      expect_true(shared$.usage$last$calls >= 1L)
      expect_true(shared$.usage$plan$calls >= 1L)
      expect_match(shared$.usage$plan$label, "utterances; retries excluded")
      expect_identical(seen$config$model_params$temperature, 0.4)
      expect_identical(seen$config$model_params$max_tokens, 480L)
      expect_identical(seen$config$model_params$reasoning_effort, "low")
      expect_identical(shiny::isolate(shared$finished_session()), result())

      result_ui <- as.character(output$results)[[1]]
      expect_match(result_ui, "Download full session")
      expect_match(result_ui, "Download transcript")
      expect_match(result_ui, "Analyze this session")
      expect_match(result_ui, "llmr-text-block")

      session$setInputs(analyze_this = 1)
      handoff <- shiny::isolate(shared$analysis_handoff())
      expect_identical(handoff$result, result())
      expect_match(handoff$label, "public libraries")
    })
})

test_that("the run module makes NO call in demo mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("demo", TRUE)
  called <- new.env(); called$n <- 0L
  fake_run <- function(...) { called$n <- called$n + 1L; stop("must not be called") }
  shiny::testServer(
    FocusGroup:::.fg_run_server, args = list(shared = shared, run_fun = fake_run),
    {
      session$setInputs(topic = "x", n_participants = 3, flow = "round_robin",
                        max_resp = 1, seed = 110, run = 1)
      expect_equal(called$n, 0L)        # demo mode blocked the live run
      expect_null(result())
    })
})

# (The persona overview/selection is now the shared LLMR.shiny selector module,
# tested in LLMR.shiny; FocusGroup only wires its result into the run.)

test_that("the run module uses the persona runner when source = anes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("live", TRUE)
  seen <- new.env()
  fake_persona <- function(topic, n_participants, rows, flow, message_mode, seed,
                           max_participant_responses, config, data = NULL,
                           persona_cols = NULL,
                           .runner = NULL) {
    seen$rows <- rows
    seen$topic <- topic
    seen$persona_cols <- persona_cols
    structure(list(
      focus_group = structure(list(topic = topic), class = "FocusGroup"),
      transcript = data.frame(message_id = 1L, round = 1L,
                              speaker_id = "MOD", text = "Hi.",
                              stringsAsFactors = FALSE),
      summary = "A short session.",
      participants = data.frame(id = "MOD"),
      usage = list(sent = 10L, rec = 5L, total = 15L),
      metadata = list(message_mode = message_mode)
    ), class = c("focus_group_result", "list"))
  }
  never <- function(...) stop("synthetic runner must not be called for anes source")
  shiny::testServer(
    FocusGroup:::.fg_run_server,
    args = list(shared = shared, run_fun = never, persona_run_fun = fake_persona),
    {
      chosen_cols <- names(FocusGroup:::.fg_personas())[1:2]
      session$setInputs(topic = "trust", n_participants = 3, flow = "round_robin",
                        message_mode = "roleflip", max_resp = 1, seed = 110,
                        source = "anes", persona_cols = chosen_cols)
      # selection comes from the nested LLMR.shiny selector module; with nothing
      # selected the runner draws a diverse sample (rows = integer(0)).
      session$setInputs(run = 1)
      expect_true(inherits(result()$focus_group, "FocusGroup"))
      expect_equal(seen$topic, "trust")
      expect_true(is.integer(seen$rows) || is.null(seen$rows))
      expect_identical(seen$persona_cols, chosen_cols)
    })
})

test_that("the Analyze module consumes a finished-session handoff", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("demo", TRUE, active_tab = "analyze")
  handed_off <- structure(
    list(
      focus_group = structure(
        list(topic = "handoff topic", conversation_log = sample_fg_log()),
        class = "FocusGroup"
      ),
      transcript = data.frame(
        message_id = 1:3,
        speaker_id = c("Moderator", "P1", "P2"),
        text = c("Question", "Answer one", "Answer two"),
        stringsAsFactors = FALSE
      )
    ),
    class = c("focus_group_result", "list")
  )
  seen <- new.env()
  fake_analysis <- function(focus_group_result, num_topics, include_plots,
                            config = NULL, .runner = NULL) {
    seen$focus_group <- focus_group_result
    seen$num_topics <- num_topics
    seen$include_plots <- include_plots
    list(
      basic_stats = list(
        speaker_stats = data.frame(
          speaker_id = c("P1", "P2"),
          utterance_count = c(1L, 1L),
          total_words = c(2L, 2L),
          avg_words_per_utterance = c(2, 2)
        ),
        full_transcript = "P1: Answer one\nP2: Answer two"
      ),
      topics = list(),
      tfidf = data.frame(),
      readability = data.frame(),
      themes = character(),
      model_summary = character(),
      plots = list(),
      issues = data.frame(
        component = "topics",
        reason = "Optional packages are not installed.",
        stringsAsFactors = FALSE
      )
    )
  }

  shiny::testServer(
    FocusGroup:::.fg_analyze_server,
    args = list(shared = shared, analysis_fun = fake_analysis),
    {
      session$flushReact()
      shared$analysis_handoff(list(
        result = handed_off,
        label = "Finished run: handoff topic"
      ))
      session$flushReact()
      expect_identical(shiny::isolate(raw()), handed_off)
      expect_identical(
        shiny::isolate(source_label()),
        "Finished run: handoff topic"
      )

      session$setInputs(num_topics = 4L, include_plots = FALSE, analyze = 1)
      expect_identical(shiny::isolate(transcript()), handed_off$transcript)
      expect_identical(seen$focus_group, handed_off$focus_group)
      expect_identical(seen$num_topics, 4L)
      expect_false(seen$include_plots)
      expect_identical(
        shiny::isolate(analysis())$basic_stats$full_transcript,
        "P1: Answer one\nP2: Answer two"
      )
      result_ui <- as.character(output$results)[[1]]
      expect_match(result_ui, "Basic statistics")
      expect_match(result_ui, "TF-IDF")
      expect_match(result_ui, "llmr-text-block")

      session$setInputs(load_demo = 1)
      session$setInputs(analyze = 2)
      expect_identical(
        shiny::isolate(source_label()),
        "Bundled demo transcript"
      )
      expect_s3_class(shiny::isolate(focus_group()), "FocusGroup")
    }
  )
})

test_that("model analysis receives the shared generation settings", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("live", TRUE, active_tab = "analyze")
  handed_off <- structure(
    list(
      focus_group = structure(
        list(topic = "model topic", conversation_log = sample_fg_log()),
        class = "FocusGroup"
      ),
      transcript = data.frame(
        speaker_id = c("Moderator", "P1", "P2"),
        text = c("Question", "Answer one", "Answer two"),
        stringsAsFactors = FALSE
      )
    ),
    class = c("focus_group_result", "list")
  )
  configs <- list()
  fake_analysis <- function(focus_group_result, num_topics, include_plots,
                            config = NULL, .runner = NULL) {
    configs[[length(configs) + 1L]] <<- list(config = config)
    list(
      basic_stats = list(
        speaker_stats = data.frame(
          speaker_id = "P1",
          utterance_count = 1L,
          total_words = 2L,
          avg_words_per_utterance = 2
        ),
        full_transcript = "P1: Answer one"
      ),
      topics = list(),
      tfidf = data.frame(),
      readability = data.frame(),
      themes = if (is.null(config)) character() else "Theme",
      model_summary = if (is.null(config)) character() else "Summary",
      plots = list(),
      issues = data.frame(component = character(), reason = character())
    )
  }

  shiny::testServer(
    FocusGroup:::.fg_analyze_server,
    args = list(shared = shared, analysis_fun = fake_analysis),
    {
      session$flushReact()
      shared$analysis_handoff(list(
        result = handed_off,
        label = "Finished run: model topic"
      ))
      session$flushReact()
      session$setInputs(num_topics = 3L, include_plots = FALSE, analyze = 1)
      session$setInputs(model_analyze = 1)

      expect_length(configs, 2L)
      expect_null(configs[[1]]$config)
      expect_identical(
        configs[[2]]$config$model_params$temperature,
        0.4
      )
      expect_identical(
        configs[[2]]$config$model_params$max_tokens,
        480L
      )
      expect_identical(
        configs[[2]]$config$model_params$reasoning_effort,
        "low"
      )
      expect_identical(shared$.usage$last$calls, 2L)
      expect_identical(shiny::isolate(analysis())$themes, "Theme")
      expect_identical(
        shiny::isolate(analysis())$model_summary,
        "Summary"
      )
    }
  )
})

test_that("the continuation module plans and runs all configured arms", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("live", TRUE, active_tab = "experiment")
  group <- fake_fg()
  group$conversation_log <- sample_fg_log()
  group$moderator_id <- "Moderator"

  shiny::testServer(
    FocusGroup:::.fg_experiment_server,
    args = list(shared = shared),
    {
      fg(group)
      session$setInputs(
        cut = 3L,
        perturb_message_id = 2L,
        arm_1_enabled = TRUE,
        arm_1_label = "Reliable buses",
        arm_1_text = "The buses are reliable now.",
        arm_2_enabled = TRUE,
        arm_2_label = "Free fares",
        arm_2_text = "The buses should be free.",
        arm_3_enabled = FALSE,
        runs_per_arm = 2L,
        speaker = "P1",
        question = "What changed?"
      )
      session$setInputs(run = 1)

      expect_equal(nrow(result()), 6L)
      expect_identical(
        unique(result()$arm_label),
        c("Control", "Reliable buses", "Free fares")
      )
      expect_identical(shared$.usage$last$calls, 6L)
      expect_identical(
        group$agents$P1$config$model_params$temperature,
        0.4
      )
      expect_identical(
        group$agents$P1$config$model_params$max_tokens,
        480L
      )
      expect_identical(
        group$agents$P1$config$model_params$reasoning_effort,
        "low"
      )
      result_ui <- as.character(output$results)[[1]]
      expect_match(result_ui, "Reliable buses")
      expect_match(result_ui, "Free fares")
      expect_match(result_ui, "llmr-text-block")
    }
  )
})
