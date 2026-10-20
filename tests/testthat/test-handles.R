# The IDs a researcher gives agents stay in the records and out of every
# prompt: the agents see each other under handles. Offline, with scripted
# runners.

.hd_config <- function() LLMR::llm_config("openai", "example-model")

.hd_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

# Replies long enough that the log outgrows the prompt window, so that an
# interim summary is written; probes answer with a number.
.hd_stub <- function(long = FALSE) {
  env <- new.env()
  env$n <- 0L
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    prompt <- .hd_prompt(experiments$messages[[1]])
    filler <- if (long) paste(rep("Evening hours let working families use the branch.", 260),
                              collapse = " ") else ""
    experiments$response_text <- if (grepl("private question", prompt, fixed = TRUE)) {
      "5"
    } else if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      "6"
    } else if (grepl("Summary:$", prompt)) {
      "A short summary of what was said."
    } else {
      paste(sprintf("Reply %03d: the hours matter for a concrete reason.", env$n), filler)
    }
    experiments
  }
  env
}

.hd_agents <- function(runner, ids = c("teacher_conservative", "V240001"),
                       moderator = "lead_moderator") {
  agents <- lapply(seq_along(ids), function(i) {
    FGAgent$new(ids[[i]], list(direct_persona_description = paste("Persona", i, "of the group.")),
                .hd_config(), .runner = runner)
  })
  names(agents) <- ids
  agents[[moderator]] <- FGAgent$new(moderator, list(direct_persona_description = "A moderator."),
                                      .hd_config(), is_moderator = TRUE, .runner = runner)
  agents
}

.hd_group <- function(agents, script, ...) {
  moderator <- names(agents)[length(agents)]
  FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = moderator,
    turn_taking_flow = create_conversation_flow("desire_based", agents, moderator),
    question_script = script, max_participant_responses = 2, ...)
}

.hd_script <- function(n = 6) {
  c(list(list(phase = "opening", text = "Welcome the group.")),
    lapply(seq_len(n), function(i) list(phase = "engagement_question",
                                        text = sprintf("Question %d about the hours?", i))),
    list(list(phase = "closing", text = "Close the session.")))
}

test_that("no prompt shows the agents' IDs, and the records keep them", {
  stub <- .hd_stub(long = TRUE)
  agents <- .hd_agents(stub$runner)
  fg <- .hd_group(agents, .hd_script())
  fg$run_simulation(probes = fg_probe("From 1 to 7, how much do the hours matter?",
                                      at = list("start", fg_after_item(3), "before_closing", "end")))
  result <- FocusGroup:::.fg_focus_group_result(fg, "desire_based", "roleflip", 2L, NA_integer_)
  calls <- result$calls
  expect_true(all(c("moderator", "participant", "desire", "probe", "interim_summary",
                    "summary") %in% calls$call_type))
  ids <- c("teacher_conservative", "V240001", "lead_moderator")
  for (i in seq_len(nrow(calls))) {
    prompt <- .hd_prompt(calls$messages[[i]])
    for (id in ids) {
      expect_false(grepl(id, prompt, fixed = TRUE),
                   info = paste(calls$call_type[[i]], "call", i, "shows", id))
    }
  }
  # The agents see each other as P1, P2, and Moderator.
  participant <- calls[calls$call_type == "participant", ]
  later <- .hd_prompt(participant$messages[[nrow(participant)]])
  expect_match(later, "Moderator: ", fixed = TRUE)
  expect_match(later, "In this discussion you are P[12]\\.")
  for (i in seq_len(nrow(participant))) {
    system <- participant$messages[[i]][[1]]$content
    expected <- result$participants$handle[result$participants$id == participant$agent_id[[i]]]
    expect_match(system, paste0("In this discussion you are ", expected, "."), fixed = TRUE)
  }
  # An agent reads its own earlier turns as its own.
  own <- participant$messages[[nrow(participant)]]
  speaker <- participant$agent_id[[nrow(participant)]]
  said <- result$transcript$text[result$transcript$speaker_id == speaker]
  assistant <- vapply(Filter(function(m) identical(m$role, "assistant"), own),
                      function(m) m$content, character(1))
  expect_true(length(assistant) > 0L)
  expect_true(all(assistant %in% said))

  # The records keep the IDs, beside the handles.
  tr <- result$transcript
  expect_setequal(unique(tr$speaker_id), c("System", ids))
  expect_identical(unique(tr$handle[tr$speaker_id == "teacher_conservative"]), "P1")
  expect_identical(unique(tr$handle[tr$speaker_id == "V240001"]), "P2")
  expect_identical(unique(tr$handle[tr$speaker_id == "lead_moderator"]), "Moderator")
  expect_identical(tr$item[[1]], 0L)
  expect_match(tr$text[[1]], "teacher_conservative", fixed = TRUE)
  expect_identical(result$participants$handle, c("P1", "P2", "Moderator"))
  expect_identical(result$metadata$handles,
                   data.frame(agent_id = ids, handle = c("P1", "P2", "Moderator"),
                              stringsAsFactors = FALSE))
  expect_true(all(result$routing$selected_id %in% ids))
  expect_true(all(vapply(fg$records$probes, `[[`, character(1), "agent_id") %in% ids))
  expect_true(nzchar(fg$current_conversation_summary))
})

test_that("handles = \"ids\" shows the IDs, with a message, and custom handles replace them", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  expect_message(fg <- .hd_group(agents, .hd_script(1), handles = "ids", call_on_silent = "none"),
                 "teacher_conservative, V240001, lead_moderator")
  fg$run_simulation()
  prompts <- vapply(fg$records$calls, function(call) .hd_prompt(call$messages), character(1))
  expect_true(any(grepl("teacher_conservative: ", prompts, fixed = TRUE)))
  expect_identical(unname(fg$handles), names(agents))

  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  fg <- .hd_group(agents, .hd_script(1), handles = c(teacher_conservative = "Ana"),
                  call_on_silent = "none")
  expect_identical(fg$handles, c(teacher_conservative = "Ana", V240001 = "P2",
                                 lead_moderator = "Moderator"))
  fg$run_simulation()
  prompts <- vapply(fg$records$calls, function(call) .hd_prompt(call$messages), character(1))
  expect_false(any(grepl("teacher_conservative", prompts, fixed = TRUE)))
  expect_true(any(grepl("In this discussion you are Ana.", prompts, fixed = TRUE)))
  expect_identical(fg$agents$teacher_conservative$handle, "Ana")
})

test_that("handles and agent names are checked before any call", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  build <- function(handles, ...) .hd_group(agents, .hd_script(1), handles = handles, ...)
  expect_error(build(c(teacher_conservative = "Ana", V240001 = "Ana")), "own handle")
  expect_error(build(c(teacher_conservative = "ana", V240001 = "Ana")), "own handle")
  expect_error(build(c(teacher_conservative = "P2")), "own handle")
  expect_error(build(c(teacher_conservative = "System")), "System")
  expect_error(build(c(teacher_conservative = "Participant 3")), "standalone number")
  expect_error(build(c(teacher_conservative = "10")), "standalone number")
  expect_error(build(c(nobody = "Ana")), "not in the roster: nobody")
  expect_error(build(c("Ana")), "named by the agent ID")
  expect_error(build(c(teacher_conservative = " ")), "non-empty")
  expect_error(build(c(teacher_conservative = "Ana", teacher_conservative = "Bo")), "more than once")
  expect_error(build(list(teacher_conservative = "Ana")), "character vector")
  expect_identical(build(c(teacher_conservative = "P3x"))$handles[["teacher_conservative"]], "P3x")

  renamed <- agents
  names(renamed)[1] <- "teacher"
  expect_error(FocusGroup$new("library hours", "Test.", renamed, "lead_moderator",
                              create_conversation_flow("round_robin", agents, "lead_moderator")),
               "named by its agent's `id`")
  expect_error(FocusGroup$new("library hours", "Test.", unname(agents), "lead_moderator",
                              create_conversation_flow("round_robin", agents, "lead_moderator")),
               "named by its agent's `id`")

  # An injected voice may not take a participant's handle; one from the
  # moderator's handle is the moderator's.
  fg <- build(c(teacher_conservative = "Ana"), call_on_silent = "none")
  expect_error(fg$run_simulation(injections = fg_injection("Hello.", fg_after_item(2),
                                                           from = "Ana")),
               "handle")
  fg <- build(NULL, call_on_silent = "none")
  expect_error(fg$run_simulation(injections = fg_injection("Hello.", fg_after_item(2),
                                                           from = "p2")),
               "handle")
  expect_identical(stub$n, 0L)
  fg$run_simulation(injections = fg_injection("A word from me.", fg_after_item(2),
                                              from = "Moderator"))
  injected <- Filter(function(m) isTRUE(m$metadata$injected), fg$conversation_log)[[1]]
  expect_identical(injected$speaker_id, "lead_moderator")
  expect_true(injected$is_moderator)
})

test_that("identifier columns stay out of persona text and are reported", {
  cfg <- .hd_config()
  data <- data.frame(id = c("R-101", "R-102"), respondent_id = c(9001L, 9002L),
                     age = c("30", "41"), answer = c("Yes", "No"))
  agents <- create_agents_from_data(data, 2, cfg, demographic_cols = "age")
  personas <- vapply(agents[1:2], function(a) a$persona_description, character(1))
  expect_false(any(grepl("R-10|900[12]", personas)))
  expect_true(all(grepl("age: ", personas, fixed = TRUE)))
  expect_true(all(grepl("Question: answer", personas, fixed = TRUE)))
  report <- persona_rendering_report(agents)
  ids <- report[report$variable %in% c("id", "respondent_id"), ]
  expect_identical(nrow(ids), 4L)
  expect_true(all(ids$status == "dropped"))
  expect_true(all(ids$reason == "identifier column"))
  expect_setequal(ids$code, c("R-101", "R-102", "9001", "9002"))
  expect_identical(sort(as.character(attr(agents, "source_ids")$id)), c("R-101", "R-102"))

  # The same for a data frame given to create_agents().
  agents <- create_agents(2, cfg, demographics = data.frame(persona_id = c("x7", "y8"),
                                                            age = c(30, 41)))
  personas <- vapply(agents[1:2], function(a) a$persona_description, character(1))
  expect_false(any(grepl("x7|y8|persona_id", personas)))
  report <- persona_rendering_report(agents)
  expect_identical(unique(report$reason[report$variable == "persona_id"]), "identifier column")
  expect_identical(agents$P1$demographics$persona_id, "x7")
})

test_that("an automatically chosen survey variable that differs in every row is left out", {
  lab <- function(x, labels = NULL, label) {
    x <- if (is.null(labels)) x else haven::labelled(x, labels)
    attr(x, "label") <- label
    x
  }
  d <- data.frame(
    AGE = lab(c(1L, 2L, 3L, 2L), c("18-29" = 1, "30-49" = 2, "50+" = 3), "Age group"),
    CASEID = lab(c(240001, 240002, 240003, 240004), label = "Case ID"),
    PARTY = lab(c(1L, 2L, 1L, 2L), c("Democrat" = 1, "Republican" = 2), "Party"),
    RANK = lab(c(1L, 2L, 3L, 4L), c("One" = 1, "Two" = 2, "Three" = 3, "Four" = 4), "Rank"),
    INCOME = lab(c(50000, 62000, 50000, 71000), label = "Income in dollars"))
  f <- tempfile(fileext = ".dta"); on.exit(unlink(f), add = TRUE)
  haven::write_dta(d, f)
  cfg <- .hd_config()
  msgs <- testthat::capture_messages(suppressWarnings(
    agents <- create_agents_from_survey(4, f, cfg, demographic_vars = c(age = "AGE"))))
  expect_true(any(grepl("respondent identifiers.*CASEID", msgs)))
  personas <- vapply(agents[1:4], function(a) a$persona_description, character(1))
  expect_false(any(grepl("24000[1-4]|Case ID", personas)))
  # A labeled item and an unlabeled number with a repeated value are kept.
  expect_true(all(grepl("Rank", personas, fixed = TRUE)))
  expect_true(all(grepl("Income in dollars", personas, fixed = TRUE)))
  report <- persona_rendering_report(agents)
  expect_true(all(report$reason[report$variable == "CASEID"] == "identifier column"))
  # Named in `survey_vars`, the variable is rendered.
  suppressWarnings(named <- create_agents_from_survey(
    4, f, cfg, demographic_vars = c(age = "AGE"), survey_vars = c("CASEID", "PARTY")))
  expect_true(any(grepl("24000", vapply(named[1:4], function(a) a$persona_description, ""))))
})

test_that("an imported transcript keeps its speakers' names and numbers its items", {
  data <- data.frame(
    speaker = c("Ana", "Moderator", "Ana", "Speaker 1", "Moderator", "Speaker 1"),
    text = c("Before we start.", "Welcome. What matters?", "Evenings.", "Mornings.",
             "Ana, say more?", "Weekends too."))
  expect_no_message(fg <- focus_group_from_transcript(data))
  expect_identical(unname(fg$handles), names(fg$agents))
  log <- fg$conversation_log
  expect_identical(vapply(log, `[[`, integer(1), "item"), c(0L, 1L, 1L, 1L, 2L, 2L))
  expect_identical(vapply(log, `[[`, character(1), "handle"), data$speaker)
  view <- fg$.__enclos_env__$private$agent_view()
  expect_identical(vapply(view, `[[`, character(1), "speaker_id"), data$speaker)
  tr <- FocusGroup:::.fg_transcript_data(log)
  expect_identical(tr$item, c(0L, 1L, 1L, 1L, 2L, 2L))
  expect_identical(tr$handle, data$speaker)

  # A supplied item column is kept; message ids continue after the largest.
  data$item <- c(0L, 1L, 1L, 1L, 3L, 3L)
  data$message_id <- c(1L, 2L, 5L, 9L, 10L, 14L)
  fg <- focus_group_from_transcript(data)
  expect_identical(vapply(fg$conversation_log, `[[`, integer(1), "item"), data$item)
  next_id <- fg$.__enclos_env__$private$log_message("Ana", "One more.", round = 2L,
                                                    is_moderator = FALSE)
  expect_identical(next_id, 15L)
})

test_that("summarize() labels by handle, leaves out the roster, and calls live unless given a runner", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  fg <- .hd_group(agents, .hd_script(1), call_on_silent = "none")
  fg$run_simulation()
  seen <- NULL
  recorder <- function(experiments, ...) {
    seen <<- .hd_prompt(experiments$messages[[1]])
    experiments$response_text <- "A summary."
    experiments
  }
  fg$summarize(.hd_config(), .runner = recorder)
  expect_false(grepl("teacher_conservative|V240001|lead_moderator", seen))
  expect_false(grepl("System", seen, fixed = TRUE))
  expect_match(seen, "P1 (Message", fixed = TRUE)
  expect_match(seen, "Moderator (Message", fixed = TRUE)

  # Without a runner the call goes to the provider, not to the session's
  # scripted runner.
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) stop("A live call was attempted."), .package = "LLMR")
  before <- stub$n
  expect_error(fg$summarize(.hd_config()), "live call was attempted")
  expect_identical(stub$n, before)

  empty <- .hd_group(.hd_agents(stub$runner), .hd_script(1))
  expect_error(empty$summarize(.hd_config(), .runner = recorder), "no messages to summarize")
  expect_error(fg$summarize(.hd_config(), transcript_override = "  ", .runner = recorder),
               "empty")
})
