# Offline regression tests for fixes made during CRAN release preparation.
# No network, no live LLM calls: every LLMR entry point is mocked.

# A minimal llmr_response stand-in; as.character.llmr_response reads $text.
fake_response <- function(text) structure(list(text = text), class = "llmr_response")

# Build a small mocked focus group and run it. All LLM calls return a fixed
# long-enough utterance; token usage is controlled by `tok`.
run_mocked_fg <- function(tok = list(sent = 10L, rec = 5L)) {
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  mk <- function(id, mod = FALSE) FGAgent$new(
    id, list(direct_persona_description = paste("Persona", id)), cfg, is_moderator = mod)
  agents <- list(MOD = mk("MOD", TRUE), P1 = mk("P1"), P2 = mk("P2"))
  flow <- create_conversation_flow("round_robin", agents, "MOD")
  counter <- new.env(); counter$i <- 0
  testthat::local_mocked_bindings(
    call_llm_robust = function(config, messages, ...) {
      counter$i <- counter$i + 1
      fake_response(paste(
        "This is a sufficiently long mocked utterance about libraries and",
        "budgets, numbered", counter$i, "with several extra words so that",
        "word counts vary a little between speakers."))
    },
    tokens = function(...) tok,
    finish_reason = function(...) "stop",
    .package = "LLMR",
    .env = parent.frame())
  fg <- FocusGroup$new(
    topic = "libraries", purpose = "test", agents = agents, moderator_id = "MOD",
    turn_taking_flow = flow,
    question_script = list(
      list(phase = "engagement_question", text = "Q1?"),
      list(phase = "exploration_question", text = "Q2?"),
      list(phase = "closing")),
    admin_config = cfg,
    max_participant_responses = 2)
  fg$run_simulation(verbose = FALSE)
  fg
}

test_that("a short demographics data.frame is recycled by row, not read by column", {
  skip_if_not_installed("LLMR")
  demo <- data.frame(age = c(30, 40), gender = c("Female", "Male"),
                     education = c("PhD", "BA"), stringsAsFactors = FALSE)
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  expect_warning(
    ag <- create_agents(3, demographics = demo, config = cfg),
    "recycled")
  # agent 3 gets row 1 again, as a named list -- not the education COLUMN
  expect_identical(ag[[3]]$demographics,
                   list(age = "30", gender = "Female", education = "PhD"))
  expect_identical(ag[[1]]$demographics, ag[[3]]$demographics)
  expect_identical(ag[[2]]$demographics$age, "40")
})

test_that("create_agents returns a flow-ready named list", {
  skip_if_not_installed("LLMR")
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  agents <- create_agents(2, config = cfg)
  agent_ids <- vapply(agents, function(agent) agent$id, character(1))

  expect_identical(names(agents), unname(agent_ids))
  expect_s3_class(
    create_conversation_flow("round_robin", agents, "MOD"),
    "RoundRobinFlow"
  )
})

test_that("create_agents accepts direct personas without inventing survey data", {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  personas <- c(
    "A renter who depends on the evening bus.",
    "A driver who prioritizes road maintenance."
  )
  agents <- create_agents(
    2,
    config = config,
    direct_persona_descriptions = personas
  )
  expect_identical(
    unname(vapply(agents[c("P1", "P2")],
                  function(agent) agent$persona_description, character(1))),
    personas
  )
  expect_true(all(vapply(agents[c("P1", "P2")],
                         function(agent) length(agent$survey_responses) == 0L,
                         logical(1))))
})

test_that("removed public arguments are rejected", {
  expect_error(
    analyze_focus_group(NULL, sentiment_method = "afinn"),
    "unused argument"
  )
})

test_that("focus group analysis has no sentiment artifacts", {
  fg <- structure(list(
    conversation_log = list(list(speaker_id = "P1", text = "A response.")),
    analyze = function(...) list(),
    analyze_topics = function(...) NULL,
    analyze_tfidf = function(...) NULL,
    analyze_readability = function(...) NULL,
    analyze_themes = function(...) NULL
  ), class = "FocusGroup")

  capture.output(result <- analyze_focus_group(fg, include_plots = FALSE))
  expect_s3_class(result, "focus_group_analysis")
  expect_false("sentiment" %in% names(result))
  expect_false("sentiment_analysis_prompt" %in%
                 names(get_default_prompt_templates()))
})

test_that("parse_score_0_10 reads the intended score", {
  # explicit fraction forms
  expect_identical(parse_score_0_10("8/10"), 8L)
  expect_identical(parse_score_0_10("8 out of 10"), 8L)
  expect_identical(parse_score_0_10("I'd say 6/10."), 6L)
  # scale-label echoes still resolve to the answer
  expect_identical(parse_score_0_10("Desire to talk score (0-10): 8"), 8L)
  expect_identical(parse_score_0_10("On a scale from 0 to 10, I am at 7"), 7L)
  # plain answers unchanged
  expect_identical(parse_score_0_10("7"), 7L)
  expect_identical(parse_score_0_10("Score: 9."), 9L)
  expect_identical(parse_score_0_10("no number here"), 0L)
  expect_identical(parse_score_0_10(NA_character_), 0L)
  # out-of-range clamps
  expect_identical(parse_score_0_10("10"), 10L)
})

test_that("placeholder replacement keeps backslashes and regex metacharacters", {
  val <- "path C:\\temp and \\1 backref and $5"
  expect_identical(replace_placeholders("Said: {{u}}", list(u = val)),
                   paste0("Said: ", val))
  expect_identical(replace_placeholders_known("Said: {{u}}", list(u = val)),
                   paste0("Said: ", val))
  # unknown tokens still preserved by the _known variant
  expect_identical(replace_placeholders_known("{{a}} {{keep}}", list(a = "x\\y")),
                   "x\\y {{keep}}")
})

test_that("NA token usage accumulates as zero, not NA", {
  expect_identical(.fg_tok0(NA_integer_), 0)
  expect_identical(.fg_tok0(NULL), 0)
  expect_identical(.fg_tok0(7L), 7)
  skip_if_not_installed("LLMR")
  fg <- run_mocked_fg(tok = list(sent = NA_integer_, rec = NA_integer_))
  meta_na <- vapply(fg$conversation_log[-1], function(m) is.na(m$sent_tokens),
                    logical(1))
  expect_false(any(meta_na))
})

test_that("the System roster row is excluded and messages have unique ids and shared rounds", {
  skip_if_not_installed("LLMR")
  fg <- run_mocked_fg()

  message_ids <- vapply(fg$conversation_log, function(m) m$message_id, integer(1))
  rounds <- vapply(fg$conversation_log, function(m) m$round, integer(1))
  speakers <- vapply(fg$conversation_log, function(m) m$speaker_id, character(1))
  expect_identical(message_ids, seq_along(fg$conversation_log))
  expect_identical(rounds, c(0L, 1L, 1L, 1L, 2L, 2L, 2L, 3L))
  expect_identical(anyDuplicated(message_ids), 0L)
  expect_false(any(vapply(fg$conversation_log, function(m) "turn" %in% names(m),
                          logical(1))))
  expect_identical(speakers[1], "System")

  # analyze(): no System row, no NA speaker row
  stats <- fg$analyze()$speaker_stats
  expect_false(any(is.na(stats$speaker_id)))
  expect_false("System" %in% as.character(stats$speaker_id))
  expect_setequal(as.character(stats$speaker_id), c("MOD", "P1", "P2"))

  # participation balance: System can never be the least active speaker
  pb <- fg$analyze_participation_balance()
  expect_named(
    pb$participation_stats,
    c("speaker_id", "messages", "words", "avg_words",
      "message_percentage", "word_percentage"),
    ignore.order = FALSE
  )
  expect_false("System" %in% pb$participation_stats$speaker_id)
  expect_false(identical(pb$balance_metrics$least_active_speaker, "System"))

  # explicit request still reaches the roster row
  sys_rows <- fg$analyze(speaker_ids = "System")
  expect_true("System" %in% as.character(sys_rows$speaker_stats$speaker_id))
})

test_that("plots exclude the System row", {
  skip_if_not_installed("LLMR")
  skip_if_not_installed("ggplot2")
  fg <- run_mocked_fg()
  p <- fg$plot_participation_by_agent()
  expect_false("System" %in% p$data$agent_id)
  p2 <- fg$plot_word_count_distribution()
  expect_false("System" %in% p2$data$agent_id)
})

test_that("analyze_readability reports speaker ids, not row numbers", {
  skip_if_not_installed("LLMR")
  skip_if_not_installed("quanteda")
  skip_if_not_installed("quanteda.textstats")
  fg <- run_mocked_fg()
  r <- fg$analyze_readability()
  expect_setequal(r$speaker_id, c("MOD", "P1", "P2"))
  expect_false(any(r$speaker_id %in% as.character(seq_len(nrow(r)))))
})

test_that("focusgroup.seed does not disturb the caller's RNG stream", {
  set.seed(110); before <- runif(1)
  set.seed(110)
  withr::with_options(list(focusgroup.seed = 42L),
                      invisible(generate_diverse_demographics(3)))
  after <- runif(1)
  expect_identical(before, after)
  # and the seeded draw itself is deterministic
  d1 <- withr::with_options(list(focusgroup.seed = 42L),
                            generate_diverse_demographics(4))
  d2 <- withr::with_options(list(focusgroup.seed = 42L),
                            generate_diverse_demographics(4))
  expect_identical(d1, d2)
})

test_that("the GUI persona path applies the seed to the respondent draw", {
  skip_if_not_installed("LLMR")
  testthat::local_mocked_bindings(
    call_llm_robust = function(config, messages, ...)
      fake_response("A sufficiently long mocked reply about the topic at hand today."),
    tokens = function(...) list(sent = 1L, rec = 1L),
    finish_reason = function(...) "stop",
    .package = "LLMR")
  personas <- data.frame(age = as.character(20 + 1:10),
                         gender = rep(c("Male", "Female"), 5),
                         opinion = paste("view", 1:10), stringsAsFactors = FALSE)
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  run_once <- function() FocusGroup:::.fg_run_from_personas(
    topic = "topic", n_participants = 3, rows = integer(0), flow = "round_robin",
    message_mode = "roleflip", seed = 110L, max_participant_responses = 1L,
    config = cfg, data = personas)
  result1 <- run_once()
  result2 <- run_once()
  expect_s3_class(result1, "focus_group_result")
  expect_named(
    result1,
    c("focus_group", "transcript", "summary", "participants", "usage", "metadata"),
    ignore.order = FALSE
  )
  expect_named(
    result1$participants,
    c("id", "role", "is_moderator", "persona", "demographics",
      "survey_responses", "provider", "model"),
    ignore.order = FALSE
  )
  p1 <- vapply(result1$focus_group$agents,
               function(a) a$persona_description, "")
  p2 <- vapply(result2$focus_group$agents,
               function(a) a$persona_description, "")
  # the same seed draws the same respondents, so agent creation saw the seed
  expect_identical(p1, p2)
})
