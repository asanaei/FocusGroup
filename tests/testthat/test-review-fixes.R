# Regression tests for the 0.5.2 corrections: a runnable default, no
# fabricated or recycled participants, value-preserving ingestion, a leak-free
# continuation, sanitized session saves, and resilient summaries.

.rv_cfg <- function() LLMR::llm_config("groq", "demo", temperature = 0)
.rv_echo_runner <- function(experiments, ...) {
  experiments$response_text <- rep_len(
    c("A concrete point about the topic, stated plainly. 7",
      "6", "8", "A second view with a reason attached. 5"),
    nrow(experiments))
  experiments
}

test_that("the untouched default plans within max_calls and runs offline", {
  skip_if_not_installed("LLMR")
  guide <- eval(formals(run_focus_group)$guide)
  script <- FocusGroup:::.fg_build_question_script(guide, "local transit")
  phases <- vapply(script, `[[`, character(1), "phase")
  response_phases <- c("icebreaker_question", "engagement_question",
                       "exploration_question", "probing_focused",
                       "ending_question", "generic_discussion")
  q <- sum(phases %in% response_phases)
  est <- length(script) + q * 2L + q * 2L * 6L + 1L
  expect_lte(est, eval(formals(run_focus_group)$max_calls))

  out <- suppressWarnings(run_focus_group(
    "local transit", .rv_cfg(), verbose = FALSE, seed = 110,
    .runner = .rv_echo_runner))
  expect_true(length(out$focus_group$conversation_log) > 0)
})

test_that("direct personas acquire no fabricated demographic records", {
  skip_if_not_installed("LLMR")
  ag <- create_agents(
    2, config = .rv_cfg(),
    direct_persona_descriptions = c("A retired bus driver.",
                                    "A night-shift nurse."))
  participants <- Filter(function(a) !isTRUE(a$is_moderator), ag)
  for (a in participants) {
    expect_true(is.null(a$demographics) || length(a$demographics) == 0)
  }
})

test_that("short participant frames error instead of recycling", {
  skip_if_not_installed("LLMR")
  demo <- data.frame(age = c(30, 40))
  expect_error(create_agents(4, config = .rv_cfg(), demographics = demo),
               "duplicate research units")
})

test_that("survey values keep decimals and parentheticals", {
  persona <- FocusGroup:::generate_persona(
    demographics = list(score = "1.5", race = "Asian (non-Hispanic)",
                        gender = "1. Male"))
  expect_match(persona, "1.5", fixed = TRUE)
  expect_match(persona, "Asian (non-Hispanic)", fixed = TRUE)
  expect_match(persona, "Male", fixed = TRUE)
  expect_false(grepl("1. Male", persona, fixed = TRUE))
})

test_that("the sanitized session carries no configs or runners", {
  skip_if_not_installed("LLMR")
  out <- suppressWarnings(run_focus_group(
    "sanitizer check", .rv_cfg(), n_participants = 2,
    guide = c(Opening = 1, Engagement = 1, Closing = 1),
    flow = "round_robin", verbose = FALSE, seed = 110,
    .runner = .rv_echo_runner))
  clean <- FocusGroup:::.fg_sanitize_session(out$focus_group)
  for (a in clean$agents) {
    expect_null(a$config)
    expect_null(a$.runner)
  }
  expect_null(clean$admin_config)
  # the live session is untouched
  expect_false(is.null(out$focus_group$agents[[1]]$.runner))
  # and the sanitized object serializes without closures over live state
  f <- tempfile(fileext = ".rds")
  saveRDS(clean, f)
  expect_s3_class(readRDS(f), "FocusGroup")
})

test_that("a failed final summary returns the completed transcript", {
  skip_if_not_installed("LLMR")
  failing_summary_runner <- function(experiments, ...) {
    user <- vapply(experiments$messages, function(m) {
      paste(unlist(m), collapse = " ")
    }, character(1))
    if (any(grepl("Provide a concise prose summary", user, fixed = TRUE))) {
      stop("provider exploded")
    }
    experiments$response_text <- rep_len("A short reply. 7", nrow(experiments))
    experiments
  }
  expect_warning(
    out <- run_focus_group(
      "summary failure", .rv_cfg(), n_participants = 2,
      guide = c(Opening = 1, Engagement = 1),
      flow = "round_robin", verbose = FALSE, seed = 110,
      .runner = failing_summary_runner),
    "final summary")
  expect_true(length(out$focus_group$conversation_log) > 0)
})

test_that("blank strings count zero words", {
  expect_identical(FocusGroup:::.fg_word_count(c("two words", "", "  ", "one")),
                   c(2L, 0L, 0L, 1L))
})

test_that("imported moderator matching is exact, not substring", {
  df <- data.frame(
    speaker = c("Moderator", "Modesto", "P2"),
    text = c("Welcome.", "I drive daily.", "I take the bus."))
  fg <- focus_group_from_transcript(df, speaker_col = "speaker",
                                    text_col = "text")
  flags <- vapply(fg$agents, function(a) isTRUE(a$is_moderator), logical(1))
  expect_true(flags[["Moderator"]])
  expect_false(flags[["Modesto"]])
})

test_that("a bare create_agents draws real ANES respondents", {
  skip_if_not_installed("LLMR")
  set.seed(110)
  ag <- create_agents(3, config = .rv_cfg())
  participants <- Filter(function(a) !isTRUE(a$is_moderator), ag)
  expect_length(participants, 3L)
  # personas carry real ANES demographic fields, not invented ones
  personas <- vapply(participants, function(a) a$persona_description,
                     character(1))
  expect_true(all(nzchar(personas)))
  demo_fields <- LLMR::llm_persona_demographic_fields(LLMR::anes_2024_personas)
  expect_true(length(demo_fields) > 0)
  # and no invented survey battery about technology or privacy
  expect_false(any(grepl("tech_usage_comfort|privacy_concern_level",
                         personas)))
  # asking for more participants than the example holds is refused
  expect_error(create_agents(101, config = .rv_cfg()), "bundled ANES")
})

test_that("topic documents are utterances, not speakers, by default", {
  skip_if_not_installed("topicmodels")
  skip_if_not_installed("tidytext")
  long <- paste(rep("transit buses routes schedules riders fares", 6),
                collapse = " ")
  df <- data.frame(
    speaker = rep(c("P1", "P2"), each = 4),
    text = rep(long, 8))
  fg <- focus_group_from_transcript(df, speaker_col = "speaker",
                                    text_col = "text")
  res <- suppressWarnings(fg$analyze_topics(num_topics = 2))
  expect_identical(res$unit, "message")
  expect_equal(res$num_documents, 8L)   # one per utterance
  res_sp <- suppressWarnings(fg$analyze_topics(num_topics = 2,
                                               unit = "speaker"))
  expect_identical(res_sp$unit, "speaker")
  expect_equal(res_sp$num_documents, 2L)  # one per speaker
})
