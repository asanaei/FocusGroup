.session_run <- function(runner, ...) run_focus_group("Library hours",
  LLMR::llm_config("openai", "example-model"), n_participants = 2,
  demographics = data.frame(age = c(30, 40)),
  guide = list(Opening = "Welcome.", Engagement = "Which hours?", Closing = "Close."),
  flow = "round_robin", verbose = FALSE, max_participant_responses = 1,
  .runner = runner, ...)

test_that("partial sessions preserve the live prefix, cause, and operation phase", {
  for (n in 1:4) {
    e <- tryCatch(.session_run(.session_failure(n)), focusgroup_incomplete_session = identity)
    expect_s3_class(e, "focusgroup_incomplete_session")
    expect_s3_class(e, "rlang_error")
    expect_s3_class(e$parent, "fixture_provider_error")
    expect_identical(e$phase, c("opening", "engagement_question", "engagement_question", "closing")[[n]])
    expect_identical(e$partial$metadata$failure$phase, e$phase)
    expect_identical(e$partial$metadata$failure$message, "fixture provider failure")
    expect_false(e$partial$metadata$complete)
    expect_named(e$partial, c("focus_group", "transcript", "summary", "participants", "usage", "metadata",
                              "calls", "routing", "probes", "injections"))
    expect_equal(nrow(e$partial$transcript), n)
    expect_equal(e$partial$usage$total, 9 * (n - 1))
    expect_true(is.na(e$partial$usage$unknown))
  }
  expect_error(.session_run(.session_failure(1), max_calls = Inf), "finite positive")
})

test_that("failed utterance retries transfer every returned call to the group", {
  for (second in list(list(response_text = ""),
                      structure(list(message = "retry failed", call = NULL), class = c("error", "condition")))) {
    first <- list(response_text = "Incomplete...", sent = 10, rec = 5, total = 17)
    e <- tryCatch(.session_run(.usage_runner(list(first, second))), focusgroup_incomplete_session = identity)
    expect_s3_class(e, "focusgroup_incomplete_session")
    expect_equal(e$partial$usage$total, 17)
    expect_equal(e$partial$usage$sent, 10)
    expect_equal(e$partial$focus_group$agents$MOD$tokens_total_agent, 17)
    if (inherits(second, "condition")) expect_true(is.na(e$partial$usage$unknown)) else expect_equal(e$partial$usage$unknown, 1)
  }
  failed <- list(success = FALSE, error_message = "unsuccessful row", total = 7)
  e <- tryCatch(.session_run(.usage_runner(list(failed))), focusgroup_incomplete_session = identity)
  expect_match(conditionMessage(e$parent), "unsuccessful row")
  expect_equal(e$partial$usage$total, 7); expect_equal(e$partial$usage$unknown, 1)
})

test_that("complete results include final summary failures without losing usage", {
  ok <- .session_run(.session_failure(99))
  expect_true(ok$metadata$complete); expect_null(ok$metadata$failure)
  expect_warning(out <- .session_run(.session_failure(5)), "final summary call failed")
  expect_true(out$metadata$complete); expect_true(is.na(out$summary))
  expect_equal(out$usage$total, 36)
})

test_that("interim summaries have a separate failure phase and recorded usage", {
  # Two moderator turns of about 55,000 estimated tokens each: after the
  # second, the 64,000-token prompt window drops the first, and the interim
  # summary that covers it is the third call.
  n <- 0L
  runner <- function(experiments, ...) {
    n <<- n + 1L
    experiments$response_text <- if (n <= 2) paste0(strrep("Long discussion. ", 13000), ".") else "Summary."
    experiments$total <- 7L
    if (n == 3) { experiments$success <- FALSE; experiments$error_message <- "interim failed" }
    experiments
  }
  guide <- list(Opening = c("Welcome.", "Explain the ground rules."))
  e <- tryCatch(run_focus_group("Hours", LLMR::llm_config("openai", "example-model"),
    1, guide = guide, demographics = data.frame(age = 40),
    flow = "round_robin", .runner = runner, verbose = FALSE), focusgroup_incomplete_session = identity)
  expect_identical(e$phase, "interim_summary")
  expect_equal(e$partial$usage$total, 21); expect_equal(e$partial$usage$unknown, 3)
  n <- 0L
  success <- function(experiments, ...) {
    out <- runner(experiments, ...); out$success <- TRUE; out
  }
  r <- run_focus_group("Hours", LLMR::llm_config("openai", "example-model"),
    1, guide = guide, demographics = data.frame(age = 40),
    flow = "round_robin", .runner = success, verbose = FALSE)
  expect_equal(n, 4); expect_equal(r$usage$total, 28)
})

test_that("session sanitization preserves provenance and status through the flow", {
  e <- tryCatch(run_focus_group("Hours", LLMR::llm_config("openai", "example-model"),
    n_participants = 1, guide = list(Engagement = "Hours?"), flow = "round_robin",
    verbose = FALSE, .runner = .session_failure(2)),
    focusgroup_incomplete_session = identity)
  fg <- e$partial$focus_group
  expect_length(attr(fg$agents, "source_rows"), 1L)
  clean <- .fg_sanitize_session(fg)
  expect_identical(attributes(clean$agents), attributes(fg$agents))
  expect_identical(clean$turn_taking_flow$agents, clean$agents)
  expect_false(identical(clean$turn_taking_flow, fg$turn_taking_flow))
  for (a in clean$turn_taking_flow$agents) { expect_null(a$config); expect_null(a$.runner) }
  expect_null(clean$admin_config)
  expect_false(is.null(fg$agents$MOD$config)); expect_true(is.function(fg$agents$MOD$.runner))
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path))
  saveRDS(clean, path); restored <- readRDS(path)
  expect_identical(attr(restored, "session_status"), list(complete = FALSE,
    failure = list(phase = "engagement_question", message = "fixture provider failure")))
  expect_output(print(restored), "Status: incomplete (stopped during engagement_question)", fixed = TRUE)
  attr(restored, "session_status") <- NULL
  expect_false(any(grepl("Status", capture.output(print(restored)))))
})

test_that("successful mixed retries retain group subtotals independently of message NA", {
  records <- list(list(response_text = "Incomplete...", sent = 10, rec = 5, total = 17),
    list(response_text = "Complete reply."), list(sent = 2, rec = 3, total = 9))
  out <- .session_run(.usage_runner(records))
  expect_true(out$metadata$complete)
  expect_equal(out$usage$sent, 18); expect_equal(out$usage$rec, 17)
  expect_equal(out$usage$total, 53); expect_equal(out$usage$unknown, 1)
  expect_true(is.na(out$transcript$sent_tokens[2]))
  expect_equal(out$transcript$metadata[[2]]$usage_unknown, 1)
  expect_equal(out$focus_group$agents$MOD$tokens_total_agent, 35)
})
