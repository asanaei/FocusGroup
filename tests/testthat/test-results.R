.result_runner <- function(experiments, ...) {
  prompts <- vapply(experiments$messages, function(messages) {
    paste(vapply(messages, function(message) {
      as.character(message$content %||% "")
    }, character(1)), collapse = "\n")
  }, character(1))
  experiments$response_text <- ifelse(
    grepl("Summary:", prompts, fixed = TRUE),
    "The group discussed library access and scheduling.",
    paste("This scripted response states a specific view about library hours",
          "and gives one reason for the group to consider."))
  experiments$success <- TRUE
  experiments$sent <- 2L
  experiments$rec <- 3L
  experiments$total <- 5L
  experiments
}

.small_result <- function(...) {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  run_focus_group(
    topic = "library hours",
    n_participants = 1,
    guide = list(
      Opening = "Welcome the participant.",
      Engagement = "Which library hours matter most?",
      Closing = "Close the discussion."
    ),
    flow = "round_robin",
    config = config,
    max_participant_responses = 1,
    verbose = FALSE,
    .runner = .result_runner,
    ...
  )
}

test_that("run_focus_group returns the fixed result class and fields", {
  result <- .small_result()
  expect_s3_class(result, "focus_group_result")
  expect_named(
    result,
    c("focus_group", "transcript", "summary", "participants", "usage", "metadata"),
    ignore.order = FALSE
  )
  expect_s3_class(result$focus_group, "FocusGroup")
  expect_true(is.data.frame(result$transcript))
  expect_true(is.data.frame(result$participants))
  expect_named(
    result$participants,
    c("id", "role", "is_moderator", "persona", "demographics",
      "survey_responses", "provider", "model"),
    ignore.order = FALSE
  )
  expect_true(is.character(result$participants$id))
  expect_true(is.character(result$participants$role))
  expect_true(is.logical(result$participants$is_moderator))
  expect_true(is.list(result$participants$demographics))
  expect_true(is.list(result$participants$survey_responses))
  expect_type(result$summary, "character")
  expect_length(result$summary, 1L)
  expect_named(result$usage, c("sent", "rec", "total"), ignore.order = FALSE)
  expect_true(all(vapply(result$usage, is.numeric, logical(1))))
  expect_equal(result$usage$total, result$usage$sent + result$usage$rec)
  expect_true(is.list(result$metadata))

  expect_true(all(c("message_id", "round", "phase", "speaker_id", "metadata") %in%
                    names(result$transcript)))
  expect_identical(result$transcript$message_id,
                   seq_len(nrow(result$transcript)))
  expect_false("turn" %in% names(result$transcript))
  expect_true(is.list(result$transcript$metadata))
})

test_that("result metadata records provenance without retaining a config object", {
  result <- .small_result()
  expect_named(
    result$metadata,
    c("topic", "purpose", "flow", "message_mode", "n_participants",
      "estimated_calls", "provider", "model"),
    ignore.order = FALSE
  )
  expect_identical(result$metadata$message_mode, "roleflip")
  expect_false(any(vapply(result$metadata, inherits, logical(1), "llm_config")))
  expect_false(any(grepl("api_key|credential", names(result$metadata),
                         ignore.case = TRUE)))
})

test_that("the call-count preflight stops unless the caller confirms", {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  response <- structure(
    list(text = paste("This mocked provider response states one position",
                      "and gives a concrete reason for discussion.")),
    class = "llmr_response"
  )
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) response,
    tokens = function(...) list(sent = 1L, rec = 1L, total = 2L),
    finish_reason = function(...) "stop",
    .package = "LLMR"
  )
  call <- function(confirm) run_focus_group(
    topic = "library hours",
    n_participants = 1,
    guide = c(Opening = 1, Closing = 1),
    flow = "round_robin",
    config = config,
    max_calls = 1,
    confirm = confirm,
    verbose = FALSE
  )
  expect_error(call(FALSE), "estimated|confirm|max_calls")
  expect_s3_class(call(TRUE), "focus_group_result")
})

test_that("offline runners retain the estimate without a live-call stop", {
  result <- .small_result(max_calls = 1, confirm = FALSE)
  expect_s3_class(result, "focus_group_result")
  expect_gt(result$metadata$estimated_calls, 0L)
})

test_that("result and FocusGroup print methods are concise", {
  result <- .small_result()
  result_print <- capture.output(print(result))
  group_print <- capture.output(print(result$focus_group))
  expect_lte(length(result_print), 12L)
  expect_lte(length(group_print), 12L)
  expect_match(paste(result_print, collapse = "\n"), "focus_group_result")
  expect_match(paste(group_print, collapse = "\n"), "FocusGroup")
})

test_that("a FocusGroup object cannot run twice", {
  result <- .small_result()
  expect_error(
    result$focus_group$run_simulation(verbose = FALSE),
    "run_focus_studio|continuation"
  )
})
