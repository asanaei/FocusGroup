# The default number of participant responses per question is 2, as
# documented: the option set at load time and the estimate that the live gate
# computes for an untouched call agree with the help.

test_that("the load default and actual live gate use two responses", {
  withr::local_options(list(focusgroup.max_participant_responses = NULL))
  FocusGroup:::.onLoad(NULL, "FocusGroup")
  expect_identical(getOption("focusgroup.max_participant_responses"), 2L)
  cfg <- LLMR::llm_config("openai", "example-model")
  count <- 0L
  live <- function(...) { count <<- count + 1L; data.frame(response_text = "8", sent = 0, rec = 0) }
  testthat::local_mocked_bindings(call_llm_robust = live,
    call_llm_broadcast = function(config, messages, ...) {
      count <<- count + length(messages)
      data.frame(response_text = rep("8", length(messages)), success = TRUE, sent = 0, rec = 0)
    }, .package = "LLMR")
  call <- function(...) run_focus_group("Hours", cfg, demographics = data.frame(age = 31:36), verbose = FALSE,
                                      call_on_silent = "none", ...)
  expect_error(call(max_calls = 92), "Estimated model outputs.*93")
  expect_identical(count, 0L)
  result <- call(max_calls = 93)
  expect_identical(result$metadata$estimated_calls, 93L)
  # 8 moderator lines, 12 replies, the summary, and 66 desire scores: all
  # six participants at the first pick of each of the six questions and five
  # at the second, since only the speaker just before is passed over.
  expect_equal(count, 87)
  expect_equal(result$focus_group$max_participant_responses, 2)
  count <- 0L
  withr::local_options(list(focusgroup.max_participant_responses = 3L))
  FocusGroup:::.onLoad(NULL, "FocusGroup")
  expect_identical(getOption("focusgroup.max_participant_responses"), 3L)
  expect_error(call(max_calls = 100), "135")
  expect_identical(count, 0L)
  expect_s3_class(call(max_calls = 100, confirm = TRUE), "focus_group_result")
  expect_identical(call(max_participant_responses = 2)$metadata$estimated_calls, 93L)
  for (limit in c(Inf, -Inf, NA, NaN, 1.5, .Machine$integer.max + 1)) expect_error(call(max_calls = limit), "max_calls")
  expect_error(call(max_participant_responses = .Machine$integer.max), "estimate.*integer range")
  expect_error(.fg_validate_estimate(NA_real_), "estimate is unavailable")
  expect_error(call(guide = c(Engagement = .Machine$integer.max)), "estimate.*integer range")
  withr::local_options(list(focusgroup.max_participant_responses = NULL))
  agents <- create_agents(1, cfg, demographics = data.frame(age = 40))
  g <- suppressMessages(FocusGroup$new("Hours", "Study", agents, "MOD", create_conversation_flow("round_robin", agents, "MOD")))
  expect_equal(g$max_participant_responses, 2)
})
