.broadcast_group <- function(models = c("example-model", "example-model"), runner = NULL) {
  agents <- lapply(seq_along(models), function(i) FGAgent$new(paste0("P", i),
    list(direct_persona_description = paste("Participant", i)),
    LLMR::llm_config("openai", models[i]), .runner = runner))
  names(agents) <- paste0("P", seq_along(models))
  agents$MOD <- FGAgent$new("MOD", list(direct_persona_description = "Moderator."),
    LLMR::llm_config("openai", "example-model"), is_moderator = TRUE, .runner = runner)
  FocusGroup$new("Hours", "Study", agents, "MOD",
    create_conversation_flow("desire_based", agents, "MOD"),
    question_script = list(list(phase = "engagement_question", text = "Hours?")))
}

test_that("broadcast records every returned row including unknown and independent totals", {
  out <- NULL
  testthat::local_mocked_bindings(
    call_llm_broadcast = function(...) out,
    call_llm_robust = function(...) stop("unexpected fallback"), .package = "LLMR")
  cases <- list(
    list(data.frame(response_text = c("8", "9"), success = TRUE), c(0, 0, 0, 2)),
    list(data.frame(response_text = c("8", "9"), success = TRUE, sent = c(0, 2), rec = c(0, NA)), c(2, 0, 0, 1)),
    list(data.frame(response_text = c("8", "9"), success = TRUE, sent = 0, rec = 0), c(0, 0, 0, 0)),
    list(data.frame(response_text = c("8", "9"), success = TRUE, total = c(7, 9)), c(0, 0, 16, 2)))
  for (case in cases) {
    out <- case[[1]]; g <- .broadcast_group()
    g$turn_taking_flow$select_next_speaker(g)
    expect_equal(c(g$total_tokens_sent, g$total_tokens_received, g$total_tokens, g$calls_usage_unknown), case[[2]])
    expect_equal(sum(vapply(g$agents, function(a) a$tokens_total_agent, 0)), case[[2]][3])
    expect_equal(sum(vapply(g$agents, function(a) a$usage_unknown_agent, 0)), case[[2]][4])
  }
  out <- as.list(cases[[4]][[1]])
  g <- .broadcast_group(); g$turn_taking_flow$select_next_speaker(g)
  expect_equal(g$total_tokens, 16)
})

test_that("broadcast row identifiers map scores and usage to the same agents", {
  testthat::local_mocked_bindings(call_llm_broadcast = function(...) {
    data.frame(.experiment_id = c(2L, 1L), response_text = c("10", "0"), success = TRUE,
               sent = c(20, 10), rec = 1, total = c(22, 12))
  }, call_llm_robust = function(...) stop("unexpected fallback"), .package = "LLMR")
  g <- .broadcast_group()
  expect_identical(g$turn_taking_flow$select_next_speaker(g)$id, "P2")
  expect_equal(g$agents$P1$tokens_sent_agent, 10)
  expect_equal(g$agents$P2$tokens_total_agent, 22)
  expect_equal(g$total_tokens, 34)
})

test_that("unscored broadcast rows are accounted before heterogeneous rescoring", {
  fallback <- 0L
  testthat::local_mocked_bindings(call_llm_broadcast = function(config, messages, ...) {
    data.frame(response_text = if (config$model == "one") "bad score" else "9", success = TRUE,
               sent = 2, rec = 3, total = 8)
  }, call_llm_robust = function(...) {
    fallback <<- fallback + 1L
    data.frame(response_text = "8", sent = 4, rec = 5, total = 12)
  }, .package = "LLMR")
  g <- .broadcast_group(c("one", "two"))
  g$turn_taking_flow$select_next_speaker(g)
  expect_equal(fallback, 1)
  expect_equal(g$total_tokens, 28)
  expect_equal(g$agents$P1$tokens_total_agent, 20)
  expect_equal(g$agents$P2$tokens_total_agent, 8)
})

test_that("aggregate and malformed broadcasts never invent per-agent allocations", {
  out <- list(response_text = c("8", "9"), success = c(TRUE, TRUE), sent = 11, rec = 7, total = 21)
  testthat::local_mocked_bindings(call_llm_broadcast = function(...) out,
    call_llm_robust = function(...) data.frame(response_text = "8", sent = 0, rec = 0), .package = "LLMR")
  g <- .broadcast_group()
  expect_warning(g$turn_taking_flow$select_next_speaker(g), "Per-agent usage is unavailable")
  expect_equal(g$total_tokens, 21)
  expect_equal(g$agents$P1$tokens_sent_agent, 0)
  expect_equal(g$agents$P2$tokens_received_agent, 0)
  out <- data.frame(total = c(3, 4))
  g <- .broadcast_group(); g$turn_taking_flow$select_next_speaker(g)
  expect_equal(g$total_tokens, 7)
  expect_equal(g$calls_usage_unknown, 2)
})

test_that("desire retries count unknown rows and session calls exceed transcript turns", {
  runner <- .usage_runner(list(list(response_text = ""), list(response_text = "8")))
  g <- .broadcast_group("example-model", runner)
  g$turn_taking_flow$select_next_speaker(g)
  expect_equal(g$calls_usage_unknown, 2)
  expect_equal(g$agents$P1$usage_unknown_agent, 2)
  calls <- 0L
  runner <- function(experiments, ...) {
    calls <<- calls + 1L
    experiments$response_text <- "8"
    experiments
  }
  result <- run_focus_group("Hours", LLMR::llm_config("openai", "example-model"),
    2, demographics = data.frame(age = 30:31), guide = list(Engagement = "Hours?"),
    max_participant_responses = 1, verbose = FALSE, .runner = runner)
  expect_equal(result$usage$unknown, calls)
  expect_gt(calls, nrow(result$transcript))
  expect_true(all(is.na(result$transcript$sent_tokens[-1])))
  expect_equal(result$transcript$metadata[[2]]$usage_unknown, 1)
  reused <- result$focus_group$agents
  g <- FocusGroup$new("Hours", "Study", reused, "MOD",
    create_conversation_flow("round_robin", reused, "MOD"),
    question_script = list(list(phase = "closing", text = "Close.")))
  g$run_simulation()
  expect_equal(g$calls_usage_unknown, 2)
})

test_that("a broadcast without returned records leaves coverage unavailable", {
  testthat::local_mocked_bindings(call_llm_broadcast = function(...) NULL,
    call_llm_robust = function(...) data.frame(response_text = "8", sent = 2, rec = 3), .package = "LLMR")
  g <- .broadcast_group()
  g$turn_taking_flow$select_next_speaker(g)
  expect_equal(g$total_tokens, 10)
  expect_true(is.na(g$calls_usage_unknown))
})
