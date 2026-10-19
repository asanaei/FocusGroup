test_that("usage distinguishes missing components, empty frames, and known zero", {
  for (x in list(NULL, list(), list(sent = NULL), list(sent = NA), "reply")) {
    u <- extract_token_counts(x)
    expect_equal(unlist(u[1:4]), c(sent = 0, rec = 0, total = 0, unknown = 1))
  }
  expect_equal(extract_token_counts(data.frame())$unknown, 0)
  expect_equal(extract_token_counts(list(sent = 0, rec = 0))$unknown, 0)
  a <- extract_token_counts(list(sent = 0)); b <- extract_token_counts(list(rec = 0))
  expect_equal(a$sent_unknown, 0); expect_equal(a$rec_unknown, 1)
  expect_equal(b$sent_unknown, 1); expect_equal(b$rec_unknown, 0)
  expect_equal(extract_token_counts(data.frame(sent = c(10, 20), rec = c(1, 2), total = c(11, NA)))$total, 33)
  expect_equal(extract_token_counts(data.frame(sent = c(10, NA), rec = c(NA, 5)))$total, 0)
  expect_equal(extract_token_counts(list(total = 20))$total, 20)
  expect_equal(extract_token_counts(list(sent = 2, rec = 3, total = 0))$total, 0)
})

test_that("invalid token metadata warns without numeric coercion", {
  for (value in list(factor("3"), "3", c(2, 3), -1, 0.5, Inf, NaN, matrix(1))) {
    expect_warning(u <- extract_token_counts(list(sent = value)), "Token count `sent`")
    expect_equal(u$sent, 0)
    expect_equal(u$sent_unknown, 1)
  }
  u <- extract_token_counts(data.frame(sent = rep(.Machine$integer.max, 2), rec = c(1, 1)))
  expect_equal(u$sent, 2 * as.double(.Machine$integer.max))
  expect_true(is.finite(u$total))
  expect_warning(u <- extract_token_counts(list(sent = .Machine$double.xmax, rec = .Machine$double.xmax)), "Token count `total`")
  expect_equal(u$total_unknown, 1)
})

test_that("message retries keep observed usage and strict component metadata", {
  known <- list(sent = 10, rec = 5, total = 17)
  for (records in list(list(known, list()), list(list(), known), list(list(), list()),
                       list(list(sent = 10, total = 17), list(rec = 5, total = 8)))) {
    records[[1]]$response_text <- "Incomplete..."
    a <- .usage_agent(.usage_runner(records))
    out <- .usage_utterance(a)
    expect_true(is.na(out$meta$sent_tokens)); expect_true(is.na(out$meta$rec_tokens))
    expect_equal(a$tokens_sent_agent, sum(vapply(records, function(x) x$sent %||% 0, 0)))
    expect_equal(a$tokens_received_agent, sum(vapply(records, function(x) x$rec %||% 0, 0)))
    expected <- sum(vapply(records, function(x) x$total %||% 0, 0))
    expect_equal(a$tokens_total_agent, expected)
    expect_equal(out$meta$usage_unknown, if (identical(records[[2]], known) || identical(records[[2]], list()) && length(records[[1]]) > 1) 1 else 2)
    if (all(vapply(records, function(x) !is.null(x$total), TRUE))) {
      expect_equal(out$meta$total_tokens, expected)
    } else expect_true(is.na(out$meta$total_tokens))
  }
})

test_that("returned failed rows retain usage and diagnostic messages", {
  runner <- .usage_runner(list(list(success = FALSE, error_message = "provider rejected", sent = 4, total = 9)))
  a <- .usage_agent(runner)
  expect_error(.usage_utterance(a), "provider rejected", class = "focusgroup_model_error")
  expect_equal(a$tokens_sent_agent, 4); expect_equal(a$tokens_total_agent, 9)
  expect_equal(a$usage_unknown_agent, 1)
  for (runner in list(function(...) "text", function(x, ...) x[FALSE, ],
                     function(x, ...) data.frame(sent = 3))) {
    expect_error(.fg_call_llm(a$config, list(), runner), "experiment rows")
  }
})

test_that("summaries record usage before output validation, including internal calls", {
  g <- focus_group_from_transcript(data.frame(speaker = c("MOD", "P1"), text = c("Hours?", "Evenings.")))
  cfg <- LLMR::llm_config("openai", "example-model")
  runner <- .usage_runner(list(list(sent = 2, rec = 3, total = 9)))
  g$summarize(cfg, internal_call = TRUE, .runner = runner)
  g$summarize(cfg, .runner = runner)
  expect_equal(g$total_tokens, 18)
  expect_error(g$summarize(cfg, .runner = .usage_runner(list(list(response_text = "", total = 7)))), "empty model")
  expect_equal(g$total_tokens, 25); expect_equal(g$calls_usage_unknown, 1)
  expect_equal(g$agents$MOD$tokens_total_agent, 0)
})

test_that("usage printing distinguishes known zero, unknown, and lower bounds", {
  x <- structure(list(metadata = list(topic = "Hours", n_participants = 1L),
    transcript = data.frame(text = "Reply"), usage = list(total = 0, unknown = 0)), class = "focus_group_result")
  expect_output(print(x), "Tokens: 0")
  x$usage$unknown <- 1
  expect_output(print(x), "Tokens: unknown")
  x$usage$total <- 8
  expect_output(print(x), "Tokens: at least 8")
  expect_output(print(x), "incomplete for 1 call")
  x$usage$unknown <- NA_real_
  expect_output(print(x), "undetermined number of calls")
  x$usage$unknown <- NULL
  expect_output(print(x), "Tokens: at least 8")
  expect_output(print(x), "completeness was not recorded")
})

test_that("llmr responses derive missing totals without integer overflow", {
  testthat::local_mocked_bindings(tokens = function(...) list(sent = 3, rec = 4, total = NA), .package = "LLMR")
  expect_equal(extract_token_counts(structure(list(text = "Reply."), class = "llmr_response"))$total, 7)
  large <- as.double(.Machine$integer.max) + 1
  a <- .usage_agent(.usage_runner(list(list(sent = large, rec = 1))))
  out <- .usage_utterance(a)
  expect_equal(out$meta$total_tokens, large + 1)
  tr <- .fg_transcript_data(list(c(list(speaker_id = "P1", text = out$text), out$meta)))
  expect_equal(tr$sent_tokens, large)
  expect_warning(u <- extract_token_counts(data.frame(sent = rep(.Machine$double.xmax, 2))), "numeric range")
  expect_true(is.finite(u$sent)); expect_true(is.na(u$unknown))
})
