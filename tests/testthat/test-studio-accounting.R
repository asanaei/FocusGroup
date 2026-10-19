test_that("action recorders count returned calls independently of response rows", {
  recorder <- .fg_usage_recorder(.usage_runner(list(
    list(response_text = "Incomplete...", sent = 10, rec = 5, total = 17),
    list(response_text = "Complete reply."),
    list(response_text = "Incomplete...", total = 9),
    list(response_text = "Complete reply.", sent = 2, rec = 3))), safe = TRUE)
  g <- focus_group_from_transcript(data.frame(speaker = c("MOD", "P1"), text = c("Hours?", "Evenings.")))
  res <- .fg_run_experiment_many(g, g$conversation_log, "P1", 2,
    arms = data.frame(label = "Morning", text = "Mornings."), runs_per_arm = 1,
    config = LLMR::llm_config("openai", "example-model"), .runner = recorder$runner)
  expect_equal(nrow(res), 2)
  expect_true(all(is.na(res$sent_tokens)))
  expect_equal(recorder$value()$calls, 4)
  expect_equal(recorder$value()$sent, 12); expect_equal(recorder$value()$rec, 8)
  expect_equal(recorder$value()$total, 31); expect_equal(recorder$value()$unknown, 2)
  added <- NULL
  .fg_account_action(recorder, list(add_usage = function(x) added <<- x), function(x) NULL)
  expect_named(added, c("calls", "sent", "received", "total", "unknown",
                        "sent_unknown", "received_unknown"))
  expect_equal(added$calls, 4); expect_equal(added$total, 31)
  expect_equal(added$sent_unknown, 2); expect_equal(added$received_unknown, 2)
  zero <- .fg_usage_recorder(function(...) stop("failure"), safe = TRUE)
  expect_error(zero$runner(data.frame()), "failure")
  expect_equal(zero$value()$calls, 0); expect_true(is.na(zero$value()$unknown))
  expect_true(is.na(zero$value()$sent_unknown)); expect_true(is.na(zero$value()$rec_unknown))
})

test_that("action accounting survives errors and promoted warnings", {
  for (callback in list(function(x) stop("callback failed"), function(x) warning("callback warning"))) {
    recorder <- .fg_usage_recorder(.usage_runner(list(list(total = 7))), safe = TRUE)
    recorder$runner(data.frame(.experiment_id = 1L))
    marked <- FALSE; notices <- character()
    withr::local_options(list(warn = 2))
    expect_false(.fg_account_action(recorder, list(add_usage = callback),
      function(x) marked <<- x, function(x) notices <<- c(notices, x)))
    expect_true(marked); expect_length(notices, 1)
    expect_match(notices, "Generated results remain available")
  }
  recorder <- .fg_usage_recorder(.usage_runner(list(list(sent = "bad"))), safe = TRUE)
  expect_no_error(out <- recorder$runner(data.frame(.experiment_id = 1L)))
  expect_identical(out$response_text, "A complete reply.")
  expect_match(recorder$issues(), "Token count")
})

test_that("analysis action recorders include calls preceding failure and exclude history", {
  g <- focus_group_from_transcript(data.frame(speaker = c("MOD", "P1"), text = c("Hours?", "Evenings.")))
  g$total_tokens_sent <- 1000; g$total_tokens <- 2000
  fail <- structure(list(message = "summary failed", call = NULL), class = c("error", "condition"))
  recorder <- .fg_usage_recorder(.usage_runner(list(list(sent = 2, rec = 3, total = 9), fail)))
  expect_error(analyze_focus_group(g, include_plots = FALSE,
    config = LLMR::llm_config("openai", "example-model"), .runner = recorder$runner), "summary failed")
  expect_equal(recorder$value()$calls, 1); expect_equal(recorder$value()$total, 9)
  expect_equal(g$total_tokens, 2009)
  expect_true(is.na(recorder$value()$unknown))
})

test_that("Studio retains partial runs and accounts once after accounting failure", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  notifications <- character(); attempts <- 0L
  testthat::local_mocked_bindings(build_runner = function(...) .session_failure(2), .package = "LLMR.shiny")
  testthat::local_mocked_bindings(showNotification = function(ui, ...) {
    notifications <<- c(notifications, as.character(ui)); "notice"
  }, .package = "shiny")
  shared <- fake_shared("live", TRUE)
  shared$add_usage <- function(x) { attempts <<- attempts + 1L; warning("fixture accounting warning") }
  withr::local_options(list(warn = 2))
  shiny::testServer(.fg_run_server, args = list(shared = shared), {
    session$setInputs(topic = "Hours", n_participants = 2L, flow = "round_robin", max_resp = 1,
      guide_opening_included = FALSE, guide_icebreaker_included = FALSE,
      guide_engagement_included = TRUE, guide_engagement = "Which hours?",
      guide_exploration_included = FALSE, guide_closing_included = FALSE)
    session$setInputs(run = 1L)
    expect_false(result()$metadata$complete)
    expect_match(as.character(err()), "Partial session")
    expect_true(accounting_incomplete())
    expect_equal(nrow(result()$transcript), 2)
    expect_equal(attempts, 1)
    expect_match(paste(notifications, collapse = " "), "Generated results remain available")
    session$setInputs(topic = "New topic")
    expect_false(is.null(result()))
  })
})

test_that("Studio import warnings accompany the canonical table and mapping", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  shared <- fake_shared("demo", FALSE, active_tab = "analyze")
  notices <- character()
  testthat::local_mocked_bindings(showNotification = function(ui, ...) {
    notices <<- c(notices, as.character(ui)); "notice"
  }, .package = "shiny")
  shiny::testServer(.fg_analyze_server, args = list(shared = shared), {
    original <- data.frame(who = c("MOD", "", "P1"), said = c("Question?", "Drop", NA))
    raw(original)
    session$setInputs(speaker_col = "who", text_col = "said", include_plots = FALSE, analyze = 1L)
    expect_equal(nrow(transcript()), 2)
    expect_identical(transcript()$text, c("Question?", ""))
    expect_identical(raw(), original)
    expect_identical(analysis_settings()$text_col, "text")
    expect_true(any(grepl("Dropped 1", notices)))
    expect_true(any(grepl("1 utterance", notices)))
  })
})

test_that("Studio successful results survive a failing accounting callback", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  notices <- character()
  testthat::local_mocked_bindings(build_runner = function(...) .usage_runner(list(list(sent = 2, rec = 3, total = 9))), .package = "LLMR.shiny")
  testthat::local_mocked_bindings(showNotification = function(ui, ...) {
    notices <<- c(notices, as.character(ui)); "notice"
  }, .package = "shiny")
  shared <- fake_shared("live", TRUE)
  shared$add_usage <- function(x) stop("fixture accounting failure")
  shiny::testServer(.fg_run_server, args = list(shared = shared), {
    session$setInputs(topic = "Hours", n_participants = 2L, flow = "round_robin", max_resp = 1,
      guide_opening_included = FALSE, guide_icebreaker_included = FALSE,
      guide_engagement_included = TRUE, guide_engagement = "Which hours?",
      guide_exploration_included = FALSE, guide_closing_included = FALSE)
    session$setInputs(run = 1L)
    expect_true(result()$metadata$complete)
    expect_true(accounting_incomplete())
    expect_match(paste(notices, collapse = " "), "fixture accounting failure")
    expect_s3_class(.fg_sanitize_session(result()$focus_group), "FocusGroup")
    session$setInputs(topic = "New topic", run = 2L)
    expect_true(result()$metadata$complete)
  })
})

test_that("both Studio session paths retain first-call failures with zero returned calls", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  testthat::local_mocked_bindings(build_runner = function(...) .session_failure(1), .package = "LLMR.shiny")
  for (source in c("synthetic", "anes")) {
    shared <- fake_shared("live", TRUE)
    additions <- list()
    shared$add_usage <- function(x) additions[[length(additions) + 1L]] <<- x
    shiny::testServer(.fg_run_server, args = list(shared = shared), {
      session$setInputs(source = source, topic = "Hours", n_participants = 2L, flow = "round_robin", max_resp = 1,
        guide_opening_included = FALSE, guide_icebreaker_included = FALSE,
        guide_engagement_included = TRUE, guide_engagement = "Which hours?",
        guide_exploration_included = FALSE, guide_closing_included = FALSE)
      session$setInputs(run = 1L)
      expect_false(result()$metadata$complete)
      expect_equal(nrow(result()$transcript), 1)
      expect_length(additions, 1)
      expect_equal(additions[[1]]$calls, 0)
      expect_true(is.na(additions[[1]]$unknown))
      expect_match(as.character(err()), "partial result is available")
    })
  }
})

test_that("Studio repeated model analysis accounts only each action before failure", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  testthat::local_mocked_bindings(build_runner = function(...) .session_failure(2), .package = "LLMR.shiny")
  shared <- fake_shared("live", TRUE, active_tab = "analyze")
  additions <- list(); shared$add_usage <- function(x) additions[[length(additions) + 1L]] <<- x
  g <- focus_group_from_transcript(data.frame(speaker = c("MOD", "P1"), text = c("Hours?", "Evenings.")))
  g$total_tokens <- 1000
  shiny::testServer(.fg_analyze_server, args = list(shared = shared), {
    raw(g)
    session$setInputs(include_plots = FALSE, analyze = 1L)
    before <- analysis()
    session$setInputs(model_analyze = 1L)
    expect_identical(analysis(), before)
    session$setInputs(model_analyze = 2L)
    expect_length(additions, 2)
    expect_equal(vapply(additions, function(x) x$calls, 0), c(1, 1))
    expect_equal(vapply(additions, function(x) x$total, 0), c(9, 9))
    expect_equal(focus_group()$total_tokens, 1018)
    expect_false(is.null(err()))
  })
})

test_that("Studio continuation usage includes retries separately from response rows", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  records <- list(list(response_text = "Incomplete...", sent = 10, rec = 5, total = 17),
    list(response_text = "Complete reply."), list(response_text = "Incomplete...", total = 9),
    list(response_text = "Complete reply.", sent = 2, rec = 3))
  testthat::local_mocked_bindings(build_runner = function(...) .usage_runner(records), .package = "LLMR.shiny")
  shared <- fake_shared("live", TRUE, active_tab = "experiment")
  g <- focus_group_from_transcript(data.frame(speaker = c("MOD", "P1"), text = c("Hours?", "Evenings.")))
  shiny::testServer(.fg_experiment_server, args = list(shared = shared), {
    fg(g)
    session$setInputs(cut = 2L, perturb_message_id = 2L, arm_1_enabled = TRUE,
      arm_1_label = "Morning", arm_1_text = "Mornings.", arm_2_enabled = FALSE,
      arm_3_enabled = FALSE, runs_per_arm = 1L, speaker = "P1", question = "Hours?")
    session$setInputs(run = 1L)
    expect_equal(nrow(result()), 2)
    expect_equal(shared$.usage$last$calls, 4)
    expect_equal(shared$.usage$last$total, 31)
    expect_equal(shared$.usage$last$unknown, 2)
    expect_true(all(is.na(result()$sent_tokens)))
  })
})

test_that("recorder extraction failures keep the content and record the call as unknown", {
  recorder <- .fg_usage_recorder(.usage_runner(list(list())), safe = TRUE)
  testthat::local_mocked_bindings(.fg_usage_record = function(...) stop("fixture extraction failure"))
  out <- .fg_call_llm(LLMR::llm_config("openai", "example-model"), list(), recorder$runner)
  expect_identical(out$response_text, "A complete reply.")
  expect_equal(recorder$value()$calls, 1)
  marked <- FALSE; notice <- NULL; added <- NULL
  expect_false(.fg_account_action(recorder, list(add_usage = function(x) added <<- x),
    function(x) marked <<- x, function(x) notice <<- x))
  expect_equal(added$calls, 1); expect_true(is.na(added$unknown))
  expect_true(marked); expect_match(notice, "recorded, but it may be incomplete")
  expect_match(notice, "fixture extraction failure")
})

# The Studio's tile is drawn by LLMR.shiny; these tests pass FocusGroup's
# records through the real usage_add() and usage_tile().
.tile_shared <- function() {
  state <- LLMR.shiny::usage_empty()
  list(add_usage = function(tokens) state <<- LLMR.shiny::usage_add(state, tokens),
       state = function() state,
       tile = function() as.character(LLMR.shiny::usage_tile(state)))
}

test_that("the usage tile shows a lower bound when a call reported no usage", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  recorder <- .fg_usage_recorder(.usage_runner(list(
    list(sent = 10, rec = 5), list())), safe = TRUE)
  for (i in 1:2) recorder$runner(data.frame(.experiment_id = 1L))
  shared <- .tile_shared(); notices <- character()
  expect_no_warning(expect_true(.fg_account_action(recorder, shared, function(x) NULL,
    function(x) notices <<- c(notices, x))))
  expect_length(notices, 0)
  state <- shared$state()
  expect_identical(state$calls, 2L); expect_identical(state$unknown, 1L)
  expect_identical(state$sent_unknown, 1L); expect_identical(state$received_unknown, 1L)
  expect_match(shared$tile(), "Sent at least 10 / Received at least 5 tokens", fixed = TRUE)
  expect_match(shared$tile(), "Usage was not reported for 1 reply.", fixed = TRUE)

  complete <- .fg_usage_recorder(.usage_runner(list(list(sent = 10, rec = 5))), safe = TRUE)
  complete$runner(data.frame(.experiment_id = 1L))
  exact <- .tile_shared()
  expect_true(.fg_account_action(complete, exact, function(x) NULL, function(x) stop("no notice")))
  expect_match(exact$tile(), "Sent 10 / Received 5 tokens", fixed = TRUE)
  expect_false(grepl("not reported", exact$tile(), fixed = TRUE))
})

test_that("calls made before a runner error are recorded with completeness unknown", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  recorder <- .fg_usage_recorder(.session_failure(3), safe = TRUE)
  recorder$runner(data.frame(.experiment_id = 1L))
  recorder$runner(data.frame(.experiment_id = 1L))
  expect_error(recorder$runner(data.frame(.experiment_id = 1L)), "fixture provider failure")
  shared <- .tile_shared(); marked <- NA; notices <- character()
  withr::local_options(list(warn = 2))
  expect_no_error(.fg_account_action(recorder, shared, function(x) marked <<- x,
                                     function(x) notices <<- c(notices, x)))
  expect_false(marked); expect_length(notices, 0)
  state <- shared$state()
  expect_identical(state$calls, 2L)
  expect_identical(state$sent, 4); expect_identical(state$received, 6)
  expect_true(is.na(state$unknown)); expect_true(is.na(state$sent_unknown))
  expect_match(shared$tile(), "2 calls", fixed = TRUE)
  expect_match(shared$tile(), "Sent at least 4 / Received at least 6 tokens", fixed = TRUE)
  expect_match(shared$tile(), "Usage was not reported for some replies.", fixed = TRUE)
})

test_that("Studio persona runs return partial sessions with provenance", {
  e <- tryCatch(.fg_run_from_personas("Hours", 2, c(3, 1), "round_robin", "flat", 110L, 1L,
    LLMR::llm_config("openai", "example-model"), data = data.frame(id = 11:13, age = 30:32),
    persona_cols = "age", guide = list(Engagement = "Hours?"), .runner = .session_failure(2)),
    focusgroup_incomplete_session = identity)
  expect_s3_class(e, "focusgroup_incomplete_session")
  expect_identical(attr(e$partial$focus_group$agents, "source_rows"), c(P1 = 3L, P2 = 1L))
  expect_identical(attr(e$partial$focus_group$agents, "source_ids")$id, c(13L, 11L))
  clean <- .fg_sanitize_session(e$partial$focus_group)
  expect_identical(attr(clean$agents, "source_ids")$id, c(13L, 11L))
})
