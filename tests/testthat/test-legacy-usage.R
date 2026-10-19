test_that("serialized locked legacy bindings remain readable and collect new analysis calls", {
  legacy_method <- function(config, .runner = NULL) {
    out <- .fg_call_llm(config, list(list(role = "user", content = "Analyze.")), .runner)
    usage <- extract_token_counts(out)
    .fg_add_group_usage(self, usage$sent, usage$rec)
    .fg_response_text(out)
  }
  legacy <- R6::R6Class("FocusGroup", public = list(topic = "Hours", purpose = "Study",
    agents = NULL, moderator_id = "MOD", conversation_log = NULL, final_summary = NULL,
    total_tokens_sent = 13, total_tokens_received = 4,
    analyze_themes = legacy_method, summarize = legacy_method,
    analyze = function() list(speaker_stats = data.frame(speaker_id = "P1", utterance_count = 1L), full_transcript = "P1: Evening.")))$new()
  imported <- focus_group_from_transcript(data.frame(speaker = c("MOD", "P1"), text = c("Hours?", "Evenings.")))
  legacy$agents <- imported$agents; legacy$conversation_log <- imported$conversation_log
  legacy <- unserialize(serialize(legacy, NULL))
  expect_true(environmentIsLocked(legacy))
  expect_false("total_tokens" %in% names(legacy))
  result <- .fg_focus_group_result(legacy, "round_robin", "flat", 1, 3)
  expect_true(is.na(result$usage$total)); expect_true(is.na(result$usage$unknown))
  expect_output(print(result), "Tokens: unknown")
  expect_output(print(result), "undetermined number of calls")
  expect_false(any(grepl("Status", capture.output(print(legacy)))))
  analysis <- analyze_focus_group(legacy, include_plots = FALSE,
    config = LLMR::llm_config("openai", "example-model"),
    .runner = .usage_runner(list(list(sent = 2, rec = 3, total = 9))))
  expect_named(analysis, c("basic_stats", "topics", "tfidf", "readability", "themes", "model_summary", "plots", "issues"))
  expect_equal(legacy$total_tokens_sent, 17)
  expect_equal(attr(legacy, "recorded_usage")$total, 18)
  expect_false("calls_usage_unknown" %in% names(legacy))
})

test_that("legacy continuation agents are reconstructed without changing the saved object", {
  legacy <- R6::R6Class("FGAgent", public = list(id = "P1", config = NULL, .runner = NULL,
    persona_description = "A participant.", communication_style_instruction = "Plain speech.",
    demographics = list(age = 40), survey_responses = NULL, is_moderator = FALSE,
    tokens_sent_agent = 11, tokens_received_agent = 12, history = list(),
    generate_utterance = function(...) stop("historical method")))$new()
  legacy <- unserialize(serialize(legacy, NULL))
  expect_true(environmentIsLocked(legacy))
  out <- .fg_generate_once(legacy, "Hours", "", "State your view.", config = LLMR::llm_config("openai", "example-model"),
    .runner = .usage_runner(list(list(sent = 2, rec = 3, total = 9))))
  expect_equal(out$meta$total_tokens, 9)
  # The rebuilt agent sends the saved persona and style unchanged.
  seen <- NULL
  capture <- function(experiments, ...) {
    seen <<- experiments$messages[[1]]
    .usage_runner(list(list(sent = 2, rec = 3, total = 9)))(experiments, ...)
  }
  .fg_generate_once(legacy, "Hours", "",
    "{{persona_description}} | {{communication_style_instruction}} | State your view.",
    config = LLMR::llm_config("openai", "example-model"), .runner = capture)
  sent <- paste(vapply(seen, function(m) m$content, ""), collapse = "\n")
  expect_match(sent, "A participant.", fixed = TRUE)
  expect_identical(lengths(regmatches(sent, gregexpr("Plain speech.", sent, fixed = TRUE))), 1L)
  expect_false(grepl("Your communication style is", sent, fixed = TRUE))
  expect_equal(legacy$tokens_sent_agent, 11)
  expect_false("tokens_total_agent" %in% names(legacy))
  expect_error(legacy$generate_utterance(), "historical method")
})
