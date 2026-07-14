test_that("a scripted runner completes a desire-based session offline", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$prompts <- character()

  scripted_runner <- function(config, messages) {
    calls$n <- calls$n + 1L
    content <- vapply(messages, function(message) {
      if (is.list(message)) as.character(message$content) else as.character(message)
    }, character(1))
    prompt <- paste(content, collapse = "\n")
    calls$prompts <- c(calls$prompts, prompt)

    if (grepl("Desire to talk score", prompt, fixed = TRUE)) return("8")
    if (grepl("Summary:", prompt, fixed = TRUE)) {
      return("The scripted session recorded distinct views and a concrete disagreement.")
    }
    paste(
      "This scripted utterance gives a concrete view of public transit funding",
      "and a reason that another participant can question or extend."
    )
  }

  testthat::local_mocked_bindings(
    call_llm_robust = function(...) stop("live robust call reached"),
    call_llm_broadcast = function(...) stop("live broadcast call reached"),
    .package = "LLMR"
  )

  result <- run_focus_group(
    topic = "public transit funding",
    participants = 2,
    turns_per_phase = list(
      Opening = "Welcome the participants.",
      Engagement = "Which funding priority matters most?",
      Closing = "Thank the participants and close the discussion."
    ),
    conversation_flow = "desire_based",
    runner = scripted_runner,
    max_participant_responses = 1,
    verbose = FALSE
  )

  fg <- result$focus_group
  speakers <- vapply(fg$conversation_log, `[[`, character(1), "speaker_id")
  phases <- vapply(fg$conversation_log, `[[`, character(1), "phase")

  expect_true("MOD" %in% speakers)
  expect_true(any(speakers %in% c("P1", "P2")))
  expect_true(all(c("opening", "engagement_question", "closing") %in% phases))
  expect_identical(fg$turn_taking_flow$last_scoring_mode, "per_agent")
  expect_match(fg$final_summary, "scripted session")
  expect_gt(calls$n, 0L)
  expect_identical(fg$total_tokens_sent, 0)
  expect_identical(fg$total_tokens_received, 0)

  basic <- fg$analyze()
  expect_s3_class(basic$speaker_stats, "tbl_df")
  expect_true(nrow(basic$speaker_stats) >= 3L)

  themes <- fg$analyze_themes()
  expect_type(themes, "character")
  expect_true(nzchar(themes))
  expect_true(all(vapply(fg$agents, function(agent) {
    identical(agent$runner, scripted_runner)
  }, logical(1))))
})

test_that("a runner may return an llmr_response", {
  response <- structure(
    list(
      text = paste(
        "This response is long enough to constitute a complete focus group turn",
        "and it includes a concrete reason for the stated position."
      ),
      response_id = "offline-response",
      duration_s = 0.01
    ),
    class = "llmr_response"
  )
  runner <- function(config, messages) response
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")

  testthat::local_mocked_bindings(
    tokens = function(...) list(sent = 4L, rec = 3L, total = 7L),
    finish_reason = function(...) "stop",
    .package = "LLMR"
  )

  agent <- FGAgent$new(
    id = "P1",
    agent_details = list(direct_persona_description = "A participant."),
    llm_config = cfg,
    runner = runner
  )
  out <- agent$generate_utterance(
    topic = "libraries",
    conversation_history_string = "No prior discussion.",
    utterance_prompt_template = "State your view.",
    conversation_log = list()
  )

  expect_identical(out$meta$response_id, "offline-response")
  expect_identical(out$meta$finish_reason, "stop")
  expect_identical(out$meta$total_tokens, 7L)
  expect_identical(agent$tokens_sent_agent, 4)
  expect_identical(agent$tokens_received_agent, 3)
})

test_that("runner return values are validated", {
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  expect_error(
    FocusGroup:::.fg_call_llm(cfg, "prompt", runner = function(config, messages) 1),
    "character scalar or an `llmr_response`"
  )
})
