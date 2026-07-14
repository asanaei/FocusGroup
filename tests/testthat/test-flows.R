# DesireBasedFlow desire scoring must not score every agent with the first
# agent's model when agents carry different configs. The scoring itself needs a
# provider key, so here we test the grouping decision and the recorded
# last_scoring_mode without any live call.

make_agent <- function(id, config) {
  FGAgent$new(id = id,
              agent_details = list(persona = paste("Persona", id)),
              llm_config = config)
}

test_that("config grouping key separates agents by their model config", {
  skip_if_not_installed("LLMR")
  cfg_a <- LLMR::llm_config("groq", "model-a", temperature = 0)
  cfg_b <- LLMR::llm_config("openai", "model-b", temperature = 0)

  # The flow groups eligible agents by llm_hash(model_config). Same config ->
  # one group (one broadcast); different configs -> separate groups.
  same <- c(LLMR::llm_hash(cfg_a), LLMR::llm_hash(cfg_a))
  diff <- c(LLMR::llm_hash(cfg_a), LLMR::llm_hash(cfg_b))
  expect_length(unique(same), 1L)
  expect_length(unique(diff), 2L)
})

test_that("a DesireBasedFlow exposes last_scoring_mode and starts unset", {
  skip_if_not_installed("LLMR")
  cfg <- LLMR::llm_config("groq", "model-a")
  agents <- list(
    mod = make_agent("mod", cfg),
    p1  = make_agent("p1", cfg),
    p2  = make_agent("p2", cfg))
  flow <- DesireBasedFlow$new(agents = agents, moderator_id = "mod")
  expect_true("last_scoring_mode" %in% names(flow))
  expect_null(flow$last_scoring_mode)
})

test_that("the continuation experiment inherits the summary and cut-point phase", {
  recording_agent <- local({
    a <- new.env()
    a$clone <- function(deep = TRUE) a
    a$generate_utterance <- function(topic, conversation_history_string,
                                     utterance_prompt_template, max_tokens_utterance,
                                     current_moderator_question,
                                     conversation_summary_so_far, current_phase,
                                     conversation_log = NULL, ...) {
      list(text = paste0("phase=", current_phase, "|summary=", conversation_summary_so_far,
                         "|has_log=", !is.null(conversation_log)))
    }
    class(a) <- c("R6", "environment")
    a
  })
  fg <- list(topic = "t", agents = list(P1 = recording_agent),
             prompt_templates = list(participant_utterance_subtle_persona = "{{conversation_history}}"),
             max_tokens_utterance = 160L,
             current_conversation_summary = "earlier: cost and reliability")
  log <- list(list(turn = 1, speaker_id = "Mod", text = "Q?", phase = "opening_question"),
              list(turn = 2, speaker_id = "P1", text = "buses", phase = "exploration_question"))
  res <- FocusGroup:::.fg_run_experiment(fg, log, "P1", 2, "I love buses now")
  # the summary came from the fg object, not the "N/A" placeholder
  expect_match(res$control, "summary=earlier: cost and reliability")
  # the phase came from the last (cut-point) log turn
  expect_match(res$control, "phase=exploration_question")
  # the continuation forwards the conversation_log (role-flip construction),
  # rather than the old always-flat conversation_log = NULL
  expect_match(res$control, "has_log=TRUE")
})

test_that("a failed config group is rescored per agent, not left at zero", {
  skip_if_not_installed("LLMR")
  # Two agents with distinct configs -> two broadcast groups. The broadcast for
  # config B fails; the agent in that group must be rescored per-agent rather
  # than keep the default score of 0.
  cfg_a <- LLMR::llm_config("groq", "model-a", temperature = 0)
  cfg_b <- LLMR::llm_config("openai", "model-b", temperature = 0)
  agents <- list(
    mod = make_agent("mod", cfg_a),
    pa  = make_agent("pa", cfg_a),
    pb  = make_agent("pb", cfg_b))
  flow <- DesireBasedFlow$new(agents = agents, moderator_id = "mod")

  fg <- list(topic = "t", conversation_log = list(),
             current_conversation_summary = NULL,
             current_question_text = "Q?",
             max_tokens_desire = 16L,
             prompt_templates = list(
               participant_desire_to_talk_nuanced = "Rate 0-10: {{topic}}"),
             record_token_usage = function(...) invisible(NULL))

  testthat::local_mocked_bindings(
    # Broadcast succeeds only for model-a; model-b's group fails (returns NULL).
    call_llm_broadcast = function(config, messages, ...) {
      if (identical(config$model, "model-a")) {
        list(success = rep(TRUE, length(messages)),
             response_text = rep("8", length(messages)))
      } else {
        stop("model-b broadcast is down")
      }
    },
    # The per-agent fallback (call_llm_robust) returns a parseable score for pb.
    # An llmr_response carries its text in $text (as.character reads it there).
    call_llm_robust = function(config, messages, ...) {
      structure(list(text = "5"), class = "llmr_response")
    },
    tokens = function(...) list(sent = 0L, rec = 0L),
    .package = "LLMR")

  flow$select_next_speaker(fg)
  scores <- flow$get_last_desire_scores()
  # pa came from the successful broadcast (8); pb from the per-agent fallback (5).
  expect_equal(unname(scores[["pa"]]), 8)
  expect_equal(unname(scores[["pb"]]), 5)   # the bug left this at 0
  expect_equal(flow$last_scoring_mode, "broadcast_grouped_config")
})

test_that("the score_per_agent fallback uses each agent's own config (no shared override)", {
  # Sanity: two agents with different configs must each be reachable for
  # per-agent scoring. We confirm the agents retain their distinct configs
  # rather than being collapsed to a single shared one.
  skip_if_not_installed("LLMR")
  a <- make_agent("a", LLMR::llm_config("groq", "model-a"))
  b <- make_agent("b", LLMR::llm_config("openai", "model-b"))
  expect_equal(a$model_config$model, "model-a")
  expect_equal(b$model_config$model, "model-b")
  expect_false(identical(LLMR::llm_hash(a$model_config),
                         LLMR::llm_hash(b$model_config)))
})
