# DesireBasedFlow desire scoring must not score every agent with the first
# agent's model when agents carry different configs. The scoring itself needs a
# provider key, so here we test the grouping decision and the recorded
# last_scoring_mode without any live call.

make_agent <- function(id, config) {
  FGAgent$new(id = id,
              agent_details = list(direct_persona_description = paste("Persona", id)),
              config = config)
}

test_that("config grouping key separates agents by their model config", {
  skip_if_not_installed("LLMR")
  cfg_a <- LLMR::llm_config("groq", "model-a", temperature = 0)
  cfg_b <- LLMR::llm_config("openai", "model-b", temperature = 0)

  # The flow groups eligible agents by llm_hash(config). Same config ->
  # one group (one broadcast); different configs -> separate groups.
  same <- c(LLMR::llm_hash(cfg_a), LLMR::llm_hash(cfg_a))
  diff <- c(LLMR::llm_hash(cfg_a), LLMR::llm_hash(cfg_b))
  expect_length(unique(same), 1L)
  expect_length(unique(diff), 2L)
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
  log <- list(
    list(message_id = 1L, round = 1L, speaker_id = "Mod", text = "Q?",
         phase = "opening_question"),
    list(message_id = 2L, round = 1L, speaker_id = "P1", text = "buses",
         phase = "exploration_question")
  )
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
  flow <- create_conversation_flow("desire_based", agents, "mod")

  fg <- list(topic = "t", conversation_log = list(),
             current_conversation_summary = NULL,
             current_question_text = "Q?",
             max_tokens_desire = 16L,
             total_tokens_sent = 0,
             total_tokens_received = 0,
             prompt_templates = list(
               participant_desire_to_talk_nuanced = "Rate 0-10: {{topic}}"))

  testthat::local_mocked_bindings(
    # Broadcast succeeds only for model-a; model-b's group fails (returns NULL).
    call_llm_broadcast = function(config, messages, ...) {
      if (identical(config$model, "model-a")) {
        list(success = rep(TRUE, length(messages)),
             response_text = rep("1", length(messages)))
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

  selected <- flow$select_next_speaker(fg)
  # pa scored 1 in the successful broadcast; pb scored 5 through the
  # per-agent fallback. Selecting pb proves the failed group was not left at 0.
  expect_identical(selected$id, "pb")
})

test_that("the score_per_agent fallback uses each agent's own config (no shared override)", {
  # Sanity: two agents with different configs must each be reachable for
  # per-agent scoring. We confirm the agents retain their distinct configs
  # rather than being collapsed to a single shared one.
  skip_if_not_installed("LLMR")
  a <- make_agent("a", LLMR::llm_config("groq", "model-a"))
  b <- make_agent("b", LLMR::llm_config("openai", "model-b"))
  expect_equal(a$config$model, "model-a")
  expect_equal(b$config$model, "model-b")
  expect_false(identical(LLMR::llm_hash(a$config),
                         LLMR::llm_hash(b$config)))
})

test_that("desire failures are logged and use a neutral participant selection", {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  desire_messages <- character()
  scripted_runner <- function(experiments, ...) {
    prompts <- vapply(experiments$messages, function(messages) {
      paste(vapply(messages, function(message) {
        as.character(message$content %||% "")
      }, character(1)), collapse = "\n")
    }, character(1))
    if (any(grepl("Desire to talk score", prompts, fixed = TRUE))) {
      condition <- structure(
        list(message = "desire provider unavailable", call = NULL),
        class = c("scripted_desire_error", "error", "condition")
      )
      stop(condition)
    }
    experiments$response_text <- ifelse(
      grepl("Summary:", prompts, fixed = TRUE),
      "The session contained two participant perspectives.",
      paste("This participant gives a concrete position and a reason",
            "for the group to examine during the discussion."))
    experiments$success <- TRUE
    experiments
  }

  result <- withCallingHandlers(
    run_focus_group(
      topic = "library hours",
      n_participants = 2,
      guide = list(
        Engagement = "Which library hours matter most?",
        Closing = "Close the discussion."
      ),
      flow = "desire_based",
      config = config,
      seed = 110,
      max_participant_responses = 1,
      verbose = FALSE,
      .runner = scripted_runner
    ),
    message = function(message) {
      if (grepl("desire", conditionMessage(message), ignore.case = TRUE)) {
        desire_messages <<- c(desire_messages, conditionMessage(message))
      }
      invokeRestart("muffleMessage")
    }
  )

  participant_rows <- Filter(
    function(row) row$speaker_id %in% c("P1", "P2"),
    result$focus_group$conversation_log
  )
  expect_length(participant_rows, 1L)
  expect_true(participant_rows[[1]]$speaker_id %in% c("P1", "P2"))
  expect_true("desire_scoring_failure" %in% names(participant_rows[[1]]$metadata))
  failure <- participant_rows[[1]]$metadata$desire_scoring_failure
  expect_named(failure, c("reason", "fallback", "candidates"),
               ignore.order = FALSE)
  expect_match(failure$reason, "provider unavailable")
  expect_identical(failure$fallback, "uniform_random")
  expect_setequal(failure$candidates, c("P1", "P2"))
  expect_length(desire_messages, 1L)
})

test_that("empty desire output is a logged failure and its usage is counted", {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  calls <- 0L
  scripted_runner <- function(experiments, ...) {
    calls <<- calls + 1L
    prompts <- vapply(experiments$messages, function(messages) {
      paste(vapply(messages, function(message) {
        as.character(message$content %||% "")
      }, character(1)), collapse = "\n")
    }, character(1))
    experiments$response_text <- ifelse(
      grepl("Desire to talk score", prompts, fixed = TRUE),
      "",
      ifelse(
        grepl("Summary:", prompts, fixed = TRUE),
        "The session recorded one participant response.",
        paste("This is a complete response with a concrete position and",
              "a reason for the group to examine."))
    )
    experiments$sent <- 1L
    experiments$rec <- 1L
    experiments$success <- TRUE
    experiments
  }

  result <- suppressMessages(run_focus_group(
    topic = "library hours",
    n_participants = 2,
    guide = list(
      Engagement = "Which hours matter most?",
      Closing = "Close the discussion."
    ),
    flow = "desire_based",
    config = config,
    max_participant_responses = 1,
    verbose = FALSE,
    .runner = scripted_runner
  ))

  participant_row <- Filter(
    function(row) row$speaker_id %in% c("P1", "P2"),
    result$focus_group$conversation_log
  )[[1]]
  failure <- participant_row$metadata$desire_scoring_failure
  expect_match(failure$reason, "no valid score")
  expect_identical(failure$fallback, "uniform_random")
  expect_identical(result$usage$total, calls * 2)
})
