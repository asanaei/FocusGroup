# Reasoning models: the score is read from the final answer only, a failed
# desire score stops the session unless the caller chooses "random", and the
# session's reasoning-effort setting and token limits reach every call type.

.prompt_of <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

.is_desire <- function(prompt) grepl("Desire to talk score", prompt, fixed = TRUE)

.desire_agent <- function(runner, config = LLMR::llm_config("openai", "example-model")) {
  FGAgent$new(id = "P1",
              agent_details = list(direct_persona_description = "A participant."),
              config = config, .runner = runner)
}

.score <- function(agent, ...) {
  agent$get_need_to_talk(
    topic = "library hours", conversation_history_string = "",
    desire_prompt_template = "Return one score from 0 to 10.",
    conversation_log = list(), ...)
}

test_that("a desire score is read from the final answer, not the reasoning", {
  replies <- list(
    list(response_text = "<think>At first 9 out of 10, then 2/10.</think>\n6"),
    list(response_text = "<thinking>I could say 10.</thinking> My score: 4"),
    list(response_text = paste0("<|channel|>analysis<|message|>Maybe 9/10.",
                                "<|end|><|start|>assistant<|channel|>final",
                                "<|message|>3<|return|>")),
    list(response_text = "5", thinking = "Reasoning that says 9/10.")
  )
  expected <- c(6, 4, 3, 5)
  for (i in seq_along(replies)) {
    runner <- function(experiments, ...) {
      for (name in names(replies[[i]])) experiments[[name]] <- replies[[i]][[name]]
      experiments
    }
    expect_identical(.score(.desire_agent(runner)), expected[[i]])
  }

  # An llmr_response keeps reasoning in its own field, which is never read.
  testthat::local_mocked_bindings(
    call_llm_robust = function(config, messages, ...) {
      structure(list(text = "7", thinking = "I would say 10/10."),
                class = "llmr_response")
    },
    tokens = function(...) list(sent = 1L, rec = 1L, total = 2L),
    .package = "LLMR")
  expect_identical(.score(.desire_agent(NULL)), 7)
})

test_that("reasoning that never closes is no answer, and the retry is tried", {
  calls <- 0L
  seen <- list()
  runner <- function(experiments, ...) {
    calls <<- calls + 1L
    seen[[calls]] <<- experiments$config[[1]]$model_params$max_tokens
    experiments$response_text <- if (calls == 1L) "<think>Weighing 9/10 against" else "8"
    experiments$finish_reason <- if (calls == 1L) "length" else "stop"
    experiments
  }
  expect_identical(.score(.desire_agent(runner), max_tokens_desire = 64L,
                          max_tokens_retry = 2048L), 8)
  expect_identical(unlist(seen), c(64L, 2048L))

  # With no retry budget above the first, the reasoning is not read as a score.
  calls <- 0L
  expect_error(.score(.desire_agent(runner), max_tokens_desire = 64L,
                      max_tokens_retry = 64L), "no valid score")
})

test_that("reasoning written into an utterance stays out of the transcript", {
  runner <- function(experiments, ...) {
    experiments$response_text <- paste(
      "<think>The persona would mention evenings.</think>",
      "Evening hours matter because I work until six on weekdays.")
    experiments
  }
  agent <- .desire_agent(runner)
  out <- agent$generate_utterance("library hours", "", "State your view.",
                                  conversation_log = list())
  expect_identical(out$text, "Evening hours matter because I work until six on weekdays.")
})

test_that("the session's choice follows the final-answer scores", {
  # P1's reasoning mentions 9/10 but it answers 2; P2 answers 8.
  reply <- function(system) {
    if (grepl("Persona one", system)) "<think>Perhaps 9/10.</think> 2"
    else if (grepl("Persona two", system)) "8" else "1"
  }
  runner <- function(experiments, ...) {
    prompt <- .prompt_of(experiments$messages[[1]])
    experiments$response_text <- if (.is_desire(prompt)) {
      reply(experiments$messages[[1]][[1]]$content)
    } else if (grepl("Summary:$", prompt)) "A short summary." else
      "Evening hours matter because many people work during the day."
    experiments
  }
  agents <- create_agents(3, config = LLMR::llm_config("openai", "example-model"),
                          direct_persona_descriptions = c("Persona one.", "Persona two.",
                                                          "Persona three."),
                          .runner = runner)
  fg <- FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents,
    moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow("desire_based", agents, "MOD"),
    question_script = list(list(phase = "engagement_question", text = "Which hours?")),
    max_participant_responses = 1, call_on_silent = "none")
  fg$run_simulation()
  speakers <- vapply(fg$conversation_log, `[[`, character(1), "speaker_id")
  expect_identical(speakers, c("System", "MOD", "P2"))
})

test_that("a desire score that cannot be read stops the session by default", {
  config <- LLMR::llm_config("openai", "example-model")
  runner <- function(experiments, ...) {
    prompt <- .prompt_of(experiments$messages[[1]])
    experiments$response_text <- if (.is_desire(prompt)) {
      "<think>I would rather not give a number.</think> I am not sure."
    } else if (grepl("Summary:$", prompt)) "A short summary." else
      "Evening hours matter because many people work during the day."
    experiments
  }
  run <- function(...) run_focus_group(
    topic = "library hours", config = config, n_participants = 2,
    guide = list(Engagement = "Which hours matter most?", Closing = "Close."),
    flow = "desire_based", max_participant_responses = 1, verbose = FALSE,
    .runner = runner, ...)
  e <- tryCatch(run(), focusgroup_incomplete_session = identity)
  expect_s3_class(e, "focusgroup_incomplete_session")
  expect_s3_class(e$parent, "focusgroup_desire_failure")
  expect_identical(e$parent$agent_id, "P1")
  expect_match(conditionMessage(e$parent), "Desire scoring failed for participant P1")
  expect_false(e$partial$metadata$complete)
  expect_false(any(e$partial$transcript$speaker_id %in% c("P1", "P2")))

  expect_error(run(on_desire_failure = "sometimes"), "should be one of")
  expect_error(create_conversation_flow(
    "desire_based", create_agents(1, config = config), "MOD",
    flow_params = list(on_desire_failure = "skip")), "on_desire_failure")
})

test_that("under \"random\" a failed score leaves the valid scores in charge", {
  reply <- function(system) {
    if (grepl("Persona one", system)) "no number here"
    else if (grepl("Persona two", system)) "2" else "9"
  }
  runner <- function(experiments, ...) {
    prompt <- .prompt_of(experiments$messages[[1]])
    experiments$response_text <- if (.is_desire(prompt)) {
      reply(experiments$messages[[1]][[1]]$content)
    } else if (grepl("Summary:$", prompt)) "A short summary." else
      "Evening hours matter because many people work during the day."
    experiments
  }
  agents <- create_agents(3, config = LLMR::llm_config("openai", "example-model"),
                          direct_persona_descriptions = c("Persona one.", "Persona two.",
                                                          "Persona three."),
                          .runner = runner)
  for (i in 1:5) {
    flow <- create_conversation_flow("desire_based", agents, "MOD",
                                     flow_params = list(on_desire_failure = "random"))
    fg <- FocusGroup$new(
      topic = "library hours", purpose = "Test.", agents = agents,
      moderator_id = "MOD", turn_taking_flow = flow,
      question_script = list(list(phase = "engagement_question", text = "Which hours?")),
      max_participant_responses = 1)
    suppressMessages(fg$run_simulation())
    row <- fg$conversation_log[[3]]
    expect_identical(row$speaker_id, "P3")
    failure <- row$metadata$desire_scoring_failure
    expect_identical(failure$fallback, "valid_scores")
    expect_identical(failure$failed, "P1")
  }
})

test_that("reasoning effort reaches every call type", {
  seen <- list()
  runner <- function(experiments, ...) {
    prompt <- .prompt_of(experiments$messages[[1]])
    type <- if (.is_desire(prompt)) "desire" else if (grepl("Summary:$", prompt)) "summary" else
      if (grepl("MODERATOR|moderator of a focus group", prompt)) "moderator" else "participant"
    seen[[length(seen) + 1L]] <<- list(
      type = type, effort = experiments$config[[1]]$model_params$reasoning_effort)
    experiments$response_text <- if (type == "desire") "6" else if (type == "summary")
      "A short summary." else "Evening hours matter because many people work during the day."
    experiments
  }
  run <- function(...) run_focus_group(
    topic = "library hours",
    config = LLMR::llm_config("openai", "example-model", reasoning_effort = "medium"),
    n_participants = 2,
    guide = list(Engagement = "Which hours matter most?", Closing = "Close."),
    flow = "desire_based", max_participant_responses = 1, verbose = FALSE,
    .runner = runner, ...)

  run(reasoning_effort = "low")
  types <- vapply(seen, `[[`, character(1), "type")
  expect_setequal(types, c("moderator", "participant", "desire", "summary"))
  expect_true(all(vapply(seen, function(x) identical(x$effort, "low"), logical(1))))

  seen <- list()
  run(reasoning_effort = c(desire = "minimal", participant = "high"))
  effort <- vapply(seen, `[[`, character(1), "effort")
  types <- vapply(seen, `[[`, character(1), "type")
  expect_true(all(effort[types == "desire"] == "minimal"))
  expect_true(all(effort[types == "participant"] == "high"))
  expect_true(all(effort[types %in% c("moderator", "summary")] == "medium"))

  seen <- list()
  run()
  expect_true(all(vapply(seen, function(x) identical(x$effort, "medium"), logical(1))))

  expect_error(run(reasoning_effort = c(talking = "low")), "call types")
  expect_error(run(reasoning_effort = c("low", "high")), "one string")
  expect_error(run(reasoning_effort = NA_character_), "reasoning_effort")
})

test_that("session token limits reach the desire retry and the summaries", {
  seen <- list()
  desire_calls <- 0L
  runner <- function(experiments, ...) {
    prompt <- .prompt_of(experiments$messages[[1]])
    cap <- experiments$config[[1]]$model_params$max_tokens
    if (.is_desire(prompt)) {
      desire_calls <<- desire_calls + 1L
      seen$desire <<- c(seen$desire, cap)
      experiments$response_text <- if (desire_calls %% 2L == 1L) "" else "6"
      experiments$finish_reason <- if (desire_calls %% 2L == 1L) "length" else "stop"
    } else if (grepl("Summary:$", prompt)) {
      seen$summary <<- c(seen$summary, cap)
      experiments$response_text <- "A short summary."
    } else {
      experiments$response_text <- "Evening hours matter because many people work during the day."
    }
    experiments
  }
  run <- function(...) run_focus_group(
    topic = "library hours", config = LLMR::llm_config("openai", "example-model"),
    n_participants = 2,
    guide = list(Engagement = "Which hours matter most?", Closing = "Close."),
    flow = "desire_based", max_participant_responses = 1, verbose = FALSE,
    .runner = runner, ...)
  run(max_tokens_config = list(desire = 1500, desire_retry = 6000, summary = 3000))
  expect_identical(as.numeric(seen$desire), c(1500, 6000, 1500, 6000))
  expect_identical(as.numeric(seen$summary), 3000)

  expect_error(run(max_tokens_config = list(desier = 50)), "Unknown `max_tokens_config`")
  expect_error(run(max_tokens_config = list(desire = 0)), "positive whole number")
  expect_error(run(max_tokens_config = c(desire = 50)), "named list")
})
