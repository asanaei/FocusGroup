test_that("a scripted experiments runner completes a desire-based session offline", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  calls$prompts <- character()
  calls$frames <- list()

  scripted_runner <- function(experiments, ...) {
    calls$n <- calls$n + 1L
    calls$frames[[calls$n]] <- experiments
    stopifnot(is.data.frame(experiments))
    stopifnot(is.list(experiments$config), is.list(experiments$messages))

    prompts <- vapply(experiments$messages, function(messages) {
      content <- vapply(messages, function(message) {
        if (is.list(message)) as.character(message$content) else as.character(message)
      }, character(1))
      paste(content, collapse = "\n")
    }, character(1))
    calls$prompts <- c(calls$prompts, prompts)

    experiments$response_text <- vapply(prompts, function(prompt) {
      if (grepl("Desire to talk score", prompt, fixed = TRUE)) return("8")
      if (grepl("Summary:", prompt, fixed = TRUE)) {
        return("The scripted session recorded distinct views and a concrete disagreement.")
      }
      paste(
        "This scripted utterance gives a concrete view of public transit funding",
        "and a reason that another participant can question or extend."
      )
    }, character(1))
    experiments$success <- TRUE
    experiments$sent <- 0L
    experiments$rec <- 0L
    experiments$total <- 0L
    experiments
  }

  testthat::local_mocked_bindings(
    call_llm_robust = function(...) stop("live robust call reached"),
    call_llm_broadcast = function(...) stop("live broadcast call reached"),
    .package = "LLMR"
  )

  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  result <- run_focus_group(
    topic = "public transit funding",
    n_participants = 2,
    guide = list(
      Opening = "Welcome the participants.",
      Engagement = "Which funding priority matters most?",
      Closing = "Thank the participants and close the discussion."
    ),
    flow = "desire_based",
    config = config,
    .runner = scripted_runner,
    max_participant_responses = 1,
    verbose = FALSE
  )

  fg <- result$focus_group
  speakers <- vapply(fg$conversation_log, `[[`, character(1), "speaker_id")
  phases <- vapply(fg$conversation_log, `[[`, character(1), "phase")

  expect_true("MOD" %in% speakers)
  expect_true(any(speakers %in% c("P1", "P2")))
  expect_true(all(c("opening", "engagement_question", "closing") %in% phases))
  expect_match(fg$final_summary, "scripted session")
  expect_gt(calls$n, 0L)
  expect_true(all(vapply(calls$frames, function(x) {
    is.data.frame(x) && all(c("config", "messages") %in% names(x)) &&
      is.list(x$config) && is.list(x$messages)
  }, logical(1))))

  basic <- fg$analyze()
  expect_s3_class(basic$speaker_stats, "tbl_df")
  expect_true(nrow(basic$speaker_stats) >= 3L)

  themes <- fg$analyze_themes(config = config)
  expect_type(themes, "character")
  expect_true(nzchar(themes))
  expect_true(all(vapply(fg$agents, function(agent) {
    identical(agent$.runner, scripted_runner)
  }, logical(1))))
})

test_that("runner response columns become call metadata", {
  scripted_runner <- function(experiments, ...) {
    experiments$response_text <- paste(
      "This response is long enough to constitute a complete focus group turn",
      "and it includes a concrete reason for the stated position."
    )
    experiments$response_id <- "offline-response"
    experiments$finish_reason <- "stop"
    experiments$duration_s <- 0.01
    experiments$sent <- 4L
    experiments$rec <- 3L
    experiments$total <- 7L
    experiments$success <- TRUE
    experiments
  }
  config <- LLMR::llm_config("openai", "gpt-4o-mini")

  agent <- FGAgent$new(
    id = "P1",
    agent_details = list(direct_persona_description = "A participant."),
    config = config,
    .runner = scripted_runner
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
})

test_that("an incomplete retry errors and all retry usage is counted", {
  calls <- 0L
  scripted_runner <- function(experiments, ...) {
    calls <<- calls + 1L
    experiments$response_text <- if (calls == 1L) "An unfinished response..." else ""
    experiments$finish_reason <- if (calls == 1L) "length" else "stop"
    experiments$sent <- 2L
    experiments$rec <- 3L
    experiments$success <- TRUE
    experiments
  }
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  agent <- FGAgent$new(
    id = "P1",
    agent_details = list(direct_persona_description = "A participant."),
    config = config,
    .runner = scripted_runner
  )

  expect_error(
    agent$generate_utterance(
      topic = "libraries",
      conversation_history_string = "No prior discussion.",
      utterance_prompt_template = "State your view.",
      conversation_log = list()
    ),
    "incomplete utterance"
  )
  expect_identical(calls, 2L)
  expect_identical(agent$tokens_sent_agent, 4)
  expect_identical(agent$tokens_received_agent, 6)
})

test_that("runner return frames are validated", {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  expect_error(
    FocusGroup:::.fg_call_llm(
      config,
      list(list(role = "user", content = "prompt")),
      .runner = function(experiments, ...) 1
    ),
    "experiment rows"
  )
  expect_error(
    FocusGroup:::.fg_call_llm(
      config,
      list(list(role = "user", content = "prompt")),
      .runner = function(experiments, ...) experiments
    ),
    "response_text"
  )
  expect_error(
    FocusGroup:::.fg_call_llm(
      config,
      list(list(role = "user", content = "prompt")),
      .runner = function(experiments, ...) {
        data.frame(response_text = "response without experiment columns")
      }
    ),
    "experiment rows|config|messages"
  )
  expect_error(
    FocusGroup:::.fg_call_llm(
      config,
      list(list(role = "user", content = "prompt")),
      .runner = function(experiments, ...) {
        experiments$response_text <- ""
        experiments$success <- FALSE
        experiments$error <- "scripted unsuccessful row"
        experiments
      }
    ),
    "scripted unsuccessful row"
  )
})

test_that("runner conditions propagate without becoming response data", {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  provider_condition <- structure(
    list(message = "scripted provider failure", call = NULL),
    class = c("scripted_provider_error", "error", "condition")
  )
  failing_runner <- function(experiments, ...) stop(provider_condition)

  expect_error(
    FocusGroup:::.fg_call_llm(
      config,
      list(list(role = "user", content = "prompt")),
      .runner = failing_runner
    ),
    class = "scripted_provider_error"
  )
})
