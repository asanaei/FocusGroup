# The session record: every model call with the messages sent and the reply
# as returned, every speaker selection with its candidates and scores, and
# the settings and versions a session ran under.

.record_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

# A runner that keeps every request it receives and replies by call type.
.recording_runner <- function(reply = NULL) {
  env <- new.env()
  env$requests <- list()
  env$replies <- character()
  env$runner <- function(experiments, ...) {
    messages <- experiments$messages[[1]]
    prompt <- .record_prompt(messages)
    text <- if (!is.null(reply)) reply(prompt, length(env$requests) + 1L) else NULL
    text <- text %||% if (grepl("Desire to talk score", prompt, fixed = TRUE)) "6" else
      if (grepl("Summary:$", prompt)) "A short summary." else
        "As a teacher, I think evening hours matter because people work days."
    env$requests[[length(env$requests) + 1L]] <- list(
      messages = messages, params = experiments$config[[1]]$model_params)
    env$replies <- c(env$replies, text)
    experiments$response_text <- text
    experiments$thinking <- "Hidden reasoning about the reply."
    experiments$sent_tokens <- 11L
    experiments$rec_tokens <- 7L
    experiments$reasoning_tokens <- 4L
    experiments
  }
  env
}

.record_session <- function(runner, flow = "desire_based", seed = 110, ...) {
  run_focus_group(
    topic = "library hours",
    config = LLMR::llm_config("openai", "example-model", temperature = 0.3),
    n_participants = 3,
    guide = list(Engagement = c("Which hours matter?", "Who is left out?"),
                 Closing = "Close the session."),
    flow = flow, max_participant_responses = 2, verbose = FALSE, seed = seed,
    call_on_silent = "none", .runner = runner, ...)
}

test_that("every call is recorded with its messages and its reply as returned", {
  rec <- .recording_runner()
  result <- .record_session(rec$runner)
  calls <- result$calls
  expect_s3_class(calls, "data.frame")
  expect_identical(nrow(calls), length(rec$requests))
  expect_identical(calls$call_id, seq_len(nrow(calls)))
  for (i in seq_len(nrow(calls))) {
    expect_identical(calls$messages[[i]], rec$requests[[i]]$messages)
    expect_identical(calls$settings[[i]]$max_tokens,
                     rec$requests[[i]]$params$max_tokens)
  }
  expect_identical(calls$raw_text, rec$replies)
  expect_setequal(unique(calls$call_type),
                  c("moderator", "participant", "desire", "summary"))

  # The raw reply is kept beside the cleaned text that entered the log.
  spoken <- calls[calls$call_type == "participant", ]
  expect_true(all(grepl("^As a teacher", spoken$raw_text)))
  expect_true(all(grepl("^I think", spoken$cleaned_text)))
  logged <- result$transcript$text[match(spoken$message_id, result$transcript$message_id)]
  expect_identical(logged, spoken$cleaned_text)

  desire <- calls[calls$call_type == "desire", ]
  expect_true(all(desire$score == 6))
  expect_true(all(is.na(desire$message_id)))
  expect_true(all(calls$reasoning_tokens == 4L))
  expect_true(all(calls$thinking == "Hidden reasoning about the reply."))
  expect_true(all(calls$total_tokens == 18L))
  expect_true(all(is.na(calls$error)))
  expect_true(all(calls$settings[[1]]$temperature == 0.3))
})

test_that("retries are recorded as second attempts", {
  rec <- .recording_runner(function(prompt, i) {
    if (i == 1L) "An unfinished opening..." else NULL
  })
  result <- suppressMessages(.record_session(rec$runner))
  first <- result$calls[1:2, ]
  expect_identical(first$call_type, c("moderator", "moderator"))
  expect_identical(first$attempt, c(1L, 2L))
  expect_identical(first$raw_text[[1]], "An unfinished opening...")
  expect_identical(first$message_id, c(2L, 2L))
  expect_gte(first$settings[[2]]$max_tokens, 1024)
})

test_that("a call that fails is recorded with its error in the partial session", {
  calls <- 0L
  runner <- function(experiments, ...) {
    calls <<- calls + 1L
    if (calls == 3L) stop("The provider is unavailable.")
    experiments$response_text <- "6"
    if (!grepl("Desire to talk score", .record_prompt(experiments$messages[[1]]),
               fixed = TRUE)) {
      experiments$response_text <- "Evening hours matter because people work days."
    }
    experiments
  }
  e <- tryCatch(.record_session(runner, flow = "round_robin"),
                focusgroup_incomplete_session = identity)
  failed <- e$partial$calls[3, ]
  expect_identical(failed$error, "The provider is unavailable.")
  expect_true(is.na(failed$raw_text))
  expect_identical(nrow(e$partial$calls), 3L)
})

test_that("no key or endpoint enters the session record", {
  rec <- .recording_runner()
  # LLMR warns about a literal key, which is the point of the fixture.
  config <- suppressWarnings(LLMR::llm_config(
    "openai", "example-model", api_key = "sk-FIXTURE-SECRET-123",
    api_url = "https://proxy.example.org/v1?key=URL-SECRET-456",
    temperature = 0.3))
  result <- run_focus_group(
    topic = "library hours", config = config, n_participants = 2,
    guide = list(Engagement = "Which hours matter?", Closing = "Close."),
    flow = "desire_based", max_participant_responses = 1, verbose = FALSE,
    .runner = rec$runner)
  stored <- paste(capture.output(str(list(result$calls, result$routing,
                                          result$metadata),
                                     nchar.max = 1e6, list.len = 1e6,
                                     vec.len = 1e6)), collapse = "\n")
  expect_false(grepl("SECRET", stored, fixed = TRUE))
  expect_false(grepl("proxy.example.org", stored, fixed = TRUE))
  serialized <- rawToChar(serialize(list(result$calls, result$routing,
                                         result$metadata), NULL, ascii = TRUE))
  expect_false(grepl("SECRET", serialized, fixed = TRUE))
  # The record is not empty, and the search finds a planted value.
  expect_gt(nrow(result$calls), 0L)
  expect_true(all(lengths(result$calls$messages) > 0L))
  planted <- result$calls
  planted$settings[[1]]$note <- "sk-FIXTURE-SECRET-123"
  expect_true(grepl("SECRET", rawToChar(serialize(planted, NULL, ascii = TRUE)),
                    fixed = TRUE))
})

test_that("an llmr_response call records its reasoning text and reasoning tokens", {
  response <- structure(
    list(text = "6", thinking = "The reasoning says 9/10.",
         finish_reason = "stop", response_id = "resp-1", duration_s = 0.5,
         usage = list(sent = 20L, rec = 9L, total = 29L, reasoning = 5L)),
    class = "llmr_response")
  speech <- response
  speech$text <- "Evening hours matter because many residents work late."
  testthat::local_mocked_bindings(
    call_llm_robust = function(config, messages, ...) {
      if (grepl("Desire to talk score", .record_prompt(messages), fixed = TRUE)) response
      else speech
    },
    call_llm_broadcast = function(config, messages, ...) {
      data.frame(success = rep(TRUE, length(messages)),
                 response_text = rep("<think>Maybe 9/10.</think> 6", length(messages)),
                 finish_reason = "stop", sent_tokens = 3L, rec_tokens = 2L,
                 reasoning_tokens = 1L, stringsAsFactors = FALSE)
    },
    tokens = function(x) x$usage,
    finish_reason = function(x) x$finish_reason,
    .package = "LLMR")
  result <- run_focus_group(
    topic = "library hours", config = LLMR::llm_config("openai", "example-model"),
    n_participants = 2,
    guide = list(Engagement = "Which hours matter?", Closing = "Close."),
    flow = "desire_based", max_participant_responses = 1, verbose = FALSE)
  calls <- result$calls
  speaking <- calls[calls$call_type == "moderator", ][1, ]
  expect_identical(speaking$reasoning_tokens, 5L)
  expect_identical(speaking$thinking, "The reasoning says 9/10.")
  expect_identical(speaking$response_id, "resp-1")

  # The live desire path broadcasts; each row is one call of one agent.
  desire <- calls[calls$call_type == "desire", ]
  expect_setequal(desire$agent_id, c("P1", "P2"))
  expect_identical(desire$score, c(6, 6))
  expect_identical(desire$cleaned_text, c("6", "6"))
  expect_identical(desire$reasoning_tokens, c(1L, 1L))
  expect_true(all(lengths(desire$messages) > 0L))
})

test_that("every pick has a routing record, whatever the flow", {
  for (flow in c("round_robin", "probabilistic", "desire_based")) {
    rec <- .recording_runner()
    result <- .record_session(rec$runner, flow = flow)
    routing <- result$routing
    participant_ids <- result$transcript$message_id[
      result$transcript$speaker_id %in% c("P1", "P2", "P3")]
    expect_identical(routing$message_id, participant_ids, info = flow)
    expect_identical(routing$turn, seq_along(participant_ids), info = flow)
    expect_identical(routing$selected_id,
                     result$transcript$speaker_id[participant_ids], info = flow)
    expect_true(all(routing$flow == flow), info = flow)
    expect_true(all(lengths(routing$candidates) > 0L), info = flow)
  }

  rr <- .record_session(.recording_runner()$runner, flow = "round_robin")$routing
  expect_true(all(rr$rule == "rotation"))
  expect_identical(rr$selected_id, c("P1", "P2", "P3", "P1"))

  prob <- .record_session(.recording_runner()$runner, flow = "probabilistic")$routing
  expect_true(all(prob$rule == "propensity_draw"))
  expect_true(all(abs(vapply(prob$scores, sum, numeric(1)) - 1) < 1e-12))

  # Equal desire scores: every eligible participant is tied and the tie is
  # broken at random; the previous speaker is excluded with its reason.
  desire <- .record_session(.recording_runner()$runner)$routing
  expect_true(all(desire$rule == "highest_score"))
  expect_true(all(desire$tie_break == "random"))
  expect_identical(desire$tied[[1]], c("P1", "P2", "P3"))
  expect_identical(unname(desire$excluded[[2]]), "previous_speaker")
  expect_identical(names(desire$excluded[[2]]), desire$selected_id[[1]])
  expect_identical(names(desire$scores[[2]]), desire$tied[[2]])
})

test_that("a clear desire winner is recorded without a tie-break", {
  runner <- function(experiments, ...) {
    messages <- experiments$messages[[1]]
    prompt <- .record_prompt(messages)
    experiments$response_text <- if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      if (grepl("Persona two", messages[[1]]$content, fixed = TRUE)) "9" else "4"
    } else if (grepl("Summary:$", prompt)) "A short summary." else
      "Evening hours matter because people work days."
    experiments
  }
  agents <- create_agents(3, config = LLMR::llm_config("openai", "example-model"),
                          direct_persona_descriptions = c("Persona one.", "Persona two.",
                                                          "Persona three."),
                          .runner = runner)
  fg <- FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow("desire_based", agents, "MOD"),
    question_script = list(list(phase = "engagement_question", text = "Which hours?")),
    max_participant_responses = 1)
  fg$run_simulation()
  pick <- fg$records$routing[[1]]
  expect_identical(pick$selected_id, "P2")
  expect_identical(pick$tie_break, "none")
  expect_identical(pick$tied, "P2")
  expect_identical(pick$scores, c(P1 = 4, P2 = 9, P3 = 4))
  expect_identical(pick$threshold, 3)
})

test_that("a custom flow's picks are recorded as custom", {
  FirstFlow <- R6::R6Class("FirstFlow", inherit = ConversationFlow, public = list(
    select_next_speaker = function(focus_group) self$agents[[self$participant_ids[1]]]))
  rec <- .recording_runner()
  agents <- create_agents(2, config = LLMR::llm_config("openai", "example-model"),
                          direct_persona_descriptions = c("A.", "B."),
                          .runner = rec$runner)
  fg <- FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
    turn_taking_flow = FirstFlow$new(agents, "MOD"),
    question_script = list(list(phase = "engagement_question", text = "Which hours?")),
    max_participant_responses = 2, call_on_silent = "none")
  fg$run_simulation()
  rules <- vapply(fg$records$routing, `[[`, character(1), "rule")
  expect_identical(rules, c("custom", "custom"))
  expect_identical(vapply(fg$records$routing, `[[`, character(1), "flow"),
                   c("FirstFlow", "FirstFlow"))
})

test_that("a flow's choice of no participant or of the moderator records no routing row", {
  # Odd picks give the floor to the first participant; even picks end the
  # discussion of the item, leaving a record of their own that must not be
  # copied for a later pick.
  EndingFlow <- R6::R6Class("EndingFlow", inherit = ConversationFlow, public = list(
    picks = 0L,
    ends_with = "none",
    select_next_speaker = function(focus_group) {
      self$picks <- self$picks + 1L
      if (self$picks %% 2L == 1L) return(self$agents[[self$participant_ids[1]]])
      self$routing_record <- list(flow = "EndingFlow", rule = "ended")
      if (identical(self$ends_with, "moderator")) self$agents[[self$moderator_id]] else NULL
    }))
  for (ending in c("none", "moderator")) {
    rec <- .recording_runner()
    agents <- create_agents(3, config = LLMR::llm_config("openai", "example-model"),
                            direct_persona_descriptions = c("A.", "B.", "C."),
                            .runner = rec$runner)
    flow <- EndingFlow$new(agents, "MOD")
    flow$ends_with <- ending
    fg <- FocusGroup$new(
      topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
      turn_taking_flow = flow,
      question_script = list(list(phase = "engagement_question", text = "Which hours?"),
                             list(phase = "engagement_question", text = "Who is left out?")),
      max_participant_responses = 2, call_on_silent = 1L)
    fg$run_simulation(injections = fg_injection("A word from outside.", fg_before_turn(2),
                                                from = "Guest"),
                      final_summary = FALSE)
    result <- FocusGroup:::.fg_focus_group_result(fg, "EndingFlow", "roleflip", 3L, NA_integer_)
    routing <- result$routing
    expect_identical(routing$turn, 1:4, info = ending)
    expect_identical(routing$rule, c("custom", "called_on", "called_on", "custom"), info = ending)
    expect_identical(routing$selected_id, c("P1", "P2", "P3", "P1"), info = ending)
    expect_false(anyNA(routing$message_id), info = ending)
    tr <- result$transcript
    replies <- tr$message_id[tr$speaker_id %in% c("P1", "P2", "P3")]
    expect_identical(routing$message_id, replies, info = ending)
    # The point before turn 2 came when the flow was asked for turn 2, so the
    # injection stands before the call-on line whose reply is turn 2.
    injected <- which(vapply(tr$metadata, function(m) isTRUE(m$injected), logical(1)))
    line <- which(vapply(tr$metadata, function(m) !is.null(m$call_on), logical(1)))[1]
    expect_identical(injected + 1L, line, info = ending)
    expect_identical(tr$speaker_id[[line + 1L]], "P2", info = ending)
  }
})

test_that("session metadata records the settings, templates, and versions", {
  rec <- .recording_runner()
  result <- .record_session(
    rec$runner, reasoning_effort = c(desire = "low"),
    max_tokens_config = list(desire = 32))
  meta <- result$metadata
  expect_identical(meta$max_participant_responses, 2L)
  expect_identical(meta$seed, 110L)
  expect_identical(meta$on_desire_failure, "error")
  expect_identical(meta$reasoning_effort, list(desire = "low"))
  expect_identical(meta$max_tokens$desire, 32)
  expect_identical(meta$max_tokens$summary, 400)
  expect_identical(meta$versions$FocusGroup,
                   as.character(utils::packageVersion("FocusGroup")))
  expect_identical(meta$versions$LLMR, as.character(utils::packageVersion("LLMR")))
  expect_identical(meta$prompt_templates, result$focus_group$prompt_templates)
  expect_identical(meta$standing_rules$participant, FocusGroup:::.fg_standing_rules(FALSE))
  expect_identical(meta$disclosure, "transparent")
  expect_s3_class(meta$started, "POSIXct")

  settings <- meta$call_settings
  desire <- settings[settings$call_type == "desire", ]
  expect_identical(nrow(desire), 3L)
  expect_true(all(vapply(desire$settings, function(s) identical(s$reasoning_effort, "low"),
                         logical(1))))
  expect_true(all(vapply(desire$settings, function(s) s$max_tokens == 32, logical(1))))
  moderator <- settings[settings$call_type == "moderator", ]
  expect_null(moderator$settings[[1]]$reasoning_effort)
  expect_identical(moderator$settings[[1]]$temperature, 0.3)

  # Without a seed the record says so.
  expect_true(is.na(.record_session(.recording_runner()$runner, seed = NULL)$metadata$seed))
})
