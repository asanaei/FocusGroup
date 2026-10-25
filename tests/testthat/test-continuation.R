# Continuing a recorded session: the state at a cut, rebuilt from the record,
# against the state of the same session paused there, and counterfactual
# arms. Offline, with runners whose replies depend only on the prompt.

.cont_config <- function() LLMR::llm_config("openai", "example-model")

.cont_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""), character(1)),
        collapse = "\n")
}

# Replies that depend only on the prompt: desire scores from 0 to 10, ties
# and scores below the threshold included; moderator lines that now and then
# put the question to P2; numbered participant replies.
.cont_runner <- function(experiments, ...) {
  prompt <- .cont_prompt(experiments$messages[[1]])
  h <- sum(utf8ToInt(prompt))
  experiments$response_text <- if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
    as.character(h %% 11L)
  } else if (grepl("Summary:$", prompt)) {
    "A short summary of the session."
  } else if (grepl("private question", prompt, fixed = TRUE)) {
    "5"
  } else if (grepl("In this discussion you are", prompt, fixed = TRUE)) {
    sprintf("Reply %d: evening hours matter to people who work days.", h %% 9973L)
  } else if (h %% 3L == 0L) {
    "P2, what would you add to that?"
  } else {
    sprintf("Moderator line %d: what do you all think?", h %% 9973L)
  }
  experiments
}

.cont_guide <- list(
  list(phase = "opening", text = "Welcome, everyone.", verbatim = TRUE),
  list(phase = "engagement_question", text = "Which library hours matter most to you?"),
  list(phase = "engagement_question", text = "Who is left out by the current hours?"),
  list(phase = "exploration_question", text = "What would you change first?"),
  list(phase = "closing", text = "Thank you all; this closes the session.", verbatim = TRUE))

.cont_make <- function(flow, call_on, runner = .cont_runner, n = 3L, responses = 2L) {
  function() {
    agents <- create_agents(n, config = .cont_config(),
                            direct_persona_descriptions = paste("Persona", seq_len(n)),
                            .runner = runner)
    params <- switch(flow,
      probabilistic = list(recovery_increment = 0.3),
      desire_based = list(min_desire_threshold = 4, on_desire_failure = "error"),
      list())
    FocusGroup$new(topic = "library hours", purpose = "Test the continuation.",
                   agents = agents, moderator_id = "MOD",
                   turn_taking_flow = create_conversation_flow(flow, agents, "MOD",
                                                               flow_params = params),
                   question_script = .cont_guide, max_participant_responses = responses,
                   call_on_silent = call_on)
  }
}

.cont_injections <- list(
  none = list(),
  mixed = list(
    fg_injection("A word from the library board: budgets are fixed.", fg_after_item(2),
                 from = "Board member"),
    fg_injection("Speaking for myself, the evening hours are what I need.",
                 fg_before_turn(3), from = "P3"),
    fg_injection("P1, do you agree with that?", fg_before_turn(4))))

.cont_run <- function(make, injections) {
  fg <- make()
  withr::with_seed(110, fg$run_simulation(injections = injections, final_summary = FALSE))
  fg
}

# The same session, paused at fg_before_turn(t) the way run_arms() pauses
# its prefix: the plan holds the session's injections and the pause. A
# participant's injected turn at that point is turn t itself, which the cut
# leaves out.
.cont_paused <- function(make, injections, t) {
  fg <- make()
  p <- fg$.__enclos_env__$private
  participants <- setdiff(names(fg$agents), fg$moderator_id)
  injections <- Filter(function(inj) {
    !(inj$from %in% participants && identical(format(inj$at), sprintf("fg_before_turn(%d)", t)))
  }, injections)
  status <- withr::with_seed(110, {
    p$prepare_plan(FocusGroup:::.fg_injection_list(injections), list(),
                   pause_at = paste0("before_turn:", t))
    fg$message_mode <- FocusGroup:::.fg_message_mode()
    p$start_records(FALSE)
    p$with_records({
      p$session_begin(NULL, FALSE)
      p$session_continue(FALSE)
    })
  })
  expect_identical(status, "paused")
  fg
}

# Fields left out of the comparison, and why. Everything else the session
# and its flow hold is compared.
.cont_exempt <- list(
  # The plan is the run's own: the record's injections are in the log and
  # are not planned again.
  session = "plan",
  # A desire-based flow keeps its last scores, scoring path, and whether it
  # has warned about a failed score; none of them decides a later turn.
  flow = c("last_desire_scores", "last_scoring_mode", "failure_message_emitted"))

.cont_fields <- function(env, drop) {
  out <- as.list(env, all.names = TRUE)
  out <- out[!vapply(out, is.function, logical(1))]
  out <- out[setdiff(names(out), c(drop, ".__enclos_env__", "super", "self", "private"))]
  out[order(names(out))]
}

.cont_tidy <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  x[order(names(x))]
}

.cont_state <- function(fg) {
  p <- fg$.__enclos_env__$private
  flow <- fg$turn_taking_flow
  private <- .cont_fields(p, .cont_exempt$session)
  private$state <- .cont_tidy(private$state)
  transcript <- FocusGroup:::.fg_transcript_data(fg$conversation_log)
  transcript$timestamp <- NULL
  calls <- FocusGroup:::.fg_calls_data(fg$records$calls)
  calls$messages <- NULL
  calls$timestamp <- NULL
  info <- fg$records$info
  info$started <- NULL
  list(
    private = private,
    flow_class = class(flow),
    flow_private = .cont_fields(flow$.__enclos_env__$private, .cont_exempt$flow),
    flow_public = list(last_speaker_id = flow$last_speaker_id,
                       selection_metadata = flow$selection_metadata,
                       routing_record = flow$routing_record,
                       participant_ids = flow$participant_ids,
                       moderator_id = flow$moderator_id,
                       agents = names(flow$agents)),
    execution_phase = attr(fg, "execution_phase"),
    session_status = attr(fg, "session_status"),
    current_question_text = fg$current_question_text,
    current_conversation_summary = fg$current_conversation_summary,
    final_summary = fg$final_summary,
    message_mode = fg$message_mode,
    handles = fg$handles,
    agent_handles = vapply(fg$agents, function(a) a$handle, character(1)),
    call_on_silent = fg$call_on_silent,
    transcript = transcript,
    routing = FocusGroup:::.fg_routing_data(fg$records$routing),
    probes = FocusGroup:::.fg_probes_data(fg$records$probes),
    injections = FocusGroup:::.fg_injections_data(fg$records$injections),
    calls = calls,
    info = info,
    tokens = c(fg$total_tokens_sent, fg$total_tokens_received, fg$total_tokens,
               fg$calls_usage_unknown))
}

.cont_turns <- function(fg) {
  ids <- setdiff(names(fg$agents), fg$moderator_id)
  sum(vapply(fg$conversation_log, function(m) m$speaker_id %in% ids, logical(1)))
}

# Three participants with two responses a question, under each flow, with
# call-ons on and off; and four participants with one response, so that the
# moderator calls on several in a row.
.cont_cases <- function() {
  cases <- list()
  for (flow in c("round_robin", "probabilistic", "desire_based")) {
    for (call_on in c("none", "default")) {
      for (inj in names(.cont_injections)) {
        cases[[length(cases) + 1L]] <- list(flow = flow, call_on = call_on, inj = inj,
                                            n = 3L, responses = 2L)
      }
    }
  }
  for (flow in c("round_robin", "desire_based")) {
    for (inj in names(.cont_injections)) {
      cases[[length(cases) + 1L]] <- list(flow = flow, call_on = "default", inj = inj,
                                          n = 4L, responses = 1L)
    }
  }
  cases
}

test_that("the state rebuilt at every turn equals the state of the session paused there", {
  checked <- 0L
  stages <- character()
  addressed <- 0L
  queued <- 0L
  rules <- character()
  for (case in .cont_cases()) {
    call_on <- if (identical(case$call_on, "none")) "none" else c("opening", "midpoint", "end")
    make <- .cont_make(case$flow, call_on, n = case$n, responses = case$responses)
    injections <- .cont_injections[[case$inj]]
    full <- .cont_run(make, injections)
    rules <- c(rules, vapply(full$records$routing, `[[`, character(1), "rule"))
    for (t in seq_len(.cont_turns(full))) {
      label <- sprintf("%s, %d participants, call-ons %s, injections %s, turn %d",
                       case$flow, case$n, case$call_on, case$inj, t)
      paused <- .cont_paused(make, injections, t)
      rebuilt <- FocusGroup:::.fg_clone_session(full)
      rebuilt$.__enclos_env__$private$position_at(t, final_summary = FALSE)
      a <- .cont_state(rebuilt)
      b <- .cont_state(paused)
      for (field in names(b)) expect_identical(a[[field]], b[[field]], label = paste(label, field))
      state <- paused$.__enclos_env__$private$state
      stages <- c(stages, state$stage)
      addressed <- addressed + !is.null(state$addressed)
      queued <- max(queued, state[["call_on"]]$position %||% 0L)
      checked <- checked + 1L
    }
  }
  # 138 cuts: every turn of 16 sessions (three flows, call-ons on and off,
  # with and without injections, and the wider call-on sessions). Among them
  # are cuts at call-on turns deep in a queue and at addressed turns, and
  # the sessions hold every kind of pick.
  expect_identical(checked, 138L)
  expect_gte(queued, 3L)
  expect_true(all(c("turn", "call_on") %in% stages))
  expect_gt(addressed, 5L)
  expect_true(all(c("rotation", "propensity_draw", "highest_score", "threshold_lowered",
                    "highest_below_threshold", "addressed", "called_on", "injected") %in%
                    rules))
})
