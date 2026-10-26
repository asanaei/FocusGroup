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

# The same replies with desire scores from 0 to 3, all below the threshold
# of 4, so that the desire flow lowers its threshold or takes the highest
# score below it.
.cont_low_runner <- function(experiments, ...) {
  prompt <- .cont_prompt(experiments$messages[[1]])
  experiments <- .cont_runner(experiments, ...)
  if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
    experiments$response_text <- as.character(sum(utf8ToInt(prompt)) %% 4L)
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
  flow = c("last_desire_scores", "last_scoring_mode", "failure_message_emitted"),
  # An agent's model configuration and runner are not part of the record:
  # a continuation supplies them. Its own history and token counters record
  # the calls it made, which a rebuilt session did not make.
  agent = c("config", ".runner", "history", "tokens_sent_agent", "tokens_received_agent",
            "tokens_total_agent", "usage_unknown_agent"),
  # Times differ between two runs: the start of the record, and the time of
  # each message and call. The messages a call sent are not written to a
  # session file.
  times = c("started", "timestamp"), calls = "messages")

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
  calls <- calls[setdiff(names(calls), c(.cont_exempt$calls, .cont_exempt$times))]
  info <- fg$records$info
  info <- info[setdiff(names(info), .cont_exempt$times)]
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
    agents = lapply(fg$agents, .cont_fields, drop = .cont_exempt$agent),
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
  # Desire scores below the threshold, for the lowered-threshold picks.
  for (inj in names(.cont_injections)) {
    cases[[length(cases) + 1L]] <- list(flow = "desire_based", call_on = "default",
                                        inj = inj, n = 3L, responses = 2L,
                                        runner = .cont_low_runner)
  }
  cases
}

test_that("the state rebuilt at every turn, in memory and from its file, equals the paused state", {
  checked <- 0L
  stages <- character()
  addressed <- 0L
  queued <- 0L
  rules <- character()
  for (case in .cont_cases()) {
    call_on <- if (identical(case$call_on, "none")) "none" else c("opening", "midpoint", "end")
    make <- .cont_make(case$flow, call_on, runner = case$runner %||% .cont_runner,
                       n = case$n, responses = case$responses)
    injections <- .cont_injections[[case$inj]]
    full <- .cont_run(make, injections)
    # The record after a round trip through its session file.
    lines <- format(FocusGroup:::.fg_object_result(full))
    read <- focus_group_from_transcript(lines)
    expect_identical(format(FocusGroup:::.fg_object_result(read)), lines)
    rules <- c(rules, vapply(full$records$routing, `[[`, character(1), "rule"))
    for (t in seq_len(.cont_turns(full))) {
      label <- sprintf("%s, %d participants, call-ons %s, injections %s, turn %d",
                       case$flow, case$n, case$call_on, case$inj, t)
      paused <- .cont_paused(make, injections, t)
      rebuilt <- FocusGroup:::.fg_clone_session(full)
      rebuilt$.__enclos_env__$private$position_at(t, final_summary = FALSE)
      from_file <- FocusGroup:::.fg_clone_session(read)
      from_file$.__enclos_env__$private$position_at(t, final_summary = FALSE)
      a <- .cont_state(rebuilt)
      b <- .cont_state(paused)
      f <- .cont_state(from_file)
      for (field in names(b)) {
        expect_identical(a[[field]], b[[field]], label = paste(label, field))
        expect_identical(f[[field]], b[[field]], label = paste(label, field, "from the file"))
      }
      state <- paused$.__enclos_env__$private$state
      stages <- c(stages, state$stage)
      addressed <- addressed + !is.null(state$addressed)
      queued <- max(queued, state[["call_on"]]$position %||% 0L)
      checked <- checked + 1L
    }
  }
  # 160 cuts: every turn of 18 sessions (three flows, call-ons on and off,
  # with and without injections, the wider call-on sessions, and desire
  # scores below the threshold). Among them are cuts at call-on turns deep
  # in a queue, at an interjection during a call-on, and at addressed turns,
  # and the sessions hold every kind of pick.
  expect_identical(checked, 160L)
  expect_gte(queued, 3L)
  expect_true(all(c("turn", "call_on") %in% stages))
  expect_gt(addressed, 5L)
  expect_true(all(c("rotation", "propensity_draw", "highest_score", "threshold_lowered",
                    "highest_below_threshold", "addressed", "called_on", "injected") %in%
                    rules))
})

# ---- Counterfactual arms from a record ---------------------------------------------

# A runner that keeps every request and replies by the prompt alone.
.cont_recorder <- function() {
  env <- new.env()
  env$requests <- list()
  env$runner <- function(experiments, ...) {
    env$requests[[length(env$requests) + 1L]] <- experiments$messages[[1]]
    .cont_runner(experiments, ...)
  }
  env
}

.cont_record <- function(flow = "round_robin", runner = .cont_runner, ...) {
  run_focus_group(
    topic = "library hours", config = .cont_config(), n_participants = 3,
    guide = list(Opening = "Welcome, everyone.",
                 Engagement = c("Which library hours matter most to you?",
                                "Who is left out by the current hours?"),
                 Exploration = "What would you change first?",
                 Closing = "Thank you all; this closes the session."),
    flow = flow, max_participant_responses = 2, moderator_verbatim = c(1, 5),
    seed = 110, verbose = FALSE, .runner = runner, ...)
}

.cont_arms <- function(record, t = 3L, runner = .cont_runner, from = "P2", ...) {
  run_focus_group_arms(
    record, config = .cont_config(),
    arms = list(same = NULL,
                changed = fg_injection("I say the evening hours should stay, whatever it costs.",
                                       at = fg_before_turn(t), from = from)),
    seed = 110, verbose = FALSE, .runner = runner, ...)
}

test_that("a regenerated arm sends the recorded prompts and, under round robin, repeats the record", {
  record <- .cont_record()
  rec <- .cont_recorder()
  tr <- record$transcript
  turns <- which(tr$speaker_id %in% c("P1", "P2", "P3"))
  cut <- turns[[3]]
  # The changed arm has the speaker of turn 3 say something else.
  speaker <- tr$speaker_id[[cut]]
  out <- .cont_arms(record, t = 3L, runner = rec$runner, from = speaker)
  same <- out$arms$same
  expect_identical(out$metadata$prefix_messages, cut - 1L)
  # The first call after the cut sends the messages the recorded call for
  # turn 3 sent.
  recorded <- record$calls[record$calls$message_id %in% tr$message_id[[cut]], ]
  expect_identical(rec$requests[[1]], recorded$messages[[1]])
  # And the regenerated arm repeats the recorded remainder, call for call.
  cols <- c("message_id", "speaker_id", "handle", "text", "item", "round", "phase")
  expect_identical(same$transcript[, cols], tr[, cols])
  later <- record$calls[record$calls$call_id > out$arms$same$metadata$branch$prefix_calls, ]
  arm_calls <- same$calls[same$calls$call_id > same$metadata$branch$prefix_calls, ]
  expect_identical(arm_calls$messages, later$messages)
  expect_identical(same$summary, record$summary)
  expect_identical(same$routing[, c("turn", "rule", "selected_id", "message_id")],
                   record$routing[, c("turn", "rule", "selected_id", "message_id")])

  # The changed arm says the injected text as turn 3, and both arms share
  # the prefix.
  changed <- out$arms$changed
  ctr <- changed$transcript
  expect_identical(ctr$text[[cut]], "I say the evening hours should stay, whatever it costs.")
  expect_identical(ctr$speaker_id[[cut]], speaker)
  expect_true(ctr$metadata[[cut]]$as_turn)
  expect_identical(changed$routing$rule[[3]], "injected")
  # Turn 3 answers the call-on line for the participant round robin left
  # out of item 2, and the injected turn takes its place.
  expect_identical(record$routing$rule[[3]], "called_on")
  expect_identical(changed$injections$replaced, "called_on")
  expect_identical(changed$metadata$branch$prefix_hash, same$metadata$branch$prefix_hash)
  expect_identical(out$metadata$prefix_hash,
                   FocusGroup:::.fg_prefix_hash(record$focus_group$conversation_log[seq_len(cut - 1L)]))
  expect_identical(ctr[seq_len(cut - 1L), cols], tr[seq_len(cut - 1L), cols])
  expect_true(all(c(same$metadata$complete, changed$metadata$complete)))
  # The prefix's records are kept: its picks, and the calls made before the cut.
  expect_identical(changed$routing$turn, seq_len(nrow(changed$routing)))
  expect_identical(nrow(out$prefix$calls), same$metadata$branch$prefix_calls)
  expect_identical(out$prefix$calls$call_id, record$calls$call_id[seq_len(nrow(out$prefix$calls))])
})

test_that("a session file written and read back continues the same way", {
  record <- .cont_record()
  path <- tempfile(fileext = ".fg.jsonl")
  writeLines(format(record), path)
  fg <- focus_group_from_transcript(path)
  a <- .cont_arms(fg)
  b <- .cont_arms(record)
  for (arm in c("same", "changed")) {
    expect_identical(a$arms[[arm]]$transcript[, c("speaker_id", "text")],
                     b$arms[[arm]]$transcript[, c("speaker_id", "text")])
  }
})

test_that("a continuation checks every argument before any call", {
  record <- .cont_record()
  calls <- 0L
  counting <- function(experiments, ...) {
    calls <<- calls + 1L
    .cont_runner(experiments, ...)
  }
  arms <- list(same = NULL, changed = fg_injection("Text.", fg_before_turn(3), from = "P2"))
  go <- function(...) run_focus_group_arms(record, config = .cont_config(), verbose = FALSE,
                                           .runner = counting, ...)
  expect_error(go(arms = arms, flow = "desire_based"), "fixes `flow`")
  expect_error(go(arms = arms, n_participants = 4, handles = "ids"),
               "fixes `n_participants`, `handles`")
  expect_error(run_focus_group_arms(record, arms = arms), "config")
  expect_error(go(arms = list(a = NULL, b = fg_injection("Text.", fg_after_item(2)))),
               "fg_before_turn")
  expect_error(go(arms = list(a = NULL, b = fg_injection("Text.", fg_before_turn(40),
                                                         from = "P2"))),
               "beyond")
  expect_error(go(arms = list(a = NULL, b = list(
    fg_injection("Text.", fg_before_turn(3), from = "P2"),
    fg_injection("Late.", fg_after_item(2), from = "Guest")))), "before the branch point")
  expect_error(go(arms = list(a = NULL, b = list(
    fg_injection("Text.", fg_before_turn(3), from = "P2"),
    fg_injection("Later.", fg_after_item(4), from = "P1")))), "participant's turn")
  expect_error(go(arms = list(a = NULL, b = fg_injection("Text.", fg_before_turn(3),
                                                         from = "P7"))),
               "named like a participant")
  expect_error(go(arms = arms, probes = fg_probe("Q?", at = fg_after_item(2))),
               "already passed")
  expect_error(go(arms = arms, max_tokens_config = list(desire = 0)), "positive whole")
  custom <- record
  custom$metadata$flow <- "FirstFlow"
  expect_error(run_focus_group_arms(custom, config = .cont_config(), arms = arms,
                                    verbose = FALSE, .runner = counting), "custom")
  human <- focus_group_from_transcript(data.frame(
    speaker = c("Moderator", "Ana", "Ben", "Moderator", "Ana"),
    text = c("Which hours matter?", "Evenings.", "Mornings.", "Who is left out?",
             "Shift workers.")))
  expect_error(run_focus_group_arms(human, config = .cont_config(), verbose = FALSE,
                                    arms = list(a = NULL, b = fg_injection(
                                      "Text.", fg_before_turn(2), from = "Ben")),
                                    .runner = counting), "no topic")
  topical <- focus_group_from_transcript(human, topic = "library hours")
  expect_error(run_focus_group_arms(topical, config = .cont_config(), verbose = FALSE,
                                    arms = list(a = NULL, b = fg_injection(
                                      "Text.", fg_before_turn(2), from = "Ben")),
                                    .runner = counting), "Ana, Ben have no persona")
  expect_identical(calls, 0L)
  # The estimate is checked for a live run.
  expect_error(run_focus_group_arms(record, config = .cont_config(), arms = arms,
                                    max_calls = 1, verbose = FALSE), "max_calls")
})

test_that("a continuation restores the caller's random-number state and reproduces under a seed", {
  set.seed(5)
  before <- .Random.seed
  record <- .cont_record(flow = "probabilistic")
  expect_identical(.Random.seed, before)
  a <- .cont_arms(record)
  expect_identical(.Random.seed, before)
  b <- .cont_arms(record)
  expect_identical(a$arms$same$transcript$text, b$arms$same$transcript$text)
  expect_identical(a$arms$changed$routing$selected_id, b$arms$changed$routing$selected_id)
})

test_that("the player and the summaries read arms continued with an injected turn", {
  record <- .cont_record(flow = "desire_based")
  out <- .cont_arms(record, t = 4L)
  pages <- fg_player(out, file = tempfile(), open = FALSE)
  expect_identical(names(pages), c("same", "changed"))
  turns <- fg_turns(out)
  expect_identical(turns$rule[turns$segment == "changed" & turns$injected], "injected")
  stub <- .sum_runner()
  summary <- summarize_focus_group(out, .cont_config(), .runner = stub$runner)
  expect_identical(summary$metadata$segments, c("prefix", "same", "changed"))
  changed <- stub$prompts[summary$calls$segment == "changed"]
  expect_true(any(grepl("I say the evening hours should stay, whatever it costs.",
                        changed, fixed = TRUE)))
  expect_false(any(grepl("I say the evening hours should stay",
                         stub$prompts[summary$calls$segment == "same"], fixed = TRUE)))
})

test_that("a human transcript continues with the personas given", {
  lines <- c(
    '{"type":"session","topic":"library hours"}',
    '{"type":"participant","id":"Chair","is_moderator":true}',
    '{"type":"participant","id":"Ana"}',
    '{"type":"participant","id":"Ben"}',
    '{"type":"message","speaker_id":"Chair","text":"Which hours matter most to you?"}',
    '{"type":"message","speaker_id":"Ana","text":"Evenings, after work."}',
    '{"type":"message","speaker_id":"Ben","text":"Mornings, before the crowds."}',
    '{"type":"message","speaker_id":"Chair","text":"Who is left out by the current hours?"}',
    '{"type":"message","speaker_id":"Ben","text":"Shift workers, mostly."}',
    '{"type":"message","speaker_id":"Ana","text":"And parents."}',
    '{"type":"message","speaker_id":"Chair","text":"Thank you all."}')
  fg <- focus_group_from_transcript(lines, personas = c(Ana = "A nurse who works days.",
                                                         Ben = "A baker who works nights."))
  rec <- .cont_recorder()
  out <- run_focus_group_arms(
    fg, config = .cont_config(),
    arms = list(same = NULL,
                changed = fg_injection("Honestly, weekends matter more to me.",
                                       at = fg_before_turn(2), from = "Ben")),
    final_summary = FALSE, verbose = FALSE, .runner = rec$runner)
  for (arm in out$arms) {
    tr <- arm$transcript
    expect_true(arm$metadata$complete)
    # The later moderator lines are the recorded ones, delivered as written.
    expect_identical(tr$text[tr$speaker_id == "Chair"],
                     c("Which hours matter most to you?",
                       "Who is left out by the current hours?", "Thank you all."))
    expect_identical(sum(tr$speaker_id %in% c("Ana", "Ben")), 4L)
  }
  changed <- out$arms$changed$transcript
  expect_identical(changed$text[[3]], "Honestly, weekends matter more to me.")
  # Round robin continues from the last speaker: after Ana comes Ben, whose
  # turn the injection takes; the next item opens with Ana.
  expect_identical(out$arms$same$transcript$speaker_id[3], "Ben")
  expect_match(.cont_prompt(rec$requests[[1]]), "A baker who works nights.", fixed = TRUE)
  expect_false(any(grepl("placeholder", vapply(rec$requests, .cont_prompt, character(1)))))
  expect_identical(nrow(out$arms$same$calls), 3L)
})

test_that("the interim summary in force at the cut is the one the record holds", {
  record <- .cont_record()
  calls <- record$calls
  row <- calls[1, ]
  row$call_id <- nrow(calls) + 1L
  row$call_type <- "interim_summary"
  row$round <- 1L
  row$cleaned_text <- "Earlier, the group agreed that evenings matter."
  row$message_id <- NA_integer_
  # A record whose calls hold an interim summary made after round 1.
  record$calls <- rbind(calls, row)
  fg <- focus_group_from_transcript(record)
  priv <- fg$.__enclos_env__$private
  priv$position_at(3L)
  expect_identical(fg$current_conversation_summary,
                   "Earlier, the group agreed that evenings matter.")
  expect_identical(priv$last_summary_round, 1L)
  rec <- .cont_recorder()
  .cont_arms(record, t = 3L, runner = rec$runner)
  expect_match(.cont_prompt(rec$requests[[1]]),
               "Summary of earlier discussion: Earlier, the group agreed", fixed = TRUE)
  fresh <- focus_group_from_transcript(.cont_record())
  fresh$.__enclos_env__$private$position_at(3L)
  expect_null(fresh$current_conversation_summary)
})

test_that("the prefix keeps every call made before the cut, a probe's re-ask included", {
  unsure <- function(experiments, ...) {
    messages <- experiments$messages[[1]]
    last <- messages[[length(messages)]]$content
    if (grepl("could not read one number", last, fixed = TRUE)) {
      experiments$response_text <- ""
      return(experiments)
    }
    if (grepl("private question", .cont_prompt(messages), fixed = TRUE)) {
      experiments$response_text <- "I am not sure."
      return(experiments)
    }
    .cont_runner(experiments, ...)
  }
  record <- suppressWarnings(.cont_record(
    runner = unsure, probes = fg_probe("From 1 to 7?", at = fg_before_turn(3), scale = c(1, 7))))
  turn_3 <- record$transcript$message_id[record$transcript$speaker_id %in% c("P1", "P2", "P3")][3]
  before <- max(record$calls$call_id[record$calls$call_id <
                                       min(record$calls$call_id[record$calls$message_id %in% turn_3])])
  out <- .cont_arms(record, t = 3L)
  expect_identical(out$arms$same$metadata$branch$prefix_calls, before)
  expect_identical(out$prefix$calls$call_id, seq_len(before))
  # The prefix and its arms read back as written.
  for (x in list(out$prefix, out$arms$changed)) {
    lines <- format(x)
    expect_identical(format(FocusGroup:::.fg_object_result(focus_group_from_transcript(lines))),
                     lines)
  }
})

test_that("a transcript file continues under the settings it gives", {
  lines <- c(
    paste0('{"type":"session","topic":"library hours","flow":"desire_based",',
           '"max_participant_responses":2}'),
    '{"type":"participant","id":"Chair","is_moderator":true,"persona":"A moderator."}',
    '{"type":"participant","id":"Ana","persona":"A nurse."}',
    '{"type":"participant","id":"Ben","persona":"A baker."}',
    '{"type":"item","item":1,"phase":"engagement_question","text":"Which hours?"}',
    '{"type":"item","item":2,"phase":"exploration_question","text":"What would you change?"}',
    '{"type":"message","speaker_id":"Chair","text":"Which hours matter?","item":1}',
    '{"type":"message","speaker_id":"Ana","text":"Evenings.","item":1}',
    '{"type":"message","speaker_id":"Ben","text":"Mornings.","item":1}')
  rec <- .cont_recorder()
  out <- run_focus_group_arms(
    focus_group_from_transcript(lines), config = .cont_config(),
    arms = list(same = NULL, changed = fg_injection("Weekends.", fg_before_turn(2),
                                                    from = "Ben")),
    final_summary = FALSE, verbose = FALSE, .runner = rec$runner)
  same <- out$arms$same
  expect_identical(same$metadata$flow, "desire_based")
  expect_true(all(same$routing$rule[-1] %in% c("highest_score", "threshold_lowered",
                                               "highest_below_threshold", "addressed")))
  # Item 2 is not delivered as written: the moderator writes its line.
  expect_true(any(same$calls$call_type == "moderator"))
})

test_that("a called-on turn whose item is no call-on point stops the continuation", {
  record <- .cont_record()
  record$metadata$call_on_silent <- list(points = character(), items = integer())
  fg <- focus_group_from_transcript(record)
  expect_error(fg$.__enclos_env__$private$position_at(3L), "not a call-on point")
})

# A record as FocusGroup 0.5.5 saved it without its session object: no
# guide, no flow parameters, no communication styles.
.cont_old_record <- function(result) {
  result$metadata$guide <- NULL
  result$metadata$flow_params <- NULL
  result$participants$communication_style <- NULL
  result$focus_group <- NULL
  result
}

test_that("a 0.5.5 record continues with its recorded phases and flow settings", {
  result <- suppressMessages(run_focus_group(
    "library hours", .cont_config(), n_participants = 3, flow = "desire_based",
    on_desire_failure = "random", max_participant_responses = 2,
    guide = list(Opening = "Welcome, everyone.",
                 Engagement = c("Which library hours matter most to you?",
                                "Who is left out by the current hours?"),
                 Exploration = "What would you change first?",
                 Closing = "Thank you all; this closes the session."),
    moderator_verbatim = c(1, 5), verbose = FALSE, seed = 110, .runner = .cont_runner))
  old <- .cont_old_record(result)
  tr <- result$transcript
  lines <- tr[tr$is_moderator & !duplicated(tr$item) & tr$item > 0, ]

  expect_warning(fg <- focus_group_from_transcript(old), class = "focusgroup_guide_rebuilt")
  w <- tryCatch(focus_group_from_transcript(old), focusgroup_guide_rebuilt = function(w) w)
  # The opening and closing lines were delivered verbatim, so their texts
  # are known; the generated lines of items 2 to 4 are not.
  expect_identical(w$items, 2:4)
  expect_match(conditionMessage(w), "delivers those moderator lines as recorded")
  expect_identical(vapply(fg$question_script, `[[`, character(1), "phase"), lines$phase)
  expect_identical(vapply(fg$question_script, `[[`, character(1), "text"), lines$text)
  expect_true(all(vapply(fg$question_script, function(i) isTRUE(i$verbatim), logical(1))))
  expect_identical(fg$turn_taking_flow$.__enclos_env__$private$on_desire_failure, "random")
  expect_identical(jsonlite::parse_json(format(.fg_object_result(fg))[[1]])$on_desire_failure,
                   "random")

  # A continuation delivers the recorded guide lines, under their phases.
  out <- suppressWarnings(run_focus_group_arms(
    old, .cont_config(), verbose = FALSE, seed = 110,
    arms = list(same = NULL, alt = fg_injection("Mornings.", fg_before_turn(2), from = "P1")),
    .runner = .cont_runner))
  for (arm in out$arms) {
    arm_tr <- arm$transcript
    first <- arm_tr[arm_tr$is_moderator & !duplicated(arm_tr$item) & arm_tr$item > 0, ]
    expect_identical(first$text, lines$text)
    expect_identical(first$phase, lines$phase)
  }

  # Hard negatives: the record with its guide reads without the warning and
  # keeps the guide as run; a human transcript keeps the inferred phases.
  expect_no_warning(fg_new <- focus_group_from_transcript(result))
  expect_identical(fg_new$question_script, result$metadata$guide)
  human <- c('{"type":"session","versions":{"R":"4.5.0"}}',
             '{"type":"message","speaker_id":"Moderator","text":"Which hours?"}',
             '{"type":"message","speaker_id":"Ana","text":"Evenings."}',
             '{"type":"message","speaker_id":"Moderator","text":"Thank you."}')
  expect_no_warning(fg_h <- focus_group_from_transcript(human))
  expect_identical(vapply(fg_h$question_script, `[[`, character(1), "phase"),
                   c("generic_discussion", "closing"))
  # Analysis of the old record does not warn about continuing it.
  expect_no_warning(suppressMessages(analyze_focus_group(old)))
})
