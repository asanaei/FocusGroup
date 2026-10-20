# Injected messages, sessions branched from one prefix, private probes, and
# verbatim moderator lines, all offline with scripted runners.

.inj_config <- function() LLMR::llm_config("openai", "example-model")

.inj_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

# A runner that numbers its replies, so that every call's text is distinct,
# and keeps every prompt it receives.
.inj_stub <- function() {
  env <- new.env()
  env$n <- 0L
  env$prompts <- character()
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    prompt <- .inj_prompt(experiments$messages[[1]])
    env$prompts <- c(env$prompts, prompt)
    experiments$response_text <- if (grepl("private question", prompt, fixed = TRUE)) {
      "5"
    } else if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      "6"
    } else if (grepl("Summary:$", prompt)) {
      "A short summary of the session."
    } else {
      sprintf("Reply %03d: evening hours matter to working families.", env$n)
    }
    experiments
  }
  env
}

.objection <- "A reasoned objection: longer hours would cut two weekday programs."
.probe_question <- "Where do you stand on longer library hours, from 1 to 7?"

test_that("two arms injected at guide item 4 share the prefix and each run the full remainder", {
  stub <- .inj_stub()
  out <- run_focus_group_arms(
    topic = "library hours", config = .inj_config(), n_participants = 3,
    arms = list(
      objection = fg_injection(.objection, at = fg_after_item(4),
                               from = "Guest expert"),
      control = NULL),
    probes = fg_probe(.probe_question, at = list(fg_after_item(4), "end"),
                      label = "position"),
    flow = "round_robin", max_participant_responses = 2, seed = 110,
    verbose = FALSE, .runner = stub$runner)
  expect_s3_class(out, "focus_group_arms")
  objection <- out$arms$objection$transcript
  control <- out$arms$control$transcript
  m <- out$metadata$prefix_messages

  # Identical messages before the injection, in both arms and in the prefix.
  shared <- c("message_id", "round", "speaker_id", "text", "phase")
  expect_identical(objection[seq_len(m), shared], control[seq_len(m), shared])
  expect_identical(objection[seq_len(m), shared], out$prefix$transcript[, shared])
  expect_identical(out$arms$objection$metadata$branch$prefix_hash,
                   out$arms$control$metadata$branch$prefix_hash)
  # The prefix ends with the moderator's line for item 4, the second
  # engagement question. Before it come the four guide lines and the
  # call-on line for P3, the one participant round robin left silent at the
  # icebreaker.
  expect_identical(objection$speaker_id[[m]], "MOD")
  expect_identical(objection$text[[m]] , out$prefix$transcript$text[[m]])
  expect_identical(sum(objection$is_moderator[seq_len(m)]), 5L)
  expect_identical(sum(vapply(objection$metadata[seq_len(m)],
                              function(x) !is.null(x$call_on), logical(1))), 1L)
  expect_identical(objection$phase[[m]], "engagement_question")

  # The injected message is flagged with its arm and cut point.
  injected <- objection[m + 1L, ]
  expect_identical(injected$speaker_id, "Guest expert")
  expect_identical(injected$text, .objection)
  flag <- injected$metadata[[1]]
  expect_true(flag$injected)
  expect_identical(flag$injection$arm, "objection")
  expect_identical(flag$injection$label, "objection")
  expect_identical(flag$injection$point, "fg_after_item(4)")
  expect_identical(flag$injection$cut_after_message_id, objection$message_id[[m]])
  expect_false(any(vapply(control$metadata, function(x) isTRUE(x$injected), logical(1))))
  expect_identical(out$injections$arm, "objection")
  expect_identical(out$injections$message_id, injected$message_id)

  # A full remainder in each branch: every later question, the closing, and
  # the final summary. Round robin gives everyone a turn after the first
  # call-on, so the later call-on points find no one silent.
  for (arm in out$arms) {
    tr <- arm$transcript
    expect_true(arm$metadata$complete)
    expect_identical(sum(tr$is_moderator), 9L)
    expect_identical(sum(tr$phase == "exploration_question" & tr$is_moderator), 3L)
    expect_identical(tr$phase[[nrow(tr)]], "closing")
    expect_identical(arm$summary, "A short summary of the session.")
  }
  expect_identical(nrow(objection), nrow(control) + 1L)
  # The arms continue separately: their remainders differ.
  expect_false(identical(objection$text[-seq_len(m + 1L)],
                         control$text[-seq_len(m)]))

  # The prefix was run once: the runner saw the prefix calls once and each
  # arm's remainder once.
  prefix_calls <- out$arms$objection$metadata$branch$prefix_calls
  remainder <- function(arm) nrow(out$arms[[arm]]$calls) - prefix_calls
  expect_identical(stub$n, prefix_calls + remainder("objection") + remainder("control"))
  expect_identical(out$arms$control$calls[seq_len(prefix_calls), "raw_text"],
                   out$arms$objection$calls[seq_len(prefix_calls), "raw_text"])

  # Probe answers are saved outside the shared log: asked once in the prefix
  # and at the end of each arm, never logged, and never in a later prompt.
  probes <- out$probes
  expect_identical(nrow(probes), 9L)
  expect_identical(sum(is.na(probes$arm) & probes$point == "fg_after_item(4)"), 3L)
  expect_setequal(probes$arm[probes$point == "end"], c("objection", "control"))
  expect_true(all(probes$answer == "5"))
  expect_true(all(probes$question == .probe_question))
  for (arm in out$arms) {
    expect_false(any(grepl(.probe_question, arm$transcript$text, fixed = TRUE)))
    later <- arm$calls[arm$calls$call_type != "probe", ]
    expect_false(any(vapply(later$messages, function(msgs) {
      grepl(.probe_question, .inj_prompt(msgs), fixed = TRUE)
    }, logical(1))))
  }
  # The pre-injection probe follows the moderator's line at item 4.
  expect_true(all(probes$after_message_id[is.na(probes$arm)] == m))

  # Usage counts the prefix once.
  usage_total <- out$prefix$usage$total
  expect_true(is.list(out$usage))
  expect_output(print(out), "Branch point: fg_after_item\\(4\\)")
})

test_that("a single run takes injections and probes, and verbatim lines", {
  stub <- .inj_stub()
  guide <- list(Opening = "Welcome, everyone, to tonight's discussion.",
                Engagement = c("Which hours matter most to you?", "Who is left out?"),
                Closing = "Thank you all; this closes the session.")
  result <- run_focus_group(
    topic = "library hours", config = .inj_config(), n_participants = 2,
    guide = guide, flow = "round_robin", max_participant_responses = 2,
    injections = fg_injection(.objection, at = fg_before_turn(2)),
    probes = fg_probe(.probe_question, at = "start"),
    moderator_verbatim = c(1, 4), verbose = FALSE, .runner = stub$runner)
  tr <- result$transcript

  # Before participant turn 2: between the first and the second participant
  # message, from the moderator.
  first_two <- which(tr$speaker_id %in% c("P1", "P2"))[1:2]
  injected <- which(vapply(tr$metadata, function(x) isTRUE(x$injected), logical(1)))
  expect_identical(injected, first_two[[1]] + 1L)
  expect_identical(tr$speaker_id[[injected]], "MOD")
  expect_true(tr$is_moderator[[injected]])
  expect_identical(result$injections$turn, 2L)

  # The verbatim items are the moderator's lines as written, with no call.
  moderator <- tr[tr$is_moderator & !vapply(tr$metadata, function(x) isTRUE(x$injected), logical(1)), ]
  expect_identical(moderator$text[[1]], guide$Opening)
  expect_identical(moderator$text[[4]], guide$Closing)
  expect_true(moderator$metadata[[1]]$verbatim)
  expect_true(is.na(moderator$model[[1]]))
  expect_identical(sum(result$calls$call_type == "moderator"), 2L)
  expect_false(any(moderator$message_id[c(1, 4)] %in% result$calls$message_id))
  expect_false(isTRUE(moderator$metadata[[2]]$verbatim))

  # The start probe is asked before any moderator line.
  expect_identical(result$probes$point, c("start", "start"))
  expect_identical(result$probes$after_message_id, c(1L, 1L))
  expect_identical(nrow(result$probes), 2L)
  expect_true(all(result$calls$call_type[1:2] == "probe"))
})

test_that("an injection for the participants alone never reaches the moderator", {
  stub <- .inj_stub()
  result <- run_focus_group(
    topic = "library hours", config = .inj_config(), n_participants = 2,
    guide = list(Engagement = c("Which hours matter most?", "Who is left out?"),
                 Closing = "Close the session."),
    flow = "desire_based", max_participant_responses = 1,
    injections = fg_injection(.objection, at = fg_after_item(1),
                              visibility = "participants"),
    verbose = FALSE, .runner = stub$runner)
  calls <- result$calls
  injected_id <- result$injections$message_id
  after <- calls$call_id > min(calls$call_id[calls$call_type == "desire"])
  sees <- vapply(calls$messages, function(msgs) {
    grepl(.objection, .inj_prompt(msgs), fixed = TRUE)
  }, logical(1))
  expect_true(all(sees[after & calls$call_type %in% c("participant", "desire")]))
  expect_false(any(sees[calls$call_type == "moderator"]))
  expect_identical(result$transcript$metadata[[injected_id]]$injection$visibility,
                   "participants")

  # With shared visibility the moderator reads it as its own line.
  stub <- .inj_stub()
  shared <- run_focus_group(
    topic = "library hours", config = .inj_config(), n_participants = 2,
    guide = list(Engagement = c("Which hours matter most?", "Who is left out?"),
                 Closing = "Close the session."),
    flow = "round_robin", max_participant_responses = 1,
    injections = fg_injection(.objection, at = fg_after_item(1)),
    verbose = FALSE, .runner = stub$runner)
  later_moderator <- shared$calls[shared$calls$call_type == "moderator", ][-1, ]
  expect_true(all(vapply(later_moderator$messages, function(msgs) {
    grepl(.objection, .inj_prompt(msgs), fixed = TRUE)
  }, logical(1))))
})

test_that("an injected run reproduces its arm under the same seed", {
  # Replies depend only on the prompt, and the probabilistic flow draws its
  # speakers from the random-number stream.
  runner <- function(experiments, ...) {
    prompt <- .inj_prompt(experiments$messages[[1]])
    experiments$response_text <- if (grepl("Summary:$", prompt)) "A short summary." else
      sprintf("I reply after %d characters of context, about evening hours.",
              nchar(prompt))
    experiments
  }
  objection <- fg_injection(.objection, at = fg_after_item(3), from = "Guest expert")
  args <- list(topic = "library hours", config = .inj_config(), n_participants = 3,
               flow = "probabilistic", max_participant_responses = 2, seed = 110,
               verbose = FALSE, .runner = runner)
  arms <- do.call(run_focus_group_arms,
                  c(args, list(arms = list(objection = objection, control = NULL))))
  single <- do.call(run_focus_group, c(args, list(injections = objection)))
  plain <- do.call(run_focus_group, args)
  cols <- c("speaker_id", "text")
  expect_identical(single$transcript[, cols], arms$arms$objection$transcript[, cols])
  expect_identical(plain$transcript[, cols], arms$arms$control$transcript[, cols])
})

test_that("points, injections, probes, and arms are checked before any call", {
  expect_error(fg_after_item(0), "positive whole number")
  expect_error(fg_before_turn(1.5), "positive whole number")
  expect_error(fg_injection("", fg_after_item(1)), "non-empty string")
  expect_error(fg_injection("Text.", at = 4), "fg_after_item")
  expect_error(fg_injection("Text.", fg_after_item(1), visibility = "moderator"),
               "should be one of")
  expect_error(fg_injection("Text.", "end"), "fg_after_item")
  expect_error(fg_probe("Q?", at = list("start", "start")), "same point")
  expect_error(fg_probe("Q?", at = "middle"), "start")

  stub <- .inj_stub()
  run <- function(...) run_focus_group(
    topic = "library hours", config = .inj_config(), n_participants = 2,
    guide = list(Engagement = "Which hours?", Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    .runner = stub$runner, ...)
  e <- tryCatch(run(injections = fg_injection("Text.", fg_after_item(3))),
                error = identity)
  expect_match(conditionMessage(e), "beyond the 2 item")
  expect_false(inherits(e, "focusgroup_incomplete_session"))
  # One turn for the question and one for each participant the moderator
  # may call on at its end.
  expect_error(run(injections = fg_injection("Text.", fg_before_turn(4))),
               "beyond the 3 participant turn")
  expect_error(run(injections = fg_injection("Text.", fg_before_turn(2)),
                   call_on_silent = "none"),
               "beyond the 1 participant turn")
  expect_error(run(injections = fg_injection("Text.", fg_after_item(1), from = "P1")),
               "participant")
  expect_error(run(injections = "Text."), "fg_injection")
  expect_error(run(probes = fg_probe("Q?", "end", participants = "P9")), "P9")
  expect_error(run(moderator_verbatim = 3), "moderator_verbatim")
  expect_error(run_focus_group(
    topic = "library hours", config = .inj_config(), n_participants = 1,
    guide = NULL, moderator_verbatim = TRUE, flow = "round_robin",
    verbose = FALSE, .runner = stub$runner), "no text")

  arms_run <- function(arms) run_focus_group_arms(
    topic = "library hours", config = .inj_config(), arms = arms,
    n_participants = 2, guide = list(Engagement = c("Q1?", "Q2?"), Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    .runner = stub$runner)
  e <- tryCatch(arms_run(list(a = fg_injection("A.", fg_after_item(1)),
                              b = fg_injection("B.", fg_after_item(2)))),
                error = identity)
  expect_match(conditionMessage(e), "same point")
  expect_false(inherits(e, "focusgroup_incomplete_arms"))
  expect_error(arms_run(list(a = fg_injection("A.", fg_after_item(9)), b = NULL)),
               "beyond the 3 item")
  expect_error(arms_run(list(fg_injection("A.", fg_after_item(1)), NULL)), "name")
  expect_error(arms_run(list(a = NULL, b = NULL)), "At least one arm")
  expect_error(arms_run(list(a = fg_injection("A.", fg_after_item(1)))), "at least two")
  expect_identical(stub$n, 0L)
})

test_that("a point the session never reaches is reported", {
  stub <- .inj_stub()
  agents <- create_agents(2, config = .inj_config(),
                          direct_persona_descriptions = c("A.", "B."),
                          .runner = stub$runner)
  fg <- FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow("round_robin", agents, "MOD"),
    question_script = list(list(phase = "engagement_question", text = "Q1?"),
                           list(phase = "engagement_question", text = "Q2?"),
                           list(phase = "closing", text = "Close.")),
    max_participant_responses = 1, call_on_silent = "none")
  expect_warning(
    fg$run_simulation(num_rounds = 1,
                      injections = fg_injection(.objection, fg_after_item(2)),
                      probes = fg_probe(.probe_question, "end")),
    "was not delivered")
  expect_false(fg$records$injections[[1]]$delivered)
  expect_length(fg$records$probes, 2L)

  # An arm run whose branch point is not reached stops without running arms.
  fg2 <- FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow("round_robin", agents, "MOD"),
    question_script = list(list(phase = "engagement_question", text = "Q1?"),
                           list(phase = "engagement_question", text = "Q2?"),
                           list(phase = "closing", text = "Close.")),
    max_participant_responses = 1, call_on_silent = "none")
  before <- stub$n
  expect_error(
    fg2$run_arms(list(a = fg_injection("A.", fg_after_item(2)), b = NULL),
                 num_rounds = 1),
    class = "focusgroup_injection_unreached")
  # The prefix ran its first round only, the question and one reply; no arm
  # and no final summary.
  expect_identical(stub$n - before, 2L)
})

test_that("a failing arm returns the completed arms and the prefix", {
  calls <- 0L
  base <- .inj_stub()
  runner <- function(experiments, ...) {
    calls <<- calls + 1L
    # Calls 1-3 are the prefix, 4-6 the first arm, and 8 the second arm's
    # closing line.
    if (calls == 8L) stop("The provider is unavailable.")
    base$runner(experiments, ...)
  }
  e <- tryCatch(run_focus_group_arms(
    topic = "library hours", config = .inj_config(),
    arms = list(objection = fg_injection(.objection, fg_after_item(2)), control = NULL),
    n_participants = 2, guide = list(Engagement = c("Q1?", "Q2?"), Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    call_on_silent = "none", .runner = runner), focusgroup_incomplete_arms = identity)
  expect_s3_class(e, "focusgroup_incomplete_arms")
  expect_match(conditionMessage(e$parent), "unavailable")
  partial <- e$partial
  expect_s3_class(partial, "focus_group_arms")
  expect_false(partial$metadata$complete)
  expect_identical(e$arm, "control")
  expect_identical(names(partial$arms), c("objection", "control"))
  expect_true(partial$arms$objection$metadata$complete)
  failed <- partial$arms$control
  expect_false(failed$metadata$complete)
  expect_identical(failed$metadata$failure$phase, "closing")
  expect_identical(failed$calls$error[[nrow(failed$calls)]], "The provider is unavailable.")
  expect_identical(nrow(partial$prefix$transcript), 4L)
})
