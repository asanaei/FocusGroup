# The moderator calls on the participants who have not spoken: offline, with
# scripted runners.

.co_config <- function() LLMR::llm_config("openai", "example-model")

.co_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

# Equal desire scores, numbered replies, and every request kept.
.co_stub <- function() {
  env <- new.env()
  env$n <- 0L
  env$requests <- list()
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    messages <- experiments$messages[[1]]
    env$requests[[env$n]] <- messages
    prompt <- .co_prompt(messages)
    experiments$response_text <- if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      "6"
    } else if (grepl("Summary:$", prompt)) {
      "A short summary."
    } else {
      sprintf("Reply %03d: evening hours matter to working families.", env$n)
    }
    experiments
  }
  env
}

.co_is_call_on <- function(transcript) {
  vapply(transcript$metadata, function(m) !is.null(m$call_on), logical(1))
}

test_that("the default guide calls on the silent participants at items 2, 4, and 7", {
  stub <- .co_stub()
  result <- run_focus_group(
    "library hours", .co_config(), n_participants = 6, flow = "desire_based",
    seed = 110, verbose = FALSE, .runner = stub$runner)
  tr <- result$transcript
  routing <- result$routing
  participants <- paste0("P", 1:6)

  expect_identical(result$metadata$call_on_silent,
                   list(points = c("opening", "midpoint", "end"), items = c(2L, 4L, 7L)))
  lines <- tr[.co_is_call_on(tr), ]
  expect_identical(sort(unique(lines$item)), c(2L, 4L, 7L))

  # At each point the moderator calls on exactly the participants with no
  # message since the previous point, in roster order.
  mark <- 0L
  for (item in c(2L, 4L, 7L)) {
    at <- lines[lines$item == item, ]
    before <- tr[tr$message_id > mark & tr$message_id < min(at$message_id), ]
    silent <- setdiff(participants, before$speaker_id)
    called <- vapply(at$metadata, function(m) m$call_on$participant, character(1))
    expect_identical(called, silent, info = item)
    last_reply <- max(at$message_id) + 1L
    mark <- last_reply
  }

  # Each line names the participant's handle, is delivered as written, and
  # is followed by that participant.
  for (i in seq_len(nrow(lines))) {
    meta <- lines$metadata[[i]]
    expect_identical(lines$text[[i]], paste0(meta$call_on$handle,
      ", I'd like to hear from you as well. What are your thoughts?"))
    expect_true(meta$verbatim)
    expect_true(is.na(lines$model[[i]]))
    reply <- tr[tr$message_id == lines$message_id[[i]] + 1L, ]
    expect_identical(reply$speaker_id, meta$call_on$participant)
    expect_true(reply$metadata[[1]]$called_on)
  }
  expect_false(any(lines$message_id %in% result$calls$message_id))

  # The replies have routing rows "called_on" that point to their lines, and
  # no desire score precedes them.
  called <- routing[routing$rule == "called_on", ]
  expect_identical(nrow(called), nrow(lines))
  expect_identical(called$trigger_message_id, lines$message_id)
  expect_identical(called$message_id, lines$message_id + 1L)
  expect_true(all(lengths(called$scores) == 0L))
  expect_identical(lengths(called$candidates), rep(1L, nrow(called)))
  expect_true(all(is.na(routing$trigger_message_id[routing$rule != "called_on"])))
  expect_identical(routing$turn, seq_len(nrow(routing)))
  # Every flow pick scores six or five participants; the called turns add
  # none.
  flow_picks <- sum(routing$rule != "called_on")
  desire <- sum(result$calls$call_type == "desire")
  expect_identical(desire, 6L * 6L + 5L * 6L)
  expect_identical(flow_picks, 12L)

  # The called participant's instruction carries the cue; no other turn's does.
  cue <- "The moderator has asked you directly for your view."
  participant_calls <- result$calls[result$calls$call_type == "participant", ]
  has_cue <- vapply(participant_calls$messages, function(m) {
    grepl(cue, m[[length(m)]]$content, fixed = TRUE)
  }, logical(1))
  expect_identical(participant_calls$message_id[has_cue], called$message_id)

  # Everyone has spoken by the end of item 2.
  spoke <- unique(tr$speaker_id[tr$item <= 2L & tr$speaker_id %in% participants])
  expect_setequal(spoke, participants)

  # The participation balance counts the replies to call-ons.
  balance <- result$focus_group$analyze_participation_balance()$participation_stats
  counts <- table(factor(called$selected_id, participants))
  expect_identical(as.integer(balance$called_on[match(participants, balance$speaker_id)]),
                   as.integer(counts))
  expect_identical(balance$called_on[balance$speaker_id == "MOD"], 0L)
})

test_that("with call-ons off the session runs as before, and round robin keeps its rotation", {
  stub <- .co_stub()
  plain <- run_focus_group(
    "library hours", .co_config(), n_participants = 3, flow = "round_robin",
    max_participant_responses = 2, call_on_silent = "none", verbose = FALSE,
    .runner = stub$runner)
  expect_identical(plain$transcript$speaker_id, c(
    "System", "MOD", "MOD", "P1", "P2", "MOD", "P3", "P1", "MOD", "P2", "P3",
    "MOD", "P1", "P2", "MOD", "P3", "P1", "MOD", "P2", "P3", "MOD"))
  expect_true(all(plain$routing$rule == "rotation"))
  expect_false(any(.co_is_call_on(plain$transcript)))
  expect_identical(plain$metadata$call_on_silent,
                   list(points = character(), items = integer()))
  expect_identical(plain$metadata$estimated_calls, 8L + 12L + 1L)
  off <- run_focus_group(
    "library hours", .co_config(), n_participants = 2, flow = "round_robin",
    guide = list(Engagement = "Q?", Closing = "Close."), call_on_silent = FALSE,
    max_participant_responses = 1, verbose = FALSE, .runner = .co_stub()$runner)
  expect_identical(off$transcript$speaker_id, c("System", "MOD", "P1", "MOD"))

  # With call-ons, P3 is called on at the icebreaker; the rotation goes on
  # from where it was, so P3 opens the next question.
  stub <- .co_stub()
  on <- run_focus_group(
    "library hours", .co_config(), n_participants = 3, flow = "round_robin",
    max_participant_responses = 2, verbose = FALSE, .runner = stub$runner)
  tr <- on$transcript
  expect_identical(tr$speaker_id[tr$item == 2L], c("MOD", "P1", "P2", "MOD", "P3"))
  expect_identical(tr$speaker_id[tr$item == 3L], c("MOD", "P3", "P1"))
  expect_identical(on$routing$rule[1:3], c("rotation", "rotation", "called_on"))
  expect_identical(sum(.co_is_call_on(tr)), 1L)
  expect_identical(on$metadata$estimated_calls, 8L + 12L + 9L + 1L)
})

test_that("call-on points resolve against the guide and are checked before any call", {
  phases <- c("opening", "icebreaker_question", "engagement_question",
              "engagement_question", "exploration_question", "closing")
  expect_identical(FocusGroup:::.fg_call_on_points(c("opening", "midpoint", "end"), phases),
                   list(points = c("opening", "midpoint", "end"), items = c(2L, 3L, 5L)))
  expect_identical(FocusGroup:::.fg_call_on_points("end", phases),
                   list(points = "end", items = 5L))
  expect_identical(FocusGroup:::.fg_call_on_points(c(5, 3, 3), phases),
                   list(points = c("item:3", "item:5"), items = c(3L, 5L)))
  for (off in list("none", FALSE, NULL)) {
    expect_identical(FocusGroup:::.fg_call_on_points(off, phases),
                     list(points = character(), items = integer()))
  }
  # One discussion item: the three points fall on it and count once.
  one <- c("opening", "engagement_question", "closing")
  expect_identical(FocusGroup:::.fg_call_on_points(c("opening", "midpoint", "end"), one),
                   list(points = "opening", items = 2L))
  # No discussion item: no point.
  expect_identical(FocusGroup:::.fg_call_on_points(c("opening", "end"), c("opening", "closing")),
                   list(points = character(), items = integer()))
  expect_error(FocusGroup:::.fg_call_on_points(1, phases), "1 \\(opening\\)")
  expect_error(FocusGroup:::.fg_call_on_points(c(2, 6), phases), "6 \\(closing\\)")
  expect_error(FocusGroup:::.fg_call_on_points(7, phases), "from 1 to 6")
  expect_error(FocusGroup:::.fg_call_on_points(2.5, phases), "whole numbers")
  expect_error(FocusGroup:::.fg_call_on_points("start", phases), "opening")
  expect_error(FocusGroup:::.fg_call_on_points(c("none", "end"), phases), "opening")
  expect_error(FocusGroup:::.fg_call_on_points(character(), phases), "opening")
  expect_error(FocusGroup:::.fg_call_on_points(NA, phases), "whole numbers")
  expect_identical(FocusGroup:::.fg_call_on_points(TRUE, phases),
                   FocusGroup:::.fg_call_on_points(c("opening", "midpoint", "end"), phases))

  stub <- .co_stub()
  run <- function(...) run_focus_group(
    "library hours", .co_config(), n_participants = 2,
    guide = list(Opening = "Welcome.", Engagement = "Which hours?", Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    .runner = stub$runner, ...)
  expect_error(run(call_on_silent = 1), "not answer")
  expect_error(run(call_on_silent = "sometimes"), "opening")
  expect_error(run(call_on_text = "Please speak up."), "\\{participant\\}")
  expect_error(run(call_on_text = c("{participant}?", "{participant}!")), "one string")
  expect_identical(stub$n, 0L)

  # A one-question guide gets one point, and the custom line names the handle.
  result <- run(call_on_text = "{participant}, what is your view?")
  lines <- result$transcript[.co_is_call_on(result$transcript), ]
  expect_identical(lines$text, "P2, what is your view?")
  expect_identical(lines$metadata[[1]]$call_on$point, "opening")
  expect_identical(result$metadata$call_on_text, "{participant}, what is your view?")
  expect_identical(result$metadata$call_on_silent$items, 2L)
})

test_that("arms branched at item 4 share the opening call-ons in their prefix", {
  stub <- .co_stub()
  out <- run_focus_group_arms(
    "library hours", .co_config(),
    arms = list(objection = fg_injection("Longer hours would cut two programs.",
                                         at = fg_after_item(4), from = "Guest expert"),
                control = NULL),
    n_participants = 4, flow = "desire_based", max_participant_responses = 2,
    seed = 110, verbose = FALSE, .runner = stub$runner)
  m <- out$metadata$prefix_messages
  prefix <- out$prefix$transcript
  expect_true(any(.co_is_call_on(prefix) & prefix$item == 2L))
  shared <- c("message_id", "round", "item", "speaker_id", "handle", "text", "phase")
  for (arm in out$arms) {
    expect_identical(arm$transcript[seq_len(m), shared], prefix[, shared])
    expect_identical(arm$routing$rule[seq_len(nrow(out$prefix$routing))],
                     out$prefix$routing$rule)
    expect_true(arm$metadata$complete)
  }
  expect_identical(out$arms$objection$metadata$call_on_silent$items, c(2L, 4L, 7L))
  # The estimate is one session's, reserve included, times the arms.
  expect_identical(out$metadata$estimated_calls,
                   2L * (8L + 12L + 12L * 4L + 3L * 4L + 1L))
})

test_that("a session paused at a call-on turn resumes after the line, without repeating it", {
  stub <- .co_stub()
  # Round robin with three participants: turns 1 and 2 answer the icebreaker
  # and turn 3 is P3's reply to the call-on line.
  out <- run_focus_group_arms(
    "library hours", .co_config(),
    arms = list(note = fg_injection("A note from outside the room.",
                                    at = fg_before_turn(3), from = "Guest expert"),
                control = NULL),
    n_participants = 3, flow = "round_robin", max_participant_responses = 2,
    seed = 110, verbose = FALSE, .runner = stub$runner)
  prefix <- out$prefix$transcript
  last <- prefix[nrow(prefix), ]
  expect_identical(last$metadata[[1]]$call_on$participant, "P3")
  for (name in names(out$arms)) {
    tr <- out$arms[[name]]$transcript
    lines <- tr[.co_is_call_on(tr) & tr$item == 2L, ]
    expect_identical(nrow(lines), 1L, info = name)
    turn3 <- out$arms[[name]]$routing[3, ]
    expect_identical(turn3$rule, "called_on")
    expect_identical(turn3$selected_id, "P3")
    expect_identical(turn3$trigger_message_id, lines$message_id)
  }
  note <- out$arms$note$transcript
  at <- which(note$speaker_id == "Guest expert")
  expect_identical(at, nrow(prefix) + 1L)
  expect_identical(note$speaker_id[[at + 1L]], "P3")
  expect_identical(out$arms$control$transcript$speaker_id[[nrow(prefix) + 1L]], "P3")
})

test_that("the estimate holds the call-on reserve and the default session fits max_calls", {
  cfg <- .co_config()
  count <- 0L
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) {
      count <<- count + 1L
      data.frame(response_text = "8", sent = 0, rec = 0)
    },
    call_llm_broadcast = function(config, messages, ...) {
      count <<- count + length(messages)
      data.frame(response_text = rep("8", length(messages)), success = TRUE,
                 sent = 0, rec = 0)
    }, .package = "LLMR")
  call <- function(...) run_focus_group("Hours", cfg, demographics = data.frame(age = 31:36),
                                        verbose = FALSE, seed = 110, ...)
  # 93 outputs as in 0.5.4 and up to six replies at each of three points.
  expect_error(call(max_calls = 110), "Estimated model outputs \\(111\\)")
  expect_identical(count, 0L)
  expect_identical(eval(formals(run_focus_group)$max_calls), 120L)
  expect_identical(eval(formals(run_focus_group_arms)$max_calls), 120L)
  result <- call()
  expect_identical(result$metadata$estimated_calls, 111L)
  expect_lte(count, 111L)
  expect_identical(count, nrow(result$calls))
  expect_identical(call(final_summary = FALSE, max_calls = 110)$metadata$estimated_calls, 110L)
  expect_identical(call(call_on_silent = "end", max_calls = 99)$metadata$estimated_calls, 99L)
})
