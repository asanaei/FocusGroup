# Self-labels removed from replies, the optional final summary, and the point
# before the closing line. Offline, with scripted runners.

.so_config <- function() LLMR::llm_config("openai", "example-model")

.so_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

.so_stub <- function(reply = NULL) {
  env <- new.env()
  env$n <- 0L
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    messages <- experiments$messages[[1]]
    prompt <- .so_prompt(messages)
    text <- if (!is.null(reply)) reply(messages, prompt) else NULL
    experiments$response_text <- text %||% if (grepl("private question", prompt, fixed = TRUE)) {
      "5"
    } else if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
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

test_that("a label naming the speaker itself is removed, another speaker's is kept", {
  strip <- function(text, labels) FocusGroup:::.fg_strip_self_label(text, labels)
  moderator <- c("Moderator", "MOD")
  removed <- list(
    list("**MOD:** Welcome, everyone.", moderator, "Welcome, everyone."),
    list("**Moderator**: Welcome, everyone.", moderator, "Welcome, everyone."),
    list("MOD: Welcome.", moderator, "Welcome."),
    list("moderator:Welcome.", moderator, "Welcome."),
    list("P3: I think evenings matter.", c("P3", "teacher"), "I think evenings matter."),
    list("  **P3:**  I think so.", c("P3", "teacher"), "I think so."),
    list("teacher: I think so.", c("P3", "teacher"), "I think so."),
    list("J.R.: I think so.", "J.R.", "I think so."))
  for (case in removed) {
    out <- strip(case[[1]], case[[2]])
    expect_true(out$removed, info = case[[1]])
    expect_identical(out$text, case[[3]], info = case[[1]])
  }
  kept <- list(
    list("P2: I think evenings matter.", c("P3", "teacher")),
    list("P30: I think so.", c("P3", "teacher")),
    list("Moderator: you asked about cost.", c("P3", "teacher")),
    list("P3 said: evenings matter.", c("P3", "teacher")),
    list("I agree. P3: yes.", c("P3", "teacher")),
    list("MODERATE: hours matter.", moderator),
    list("JxR.: I think so.", "J.R."),
    list("Welcome, everyone.", moderator))
  for (case in kept) {
    out <- strip(case[[1]], case[[2]])
    expect_false(out$removed, info = case[[1]])
    expect_identical(out$text, case[[1]], info = case[[1]])
  }
})

test_that("self-labels are cleaned in a session and the raw reply stays in the calls", {
  stub <- .so_stub(function(messages, prompt) {
    system <- messages[[1]]$content
    if (grepl("MODERATOR", prompt, fixed = TRUE)) {
      "**MOD:** Welcome, everyone; which hours matter most to you?"
    } else if (grepl("In this discussion you are P3.", system, fixed = TRUE) &&
               !grepl("Desire to talk", prompt, fixed = TRUE)) {
      "P3: Evening hours matter most for working parents like me."
    } else if (grepl("In this discussion you are P2.", system, fixed = TRUE) &&
               !grepl("Desire to talk", prompt, fixed = TRUE)) {
      "P3: you said evenings, but mornings matter for retirees."
    } else {
      NULL
    }
  })
  result <- run_focus_group(
    "library hours", .so_config(), n_participants = 3,
    guide = list(Engagement = "Which hours matter?", Closing = "Close."),
    flow = "round_robin", max_participant_responses = 3, call_on_silent = "none",
    verbose = FALSE, .runner = stub$runner)
  tr <- result$transcript
  flagged <- vapply(tr$metadata, function(m) isTRUE(m$self_label_removed), logical(1))
  moderator <- tr[tr$is_moderator, ]
  expect_identical(moderator$text[[1]], "Welcome, everyone; which hours matter most to you?")
  expect_true(moderator$metadata[[1]]$self_label_removed)
  p3 <- tr[tr$speaker_id == "P3", ]
  expect_identical(p3$text, "Evening hours matter most for working parents like me.")
  expect_true(p3$metadata[[1]]$self_label_removed)
  p2 <- tr[tr$speaker_id == "P2", ]
  expect_identical(p2$text, "P3: you said evenings, but mornings matter for retirees.")
  expect_null(p2$metadata[[1]]$self_label_removed)
  expect_identical(sum(flagged), 3L)
  calls <- result$calls
  expect_true(any(calls$raw_text == "**MOD:** Welcome, everyone; which hours matter most to you?"))
  expect_true(any(calls$raw_text == "P3: Evening hours matter most for working parents like me."))
  expect_true(any(calls$cleaned_text == "Evening hours matter most for working parents like me.",
                  na.rm = TRUE))
})

test_that("final_summary = FALSE makes no summary call", {
  stub <- .so_stub()
  run <- function(...) run_focus_group(
    "library hours", .so_config(), n_participants = 2,
    guide = list(Engagement = "Which hours matter?", Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    .runner = stub$runner, ...)
  with <- run()
  without <- run(final_summary = FALSE)
  expect_identical(with$summary, "A short summary.")
  expect_identical(without$summary, character())
  expect_false("summary" %in% without$calls$call_type)
  expect_true("summary" %in% with$calls$call_type)
  expect_true(with$metadata$final_summary)
  expect_false(without$metadata$final_summary)
  expect_identical(without$metadata$estimated_calls, with$metadata$estimated_calls - 1L)
  expect_identical(nrow(without$calls), nrow(with$calls) - 1L)
  expect_error(run(final_summary = NA), "final_summary")

  arms <- run_focus_group_arms(
    "library hours", .so_config(),
    arms = list(a = fg_injection("A note.", fg_after_item(1), from = "Guest expert"), b = NULL),
    n_participants = 2, guide = list(Engagement = "Which hours matter?", Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    final_summary = FALSE, .runner = stub$runner)
  for (arm in arms$arms) {
    expect_identical(arm$summary, character())
    expect_false("summary" %in% arm$calls$call_type)
  }
  expect_identical(arms$metadata$estimated_calls, 2L * without$metadata$estimated_calls)

  agents <- create_agents(2, .so_config(), direct_persona_descriptions = c("A.", "B."),
                          .runner = stub$runner)
  fg <- FocusGroup$new("library hours", "Test.", agents, "MOD",
                       create_conversation_flow("round_robin", agents, "MOD"),
                       question_script = list(list(phase = "engagement_question", text = "Q?")))
  fg$run_simulation(final_summary = FALSE)
  expect_null(fg$final_summary)
})

test_that("the before_closing point comes after the discussion and before the closing line", {
  stub <- .so_stub()
  question <- "From 1 to 7, how much do evening hours matter to you?"
  result <- run_focus_group(
    "library hours", .so_config(), n_participants = 3,
    guide = list(Engagement = c("Which hours matter?", "Who is left out?"),
                 Closing = "Thank you all; this closes the session."),
    moderator_verbatim = 3, flow = "round_robin", max_participant_responses = 1,
    probes = fg_probe(question, at = list("before_closing", "end"), label = "hours"),
    injections = fg_injection("One last thought before we close.", at = "before_closing"),
    verbose = FALSE, .runner = stub$runner)
  tr <- result$transcript
  closing <- which(tr$phase == "closing")
  expect_identical(length(closing), 1L)
  # The injection is the message just before the closing line, after the
  # call-ons at the last question.
  expect_true(isTRUE(tr$metadata[[closing - 1L]]$injected))
  expect_identical(tr$metadata[[closing - 1L]]$injection$point, "before_closing")
  expect_false(is.null(tr$metadata[[closing - 2L]]$called_on))
  probes <- result$probes
  before <- probes[probes$point == "before_closing", ]
  end <- probes[probes$point == "end", ]
  expect_identical(nrow(before), 3L)
  expect_identical(nrow(end), 3L)
  expect_true(all(before$after_message_id == tr$message_id[[closing - 2L]]))
  expect_true(all(end$after_message_id == tr$message_id[[closing]]))
  # The probe asked before the closing does not see it; the one at the end does.
  closing_line <- "Thank you all; this closes the session."
  sees <- function(call_ids) vapply(call_ids, function(id) {
    grepl(closing_line, .so_prompt(result$calls$messages[[id]]), fixed = TRUE)
  }, logical(1))
  expect_false(any(sees(before$call_id)))
  expect_true(all(sees(end$call_id)))
  # The probe answers stay out of the transcript.
  expect_false(any(grepl(question, tr$text, fixed = TRUE)))
})

test_that("without a closing item the before_closing point comes just before the end", {
  stub <- .so_stub()
  result <- run_focus_group(
    "library hours", .so_config(), n_participants = 2,
    guide = list(Engagement = "Which hours matter?"),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    probes = fg_probe("From 1 to 7?", at = list("end", "before_closing")),
    .runner = stub$runner)
  probes <- result$probes
  expect_identical(probes$point, c("before_closing", "before_closing", "end", "end"))
  expect_identical(length(unique(probes$after_message_id)), 1L)

  # A session cut short by num_rounds never reaches it.
  agents <- create_agents(2, .so_config(), direct_persona_descriptions = c("A.", "B."),
                          .runner = stub$runner)
  fg <- FocusGroup$new("library hours", "Test.", agents, "MOD",
                       create_conversation_flow("round_robin", agents, "MOD"),
                       question_script = list(list(phase = "engagement_question", text = "Q1?"),
                                              list(phase = "engagement_question", text = "Q2?"),
                                              list(phase = "closing", text = "Close.")))
  expect_warning(fg$run_simulation(num_rounds = 1,
                                   probes = fg_probe("From 1 to 7?", at = "before_closing")),
                 "before_closing")
  expect_length(fg$records$probes, 0L)

  expect_s3_class(fg_injection("Text.", at = "before_closing"), "fg_injection")
  expect_identical(format(fg_injection("Text.", at = "before_closing")$at), "before_closing")
  expect_error(fg_injection("Text.", at = "start"), "before_closing")
  expect_error(fg_probe("Q?", at = "after_closing"), "before_closing")
})

test_that("arms can branch at the point before the closing", {
  stub <- .so_stub()
  out <- run_focus_group_arms(
    "library hours", .so_config(),
    arms = list(note = fg_injection("A last note.", at = "before_closing",
                                    from = "Guest expert"),
                control = NULL),
    n_participants = 2, guide = list(Engagement = "Which hours matter?", Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    .runner = stub$runner)
  expect_identical(out$branch_point, "before_closing")
  prefix <- out$prefix$transcript
  expect_false("closing" %in% prefix$phase)
  for (arm in out$arms) {
    expect_identical(arm$transcript$phase[[nrow(arm$transcript)]], "closing")
  }
  expect_identical(out$arms$note$transcript$speaker_id[[nrow(prefix) + 1L]], "Guest expert")
})
