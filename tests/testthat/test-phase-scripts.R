test_that("numeric phase counts are honored for every phase", {
  counts <- c(Opening = 2, Icebreaker = 3, Engagement = 2,
              Exploration = 4, Closing = 2)
  script <- FocusGroup:::.fg_build_question_script(counts, "public transit")
  phases <- vapply(script, `[[`, character(1), "phase")

  expect_identical(
    phases,
    rep(c("opening", "icebreaker_question", "engagement_question",
          "exploration_question", "closing"), unname(counts))
  )
  expect_length(script, sum(counts))
})

test_that("numeric phase banks vary questions within each phase", {
  script <- FocusGroup:::.fg_build_question_script(
    c(Opening = 2, Icebreaker = 3, Engagement = 3,
      Exploration = 3, Closing = 2),
    "public transit"
  )
  phases <- vapply(script, `[[`, character(1), "phase")
  text <- vapply(script, `[[`, character(1), "text")

  for (phase in unique(phases)) {
    expect_true(length(unique(text[phases == phase])) > 1L)
  }
})

test_that("named character lists preserve custom phase scripts", {
  custom <- list(
    Opening = c("Welcome the group.", "State the ground rules."),
    Icebreaker = c("What first comes to mind?", "What shaped that view?"),
    Engagement = "Where does this enter daily life?",
    Exploration = c("What is the central trade-off?", "What evidence is missing?"),
    Closing = c("Summarize the disagreement.", "Thank the participants.")
  )
  script <- FocusGroup:::.fg_build_question_script(custom, "public transit")

  expect_identical(vapply(script, `[[`, character(1), "text"), unname(unlist(custom)))
  expect_identical(
    vapply(script, `[[`, character(1), "phase"),
    rep(c("opening", "icebreaker_question", "engagement_question",
          "exploration_question", "closing"), lengths(custom))
  )
})

test_that("a session executes every requested opening and closing turn", {
  captured_messages <- list()
  runner <- function(config, messages) {
    captured_messages[[length(captured_messages) + 1L]] <<- messages
    paste(
      "This scripted moderator or participant turn states one concrete point",
      "about the topic and supplies a reason for the group to consider."
    )
  }
  result <- run_focus_group(
    topic = "public transit",
    participants = 1,
    turns_per_phase = c(Opening = 2, Icebreaker = 2, Engagement = 1,
                        Exploration = 1, Closing = 2),
    conversation_flow = "round_robin",
    runner = runner,
    max_participant_responses = 1,
    verbose = FALSE
  )

  moderator_log <- Filter(function(message) isTRUE(message$is_moderator),
                          result$focus_group$conversation_log)
  phase_counts <- table(vapply(moderator_log, `[[`, character(1), "phase"))
  expect_identical(as.integer(phase_counts[c("opening", "icebreaker_question",
                                             "engagement_question", "exploration_question",
                                             "closing")]),
                   c(2L, 2L, 1L, 1L, 2L))

  expected_script <- FocusGroup:::.fg_build_question_script(
    c(Opening = 2, Icebreaker = 2, Engagement = 1,
      Exploration = 1, Closing = 2),
    "public transit"
  )
  expected_text <- vapply(expected_script, `[[`, character(1), "text")
  prompt_text <- unlist(lapply(captured_messages, function(messages) {
    vapply(messages, function(message) as.character(message$content), character(1))
  }), use.names = FALSE)
  expect_true(all(vapply(expected_text, function(script) {
    any(grepl(script, prompt_text, fixed = TRUE))
  }, logical(1))))
  expect_false(any(grepl("Your task is to deliver the opening remarks", prompt_text,
                         fixed = TRUE)))
  expect_false(any(grepl("Your task is to deliver the closing remarks", prompt_text,
                         fixed = TRUE)))
})

test_that("a single numeric closing turn closes the session", {
  script <- FocusGroup:::.fg_build_question_script(c(Closing = 1), "public transit")
  expect_identical(script[[1]]$text,
                   "Thank the participants, state that the discussion is complete, and close the session.")
})

test_that("invalid phase specifications are rejected", {
  expect_identical(FocusGroup:::.fg_build_question_script(NULL, "topic"), list())
  expect_error(FocusGroup:::.fg_build_question_script(c(1, 2), "topic"),
               "phase names")
  expect_error(FocusGroup:::.fg_build_question_script(c(Unknown = 1), "topic"),
               "Unknown phase")
  expect_error(FocusGroup:::.fg_build_question_script(c(Opening = 1.5), "topic"),
               "whole number")
  expect_error(FocusGroup:::.fg_build_question_script(
    list(Closing = "Close the group.", Engagement = "Ask a question."), "topic"),
    "must follow the order")
  expect_error(FocusGroup:::.fg_build_question_script(list(Opening = " "), "topic"),
               "non-missing and non-empty")
  expect_error(FocusGroup:::.fg_build_question_script(c(Opening = 0), "topic"),
               "at least one moderator turn")
})
