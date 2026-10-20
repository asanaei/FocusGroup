# Who gets the floor: the previous-speaker exclusion lasts only until the
# moderator speaks, and a moderator line that names one participant gives
# that participant the next turn. Offline, with scripted runners.

.rt_config <- function() LLMR::llm_config("openai", "example-model")

.rt_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

# Desire scores by persona: "Persona one." scores `top`, everyone else 2.
.rt_runner <- function(top = "9", moderator = NULL) {
  env <- new.env()
  env$n <- 0L
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    messages <- experiments$messages[[1]]
    prompt <- .rt_prompt(messages)
    experiments$response_text <- if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      if (grepl("Persona one", messages[[1]]$content, fixed = TRUE)) top else "2"
    } else if (grepl("Summary:$", prompt)) {
      "A short summary."
    } else if (!is.null(moderator) && grepl("MODERATOR", prompt, fixed = TRUE)) {
      moderator
    } else {
      sprintf("Reply %03d: evening hours matter to working families.", env$n)
    }
    experiments
  }
  env
}

.rt_group <- function(runner, script, flow = "desire_based", responses = 1L, ...) {
  agents <- create_agents(3, config = .rt_config(),
                          direct_persona_descriptions = c("Persona one.", "Persona two.",
                                                          "Persona three."),
                          .runner = runner)
  FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow(flow, agents, "MOD"),
    question_script = script, max_participant_responses = responses, ...)
}

test_that("the participant who closed one question may open the next", {
  stub <- .rt_runner()
  fg <- .rt_group(stub$runner, list(
    list(phase = "engagement_question", text = "Which hours matter?"),
    list(phase = "engagement_question", text = "Who is left out?"),
    list(phase = "exploration_question", text = "What would you change?")),
    call_on_silent = "none")
  fg$run_simulation()
  routing <- fg$records$routing
  # P1 has the highest desire score throughout and answers every question:
  # the moderator's line between questions ends the exclusion.
  expect_identical(vapply(routing, `[[`, character(1), "selected_id"),
                   c("P1", "P1", "P1"))
  expect_true(all(lengths(lapply(routing, `[[`, "excluded")) == 0L))
  expect_true(all(vapply(routing, function(r) length(r$scores) == 3L, logical(1))))

  # Within a question the exclusion still bars back-to-back turns.
  stub <- .rt_runner()
  fg <- .rt_group(stub$runner, list(
    list(phase = "engagement_question", text = "Which hours matter?")),
    responses = 2L, call_on_silent = "none")
  fg$run_simulation()
  second <- fg$records$routing[[2]]
  expect_identical(second$excluded, c(P1 = "previous_speaker"))
  expect_false(identical(second$selected_id, "P1"))
})

test_that("a flow without note_moderator_turn() still runs", {
  OldFlow <- R6::R6Class("OldFlow", public = list(
    agents = NULL, participant_ids = NULL, last_speaker_id = NULL,
    initialize = function(agents) {
      self$agents <- agents
      self$participant_ids <- setdiff(names(agents), "MOD")
    },
    select_next_speaker = function(focus_group) self$agents[[self$participant_ids[[1]]]],
    update_state_post_selection = function(speaker_id, focus_group) {
      self$last_speaker_id <- speaker_id
    }))
  stub <- .rt_runner()
  agents <- create_agents(2, config = .rt_config(),
                          direct_persona_descriptions = c("A.", "B."),
                          .runner = stub$runner)
  flow <- OldFlow$new(agents)
  class(flow) <- c("OldFlow", "ConversationFlow", class(flow))
  fg <- FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
    turn_taking_flow = flow,
    question_script = list(list(phase = "engagement_question", text = "Q?")),
    max_participant_responses = 1, call_on_silent = "none")
  fg$run_simulation()
  expect_identical(flow$last_speaker_id, "P1")
})

test_that("the addressee is the one participant the last question opens or closes with", {
  handles <- stats::setNames(paste0("P", 1:10), paste0("id", 1:10))
  addressee <- function(text) FocusGroup:::.fg_addressee(text, handles)
  positives <- c(
    "Thanks, P2. P3, do you agree?" = "id3",
    "Could you say more, P4?" = "id4",
    "P10, what would you change first?" = "id10",
    "Thanks, P10. What do you think, P1?" = "id1",
    "**P5**, could you give an example?" = "id5",
    "That is a strong point.\n\nP6, how does it look from where you sit?" = "id6")
  for (text in names(positives)) {
    expect_identical(addressee(text), positives[[text]], info = text)
  }
  negatives <- c(
    "As P2 said, what do others think?",
    "P2, P3, and P5 all mentioned cost. What does everyone think?",
    "Does anyone agree with P2?",
    "What do you think, P2 and P3?",
    "P3, do you and the rest of the group agree?",
    "P3, could you say more? What led you there?",
    "What did P3 mean by that?",
    "P3 made a point about cost. What do you think?",
    "P3, thank you for that.",
    "Thank you, P3.",
    "What do you think, P30?",
    "P1, P2, what do you both think?",
    "Welcome, everyone. P4, what do all of you think?",
    "")
  for (text in negatives) {
    expect_identical(addressee(text), NA_character_, info = text)
  }
  expect_identical(FocusGroup:::.fg_addressee("P3, do you agree?", character()), NA_character_)
  expect_identical(FocusGroup:::.fg_addressee(NA_character_, handles), NA_character_)
  # Handles of any form match only themselves.
  named <- c(a = "Ana", b = "Anabel", c = "J.R.")
  expect_identical(FocusGroup:::.fg_addressee("Anabel, do you agree?", named), "b")
  expect_identical(FocusGroup:::.fg_addressee("Could you say more, J.R.?", named), "c")
  expect_identical(FocusGroup:::.fg_addressee("Could you say more, JxR.?", named), NA_character_)
})

test_that("a moderator line that addresses a participant gives that participant the next turn", {
  addressed_line <- "Thank you all for joining. P3, what do you think about evening hours?"
  stub <- .rt_runner(top = "9", moderator = addressed_line)
  fg <- .rt_group(stub$runner, list(
    list(phase = "engagement_question", text = "Which hours matter?")),
    responses = 2L, call_on_silent = "none")
  fg$run_simulation()
  log <- fg$conversation_log
  routing <- fg$records$routing
  question <- log[[2]]
  expect_identical(question$text, addressed_line)
  # P3 answers although P1 holds the highest desire score; then the flow
  # chooses again.
  expect_identical(vapply(routing, `[[`, character(1), "rule"),
                   c("addressed", "highest_score"))
  expect_identical(vapply(routing, `[[`, character(1), "selected_id"), c("P3", "P1"))
  expect_identical(routing[[1]]$trigger_message_id, question$message_id)
  expect_null(routing[[1]]$scores)
  expect_true(is.na(routing[[2]]$trigger_message_id))
  expect_true(log[[3]]$metadata$addressed)
  expect_identical(log[[3]]$speaker_id, "P3")
  # No desire score was asked for the addressed turn.
  calls <- fg$records$calls
  types <- vapply(calls, `[[`, character(1), "call_type")
  expect_identical(sum(types == "desire"), 2L)

  # A line that also asks the group leaves the choice to the flow.
  stub <- .rt_runner(top = "9", moderator = "P3, what does everyone think?")
  fg <- .rt_group(stub$runner, list(
    list(phase = "engagement_question", text = "Which hours matter?")),
    call_on_silent = "none")
  fg$run_simulation()
  expect_identical(fg$records$routing[[1]]$rule, "highest_score")
  expect_identical(fg$records$routing[[1]]$selected_id, "P1")
})

test_that("an injected moderator line can address a participant", {
  stub <- .rt_runner(top = "9")
  fg <- .rt_group(stub$runner, list(
    list(phase = "engagement_question", text = "Which hours matter?")),
    responses = 2L, call_on_silent = "none")
  fg$run_simulation(injections = fg_injection("Could you take this one, P2?",
                                              at = fg_before_turn(2)))
  routing <- fg$records$routing
  expect_identical(vapply(routing, `[[`, character(1), "selected_id"), c("P1", "P2"))
  expect_identical(routing[[2]]$rule, "addressed")
  injected <- Filter(function(m) isTRUE(m$metadata$injected), fg$conversation_log)[[1]]
  expect_identical(routing[[2]]$trigger_message_id, injected$message_id)
})

test_that("the moderator prompts no longer name a participant by roster order", {
  templates <- get_default_prompt_templates()
  expect_false(grepl("other_participant_id_placeholder", templates$moderator_probing_focused,
                     fixed = TRUE))
  expect_match(templates$moderator_probing_focused, "another participant", fixed = TRUE)
  expect_false(grepl("participant ID", templates$moderator_opening, fixed = TRUE))
})
