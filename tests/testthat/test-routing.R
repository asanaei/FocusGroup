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
