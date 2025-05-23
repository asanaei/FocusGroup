# File: R/ConversationFlow.R
# Purpose: Defines base class and implementations for turn-taking mechanisms.

#' @import R6
#' @importFrom stats runif setNames
NULL

#' ConversationFlow Base Class
#'
#' @description
#' `ConversationFlow` is an R6 base class defining the interface for turn-taking
#' mechanisms in a focus group simulation. Subclasses implement specific strategies
#' for selecting the next speaker.
#'
#' @field agents A named list of `FGAgent` objects participating in the conversation.
#' @field agent_ids A character vector of agent identifiers (names of the `agents` list).
#' @field participant_ids A character vector of agent identifiers, excluding the moderator.
#' @field moderator_id Character. The ID of the moderator agent.
#' @field last_speaker_id The ID of the agent who last spoke. Can be `NULL`.
#'
#' @section Methods for Subclassing:
#' \describe{
#'   \item{`initialize(agents, moderator_id)`}{Sets up the flow. Call `super$initialize(agents, moderator_id)`.}
#'   \item{`select_next_speaker(focus_group)`}{*Required*. Logic to choose the next `FGAgent` to speak.
#'     Should return the agent object or `NULL`.}
#'   \item{`update_state_post_selection(speaker_id, focus_group)`}{Optional. Updates internal state
#'     after a speaker has spoken. Base implementation updates `self$last_speaker_id`.}
#' }
#' @export
ConversationFlow <- R6::R6Class("ConversationFlow",
  public = list(
    agents = NULL,
    agent_ids = NULL,
    participant_ids = NULL,
    moderator_id = NULL,
    last_speaker_id = NULL,

    #' @description Initializes the ConversationFlow object.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    initialize = function(agents, moderator_id) {
      if (!is.list(agents) || length(agents) == 0 || !all(sapply(agents, inherits, "FGAgent"))) {
        stop("Flow 'agents' must be a non-empty list of FGAgent objects.")
      }
      if (is.null(names(agents)) || any(nchar(names(agents)) == 0)) {
        stop("Flow 'agents' list must be named with agent IDs.")
      }
      if (!is.character(moderator_id) || length(moderator_id) != 1 || !(moderator_id %in% names(agents))) {
        stop("Flow 'moderator_id' must be a valid ID present in the agents list.")
      }
      self$agents <- agents
      self$agent_ids <- names(agents)
      self$moderator_id <- moderator_id
      self$participant_ids <- setdiff(self$agent_ids, self$moderator_id)
      self$last_speaker_id <- NULL
    },

    #' @description Selects the next speaker. Must be implemented by subclasses.
    #' @param focus_group The `FocusGroup` object managing the simulation, providing context.
    #' @return The `FGAgent` object of the selected speaker, or `NULL`.
    select_next_speaker = function(focus_group) {
      stop("select_next_speaker() must be implemented by a subclass of ConversationFlow.")
    },

    #' @description Updates internal state after a speaker is selected.
    #' @param speaker_id The ID of the agent who was selected and just spoke.
    #' @param focus_group The `FocusGroup` object.
    update_state_post_selection = function(speaker_id, focus_group) {
      self$last_speaker_id <- speaker_id
    }
  )
)

#' RoundRobinFlow Class
#'
#' @description
#' Implements a round-robin turn-taking mechanism among participants.
#' The moderator is typically handled by the `FocusGroup`'s phase logic,
#' so this flow focuses on cycling through non-moderator agents.
#'
#' @field current_participant_index Integer. Current position in the rotation of participants.
#' @export
RoundRobinFlow <- R6::R6Class("RoundRobinFlow",
  inherit = ConversationFlow,
  public = list(
    current_participant_index = 0,

    #' @description Initialize RoundRobinFlow.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    initialize = function(agents, moderator_id) {
      super$initialize(agents, moderator_id)
      self$current_participant_index <- 0 # Start before the first participant
    },

    #' @description Selects the next participant in round-robin order.
    #' @param focus_group The `FocusGroup` object.
    #' @return The selected `FGAgent` (a participant), or `NULL` if no participants.
    select_next_speaker = function(focus_group) {
      if (length(self$participant_ids) == 0) {
        # warning("RoundRobinFlow: No participants available to select.")
        return(NULL) # No participants to select
      }

      self$current_participant_index <- (self$current_participant_index %% length(self$participant_ids)) + 1
      next_speaker_id <- self$participant_ids[self$current_participant_index]

      return(self$agents[[next_speaker_id]])
    }
  )
)

#' ProbabilisticFlow Class
#'
#' @description
#' Selects the next speaker probabilistically based on propensities.
#' Propensities decrease after speaking and recover over time.
#'
#' @field propensities Named numeric vector. Current speaking propensities for each agent.
#' @field base_propensities Named numeric vector. Base propensities for each agent.
#' @field recovery_increment Numeric. Factor by which propensities recover towards base.
#' @export
ProbabilisticFlow <- R6::R6Class("ProbabilisticFlow",
  inherit = ConversationFlow,
  public = list(
    propensities = NULL,
    base_propensities = NULL,
    recovery_increment = 0.1, # Default recovery rate

    #' @description Initialize ProbabilisticFlow.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    #' @param initial_propensities Named numeric vector. Optional. Base propensities for each agent.
    #'        If `NULL`, defaults to 1.0 for all participants and 1.5 for the moderator.
    #'        Names must match agent IDs.
    #' @param recovery_increment Numeric. Rate at which propensity recovers (0 to 1).
    initialize = function(agents, moderator_id, initial_propensities = NULL, recovery_increment = 0.1) {
      super$initialize(agents, moderator_id)
      self$recovery_increment <- recovery_increment

      if (is.null(initial_propensities)) {
        self$base_propensities <- stats::setNames(rep(1.0, length(self$agent_ids)), self$agent_ids)
        if (self$moderator_id %in% self$agent_ids) {
          self$base_propensities[self$moderator_id] <- 1.5 # Moderator slightly more likely by default
        }
      } else {
        if (!is.numeric(initial_propensities) || is.null(names(initial_propensities)) ||
            !all(names(initial_propensities) %in% self$agent_ids) ||
            !all(self$agent_ids %in% names(initial_propensities))) {
          stop("ProbabilisticFlow: initial_propensities must be a named numeric vector with names matching all agent_ids.")
        }
        self$base_propensities <- initial_propensities[self$agent_ids] # Ensure correct order
      }
      self$propensities <- self$base_propensities
    },

    #' @description Selects the next speaker based on current propensities.
    #' @param focus_group The `FocusGroup` object.
    #' @return The selected `FGAgent`, or `NULL` if no eligible speaker.
    select_next_speaker = function(focus_group) {
      eligible_ids <- self$agent_ids
      # Prevent immediate self-succession if multiple agents are available and it's not the moderator
      if (!is.null(self$last_speaker_id) && length(self$agent_ids) > 1 && self$last_speaker_id != self$moderator_id) {
         # Allow moderator to speak again if needed by script, but participants shouldn't self-succeed easily
        if (self$last_speaker_id %in% self$participant_ids) {
            eligible_ids <- setdiff(self$agent_ids, self$last_speaker_id)
        }
      }
      
      if (length(eligible_ids) == 0) { # Fallback if last_speaker_id was the only one eligible
          eligible_ids <- self$agent_ids
      }

      current_eligible_propensities <- self$propensities[eligible_ids]
      current_eligible_propensities[current_eligible_propensities < 0] <- 0 # Floor at 0

      if (all(current_eligible_propensities == 0)) {
        # warning("ProbabilisticFlow: All eligible agents have zero propensity. Assigning equal probability.")
        if (length(eligible_ids) > 0) {
            normalized_probs <- rep(1/length(eligible_ids), length(eligible_ids))
        } else {
            return(NULL) # No one to select
        }
      } else {
        normalized_probs <- current_eligible_propensities / sum(current_eligible_propensities)
      }
      
      if(length(eligible_ids) == 0) return(NULL)
      if(length(eligible_ids) == 1) {
        next_speaker_id <- eligible_ids[1]
      } else {
        next_speaker_id <- sample(eligible_ids, 1, prob = normalized_probs)
      }
      
      return(self$agents[[next_speaker_id]])
    },

    #' @description Updates propensities after a speaker is selected.
    #' @param speaker_id The ID of the agent who spoke.
    #' @param focus_group The `FocusGroup` object.
    update_state_post_selection = function(speaker_id, focus_group) {
      super$update_state_post_selection(speaker_id, focus_group)

      # Agent who just spoke has their propensity reset (or significantly reduced)
      # Moderator's propensity might be managed differently or reset less drastically
      if (speaker_id == self$moderator_id) {
          self$propensities[[speaker_id]] <- self$base_propensities[[speaker_id]] * 0.5 # Moderator recovers faster or resets higher
      } else {
          self$propensities[[speaker_id]] <- 0 # Participants reset to 0
      }

      # Others recover
      for (id in self$agent_ids) {
        if (id != speaker_id) {
          self$propensities[[id]] <- min(
            self$base_propensities[[id]],
            self$propensities[[id]] + self$recovery_increment * self$base_propensities[[id]]
          )
        }
      }
    }
  )
)

#' DesireBasedFlow Class
#'
#' @description
#' Selects the next participant speaker based on their LLM-rated "desire to talk".
#' The moderator's turns are primarily handled by the `FocusGroup`'s phase/script logic.
#' This flow is mainly for choosing which participant responds to the moderator.
#'
#' @field last_desire_scores Named numeric vector. Stores the most recent desire scores.
#' @field min_desire_threshold Numeric. Minimum desire score for a participant to be considered.
#' @export
DesireBasedFlow <- R6::R6Class("DesireBasedFlow",
  inherit = ConversationFlow,
  public = list(
    last_desire_scores = NULL,
    min_desire_threshold = 3, # Default minimum desire to speak

    #' @description Initialize DesireBasedFlow.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    #' @param min_desire_threshold Numeric. Minimum desire score to be eligible.
    initialize = function(agents, moderator_id, min_desire_threshold = 3) {
      super$initialize(agents, moderator_id)
      self$last_desire_scores <- stats::setNames(numeric(length(self$participant_ids)), self$participant_ids)
      self$min_desire_threshold <- min_desire_threshold
    },

    #' @description Selects the next participant based on desire to talk.
    #' @param focus_group The `FocusGroup` object, providing context like current question, history.
    #' @return The selected `FGAgent` (a participant), or `NULL` if no participant meets threshold.
    select_next_speaker = function(focus_group) {
      if (length(self$participant_ids) == 0) return(NULL)

      desire_scores <- stats::setNames(numeric(length(self$participant_ids)), self$participant_ids)

      history_string <- format_conversation_history(
        focus_group$conversation_log,
        n_recent = 7, # Configurable
        include_summary = focus_group$current_conversation_summary %||% NULL
      )
      
      last_utterance_info <- if (length(focus_group$conversation_log) > 0) {
          focus_group$conversation_log[[length(focus_group$conversation_log)]]
      } else {
          list(speaker_id = "N/A", text = "N/A")
      }

      for (agent_id in self$participant_ids) {
        # Prevent immediate self-succession among participants
        if (!is.null(self$last_speaker_id) && agent_id == self$last_speaker_id && length(self$participant_ids) > 1) {
          desire_scores[agent_id] <- -1 # Effectively makes them ineligible if others want to speak
          next
        }
        agent <- self$agents[[agent_id]]
        desire_scores[agent_id] <- agent$get_need_to_talk(
          topic = focus_group$topic,
          conversation_history_string = history_string,
          desire_prompt_template = focus_group$prompt_templates$participant_desire_to_talk_nuanced,
          max_tokens_desire = focus_group$max_tokens_desire,
          current_moderator_question = focus_group$current_question_text %||% "N/A",
          last_speaker_id = last_utterance_info$speaker_id,
          last_utterance_text = last_utterance_info$text
        )
      }
      self$last_desire_scores <- desire_scores

      eligible_participants <- desire_scores[desire_scores >= self$min_desire_threshold & desire_scores > -1]

      if (length(eligible_participants) == 0) {
        # warning("DesireBasedFlow: No participant meets the desire threshold. Moderator might need to re-engage.")
        return(NULL) # No one wants to speak enough
      }
      
      max_score <- max(eligible_participants, na.rm = TRUE)
      candidates <- names(eligible_participants[eligible_participants == max_score])
      
      if (length(candidates) == 0) { # Should not happen if eligible_participants is not empty
        return(NULL)
      } else if (length(candidates) == 1) {
        next_speaker_id <- candidates[1]
      } else {
        # Tie-breaking: random among highest scorers
        next_speaker_id <- sample(candidates, 1)
      }
      return(self$agents[[next_speaker_id]])
    },

    #' @description Get the last calculated desire scores for participants.
    #' @return A named numeric vector of desire scores.
    get_last_desire_scores = function() {
      return(self$last_desire_scores)
    }
  )
)

#' Create a Conversation Flow Object
#'
#' Factory function to create an instance of a `ConversationFlow` subclass.
#'
#' @param mode Character. The type of turn-taking flow to create.
#'        Supported: "round_robin", "probabilistic", "desire_based".
#' @param agents A named list of `FGAgent` objects.
#' @param moderator_id Character. The ID of the moderator agent.
#' @param flow_params List. Additional parameters specific to the chosen flow type.
#'   For "probabilistic": `initial_propensities` (named numeric vector), `recovery_increment` (numeric).
#'   For "desire_based": `min_desire_threshold` (numeric).
#' @return An initialized `ConversationFlow` object (e.g., `RoundRobinFlow`, `ProbabilisticFlow`, `DesireBasedFlow`).
#' @export
#' @examples
#' # Assuming FGAgent class and agents list (agent1, agent2, mod) are defined
#' # mod_id <- "Moderator"
#' # all_my_agents <- list(Agent1 = agent1, Agent2 = agent2, Moderator = mod)
#' # flow <- create_conversation_flow("desire_based", all_my_agents, mod_id)
create_conversation_flow <- function(mode, agents, moderator_id, flow_params = list()) {
  if (!is.character(mode) || length(mode) != 1) {
    stop("'mode' must be a single character string.")
  }
  # agents and moderator_id validated by ConversationFlow$initialize

  switch(mode,
    "round_robin" = RoundRobinFlow$new(agents, moderator_id),
    "probabilistic" = ProbabilisticFlow$new(
      agents,
      moderator_id,
      initial_propensities = flow_params$initial_propensities, # Must be named list matching agent_ids
      recovery_increment = flow_params$recovery_increment %||% 0.1
    ),
    "desire_based" = DesireBasedFlow$new(
      agents,
      moderator_id,
      min_desire_threshold = flow_params$min_desire_threshold %||% 3
      ),
    stop(paste0("Unknown conversation flow mode: '", mode,
                "'. Supported modes are 'round_robin', 'probabilistic', 'desire_based'."))
  )
} 