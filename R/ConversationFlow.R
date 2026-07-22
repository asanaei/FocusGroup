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
#' @field participant_ids A character vector of agent identifiers, excluding the moderator.
#' @field moderator_id Character. The ID of the moderator agent.
#' @field last_speaker_id The ID of the agent who last spoke. Can be `NULL`.
#' @field selection_metadata Details recorded for the latest selection.
#'   Desire-scoring failures record the condition and neutral fallback here.
#'
#' @section Methods for Subclassing:
#' \describe{
#'   \item{`initialize(agents, moderator_id)`}{Sets up the flow. Call `super$initialize(agents, moderator_id)`.}
#'   \item{`select_next_speaker(focus_group)`}{*Required*. Logic to choose the next `FGAgent` to speak.
#'     Should return the agent object or `NULL`.}
#'   \item{`update_state_post_selection(speaker_id, focus_group)`}{Optional. Updates internal state
#'     after a speaker has spoken. Base implementation updates `self$last_speaker_id`.}
#' }
#' @return An R6 generator meant for subclassing; instances are built by
#'   [create_conversation_flow()], which returns a `ConversationFlow`
#'   subclass object ready for [FocusGroup].
#' @examples
#' RandomFlow <- R6::R6Class("RandomFlow", inherit = ConversationFlow,
#'   public = list(
#'     select_next_speaker = function(focus_group) {
#'       ids <- self$participant_ids
#'       self$agents[[sample(ids, 1)]]
#'     }))
#' @export
ConversationFlow <- R6::R6Class("ConversationFlow",
  public = list(
    agents = NULL,
    participant_ids = NULL,
    moderator_id = NULL,
    last_speaker_id = NULL,
    selection_metadata = NULL,

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
      private$agent_ids <- names(agents)
      self$moderator_id <- moderator_id
      self$participant_ids <- setdiff(private$agent_ids, self$moderator_id)
      self$last_speaker_id <- NULL
      self$selection_metadata <- NULL
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
  ),
  private = list(
    agent_ids = NULL
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
#' @noRd
RoundRobinFlow <- R6::R6Class("RoundRobinFlow",
  inherit = ConversationFlow,
  public = list(
    #' @description Initialize RoundRobinFlow.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    initialize = function(agents, moderator_id) {
      super$initialize(agents, moderator_id)
      private$current_participant_index <- 0
    },

    #' @description Selects the next participant in round-robin order.
    #' @param focus_group The `FocusGroup` object.
    #' @return The selected `FGAgent` (a participant), or `NULL` if no participants.
    select_next_speaker = function(focus_group) {
      if (length(self$participant_ids) == 0) {
        # warning("RoundRobinFlow: No participants available to select.")
        return(NULL) # No participants to select
      }

      private$current_participant_index <-
        (private$current_participant_index %% length(self$participant_ids)) + 1
      next_speaker_id <- self$participant_ids[private$current_participant_index]

      return(self$agents[[next_speaker_id]])
    }
  ),
  private = list(
    current_participant_index = 0
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
#' @noRd
ProbabilisticFlow <- R6::R6Class("ProbabilisticFlow",
  inherit = ConversationFlow,
  public = list(
    #' @description Initialize ProbabilisticFlow.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    #' @param initial_propensities Named numeric vector. Optional. Base propensities for each participant.
    #'        If `NULL`, defaults to 1.0 for all participants (moderator excluded).
    #'        Names must match participant IDs.
    #' @param recovery_increment Numeric. Rate at which propensity recovers (0 to 1).
    initialize = function(agents, moderator_id, initial_propensities = NULL, recovery_increment = 0.1) {
      super$initialize(agents, moderator_id)
      private$recovery_increment <- recovery_increment
      ids <- self$participant_ids
      if (is.null(initial_propensities)) {
        private$base_propensities <- stats::setNames(rep(1.0, length(ids)), ids)
      } else {
        if (!is.numeric(initial_propensities) || is.null(names(initial_propensities)) ||
            !all(names(initial_propensities) %in% ids) ||
            !all(ids %in% names(initial_propensities))) {
          stop("ProbabilisticFlow: initial_propensities must be named numeric for all participant_ids.")
        }
        private$base_propensities <- initial_propensities[ids]
      }
      private$propensities <- private$base_propensities
    },

    #' @description Selects the next speaker based on current propensities.
    #' @param focus_group The `FocusGroup` object.
    #' @return The selected `FGAgent`, or `NULL` if no eligible speaker.
    select_next_speaker = function(focus_group) {
      eligible_ids <- self$participant_ids
      
      if(length(eligible_ids) == 0) return(NULL)

      current_eligible_propensities <- private$propensities[eligible_ids]
      current_eligible_propensities[current_eligible_propensities < 0] <- 0

      if (all(current_eligible_propensities == 0)) {
        if (length(eligible_ids) > 0) {
            normalized_probs <- rep(1/length(eligible_ids), length(eligible_ids))
        } else {
            return(NULL)
        }
      } else {
        normalized_probs <- current_eligible_propensities / sum(current_eligible_propensities)
      }
      
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

      if (speaker_id %in% names(private$propensities)) {
        private$propensities[[speaker_id]] <- 0
      }

      for (id in names(private$propensities)) {
        if (id != speaker_id) {
          private$propensities[[id]] <- min(
            private$base_propensities[[id]],
            private$propensities[[id]] + private$recovery_increment * private$base_propensities[[id]]
          )
        }
      }
    }
  ),
  private = list(
    propensities = NULL,
    base_propensities = NULL,
    recovery_increment = 0.1
  )
)

#' DesireBasedFlow Class
#'
#' @description
#' Selects the next participant speaker based on their LLM-rated "desire to talk".
#' The moderator's turns are primarily handled by the `FocusGroup`'s phase/script logic.
#' This flow is mainly for choosing which participant responds to the moderator.
#'
#' @field min_desire_threshold Numeric. Minimum desire score for a participant to be considered.
#' @noRd
DesireBasedFlow <- R6::R6Class("DesireBasedFlow",
  inherit = ConversationFlow,
  public = list(
    #' @description Initialize DesireBasedFlow.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    #' @param min_desire_threshold Numeric. Minimum desire score to be eligible.
    initialize = function(agents, moderator_id, min_desire_threshold = 3) {
      super$initialize(agents, moderator_id)
      private$last_desire_scores <- stats::setNames(
        rep(NA_real_, length(self$participant_ids)), self$participant_ids)
      private$min_desire_threshold <- min_desire_threshold
    },

    #' @description Selects the next participant based on desire to talk.
    #' @param focus_group The `FocusGroup` object, providing context like current question, history.
    #' @return The selected `FGAgent`. When no participant clears the desire
    #'   threshold the highest-scoring participant is chosen, so this returns
    #'   `NULL` only when there are no participants.
    select_next_speaker = function(focus_group) {
      if (length(self$participant_ids) == 0) return(NULL)

      self$selection_metadata <- NULL
      desire_scores <- stats::setNames(
        rep(NA_real_, length(self$participant_ids)), self$participant_ids)
      failures <- list()

      history_string <- make_prompt_history(
        focus_group$conversation_log,
        include_summary = focus_group$current_conversation_summary %||% NULL,
        max_tokens_history = 64000L
      )

      last_utterance_info <- if (length(focus_group$conversation_log) > 0) {
        focus_group$conversation_log[[length(focus_group$conversation_log)]]
      } else {
        list(speaker_id = "N/A", text = "N/A")
      }

      # Vectorized parallel desire scoring using LLMR broadcast when available
      eligible_ids <- self$participant_ids
      if (!is.null(self$last_speaker_id) && self$last_speaker_id %in% eligible_ids && length(eligible_ids) > 1) {
        eligible_ids <- setdiff(eligible_ids, self$last_speaker_id)
      }
      score_per_agent <- function(ids) {
        for (pid in ids) {
          agent <- self$agents[[pid]]
          sent_before <- agent$tokens_sent_agent
          rec_before <- agent$tokens_received_agent
          sc <- tryCatch(
            agent$get_need_to_talk(
              topic = focus_group$topic,
              conversation_history_string = history_string,
              # In roleflip mode use the placeholder-free desire instruction (so
              # the role-flipped desire path actually runs); in flat mode use the
              # legacy template (true single-message construction).
              desire_prompt_template = .fg_pick_template(focus_group$prompt_templates, "desire"),
              max_tokens_desire = focus_group$max_tokens_desire,
              current_moderator_question = focus_group$current_question_text %||% "N/A",
              last_speaker_id = last_utterance_info$speaker_id,
              last_utterance_text = last_utterance_info$text,
              conversation_log = focus_group$conversation_log
            ),
            error = function(e) {
              failures[[pid]] <<- conditionMessage(e)
              NA_real_
            }
          )
          sent_used <- .fg_tok0(agent$tokens_sent_agent - sent_before)
          rec_used <- .fg_tok0(agent$tokens_received_agent - rec_before)
          if ((sent_used + rec_used) > 0) {
            .fg_add_group_usage(focus_group, sent_used, rec_used)
          }
          desire_scores[pid] <<- as.numeric(sc)
        }
      }

      # Broadcast desire scoring within groups of agents that share a model
      # config. Broadcasting requires ONE config, so agents with different
      # providers/models/params must be grouped separately -- otherwise every
      # agent would be scored with the first agent's model. The common case
      # (all agents share a config) is a single group, i.e. one broadcast, as
      # before. The private scoring mode records which path ran.
      # Each pid gets a role-flipped message array (own turns -> assistant) so
      # the broadcast scores novelty from each agent's own perspective. Build the
      # transcript once and close over it. A legacy/custom desire template that
      # inlines the transcript keeps the flat single-user message.
      # Mode-aware template: roleflip -> placeholder-free desire instruction;
      # flat -> legacy template (true single-message construction). The grepl gate
      # then routes each correctly.
      desire_tpl <- .fg_pick_template(focus_group$prompt_templates, "desire")
      desire_legacy <- grepl("\\{\\{(conversation_history|persona_description)\\}\\}", desire_tpl)
      desire_transcript <- .fg_log_to_transcript(focus_group$conversation_log)
      build_desire_msg <- function(pid) {
        prompt_values <- list(
          persona_description = self$agents[[pid]]$persona_description,
          topic = focus_group$topic,
          conversation_history = history_string,
          current_moderator_question = focus_group$current_question_text %||% "N/A",
          last_speaker_id = last_utterance_info$speaker_id,
          last_utterance_text = last_utterance_info$text
        )
        instruction <- replace_placeholders(desire_tpl, prompt_values)
        if (desire_legacy) {
          return(list(list(role = "user", content = instruction)))
        }
        sys_text <- paste(c(self$agents[[pid]]$persona_description,
                            "You are rating your own desire to speak next."),
                          collapse = "\n")
        .fg_build_agent_messages(desire_transcript, pid, sys_text,
                                 instruction = instruction, self_state = NULL)
      }

      record_group_usage <- function(out, ids) {
        row_usage_found <- FALSE
        for (i in seq_along(ids)) {
          pid <- ids[[i]]
          row_obj <- if (is.data.frame(out)) out[i, , drop = FALSE] else
            lapply(out, function(x) if (length(x) >= i) x[[i]] else NULL)
          usage <- extract_token_counts(row_obj)
          if ((usage$sent + usage$rec) > 0) {
            row_usage_found <- TRUE
            self$agents[[pid]]$tokens_sent_agent <- self$agents[[pid]]$tokens_sent_agent + usage$sent
            self$agents[[pid]]$tokens_received_agent <- self$agents[[pid]]$tokens_received_agent + usage$rec
            .fg_add_group_usage(focus_group, usage$sent, usage$rec)
          }
        }
        if (!row_usage_found) {
          usage <- extract_token_counts(out)
          if ((usage$sent + usage$rec) > 0) {
            sent_each <- floor(usage$sent / length(ids))
            rec_each <- floor(usage$rec / length(ids))
            for (pid in ids) {
              self$agents[[pid]]$tokens_sent_agent <- self$agents[[pid]]$tokens_sent_agent + sent_each
              self$agents[[pid]]$tokens_received_agent <- self$agents[[pid]]$tokens_received_agent + rec_each
            }
            .fg_add_group_usage(focus_group, usage$sent, usage$rec)
          }
        }
      }

      # Broadcast one homogeneous group. Returns the ids it could NOT score (an
      # empty character vector means all scored). A reply that is empty or
      # length-truncated is reported as unscored rather than scored 0, so the
      # caller re-scores it on the per-agent path (which retries at a larger
      # budget). The desire budget follows the configured `max_tokens_desire`,
      # and no temperature is injected (some reasoning models need their native
      # value; the score is a single integer).
      broadcast_group <- function(ids) {
        cfg <- self$agents[[ids[1]]]$config
        cfg$model_params$max_tokens <- focus_group$max_tokens_desire
        cfg$model_params$temperature <- NULL
        out <- tryCatch(
          LLMR::call_llm_broadcast(config = cfg, messages = lapply(ids, build_desire_msg),
                                   tries = 5, wait_seconds = 2, backoff_factor = 3,
                                   progress = FALSE),
          error = function(...) NULL)
        if (is.null(out) || is.null(out$success) || is.null(out$response_text)) return(ids)
        unscored <- character(0)
        for (i in seq_along(ids)) {
          txt <- out$response_text[i]
          scored <- isTRUE(out$success[i]) && nzchar(trimws(txt %||% ""))
          if (scored) desire_scores[ids[[i]]] <<- parse_score_0_10(txt)
          else unscored <- c(unscored, ids[[i]])
        }
        record_group_usage(out, ids)
        unscored
      }

      if (length(eligible_ids) > 0) {
        used_broadcast <- FALSE
        has_runner <- vapply(eligible_ids, function(pid) {
          !is.null(self$agents[[pid]]$.runner)
        }, logical(1))
        if (!any(has_runner) && requireNamespace("LLMR", quietly = TRUE) &&
            length(eligible_ids) <= 8) {
          # Group by a content hash of each agent's config so heterogeneous
          # configs each broadcast under their own model.
          gkey <- vapply(eligible_ids, function(pid)
            tryCatch(LLMR::llm_hash(self$agents[[pid]]$config),
                     error = function(...) pid), character(1))
          groups <- split(eligible_ids, gkey)
          private$last_scoring_mode <- if (length(groups) == 1L)
            "broadcast_shared_config" else "broadcast_grouped_config"
          # A group whose broadcast fails must not leave its agents at the
          # default score of 0; collect those ids and score them per-agent
          # below. Only when EVERY group fails do we drop to the full per-agent
          # path (and relabel the mode accordingly).
          failed_ids <- character(0)
          for (g in groups) failed_ids <- c(failed_ids, broadcast_group(g))
          used_broadcast <- length(failed_ids) < length(eligible_ids)
          if (used_broadcast && length(failed_ids)) score_per_agent(failed_ids)
        }

        # Fallback only when broadcast did not run at all, or every group failed.
        if (!used_broadcast) {
          private$last_scoring_mode <- "per_agent"
          score_per_agent(eligible_ids)
        }
      }

      private$last_desire_scores <- desire_scores

      if (length(failures)) {
        reasons <- unique(unlist(failures, use.names = FALSE))
        self$selection_metadata <- list(
          desire_scoring_failure = list(
            reason = paste(reasons, collapse = "; "),
            fallback = "uniform_random",
            candidates = unname(eligible_ids)
          )
        )
        if (!private$failure_message_emitted) {
          message(
            "Desire scoring failed; selecting uniformly among eligible participants."
          )
          private$failure_message_emitted <- TRUE
        }
        return(self$agents[[sample(eligible_ids, 1L)]])
      }

      scored <- desire_scores[eligible_ids]
      eligible_participants <- scored[
        !is.na(scored) & scored >= private$min_desire_threshold
      ]

      # If no one meets the threshold but there's ongoing conversation, lower the bar
      if (length(eligible_participants) == 0 && length(focus_group$conversation_log) > 2) {
        # Allow lower threshold for ongoing conversations to maintain flow
        eligible_participants <- scored[
          !is.na(scored) & scored >= (private$min_desire_threshold - 1)
        ]
      }

      if (length(eligible_participants) == 0) {
        # If still no one eligible, pick the highest scored participant even if 0 to keep conversation moving
        if (length(scored) > 0) {
          max_score <- max(scored, na.rm = TRUE)
          candidates <- names(scored[scored == max_score])
          next_speaker_id <- if (length(candidates) == 1) candidates[1] else sample(candidates, 1)
          return(self$agents[[next_speaker_id]])
        }
        return(NULL)
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
    }
  ),

  private = list(
    last_desire_scores = NULL,
    last_scoring_mode = NULL,
    min_desire_threshold = 3,
    failure_message_emitted = FALSE
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
#' # mod_id <- "MOD"
#' # all_my_agents <- list(P1 = agent1, P2 = agent2, MOD = mod)
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
