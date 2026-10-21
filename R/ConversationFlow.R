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
#' @field selection_metadata Details recorded for the latest selection when
#'   it departs from the flow's usual rule, such as a lowered desire threshold
#'   or, under `on_desire_failure = "random"`, a desire-scoring failure.
#' @field routing_record The record of the latest selection, whatever rule
#'   made it: a list with `flow`, `rule`, `candidates`, `excluded` (a named
#'   character vector giving the reason each excluded participant was passed
#'   over), `scores`, `threshold`, `tied`, `tie_break`, and `selected_id`. A
#'   running session copies it into its routing record after every pick. A
#'   subclass that does not set it is recorded with rule `"custom"`.
#'
#' @section Methods for Subclassing:
#' \describe{
#'   \item{`initialize(agents, moderator_id)`}{Sets up the flow. Call `super$initialize(agents, moderator_id)`.}
#'   \item{`select_next_speaker(focus_group)`}{*Required*. Logic to choose the next `FGAgent` to speak.
#'     Should return the agent object or `NULL`.}
#'   \item{`update_state_post_selection(speaker_id, focus_group)`}{Optional. Updates internal state
#'     after a speaker has spoken. Base implementation updates `self$last_speaker_id`.}
#'   \item{`note_moderator_turn()`}{Optional. Called after every moderator
#'     line: a guide item, a call-on line, or an injected moderator message.
#'     Base implementation clears `self$last_speaker_id`, so that a flow that
#'     passes over the previous speaker bars only back-to-back participant
#'     turns.}
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
    routing_record = NULL,

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
      self$routing_record <- NULL
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
    },

    #' @description Notes that the moderator has just spoken. The participant
    #'   who spoke before the moderator's line is then no longer the previous
    #'   speaker: whoever closed one question may open the next.
    #' @return The flow, invisibly.
    note_moderator_turn = function() {
      self$last_speaker_id <- NULL
      invisible(self)
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

    #' @description Selects the next participant in round-robin order. A
    #'   turn the moderator gave outside the rotation (an addressed or
    #'   called-on turn) leaves the rotation where it was; when the next in
    #'   the rotation is the participant who has just spoken, the rotation
    #'   passes on to the one after, so no one speaks twice in a row.
    #' @param focus_group The `FocusGroup` object.
    #' @return The selected `FGAgent` (a participant), or `NULL` if no participants.
    select_next_speaker = function(focus_group) {
      if (length(self$participant_ids) == 0) {
        # warning("RoundRobinFlow: No participants available to select.")
        return(NULL) # No participants to select
      }

      n <- length(self$participant_ids)
      private$current_participant_index <- (private$current_participant_index %% n) + 1
      excluded <- character()
      just_spoke <- self$last_speaker_id
      if (n > 1L && !is.null(just_spoke) &&
          identical(self$participant_ids[private$current_participant_index], just_spoke)) {
        excluded[[just_spoke]] <- "previous_speaker"
        private$current_participant_index <- (private$current_participant_index %% n) + 1
      }
      next_speaker_id <- self$participant_ids[private$current_participant_index]
      self$routing_record <- .fg_routing_record(
        "round_robin", "rotation", self$participant_ids, next_speaker_id,
        excluded = excluded,
        position = as.integer(private$current_participant_index))

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
      self$routing_record <- .fg_routing_record(
        "probabilistic", "propensity_draw", eligible_ids, next_speaker_id,
        scores = stats::setNames(as.numeric(normalized_probs), eligible_ids),
        tie_break = if (length(eligible_ids) > 1L) "draw" else "none")

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
#' @field on_desire_failure Character. `"error"` stops the session when a
#'   participant's desire score cannot be obtained; `"random"` continues with
#'   the valid scores.
#' @noRd
DesireBasedFlow <- R6::R6Class("DesireBasedFlow",
  inherit = ConversationFlow,
  public = list(
    #' @description Initialize DesireBasedFlow.
    #' @param agents A named list of `FGAgent` objects.
    #' @param moderator_id Character. The ID of the moderator agent.
    #' @param min_desire_threshold Numeric. Minimum desire score to be eligible.
    #' @param on_desire_failure Character. `"error"` or `"random"`.
    initialize = function(agents, moderator_id, min_desire_threshold = 3,
                          on_desire_failure = "error") {
      super$initialize(agents, moderator_id)
      private$last_desire_scores <- stats::setNames(
        rep(NA_real_, length(self$participant_ids)), self$participant_ids)
      private$min_desire_threshold <- min_desire_threshold
      private$on_desire_failure <- match.arg(on_desire_failure, c("error", "random"))
    },

    #' @description Selects the next participant based on desire to talk.
    #' @param focus_group The `FocusGroup` object, providing context like current question, history.
    #' @return The selected `FGAgent`. When no participant clears the desire
    #'   threshold the highest-scoring participant is chosen, so this returns
    #'   `NULL` only when there are no participants. A score that cannot be
    #'   obtained stops the session with an error of class
    #'   `focusgroup_desire_failure` under `on_desire_failure = "error"`; under
    #'   `"random"` the speaker is chosen by the same rule among the valid
    #'   scores, or uniformly among the eligible participants when none is
    #'   valid, and the failure is recorded.
    select_next_speaker = function(focus_group) {
      if (length(self$participant_ids) == 0) return(NULL)

      self$selection_metadata <- NULL
      self$routing_record <- NULL
      desire_scores <- stats::setNames(
        rep(NA_real_, length(self$participant_ids)), self$participant_ids)
      failures <- list()

      # The participants score their desire from the log as they see it,
      # with every speaker under its handle.
      agent_log <- .fg_agent_log(focus_group)
      history_string <- make_prompt_history(
        agent_log,
        include_summary = focus_group$current_conversation_summary %||% NULL,
        max_tokens_history = 64000L
      )

      last_utterance_info <- if (length(agent_log) > 0) {
        agent_log[[length(agent_log)]]
      } else {
        list(speaker_id = "N/A", text = "N/A")
      }

      # Vectorized parallel desire scoring using LLMR broadcast when available
      eligible_ids <- self$participant_ids
      excluded <- character()
      if (!is.null(self$last_speaker_id) && self$last_speaker_id %in% eligible_ids && length(eligible_ids) > 1) {
        eligible_ids <- setdiff(eligible_ids, self$last_speaker_id)
        excluded[[self$last_speaker_id]] <- "previous_speaker"
      }
      desire_call_params <- .fg_call_params(focus_group, "desire")
      score_per_agent <- function(ids) {
        for (pid in ids) {
          agent <- self$agents[[pid]]
          sc <- tryCatch(
            .fg_with_group_usage(focus_group, agent$get_need_to_talk(
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
              conversation_log = agent_log,
              max_tokens_retry = focus_group$max_tokens_desire_retry,
              call_params = desire_call_params
            )),
            error = function(e) {
              failures[[pid]] <<- e
              NA_real_
            }
          )
          if (is.na(sc) && identical(private$on_desire_failure, "error")) {
            private$stop_for_desire_failure(pid, failures[[pid]])
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
      desire_transcript <- .fg_log_to_transcript(agent_log)
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
        agent <- self$agents[[pid]]
        .fg_build_agent_messages(desire_transcript, .fg_agent_label(agent),
                                 .fg_desire_system_text(agent),
                                 instruction = instruction, self_state = NULL)
      }

      # Record a broadcast's usage on the group, one record per returned row,
      # and on each agent when the rows can be matched to the agents scored.
      # Returns the ids in row order, or NULL when the rows cannot be matched.
      record_group_usage <- function(out, ids) {
        rows <- .fg_usage_rows(out)
        row_level <- is.data.frame(out) || length(rows) == length(ids)
        mapping <- seq_along(rows)
        if (is.list(out) && "agent_id" %in% names(out)) {
          mapping <- match(out$agent_id, ids)
        } else if (is.list(out) && ".experiment_id" %in% names(out)) {
          mapping <- match(out$.experiment_id, seq_along(ids))
        }
        matched <- row_level && length(rows) == length(ids) &&
          !anyNA(mapping) && !anyDuplicated(mapping)
        for (i in seq_along(rows)) {
          usage <- .fg_usage_record(rows[[i]])
          if (!row_level) usage$unknown <- NA_real_
          .fg_record_group(focus_group, usage)
          if (matched) .fg_add_agent_usage(self$agents[[ids[[mapping[[i]]]]]], usage)
        }
        if (!matched) {
          warning(paste("Broadcast usage could not be matched to individual agent calls.",
                        "Per-agent usage is unavailable for this batch."), call. = FALSE)
        }
        if (matched) ids[mapping] else NULL
      }

      # Broadcast one homogeneous group. Returns the ids it could NOT score (an
      # empty character vector means all scored). A reply that is empty or
      # length-truncated is reported as unscored rather than scored 0, so the
      # caller re-scores it on the per-agent path (which retries at a larger
      # budget). The desire budget follows the configured `max_tokens_desire`;
      # the configuration's temperature is kept, and none is added when it is
      # unset (some reasoning models accept only their native value).
      broadcast_group <- function(ids) {
        cfg <- self$agents[[ids[1]]]$config
        cfg$model_params$max_tokens <- focus_group$max_tokens_desire
        cfg <- .fg_with_params(cfg, desire_call_params)
        group_msgs <- lapply(ids, build_desire_msg)
        broadcast_error <- NULL
        out <- tryCatch(
          LLMR::call_llm_broadcast(config = cfg, messages = group_msgs,
                                   tries = 5, wait_seconds = 2, backoff_factor = 3,
                                   progress = FALSE),
          error = function(e) {
            broadcast_error <<- e
            NULL
          })
        if (is.null(out)) {
          .fg_add_group_usage(focus_group, 0, 0, 0, NA_real_)
          for (i in seq_along(ids)) {
            .fg_emit_call(.fg_call_record(
              cfg, group_msgs[[i]], list(type = "desire", agent_id = ids[[i]]),
              error = broadcast_error))
          }
          return(ids)
        }
        mapped <- record_group_usage(out, ids)
        rows <- .fg_usage_rows(out)
        for (i in seq_along(rows)) {
          pid <- if (is.null(mapped)) NA_character_ else mapped[[i]]
          record <- .fg_call_record(
            cfg, if (is.na(pid)) list() else group_msgs[[match(pid, ids)]],
            list(type = "desire", agent_id = pid), response = rows[[i]])
          if (!isTRUE(rows[[i]]$success)) {
            record$error <- as.character(rows[[i]]$error_message %|NA|%
                                           "The broadcast reported an unsuccessful call.")
          }
          record$cleaned_text <- .fg_final_answer(
            if (is.na(record$raw_text)) "" else record$raw_text)
          record$score <- if (isTRUE(rows[[i]]$success) &&
                              !identical(record$finish_reason, "length"))
            as.numeric(parse_score_0_10(record$cleaned_text)) else NA_real_
          .fg_emit_call(record)
        }
        if (is.null(mapped)) return(ids)
        if (is.null(out) || is.null(out$success) || is.null(out$response_text)) return(ids)
        unscored <- character(0)
        for (i in seq_along(mapped)) {
          txt <- as.character(out$response_text[i] %||% "")
          txt <- if (!length(txt) || is.na(txt[[1]])) "" else .fg_final_answer(txt[[1]])
          sc <- if (isTRUE(out$success[i]) && nzchar(trimws(txt)) &&
                    !identical(as.character(out$finish_reason[i] %||% NA), "length"))
            parse_score_0_10(txt) else NA_integer_
          if (!is.na(sc)) desire_scores[mapped[[i]]] <<- as.numeric(sc)
          else unscored <- c(unscored, mapped[[i]])
        }
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
          # below, after the broadcast's usage has been recorded. Only when
          # EVERY group fails do we drop to the full per-agent path (and
          # relabel the mode accordingly).
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

      # Only `on_desire_failure = "random"` reaches here with failures; the
      # valid scores are kept, and the speaker is drawn uniformly only when
      # none is valid.
      scored <- desire_scores[eligible_ids]
      valid <- scored[!is.na(scored)]
      for (pid in names(failures)) excluded[[pid]] <- "score_failed"
      route <- function(rule, selected, tied = character(), tie_break = "none",
                        threshold = private$min_desire_threshold) {
        self$routing_record <- .fg_routing_record(
          "desire_based", rule, eligible_ids, selected, excluded = excluded,
          scores = scored, threshold = threshold, tied = tied,
          tie_break = tie_break, scoring_mode = private$last_scoring_mode)
        self$agents[[selected]]
      }
      failure_record <- NULL
      if (length(failures)) {
        reasons <- unique(vapply(failures, conditionMessage, character(1)))
        failure_record <- list(
          reason = paste(reasons, collapse = "; "),
          fallback = if (length(valid)) "valid_scores" else "uniform_random",
          candidates = unname(eligible_ids),
          failed = names(failures)
        )
        if (!private$failure_message_emitted) {
          message(paste(
            "Desire scoring failed; choosing among the valid scores, or",
            "uniformly when none is valid (on_desire_failure = \"random\")."))
          private$failure_message_emitted <- TRUE
        }
        if (!length(valid)) {
          self$selection_metadata <- list(desire_scoring_failure = failure_record)
          return(route("uniform_after_failure", sample(eligible_ids, 1L),
                       tied = unname(eligible_ids),
                       tie_break = if (length(eligible_ids) > 1L) "random" else "none",
                       threshold = NA_real_))
        }
      }
      with_failure <- function(metadata) {
        if (!is.null(failure_record)) {
          metadata$desire_scoring_failure <- failure_record
        }
        metadata
      }

      eligible_participants <- valid[valid >= private$min_desire_threshold]
      threshold_used <- private$min_desire_threshold

      # If no one meets the threshold but there's ongoing conversation, lower the bar
      if (length(eligible_participants) == 0 && length(agent_log) > 2) {
        # Allow lower threshold for ongoing conversations to maintain flow
        eligible_participants <- valid[valid >= (private$min_desire_threshold - 1)]
        if (length(eligible_participants)) {
          threshold_used <- private$min_desire_threshold - 1
          self$selection_metadata <- list(
            flow = "desire_based", fallback = "threshold_lowered",
            threshold = private$min_desire_threshold - 1,
            scores = as.list(scored))
        }
      }

      if (length(eligible_participants) == 0) {
        # If still no one eligible, pick the highest scored participant even if 0 to keep conversation moving
        max_score <- max(valid)
        candidates <- names(valid)[valid == max_score]
        next_speaker_id <- if (length(candidates) == 1) candidates[1] else sample(candidates, 1)
        self$selection_metadata <- with_failure(list(
          flow = "desire_based", fallback = "highest_below_threshold",
          threshold = private$min_desire_threshold,
          scores = as.list(scored), selected_id = next_speaker_id))
        return(route("highest_below_threshold", next_speaker_id, tied = candidates,
                     tie_break = if (length(candidates) > 1L) "random" else "none"))
      }

      max_score <- max(eligible_participants)
      candidates <- names(eligible_participants)[eligible_participants == max_score]
      if (length(candidates) == 1) {
        next_speaker_id <- candidates[1]
      } else {
        # Tie-breaking: random among highest scorers
        next_speaker_id <- sample(candidates, 1)
      }
      if (!is.null(failure_record)) {
        self$selection_metadata <- with_failure(self$selection_metadata %||% list())
      }
      return(route(
        if (threshold_used < private$min_desire_threshold) "threshold_lowered" else "highest_score",
        next_speaker_id, tied = candidates,
        tie_break = if (length(candidates) > 1L) "random" else "none",
        threshold = threshold_used))
    }
  ),

  private = list(
    last_desire_scores = NULL,
    last_scoring_mode = NULL,
    min_desire_threshold = 3,
    on_desire_failure = "error",
    failure_message_emitted = FALSE,

    stop_for_desire_failure = function(agent_id, cause) {
      rlang::abort(
        c(sprintf("Desire scoring failed for participant %s: %s", agent_id,
                  conditionMessage(cause)),
          i = paste("Set `on_desire_failure = \"random\"` to choose among the",
                    "valid scores instead.")),
        class = "focusgroup_desire_failure", agent_id = agent_id,
        parent = cause)
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
#'   For "desire_based": `min_desire_threshold` (numeric) and
#'   `on_desire_failure`, `"error"` (the default) or `"random"`. Under
#'   `"error"` a desire score that cannot be obtained, after its retry, stops
#'   the session with an error of class `focusgroup_desire_failure`. Under
#'   `"random"` the speaker is chosen among the participants with valid
#'   scores by the usual rule, and uniformly among the eligible participants
#'   only when none has a valid score; the failure is recorded in the
#'   selected message's `metadata$desire_scoring_failure`. Earlier versions
#'   always chose uniformly at random when any score failed.
#' @return An initialized `ConversationFlow` object (e.g., `RoundRobinFlow`, `ProbabilisticFlow`, `DesireBasedFlow`).
#' @export
#' @examples
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                         api_key = LLMR::llm_api_key_env("GROQ_API_KEY"))
#' agents <- create_agents(
#'   n_participants = 2, config = cfg,
#'   direct_persona_descriptions = c("A retired teacher.", "A night-shift nurse."))
#' flow <- create_conversation_flow("desire_based", agents, "MOD",
#'                                  flow_params = list(min_desire_threshold = 4))
#' class(flow)
#' flow$participant_ids
create_conversation_flow <- function(mode, agents, moderator_id, flow_params = list()) {
  th <- flow_params$min_desire_threshold
  if (!is.null(th) && (!is.numeric(th) || length(th) != 1L || is.na(th) ||
                       th < 0 || th > 10)) {
    stop("`min_desire_threshold` must be one number between 0 and 10.",
         call. = FALSE)
  }
  ri <- flow_params$recovery_increment
  if (!is.null(ri) && (!is.numeric(ri) || length(ri) != 1L || is.na(ri) ||
                       ri < 0 || ri > 1)) {
    stop("`recovery_increment` must be one number between 0 and 1.",
         call. = FALSE)
  }
  ip <- flow_params$initial_propensities
  if (!is.null(ip) && (!is.numeric(ip) || anyNA(ip) || any(!is.finite(ip)) ||
                       any(ip < 0) || !any(ip > 0))) {
    stop("`initial_propensities` must be finite, nonnegative, and not all zero.",
         call. = FALSE)
  }
  odf <- flow_params$on_desire_failure %||% "error"
  if (!is.character(odf) || length(odf) != 1L || !odf %in% c("error", "random")) {
    stop("`on_desire_failure` must be \"error\" or \"random\".", call. = FALSE)
  }
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
      min_desire_threshold = flow_params$min_desire_threshold %||% 3,
      on_desire_failure = odf
      ),
    stop(paste0("Unknown conversation flow mode: '", mode,
                "'. Supported modes are 'round_robin', 'probabilistic', 'desire_based'."))
  )
}

# A flow's record of one selection. `excluded` names the participants passed
# over, with the reason for each; `scores` are desire scores or, for the
# probabilistic flow, selection probabilities.
.fg_routing_record <- function(flow, rule, candidates, selected_id,
                               excluded = character(), scores = NULL,
                               threshold = NA_real_, tied = character(),
                               tie_break = "none", scoring_mode = NULL,
                               position = NA_integer_) {
  list(flow = flow, rule = rule, candidates = unname(as.character(candidates)),
       excluded = excluded, scores = scores, threshold = as.numeric(threshold),
       tied = unname(as.character(tied)), tie_break = tie_break,
       selected_id = as.character(selected_id),
       scoring_mode = scoring_mode %||% NA_character_,
       position = as.integer(position))
}
