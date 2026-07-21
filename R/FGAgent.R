# File: R/FGAgent.R
# Purpose: Defines the FGAgent R6 class for focus group participants and moderators.

#' @import R6
#' @importFrom LLMR call_llm_robust llm_config
NULL

#' FGAgent Class
#'
#' Represents an agent (participant or moderator) in a focus group simulation.
#' Each agent has a unique ID, a persona, an LLM configuration, and methods to
#' generate utterances and express a desire to speak.
#'
#' @field id Character. Unique identifier for the agent.
#' @field persona_description Character. Textual description of the agent's persona,
#'   derived from demographics, survey responses, or direct input.
#' @field communication_style_instruction Character. A specific instruction about the agent's
#'   communication style, to be included in prompts.
#' @field config An `llm_config` object (from the `LLMR` package) specifying the
#'   LLM provider, model, API key, and other parameters for this agent.
#' @field .runner `NULL` or an experiments-frame runner. It receives a data
#'   frame with `config` and `messages` list-columns and returns those rows with
#'   at least `response_text`.
#' @field is_moderator Logical. `TRUE` if the agent is the moderator, `FALSE` otherwise.
#' @field history List. A log of utterances made by this agent during the simulation.
#' @field tokens_sent_agent Numeric. Total tokens sent by this agent.
#' @field tokens_received_agent Numeric. Total tokens received by this agent.
#'
#' @section Customizing Agents:
#' `FGAgent` is designed to be flexible:
#' \itemize{
#'   \item **Per-Agent LLM Configuration**: Each agent is initialized with its own
#'     `llm_config` (stored in the `config` field). This allows different
#'     agents to use different LLMs, temperatures, or even providers.
#'   \item **Persona Definition**: The `agent_details` (containing `demographics`,
#'     `survey_responses`, `direct_persona_description`, and/or `communication_style`)
#'     are used to construct the `persona_description` and `communication_style_instruction`.
#'   \item **Subclassing**: For advanced customization, inherit from `FGAgent` to override
#'     methods or add new fields.
#' }
#'
#' @export
FGAgent <- R6::R6Class("FGAgent",
  public = list(
    id = NULL,
    persona_description = NULL,
    communication_style_instruction = NULL,
    config = NULL,
    .runner = NULL,
    #' @field role Character. "moderator" or "participant" for convenience in reports.
    role = NULL,
    #' @field demographics Named list. Raw demographics used to build persona.
    demographics = NULL,
    #' @field survey_responses Named list. Raw survey responses used to build persona.
    survey_responses = NULL,
    is_moderator = FALSE,
    history = list(),
    tokens_sent_agent = 0,
    tokens_received_agent = 0,

    #' @description Initialize a new FGAgent.
    #' @param id Character. A unique identifier for the agent.
    #' @param agent_details List. Contains information to build the agent's persona.
    #'   Can include:
    #'   \itemize{
    #'     \item `demographics`: A named list of demographic attributes (e.g., `list(age = 30, occupation = "teacher")`).
    #'     \item `survey_responses`: A named list of survey questions and answers.
    #'     \item `direct_persona_description`: A character string to be used directly as the persona. Overrides demographics/survey if provided.
    #'     \item `communication_style`: A character string describing the agent's communication style (e.g., "analytical and direct", "empathetic and story-driven").
    #'   }
    #'   If `is_moderator` is `TRUE` and no specific details are provided, a default moderator persona is used.
    #' @param config An `llm_config` object from `LLMR::llm_config()`. It may be
    #'   `NULL` only for an analysis-only agent that will not generate output.
    #' @param is_moderator Logical. `TRUE` if this agent is the moderator, `FALSE` otherwise.
    #' @param .runner `NULL` or an experiments-frame runner. `NULL` uses live
    #'   LLMR calls.
    initialize = function(id, agent_details, config, is_moderator = FALSE,
                          .runner = NULL) {
      if (!is.character(id) || length(id) != 1 || nchar(id) == 0) {
        stop("Agent 'id' must be a non-empty character string.")
      }
      if (!is.list(agent_details)) {
        stop("'agent_details' must be a list.")
      }
      if (!is.null(config) && !inherits(config, "llm_config")) {
        stop("'config' must be NULL or an 'llm_config' object from LLMR::llm_config().")
      }
      if (!is.null(.runner) && !is.function(.runner)) {
        stop("'.runner' must be NULL or an experiments-frame runner.")
      }

      self$id <- id
      self$config <- config
      self$.runner <- .runner
      self$is_moderator <- is_moderator
      self$role <- if (is_moderator) "moderator" else "participant"
      # Expose raw inputs for reporting/analysis convenience
      self$demographics <- agent_details$demographics %||% list()
      self$survey_responses <- agent_details$survey_responses %||% list()
      self$history <- list()
      self$tokens_sent_agent <- 0
      self$tokens_received_agent <- 0

      persona_elements <- private$construct_persona_elements(agent_details, is_moderator)
      self$persona_description <- persona_elements$description
      self$communication_style_instruction <- persona_elements$style_instruction

      if (is.null(self$persona_description) || nchar(trimws(self$persona_description)) == 0) {
        stop(paste("Persona description for agent", id, "could not be established or is empty."))
      }
    },

    #' @description Generate an utterance for the agent.
    #' @param topic Character. The current discussion topic.
    #' @param conversation_history_string Character. Formatted string of recent conversation history.
    #' @param utterance_prompt_template Character. The prompt template to use.
    #' @param max_tokens_utterance Integer. Maximum tokens for the generated utterance.
    #' @param current_moderator_question Character. The current question posed by the moderator.
    #' @param conversation_summary_so_far Character. A summary of earlier parts of the conversation.
    #' @param current_phase Character. The current phase of the focus group (e.g., "icebreaker", "exploration").
    #' @param conversation_log List or NULL. The structured conversation log. When
    #'   supplied (and the template is not a legacy flat template), the message is
    #'   built role-flipped: this agent's own prior turns become `assistant`
    #'   messages and others' become labeled `user` messages. `NULL` keeps the
    #'   legacy flat single-user-message construction.
    #' @param standing_rules Character or NULL. The system-message standing rules
    #'   (persona-anchoring, safety, etc.) for the role-flipped path; `NULL` uses a
    #'   built-in default appropriate to the agent's role.
    #' @param self_state Logical. If `TRUE` (default), a compact "points you have
    #'   already made" digest of the agent's own prior turns is added to the system
    #'   message (participants only) to discourage self-repetition.
    #' @return A list with `text` (the generated utterance) and `meta` (a list
    #'   of call metadata: token counts, finish reason, provider, model, timing).
    generate_utterance = function(topic,
                                  conversation_history_string,
                                  utterance_prompt_template,
                                  max_tokens_utterance = 150,
                                  current_moderator_question = "N/A",
                                  conversation_summary_so_far = "N/A",
                                  current_phase = "discussion",
                                  conversation_log = NULL,
                                  standing_rules = NULL,
                                  self_state = TRUE) {

      if (is.null(self$config)) {
        stop("An explicit config is required to generate an utterance.", call. = FALSE)
      }

      prompt_values <- list(
        persona_description = self$persona_description,
        communication_style_instruction = self$communication_style_instruction %||% "",
        topic = topic,
        conversation_history = conversation_history_string %||% "No prior messages in this segment.",
        current_moderator_question = current_moderator_question %||% "N/A",
        conversation_summary_so_far = conversation_summary_so_far %||% "N/A",
        focus_group_purpose = "{{focus_group_purpose}}", # This will be filled by FocusGroup if template uses it
        last_speaker_id = "{{last_speaker_id}}",
        last_utterance_text = "{{last_utterance_text}}",
        other_participant_id_placeholder = "{{other_participant_id_placeholder}}", # For moderator prompts
        dominant_speaker_id_placeholder = "{{dominant_speaker_id_placeholder}}",
        quiet_speaker_id_placeholder = "{{quiet_speaker_id_placeholder}}",
        next_focus_group_question = "{{next_focus_group_question}}"
      )
      final_prompt <- replace_placeholders(utterance_prompt_template, prompt_values)

      # If topic is missing/empty, remove any topic-specific lines from the prompt to avoid empty 'Main topic' statements
      if (is.null(topic) || !nzchar(trimws(topic))) {
        # Remove lines mentioning the topic to avoid prompting the model to output an empty topic line
        final_prompt <- gsub("(?mi)^.*\\b(main topic|focus group topic)\\b.*$", "", final_prompt, perl = TRUE)
        # Collapse excessive blank lines
        final_prompt <- gsub("\n{3,}", "\n\n", final_prompt)
        final_prompt <- sub("^(\n)+", "", final_prompt)
      }

      current_config <- self$config
      current_config$model_params$max_tokens <- max_tokens_utterance
      current_config$model_params$temperature <- current_config$model_params$temperature %||% 0.7

      # Build the messages. A legacy/custom template that inlines the transcript
      # (via {{conversation_history}}/{{persona_description}}) keeps the old
      # single-user-message path so user customizations run byte-for-byte. The
      # default path role-flips: persona+rules -> system, the transcript -> own
      # turns as assistant and others as labeled user, the current question ->
      # trailing user instruction.
      legacy_template <- grepl("\\{\\{(conversation_history|persona_description)\\}\\}",
                               utterance_prompt_template)
      if (legacy_template || is.null(conversation_log)) {
        msgs <- list(list(role = "user", content = final_prompt))
      } else {
        transcript <- .fg_log_to_transcript(conversation_log)
        sys_text <- paste(c(
          self$persona_description,
          self$communication_style_instruction %||% "",
          if (!is.null(conversation_summary_so_far) &&
              nzchar(conversation_summary_so_far) &&
              !identical(conversation_summary_so_far, "N/A"))
            paste0("Summary of earlier discussion: ", conversation_summary_so_far),
          standing_rules %||% .fg_standing_rules(self$is_moderator)
        ), collapse = "\n\n")
        instruction <- final_prompt   # the filled template = this turn's task/question
        ss <- if (isTRUE(self_state) && !self$is_moderator)
          .fg_self_state(transcript, self$id) else NULL
        msgs <- .fg_build_agent_messages(transcript, self$id, sys_text,
                                         instruction = instruction, self_state = ss)
      }

      response_obj <- .fg_call_llm(
        config = current_config,
        messages = msgs,
        .runner = self$.runner,
        tries = 5,
        wait_seconds = 2,
        backoff_factor = 3
      )

      utterance_text <- .fg_response_text(response_obj)
      u <- extract_token_counts(response_obj)
      # NA-safe: a provider that reports no usage yields NA counts, which must
      # accumulate as 0, not turn the running totals into NA for good.
      agg_sent <- .fg_tok0(u$sent)
      agg_rec  <- .fg_tok0(u$rec)
      self$tokens_sent_agent     <- self$tokens_sent_agent     + agg_sent
      self$tokens_received_agent <- self$tokens_received_agent + agg_rec

      # meta is built from aggregates after any retry so the caller always has full usage.
      final_response_obj <- response_obj
      
      # With role-flip the agent's own voice is in the assistant channel, so the
      # "As <id>..." leakage the old persona-specific rewrites fought is rare.
      # Keep one thin guard against a leading role preamble; drop the rest.
      original_text <- utterance_text
      cleaned <- trimws(original_text)
      cleaned <- sub("^As\\s+(an?|the)\\b[^,]*,\\s*", "", cleaned, ignore.case = TRUE)
      if (!nzchar(cleaned)) cleaned <- original_text
      # Keep the longer/non-empty between cleaned and original
      if (nchar(cleaned) >= 20 || nchar(original_text) == 0) {
        utterance_text <- cleaned
      } else {
        utterance_text <- trimws(original_text)
      }

      # Fallback: retry once only when the model was actually cut off, returned
      # nothing, or trailed off mid-thought. A short, well-formed answer ("I
      # agree.") is normal in a discussion and is left alone; padding it would
      # make the transcript read artificially.
      first_finish <- .fg_response_finish(final_response_obj)
      is_incomplete <- !nzchar(trimws(utterance_text)) ||
        identical(first_finish, "length") ||
        grepl("\\.\\.\\.\\s*$", utterance_text)
      if (is_incomplete) {
        complete_cue <- paste0(
          "Important: your prior output was incomplete. Now provide a complete, ",
          "self-contained response of 3-5 sentences. Do not mention your persona ",
          "or role. Do not start with 'As a'. Focus on the current question (",
          current_moderator_question %||% "", ") and the recent discussion."
        )
        # Append the cue as a trailing user turn on the same array and renormalize
        # so it does not sit adjacent to the prior trailing instruction.
        retry_msgs <- LLMR::ensure_alternating_messages(
          c(msgs, list(list(role = "user", content = complete_cue))))
        current_config$model_params$max_tokens <- max(max_tokens_utterance, 320L)
        response_obj2 <- .fg_call_llm(
          config = current_config,
          messages = retry_msgs,
          .runner = self$.runner,
          tries = 3,
          wait_seconds = 2,
          backoff_factor = 2
        )
        u2 <- extract_token_counts(response_obj2)
        retry_sent <- .fg_tok0(u2$sent)
        retry_rec <- .fg_tok0(u2$rec)
        agg_sent <- agg_sent + retry_sent
        agg_rec <- agg_rec + retry_rec
        self$tokens_sent_agent <- self$tokens_sent_agent + retry_sent
        self$tokens_received_agent <- self$tokens_received_agent + retry_rec

        cand <- trimws(.fg_response_text(response_obj2))
        cand <- sub("^As\\s+(an?|the)\\b[^,]*,\\s*", "", cand, ignore.case = TRUE)
        retry_incomplete <- !nzchar(cand) ||
          identical(.fg_response_finish(response_obj2), "length") ||
          grepl("\\.\\.\\.\\s*$", cand)
        if (retry_incomplete) {
          stop("The model returned an incomplete utterance after retry.",
               call. = FALSE)
        }
        utterance_text <- cand
        final_response_obj <- response_obj2
      }

      meta <- list(
        response_id   = .fg_response_field(final_response_obj, "response_id", NA_character_),
        finish_reason = .fg_response_finish(final_response_obj),
        sent_tokens   = as.integer(agg_sent),
        rec_tokens    = as.integer(agg_rec),
        total_tokens  = as.integer(agg_sent + agg_rec),
        duration_s    = .fg_response_field(final_response_obj, "duration_s", NA_real_),
        provider      = self$config$provider %||% NA_character_,
        model         = self$config$model %||% NA_character_
      )

      self$history <- c(self$history, list(list(topic = topic, text = utterance_text, timestamp = Sys.time(), phase = current_phase)))
      return(list(text = utterance_text, meta = meta))
    },

    #' @description Get the agent's "desire to talk" score.
    #' This method queries the LLM to rate how strongly the agent feels the need to
    #' contribute to the discussion at the current moment.
    #' @param topic Character. The current discussion topic.
    #' @param conversation_history_string Character. Formatted string of recent conversation history.
    #' @param desire_prompt_template Character. The prompt template to use for this query.
    #' @param max_tokens_desire Integer. Maximum tokens for the LLM's response to the desire query.
    #' @param current_moderator_question Character. The current question posed by the moderator.
    #' @param last_speaker_id Character. The ID of the agent who spoke last.
    #' @param last_utterance_text Character. The text of the last utterance.
    #' @param conversation_log List or NULL. The structured conversation log. When
    #'   supplied (and the template is not a legacy flat template), desire scoring
    #'   is role-flipped so the agent reads its own prior turns as its own voice;
    #'   `NULL` keeps the legacy flat construction.
    #' @return Numeric. A score from 0 (no desire) to 10 (very strong desire).
    get_need_to_talk = function(topic,
                                conversation_history_string,
                                desire_prompt_template,
                                max_tokens_desire = 20,
                                current_moderator_question = "N/A",
                                last_speaker_id = "N/A",
                                last_utterance_text = "N/A",
                                conversation_log = NULL) {
      if (is.null(self$config)) {
        stop("An explicit config is required for desire scoring.", call. = FALSE)
      }
      prompt_values <- list(
        persona_description = self$persona_description,
        topic = topic,
        conversation_history = conversation_history_string %||% "No prior messages in this segment.",
        current_moderator_question = current_moderator_question %||% "N/A",
        last_speaker_id = last_speaker_id %||% "another participant",
        last_utterance_text = last_utterance_text %||% "(no recent utterance)"
      )
      final_prompt <- replace_placeholders(desire_prompt_template, prompt_values)

      current_config <- self$config
      current_config$model_params$max_tokens <- max_tokens_desire
      # Do not inject a temperature for desire scoring: some reasoning models
      # require their native temperature (e.g. 1) and the score is a single
      # integer, so the caller's sampling setting is left untouched.
      current_config$model_params$temperature <- NULL

      # Desire scoring is still an agent-perspective judgment: if it reads its own
      # prior turns as external "SpeakerID:" text it can misjudge novelty. Role-
      # flip when the structured log is available; no self-state block (keep it
      # cheap). A legacy/custom template that inlines the transcript stays flat.
      legacy_template <- grepl("\\{\\{(conversation_history|persona_description)\\}\\}",
                               desire_prompt_template)
      if (legacy_template || is.null(conversation_log)) {
        desire_msgs <- list(list(role = "user", content = final_prompt))
      } else {
        transcript <- .fg_log_to_transcript(conversation_log)
        sys_text <- paste(c(self$persona_description,
                            "You are rating your own desire to speak next."),
                          collapse = "\n")
        desire_msgs <- .fg_build_agent_messages(transcript, self$id, sys_text,
                                                instruction = final_prompt,
                                                self_state = NULL)
      }

      score_once <- function(cfg) {
        ro <- .fg_call_llm(
          config = cfg, messages = desire_msgs,
          .runner = self$.runner,
          tries = 5, wait_seconds = 2, backoff_factor = 3
        )
        u <- extract_token_counts(ro)
        self$tokens_sent_agent <- self$tokens_sent_agent + .fg_tok0(u$sent)
        self$tokens_received_agent <- self$tokens_received_agent + .fg_tok0(u$rec)
        ro
      }

      response_obj <- score_once(current_config)
      response_text <- .fg_response_text(response_obj)

      # A reasoning model can spend the whole token budget on hidden reasoning and
      # return an empty (or length-truncated) reply, which would silently score 0
      # and flatten turn selection. When that happens, retry once with a generous
      # budget so the visible integer can emerge. Costs nothing for ordinary
      # models, which answer within the default budget on the first call.
      truncated <- identical(.fg_response_finish(response_obj), "length")
      if ((!nzchar(trimws(response_text)) || truncated) &&
          max_tokens_desire < .fg_desire_retry_tokens) {
        retry_cfg <- current_config
        retry_cfg$model_params$max_tokens <- .fg_desire_retry_tokens
        response_obj <- score_once(retry_cfg)
        response_text <- .fg_response_text(response_obj)
      }

      fraction_score <- grepl(
        "\\b(10|[0-9])\\s*(/|out of)\\s*10\\b",
        response_text, ignore.case = TRUE, perl = TRUE
      )
      without_ranges <- gsub(
        "\\(?\\b(10|[0-9])\\s*(-|to)\\s*(10|[0-9])\\b\\)?", " ",
        response_text, ignore.case = TRUE
      )
      scalar_score <- grepl("\\b(10|[0-9])\\b", without_ranges, perl = TRUE)
      if (!nzchar(trimws(response_text)) || !(fraction_score || scalar_score)) {
        stop("Desire scoring returned no valid score from 0 to 10.",
             call. = FALSE)
      }

      sc <- parse_score_0_10(response_text)
      return(as.numeric(sc))
    }
  ),

  private = list(
    # Constructs persona description and communication style instruction
    construct_persona_elements = function(details, is_moderator_flag) {
      desc_parts <- c()
      style_instr <- ""

      if (!is.null(details$direct_persona_description) && nzchar(details$direct_persona_description)) {
        desc_parts <- c(desc_parts, details$direct_persona_description)
      } else {
        if (!is.null(details$demographics) && is.list(details$demographics) && length(details$demographics) > 0) {
          desc_parts <- c(desc_parts, paste("Key characteristics:", format_demographics(details$demographics)))
        }
        if (!is.null(details$survey_responses) && is.list(details$survey_responses) && length(details$survey_responses) > 0) {
          desc_parts <- c(desc_parts, format_survey_responses(details$survey_responses))
        }
      }

      if (!is.null(details$communication_style) && nzchar(details$communication_style)) {
        style_instr <- paste("Your communication style is:", details$communication_style,
                             "Incorporate this style naturally into your responses.")
      }

      final_description <- paste(desc_parts, collapse = "\n\n")

      if (is_moderator_flag) {
        if (nchar(trimws(final_description)) == 0) { # No specific details for moderator
          final_description <- "Role: Experienced Focus Group Moderator. Objective: To facilitate a productive discussion, ensuring all participants can voice opinions. Skilled in asking probing questions, keeping conversation on track, and managing group dynamics. Aims for a comfortable, open environment. You are neutral and do not express personal opinions on the topic."
        }
        if (nchar(trimws(style_instr)) == 0) {
          style_instr <- "Your communication style is: Professional, clear, encouraging, and neutral."
        }
      } else { # Participant
         if (nchar(trimws(final_description)) == 0) {
            final_description <- "A thoughtful participant with general interest in the topic." # Generic fallback
         }
         if (nchar(trimws(style_instr)) == 0) {
            style_instr <- "Communicate naturally and clearly." # Generic fallback
         }
      }
      return(list(description = trimws(final_description), style_instruction = trimws(style_instr)))
    }
  )
)
