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
#' @field model_config An `llm_config` object (from the `LLMR` package) specifying the
#'   LLM provider, model, API key, and other parameters for this agent.
#' @field is_moderator Logical. `TRUE` if the agent is the moderator, `FALSE` otherwise.
#' @field history List. A log of utterances made by this agent during the simulation.
#' @field tokens_sent_agent Numeric. Total tokens sent by this agent.
#' @field tokens_received_agent Numeric. Total tokens received by this agent.
#'
#' @section Customizing Agents:
#' `FGAgent` is designed to be flexible:
#' \itemize{
#'   \item **Per-Agent LLM Configuration**: Each agent is initialized with its own `model_config`.
#'     This allows different agents to use different LLMs, temperatures, or even providers.
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
    model_config = NULL,
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
    #' @param model_config An `llm_config` object from `LLMR::llm_config()`.
    #' @param is_moderator Logical. `TRUE` if this agent is the moderator, `FALSE` otherwise.
    initialize = function(id, agent_details, model_config, is_moderator = FALSE) {
      if (!is.character(id) || length(id) != 1 || nchar(id) == 0) {
        stop("Agent 'id' must be a non-empty character string.")
      }
      if (!is.list(agent_details)) {
        stop("'agent_details' must be a list.")
      }
      if (!inherits(model_config, "llm_config")) {
        stop("'model_config' must be an 'llm_config' object from LLMR::llm_config().")
      }

      self$id <- id
      self$model_config <- model_config
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
    #' @return Character string containing the agent's generated utterance.
    generate_utterance = function(topic,
                                  conversation_history_string,
                                  utterance_prompt_template,
                                  max_tokens_utterance = 150,
                                  current_moderator_question = "N/A",
                                  conversation_summary_so_far = "N/A",
                                  current_phase = "discussion") {

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

      current_call_config <- self$model_config
      current_call_config$model_params$max_tokens <- max_tokens_utterance
      current_call_config$model_params$temperature <- current_call_config$model_params$temperature %||% 0.7

      response_obj <- LLMR::call_llm_robust(
        config = current_call_config,
        messages = list(list(role = "user", content = final_prompt)),
        tries = 5,
        wait_seconds = 2,
        backoff_factor = 3
      )

      utterance_text <- as.character(response_obj)
      u <- LLMR::tokens(response_obj)
      self$tokens_sent_agent <- self$tokens_sent_agent + (u$sent %||% 0L)
      self$tokens_received_agent <- self$tokens_received_agent + (u$rec %||% 0L)

      meta <- list(
        response_id   = response_obj$response_id %||% NA_character_,
        finish_reason = LLMR::finish_reason(response_obj) %||% NA_character_,
        sent_tokens   = u$sent %||% NA_integer_,
        rec_tokens    = u$rec %||% NA_integer_,
        total_tokens  = u$total %||% NA_integer_,
        duration_s    = response_obj$duration_s %||% NA_real_,
        provider      = self$model_config$provider %||% NA_character_,
        model         = self$model_config$model %||% NA_character_
      )
      
      # Clean up potential self-references if LLM includes them despite instructions
      original_text <- utterance_text
      cleaned <- trimws(original_text)
      cleaned <- sub("^As\\s+(an?|the)\\b[^,]*,\\s*", "", cleaned, ignore.case = TRUE)
      cleaned <- sub(paste0("^As (an?|the) ", self$id, ", I (think|feel|believe)"), "I \\2", cleaned, ignore.case = TRUE)
      cleaned <- sub(paste0("^My name is ", self$id, " and I think"), "I think", cleaned, ignore.case = TRUE)
      cleaned <- sub("^As a participant with persona.*?:\\s*", "", cleaned, ignore.case = TRUE)
      cleaned <- sub("^My name is[^\\n.]*([.]|\\n)\\s*", "", cleaned, ignore.case = TRUE)
      if (!nzchar(cleaned)) cleaned <- "I think the key point is the policy specifics and trade-offs for voters."
      # Keep the longer/non-empty between cleaned and original
      if (nchar(cleaned) >= 20 || nchar(original_text) == 0) {
        utterance_text <- cleaned
      } else {
        utterance_text <- trimws(original_text)
      }

      # Fallback: if the utterance is empty or clearly incomplete, request a complete response once
      is_incomplete <- nchar(utterance_text) < 40 || grepl("\\.\\.\\.$", utterance_text) || !grepl("[.!?]$", utterance_text)
      if (is_incomplete) {
        completion_prompt <- paste0(
          final_prompt,
          "\n\nImportant: Your prior output was incomplete. Now provide a complete, self-contained response of 3-5 sentences.\n",
          "Do not mention your persona or role. Do not start with 'As a'. Focus on the current question (",
          current_moderator_question %||% "",
          ") and the recent discussion."
        )
        current_call_config$model_params$max_tokens <- max(max_tokens_utterance, 320L)
        response_obj2 <- LLMR::call_llm_robust(
          config = current_call_config,
          messages = list(list(role = "user", content = completion_prompt)),
          tries = 3,
          wait_seconds = 2,
          backoff_factor = 2
        )
        cand <- as.character(response_obj2)
        cand <- gsub(paste0("^As (an?|the) ", self$id, ", I (think|feel|believe)"), "I \\2", cand, ignore.case = TRUE)
        cand <- gsub(paste0("^My name is ", self$id, " and I think"), "I think", cand, ignore.case = TRUE)
        cand <- gsub(paste0("^As a participant with persona.*?:\\s*"), "", cand, ignore.case = TRUE)
        cand <- sub("^My name is[^\\n.]*([.]|\\n)\\s*", "", cand, ignore.case = TRUE)
        cand <- trimws(cand)
        if (nchar(cand) >= max(nchar(utterance_text), 40)) {
          utterance_text <- cand
          # Update meta with second call usage if available
          u2 <- LLMR::tokens(response_obj2)
          self$tokens_sent_agent <- self$tokens_sent_agent + (u2$sent %||% 0L)
          self$tokens_received_agent <- self$tokens_received_agent + (u2$rec %||% 0L)
        }
      }

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
    #' @return Numeric. A score from 0 (no desire) to 10 (very strong desire).
    get_need_to_talk = function(topic,
                                conversation_history_string,
                                desire_prompt_template,
                                max_tokens_desire = 20,
                                current_moderator_question = "N/A",
                                last_speaker_id = "N/A",
                                last_utterance_text = "N/A") {
      prompt_values <- list(
        persona_description = self$persona_description,
        topic = topic,
        conversation_history = conversation_history_string %||% "No prior messages in this segment.",
        current_moderator_question = current_moderator_question %||% "N/A",
        last_speaker_id = last_speaker_id %||% "another participant",
        last_utterance_text = last_utterance_text %||% "(no recent utterance)"
      )
      final_prompt <- replace_placeholders(desire_prompt_template, prompt_values)

      current_call_config <- self$model_config
      current_call_config$model_params$max_tokens <- max_tokens_desire
      current_call_config$model_params$temperature <- 0.1

      response_obj <- LLMR::call_llm_robust(
        config = current_call_config,
        messages = list(list(role = "user", content = final_prompt)),
        tries = 5,
        wait_seconds = 2,
        backoff_factor = 3
      )
      response_text <- as.character(response_obj)
      u <- LLMR::tokens(response_obj)
      self$tokens_sent_agent <- self$tokens_sent_agent + (u$sent %||% 0L)
      self$tokens_received_agent <- self$tokens_received_agent + (u$rec %||% 0L)

      # Try to parse a number from 0-10. More robust parsing.
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