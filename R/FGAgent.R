# File: R/FGAgent.R
# Purpose: Defines the FGAgent R6 class for focus group participants and moderators.

#' @import R6
#' @importFrom LLMR call_llm_robust llm_config
NULL

#' FGAgent Class
#'
#' @description
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

      current_call_config <- self$model_config
      current_call_config$model_params$max_tokens <- max_tokens_utterance

      response_obj <- LLMR::call_llm_robust(
        config = current_call_config,
        messages = list(list(role = "user", content = final_prompt)),
        tries = 5
      )
      
      utterance_text <- as.character(response_obj) # Assuming robust call returns text directly
      usage <- .extract_llm_usage(response_obj) # Hypothetical, depends on LLMR
      self$tokens_sent_agent <- self$tokens_sent_agent + (usage$prompt_tokens %||% 0)
      self$tokens_received_agent <- self$tokens_received_agent + (usage$completion_tokens %||% 0)
      
      # Clean up potential self-references if LLM includes them despite instructions
      utterance_text <- gsub(paste0("^As (an?|the) ", self$id, ", I (think|feel|believe)"), "I \\2", utterance_text, ignore.case = TRUE)
      utterance_text <- gsub(paste0("^My name is ", self$id, " and I think"), "I think", utterance_text, ignore.case = TRUE)
      utterance_text <- gsub(paste0("^As a participant with persona.*?:\\s*"), "", utterance_text, ignore.case = TRUE)
      utterance_text <- trimws(utterance_text)

      self$history <- c(self$history, list(list(topic = topic, text = utterance_text, timestamp = Sys.time(), phase = current_phase)))
      return(utterance_text)
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
      current_call_config$model_params$temperature <- 0.1 # Lower temp for more deterministic score

      response_obj <- LLMR::call_llm_robust(
        config = current_call_config,
        messages = list(list(role = "user", content = final_prompt)),
        tries = 5
      )
      response_text <- as.character(response_obj)
      usage <- .extract_llm_usage(response_obj)
      self$tokens_sent_agent <- self$tokens_sent_agent + (usage$prompt_tokens %||% 0)
      self$tokens_received_agent <- self$tokens_received_agent + (usage$completion_tokens %||% 0)

      # Try to parse a number from 0-10. More robust parsing.
      score_match <- regmatches(response_text, regexpr("\\b(10|[0-9])\\b", response_text))
      if (length(score_match) > 0) {
        score <- as.numeric(score_match[[1]])
        return(min(max(score, 0), 10)) # Clamp to 0-10 range
      } else {
        warning(paste("Agent", self$id, "could not parse need_to_talk score from LLM response:", response_text, ". Assigning random low score (0-3)."))
        return(sample(0:3, 1)) # Return a random low score
      }
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