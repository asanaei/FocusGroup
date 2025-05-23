# File: R/utils.R
# Purpose: Utility functions for the FocusGroup package

#' @importFrom stats runif setNames
#' @importFrom utils modifyList
#' @importFrom rlang .data
#' @importFrom stringr str_count str_split
NULL

# Null-coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# --- Formatting Helpers ---

#' Format Demographics List to Text
#'
#' Converts a list of demographic information into a formatted string suitable for LLM prompts.
#'
#' @param demographics A named list where keys are demographic attributes (e.g., "age", "occupation")
#'        and values are the corresponding characteristics.
#' @return A character string with demographics formatted as "key1: value1; key2: value2; ...".
#'         Returns "No specific demographics provided." if the input is empty or uninformative.
#' @export
#' @examples
#' format_demographics(list(age = 30, occupation = "Engineer"))
format_demographics <- function(demographics) {
  if (is.null(demographics) || length(demographics) == 0) {
    return("No specific demographics provided.")
  }
  # Filter out NULL, NA, or empty string values before pasting
  demographics <- Filter(function(x) !is.null(x) && !is.na(x) && nzchar(as.character(x)), demographics)
  if (length(demographics) == 0) {
    return("No specific demographics provided after filtering empty values.")
  }
  paste(names(demographics), demographics, sep = ": ", collapse = "; ")
}

#' Format Survey Responses to Text
#'
#' Converts a list of survey responses into a formatted string suitable for LLM prompts.
#'
#' @param survey_responses A named list where keys are survey questions and values are the
#'        participant's answers.
#' @return A character string with survey responses formatted for clarity.
#'         Returns "No survey responses provided." if the input is empty.
#' @export
#' @examples
#' format_survey_responses(list(
#'   "What is your main concern about AI?" = "Job displacement.",
#'   "How often do you use product X?" = "Daily."
#' ))
format_survey_responses <- function(survey_responses) {
  if (is.null(survey_responses) || length(survey_responses) == 0) {
    return("No survey responses provided.")
  }
  formatted <- sapply(names(survey_responses), function(q) {
    paste0("Question: ", q, "\n  Answer: ", survey_responses[[q]])
  })
  paste("The participant provided the following responses to a pre-session questionnaire:\n",
        paste(formatted, collapse = "\n\n"))
}

#' Format Conversation History for Prompts
#'
#' Creates a string representation of the recent conversation history, optionally including
#' a summary of earlier parts of the discussion.
#'
#' @param conversation_log A list of message objects from the simulation. Each message
#'        should be a list with at least `speaker_id` and `text`.
#' @param n_recent Integer. The number of most recent messages to include in detail.
#' @param include_summary Character string. An optional summary of earlier parts of the
#'        conversation to prepend to the recent history.
#' @return A character string representing the formatted conversation history.
#' @export
#' @examples
#' log <- list(
#'   list(speaker_id = "Alice", text = "Hello!"),
#'   list(speaker_id = "Bob", text = "Hi Alice!"),
#'   list(speaker_id = "Alice", text = "How are you?")
#' )
#' format_conversation_history(log, n_recent = 2)
#' format_conversation_history(log, n_recent = 1, include_summary = "They greeted each other.")
format_conversation_history <- function(conversation_log, n_recent = 7, include_summary = NULL) {
  history_parts <- character(0)

  if (!is.null(include_summary) && nzchar(trimws(include_summary))) {
    history_parts <- c(history_parts, paste("Summary of earlier discussion:", include_summary, "\n---\nRecent turns:"))
  }

  if (length(conversation_log) == 0) {
    if (length(history_parts) > 0) return(paste(history_parts, collapse = "\n"))
    return("The conversation has not started yet.")
  }

  start_index <- max(1, length(conversation_log) - n_recent + 1)
  recent_messages_list <- conversation_log[start_index:length(conversation_log)]

  if (length(recent_messages_list) > 0) {
    formatted_messages <- sapply(recent_messages_list, function(msg) {
      paste0(msg$speaker_id %||% "UnknownSpeaker", ": ", msg$text %||% "")
    })
    history_parts <- c(history_parts, paste(formatted_messages, collapse = "\n"))
  } else if (length(history_parts) == 0) {
    # No summary and no recent messages (e.g., n_recent = 0 and log is not empty)
     return("No recent messages to display based on n_recent setting.")
  }
  
  if (length(history_parts) == 0) { # Should only happen if log was empty and no summary
      return("The conversation has not started yet.")
  }
  
  paste(history_parts, collapse = "\n")
}


#' Replace Placeholders in a String
#'
#' Replaces placeholders of the form `{{key}}` in a template string with
#' corresponding values from a named list.
#'
#' @param template_string A character string containing placeholders.
#' @param values_list A named list where names correspond to placeholders in the
#'        template string (without the curly braces).
#' @return A character string with placeholders replaced by their values. If a
#'         placeholder is not found in `values_list` or its value is `NULL`,
#'         it's replaced with an empty string.
#' @export
#' @examples
#' replace_placeholders("Hello, {{name}}! Today is {{day}}.",
#'                      list(name = "Alice", day = "Monday"))
#' replace_placeholders("Topic: {{topic}}, Question: {{question}}",
#'                      list(topic = "AI Ethics")) # {{question}} becomes ""
replace_placeholders <- function(template_string, values_list) {
  if (is.null(template_string) || !is.character(template_string) || length(template_string) != 1) {
    stop("template_string must be a single character string.")
  }
  # Ensure values_list is a list, even if NULL or empty
  if (is.null(values_list)) values_list <- list()
  if (!is.list(values_list)) stop("values_list must be a list.")

  processed_string <- template_string
  if (length(values_list) > 0 || grepl("\\{\\{[^\\}]+\\}\\}", processed_string)) {
    # Find all unique placeholders in the template
    placeholders_found <- gregexpr("\\{\\{([^\\}]+)\\}\\}", processed_string, perl = TRUE)
    if (placeholders_found[[1]][1] != -1) {
      placeholder_matches <- regmatches(processed_string, placeholders_found)
      placeholder_keys <- unique(gsub("\\{\\{|\\}\\}", "", unlist(placeholder_matches)))

      for (key in placeholder_keys) {
        placeholder_regex <- paste0("\\{\\{", gsub("([.+?^${}()|\\[\\]\\\\])", "\\\\\\1", key, perl=TRUE), "\\}\\}") # Escape key for regex
        replacement_value <- if (key %in% names(values_list) && !is.null(values_list[[key]])) {
          as.character(values_list[[key]])
        } else {
          "" # Replace with empty string if key not in list or value is NULL
        }
        processed_string <- gsub(placeholder_regex, replacement_value, processed_string)
      }
    }
  }
  return(processed_string)
}

# LLM Response Parsers (simplified, assuming LLMR provides structured output)
# These might need adjustment based on how LLMR standardizes responses from different providers.
# For now, we assume LLMR::call_llm_robust returns the main text directly
# and token counts can be accessed if LLMR stores them as attributes or in a structured list.

# This is a placeholder; actual token extraction will depend on LLMR's output.
# For now, we'll assume LLMR::call_llm_robust might return an object
# where token info is in an attribute or a list element.
# If not, we'd need to parse the raw JSON response if available.
.extract_llm_usage <- function(llm_response_object) {
  # Ideal scenario: LLMR standardizes this.
  # Example: if (inherits(llm_response_object, "llmr_response_with_usage")) {
  #   return(list(
  #     prompt_tokens = llm_response_object$usage$prompt_tokens,
  #     completion_tokens = llm_response_object$usage$completion_tokens
  #   ))
  # }
  # Fallback: Try to find common patterns if raw response is available
  raw_resp <- attr(llm_response_object, "raw_response_details") # Hypothetical attribute
  if (!is.null(raw_resp) && is.list(raw_resp) && !is.null(raw_resp$usage)) {
    return(list(
      prompt_tokens = raw_resp$usage$prompt_tokens %||% raw_resp$usage$input_tokens %||% 0,
      completion_tokens = raw_resp$usage$completion_tokens %||% raw_resp$usage$output_tokens %||% 0
    ))
  }
  return(list(prompt_tokens = 0, completion_tokens = 0)) # Default if not found
} 