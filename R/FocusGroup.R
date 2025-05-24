# File: R/FocusGroup.R
# Purpose: Defines the main FocusGroup R6 class for simulation management.

#' @import R6
#' @import dplyr
#' @import tidyr
#' @importFrom LLMR call_llm_robust llm_config
#' @import quanteda
#' @import quanteda.textstats
#' @import topicmodels
#' @import tidytext
#' @import ggplot2
NULL

#' FocusGroup R6 Class
#'
#' @description
#' Main R6 class for managing and running a focus group simulation.
#' It orchestrates the simulation through distinct phases (Opening, Icebreaker,
#' Engagement, Exploration, Closing) guided by a question script, manages agent
#' interactions via a turn-taking flow, and provides methods for analysis and
#' visualization of the conversation.
#'
#' @field topic Character. The main topic of the focus group discussion.
#' @field purpose Character. The primary purpose or research objective of conducting the focus group.
#' @field agents Named list. A list of `FGAgent` objects participating in the simulation, indexed by their IDs.
#' @field moderator_id Character. The ID of the agent designated as the moderator.
#' @field conversation_log List. A chronological log of all messages. Each message is a list:
#'   `turn`, `speaker_id`, `is_moderator`, `text`, `timestamp`, `phase`.
#' @field turn_taking_flow A `ConversationFlow` object dictating participant turn-taking.
#' @field prompt_templates List. Holds prompt templates for agent/moderator actions.
#' @field question_script List. A structured list defining phases and specific questions/actions for the moderator.
#'   Each element is a list with `phase` (e.g., "opening", "icebreaker_question") and optionally `text` (for specific questions).
#' @field current_phase_index Integer. Tracks the current position in the `question_script`.
#' @field current_question_text Character. Text of the current question being discussed.
#' @field current_conversation_summary Character. An LLM-generated summary of earlier parts of the conversation,
#'   used for managing context length in prompts.
#' @field llm_config_admin An `llm_config` object for group-level LLM tasks (summarization, analysis).
#' @field max_tokens_utterance Integer. Default max tokens for participant utterances.
#' @field max_tokens_moderator Integer. Default max tokens for moderator utterances.
#' @field max_tokens_desire Integer. Default max tokens for desire-to-talk queries.
#' @field total_tokens_sent Numeric. Total tokens sent across all LLM calls in the group.
#' @field total_tokens_received Numeric. Total tokens received across all LLM calls.
#'
#' @export
FocusGroup <- R6::R6Class("FocusGroup",
  public = list(
    topic = NULL,
    purpose = NULL,
    agents = list(),
    moderator_id = NULL,
    conversation_log = list(),
    turn_taking_flow = NULL,
    prompt_templates = list(),
    question_script = list(),
    current_phase_index = 0,
    current_question_text = NULL,
    current_conversation_summary = NULL,
    llm_config_admin = NULL, # For summarization, LLM-based analysis
    max_tokens_utterance = 200,
    max_tokens_moderator = 150,
    max_tokens_desire = 20,
    total_tokens_sent = 0,
    total_tokens_received = 0,

    #' @description Initialize a new FocusGroup simulation.
    #' @param topic Character. The main discussion topic.
    #' @param purpose Character. The primary purpose of the focus group.
    #' @param agents Named list of initialized `FGAgent` objects.
    #' @param moderator_id Character. The ID of the agent acting as moderator.
    #' @param turn_taking_flow An initialized `ConversationFlow` object.
    #' @param question_script List. Moderator's script defining phases and questions.
    #'        If empty, a minimal default script (opening, generic discussion, closing) is used.
    #' @param prompt_templates List. Custom prompt templates. Defaults are used if not provided.
    #' @param llm_config_admin An `llm_config` object for administrative LLM tasks (e.g., summarization, LLM-based analysis).
    #'        If NULL, the moderator's `model_config` will be used for these tasks.
    #' @param max_tokens_config List. Optional. Named list with `utterance`, `moderator`, `desire`
    #'        to override default max token limits for these LLM call types.
    initialize = function(topic, purpose, agents, moderator_id, turn_taking_flow,
                          question_script = list(), prompt_templates = list(),
                          llm_config_admin = NULL, max_tokens_config = list()) {
      # Validations
      if (!is.character(topic) || length(topic) != 1 || nchar(topic) == 0) stop("'topic' must be a non-empty character string.")
      if (!is.character(purpose) || length(purpose) != 1 || nchar(purpose) == 0) stop("'purpose' must be a non-empty character string.")
      if (!is.list(agents) || length(agents) < 1 || !all(sapply(agents, inherits, "FGAgent"))) stop("'agents' must be a list of at least one FGAgent object.")
      if (!is.character(moderator_id) || !(moderator_id %in% names(agents))) stop("'moderator_id' must be a valid ID of an agent in 'agents'.")
      if (!inherits(turn_taking_flow, "ConversationFlow")) stop("'turn_taking_flow' must be a ConversationFlow object.")
      if (!is.list(question_script)) stop("'question_script' must be a list.")

      self$topic <- topic
      self$purpose <- purpose
      self$agents <- agents
      self$moderator_id <- moderator_id
      if(!is.null(self$agents[[moderator_id]])) self$agents[[moderator_id]]$is_moderator <- TRUE # Ensure flag is set
      self$turn_taking_flow <- turn_taking_flow
      self$conversation_log <- list()

      if (length(question_script) == 0) {
        message("No question_script provided. Using a minimal default script (opening, generic discussion, closing).")
        self$question_script <- list(
            list(phase = "opening"),
            list(phase = "generic_discussion", text = self$topic),
            list(phase = "closing")
        )
      } else {
        self$question_script <- question_script
      }
      self$current_phase_index <- 0 # Will be incremented before first use

      default_prompts_list <- get_default_prompt_templates()
      self$prompt_templates <- utils::modifyList(default_prompts_list, prompt_templates %||% list())

      self$llm_config_admin <- llm_config_admin %||% self$agents[[moderator_id]]$model_config

      self$max_tokens_utterance <- max_tokens_config$utterance %||% self$max_tokens_utterance
      self$max_tokens_moderator <- max_tokens_config$moderator %||% self$max_tokens_moderator
      self$max_tokens_desire <- max_tokens_config$desire %||% self$max_tokens_desire

      self$total_tokens_sent <- 0
      self$total_tokens_received <- 0

      # Initialize token tracking for all agents
      private$last_agent_tokens <- stats::setNames(
        lapply(names(self$agents), function(id) list(sent = 0, received = 0)),
        names(self$agents)
      )
    },

    #' @description Run the full focus group simulation.
    #' Iterates through the `question_script` phases or a specified number of turns.
    #' @param num_turns Integer. Optional. Maximum number of turns to run. If `NULL` (default),
    #'        the simulation runs until the `question_script` is exhausted or the moderator
    #'        decides to end. If both `num_turns` and `question_script` are provided,
    #'        the simulation stops at whichever condition is met first.
    #' @param verbose Logical. If `TRUE`, prints progress and utterances to the console.
    #' @return Invisibly returns the `conversation_log`.
    run_simulation = function(num_turns = NULL, verbose = FALSE) {
      if (verbose) {
        cat("Starting focus group simulation...\n")
        cat("Topic:", self$topic, "\n")
        cat("Purpose:", self$purpose, "\n")
      }

      self$current_phase_index <- 0 # Reset for fresh run
      turn_counter <- 0
      simulation_active <- TRUE

      while(simulation_active) {
        turn_counter <- turn_counter + 1
        if (!is.null(num_turns) && turn_counter > num_turns) {
            if (verbose) cat("\n--- Reached max_turns limit (", num_turns, "). Simulation Ended ---\n")
            break
        }

        # advance_turn returns FALSE if script ends or moderator closes
        simulation_active <- self$advance_turn(current_turn_number = turn_counter, verbose = verbose)

        # Interim summary logic (example: every 5 participant turns or N total words)
        # This can be made more sophisticated
        if (turn_counter %% 10 == 0 && length(self$conversation_log) > 5) { # Arbitrary condition
            if (verbose) cat("\n--- Generating interim summary for context management ---\n")
            current_transcript_for_summary <- private$get_recent_transcript_for_summary(20) # last 20 utterances

            # Use a less detailed summary for context window
            interim_summary_text <- self$summarize(llm_config = self$llm_config_admin, summary_level = 3, max_tokens = 150, internal_call = TRUE, transcript_override = current_transcript_for_summary)
            self$current_conversation_summary <- interim_summary_text
            if (verbose && !is.null(interim_summary_text)) cat("Interim summary generated.\n")
        }
      }
      if (verbose && simulation_active) cat("\n--- Simulation Concluded (script ended or moderator closed) ---\n")

      # Final summary of the entire discussion
      if (verbose) cat("\n--- Generating final summary of the entire discussion ---\n")
      final_summary <- self$summarize(llm_config = self$llm_config_admin, summary_level = 1, internal_call = FALSE) # Level 1 for overall summary
      private$log_message("System", paste("Final Summary:\n", final_summary), turn_counter + 1, is_moderator = FALSE, phase = "final_summary")
      if (verbose) cat("Final Summary:\n", final_summary, "\n")

      invisible(self$conversation_log)
    },

    #' @description Advance the simulation by one logical step (moderator action + participant response if applicable).
    #' @param current_turn_number Integer. The current turn number for logging.
    #' @param verbose Logical. If `TRUE`, print progress.
    #' @return Logical. `TRUE` if the simulation should continue, `FALSE` if it should end.
    advance_turn = function(current_turn_number, verbose = FALSE) {
      moderator_agent <- self$agents[[self$moderator_id]]

      # 1. Determine Moderator's Action based on script/phase
      current_phase_details <- private$get_next_phase_or_question()
      if (is.null(current_phase_details)) {
        if (verbose) cat("Moderator script exhausted. Ending simulation.\n")
        return(FALSE) # Script ended
      }
      current_phase_name <- current_phase_details$phase
      self$current_question_text <- current_phase_details$text %||% self$topic # Fallback to topic if no specific question text

      if (verbose) {
        cat(paste0("\n--- Turn ", current_turn_number, " | Phase: ", current_phase_name))
        if (nzchar(self$current_question_text) && current_phase_name != "opening" && current_phase_name != "closing") {
            cat(paste0(" | Current Question/Focus: ", substr(self$current_question_text,1,100), "..."))
        }
        cat(" ---\n")
      }

      # 2. Moderator Speaks
      moderator_prompt_key <- paste0("moderator_", current_phase_name)
      if (!moderator_prompt_key %in% names(self$prompt_templates)) {
          # Fallback to a generic moderator prompt if phase-specific one is missing
          warning(paste("Prompt template for phase '", current_phase_name, "' (key: '", moderator_prompt_key, "') not found. Using generic moderator prompt.", sep=""))
          moderator_prompt_key <- "moderator_generic_utterance"
      }
      moderator_template <- self$prompt_templates[[moderator_prompt_key]]

      # Determine max tokens for this specific moderator action
      mod_tokens <- self$max_tokens_moderator
      if (current_phase_name == "opening") mod_tokens <- 300
      if (current_phase_name == "closing") mod_tokens <- 250
      if (current_phase_name == "summarizing") mod_tokens <- 200

      # Fill placeholders specific to moderator prompts
      # Some placeholders might be specific to certain moderator prompts (e.g., {{next_focus_group_question}})
      # These should be dynamically determined or passed if needed.
      # For now, `generate_utterance` will use what's available in its `prompt_values`.

      # Construct conversation history for the moderator
      history_for_moderator <- format_conversation_history(
        self$conversation_log,
        n_recent = 7, # Moderator might need slightly more context
        include_summary = self$current_conversation_summary
      )

      moderator_utterance <- moderator_agent$generate_utterance(
        topic = self$topic,
        conversation_history_string = history_for_moderator,
        utterance_prompt_template = moderator_template,
        max_tokens_utterance = mod_tokens,
        current_moderator_question = self$current_question_text, # This is the question *being asked* or *related to*
        conversation_summary_so_far = self$current_conversation_summary,
        current_phase = current_phase_name
      )
      private$log_message(self$moderator_id, moderator_utterance, current_turn_number, is_moderator = TRUE, phase = current_phase_name)
      if (verbose) cat(paste0(self$moderator_id, " (Moderator): ", moderator_utterance, "\n"))

      # Update total tokens
      self$total_tokens_sent <- self$total_tokens_sent + moderator_agent$tokens_sent_agent - private$last_agent_tokens[[self$moderator_id]]$sent
      self$total_tokens_received <- self$total_tokens_received + moderator_agent$tokens_received_agent - private$last_agent_tokens[[self$moderator_id]]$received
      private$last_agent_tokens[[self$moderator_id]] <- list(sent = moderator_agent$tokens_sent_agent, received = moderator_agent$tokens_received_agent)


      # If moderator's action was closing, end simulation
      if (current_phase_name == "closing") {
        if (verbose) cat("Moderator initiated closing. Ending simulation.\n")
        return(FALSE)
      }

      # 3. Participant(s) Respond (if applicable for the phase)
      # Typically, after moderator asks a question or makes a point requiring response.
      # Phases like "opening", "summarizing", "transition", "closing" might not immediately solicit a participant response via flow.
      phases_expecting_participant_response <- c("icebreaker_question", "engagement_question", "exploration_question", "probing_focused", "ending_question", "generic_discussion")

      if (current_phase_name %in% phases_expecting_participant_response) {
        # For now, let one participant respond per moderator turn that expects response.
        # More complex logic could allow multiple participants or a "free discussion" period.
        next_participant <- self$turn_taking_flow$select_next_speaker(self) # Flow should select a PARTICIPANT

        if (!is.null(next_participant)) {
          if (next_participant$id == self$moderator_id && length(self$agents) > 1) {
             if (verbose) cat("Turn-taking flow selected moderator during participant response phase. Moderator may need to re-prompt or manage flow.\n")
          } else {
            if (verbose) cat(paste("Selected participant by flow:", next_participant$id, "\n"))

            history_for_participant <- format_conversation_history(
              self$conversation_log, # Now includes moderator's latest utterance
              n_recent = 5,
              include_summary = self$current_conversation_summary
            )

            participant_utterance <- next_participant$generate_utterance(
              topic = self$topic,
              conversation_history_string = history_for_participant,
              utterance_prompt_template = self$prompt_templates$participant_utterance_subtle_persona,
              max_tokens_utterance = self$max_tokens_utterance,
              current_moderator_question = self$current_question_text, # The question they are responding to
              conversation_summary_so_far = self$current_conversation_summary,
              current_phase = current_phase_name
            )
            private$log_message(next_participant$id, participant_utterance, current_turn_number, is_moderator = FALSE, phase = current_phase_name)
            if (verbose) cat(paste0(next_participant$id, ": ", participant_utterance, "\n"))

            self$turn_taking_flow$update_state_post_selection(next_participant$id, self)

            # Update total tokens
            self$total_tokens_sent <- self$total_tokens_sent + next_participant$tokens_sent_agent - private$last_agent_tokens[[next_participant$id]]$sent
            self$total_tokens_received <- self$total_tokens_received + next_participant$tokens_received_agent - private$last_agent_tokens[[next_participant$id]]$received
            private$last_agent_tokens[[next_participant$id]] <- list(sent = next_participant$tokens_sent_agent, received = next_participant$tokens_received_agent)

          }
        } else {
          if (verbose) cat("No participant selected by flow (e.g., low desire). Moderator might need to prompt again or manage participation.\n")
          # Moderator could have a "manage_participation" phase/action here.
        }
      }

      utils::flush.console()
      return(TRUE) # Continue simulation
    },

    #' @description Generate a summary of the conversation using an LLM.
    #' @param llm_config An `llm_config` object for the summarization LLM. If `NULL`, uses `self$llm_config_admin`.
    #' @param summary_level Integer (1-3). 1: Prose overview, 2: Detailed bulleted, 3: Short bulleted takeaways.
    #' @param max_tokens Integer. Optional. Max tokens for the summary.
    #' @param internal_call Logical. If TRUE, this is an internal call (e.g. for context window management) and token counts are not added to the agent who "owns" llm_config_admin.
    #' @param transcript_override Character. Optional. If provided, this transcript is summarized instead of `self$conversation_log`.
    #' @return Character string containing the generated summary.
    summarize = function(llm_config = NULL, summary_level = 1, max_tokens = NULL, internal_call = FALSE, transcript_override = NULL) {
      if (!requireNamespace("LLMR", quietly = TRUE)) stop("LLMR package is required.")

      active_llm_config <- llm_config %||% self$llm_config_admin
      if (is.null(active_llm_config)) stop("An llm_config must be provided or set as llm_config_admin for summarization.")
      if (!inherits(active_llm_config, "llm_config")) stop("'active_llm_config' must be a valid 'llm_config' object.")
      if (!summary_level %in% 1:3) stop("'summary_level' must be 1, 2, or 3.")

      transcript_to_use <- transcript_override
      if (is.null(transcript_to_use)) {
          if (length(self$conversation_log) == 0) {
            warning("Conversation log is empty. Returning an empty summary.")
            return("The conversation log is empty. No summary can be generated.")
          }
          transcript_to_use <- paste(sapply(self$conversation_log, function(msg) {
            paste0(msg$speaker_id, " (Turn ", msg$turn, ", Phase: ", msg$phase, "): ", msg$text)
          }), collapse = "\n\n")
      }
      if (nchar(trimws(transcript_to_use)) == 0) {
          warning("Transcript for summary is empty. Returning empty summary.")
          return("Transcript is empty.")
      }


      topic_context <- paste0("The main topic of the discussion was: ", self$topic, "\n",
                              "The purpose of the focus group was: ", self$purpose, "\n\n")

      level_instructions <- ""
      estimated_tokens <- NULL # For LLM call

      if (summary_level == 1) {
        level_instructions <- "Provide a concise prose summary (approx. 200-300 words) of the focus group discussion. Focus on main themes, overall sentiment, key agreements/disagreements, and broad conclusions."
        estimated_tokens <- max_tokens %||% 400
      } else if (summary_level == 2) {
        level_instructions <- "Analyze the focus group transcript and provide a detailed, bulleted summary. Highlight key quotes (attributed), anecdotes, new arguments, surprising statements, and points of agreement/disagreement. Organize logically."
        estimated_tokens <- max_tokens %||% 1000
      } else if (summary_level == 3) { # Short, for context window or quick overview
        level_instructions <- "Provide a very short, bulleted overall summary (approx. 3-5 key bullet points) of the most critical takeaways. Focus on impactful points, arguments, and surprising insights. Keep it extremely concise."
        estimated_tokens <- max_tokens %||% 150
      }

      full_prompt <- paste0(
        topic_context,
        level_instructions,
        "\n\nHere is the transcript (or relevant portion):\n--------------------\n",
        transcript_to_use,
        "\n--------------------\nSummary:"
      )

      current_call_config <- active_llm_config
      if (!is.null(estimated_tokens)) {
        current_call_config$model_params$max_tokens <- estimated_tokens
      }

      response_obj <- LLMR::call_llm_robust(
        config = current_call_config,
        messages = list(list(role = "user", content = full_prompt)),
        tries = 5
      )
      summary_text <- as.character(response_obj)

      if (!internal_call) { # Only add to group totals if it's an "external" summarization request
          usage <- .extract_llm_usage(response_obj)
          self$total_tokens_sent <- self$total_tokens_sent + (usage$prompt_tokens %||% 0)
          self$total_tokens_received <- self$total_tokens_received + (usage$completion_tokens %||% 0)
      }
      return(summary_text)
    },

    # --- Analysis Methods ---
    #' @description Basic analysis of the conversation log.
    #' @param turns Integer vector. Optional. Specific turns to analyze. If `NULL`, analyzes all turns.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze. If `NULL`, analyzes all speakers.
    #' @return A list with `speaker_stats` (a tibble: speaker_id, utterance_count, total_words, avg_words_per_utterance)
    #'         and `full_transcript` (character string).
    analyze = function(turns = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(turns, speaker_ids)

      if (length(filtered_log) == 0) {
        warning("Filtered conversation log is empty for basic analysis.")
        # Return empty structure consistent with non-empty case
        all_agent_ids <- names(self$agents)
        speaker_stats_df <- dplyr::tibble(
          speaker_id = all_agent_ids,
          utterance_count = 0L,
          total_words = 0L,
          avg_words_per_utterance = 0.0
        )
        return(list(speaker_stats = speaker_stats_df, full_transcript = "Filtered conversation log is empty."))
      }

      log_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
        dplyr::tibble(
          speaker_id = x$speaker_id %||% NA_character_,
          text = x$text %||% ""
        )
      }))

      speaker_stats_df <- log_df %>%
        dplyr::mutate(word_count = sapply(strsplit(as.character(.data$text), "\\s+"), length)) %>%
        dplyr::group_by(.data$speaker_id) %>%
        dplyr::summarise(
          utterance_count = dplyr::n(),
          total_words = sum(.data$word_count, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(avg_words_per_utterance = ifelse(.data$utterance_count > 0, .data$total_words / .data$utterance_count, 0.0))

      # Ensure all agents are in the stats, even if they didn't speak in the filtered log
      all_agent_ids <- names(self$agents)
      current_speakers_in_stats <- unique(speaker_stats_df$speaker_id)
      missing_agents <- setdiff(all_agent_ids, current_speakers_in_stats)

      if (length(missing_agents) > 0) {
        missing_df <- dplyr::tibble(
          speaker_id = missing_agents, utterance_count = 0L, total_words = 0L, avg_words_per_utterance = 0.0
        )
        speaker_stats_df <- dplyr::bind_rows(speaker_stats_df, missing_df)
      }

      # Order by original agent order if possible, or alphabetically
      ordered_agent_ids <- intersect(all_agent_ids, speaker_stats_df$speaker_id)
      if(length(ordered_agent_ids) > 0) {
          speaker_stats_df <- speaker_stats_df %>%
            dplyr::mutate(speaker_id = factor(.data$speaker_id, levels = all_agent_ids)) %>%
            dplyr::arrange(.data$speaker_id)
      }


      full_transcript_text <- paste(sapply(filtered_log, function(msg) {
        paste0(msg$speaker_id, " (Turn ", msg$turn, ", Phase: ", msg$phase, "): ", msg$text)
      }), collapse = "\n\n")

      return(list(speaker_stats = speaker_stats_df, full_transcript = full_transcript_text))
    },

    #' @description Perform LDA topic modeling on the conversation.
    #' @param num_topics Integer. Number of topics to identify.
    #' @param min_doc_length Integer. Min words for a speaker's aggregated text to be a document.
    #' @param top_n_terms Integer. Number of top terms per topic to return.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @param ... Additional arguments to `topicmodels::LDA()`.
    #' @return A list with LDA model, topic terms, and document-topic proportions. `NULL` on failure.
    analyze_topics = function(num_topics = 5, min_doc_length = 20, top_n_terms = 10, turns = NULL, speaker_ids = NULL, ...) {
      if (!all(sapply(c("topicmodels", "tidytext", "dplyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages topicmodels, tidytext, and dplyr are required for LDA. Please install them.")
      }

      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
          warning("Filtered log is empty for topic analysis.")
          return(NULL)
      }

      tryCatch({
        # Extract texts and speaker IDs
        texts <- sapply(filtered_log, function(x) x$text)
        speaker_ids_vec <- sapply(filtered_log, function(x) x$speaker_id)

        # Aggregate by speaker to create documents
        speaker_texts <- tapply(texts, speaker_ids_vec, paste, collapse = " ")

        # Filter documents by minimum length
        word_counts <- sapply(speaker_texts, function(text) length(strsplit(text, "\\s+")[[1]]))
        valid_docs <- names(speaker_texts)[word_counts >= min_doc_length]

        if (length(valid_docs) < 2) {
          warning("Not enough valid documents for topic modeling (minimum 2 required)")
          return(NULL)
        }

        if (length(valid_docs) < num_topics) {
          warning("Number of valid documents is less than num_topics. Reducing num_topics.")
          num_topics <- length(valid_docs)
        }

        # Create document-term matrix using tidytext
        valid_texts <- speaker_texts[valid_docs]

        # Convert to tidy format and create DTM
        tidy_docs <- dplyr::tibble(
          document = names(valid_texts),
          text = valid_texts
        ) %>%
          tidytext::unnest_tokens(.data$word, .data$text) %>%
          dplyr::anti_join(tidytext::stop_words, by = "word") %>%
          dplyr::filter(nchar(.data$word) > 2) %>%
          dplyr::count(.data$document, .data$word, sort = TRUE)

        # Cast to DTM
        dtm <- tidy_docs %>%
          tidytext::cast_dtm(.data$document, .data$word, .data$n)

        # Check if DTM has sufficient terms
        if (ncol(dtm) < 5) {
          warning("Not enough unique terms for meaningful topic modeling")
          return(NULL)
        }

        # Run LDA
        lda_model <- topicmodels::LDA(dtm, k = num_topics, control = list(seed = 1234), ...)

        # Extract topic-term probabilities (beta)
        topics_beta <- tidytext::tidy(lda_model, matrix = "beta")

        # Get top terms for each topic
        top_terms <- topics_beta %>%
          dplyr::group_by(.data$topic) %>%
          dplyr::slice_max(.data$beta, n = top_n_terms, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(.data$topic, dplyr::desc(.data$beta))

        # Extract document-topic probabilities (gamma)
        topics_gamma <- tidytext::tidy(lda_model, matrix = "gamma")

        # Create topic labels based on top terms
        topic_labels <- top_terms %>%
          dplyr::group_by(.data$topic) %>%
          dplyr::slice_head(n = 3) %>%
          dplyr::summarise(label = paste(.data$term, collapse = ", "), .groups = 'drop')

        return(list(
          lda_model = lda_model,
          top_terms = top_terms,
          document_topics = topics_gamma,
          topic_labels = topic_labels,
          dtm = dtm,
          num_documents = length(valid_docs),
          num_topics = num_topics
        ))

      }, error = function(e) {
        warning(paste("Topic analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Calculate TF-IDF scores for terms per participant.
    #' @param top_n_terms Integer. Number of top TF-IDF terms per participant.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @param ... Additional arguments to `tidytext::unnest_tokens`.
    #' @return A tibble with TF-IDF scores.
    analyze_tfidf = function(top_n_terms = 10, turns = NULL, speaker_ids = NULL, ...) {
      if (!all(sapply(c("tidytext", "dplyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages tidytext and dplyr are required for TF-IDF. Please install them.")
      }
      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
          warning("Filtered log is empty for TF-IDF analysis.")
          return(dplyr::tibble())
      }

      tryCatch({
        # Extract texts and speaker IDs
        texts <- sapply(filtered_log, function(x) x$text)
        speaker_ids_vec <- sapply(filtered_log, function(x) x$speaker_id)

        # Create a data frame for analysis
        text_df <- dplyr::tibble(
          speaker_id = speaker_ids_vec,
          text = texts
        )

        # Aggregate by speaker to create documents
        speaker_docs <- text_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(document = paste(.data$text, collapse = " "), .groups = 'drop')

        # Tokenize and calculate TF-IDF using tidytext approach
        tfidf_results <- speaker_docs %>%
          tidytext::unnest_tokens(.data$word, .data$document, ...) %>%
          dplyr::anti_join(tidytext::stop_words, by = "word") %>%
          dplyr::filter(nchar(.data$word) > 2) %>%
          dplyr::count(.data$speaker_id, .data$word, sort = TRUE) %>%
          tidytext::bind_tf_idf(.data$word, .data$speaker_id, .data$n) %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::slice_max(.data$tf_idf, n = top_n_terms, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::select(.data$speaker_id, term = .data$word, .data$tf_idf) %>%
          dplyr::arrange(.data$speaker_id, dplyr::desc(.data$tf_idf))

        return(tfidf_results)
      }, error = function(e) {
        warning(paste("TF-IDF analysis failed:", e$message))
        return(dplyr::tibble())
      })
    },

    #' @description Calculate readability scores for each participant's aggregated text.
    #' @param measures Character vector. Readability measure(s) from `quanteda.textstats::textstat_readability`.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A tibble with readability scores.
    analyze_readability = function(measures = "Flesch", turns = NULL, speaker_ids = NULL) {
      if (!all(sapply(c("quanteda", "quanteda.textstats", "dplyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages quanteda, quanteda.textstats, dplyr are required for readability. Please install them.")
      }
      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
          warning("Filtered log is empty for readability analysis.")
          return(dplyr::tibble())
      }

      tryCatch({
        # Extract texts and speaker IDs
        texts <- sapply(filtered_log, function(x) x$text)
        speaker_ids_vec <- sapply(filtered_log, function(x) x$speaker_id)

        # Aggregate by speaker
        speaker_texts <- tapply(texts, speaker_ids_vec, paste, collapse = " ")

        # Filter out speakers with very short texts (less than 10 words)
        speaker_word_counts <- sapply(speaker_texts, function(text) length(strsplit(text, "\\s+")[[1]]))
        valid_speakers <- names(speaker_texts)[speaker_word_counts >= 10]

        if (length(valid_speakers) == 0) {
          warning("No speakers have sufficient text for readability analysis (minimum 10 words required)")
          return(dplyr::tibble())
        }

        # Create corpus for valid speakers only
        valid_texts <- speaker_texts[valid_speakers]
        corpus <- quanteda::corpus(valid_texts)
        quanteda::docnames(corpus) <- valid_speakers

        # Calculate readability measures
        readability_stats <- quanteda.textstats::textstat_readability(corpus, measure = measures)

        # Convert to tibble and add speaker_id column
        result <- dplyr::as_tibble(readability_stats)
        result$speaker_id <- rownames(readability_stats)

        # Reorder columns to put speaker_id first
        result <- result %>%
          dplyr::select(.data$speaker_id, dplyr::everything()) %>%
          dplyr::arrange(.data$speaker_id)

        return(result)
      }, error = function(e) {
        warning(paste("Readability analysis failed:", e$message))
        return(dplyr::tibble())
      })
    },

    #' @description Perform LLM-assisted thematic analysis on the transcript.
    #' @param llm_config An `llm_config` object for the analysis. If `NULL`, uses `self$llm_config_admin`.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with `themes_summary` (character string from LLM) and `raw_llm_response`.
    analyze_themes = function(llm_config = NULL, turns = NULL, speaker_ids = NULL) {
      active_llm_config <- llm_config %||% self$llm_config_admin
      if (is.null(active_llm_config)) stop("An llm_config is required for thematic analysis.")

      analysis_data <- self$analyze(turns = turns, speaker_ids = speaker_ids) # Gets full_transcript
      if (nchar(trimws(analysis_data$full_transcript)) == 0 || analysis_data$full_transcript == "Filtered conversation log is empty.") {
          warning("Transcript for thematic analysis is empty. Returning NULL.")
          return(NULL)
      }

      # Simple thematic analysis using LLM
      result <- tryCatch({
        theme_prompt <- replace_placeholders(
          self$prompt_templates$thematic_analysis_prompt,
          list(
            topic = self$topic,
            focus_group_purpose = self$purpose,
            full_transcript = analysis_data$full_transcript
          )
        )

        current_call_config <- active_llm_config
        current_call_config$model_params$max_tokens <- 800

        response_obj <- LLMR::call_llm_robust(
          config = current_call_config,
          messages = list(list(role = "user", content = theme_prompt)),
          tries = 5
        )

        # list(
        #   themes_summary = as.character(response_obj),
        #   raw_llm_response = response_obj
        # )
        themes_summary = as.character(response_obj)
        attr(themes_summary , 'raw_llm_response') <- response_obj
      }, error = function(e) NULL)
      # Update total tokens
      # This needs .analyze_themes_impl to return token usage.
      # For now, assuming it's handled if it were to be added.
      return(result)
    },

    #' @description Perform sentiment analysis per utterance using tidytext.
    #' @param sentiment_lexicon Character. Sentiment lexicon to use: "afinn", "bing", or "nrc".
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A tibble with `turn`, `speaker_id`, `text`, `sentiment`, `sentiment_score`.
    analyze_sentiment = function(sentiment_lexicon = "afinn", turns = NULL, speaker_ids = NULL) {
      if (!all(sapply(c("tidytext", "dplyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages tidytext and dplyr are required for sentiment analysis.")
      }

      valid_lexicons <- c("afinn", "bing", "nrc")
      if (!sentiment_lexicon %in% valid_lexicons) {
        stop("sentiment_lexicon must be one of: ", paste(valid_lexicons, collapse = ", "))
      }

      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered conversation log is empty for sentiment analysis.")
        return(dplyr::tibble())
      }

      tryCatch({
        # Extract data from filtered log
        results <- list()

        for (i in seq_along(filtered_log)) {
          msg <- filtered_log[[i]]

          # Tokenize the text
          text_tokens <- dplyr::tibble(text = msg$text) %>%
            tidytext::unnest_tokens(.data$word, .data$text) %>%
            dplyr::anti_join(tidytext::stop_words, by = "word")

          # Get sentiment scores based on lexicon
          if (sentiment_lexicon == "afinn") {
            sentiment_scores <- text_tokens %>%
              dplyr::inner_join(tidytext::get_sentiments("afinn"), by = "word") %>%
              dplyr::pull(.data$value)

            if (length(sentiment_scores) > 0) {
              avg_score <- mean(sentiment_scores)
              sentiment_label <- dplyr::case_when(
                avg_score > 0.5 ~ "Positive",
                avg_score < -0.5 ~ "Negative",
                TRUE ~ "Neutral"
              )
            } else {
              avg_score <- 0
              sentiment_label <- "Neutral"
            }

          } else if (sentiment_lexicon == "bing") {
            sentiment_counts <- text_tokens %>%
              dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
              dplyr::count(.data$sentiment, sort = TRUE)

            if (nrow(sentiment_counts) > 0) {
              pos_count <- sentiment_counts$n[sentiment_counts$sentiment == "positive"] %||% 0
              neg_count <- sentiment_counts$n[sentiment_counts$sentiment == "negative"] %||% 0
              avg_score <- pos_count - neg_count

              sentiment_label <- dplyr::case_when(
                pos_count > neg_count ~ "Positive",
                neg_count > pos_count ~ "Negative",
                TRUE ~ "Neutral"
              )
            } else {
              avg_score <- 0
              sentiment_label <- "Neutral"
            }

          } else if (sentiment_lexicon == "nrc") {
            emotions <- text_tokens %>%
              dplyr::inner_join(tidytext::get_sentiments("nrc"), by = "word") %>%
              dplyr::count(.data$sentiment, sort = TRUE)

            if (nrow(emotions) > 0) {
              top_emotion <- emotions$sentiment[1]
              sentiment_label <- dplyr::case_when(
                top_emotion %in% c("positive", "joy", "trust", "anticipation") ~ "Positive",
                top_emotion %in% c("negative", "sadness", "anger", "fear", "disgust") ~ "Negative",
                TRUE ~ "Neutral"
              )
              avg_score <- emotions$n[1]
            } else {
              sentiment_label <- "Neutral"
              avg_score <- 0
            }
          }

          results[[i]] <- list(
            turn = msg$turn,
            speaker_id = msg$speaker_id,
            text = msg$text,
            sentiment = sentiment_label,
            sentiment_score = avg_score
          )
        }

        dplyr::bind_rows(results)
      }, error = function(e) {
        warning(paste("Sentiment analysis failed:", e$message))
        return(dplyr::tibble())
      })
    },

    #' @description Perform statistical analysis on conversation patterns.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with ANOVA results, phase participation stats, and correlations.
    analyze_statistics = function(turns = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for statistical analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format for analysis
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            phase = x$phase,
            text = x$text,
            turn = x$turn
          )
        }))

        # Calculate basic statistics by speaker and phase
        stats <- conv_df %>%
          dplyr::group_by(.data$speaker_id, .data$phase) %>%
          dplyr::summarise(
            turns = dplyr::n(),
            words = sum(sapply(.data$text, function(x) length(strsplit(x, "\\s+")[[1]]))),
            avg_words = .data$words / .data$turns,
            .groups = "drop"
          )

        # Perform ANOVA on word count by phase if multiple phases exist
        word_aov <- if(length(unique(stats$phase)) > 1) {
          stats::aov(words ~ phase, data = stats)
        } else NULL

        # Calculate phase participation statistics
        phase_stats <- stats %>%
          dplyr::group_by(.data$phase) %>%
          dplyr::summarise(
            mean_turns = mean(.data$turns),
            sd_turns = stats::sd(.data$turns),
            .groups = "drop"
          )

        # Calculate correlation between turns and words
        cor_test <- if(nrow(stats) > 2) {
          stats::cor.test(stats$turns, stats$words)
        } else NULL

        list(
          word_count_anova = if(!is.null(word_aov)) summary(word_aov) else "Not enough phases for ANOVA",
          phase_participation = phase_stats,
          turns_words_correlation = cor_test
        )
      }, error = function(e) {
        warning(paste("Statistical analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Analyze participation balance and dominance patterns.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with participation statistics and balance metrics.
    analyze_participation_balance = function(turns = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for participation balance analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            text = x$text
          )
        }))

        # Calculate participation metrics
        participation <- conv_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(
            turns = dplyr::n(),
            words = sum(sapply(.data$text, function(x) length(strsplit(x, "\\s+")[[1]]))),
            avg_words = .data$words / .data$turns,
            .groups = "drop"
          )

        # Calculate participation percentages
        total_turns <- sum(participation$turns)
        total_words <- sum(participation$words)

        participation <- participation %>%
          dplyr::mutate(
            turn_percentage = (.data$turns / total_turns) * 100,
            word_percentage = (.data$words / total_words) * 100
          )

        # Calculate dominance metrics
        max_turn_pct <- max(participation$turn_percentage)
        min_turn_pct <- min(participation$turn_percentage)
        turn_range <- max_turn_pct - min_turn_pct

        list(
          participation_stats = participation,
          balance_metrics = list(
            most_active_speaker = participation$speaker_id[which.max(participation$turn_percentage)],
            least_active_speaker = participation$speaker_id[which.min(participation$turn_percentage)],
            turn_percentage_range = turn_range,
            is_balanced = turn_range < 30  # Arbitrary threshold for "balanced" participation
          )
        )
      }, error = function(e) {
        warning(paste("Participation balance analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Analyze response patterns and interaction behaviors.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with response and interaction pattern metrics.
    analyze_response_patterns = function(turns = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for response pattern analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            text = x$text,
            turn = x$turn
          )
        }))

        # Calculate response patterns by speaker
        response_patterns <- conv_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(
            total_responses = dplyr::n(),
            avg_words = mean(sapply(.data$text, function(x) length(strsplit(x, "\\s+")[[1]]))),
            question_count = sum(grepl("\\?", .data$text)),
            exclamation_count = sum(grepl("!", .data$text)),
            .groups = "drop"
          )

        # Calculate interaction patterns
        min_turn <- min(conv_df$turn)
        max_turn <- max(conv_df$turn)

        interaction_patterns <- conv_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(
            first_to_respond = sum(.data$turn == min_turn),
            last_to_respond = sum(.data$turn == max_turn),
            .groups = "drop"
          )

        list(
          response_metrics = response_patterns,
          interaction_metrics = interaction_patterns
        )
      }, error = function(e) {
        warning(paste("Response pattern analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Analyze question asking patterns during the conversation.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with question pattern analysis.
    analyze_question_patterns = function(turns = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for question pattern analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format and filter questions
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            phase = x$phase,
            text = x$text
          )
        }))

        # Extract questions and analyze patterns
        questions <- conv_df %>%
          dplyr::filter(grepl("\\?", .data$text)) %>%
          dplyr::group_by(.data$speaker_id, .data$phase) %>%
          dplyr::summarise(
            question_count = dplyr::n(),
            avg_words = mean(sapply(.data$text, function(x) length(strsplit(x, "\\s+")[[1]]))),
            .groups = "drop"
          )

        # Analyze question distribution by phase
        question_dist <- questions %>%
          dplyr::group_by(.data$phase) %>%
          dplyr::summarise(
            total_questions = sum(.data$question_count),
            unique_questioners = dplyr::n_distinct(.data$speaker_id),
            .groups = "drop"
          )

        list(
          question_patterns = questions,
          question_distribution = question_dist
        )
      }, error = function(e) {
        warning(paste("Question pattern analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Extract and analyze key phrases using n-grams.
    #' @param min_freq Integer. Minimum frequency for phrases to be considered key.
    #' @param turns Integer vector. Optional. Specific turns to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with bigram and trigram analysis.
    analyze_key_phrases = function(min_freq = 2, turns = NULL, speaker_ids = NULL) {
      # Ensure tidytext and dplyr are available
      if (!all(sapply(c("tidytext", "dplyr", "tidyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages tidytext, dplyr, and tidyr are required for key phrase analysis.")
      }

      filtered_log <- private$get_filtered_log(turns, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for key phrase analysis.")
        return(list(bigrams = dplyr::tibble(), trigrams = dplyr::tibble(),
                    total_unique_bigrams = 0, total_unique_trigrams = 0))
      }

      tryCatch({
        # Create a tibble from the conversation log
        conv_tibble <- dplyr::tibble(
          doc_id = seq_along(filtered_log), # Each message as a document for n-gram extraction
          text = sapply(filtered_log, function(x) x$text %||% "")
        )

        # --- Bigram Analysis ---
        bigrams_tokenized <- conv_tibble %>%
          tidytext::unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) %>%
          dplyr::filter(!is.na(bigram)) # Remove NA bigrams that might result from very short texts

        # Optional: Remove bigrams containing stop words
        # This is a common step to make n-grams more meaningful
        # We separate the bigram, check each word, then filter
        bigrams_separated <- bigrams_tokenized %>%
          tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE, fill = "right")

        # Anti-join against stop words for each part of the bigram
        # A bigram is removed if EITHER word1 OR word2 is a stop word.
        # You might adjust this logic (e.g., remove only if both are stop words, or if first/last are)
        data("stop_words", package = "tidytext") # Ensure stop_words is loaded

        bigrams_cleaned <- bigrams_separated %>%
          dplyr::anti_join(stop_words, by = c("word1" = "word")) %>%
          dplyr::anti_join(stop_words, by = c("word2" = "word")) %>%
          dplyr::select(doc_id, bigram) # Keep the original bigram column

        total_unique_bigrams <- dplyr::n_distinct(bigrams_cleaned$bigram)

        frequent_bigrams_df <- bigrams_cleaned %>%
          dplyr::count(bigram, sort = TRUE, name = "frequency") %>%
          dplyr::filter(frequency >= min_freq) %>%
          dplyr::rename(feature = bigram) # Match quanteda's output column name

        # --- Trigram Analysis ---
        trigrams_tokenized <- conv_tibble %>%
          tidytext::unnest_tokens(output = trigram, input = text, token = "ngrams", n = 3) %>%
          dplyr::filter(!is.na(trigram))

        trigrams_separated <- trigrams_tokenized %>%
          tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ", remove = FALSE, fill = "right")

        trigrams_cleaned <- trigrams_separated %>%
          dplyr::anti_join(stop_words, by = c("word1" = "word")) %>%
          dplyr::anti_join(stop_words, by = c("word2" = "word")) %>%
          dplyr::anti_join(stop_words, by = c("word3" = "word")) %>% # Check all three words
          dplyr::select(doc_id, trigram)

        total_unique_trigrams <- dplyr::n_distinct(trigrams_cleaned$trigram)

        frequent_trigrams_df <- trigrams_cleaned %>%
          dplyr::count(trigram, sort = TRUE, name = "frequency") %>%
          dplyr::filter(frequency >= min_freq) %>%
          dplyr::rename(feature = trigram) # Match quanteda's output column name

        list(
          bigrams = frequent_bigrams_df,
          trigrams = frequent_trigrams_df,
          total_unique_bigrams_after_stopwords = total_unique_bigrams,
          total_unique_trigrams_after_stopwords = total_unique_trigrams
        )
      }, error = function(e) {
        warning(paste("Key phrase analysis with tidytext failed:", e$message))
        return(list(bigrams = dplyr::tibble(), trigrams = dplyr::tibble(),
                    total_unique_bigrams_after_stopwords = 0, total_unique_trigrams_after_stopwords = 0))
      })
    },



    #' @description Create participation timeline plot showing cumulative turns by participant across phases.
    #' @return ggplot object
    plot_participation_timeline = function() {

      if (length(self$conversation_log) == 0) {
        warning("No conversation data available for plotting")
        # Return an empty plot with a message
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Participation Timeline",
                               subtitle = "No conversation data available") +
                 ggplot2::theme_void()) # Using theme_void for a completely empty plot
      }

      # Create a data frame from the conversation log
      # Ensure speaker_id and phase are handled if they could be NULL (though phase defaults to "unknown")
      # REMOVE 'System' participant
      # Should we remove the moderator as well?

      conv_df <- dplyr::bind_rows(lapply(self$conversation_log, function(x) {
        dplyr::tibble(
          agent_id = x$speaker_id %||% NA_character_,
          phase = x$phase %||% "unknown"
        )
      })) %>%
        # Filter out "System" messages as they are not typical participant turns
        dplyr::filter(.data$agent_id != "System")

      # Check if conv_df is empty *after* filtering "System"
      if (nrow(conv_df) == 0) {
        warning("Conversation data is empty or only contains 'System' messages after filtering. No plot generated.")
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Participation Timeline", subtitle = "No valid participant data for plotting") +
                 ggplot2::theme_void())
      }

      actual_phase_order <- unique(conv_df$phase)
      all_participants <- unique(conv_df$agent_id) # Derived from filtered data

      conv_summary <- conv_df %>%
        dplyr::group_by(.data$agent_id, .data$phase) %>%
        dplyr::summarise(turns_in_phase = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(phase = factor(.data$phase, levels = actual_phase_order)) %>%
        tidyr::complete(agent_id = all_participants,
                        phase,
                        fill = list(turns_in_phase = 0)) %>%
        dplyr::arrange(.data$agent_id, .data$phase) %>%
        dplyr::group_by(.data$agent_id) %>%
        dplyr::mutate(cumulative_turns = cumsum(.data$turns_in_phase)) %>%
        dplyr::ungroup()


      ggplot2::ggplot(conv_summary, ggplot2::aes(x = .data$phase, y = .data$cumulative_turns, color = .data$agent_id, group = .data$agent_id)) +
        ggplot2::geom_line(size = 1.2) +
        ggplot2::geom_point(size = 2.8) + ggplot2::geom_point(size = 1,color='white') + ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Participation Timeline",
          subtitle = "Cumulative turns by participant across phases",
          x = "Phase",
          y = "Cumulative Turns",
          color = "Participant"
        )
    },




    #' @description Create word count distribution plot showing message length patterns.
    #' @return ggplot object
    plot_word_count_distribution = function() {
      if (length(self$conversation_log) == 0) {
        warning("No conversation data available for plotting")
        return(NULL)
      }

      conv_df <- dplyr::bind_rows(lapply(self$conversation_log, function(x) {
        dplyr::tibble(
          agent_id = x$speaker_id,
          phase = x$phase,
          message = x$text
        )
      }))

      word_counts <- sapply(conv_df$message, function(x) length(strsplit(x, "\\s+")[[1]]))

      word_data <- data.frame(
        word_count = word_counts,
        agent_id = conv_df$agent_id,
        phase = conv_df$phase
      )

      ggplot2::ggplot(word_data, ggplot2::aes(x = .data$word_count)) +
        ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) +
        ggplot2::labs(
          title = "Word Count Distribution",
          subtitle = "Distribution of message lengths across all participants",
          x = "Words per Message",
          y = "Count"
        ) +
        ggplot2::facet_wrap(~ .data$phase, scales = "free_y")
    },

    #' @description Create participation by agent plot showing total turns per participant.
    #' @return ggplot object
    plot_participation_by_agent = function() {
      if (length(self$conversation_log) == 0) {
        warning("No conversation data available for plotting")
        return(NULL)
      }

      conv_df <- dplyr::bind_rows(lapply(self$conversation_log, function(x) {
        dplyr::tibble(
          agent_id = x$speaker_id,
          message = x$text
        )
      }))

      participation <- conv_df %>%
        dplyr::group_by(.data$agent_id) %>%
        dplyr::summarise(
          total_turns = dplyr::n(),
          total_words = sum(sapply(.data$message, function(x) length(strsplit(x, "\\s+")[[1]]))),
          avg_words_per_turn = .data$total_words / .data$total_turns,
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(.data$total_turns))

      ggplot2::ggplot(participation, ggplot2::aes(x = stats::reorder(.data$agent_id, .data$total_turns))) +
        ggplot2::geom_bar(ggplot2::aes(y = .data$total_turns), stat = "identity", fill = "steelblue", alpha = 0.8) +
        ggplot2::labs(
          title = "Participation by Agent",
          subtitle = "Total turns by participant",
          x = "Participant",
          y = "Total Turns"
        )
    },

    #' @description Create turn length timeline plot showing message length evolution over time.
    #' @return ggplot object
    plot_turn_length_timeline = function() {
      if (length(self$conversation_log) == 0) {
        warning("No conversation data available for plotting")
        return(NULL)
      }

      messages <- sapply(self$conversation_log, function(x) x$text)
      phases <- sapply(self$conversation_log, function(x) x$phase)

      word_counts <- sapply(messages, function(x) length(strsplit(x, "\\s+")[[1]]))
      timeline_data <- data.frame(
        turn_number = seq_along(word_counts),
        word_count = word_counts,
        phase = phases
      )

      window_size <- min(5, nrow(timeline_data))
      timeline_data$word_count_smooth <- stats::filter(timeline_data$word_count,
        filter = rep(1/window_size, window_size), sides = 2)

      ggplot2::ggplot(timeline_data, ggplot2::aes(x = .data$turn_number)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$word_count), alpha = 0.3, color = "gray") +
        ggplot2::geom_line(ggplot2::aes(y = .data$word_count_smooth), color = "blue", size = 1.2) +
        ggplot2::labs(
          title = "Turn Length Timeline",
          subtitle = "Evolution of message length throughout the conversation",
          x = "Turn Number",
          y = "Words per Message"
        )
    }
  ),

  private = list(
    # Helper to get the next phase/question from the script
    get_next_phase_or_question = function() {
      if (is.null(self$question_script) || length(self$question_script) == 0) {
        # If no script, run a single "generic_discussion" phase then end.
        if (self$current_phase_index == 0) { # First call
            self$current_phase_index <- 1 # Mark as started
            return(list(phase = "generic_discussion", text = self$topic))
        }
        return(NULL) # End after one generic phase if no script
      }

      self$current_phase_index <- self$current_phase_index + 1
      if (self$current_phase_index > length(self$question_script)) {
        return(NULL) # Script exhausted
      }
      return(self$question_script[[self$current_phase_index]])
    },

    # Helper to log a message
    log_message = function(speaker_id, text, turn_number, is_moderator = NULL, phase = "unknown") {
      if (is.null(is_moderator)) {
        is_moderator <- (speaker_id == self$moderator_id)
      }
      msg <- list(
        turn = turn_number %||% (length(self$conversation_log) + 1), # Fallback if turn_number not passed
        speaker_id = speaker_id,
        is_moderator = is_moderator,
        text = text,
        timestamp = Sys.time(),
        phase = phase
      )
      self$conversation_log <- c(self$conversation_log, list(msg))
    },

    # Helper to get filtered log for analysis methods
    get_filtered_log = function(turns = NULL, speaker_ids = NULL) {
      if (length(self$conversation_log) == 0) return(list())

      filtered_log <- self$conversation_log
      if (!is.null(turns)) {
        if(!is.numeric(turns)) stop("'turns' must be a numeric vector.")
        filtered_log <- Filter(function(x) x$turn %in% turns, filtered_log)
      }
      if (!is.null(speaker_ids)) {
        if(!is.character(speaker_ids)) stop("'speaker_ids' must be a character vector.")
        filtered_log <- Filter(function(x) x$speaker_id %in% speaker_ids, filtered_log)
      }
      return(filtered_log)
    },

    # Store last token counts per agent to calculate diff for group total
    last_agent_tokens = list(), # Initialized in FocusGroup$initialize

    # Helper to get recent transcript for interim summary
    get_recent_transcript_for_summary = function(n_recent_utterances = 10) {
        if (length(self$conversation_log) == 0) return("")
        start_idx <- max(1, length(self$conversation_log) - n_recent_utterances + 1)
        recent_log <- self$conversation_log[start_idx:length(self$conversation_log)]
        paste(sapply(recent_log, function(msg) {
            paste0(msg$speaker_id, ": ", msg$text)
        }), collapse = "\n\n")
    }
  )
)
