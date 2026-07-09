#' 
NULL

#' Run a Simple Focus Group Simulation
#'
#' This is the main high-level wrapper function for running focus group simulations.
#' It creates agents, sets up the focus group, runs the simulation, and returns
#' a comprehensive result object.
#'
#' @param topic Character string describing the focus group topic
#' @param participants Integer number of participants (excluding moderator)
#' @param turns_per_phase Named list or vector specifying turns for each phase.
#'   Can be a named vector like c(Opening = 2, Icebreaker = 3, Engagement = 8,
#'   Exploration = 10, Closing = 2) or a named list.
#' @param demographics Optional data frame with participant demographics. If NULL,
#'   diverse demographics will be automatically generated.
#' @param survey_responses Optional data frame with survey responses. If NULL,
#'   responses will be automatically generated.
#' @param conversation_flow Character string specifying turn-taking mechanism:
#'   "round_robin", "probabilistic", or "desire_based". Default is "desire_based".
#' @param llm_config List with LLM configuration. If NULL, uses default configuration.
#' @param seed Optional integer. Seeds R's RNG, which governs speaker selection
#'   and other in-package sampling. It does NOT make the LLM output reproducible:
#'   at `temperature > 0` the provider samples server-side, beyond R's control.
#' @param msg_mode How each agent's turn is presented to the model. `"roleflip"`
#'   (default) gives each agent its own prior turns in the assistant role and
#'   others' turns as labeled user messages, which reduces self-repetition;
#'   `"flat"` reproduces the legacy single-user-message transcript. Recorded per
#'   run so the construction is explicit rather than an implicit default.
#' @param verbose Logical, whether to print progress messages
#' @param max_participant_responses Optional integer. Maximum participant responses
#'   per moderator question before the moderator advances.
#'
#' @return A list containing:
#'   * `focus_group`: The FocusGroup object
#'   * `conversation`: Data frame with the full conversation
#'   * `summary`: Conversation summary
#'   * `basic_stats`: Basic conversation statistics
#'   * `participants`: Information about participants
#'
#' @examples
#' \dontrun{
#' # Simple focus group on social media
#' result <- run_focus_group(
#'   topic = "Social media impact on mental health",
#'   participants = 6,
#'   turns_per_phase = c(Opening = 2, Icebreaker = 3,
#'                       Engagement = 8, Exploration = 10, Closing = 2)
#' )
#'
#' # Access the conversation
#' head(result$conversation)
#'
#' # View summary
#' result$summary
#' }
#'
#' @export
run_focus_group <- function(topic,
                           participants = 6,
                           turns_per_phase = c(Opening = 2, Icebreaker = 3,
                                             Engagement = 8, Exploration = 10,
                                             Closing = 2),
                           demographics = NULL,
                           survey_responses = NULL,
                           conversation_flow = "desire_based",
                           llm_config = NULL,
                           seed = NULL,
                           msg_mode = c("roleflip","flat"),
                           verbose = TRUE,
                           max_participant_responses = NULL) {

  msg_mode <- match.arg(msg_mode)

  if (!is.null(seed)) {
    if (!requireNamespace("withr", quietly = TRUE)) stop("withr is required for deterministic seeding")
    withr::local_seed(as.integer(seed))
  }

  # Validate inputs
  if (!is.character(topic) || length(topic) != 1) {
    stop("topic must be a single character string")
  }

  if (!is.numeric(participants) || participants < 1) {
    stop("participants must be a positive integer")
  }

  flow_types <- c("round_robin", "probabilistic", "desire_based")
  if (!conversation_flow %in% flow_types) {
    stop("conversation_flow must be one of: ", paste(flow_types, collapse = ", "))
  }

  if (verbose) {
    cat("Setting up focus group simulation...\n")
    cat("Topic:", topic, "\n")
    cat("Participants:", participants, "\n")
    cat("Conversation flow:", conversation_flow, "\n")
  }

  # Resolve the configuration before agents are built, so the returned
  # config_meta reports the model actually used even when the caller relied on
  # the default (create_diverse_agents() would otherwise default internally and
  # leave config_meta with NULL provider/model).
  if (is.null(llm_config)) llm_config <- default_llmr_config()

  # Create agents
  if (verbose) cat("Creating agents...\n")
  agents <- create_diverse_agents(
    n_participants = participants,
    demographics = demographics,
    survey_responses = survey_responses,
    llm_config = llm_config
  )

  # Need to convert the list to a named list with agent IDs as names
  agents_named <- setNames(agents, sapply(agents, function(a) a$id))

  # Find moderator ID
  moderator_id <- NULL
  for (agent in agents) {
    if (agent$is_moderator || agent$id == "MOD") {
      moderator_id <- agent$id
      break
    }
  }

  if (is.null(moderator_id)) {
    stop("No moderator found in agents list")
  }

  # Create conversation flow via factory
  flow_obj <- create_conversation_flow(conversation_flow, agents_named, moderator_id)

  # Create focus group with purpose
  if (verbose) cat("Setting up focus group...\n")
  fg <- FocusGroup$new(
    topic = topic,
    purpose = paste("To explore perspectives and experiences related to", topic),
    agents = agents_named,
    moderator_id = moderator_id,
    turn_taking_flow = flow_obj,
    max_participant_responses = max_participant_responses
  )

  # Convert turns_per_phase to question script if needed
  if (!is.null(turns_per_phase)) {
    question_script <- list()
    for (phase in names(turns_per_phase)) {
      n_turns <- turns_per_phase[[phase]]
      phase_lower <- tolower(phase)

      # Map phase names to script entries
      if (phase_lower == "opening") {
        question_script <- append(question_script, list(list(phase = "opening")))
      } else if (phase_lower == "icebreaker") {
        for (i in seq_len(n_turns)) {
          question_script <- append(question_script, list(list(
            phase = "icebreaker_question",
            text = paste("What's your initial reaction or experience with", topic, "?")
          )))
        }
      } else if (phase_lower == "engagement") {
        for (i in seq_len(n_turns)) {
          question_script <- append(question_script, list(list(
            phase = "engagement_question",
            text = paste("How does", topic, "affect your daily life or work?")
          )))
        }
      } else if (phase_lower == "exploration") {
        for (i in seq_len(n_turns)) {
          question_script <- append(question_script, list(list(
            phase = "exploration_question",
            text = paste("What are your deeper thoughts or concerns about", topic, "?")
          )))
        }
      } else if (phase_lower == "closing") {
        question_script <- append(question_script, list(list(phase = "closing")))
      }
    }
    fg$question_script <- question_script
  }

  # Run simulation
  if (verbose) cat("Running simulation...\n")
  withr::with_options(
    list(focusgroup.msg_mode = msg_mode),
    fg$run_simulation(verbose = verbose)
  )

  # Generate summary and basic stats
  if (verbose) cat("Generating summary and statistics...\n")

  # Extract conversation data frame
  conversation_df <- data.frame()
  if (length(fg$conversation_log) > 0) {
    conversation_df <- do.call(rbind, lapply(fg$conversation_log, function(x) {
      data.frame(
        turn = x$turn,
        speaker_id = x$speaker_id,
        is_moderator = x$is_moderator,
        text = x$text,
        phase = x$phase,
        response_id = x$response_id,
        finish_reason = x$finish_reason,
        sent_tokens = x$sent_tokens,
        rec_tokens = x$rec_tokens,
        total_tokens = x$total_tokens,
        duration_s = x$duration_s,
        provider = x$provider,
        model = x$model,
        timestamp = x$timestamp,
        stringsAsFactors = FALSE
      )
    }))
  }

  summary_result <- if (length(fg$conversation_log) > 0) {
    fg$final_summary %||% fg$summarize(summary_level = 1)
  } else {
    "No conversation to summarize"
  }

  basic_stats <- fg$analyze()

  # Return comprehensive result
  result <- list(
    focus_group = fg,
    conversation = conversation_df,
    summary = summary_result,
    basic_stats = basic_stats,
    msg_mode = msg_mode,                                  # back-compat top-level
    config_meta = list(provider = llm_config$provider,    # parity with fg_quick()
                       model = llm_config$model,
                       msg_mode = msg_mode),
    participants = lapply(fg$agents, function(agent) {
      list(
        id = agent$id,
        is_moderator = agent$is_moderator,
        persona = agent$persona_description,
        demographics = agent$demographics,
        survey_responses = agent$survey_responses
      )
    })
  )

  if (verbose) cat("Simulation complete!\n")

  return(result)
}

#' Run a focus group quickly with sensible defaults and cost controls
#'
#' Quick focus group runner with safe defaults and cost controls
#'
#' Creates agents, a compact script, runs the simulation, and returns a structured result.
#'
#' @param topic Character. Focus group topic.
#' @param participants Integer. Number of participants (excluding moderator). Default 6.
#' @param flow Character. Turn-taking flow: one of "desire_based", "round_robin", "probabilistic".
#' @param model_config Optional `LLMR::llm_config`. If `NULL`, uses OpenAI gpt-4o-mini with small caps.
#' @param seed Optional integer. Seeds R's RNG (speaker selection and other
#'   in-package sampling); it does NOT make the LLM output reproducible, since at
#'   `temperature > 0` the provider samples server-side.
#' @param mode Character. Currently informational ("quick" or "pro"). Default "quick".
#' @param msg_mode How each agent's turn is presented to the model. `"roleflip"`
#'   (default) puts the agent's own prior turns in the assistant role and others'
#'   in labeled user messages (reduces self-repetition); `"flat"` is the legacy
#'   single-user-message transcript.
#' @param verbose Logical. Print progress.
#' @param max_participant_responses Optional integer. Maximum participant responses
#'   per moderator question before the moderator advances.
#'
#' @return A list with elements: `transcript` (data frame), `summary` (character), `participants` (list),
#'   `totals` (list), `config_meta` (list), and `focus_group` (the `FocusGroup` object).
#'
#' @examples
#' \dontrun{
#' Sys.setenv(OPENAI_API_KEY = "...")
#' res <- fg_quick("Library funding priorities", participants = 4)
#' head(res$transcript)
#' cat(res$summary)
#' }
#' @export
fg_quick <- function(topic,
                     participants = 6,
                     flow = c("desire_based","round_robin","probabilistic"),
                     model_config = NULL,
                     seed = NULL,
                     mode = c("quick","pro"),
                     msg_mode = c("roleflip","flat"),
                     verbose = TRUE,
                     max_participant_responses = NULL) {

  flow <- match.arg(flow)
  mode <- match.arg(mode)
  msg_mode <- match.arg(msg_mode)

  if (!is.null(seed)) {
    if (!requireNamespace("withr", quietly = TRUE)) stop("withr is required for deterministic seeding")
    withr::local_seed(as.integer(seed))
  }

  if (is.null(model_config)) model_config <- default_llmr_config()

  agents <- create_diverse_agents(
    n_participants = participants,
    demographics = NULL,
    survey_responses = NULL,
    llm_config = model_config
  )
  agents_named <- stats::setNames(agents, vapply(agents, function(a) a$id, ""))
  moderator_id <- "MOD"

  # Build compact script
  script <- list(
    list(phase = "opening"),
    list(phase = "engagement_question", text = paste0("From your perspective, what matters most about ", topic, " in the next year?")),
    list(phase = "engagement_question", text = paste0("What recent experience shaped your view on ", topic, "?")),
    list(phase = "exploration_question", text = paste0("What are the trade-offs or tensions around ", topic, "?")),
    list(phase = "exploration_question", text = paste0("Where do you see common ground or division on ", topic, "?")),
    list(phase = "exploration_question", text = paste0("What would change your mind on any part of ", topic, "?")),
    list(phase = "closing")
  )

  flow_obj <- create_conversation_flow(flow, agents_named, moderator_id)

  fg <- FocusGroup$new(
    topic = topic,
    purpose = paste("Explore perspectives and practical implications related to", topic),
    agents = agents_named,
    moderator_id = moderator_id,
    turn_taking_flow = flow_obj,
    question_script = script,
    max_participant_responses = max_participant_responses
  )

  withr::with_options(
    list(focusgroup.msg_mode = msg_mode),
    fg$run_simulation(verbose = verbose)
  )

  conversation_df <- if (length(fg$conversation_log)) do.call(rbind, lapply(fg$conversation_log, function(x) {
    data.frame(
      turn = x$turn,
      speaker_id = x$speaker_id,
      is_moderator = x$is_moderator,
      text = x$text,
      phase = x$phase,
      response_id = x$response_id,
      finish_reason = x$finish_reason,
      sent_tokens = x$sent_tokens,
      rec_tokens = x$rec_tokens,
      total_tokens = x$total_tokens,
      duration_s = x$duration_s,
      provider = x$provider,
      model = x$model,
      timestamp = x$timestamp,
      stringsAsFactors = FALSE
    )
  })) else data.frame()

  list(
    transcript = conversation_df,
    summary = fg$final_summary %||% fg$summarize(summary_level = 1),
    participants = lapply(fg$agents, function(a) list(id = a$id, persona = a$persona_description)),
    totals = list(total_tokens_in = fg$total_tokens_sent, total_tokens_out = fg$total_tokens_received, total_turns = length(fg$conversation_log)),
    config_meta = list(provider = model_config$provider, model = model_config$model,
                       msg_mode = msg_mode),
    focus_group = fg
  )
}

#' Quick analysis helper for a `fg_quick()` result
#'
#' @param res The object returned by `fg_quick()` (or a `FocusGroup` object).
#'
#' @return A list with `basic_stats` and `short_summary`.
#'
#' @examples
#' \dontrun{
#' out <- fg_quick("Community safety concerns", participants = 3)
#' quick <- fg_analyze_quick(out)
#' quick$basic_stats
#' }
#' @export
fg_analyze_quick <- function(res) {
  fg <- if (inherits(res, "FocusGroup")) res else res$focus_group
  
  # Basic conversation statistics
  conv_log <- fg$conversation_log
  if (length(conv_log) == 0) {
    return(list(
      basic_stats = list(messages = 0, participants = 0, summary = "No conversation data"),
      summary = "Empty conversation"
    ))
  }
  
  # Extract basic stats
  total_messages <- length(conv_log)
  participants <- unique(sapply(conv_log, function(x) if (is.null(x$speaker_id)) "Unknown" else x$speaker_id))
  participants <- participants[participants != fg$moderator_id]
  
  basic_stats <- list(
    total_messages = total_messages,
    participant_count = length(participants),
    avg_message_length = mean(sapply(conv_log, function(x) nchar(if (is.null(x$text)) "" else x$text)), na.rm = TRUE),
    total_tokens = sum(sapply(conv_log, function(x) (if (is.null(x$sent_tokens)) 0 else x$sent_tokens) +
                                       (if (is.null(x$rec_tokens)) 0 else x$rec_tokens)), na.rm = TRUE)
  )
  
  out <- list(
    basic_stats = basic_stats
  )
  out$short_summary <- fg$summarize(summary_level = 3, max_tokens = 250)
  out
}

#' Create Diverse AI Agents for Focus Group
#'
#' Creates a diverse set of AI agents with varied demographics and personas
#' for use in focus group simulations.
#'
#' @param n_participants Integer number of participants to create
#' @param demographics Optional data frame with demographics. If NULL, generates diverse demographics.
#' @param survey_responses Optional data frame with survey responses. If NULL, generates responses.
#' @param llm_config List with LLM configuration parameters
#'
#' @return List of FGAgent objects (participants + 1 moderator)
#'
#' @examples
#' \dontrun{
#' # Create 6 diverse participants
#' agents <- create_diverse_agents(6)
#'
#' # Create with custom demographics
#' demo_data <- data.frame(
#'   age = c(22, 35, 28, 41, 19, 33),
#'   gender = c("Female", "Male", "Male", "Female", "Male", "Female"),
#'   education = c("Bachelor's", "Master's", "High School", "PhD", "Some College", "Bachelor's")
#' )
#' agents <- create_diverse_agents(6, demographics = demo_data)
#' }
#'
#' @export
create_diverse_agents <- function(n_participants,
                                 demographics = NULL,
                                 survey_responses = NULL,
                                 llm_config = NULL) {

  if (!is.numeric(n_participants) || length(n_participants) != 1L ||
      is.na(n_participants) || n_participants < 1) {
    stop("`n_participants` must be a single positive integer.", call. = FALSE)
  }
  n_participants <- as.integer(n_participants)

  # Generate diverse demographics if not provided
  if (is.null(demographics)) {
    demographics <- generate_diverse_demographics(n_participants)
  }

  # Generate survey responses if not provided
  if (is.null(survey_responses)) {
    survey_responses <- generate_survey_responses(n_participants)
  }

  # Default LLM config
  if (is.null(llm_config)) llm_config <- default_llmr_config()

  # A data.frame with fewer rows than participants is recycled by row, openly.
  # (Without this, the is.list() branch below would treat the frame as a list
  # of COLUMNS and hand agent i a whole column as its persona data.)
  if (is.data.frame(demographics) && nrow(demographics) > 0 &&
      nrow(demographics) < n_participants) {
    warning(sprintf("`demographics` has %d rows for %d participants; rows are recycled in order.",
                    nrow(demographics), n_participants), call. = FALSE)
  }
  if (is.data.frame(survey_responses) && nrow(survey_responses) > 0 &&
      nrow(survey_responses) < n_participants) {
    warning(sprintf("`survey_responses` has %d rows for %d participants; rows are recycled in order.",
                    nrow(survey_responses), n_participants), call. = FALSE)
  }

  agents <- list()

  # Create participants

  for (i in seq_len(n_participants)) {
    # data.frame row -> named list of non-missing question:value pairs.
    # drop = FALSE matters: a single-column frame would otherwise collapse to a
    # vector and lose the column names.
    row_to_list <- function(df, i) {
      row <- df[i, , drop = FALSE]
      out <- list()
      for (col in names(row)) {
        v <- row[[col]]
        if (length(v) && !is.na(v) && nzchar(v) && !identical(as.character(v), "NA")) {
          out[[col]] <- as.character(v)
        }
      }
      out
    }

    agent_demographics <- if (!is.null(demographics) && is.data.frame(demographics) && nrow(demographics) > 0) {
      row_to_list(demographics, ((i - 1L) %% nrow(demographics)) + 1L)
    } else if (!is.null(demographics) && !is.data.frame(demographics) && is.list(demographics) && length(demographics) >= i) {
      demographics[[i]]
    } else {
      list(
        age = sample(18:65, 1),
        gender = sample(c("Male","Female","Nonbinary"), 1, prob = c(0.49,0.49,0.02))
      )
    }

    agent_survey <- if (!is.null(survey_responses) && is.data.frame(survey_responses) && nrow(survey_responses) > 0) {
      row_to_list(survey_responses, ((i - 1L) %% nrow(survey_responses)) + 1L)
    } else if (!is.null(survey_responses) && !is.data.frame(survey_responses) && is.list(survey_responses) && length(survey_responses) >= i) {
      survey_responses[[i]]
    } else {
      NULL
    }

    persona <- generate_persona(agent_demographics, agent_survey)

    agents[[i]] <- FGAgent$new(
      id = paste0("P", i),
      agent_details = list(
        direct_persona_description = persona,
        demographics = agent_demographics,
        survey_responses = agent_survey
      ),
      model_config = llm_config,
      is_moderator = FALSE
    )
  }

  # Create moderator
  moderator_persona <- "You are an experienced focus group moderator. You facilitate discussion, ask follow-up questions, ensure all participants have opportunities to speak, and guide the conversation through different phases. You remain neutral and encouraging."

  agents[[n_participants + 1]] <- FGAgent$new(
    id = "MOD",
    agent_details = list(
      direct_persona_description = moderator_persona,
      demographics = list(role = "professional_moderator")
    ),
    model_config = llm_config,
    is_moderator = TRUE
  )

  return(agents)
}

#' Analyze Focus Group Results
#'
#' Runs topic modeling, term-frequency, readability, and thematic analysis on
#' focus group results, with basic conversation statistics.
#'
#' @param focus_group_result Either a FocusGroup object or the result from run_focus_group()
#' @param num_topics Integer number of topics for topic modeling (default: 5)
#' @param include_plots Logical, whether to generate plots (default: TRUE)
#' @param sentiment_method Deprecated. Ignored. Sentiment analysis was removed from package scope.
#'
#' @return A list with `basic_stats`, `topics`, `sentiment` (always `NULL`;
#'   sentiment analysis was removed from package scope), `tfidf`, `readability`,
#'   `themes`, and (when `include_plots = TRUE`) `plots`.
#'
#' @examples
#' \dontrun{
#' # Run focus group and analyze
#' result <- run_focus_group("Climate change perceptions", participants = 8)
#' analysis <- analyze_focus_group(result, num_topics = 4)
#'
#' # View topic analysis
#' analysis$topics
#'
#' # View the readability table
#' analysis$readability
#' }
#'
#' @export
analyze_focus_group <- function(focus_group_result,
                               num_topics = 5,
                               include_plots = TRUE,
                               sentiment_method = "afinn") {

  # Extract FocusGroup object
  if (inherits(focus_group_result, "FocusGroup")) {
    fg <- focus_group_result
  } else if (is.list(focus_group_result) && "focus_group" %in% names(focus_group_result)) {
    fg <- focus_group_result$focus_group
  } else {
    stop("focus_group_result must be a FocusGroup object or result from run_focus_group()")
  }

  if (length(fg$conversation_log) == 0) {
    stop("No conversation data found. Run simulation first.")
  }

  cat("Performing comprehensive analysis...\n")

  # Basic statistics
  basic_stats <- fg$analyze()

  # Topic analysis
  cat("Analyzing topics...\n")
  topics <- tryCatch(
    fg$analyze_topics(num_topics = num_topics),
    error = function(e) {
      cat("Topic analysis failed:", e$message, "\n")
      NULL
    }
  )

  # Sentiment analysis
  # Sentiment analysis removed to simplify package scope
  sentiment <- NULL

  # TF-IDF analysis
  cat("Performing TF-IDF analysis...\n")
  tfidf <- tryCatch(
    fg$analyze_tfidf(),
    error = function(e) {
      cat("TF-IDF analysis failed:", e$message, "\n")
      NULL
    }
  )

  # Readability analysis
  cat("Analyzing readability...\n")
  readability <- tryCatch(
    fg$analyze_readability(),
    error = function(e) {
      cat("Readability analysis failed:", e$message, "\n")
      NULL
    }
  )

  # Thematic analysis
  cat("Performing thematic analysis...\n")
  themes <- tryCatch(
    fg$analyze_themes(),
    error = function(e) {
      cat("Thematic analysis failed:", e$message, "\n")
      NULL
    }
  )

  analysis_result <- list(
    basic_stats = basic_stats,
    topics = topics,
    sentiment = sentiment,
    tfidf = tfidf,
    readability = readability,
    themes = themes
  )

  # Generate plots if requested (ggplot2 is a Suggests; skip with a message
  # when it is not installed rather than failing the whole analysis)
  if (include_plots) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      cat("Generating visualizations...\n")
      plots <- list(
        participation_timeline = fg$plot_participation_timeline(),
        word_count_distribution = fg$plot_word_count_distribution(),
        participation_by_agent = fg$plot_participation_by_agent(),
        turn_length_timeline = fg$plot_turn_length_timeline()
      )
      analysis_result$plots <- plots
    } else {
      message("Package 'ggplot2' is not installed; skipping plots.")
    }
  }

  cat("Analysis complete!\n")
  return(analysis_result)
}

#' Generate Diverse Demographics
#'
#' Internal function to generate diverse demographic profiles for participants.
#'
#' @param n Integer number of profiles to generate
#' @return Data frame with demographic information
#'
#' @keywords internal
generate_diverse_demographics <- function(n) {
  seed_val <- getOption("focusgroup.seed", NA_integer_)
  build <- function() {
    ages <- sample(18:65, n, replace = TRUE)
    genders <- sample(c("Male", "Female", "Nonbinary"), n, replace = TRUE, prob = c(0.49, 0.49, 0.02))

    education_levels <- c("High School", "Some College", "Bachelor's", "Master's", "PhD")
    education <- sample(education_levels, n, replace = TRUE, prob = c(0.2, 0.2, 0.35, 0.2, 0.05))

    income_levels <- c("Under $30k", "$30k-$50k", "$50k-$75k", "$75k-$100k", "Over $100k")
    income <- sample(income_levels, n, replace = TRUE, prob = c(0.15, 0.25, 0.25, 0.20, 0.15))

    locations <- c("Urban", "Suburban", "Rural")
    location <- sample(locations, n, replace = TRUE, prob = c(0.4, 0.4, 0.2))

    data.frame(
      age = ages,
      gender = genders,
      education = education,
      income = income,
      location = location,
      stringsAsFactors = FALSE
    )
  }
  # with_seed keeps the seeding contained: the draw is deterministic but the
  # caller's RNG stream is restored afterwards.
  if (!is.na(seed_val)) withr::with_seed(as.integer(seed_val), build()) else build()
}

#' Generate Survey Responses
#'
#' Internal function to generate diverse survey responses for participants.
#'
#' @param n Integer number of response sets to generate
#' @return Data frame with survey responses
#'
#' @keywords internal
generate_survey_responses <- function(n) {
  # Generate responses on Likert scales for common topics
  tech_usage <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.3, 0.15))
  social_media <- sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.4, 0.25))
  privacy_concern <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.25, 0.3, 0.15))
  env_concern <- sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.15, 0.35, 0.35))

  data.frame(
    tech_usage_comfort = tech_usage,
    social_media_frequency = social_media,
    privacy_concern_level = privacy_concern,
    environmental_concern = env_concern,
    stringsAsFactors = FALSE
  )
}

#' Generate a participant persona from demographics and survey responses
#'
#' Renders the persona text for a synthetic participant from the demographics and
#' survey responses the researcher supplied. It states those facts and nothing
#' more: what a given age, education, place of residence, or survey answer implies
#' about a person is left to the model, not decided here. Mapping a demographic
#' label to a fixed disposition is the essentialism a research instrument should
#' avoid, so the package does not do it.
#'
#' Both the demographics and the survey responses are rendered in full (every
#' supplied field), not a fixed subset. Pass survey responses keyed by the
#' question wording (for survey-file input, the variable label is used as the key)
#' so the model sees the item, not a code name.
#'
#' @param demographics A named list (or single-row data frame coerced to one) of
#'   demographic fields, e.g. `list(age = 63, education = "High school",
#'   region = "South")`. Field names are shown to the model as written.
#' @param survey_responses Optional named list of survey responses, keyed by the
#'   question text: `list("Party identification" = "Strong Democrat")`.
#' @param style One of `"labeled"` (default) or `"paragraph"`. `"labeled"` lists
#'   the demographics and then a Question/Answer block; `"paragraph"` fuses them
#'   into a single natural paragraph. Defaults to
#'   `getOption("focusgroup.persona_style", "labeled")`.
#' @return Character string with the persona description.
#'
#' @keywords internal
generate_persona <- function(demographics, survey_responses = NULL,
                             style = getOption("focusgroup.persona_style", "labeled")) {
  style <- match.arg(style, c("labeled", "paragraph"))

  # Normalize a one-row data frame to a named list; drop NA/empty fields so they
  # are simply absent rather than rendered as "NA".
  to_named_list <- function(x) {
    if (is.null(x)) return(list())
    if (is.data.frame(x)) x <- as.list(x[1, , drop = FALSE])
    if (!is.list(x)) x <- as.list(x)
    x <- lapply(x, function(v) if (length(v)) v[[1]] else NA)
    keep <- vapply(x, function(v) {
      !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(as.character(v)))
    }, logical(1))
    x[keep]
  }

  # Strip leftover code prefixes/suffixes that some labeled surveys carry on a
  # value label (e.g. "1. Male", "Male (R volunteered)"). A no-op on plain text.
  tidy_value <- function(x) {
    x <- as.character(x)
    x <- gsub("^[0-9-]+\\.\\s*", "", x)
    x <- gsub("\\s*\\([^)]*\\)$", "", x)
    trimws(x)
  }

  demos <- to_named_list(demographics)
  demos <- lapply(demos, tidy_value)
  resp  <- to_named_list(survey_responses)
  resp  <- lapply(resp, tidy_value)

  demo_text <- if (length(demos)) {
    paste(sprintf("%s: %s", names(demos), unlist(demos, use.names = FALSE)),
          collapse = "; ")
  } else NULL

  closing <- paste(
    "Speak from this background as the conversation gives you reason to.",
    "Stay engaged without dominating; respond to the moderator and to what others say.")

  if (identical(style, "paragraph")) {
    bits <- c(
      "You are a participant in a focus group.",
      if (!is.null(demo_text)) sprintf("Your background: %s.", demo_text),
      if (length(resp)) sprintf(
        "On a pre-session questionnaire you said: %s.",
        paste(sprintf("%s -- %s", names(resp), unlist(resp, use.names = FALSE)),
              collapse = "; ")),
      closing)
    return(paste(bits, collapse = " "))
  }

  # "labeled" (default): a structured block, reusing the shared renderers.
  bits <- c(
    "You are a participant in a focus group.",
    if (!is.null(demo_text)) sprintf("Background: %s.", demo_text),
    if (length(resp)) format_survey_responses(resp),
    closing)
  paste(bits, collapse = "\n")
}

# Default missing-value vocabulary for labeled surveys. Configurable per call.
.fg_default_na_strings <- c("inapplicable", "missing", "refused",
                            "don't know", "don.t know", "not asked",
                            "legitimate skip")

# Decode a haven column to its value label (character); leave plain columns as
# character. NA out values matching `na_strings`.
.fg_decode_col <- function(col, na_strings) {
  v <- if (inherits(col, c("haven_labelled", "labelled"))) {
    as.character(haven::as_factor(col, levels = "labels"))
  } else as.character(col)
  if (length(na_strings)) {
    pat <- paste(na_strings, collapse = "|")
    v[grepl(pat, v, ignore.case = TRUE)] <- NA
  }
  v
}

# Choose the column key shown to the model: the human variable label (question
# wording) when present, else the column name. `named` overrides take priority.
.fg_col_key <- function(raw_lab, code, override = NULL) {
  if (!is.null(override) && nzchar(override)) return(override)
  lab <- tryCatch(attr(raw_lab[[code]], "label"), error = function(...) NULL)
  if (!is.null(lab) && nzchar(lab)) lab else code
}

# Shared core: given a decoded demographics frame and survey-responses frame
# (already row-aligned, same nrow), select rows and build agents.
.fg_agents_from_frames <- function(demo_df, survey_df, n_participants,
                                   llm_config, rows = NULL, weights = NULL) {
  N <- nrow(demo_df)
  idx <- seq_len(N)
  if (!is.null(rows)) {
    idx <- if (is.function(rows)) which(rows(demo_df)) else {
      if (is.logical(rows)) which(rows) else as.integer(rows)
    }
    idx <- idx[idx >= 1 & idx <= N]
    if (!length(idx)) stop("`rows` selected no eligible respondents.")
  }

  seed_val <- getOption("focusgroup.seed", NA_integer_)

  prob <- NULL
  if (!is.null(weights)) {
    w <- suppressWarnings(as.numeric(weights))[idx]
    w[is.na(w) | w < 0] <- 0
    if (sum(w) > 0) prob <- w / sum(w)
  }

  take <- if (length(idx) > n_participants) {
    draw <- function() sample(idx, n_participants, prob = prob)
    # with_seed keeps the seeding contained: the respondent draw is
    # deterministic but the caller's RNG stream is restored afterwards.
    if (!is.null(seed_val) && !is.na(seed_val)) {
      withr::with_seed(as.integer(seed_val), draw())
    } else draw()
  } else idx

  demo_sample <- demo_df[take, , drop = FALSE]
  survey_sample <- if (!is.null(survey_df)) survey_df[take, , drop = FALSE] else NULL

  create_diverse_agents(
    n_participants = n_participants,
    demographics = demo_sample,
    survey_responses = survey_sample,
    llm_config = llm_config
  )
}

#' Create agents from a labeled survey file
#'
#' Reads a labeled survey file (Stata `.dta`, SPSS `.sav`, or SAS
#' `.sas7bdat`), decodes the chosen variables from their value labels, and turns
#' each selected respondent into an `FGAgent` whose persona states that
#' respondent's demographics and survey answers. Numeric codes are decoded from
#' the file's own value labels, so the same call works on ANES, GSS, WVS, or any
#' other labeled file; nothing about a particular dataset is hard-coded.
#'
#' The survey answers are keyed by each variable's question wording (its label in
#' the file) so the model sees the item, not a code name. The persona draws no
#' inferences from the answers; it states them and lets the model interpret.
#'
#' @param n_participants Integer number of participants (excludes the moderator).
#' @param survey_path Path to the survey file (`.dta`, `.sav`, or `.sas7bdat`).
#' @param demographic_vars Variable names (codes) to render as demographics.
#'   May be a named vector, in which case the names are shown as the field
#'   labels. If `NULL`, common demographic variables are auto-detected by their
#'   labels.
#' @param survey_vars Variable names (codes) to render as survey responses. May
#'   be named (names become the question wording shown). If `NULL`, labeled
#'   variables that are not demographics are used.
#' @param rows Optional row selector restricting the eligible respondents before
#'   sampling: an integer or logical vector, or a predicate
#'   `function(df) -> logical` over the decoded demographics frame.
#' @param weights Optional sampling weights: a column name in the file, or a
#'   numeric vector aligned to the file's rows. Used only to weight which
#'   respondents are drawn.
#' @param na_strings Character vector of value-label substrings treated as
#'   missing (case-insensitive). Defaults to a small common set; pass your own to
#'   match another file's missing-data vocabulary.
#' @param llm_config Optional `LLMR::llm_config` for all agents.
#'
#' @return A list of `FGAgent` objects (participants + moderator).
#' @seealso [create_agents_from_data()] for an in-memory data frame, and
#'   `LLMR::anes_2024_personas` for a ready-made example.
#' @examples
#' \dontrun{
#' agents <- create_agents_from_survey(
#'   n_participants = 6,
#'   survey_path = "anes_timeseries_2024_stata.dta",
#'   demographic_vars = c(age = "V241458x", education = "V241465x"),
#'   survey_vars = c("Party identification" = "V241227x",
#'                   "Ideology" = "V241177"),
#'   rows = function(df) df$age != "18-24"   # exclude the youngest band
#' )
#' }
#' @export
create_agents_from_survey <- function(n_participants,
                                      survey_path,
                                      demographic_vars = NULL,
                                      survey_vars = NULL,
                                      rows = NULL,
                                      weights = NULL,
                                      na_strings = .fg_default_na_strings,
                                      llm_config = NULL) {
  if (!file.exists(survey_path)) stop("Survey file not found at: ", survey_path)
  if (is.null(llm_config)) llm_config <- default_llmr_config()

  ext <- tolower(tools::file_ext(survey_path))
  raw_lab <- switch(ext,
    "dta" = haven::read_dta(survey_path),
    "sav" = haven::read_sav(survey_path),
    "sas7bdat" = haven::read_sas(survey_path),
    stop("Unsupported survey file type: .", ext)
  )

  label_of <- function(v) tryCatch(attr(raw_lab[[v]], "label") %||% "", error = function(...) "")
  detect_by_label <- function(patterns) {
    labs <- vapply(names(raw_lab), label_of, "")
    names(raw_lab)[which(Reduce(`|`, lapply(patterns, function(p)
      grepl(p, labs, ignore.case = TRUE))))]
  }

  # Demographics: explicit, else auto-detect common fields by label.
  if (is.null(demographic_vars)) {
    pick1 <- function(p) { hit <- detect_by_label(p); if (length(hit)) hit[[1]] else NA_character_ }
    demographic_vars <- c(age = pick1("age"), gender = pick1(c("sex", "gender")),
                          education = pick1("educ"), race = pick1("race"),
                          income = pick1("income"))
    demographic_vars <- demographic_vars[!is.na(demographic_vars)]
  }
  # Survey vars: explicit, else any labeled var that is not a demographic.
  if (is.null(survey_vars)) {
    has_label <- vapply(names(raw_lab), function(v) nzchar(label_of(v)), logical(1))
    survey_vars <- setdiff(names(raw_lab)[has_label], unname(demographic_vars))
  }

  demo_codes <- demographic_vars[unname(demographic_vars) %in% names(raw_lab)]
  svy_codes  <- survey_vars[unname(survey_vars) %in% names(raw_lab)]
  if (!length(demo_codes)) stop("None of `demographic_vars` are in the file.")

  demo_keys <- vapply(seq_along(demo_codes), function(i)
    .fg_col_key(raw_lab, unname(demo_codes)[i], names(demo_codes)[i]), "")
  svy_keys <- if (length(svy_codes)) vapply(seq_along(svy_codes), function(i)
    .fg_col_key(raw_lab, unname(svy_codes)[i], names(svy_codes)[i]), "") else character(0)

  demo_df <- as.data.frame(lapply(unname(demo_codes), function(c)
    .fg_decode_col(raw_lab[[c]], na_strings)), stringsAsFactors = FALSE)
  names(demo_df) <- demo_keys
  survey_df <- if (length(svy_codes)) {
    s <- as.data.frame(lapply(unname(svy_codes), function(c)
      .fg_decode_col(raw_lab[[c]], na_strings)), stringsAsFactors = FALSE)
    names(s) <- svy_keys; s
  } else NULL

  # Keep respondents with at least one demographic; the row filter is applied to
  # this SAME frame so demographics and survey answers never misalign.
  keep <- rowSums(!is.na(demo_df)) > 0
  weights_vec <- if (is.character(weights) && length(weights) == 1L &&
                     weights %in% names(raw_lab)) {
    suppressWarnings(as.numeric(haven::zap_labels(raw_lab[[weights]])))
  } else weights
  demo_df <- demo_df[keep, , drop = FALSE]
  if (!is.null(survey_df)) survey_df <- survey_df[keep, , drop = FALSE]
  if (!is.null(weights_vec) && length(weights_vec) == length(keep)) weights_vec <- weights_vec[keep]

  .fg_agents_from_frames(demo_df, survey_df, n_participants, llm_config,
                         rows = rows, weights = weights_vec)
}

#' Create agents from an in-memory data frame of respondents
#'
#' Like [create_agents_from_survey()] but starting from a data frame already in
#' memory (for example `LLMR::anes_2024_personas`). Demographic columns are
#' rendered as background; the remaining columns are rendered as survey responses,
#' keyed by their question wording when the frame carries a `dictionary` attribute
#' (see [LLMR::llm_persona_split()]), else by their column names. Values are taken
#' as-is (decode and clean them first if they are still coded).
#'
#' @param data A data frame, one respondent per row.
#' @param n_participants Integer number of participants (excludes the moderator).
#' @param demographic_cols Character vector of columns to render as demographics.
#'   Defaults to the `data`'s `"demographic_fields"` attribute when present, else
#'   a small set of common demographic column names found in `data`.
#' @param rows,weights See [create_agents_from_survey()].
#' @param llm_config Optional `LLMR::llm_config` for all agents.
#' @return A list of `FGAgent` objects (participants + moderator).
#' @examples
#' \dontrun{
#' data(anes_2024_personas, package = "LLMR")
#' agents <- create_agents_from_data(anes_2024_personas, n_participants = 6)
#' }
#' @export
create_agents_from_data <- function(data, n_participants,
                                    demographic_cols = NULL,
                                    rows = NULL, weights = NULL,
                                    llm_config = NULL) {
  stopifnot(is.data.frame(data))
  if (is.null(llm_config)) llm_config <- default_llmr_config()

  if (is.null(demographic_cols)) {
    demographic_cols <- attr(data, "demographic_fields")
  }
  if (is.null(demographic_cols)) {
    common <- c("age", "sex", "gender", "education", "race/ethnicity", "race",
                "marital status", "household income", "income", "religion",
                "census region", "region", "community type", "employment status",
                "home ownership", "children in household", "union household",
                "military service", "attention to politics")
    demographic_cols <- intersect(common, names(data))
  }
  demographic_cols <- intersect(demographic_cols, names(data))
  # a score/sort column is not a persona fact
  drop_cols <- intersect(c("ideology_score"), names(data))
  survey_cols <- setdiff(names(data), c(demographic_cols, drop_cols))

  to_char <- function(df) as.data.frame(lapply(df, function(x) {
    v <- as.character(x); v[!nzchar(v %||% "")] <- NA; v
  }), stringsAsFactors = FALSE, check.names = FALSE)

  # Render with the human question wording, not the tidy column handle, when the
  # data carries a dictionary (handle -> question). Handles stay the column names
  # so dplyr select()/filter() work; the persona text reads naturally.
  dict <- attr(data, "dictionary")
  relabel <- function(cols) {
    if (is.null(dict) || !all(c("handle", "question") %in% names(dict))) return(cols)
    q <- dict$question[match(cols, dict$handle)]
    ifelse(is.na(q), cols, q)
  }

  demo_df <- to_char(data[, demographic_cols, drop = FALSE])
  names(demo_df) <- relabel(demographic_cols)
  survey_df <- if (length(survey_cols)) {
    s <- to_char(data[, survey_cols, drop = FALSE])
    names(s) <- relabel(survey_cols); s
  } else NULL

  wv <- if (is.character(weights) && length(weights) == 1L && weights %in% names(data)) {
    suppressWarnings(as.numeric(data[[weights]]))
  } else weights

  .fg_agents_from_frames(demo_df, survey_df, n_participants, llm_config,
                         rows = rows, weights = wv)
}

 
