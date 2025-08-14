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
#' @param seed Optional integer for reproducibility
#' @param verbose Logical, whether to print progress messages
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
                           verbose = TRUE) {

  if (!is.null(seed)) {
    set.seed(seed)
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

  # Create conversation flow
  flow_obj <- switch(conversation_flow,
    "round_robin" = RoundRobinFlow$new(agents_named, moderator_id),
    "probabilistic" = ProbabilisticFlow$new(agents_named, moderator_id),
    "desire_based" = DesireBasedFlow$new(agents_named, moderator_id)
  )

  # Create focus group with purpose
  if (verbose) cat("Setting up focus group...\n")
  fg <- FocusGroup$new(
    topic = topic,
    purpose = paste("To explore perspectives and experiences related to", topic),
    agents = agents_named,
    moderator_id = moderator_id,
    turn_taking_flow = flow_obj
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
        for (i in 1:n_turns) {
          question_script <- append(question_script, list(list(
            phase = "icebreaker_question",
            text = paste("What's your initial reaction or experience with", topic, "?")
          )))
        }
      } else if (phase_lower == "engagement") {
        for (i in 1:n_turns) {
          question_script <- append(question_script, list(list(
            phase = "engagement_question",
            text = paste("How does", topic, "affect your daily life or work?")
          )))
        }
      } else if (phase_lower == "exploration") {
        for (i in 1:n_turns) {
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
  fg$run_simulation(verbose = verbose)

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
    fg$summarize(summary_level = 1)
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
#' @param seed Optional integer for reproducibility.
#' @param mode Character. Currently informational ("quick" or "pro"). Default "quick".
#' @param verbose Logical. Print progress.
#'
#' @return A list with elements: `transcript` (tibble), `summary` (character), `participants` (list),
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
                     verbose = TRUE) {

  flow <- match.arg(flow)
  mode <- match.arg(mode)

  if (is.null(model_config)) {
    if (!requireNamespace("LLMR", quietly = TRUE)) stop("LLMR is required")
    model_config <- LLMR::llm_config(
      provider = "openai",
      model = "gpt-4.1",
      temperature = 0.7,
      max_tokens = 500
    )
  }

  agents <- create_diverse_agents(
    n_participants = participants,
    demographics = NULL,
    survey_responses = NULL,
    llm_config = model_config
  )
  agents_named <- stats::setNames(agents, vapply(agents, function(a) a$id, ""))
  moderator_id <- agents[[length(agents)]]$id

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
    question_script = script
  )

  fg$run_simulation(verbose = verbose)

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
    summary = fg$summarize(summary_level = 1),
    participants = lapply(fg$agents, function(a) list(id = a$id, persona = a$persona_description)),
    totals = list(total_tokens_in = fg$total_tokens_sent, total_tokens_out = fg$total_tokens_received, total_turns = length(fg$conversation_log)),
    config_meta = list(provider = model_config$provider, model = model_config$model),
    focus_group = fg
  )
}

#' Quick analysis helper for a `fg_quick()` result
#'
#' @param res The object returned by `fg_quick()` (or a `FocusGroup` object).
#'
#' @return A list with `basic_stats`, optional `sentiment` (if tidytext present), and `short_summary`.
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
  participants <- unique(sapply(conv_log, function(x) x$speaker_id %||% "Unknown"))
  participants <- participants[participants != fg$moderator_id]
  
  basic_stats <- list(
    total_messages = total_messages,
    participant_count = length(participants),
    avg_message_length = mean(sapply(conv_log, function(x) nchar(x$text %||% "")), na.rm = TRUE),
    total_tokens = sum(sapply(conv_log, function(x) (x$sent_tokens %||% 0) + (x$rec_tokens %||% 0)), na.rm = TRUE)
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

  # Generate diverse demographics if not provided
  if (is.null(demographics)) {
    demographics <- generate_diverse_demographics(n_participants)
  }

  # Generate survey responses if not provided
  if (is.null(survey_responses)) {
    survey_responses <- generate_survey_responses(n_participants)
  }

  # Default LLM config
  if (is.null(llm_config)) {
    if (requireNamespace("LLMR", quietly = TRUE)) {
      llm_config <- LLMR::llm_config(
        provider = "openai",
        model = "gpt-4o-mini",
        temperature = 0.7,
        max_tokens = 500
      )
    } else {
      stop("LLMR package is required to create agents")
    }
  }

  agents <- list()

  # Create participants

  for (i in 1:n_participants) {
    agent_demographics <- if (!is.null(demographics) && is.data.frame(demographics) && nrow(demographics) >= i) {
      # Create proper question:value pairs
      demo_row <- demographics[i, ]
      demo_list <- list()
      for (col in names(demo_row)) {
        if (!is.na(demo_row[[col]]) && demo_row[[col]] != "" && demo_row[[col]] != "NA") {
          demo_list[[col]] <- as.character(demo_row[[col]])
        }
      }
      demo_list
    } else if (!is.null(demographics) && is.list(demographics) && length(demographics) >= i) {
      demographics[[i]]
    } else {
      list(
        age = sample(18:65, 1),
        gender = sample(c("Male","Female","Nonbinary"), 1, prob = c(0.49,0.49,0.02))
      )
    }

    agent_survey <- if (!is.null(survey_responses) && is.data.frame(survey_responses) && nrow(survey_responses) >= i) {
      # Create proper question:value pairs
      survey_row <- survey_responses[i, ]
      survey_list <- list()
      for (col in names(survey_row)) {
        if (!is.na(survey_row[[col]]) && survey_row[[col]] != "" && survey_row[[col]] != "NA") {
          survey_list[[col]] <- as.character(survey_row[[col]])
        }
      }
      survey_list
    } else if (!is.null(survey_responses) && is.list(survey_responses) && length(survey_responses) >= i) {
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
#' Performs comprehensive analysis on focus group results, including topic modeling,
#' sentiment analysis, and basic statistics.
#'
#' @param focus_group_result Either a FocusGroup object or the result from run_focus_group()
#' @param num_topics Integer number of topics for topic modeling (default: 5)
#' @param include_plots Logical, whether to generate plots (default: TRUE)
#' @param sentiment_method Character, sentiment analysis method (default: "afinn")
#'
#' @return List containing various analysis results including topics, sentiment,
#'   readability, and visualizations
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
#' # View sentiment over time
#' analysis$plots$sentiment_timeline
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

  # Generate plots if requested
  if (include_plots) {
    cat("Generating visualizations...\n")
    plots <- list(
      participation_timeline = fg$plot_participation_timeline(),
      word_count_distribution = fg$plot_word_count_distribution(),
      participation_by_agent = fg$plot_participation_by_agent(),
      turn_length_timeline = fg$plot_turn_length_timeline()
    )
    analysis_result$plots <- plots
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
  if (!is.na(seed_val)) set.seed(seed_val)
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

#' Generate Persona
#'
#' Internal function to generate a persona description based on demographics and survey responses.
#'
#' @param demographics List of demographic information
#' @param survey_responses List of survey responses (optional)
#' @return Character string with persona description
#'
#' @keywords internal
generate_persona <- function(demographics, survey_responses = NULL) {
  # Clean ANES-style values (remove codes like "1. " or "99. ")
  clean_value <- function(x) {
    if (is.null(x) || is.na(x)) return("unspecified")
    x <- as.character(x)
    # Handle missing/inapplicable values
    if (grepl("inapplicable|missing|refused|don't know", x, ignore.case = TRUE)) {
      return("unspecified")
    }
    # Remove leading codes like "1. ", "99. ", etc.
    x <- gsub("^[0-9]+\\.\\s*", "", x)
    # Remove trailing codes in parentheses
    x <- gsub("\\s*\\([^)]*\\)$", "", x)
    x <- trimws(x)
    if (nchar(x) == 0) return("unspecified")
    return(x)
  }
  
  # Handle demographics as either named list or unnamed list
  get_demo_value <- function(demos, field) {
    if (is.null(demos)) return("unspecified")
    if (is.list(demos)) {
      # Try named access first
      if (!is.null(demos[[field]])) {
        return(clean_value(demos[[field]]))
      }
      # Try positional access for unnamed lists
      if (field == "age" && length(demos) >= 1) return(clean_value(demos[[1]]))
      if (field == "gender" && length(demos) >= 2) return(clean_value(demos[[2]]))
      if (field == "education" && length(demos) >= 3) return(clean_value(demos[[3]]))
    }
    return("unspecified")
  }
  
  age <- get_demo_value(demographics, "age")
  age_num <- suppressWarnings(as.numeric(age))
  if (!is.na(age_num)) age <- age_num
  gender <- tolower(get_demo_value(demographics, "gender"))
  education <- tolower(get_demo_value(demographics, "education"))

  base_persona <- sprintf(
    "You are a %s-year-old %s with %s education.",
    ifelse(!is.na(age_num), age_num, "unspecified"),
    ifelse(nzchar(gender), gender, "person"),
    ifelse(nzchar(education), education, "unspecified")
  )

  # Add personality traits based on demographics and survey
  traits <- c()

  if (!is.null(survey_responses)) {
    # Political orientation and ideology
    if (!is.null(survey_responses$party_id)) {
      party <- tolower(survey_responses$party_id)
      if (grepl("democrat", party)) {
        traits <- c(traits, "You identify as a Democrat and generally support Democratic policies and progressive values.")
      } else if (grepl("republican", party)) {
        traits <- c(traits, "You identify as a Republican and generally support Republican policies and conservative values.")
      } else if (grepl("independent", party)) {
        traits <- c(traits, "You identify as politically independent and evaluate issues on their merits rather than party lines.")
      }
    }
    
    if (!is.null(survey_responses$ideology)) {
      ideology <- tolower(survey_responses$ideology)
      if (grepl("liberal", ideology)) {
        traits <- c(traits, "You consider yourself politically liberal and support government intervention to address social and economic issues.")
      } else if (grepl("conservative", ideology)) {
        traits <- c(traits, "You consider yourself politically conservative and prefer limited government and traditional values.")
      } else if (grepl("moderate", ideology)) {
        traits <- c(traits, "You consider yourself politically moderate and take a balanced approach to most issues.")
      }
    }
    
    # Vote intent and confidence
    if (!is.null(survey_responses$vote_intent)) {
      vote <- tolower(survey_responses$vote_intent)
      if (grepl("harris", vote)) {
        traits <- c(traits, "You plan to vote for Harris in the upcoming election and believe she represents the best choice for the country.")
      } else if (grepl("trump", vote)) {
        traits <- c(traits, "You plan to vote for Trump in the upcoming election and believe he represents the best choice for the country.")
      }
    }
    
    if (!is.null(survey_responses$vote_confidence)) {
      confidence <- tolower(survey_responses$vote_confidence)
      if (grepl("very confident", confidence)) {
        traits <- c(traits, "You are very confident in your vote choice and have strong convictions about your political preferences.")
      } else if (grepl("somewhat confident", confidence)) {
        traits <- c(traits, "You are somewhat confident in your vote choice but remain open to new information.")
      }
    }
    
    # Institutional approval
    if (!is.null(survey_responses$congress_approval)) {
      approval <- tolower(survey_responses$congress_approval)
      if (grepl("approve", approval)) {
        traits <- c(traits, "You generally approve of how Congress is handling its job and trust the legislative process.")
      } else if (grepl("disapprove", approval)) {
        traits <- c(traits, "You disapprove of how Congress is handling its job and believe it needs significant reform.")
      }
    }
    
    if (!is.null(survey_responses$supreme_court_approval)) {
      approval <- tolower(survey_responses$supreme_court_approval)
      if (grepl("approve", approval)) {
        traits <- c(traits, "You approve of how the Supreme Court is handling its job and trust the judicial system.")
      } else if (grepl("disapprove", approval)) {
        traits <- c(traits, "You disapprove of how the Supreme Court is handling its job and question its impartiality.")
      }
    }
    
    # Economic views
    if (!is.null(survey_responses$economy_rating)) {
      economy <- tolower(survey_responses$economy_rating)
      if (grepl("excellent|good", economy)) {
        traits <- c(traits, "You believe the economy is performing well and feel optimistic about economic conditions.")
      } else if (grepl("poor|fair", economy)) {
        traits <- c(traits, "You believe the economy is struggling and feel concerned about economic conditions.")
      }
    }
    
    if (!is.null(survey_responses$personal_finances)) {
      finances <- tolower(survey_responses$personal_finances)
      if (grepl("better", finances)) {
        traits <- c(traits, "Your personal financial situation has improved recently, making you feel more secure.")
      } else if (grepl("worse", finances)) {
        traits <- c(traits, "Your personal financial situation has declined recently, causing you financial stress.")
      }
    }
    
    # Social policy views
    if (!is.null(survey_responses$abortion_policy)) {
      abortion <- tolower(survey_responses$abortion_policy)
      if (grepl("approve", abortion)) {
        traits <- c(traits, "You support abortion rights and believe women should have control over their reproductive choices.")
      } else if (grepl("disapprove", abortion)) {
        traits <- c(traits, "You oppose abortion and believe in protecting the rights of the unborn.")
      }
    }
    
    if (!is.null(survey_responses$immigration_policy)) {
      immigration <- tolower(survey_responses$immigration_policy)
      if (grepl("approve", immigration)) {
        traits <- c(traits, "You support more open immigration policies and believe in providing opportunities for immigrants.")
      } else if (grepl("disapprove", immigration)) {
        traits <- c(traits, "You support stricter immigration policies and believe in stronger border control.")
      }
    }
    
    if (!is.null(survey_responses$climate_policy)) {
      climate <- tolower(survey_responses$climate_policy)
      if (grepl("approve", climate)) {
        traits <- c(traits, "You support strong climate change policies and believe immediate action is necessary to address environmental issues.")
      } else if (grepl("disapprove", climate)) {
        traits <- c(traits, "You are skeptical of climate change policies and believe they may harm the economy unnecessarily.")
      }
    }
    
    if (!is.null(survey_responses$gun_policy)) {
      gun <- tolower(survey_responses$gun_policy)
      if (grepl("approve", gun)) {
        traits <- c(traits, "You support stricter gun control measures and believe they are necessary for public safety.")
      } else if (grepl("disapprove", gun)) {
        traits <- c(traits, "You oppose stricter gun control measures and believe in protecting Second Amendment rights.")
      }
    }
    
    # Election importance
    if (!is.null(survey_responses$election_importance)) {
      importance <- tolower(survey_responses$election_importance)
      if (grepl("very important", importance)) {
        traits <- c(traits, "You believe this election is extremely important and will have significant consequences for the country's future.")
      } else if (grepl("somewhat important", importance)) {
        traits <- c(traits, "You believe this election is important but may not be as consequential as some suggest.")
      }
    }
  }

  # Add age-based traits
  if (!is.na(age_num)) {
    if (age_num < 25) {
      traits <- c(traits, "You represent a younger perspective and are digitally native.")
    } else if (age_num > 50) {
      traits <- c(traits, "You bring life experience and may have traditional values.")
    }
  }

  # Add education-based traits
  if (!is.null(education)) {
    if (grepl("PhD|Master", education)) {
      traits <- c(traits, "You tend to think analytically and ask detailed questions.")
    } else if (grepl("High School", education)) {
      traits <- c(traits, "You speak in practical terms and focus on real-world applications.")
    }
  }

  # Add demographic-based traits (gate race-based generalizations)
  if (!is.null(demographics)) {
    avoid_group_stereotypes <- TRUE
    if (!avoid_group_stereotypes) {
      if (!is.null(demographics$race)) {
        race <- tolower(demographics$race)
        if (grepl("white", race)) {
          traits <- c(traits, "As a white American, you may have different perspectives on racial issues compared to people of color.")
        } else if (grepl("black|african american", race)) {
          traits <- c(traits, "As a Black American, you bring important perspectives on racial justice and equality issues.")
        } else if (grepl("hispanic|latin", race)) {
          traits <- c(traits, "As a Hispanic/Latino American, you bring important perspectives on immigration and cultural issues.")
        } else if (grepl("asian", race)) {
          traits <- c(traits, "As an Asian American, you bring important perspectives on diversity and inclusion issues.")
        }
      }
    }
    
    if (!is.null(demographics$income)) {
      income <- tolower(demographics$income)
      if (grepl("high|upper", income)) {
        traits <- c(traits, "Your higher income level gives you a different perspective on economic policies and taxation.")
      } else if (grepl("low|lower", income)) {
        traits <- c(traits, "Your lower income level gives you a different perspective on economic policies and social programs.")
      } else if (grepl("middle", income)) {
        traits <- c(traits, "As a middle-class American, you are concerned about economic stability and opportunities for your family.")
      }
    }
    
    if (!is.null(demographics$employment)) {
      employment <- tolower(demographics$employment)
      if (grepl("employed|working", employment)) {
        traits <- c(traits, "As a working person, you are concerned about job security, wages, and workplace policies.")
      } else if (grepl("unemployed", employment)) {
        traits <- c(traits, "As someone who is unemployed, you are particularly concerned about job creation and economic recovery.")
      } else if (grepl("retired", employment)) {
        traits <- c(traits, "As a retiree, you are concerned about Social Security, Medicare, and retirement security.")
      }
    }
    
    if (!is.null(demographics$veteran)) {
      veteran <- tolower(demographics$veteran)
      if (grepl("yes|veteran", veteran)) {
        traits <- c(traits, "As a veteran, you have unique perspectives on national security, foreign policy, and veterans' issues.")
      }
    }
    
    if (!is.null(demographics$union_member)) {
      union <- tolower(demographics$union_member)
      if (grepl("yes|member", union)) {
        traits <- c(traits, "As a union member, you strongly support workers' rights and collective bargaining.")
      }
    }
    
    if (!is.null(demographics$home_owner)) {
      home <- tolower(demographics$home_owner)
      if (grepl("yes|owner", home)) {
        traits <- c(traits, "As a homeowner, you are concerned about property values, housing policies, and local government.")
      } else if (grepl("no|renter", home)) {
        traits <- c(traits, "As a renter, you are concerned about housing affordability and tenant rights.")
      }
    }
    
    if (!is.null(demographics$children)) {
      children <- tolower(demographics$children)
      if (grepl("yes|have", children)) {
        traits <- c(traits, "As a parent, you are particularly concerned about education, family policies, and the future for your children.")
      }
    }
    
    if (!is.null(demographics$urban_rural)) {
      location <- tolower(demographics$urban_rural)
      if (grepl("urban|city", location)) {
        traits <- c(traits, "Living in an urban area, you are concerned about city-specific issues like public transportation and urban development.")
      } else if (grepl("rural|country", location)) {
        traits <- c(traits, "Living in a rural area, you are concerned about rural-specific issues like agricultural policy and rural infrastructure.")
      } else if (grepl("suburban", location)) {
        traits <- c(traits, "Living in a suburban area, you are concerned about suburban-specific issues like school quality and local services.")
      }
    }
  }

  persona <- paste(c(base_persona, traits), collapse = " ")

  # Add general instruction
  persona <- paste(persona,
    "Participate authentically in focus group discussions, sharing opinions that align with your background.",
    "Be engaged but not overly talkative. Respond naturally to other participants and the moderator.")

  return(persona)
}

#' Create agents from haven-coded survey data
#'
#' Builds a set of `FGAgent`s by sampling rows from a haven-coded survey file
#' and turning labeled variables into demographics/survey-responses driven personas.
#' Works with any Stata (.dta), SPSS (.sav), or SAS files that have variable labels.
#'
#' @param n_participants Integer number of participants (excludes the moderator).
#' @param survey_path Character path to survey file (e.g., .dta, .sav files)
#' @param demographic_vars Character vector of variable names to use as demographics.
#'   If NULL, attempts to auto-detect common demographic variables.
#' @param survey_vars Character vector of variable names to use as survey responses.
#'   If NULL, uses remaining non-demographic variables.
#' @param llm_config Optional `LLMR::llm_config` for all agents. If `NULL`, a small OpenAI config is created.
#'
#' @return A list of `FGAgent` objects (participants + moderator).
#' @examples
#' \dontrun{
#' # Use ANES data
#' anes_path <- "path/to/anes_timeseries_2024_stata.dta"
#' agents <- create_agents_from_survey(6, anes_path)
#' 
#' # Specify particular variables
#' agents <- create_agents_from_survey(
#'   n_participants = 4,
#'   survey_path = anes_path,
#'   demographic_vars = c("age", "gender", "education", "race"),
#'   survey_vars = c("party_id", "ideology", "vote_intent")
#' )
#' }
#' @export
create_agents_from_survey <- function(n_participants,
                                      survey_path,
                                      demographic_vars = NULL,
                                      survey_vars = NULL,
                                      llm_config = NULL) {
  if (!file.exists(survey_path)) {
    stop("Survey file not found at: ", survey_path)
  }

  if (is.null(llm_config)) {
    llm_config <- LLMR::llm_config(
      provider = "openai",
      model = "gpt-4.1",
      temperature = 0.7,
      max_tokens = 500
    )
  }
  if (is.null(llm_config)) {
    llm_config <- LLMR::llm_config(
      provider = "openai",
      model = "gpt-4.1",
      temperature = 0.7,
      max_tokens = 500
    )
  }

  # Read survey data by file type
  ext <- tolower(tools::file_ext(survey_path))
  raw_lab <- switch(ext,
    "dta" = haven::read_dta(survey_path),
    "sav" = haven::read_sav(survey_path),
    "sas7bdat" = haven::read_sas(survey_path),
    stop("Unsupported survey file type: .", ext)
  )
  raw <- as.data.frame(lapply(raw_lab, function(col) {
    if (inherits(col, c("haven_labelled","labelled"))) {
      as.character(haven::as_factor(col, levels = "labels"))
    } else {
      col
    }
  }), stringsAsFactors = FALSE)

  # Auto-detect demographic variables if not specified
  if (is.null(demographic_vars)) {
    # For ANES 2024, explicitly map the known variables
    if (grepl("anes.*2024", basename(survey_path), ignore.case = TRUE)) {
      demographic_vars <- c(
        age = "V241457",
        gender = "V241551", 
        education = "V241463",
        race = "V241552",
        marital_status = "V241553",
        income = "V241554",
        religion = "V241555",
        region = "V241556",
        urban_rural = "V241557",
        employment = "V241558",
        union_member = "V241559",
        veteran = "V241560",
        home_owner = "V241561",
        children = "V241562",
        internet_access = "V241563",
        social_media_use = "V241564",
        news_source = "V241565",
        political_interest = "V241566",
        campaign_contact = "V241567",
        volunteer_work = "V241568"
      )
      
      # Also map survey variables for richer personas
      if (is.null(survey_vars)) {
        survey_vars <- c(
          # Political variables
          party_id = "V241227x",           # Party ID summary
          vote_intent = "V241049",         # Harris vs Trump vote intent
          approval_dem = "V241006",        # Approve/disapprove Democratic party
          approval_rep = "V241007",        # Approve/disapprove Republican party
          feeling_dem = "V241166",         # Feeling thermometer: Democratic Party
          feeling_rep = "V241167",         # Feeling thermometer: Republican Party
          ideology = "V241221",            # Liberal-conservative ideology
          congress_approval = "V241127",   # Approval of Congress
          supreme_court_approval = "V241130", # Approval of Supreme Court
          president_approval = "V241134",  # Approval of President
          
          # Economic variables
          economy_rating = "V241236",      # Economy rating
          personal_finances = "V241237",   # Personal financial situation
          tax_policy = "V241238",          # Tax policy preference
          spending_priority = "V241239",   # Government spending priority
          
          # Social policy variables
          abortion_policy = "V241147",     # Abortion policy approval
          immigration_policy = "V241150",  # Immigration policy approval
          gun_policy = "V241153",          # Gun policy approval
          climate_policy = "V241156",      # Climate change policy approval
          health_care_policy = "V241159",  # Health care policy approval
          education_policy = "V241162",    # Education policy approval
          
          # Additional political variables
          biden_dropout = "V241185",       # Biden dropping from race
          trump_conviction = "V241188",    # Trump conviction impact
          election_importance = "V241191", # Election importance
          vote_confidence = "V241194"      # Confidence in vote choice
        )
      }
    } else {
      label_of <- function(v) tryCatch(attr(raw_lab[[v]], "label") %||% "", error = function(...) "")
      detect_by_label <- function(key_patterns) {
        labs <- vapply(names(raw_lab), label_of, "")
        idx <- which(Reduce(`|`, lapply(key_patterns, function(p) grepl(p, labs, ignore.case = TRUE))))
        names(raw_lab)[idx]
      }
      detect_by_name <- function(key_patterns) {
        unique(Reduce(union, lapply(key_patterns, function(p) grep(p, names(raw), ignore.case = TRUE, value = TRUE))))
      }

      vars <- list(
        age = detect_by_label(c("age")),
        gender = detect_by_label(c("sex","gender")),
        education = detect_by_label(c("educ")),
        race = detect_by_label(c("race")),
        income = detect_by_label(c("income"))
      )

      # Fallback to name matching
      if (!length(vars$age)) vars$age <- detect_by_name(c("^age$","age_?[^a-z]*$","vcf.*age","V241457"))
      if (!length(vars$gender)) vars$gender <- detect_by_name(c("gender","sex","vcf.*sex","V241551"))
      if (!length(vars$education)) vars$education <- detect_by_name(c("educ","education","vcf.*educ","V241463"))
      if (!length(vars$race)) vars$race <- detect_by_name(c("race","vcf.*race"))
      if (!length(vars$income)) vars$income <- detect_by_name(c("income","hhinc","vcf.*income"))

      # Take first match for each category
      pick_first <- function(v) if (length(v)) v[[1]] else NA_character_
      demographic_vars <- vapply(vars, pick_first, character(1))
      demographic_vars <- demographic_vars[!is.na(demographic_vars) & nzchar(demographic_vars)]
      
      if (length(demographic_vars) == 0) {
        warning("No demographic variables auto-detected. Using fallback demographics.")
        demographic_vars <- character(0)
      }
    }
  }

  # Auto-detect survey variables if not specified
  if (is.null(survey_vars)) {
    label_of <- function(v) tryCatch(attr(raw_lab[[v]], "label") %||% "", error = function(...) "")
    detect_by_label <- function(key_patterns) {
      labs <- vapply(names(raw_lab), label_of, "")
      idx <- which(Reduce(`|`, lapply(key_patterns, function(p) grepl(p, labs, ignore.case = TRUE))))
      names(raw_lab)[idx]
    }
    detect_by_name <- function(key_patterns) {
      unique(Reduce(union, lapply(key_patterns, function(p) grep(p, names(raw), ignore.case = TRUE, value = TRUE))))
    }

    vars <- list(
      party_id = detect_by_label(c("party")),
      ideology = detect_by_label(c("ideolog")),
      vote_intent = detect_by_label(c("vote","house","congress"))
    )

    # Fallback to name matching
    if (!length(vars$party_id)) vars$party_id <- detect_by_name(c("party","pid","vcf.*party"))
    if (!length(vars$ideology)) vars$ideology <- detect_by_name(c("ideo","ideolog"))
    if (!length(vars$vote_intent)) vars$vote_intent <- detect_by_name(c("vote","house","cong"))

    pick_first <- function(v) if (length(v)) v[[1]] else NA_character_
    survey_vars <- vapply(vars, pick_first, character(1))
    survey_vars <- survey_vars[!is.na(survey_vars) & nzchar(survey_vars)]
  }

  # Build demographic dataframe (preserve canonical names)
  valid_demo <- demographic_vars[demographic_vars %in% names(raw)]
  if (length(valid_demo)) {
    demo_data <- raw[, unname(valid_demo), drop = FALSE]
    names(demo_data) <- names(valid_demo)
    demo_df <- as.data.frame(lapply(demo_data, function(col) {
      if (inherits(col, c("haven_labelled","labelled"))) {
        v <- as.character(haven::as_factor(col, levels = "labels"))
      } else v <- as.character(col)
      v[grepl("inapplicable|missing|refused|don't know", v, ignore.case = TRUE)] <- NA
      v
    }), stringsAsFactors = FALSE)
    demo_df <- demo_df[rowSums(is.na(demo_df)) < ncol(demo_df), , drop = FALSE]
  } else {
    demo_df <- data.frame(
      age = sample(18:80, nrow(raw), replace = TRUE),
      gender = sample(c("Male","Female"), nrow(raw), replace = TRUE),
      education = sample(c("High School","Some College","BA","MA+"), nrow(raw), replace = TRUE),
      stringsAsFactors = FALSE
    )
  }

  # Build survey responses dataframe (preserve canonical names)
  valid_svy <- survey_vars[survey_vars %in% names(raw)]
  survey_df <- if (length(valid_svy)) {
    svy <- raw[, unname(valid_svy), drop = FALSE]
    names(svy) <- names(valid_svy)
    as.data.frame(lapply(svy, function(col) {
      if (inherits(col, c("haven_labelled","labelled"))) {
        v <- as.character(haven::as_factor(col, levels = "labels"))
      } else v <- as.character(col)
      v[grepl("inapplicable|missing|refused|don't know", v, ignore.case = TRUE)] <- NA
      v
    }), stringsAsFactors = FALSE)
  } else NULL

  # Sample rows (respect package seed option)
  seed_val <- getOption("focusgroup.seed", NA_integer_)
  if (!is.na(seed_val)) set.seed(seed_val) else set.seed(NULL)
  take <- seq_len(min(n_participants, nrow(demo_df)))
  if (nrow(demo_df) > n_participants) take <- sample.int(nrow(demo_df), n_participants)
  
  demo_sample <- demo_df[take, , drop = FALSE]
  survey_sample <- if (!is.null(survey_df)) survey_df[take, , drop = FALSE] else NULL

  # Create agents with these demographics
  agents <- create_diverse_agents(
    n_participants = n_participants,
    demographics = demo_sample,
    survey_responses = survey_sample,
    llm_config = llm_config
  )

  agents
}

 
