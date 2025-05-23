#' @importFrom stats median sd
#' @importFrom utils head
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
        message = x$text,
        phase = x$phase,
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
        demographics = if (exists("demographics", where = agent)) agent$demographics else NULL,
        survey_responses = if (exists("survey_responses", where = agent)) agent$survey_responses else NULL
      )
    })
  )

  if (verbose) cat("Simulation complete!\n")

  return(result)
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
    # Need to create proper llm_config object using LLMR
    if (requireNamespace("LLMR", quietly = TRUE)) {
      llm_config <- LLMR::llm_config(
        provider = "openai",
        model = "gpt-4o-mini",
        api_key = Sys.getenv("OPENAI_API_KEY"),
        model_params = list(temperature = 0.7, max_tokens = 500)
      )
    } else {
      stop("LLMR package is required to create agents")
    }
  }

  agents <- list()

  # Create participants
  for (i in 1:n_participants) {
    agent_demographics <- if (nrow(demographics) >= i) {
      as.list(demographics[i, ])
    } else {
      list(age = sample(18:65, 1), gender = sample(c("Male", "Female", "Female"), 1))
    }

    agent_survey <- if (!is.null(survey_responses) && nrow(survey_responses) >= i) {
      as.list(survey_responses[i, ])
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
  cat("Analyzing sentiment...\n")
  sentiment <- tryCatch(
    fg$analyze_sentiment(),
    error = function(e) {
      cat("Sentiment analysis failed:", e$message, "\n")
      NULL
    }
  )

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
  ages <- sample(18:65, n, replace = TRUE)
  genders <- sample(c("Male", "Female", "Female"), n, replace = TRUE,
                   prob = c(0.45, 0.45, 0.1))

  education_levels <- c("High School", "Some College", "Bachelor's", "Master's", "PhD")
  education <- sample(education_levels, n, replace = TRUE,
                     prob = c(0.2, 0.15, 0.35, 0.25, 0.05))

  income_levels <- c("Under $30k", "$30k-$50k", "$50k-$75k", "$75k-$100k", "Over $100k")
  income <- sample(income_levels, n, replace = TRUE,
                  prob = c(0.15, 0.25, 0.25, 0.20, 0.15))

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
  age <- demographics$age %||% "unknown age"
  gender <- demographics$gender %||% "unspecified gender"
  education <- demographics$education %||% "unspecified education"

  base_persona <- sprintf(
    "You are a %s-year-old %s with %s education.",
    age, tolower(gender), tolower(education)
  )

  # Add personality traits based on demographics and survey
  traits <- c()

  if (!is.null(survey_responses)) {
    if (!is.null(survey_responses$tech_usage_comfort)) {
      if (survey_responses$tech_usage_comfort >= 4) {
        traits <- c(traits, "You are comfortable with technology and use it frequently.")
      } else if (survey_responses$tech_usage_comfort <= 2) {
        traits <- c(traits, "You prefer traditional methods and are cautious about new technology.")
      }
    }

    if (!is.null(survey_responses$environmental_concern)) {
      if (survey_responses$environmental_concern >= 4) {
        traits <- c(traits, "You are environmentally conscious and concerned about climate change.")
      }
    }
  }

  # Add age-based traits
  if (is.numeric(age)) {
    if (age < 25) {
      traits <- c(traits, "You represent a younger perspective and are digitally native.")
    } else if (age > 50) {
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

  persona <- paste(c(base_persona, traits), collapse = " ")

  # Add general instruction
  persona <- paste(persona,
    "Participate authentically in focus group discussions, sharing opinions that align with your background.",
    "Be engaged but not overly talkative. Respond naturally to other participants and the moderator.")

  return(persona)
}

#' Null-coalescing operator
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
