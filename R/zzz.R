#' FocusGroup: LLM-Powered Focus Group Simulation and Analysis
#'
#' @description
#' The FocusGroup package provides a comprehensive framework for simulating 
#' and analyzing focus group discussions using Large Language Models (LLMs). 
#' It enables researchers to create virtual focus groups with diverse AI agents,
#' conduct structured discussions across multiple phases, and perform 
#' sophisticated analysis of the resulting conversations.
#'
#' @details
#' ## Key Features
#' 
#' * **Agent-Based Modeling**: Create diverse AI participants and moderators 
#'   with customizable personas, demographics, and survey responses
#' * **Flexible Conversation Flow**: Multiple turn-taking mechanisms including
#'   round-robin, probabilistic, and desire-based approaches
#' * **Phase-Driven Simulations**: Structured discussions with Opening,
#'   Icebreaker, Engagement, Exploration, and Closing phases
#' * **Comprehensive Analysis**: Built-in tools for topic modeling, sentiment
#'   analysis, TF-IDF analysis, readability assessment, and thematic analysis
#' * **Three Levels of Control**: From simple wrapper functions to expert-level
#'   R6 class manipulation
#'
#' ## Main Classes
#' 
#' * [FGAgent]: Represents individual participants or moderators
#' * [FocusGroup]: Manages the overall simulation and analysis
#' * [ConversationFlow]: Controls turn-taking mechanisms
#'
#' ## Quick Start
#' 
#' For simple focus group simulation:
#' ```r
#' # Run a basic focus group
#' result <- run_focus_group(
#'   topic = "Social media usage among students",
#'   participants = 6,
#'   turns_per_phase = c(Opening = 2, Icebreaker = 3, 
#'                       Engagement = 8, Exploration = 10, Closing = 2)
#' )
#' 
#' # Analyze results
#' analyze_focus_group(result)
#' ```
#'
#' For advanced control:
#' ```r
#' # Create custom agents
#' agents <- create_diverse_agents(6, demographics_file = "demographics.csv")
#' 
#' # Set up focus group
#' fg <- FocusGroup$new(
#'   agents = agents,
#'   topic = "Climate change attitudes",
#'   conversation_flow = DesireBasedFlow$new()
#' )
#' 
#' # Run simulation
#' fg$run_simulation(turns_per_phase = list(Engagement = 15, Exploration = 20))
#' 
#' # Perform analysis
#' topics <- fg$analyze_topics(num_topics = 5)
#' sentiment <- fg$analyze_sentiment()
#' ```
#'
#' @section Dependencies:
#' This package requires several key dependencies:
#' * R6: For object-oriented programming
#' * LLMR: For LLM integration and API calls
#' * dplyr, tidyr: For data manipulation
#' * ggplot2: For visualization
#' * quanteda, topicmodels, tidytext: For text analysis
#' * jsonlite: For JSON processing
#'
#' @section Configuration:
#' Before using the package, ensure you have:
#' 1. Configured LLM API credentials (OpenAI, Anthropic, etc.)
#' 2. Set appropriate model parameters for your use case
#' 3. Prepared any custom demographic or survey data files
#'
#' @author Ali Sanaei \email{sanaei@@uchicago.edu}
#' @keywords package focus-group llm simulation qualitative-research
"_PACKAGE"

NULL

NULL 