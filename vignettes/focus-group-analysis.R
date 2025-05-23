## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(FocusGroup)

## ----install, eval = FALSE----------------------------------------------------
# # Install from GitHub
# remotes::install_github("yourusername/FocusGroup")

## ----create_fg, eval = FALSE--------------------------------------------------
# # Set your OpenAI API key (or set it in your environment)
# Sys.setenv(OPENAI_API_KEY = "your-api-key-here")
# 
# # Run a focus group with 5 participants
# result <- run_focus_group(
#   topic = "Impact of Social Media on Mental Health",
#   participants = 5
# )
# 
# # View the focus group configuration
# print(result$focus_group)

## ----view_conversation, eval = FALSE------------------------------------------
# # View the conversation
# head(result$conversation)

## ----basic_analysis, eval = FALSE---------------------------------------------
# # Get basic statistics
# stats <- analyze_focus_group(result)
# print(stats$summary)

## ----text_analysis, eval = FALSE----------------------------------------------
# # Analyze text complexity
# complexity <- analyze_text_complexity(result)
# print(complexity$readability)
# 
# # Analyze response patterns
# patterns <- analyze_response_patterns(result)
# print(patterns$turn_patterns)

## ----questions, eval = FALSE--------------------------------------------------
# # Analyze question patterns
# questions <- analyze_question_patterns(result)
# print(questions$question_patterns)

## ----plots, eval = FALSE------------------------------------------------------
# # Create participation timeline
# participation_plot <- create_participation_timeline(result)
# print(participation_plot)
# 
# # Create sentiment timeline
# sentiment_plot <- create_sentiment_timeline(result)
# print(sentiment_plot)
# 
# # Create readability by phase
# readability_plot <- create_readability_by_phase(result)
# print(readability_plot)

## ----custom_agents, eval = FALSE----------------------------------------------
# # Set your OpenAI API key (or set it in your environment)
# Sys.setenv(OPENAI_API_KEY = "your-api-key-here")
# 
# # Create diverse agents
# agents <- create_diverse_agents(
#   n_participants = 3,
#   demographics = list(
#     age = c(25, 35, 45),
#     gender = c("male", "female", "non-binary"),
#     education = c("high school", "bachelor's", "master's")
#   )
# )
# 
# # Name the agents by their IDs
# agents_named <- setNames(agents, sapply(agents, function(a) a$id))
# 
# # Find moderator ID
# moderator_id <- agents_named[[1]]$id
# 
# # Create a conversation flow object
# flow_obj <- DesireBasedFlow$new(agents_named, moderator_id)
# 
# # Create the FocusGroup object
# fg <- FocusGroup$new(
#   topic = "Impact of Social Media on Mental Health",
#   purpose = "To explore perspectives and experiences related to social media.",
#   agents = agents_named,
#   moderator_id = moderator_id,
#   turn_taking_flow = flow_obj
# )

## ----custom_analysis, eval = FALSE--------------------------------------------
# # Extract conversation data
# conv_data <- result$conversation
# 
# # Custom analysis example
# word_counts <- sapply(conv_data$message, function(x) length(strsplit(x, "\\s+")[[1]]))
# summary(word_counts)

