# Example: run focus groups from the bundled ANES-derived personas.
#
# This uses the shipped `anes_2024_personas` data frame (100 real ANES 2024
# respondents, diversity-sampled, demographics coarsened, attitudes intact). No
# external file is needed, and no ANES variable codes are hard-coded: the persona
# fields and their question wording travel with the data. To run against a raw
# survey file instead, see create_agents_from_survey().

library(FocusGroup)
data(anes_2024_personas)

cfg <- LLMR::llm_config("openai", "gpt-4o-mini")   # any LLMR provider works

# --- Focus group 1: a cross-section of the personas -------------------------
agents1 <- create_agents_from_data(
  anes_2024_personas,
  n_participants = 6,
  llm_config = cfg
)
agents1 <- stats::setNames(agents1, vapply(agents1, function(a) a$id, ""))
flow1 <- create_conversation_flow("desire_based", agents1, "MOD")

fg1 <- FocusGroup$new(
  topic = "What the country should prioritize over the next year",
  purpose = "Surface the range of priorities and where people agree or differ",
  agents = agents1,
  moderator_id = "MOD",
  turn_taking_flow = flow1
)
fg1$run_simulation(verbose = TRUE)
res1 <- list(transcript = do.call(rbind, lapply(fg1$conversation_log, as.data.frame)),
             summary = fg1$summarize(summary_level = 1))

# --- Focus group 2: restrict the pool with the `rows` argument --------------
# e.g. only respondents who pay close attention to politics.
agents2 <- create_agents_from_data(
  anes_2024_personas,
  n_participants = 6,
  rows = function(df) df[["attention to politics"]] %in% c("Always", "Most of the time"),
  llm_config = cfg
)
agents2 <- stats::setNames(agents2, vapply(agents2, function(a) a$id, ""))
flow2 <- create_conversation_flow("desire_based", agents2, "MOD")
fg2 <- FocusGroup$new(
  topic = "Trust in government and institutions",
  purpose = "Explore how engaged citizens talk about institutional trust",
  agents = agents2,
  moderator_id = "MOD",
  turn_taking_flow = flow2
)
fg2$run_simulation(verbose = TRUE)
res2 <- list(transcript = do.call(rbind, lapply(fg2$conversation_log, as.data.frame)),
             summary = fg2$summarize(summary_level = 1))
