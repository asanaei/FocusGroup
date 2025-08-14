# Example scripts to run two focus groups using ANES 2024 personas

# Load package from source (for development)
if (!requireNamespace("FocusGroup", quietly = TRUE)) {
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".")
} else {
  suppressPackageStartupMessages(library(FocusGroup))
}

anes_path <- system.file("examples/anes_2024/anes_timeseries_2024_stata.dta", package = "FocusGroup")

# Source the ANES helpers
source("anes_helpers.R")

# Focus group 1: Strategies for Democrats to win back Congress in 2026 (general voters)
agents1 <- create_agents_from_anes(
  n_participants = 6,
  anes_dta_path = anes_path
)

mod_id1 <- agents1[[length(agents1)]]$id
agents1_named <- setNames(agents1, vapply(agents1, function(a) a$id, ""))
flow1 <- create_conversation_flow("desire_based", agents1_named, mod_id1)

fg1 <- FocusGroup$new(
  topic = "Best practices Democrats can employ to win back Congress in 2026",
  purpose = "Identify messaging, policy, and mobilization opportunities among likely voters",
  agents = agents1_named,
  moderator_id = mod_id1,
  turn_taking_flow = flow1
)
fg1$run_simulation(verbose = TRUE)
res1 <- list(
  transcript = do.call(rbind, lapply(fg1$conversation_log, as.data.frame)),
  summary = fg1$summarize(summary_level = 1)
)

# Focus group 2: Independent-leaning voters only
agents2 <- create_agents_from_anes(
  n_participants = 6,
  anes_dta_path = anes_path
)
mod_id2 <- agents2[[length(agents2)]]$id
agents2_named <- setNames(agents2, vapply(agents2, function(a) a$id, ""))
flow2 <- create_conversation_flow("desire_based", agents2_named, mod_id2)

fg2 <- FocusGroup$new(
  topic = "Best practices Democrats can employ to win back Congress in 2026",
  purpose = "Probe persuasion tactics among independents",
  agents = agents2_named,
  moderator_id = mod_id2,
  turn_taking_flow = flow2
)
fg2$run_simulation(verbose = TRUE)
res2 <- list(
  transcript = do.call(rbind, lapply(fg2$conversation_log, as.data.frame)),
  summary = fg2$summarize(summary_level = 1)
)

print(res1$summary)
print(res2$summary)


