# Live integration demo for FocusGroup: a small multi-agent focus-group
# simulation run against a real (cheap) model. Lives under inst/integration so
# R CMD check never runs it; you are billed only when you run it yourself. It
# exercises the full simulation path:
#   run_focus_group() -> create_agents() + a conversation flow + the moderator
#   guide, followed by offline descriptive analysis.
#
# The topic ("public libraries") is neutral and authored for this demo (no
# copyrighted text). Kept deliberately small: 3 participants, one utterance per
# participant per question, to bound the number of live calls.
#
# Run with a key in the environment, e.g.:
#   GROQ_API_KEY=... Rscript inst/integration/demo-focusgroup.R

run_focusgroup_demo <- function(provider = Sys.getenv("LLMR_DEMO_PROVIDER", "groq"),
                                model = Sys.getenv("LLMR_DEMO_MODEL", "openai/gpt-oss-20b"),
                                n_participants = 3L) {
  stopifnot(requireNamespace("FocusGroup", quietly = TRUE))
  library(FocusGroup)

  cfg <- LLMR::llm_config(provider, model, temperature = 0.7, max_tokens = 120)

  res <- run_focus_group(
    topic = "public libraries",
    config = cfg,
    n_participants = n_participants,
    guide = list(
      Opening = "Welcome the participants and state the ground rules.",
      Exploration = "Which public library priority matters most, and why?",
      Closing = "Thank the participants and close the discussion."
    ),
    flow = "round_robin",
    seed = 110,
    verbose = FALSE,
    max_participant_responses = 1L
  )

  analysis <- analyze_focus_group(res, include_plots = FALSE)
  list(result = res, analysis = analysis, config = cfg)
}

if (sys.nframe() == 0L) {
  res <- run_focusgroup_demo()
  cat("\n==== FocusGroup transcript (message | speaker | text) ====\n")
  tr <- res$result$transcript
  if (nrow(tr)) {
    for (i in seq_len(nrow(tr))) {
      cat(sprintf("[%s] %s: %s\n", tr$message_id[i], tr$speaker_id[i],
                  substr(tr$text[i], 1, 160)))
    }
  }
  cat("\n==== usage ====\n")
  print(res$result$usage)
  cat("\n==== basic stats ====\n")
  print(res$analysis$basic_stats)
}
