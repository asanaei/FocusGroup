# FocusGroup 0.5.1

Initial CRAN release.

* Simulates focus group discussions among LLM agents: a moderator guides
  participants through scripted phases (opening, icebreaker, engagement,
  exploration, closing), with numeric phase script banks or user-supplied scripts.
* Participant personas come from user-supplied demographics and survey
  responses, from labeled survey files (Stata, SPSS, SAS), or from a data
  frame such as `LLMR::anes_2024_personas`; pre-rendered silicon panel personas
  pass through without being recast as survey answers.
* Three turn-taking flows: round robin, probabilistic, and desire-based
  (participants rate their own desire to speak).
* An optional runner supports complete offline sessions, including desire
  scoring, moderator turns, participant turns, summaries, and thematic analysis.
* `focus_group_from_transcript()` imports an existing human or saved transcript
  into a `FocusGroup` object for descriptive and text analysis.
* Analysis suite for the resulting transcripts: participation and balance
  statistics, topic modeling, TF-IDF, readability, key phrases, and
  LLM-assisted thematic analysis, with optional ggplot2 visualizations.
* Optional Shiny GUI (`run_focus_studio()`, via the suggested LLMR.shiny
  package) to run a group live, analyze a transcript offline, and run paired
  continuation experiments on a saved conversation.
