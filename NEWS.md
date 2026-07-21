# FocusGroup 0.5.1

Initial CRAN release.

* `run_focus_group()` and `fg_quick()` simulate moderated discussions. A
  question script controls opening, icebreaker, engagement, exploration, and
  closing turns. `turns_per_phase` accepts phase counts or exact instructions.
* `create_diverse_agents()`, `create_agents_from_survey()`, and
  `create_agents_from_data()` construct agents from data frames, labeled survey
  files, and pre-rendered persona panels.
* `RoundRobinFlow`, `ProbabilisticFlow`, and `DesireBasedFlow` select speakers
  by order, propensity, or model-produced desire scores.
* The `runner` argument routes moderator, participant, desire-scoring, summary,
  and thematic-analysis calls through a user function. This permits a session
  to run without a provider.
* `focus_group_from_transcript()` imports an existing transcript into a
  `FocusGroup` object for descriptive and text analysis.
* `analyze_focus_group()` runs participation, topic, TF-IDF, readability,
  key-phrase, and thematic analyses. It can also return ggplot2 plots.
* `run_focus_studio()` starts the optional Shiny interface for running sessions,
  reading saved transcripts, and comparing continuations from edited histories.
