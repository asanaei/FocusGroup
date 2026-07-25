# FocusGroup 0.5.1

Initial CRAN release.

* `run_focus_group()` is the single high-level session runner. It requires an
  explicit `config`, accepts `n_participants`, `guide`, `flow`, and
  `message_mode`, and performs a model-call cost preflight before live work.
* `run_focus_group()` returns a `focus_group_result` with the stable fields
  `focus_group`, `transcript`, `summary`, `participants`, `usage`, and
  `metadata`. Its print method gives a concise run summary.
* `create_agents()`, `create_agents_from_data()`, and
  `create_agents_from_survey()` construct agent-ID-keyed rosters from supplied
  personas, in-memory records, labeled survey files, and pre-rendered persona
  panels.
* `ConversationFlow` is the public extension base. Built-in round-robin,
  probabilistic, and desire-based flows are constructed with
  `create_conversation_flow()`.
* Model configuration arguments use `config`; group-level configuration uses
  `admin_config`. The `FGAgent` configuration field is `config`.
* The `.runner` seam accepts an experiments data frame with `config` and
  `messages` list-columns and returns those rows with at least
  `response_text`.
* Session logs give every row a unique integer `message_id` and record the
  moderator cycle as `round`, while retaining `phase`.
* `analyze_focus_group()` returns a `focus_group_analysis` with stable fields,
  typed empty components, and an `issues` table. Descriptive analysis runs
  offline. Thematic analysis and model summaries require an explicit `config`.
* `focus_group_from_transcript()` accepts `moderator_id`; when it is omitted,
  speaker identifiers containing `"mod"` provide the fallback.
* `get_default_prompt_templates()` contains only templates used by supported
  package operations.
* `run_focus_studio()` starts the optional Shiny interface for running
  sessions, reading transcripts, and comparing continuations from edited
  histories.
* The studio now exports both complete `.rds` sessions and transcript-only
  CSV files, and a finished run can be passed directly to the Analyze tab.
* ANES participant selection now includes full rendered-persona inspection and
  control over which respondent columns enter each persona.
* The Analyze tab now presents `analyze_focus_group()` components, optional
  model themes and summaries, package guidance, source provenance, and timing.
* Transcript mapping now chooses text and speaker defaults by column name and
  keeps the two roles distinct when separate columns are available.
* Continuation experiments now support three labeled treatment arms, repeated
  runs per arm, true planned-call counts, retained responses, summaries, and
  response-length comparisons.
* Studio run, analysis, model-analysis, and continuation actions are disabled
  until their known inputs are ready, with the unmet requirement shown beside
  each disabled action.
* Studio live paths honor the shared temperature, maximum output token, and
  reasoning-effort settings; long text results use scrollable output blocks.
* The Run tab now provides a topic-aware, line-based editor for all five
  moderator-guide phases. Included phases, question counts, call estimates,
  and both participant-source paths use the edited guide.
* Run configuration is grouped in collapsible sections while the run action
  remains outside the sections.
* Transcript tables give text most of the available width, wrap complete
  words, and retain hidden identifiers in a column-visibility menu. Numeric
  tables use display-only rounding from `LLMR.shiny`.
* TF-IDF results now show a user-selected number of ranked terms per speaker
  in a table and, when ggplot2 is installed, a faceted bar chart.
