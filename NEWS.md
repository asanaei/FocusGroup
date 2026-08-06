# FocusGroup 0.5.2

Corrections replacing 0.5.1, which was never published.

* The untouched default is now runnable: a compact default guide (one
  opening, one icebreaker, two engagement, three exploration, one closing)
  and a default of two responses per question plan 93 model outputs, inside
  the default `max_calls` of 100. The old defaults planned 467 outputs and
  always tripped their own gate.
* Participant construction no longer invents data. A call that supplies
  nothing now samples real respondents from the bundled ANES 2024 records
  (the easy route: their demographic profile rendered with the survey's own
  question wording), where it used to fabricate ages, incomes, and four
  Likert answers from fixed distributions. Direct personas receive no
  synthetic records, and a demographics or survey frame with fewer rows than
  participants is refused instead of recycled (recycled rows are one
  research unit counted several times). `create_agents_from_data()` and
  `create_agents_from_survey()` remain the detailed routes, the first also
  carrying respondents' stated answers.
* Topic modeling treats each utterance as a document rather than pooling
  every turn by speaker. Pooling made each document a person, so the fitted
  topics separated speakers rather than subjects and a six-person group left
  six documents; `unit = "speaker"` restores the old behavior. Fitting few
  documents relative to `num_topics` now warns.
* Survey value cleanup no longer alters substance: a numbered-label prefix
  is stripped only when whitespace follows the period ("1. Male" cleans,
  "1.5" survives), and trailing parentheticals are kept ("Asian
  (non-Hispanic)" stays whole).
* Continuation comparisons no longer inject the end-of-session interim
  summary into the cut histories, which could leak post-cut information
  into both arms; perturbation targets are matched by `message_id`.
* The Studio's saved session is sanitized: no model configurations (a
  config can carry a literal key) and no runner closures leave the session.
  Continuations re-supply the configuration from the sidebar, as before.
* A failed final-summary call warns and returns the completed transcript
  instead of destroying the run.
* An unparseable desire score is `NA`, never a fabricated zero; unscored
  agents are rescored individually, flow parameters are validated, and
  threshold-lowering and highest-below-threshold fallbacks are recorded in
  the selection metadata.
* When a truncated reply is retried above a caller-set token cap, the
  escalation is announced; blank strings count zero words; the participation
  spread is reported without a "balanced" verdict; the analysis print shows
  the fitted topic count; `analyze_focus_group()` gains `speakers`
  (default `"participants"`) and `language` (English-specific methods skip,
  with a note, for other languages).
* Imported transcripts match moderators by exact alias ("mod", "moderator",
  "facilitator", "interviewer"), never by substring, and refuse ambiguity;
  the flat-mode prompt now carries the same truthful-disclosure line as the
  role-flipped mode.
* Removed the unused `default_llmr_config()` and `readr` dependency;
  R floor aligned with LLMR at 4.1.

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
