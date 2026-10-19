# FocusGroup 0.5.3

## Participants

* `create_agents_from_survey()` applies `rows` to the rows of the file, so
  `rows = c(2, 3)` returns the respondents in file rows 2 and 3, and a logical
  selector or predicate marks file rows. The selector used to be applied after
  respondents without any demographic value had been dropped, so when such a
  respondent came before the selected rows, integer and logical selectors
  returned respondents further down the file (in a file whose first respondent
  gave no demographic values, `rows = c(2, 3)` returned file rows 3 and 4). A
  predicate now also sees the respondents without demographic values; where it
  gives `NA` for them, they count as not selected. A message says how many
  respondents were left out for lack of demographic values. When `rows`
  selects only such respondents, the call stops with "`rows` selected no
  respondents with demographic values."; when the file has none with a
  demographic value, it stops with "No respondent in the file has a value for
  any of `demographic_vars`." Such a file used to give participants with
  generic personas.

* A sampling-weight column named in `weights` is no longer rendered into
  personas by `create_agents_from_survey()` or `create_agents_from_data()`,
  and `create_agents_from_survey()` leaves it out even when `demographic_vars`
  or `survey_vars` name it. Personas used to state the weight as one of the
  respondent's answers ("Question: Full sample weight Answer: 2.9"), so the
  persona text of every weighted call that rendered the default columns
  changes. When the weight column is the only demographic variable named,
  `create_agents_from_survey()` stops with "No usable `demographic_vars`
  remain after excluding the sampling-weight column."

* `create_agents_from_data()` and `create_agents_from_survey()` refuse
  sampling weights they used to repair without notice. Numeric and
  haven-labelled weights draw the same respondents as before. These inputs are
  now errors:
  - a weight column that is not in the data ("Weight column 'wt' is not
    present in the input data."); the draw used to be unweighted;
  - weights stored as a factor, text, logical values, or a matrix ("... must
    be an integer or double vector ..."); a factor used to be sampled by its
    level codes, numeric text and logical values were converted to numbers,
    and other text gave an unweighted draw;
  - weights whose length differs from the number of rows ("... has length 6;
    expected one weight for each of the 4 respondents."); surplus weights used
    to be dropped and missing ones left respondents undrawable;
  - missing, negative, or infinite weights ("... contains missing, negative,
    or nonfinite weights at source row(s) ..."); missing and negative weights
    used to count as 0, and an infinite weight made the draw fail with a base
    R error;
  - weights that are all zero ("... must contain at least one positive
    weight."), or all zero among the respondents `rows` leaves eligible ("...
    has no positive weight among the eligible respondents."); the draw used to
    fall back to equal probabilities.

  A draw with fewer positive weights than participants to fill now stops with
  "A draw of 3 respondents requires at least 3 eligible respondents with
  positive weight; found 2." instead of the base R error "too few positive
  probabilities". When exactly `n_participants` respondents are eligible, all
  of them are used whatever their weights, as before.

* `rows` is validated in the same way by both functions. Positions that are
  fractional, missing, or out of range stop with "`rows` must be whole numbers
  between 1 and the number of respondents ..."; both functions used to
  truncate them (1.9 selected row 1), drop them, or fail with an unrelated
  error. A logical selector or a predicate's result must have one logical
  value per respondent ("... must have one logical value per respondent
  ..."); a vector of the wrong length used to be used as far as it went. `NA`
  in such a vector counts as not selected, as before. A position given twice
  selects its respondent once, where either function could return one
  respondent as two participants, and asking for more participants than
  there are distinct eligible respondents stops with "4 participants were
  requested, but only 3 distinct respondents are eligible."

* Rosters returned by `create_agents_from_data()` and
  `create_agents_from_survey()`, and by `create_agents()` when it draws from
  the bundled ANES records, carry a `source_rows` attribute giving the row
  each participant was drawn from, and a `source_ids` attribute with the
  data's `id`, `respondent_id`, or `persona_id` columns when it has any. The
  rosters used to carry no record of the rows drawn.

## Sessions

* Sessions run without an explicit `max_participant_responses` are shorter.
  The option `focusgroup.max_participant_responses` is now set to 2 when the
  package loads, as the `run_focus_group()` help said; it was set to 3, so
  that documented default never applied, and `FocusGroup$new()` also fell
  back to 3. With the
  default guide and flow the estimate falls from 135 to 93 model outputs, and
  `run_focus_group(topic, config)` no longer stops at the default `max_calls`
  of 100. A call that raised `max_calls` or set `confirm = TRUE` now gets two
  participant responses per question instead of three; pass
  `max_participant_responses = 3`, or set
  `options(focusgroup.max_participant_responses = 3)`, to restore them.

* `run_focus_group()` refuses a `max_calls` that is infinite or beyond the
  integer range ("`max_calls` must be one finite positive integer."). `Inf`
  used to switch the limit off; `confirm = TRUE` still does. A guide whose
  estimate exceeds the integer range stops with "The model-output estimate
  (...) exceeds the integer range ..." before the script is built, and an
  invalid guide count is reported with its value as well as its phase.

* When a model call fails partway through a session, `run_focus_group()`
  raises an error of class `focusgroup_incomplete_session` instead of passing
  the bare error on, and the completed turns are no longer lost. The error's
  `partial` field holds the `focus_group_result` of the turns completed so
  far, its `phase` field the script phase that was running (or
  `"interim_summary"`), and its `parent` field the original error. Results
  gain `metadata$complete` and `metadata$failure`, and printing a
  `FocusGroup` from a session reports whether it completed. A failed final
  summary still warns and returns the full transcript.

* The `timestamp` column of a result's transcript is in UTC; it used to be
  shown in the session's time zone. The instants are unchanged.

## Usage

* Token usage that a provider or runner did not report is recorded as
  unknown instead of 0. A message's `sent_tokens`, `rec_tokens`, and
  `total_tokens` are `NA` when a call behind the message reported none; they
  used to read 0. `run_focus_group()` results gain `usage$unknown`, the number
  of calls whose usage was incomplete, and `sent`, `rec`, and `total` are
  lower bounds when it is positive. `usage$total` adds each call's reported
  total, or its sent and received counts when it reported no total; it used
  to be `sent + rec`, which leaves out tokens a provider counts only in its
  total. The printed token count says "at least" or "unknown" when usage is
  incomplete.

* Group usage now includes interim summaries, which used to be left out, and
  calls whose output is rejected, such as an unsuccessful runner row or a
  first reply whose retry fails; their usage used to be dropped. A reported
  count that is not a nonnegative whole number is treated as unknown with the
  warning "Token count `sent` is not a finite nonnegative whole number; it is
  treated as unknown." A response whose usage cannot be read at all warns
  "Token usage could not be read from the response; its counts are treated as
  unknown.", and a total beyond the numeric range warns "Accumulated token
  usage exceeds the numeric range; completeness is unavailable."

* Desire scores from a broadcast call are matched to agents by `agent_id` or
  `.experiment_id` when the reply carries them, and by position otherwise.
  When the reply cannot be matched to the agents scored, the warning
  "Broadcast usage could not be matched to individual agent calls." is given
  and those agents are scored again one at a time, which costs one call each.
  Such a reply used to be read by position, and its usage was split evenly
  among the agents.

* An unsuccessful runner row (`success = FALSE`) raises an error of class
  `focusgroup_model_error` whose message is the row's `error_message`, or its
  `error`; only `error` used to be read.

* `FGAgent` objects gain the fields `tokens_total_agent` and
  `usage_unknown_agent`, and `FocusGroup` objects gain `total_tokens` and
  `calls_usage_unknown`. Objects saved by earlier versions remain readable.

## Imported transcripts

* `focus_group_from_transcript()` says when no speaker id is a moderator alias
  and every speaker is therefore analyzed as a participant; it used to add the
  placeholder moderator without saying so. The help, README, and vignettes now
  describe the exact-alias rule; they still described an earlier substring
  match.

* `focus_group_from_transcript()` keeps the fields a transcript supplies:
  `message_id`, `round`, `is_moderator`, `phase`, `response_id`,
  `finish_reason`, the three token counts, `duration_s`, `provider`, `model`,
  `timestamp`, and `metadata`. It used to keep only the speaker and the text,
  setting every phase to "imported", the timestamp, provider, and model to
  `NA`, and the rounds from moderator turns, which left every round 0 when no
  moderator was found. Token counts the transcript does not supply are `NA`;
  they used to be 0.

* Missing text among the imported turns becomes an empty utterance with the
  warning "1 utterance(s) with missing text were imported as empty text."; the
  conversion used to be silent. A supplied field that cannot be converted is
  an error naming the column and rows ("Column 'round' contains invalid values
  at source row(s) 4."). Timestamps are read in UTC, and timestamps that
  cannot be parsed become `NA` with a warning giving their number.

* A supplied `is_moderator` column now decides who the moderator is, and the
  aliases are consulted only without one; the column used to be ignored, so a
  speaker named "Moderator" was the moderator even where the column said
  otherwise. A column that gives a speaker two roles, marks more than one
  speaker, or disagrees with `moderator_id` is an error, and so is naming
  "System" as the moderator ("`moderator_id` cannot be 'System'.", "'System'
  cannot be the moderator."), which used to add a placeholder. When the
  column marks no speaker, the placeholder message gives that as the reason.
  If a participant is already named "MOD", the placeholder moderator is named
  "MOD_2", or the next free suffix.

* The `FocusGroup` methods `plot_message_length_timeline()` and
  `plot_participation_by_agent()` count words with the rule the analysis
  tables use, so a blank utterance has no words and surrounding whitespace
  adds none. They used to split the raw text, which for imported text with
  leading whitespace or blank text counted one word more per affected message
  than the tables did (" hello  world " counted 3 words, and "   " counted
  1).

## Studio

* The Studio requires LLMR.shiny 0.1.3 or later and stops with "FocusGroup
  Studio requires LLMR.shiny >= 0.1.3; installed version is ..." when an
  older version is installed. FocusGroup 0.5.2 suggested 0.1.2 and did not
  check the installed version.

* The Studio's usage tile counts the calls each action made and their
  reported tokens, shown as lower bounds when a call reported none, and it
  records the calls an action made before failing. It used to record the
  planned number of calls for a continuation experiment, two calls with 0
  tokens for model analysis, and nothing for a run that failed, and it showed
  exact counts when a reply reported no usage.

* A Studio run that fails partway keeps its partial transcript for analysis
  and download, with a notice naming the phase where it stopped; it used to
  show only the error. Saved sessions keep the completion status and the
  roster's `source_rows` and `source_ids`. A continuation from a session
  saved by an earlier version rebuilds its agents with the current class,
  keeping their saved persona and style.

* The Studio's participant-source choices are labeled "ANES demographics"
  and "ANES respondent fields". They were labeled "Synthetic personas" and
  "ANES 2024 personas", although both draw respondents from the bundled ANES
  records.

* The Studio's Analyze tab shows the transcript as imported by
  `focus_group_from_transcript()`, one row per kept turn, and shows the
  importer's warnings as notifications. It used to show the uploaded rows,
  and restored the exported fields only when no row had been dropped.

* The Studio's Analyze tab reads an uploaded transcript CSV with every value
  as written, and gives the columns other than the speaker, the text, and
  `response_id` their usual types only after the columns are mapped. Type
  inference used to read the speaker ids 001 and 01 as the same number 1,
  alter long numeric ids such as 12345678901234567890, and turn the text "NA"
  into a missing utterance.

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
* Survey value cleanup strips a numbered-label prefix only when whitespace
  follows the period, so "1. Male" cleans and "1.5" survives. Trailing
  parentheticals are kept: "Asian (non-Hispanic)" stays whole.
* Continuation comparisons keep the end-of-session interim summary out of
  the cut histories, which could otherwise carry post-cut information into
  both arms. Perturbation targets are matched by `message_id`.
* The Studio's saved session is sanitized: no model configurations (a
  config can carry a literal key) and no runner closures leave the session.
  Continuations re-supply the configuration from the sidebar, as before.
* A failed final-summary call warns and returns the completed transcript
  instead of destroying the run.
* An unparseable desire score is `NA`, where it used to become a zero that
  scored as real evidence. Unscored agents are rescored individually, flow
  parameters are validated, and the selection metadata records
  threshold-lowering and highest-below-threshold fallbacks.
* A truncated reply retried above a caller-set token cap announces the
  escalation.
* Blank strings count zero words.
* Participation is reported as a spread, without a "balanced" verdict.
* The analysis print shows the fitted topic count.
* `analyze_focus_group()` gains `speakers` (default `"participants"`, which
  keeps the moderator out of the text statistics) and `language`
  (English-specific methods skip, with a note, for other languages).
* Imported transcripts match moderators by exact alias ("mod", "moderator",
  "facilitator", "interviewer") and refuse ambiguity. A substring match used
  to make "Modesto" a moderator.
* The flat-mode prompt carries the same truthful-disclosure line as the
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
