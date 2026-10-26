# FocusGroup 0.5.6

## The bundled ANES personas

* FocusGroup now ships `anes_2024_personas`, rebuilt from the ANES 2024 Time
  Series Full Release of May 19, 2026, and `create_agents()` and Focus Studio
  draw from it instead of LLMR's copy. The earlier build turned every
  unlabeled code into a missing value, so the six seven-point scales that
  label only their endpoints lost their middle answers; it kept four
  immigration-policy labels cut at 80 characters, put spouses' answers into
  two respondents' fields, dropped renters and people who never attend
  religious services through skip patterns, and ordered answer categories
  alphabetically when it chose the respondents and fit the ideology score.
  The rebuilt data write every answer of those scales as "5 on a scale from 1
  (...) to 7 (...)" and every thermometer as "70 on a 0-100 feeling
  thermometer", give full labels from the codebook, replace items not in this
  release (paper-only and GSS items) with all-mode items or drop them, and
  carry verbatim question wording in the dictionary. Only 26 of the earlier
  100 respondents remain, so results built on the old data will differ.
  LLMRpanel 0.6.3 ships an identical copy. The data remain a diversity
  sample, not representative, and its selection favors atypical respondents;
  the help page gives figures.

## Personas from data

* `create_agents_from_data()` renders columns with value labels by the rules
  of `create_agents_from_survey()` (labels, the scale sentence, missing codes,
  numeric fields, warnings), so a labelled frame and the same file give the
  same personas, and gains `na_strings` and `scale_template`. Missing-data
  labels such as "-9. Refused" in text and factor columns, and SPSS
  user-defined missing values, are left out with the reason in
  `persona_rendering_report()` and a message. `create_agents()` screens the
  frames it is given the same way and gains `na_strings`; it no longer fails
  on factor columns. It stops on columns with value labels, which it would
  show as bare codes ("party: -9"), and names `create_agents_from_data()`.

* One rule now decides whether a value label or a text value is a
  missing-data label: it matches `na_strings` and either carries a negative
  code or one of 90 and above, as the labeled code or a leading code in the
  forms "-9. ", "-9 ", "[-9] ", "(-9) ", "-9: ", and "99 - ", or says
  nothing but the matched wording once interviewer markers such as "{VOL}"
  and "(VOL)", asides in parentheses or brackets, and filler words are set
  aside. So "[-9] Refused" from `haven::as_factor(levels = "both")`, "-9
  Refused", "Don't know/Refused (VOL.)", and "Not asked in survey" are left
  out, where 0.5.5 showed them as answers, while "2. Refused to take sides",
  "5. Missing the old system", and "I keep missing the bus" are kept. A value
  label of a code from 0 to 89 that uses such words in an answer is now
  shown; it used to be left out.

* A subset of `anes_2024_personas` taken with `[` or `subset()`, which drop
  its `dictionary`, has the dictionary and demographic fields of its columns
  restored by `create_agents_from_data()` from the package's copy, with a
  message, so its answers are shown under their questions rather than the
  column handles.

* Identifier columns are recognized in any case and in common variants (ID,
  CaseID, case_id, respondent_id, respid, resp_id, persona_id, uid,
  record_id, row_id, ResponseId, participant_id, subject_id, user_id,
  PROLIFIC_PID, workerId, caseno, QKEY) and are never rendered into
  personas. A column named `pid`, which in survey data usually holds party
  identification, is rendered.

## Session files and continuation

* `format()` of a `focus_group_result` returns the lines of a session file,
  `writeLines(format(result), "session.fg.jsonl")`: one JSON object per line,
  one row of the record each, with no configuration, key, or runner; key-like
  values in the messages of failed calls, in the forms of the common
  providers' keys, headers, and JSON fields, are written as `<redacted>`.
  `focus_group_from_transcript()` reads such a file, a vector of its lines, a
  result, or a FocusGroup back into a new session, reports every problem it
  finds at once in an error of class `focusgroup_format_error`, and gains
  `personas`. `inst/fg-session-spec.md` describes the format for converting a
  raw transcript with a language model. Data-frame input works as before.
  `format()` of a FocusGroup, such as one read from a CSV transcript, gives
  its session file in the same way; `format()` of a `focus_group_arms`
  object stops and says to format one arm or the prefix.
  Fields of the metadata and of guide items that the format does not define,
  such as `metadata$condition` or an item's `label`, are written with the
  prefix `x_` and read back under their names.

* `run_focus_group_arms()` continues a recorded session when its first
  argument is a FocusGroup or a result: each arm starts from the record's
  state just before `fg_before_turn(t)`, rebuilt from the record (messages,
  guide item, turns taken, the turn-taking rule's state, the interim summary
  in force). Settings the record fixes cannot be passed again. A probe at
  the branch point is asked once before the arms, which share it. A transcript
  without moderator lines, which has no guide items to continue, stops with
  an error that says so and how to name or add the moderator.

* An injection from a participant's ID or handle at `fg_before_turn(t)` is
  that participant's turn t, recorded with the routing rule `"injected"`; it
  takes the place of the turn the session was about to give, recorded in
  `injections$replaced`. At a call-on turn, a line from someone other than
  the participant called on takes no one's place, and the participant called
  on still answers. Outside voices named like "P4" are refused, and so is a
  `from` that is one participant's ID and another's handle, before any
  call.

* Results gain `metadata$guide`, `metadata$flow_params`,
  `participants$communication_style`, and `injections$as_turn` and
  `$replaced`. `analyze_focus_group()` and Focus Studio accept a result whose
  `focus_group` is `NULL` by rebuilding it from the record. A 0.5.5 record
  without its session object, which has none of these, is read with the
  phases its moderator lines record and the `on_desire_failure` of its
  session line; its guide items are rebuilt from the recorded moderator
  lines and marked verbatim, with a warning naming the items whose guide
  text is unknown, since a continuation delivers their lines as recorded.

* `seed` now leaves the caller's random-number state as it was, and must be
  one whole number: `seed = 1.7`, which was truncated to 1, is an error.

## Probe answers as numbers

* `fg_probe()` gains `scale = c(lo, hi)`. Each answer is read as one whole
  number on that scale into the new `value` column of `probes` (restated
  ranges, fractions such as "5/7", and markup are handled; two numbers, a
  number out of range, or a number in words give `NA`, and so does an answer
  that negates, compares, or reports another's number, such as "not a 7",
  "at least 6", "2+", "minus 2", or "the others said 4"); an answer that cannot
  be read is asked again once through the new template
  `participant_probe_reask`; `attempts` counts the calls; the estimate counts
  the re-asks; an answer still unread draws a warning.

## Corrections

* `summarize_focus_group()` sends messages to the model as recorded. It used
  to replace names inside the text, which credited statements to the wrong
  participant under permuted handles and corrupted IDs and names that are
  ordinary words ("teacher", "May"). A claim whose text names a speaker, by
  label or by an imported speaker's name, must cite a message by that
  speaker (check `named_speakers`), so a claim that says "Ben said ..." while
  citing Ana's message is sent back.

* Neutral handles never take another agent's ID, the moderator's included.

* `analyze_themes()` calls the provider unless given `.runner`, as
  `summarize()` does; it no longer falls back to the session's runner.

* A provider's retries no longer draw from R's random-number stream, so a
  retry does not change later speaker draws or the common random numbers of
  arms.

* In flat message mode each participant is told its handle; a turn the
  moderator's question gave to a participant carries a cue in both modes; the
  flat template asks participants to address others by their labels.

* Self-label removal, in the engine and in `fg_player()`, treats an agent's
  handle as its own label, not its ID.

* The reply cleaner no longer deletes a participant's words: it removes only
  a short opening self-introduction ("As a retired nurse,"), marks it with
  `metadata$preamble_removed`, and keeps longer clauses ("As a teacher I
  grade papers at night, ...") and discourse openings ("As an illustration,",
  "As an alternative,", "As a side note,") as written.

* A custom flow that chooses no participant or the moderator records no
  routing row.

* A moderator question that opens with a handle and a colon ("P2: what do
  you think?"), or a short follow-up naming one participant ("And P3?",
  "What about P3?"), gives that participant the floor.

* `FocusGroup$new()` validates `max_participant_responses`.

* `summarize_focus_group()` and `fg_turns()` accept the list returned by
  `FocusGroup$run_arms()`.

# FocusGroup 0.5.5

## Calling on silent participants

* The moderator now calls on participants who have not spoken. The new
  argument `call_on_silent` of `run_focus_group()`, `run_focus_group_arms()`,
  and `FocusGroup$new()` defaults to `c("opening", "midpoint", "end")`: after
  the discussion of the first, the middle, and the last discussion item
  (items 2, 4, and 7 of the default guide), each participant who has not
  spoken since the previous such point is asked by name, one at a time, with a
  fixed line (`call_on_text`) and no model call, and answers next. The first
  point follows the round-robin opening question of Krueger (2002),
  *Designing and Conducting Focus Group Interviews*. `"none"` turns call-ons
  off; guide item numbers choose other points. Called-on replies need no
  desire score and are recorded with the routing rule `"called_on"` and the
  `trigger_message_id` of the call-on line. Sessions under the default
  settings therefore contain more turns than under 0.5.4.

* The call estimate counts one reply per participant at each call-on point.
  The default session plans 111 model outputs (93 without call-ons), and the
  default `max_calls` rises from 100 to 120. The Studio's plan counts the same
  reserve.

## Neutral handles

* Agents no longer see the IDs a user gives them. Every prompt shows
  participants as P1, P2, ... and the moderator as "Moderator", and each
  participant is told its own handle ("In this discussion you are P3."). This
  covers history lines, the roster line, moderator placeholders, desire and
  probe prompts, and the interim summary that agents read. The records keep
  the IDs; the transcript and `participants` gain a `handle` column and the
  metadata a `handles` table. `handles = "ids"` shows the IDs, with a message;
  a named vector sets handles of your own. `focus_group_from_transcript()`
  keeps the speakers' own labels.

* `FocusGroup$new()` now requires the names of `agents` to equal the agents'
  IDs.

* Identifier columns (`id`, `respondent_id`, `persona_id`) are no longer
  rendered into persona text; they stay in `source_ids` and are reported as
  dropped. `create_agents_from_survey(survey_vars = NULL)` leaves out a
  variable that looks like a case ID, with a message.

* `moderator_opening` no longer asks participants for their participant ID,
  and `moderator_probing_focused` no longer names the first participant in
  the roster.

## Speaker selection

* Under `"desire_based"` turn-taking, the participant who closed one question
  could not open the next, because the previous-speaker exclusion outlived the
  moderator's line. The new `ConversationFlow$note_moderator_turn()` clears
  it whenever the moderator speaks, so the exclusion bars only back-to-back
  participant turns.

* A moderator line whose last question opens or closes with one participant's
  handle ("P3, do you agree?", "Could you say more, P4?") gives that
  participant the next turn, recorded with the rule `"addressed"`. The flow
  used to choose, so someone else could answer. A line that names several
  participants or asks the whole group addresses no one.

* Round robin passes over a participant who has just answered the moderator,
  so no one speaks twice in a row.

## Session player

* New `fg_player()` writes a finished session to one self-contained HTML file
  that replays it step by step, for a talk or a check by eye: a title page,
  one page per participant with the persona as sent, then every logged
  message in order. Before each turn it shows the speaker-selection record:
  desire scores with the previous speaker marked as not asked and tie draws
  stated, probabilistic chances, call-ons, and addressed turns. Private
  answers appear where they were asked, and an end page gives turns per
  participant and the final summary. `mode = "static"` shows every step at
  once, and the result of `run_focus_group_arms()` gives one file per arm.
  The page is built from the records alone, calls no model, and makes no
  network request; a missing record stops with an error naming it.

* The package includes `inst/extdata/player-session.rds`, a synthetic session
  made with a scripted runner, for trying `fg_player()` offline.

## Summaries at several levels

* New `summarize_focus_group()` summarizes a session, branched arms, or an
  imported transcript by participant, guide item, disagreement, trajectory,
  turn, or whole session, at the lengths `"headline"`, `"brief"`, and
  `"detailed"`, which come back together from one call per unit. The model
  returns claims as JSON, and prose is rendered only from claims that pass
  checks run in code: cited messages exist, lie in the unit, and come from the
  speakers named; quotes are verbatim up to case, whitespace, and quotation
  marks; wording that speaks for the whole group must cite every participant
  who spoke; silent participants can be reported only as silent; private
  probe wording may not appear unless `include_probes = TRUE`. A reply that
  fails is sent back once with each failure stated; a second failure raises
  `focusgroup_summary_check_failure` with the checks, the raw replies, and the
  partial summary. `on_fail = "drop"` sets failing claims aside unless they
  report dissent or change. The summarizing model sees handles, never user
  IDs or personas. Calls are recorded in the summary's own `calls` table and
  checked against `max_calls`.

* New `fg_turns()` returns the turn table of a session without a model call:
  each message's guide item, handle, injection status and visibility,
  routing rule, and whether the speaker was addressed, called on, or followed
  the previous message.

* `get_default_prompt_templates()` gains `summary_participant`,
  `summary_item`, `summary_disagreements`, `summary_trajectory`,
  `summary_turn`, `summary_session`, and `summary_reask`.

## Other changes

* `final_summary = FALSE` in `run_focus_group()`, `run_focus_group_arms()`,
  and `run_simulation()` skips the final summary call and lowers the estimate
  by one per arm.

* A new point `"before_closing"` for `fg_probe()` and `fg_injection()` comes
  after the last discussion item and before the closing line; `"end"` comes
  after the closing line.

* A label a speaker writes for itself at the start of a reply (`**MOD:**`,
  `P3:`) is removed, and the message is flagged with `self_label_removed`; the
  raw reply stays in `calls`.

* Every message records its guide item (`item`); routing gains
  `trigger_message_id`; `analyze_participation_balance()` gains `called_on`.

* `summarize()` labels speakers by handle, with the ID beside it in the final
  summary when the two differ, and leaves out the roster line; it stops when
  there is nothing to summarize instead of returning a sentence that reads as
  a summary; and it calls the provider unless given a `.runner`, instead of
  reusing the runner of the session.

* Message ids follow the largest id in the log, so an imported log with gaps
  never gets a duplicate id.

* `jsonlite` is now imported, by `fg_player()` and `summarize_focus_group()`.

# FocusGroup 0.5.4

## Personas from survey records

* `create_agents_from_survey()` renders answers the file leaves without a
  value label; they used to be dropped without notice. A number between the
  two labeled endpoints of a scale is shown on that scale ("On a scale of 1
  to 7, where 1 is ... and 7 is ..., you place yourself at 5"); a number in a
  numeric field is shown as the number ("age: 25"); any other unlabeled code
  is shown as the code, with the warning "Shown as the bare code because the
  file has no value label for it". A scale that also labels a midpoint (0, 5,
  and 10, say) leaves an unlabeled answer as the bare code, with that
  warning. Persona text changes for files such as the ANES, whose seven-point
  scales label only their endpoints.

* New argument `scale_template` sets the sentence for an answer placed on
  such a scale, for all items or per item, using the placeholders `{lo}`,
  `{lo_label}`, `{hi}`, `{hi_label}`, and `{value}`. A template without
  `{value}`, or with an unknown placeholder, is an error; the rendering
  report records the template used.

* New argument `question_wording`, a named character vector giving the
  question as asked for each survey item. An item without a wording is shown
  under its variable label, with the warning "No question wording was
  supplied for N survey item(s)"; it used to be shown under the name given in
  `survey_vars` (such as "scale_env_vs_business") without a warning. A name
  is now used only for an item the file gives no label.

* New argument `value_labels` (a data frame with `variable`, `code`, and
  `label`, or haven's named-list form) replaces the file's label for a code
  or labels a code the file leaves unlabeled. Labels exactly 80 characters
  long, the most a Stata label holds, draw the warning "Labels exactly 80
  characters long may have been cut short".

* New function `persona_rendering_report()`: one row per requested field and
  item of each persona, saying what text it was rendered as, or that it was
  dropped and why. Rosters from `create_agents_from_survey()`,
  `create_agents_from_data()`, and `create_agents()` carry it.

* Other new warnings and messages: unlabeled negative codes are left out as
  missing, as before, now with a warning; labels of negative codes are shown
  as answers, as before, now with a warning; answers left out as missing
  values are listed in a message; requested variables that are not in the
  file are named in a warning (they used to be dropped silently); items
  shown under the same text get " [VAR]" appended, with a warning (the
  persona used to keep the first answer and lose the second). Tagged missing
  values (haven `NA(d)`) are dropped; their labels used to be rendered as
  answers unless they matched `na_strings`. Variables requested twice are
  rendered once, with a warning, as are `value_labels` and `scale_template`
  entries for variables not requested and variable labels exactly 80
  characters long.

* The default `na_strings`, now returned by the new
  `fg_default_na_strings()`, also covers the labels the ANES 2024 release
  gives its negative missing-data codes ("-3. RESTRICTED", "-6. No post
  interview", "-7. Insufficient partial, interview deleted", "-2. DK/RF in
  ...", "-2. Data will be released separately when available", and others)
  and the WVS labels "No answer", "Not applicable", and "Not available".
  Such answers are now left out as missing; they used to be shown as answers
  ("Answer: -3. RESTRICTED"). "Error" and "RESTRICTED" count only as a whole
  label, and "break off" and "insufficient" only in a label with a negative
  code.

* Numbers in personas are written in full ("income: 100000"); values of
  100000 and above used to appear in scientific notation ("1e+05").

## Injections, branches, and probes

* New `fg_injection()` puts a message into a session at a stated point,
  `fg_after_item(k)` (after the moderator's line at guide item k) or
  `fg_before_turn(t)`, from the moderator or a named outside voice such as
  "Guest expert", either into the shared log or only into the participants'
  prompts. Injected messages are flagged in the log with their arm and cut
  point.

* New `run_focus_group_arms()` runs a session up to the injection point once
  and then the full remainder once per arm, so the arms share every message
  before the injection; each branch records the prefix it continued, and
  each arm restores the random-number state of the branch point.

* New `fg_probe()` asks each participant a question at stated points
  (including "start" and "end"); the answers are saved in the result's
  `probes` table and never enter the shared log or later prompts.

* New `moderator_verbatim` (`TRUE`, or guide item numbers) delivers guide
  items as written, without a model call.

* `run_focus_group()` gains `injections`, `probes`, and `moderator_verbatim`.
  New errors are raised before any call for a point beyond the guide's items
  or turns, an injection from a participant or "System", arms that are
  unnamed, not distinct, fewer than two, inject nothing, or name different
  first points, and invalid `moderator_verbatim` values; a point the session
  never reached draws a warning.

## Reasoning models

* Under `"desire_based"` turn-taking, a desire score that cannot be obtained
  after its retry now stops the session with an error of class
  `focusgroup_desire_failure`, returned inside a
  `focusgroup_incomplete_session`. It used to discard every agent's valid
  score and draw the speaker uniformly at random, noted only in metadata.
  `on_desire_failure = "random"` continues instead: it chooses among the
  valid scores by the usual rule and draws at random only when none is
  valid, recording the fallback.

* Scores and utterances are read from the final answer only: reasoning in
  `<think>`, `<thinking>`, or `<reasoning>` tags or in the gpt-oss channel
  markup is removed, and a reasoning block that never closes counts as no
  answer. A number inside the reasoning used to be read as the score.

* Desire-scoring calls keep a configured temperature; it used to be removed,
  against a code comment saying it was kept. None is added when the
  configuration sets none.

* `run_focus_group()` gains `reasoning_effort` (one setting for every call,
  or one per call type: moderator, participant, desire, summary, probe) and
  `max_tokens_config`, which now also sets `desire_retry` (default 256),
  `summary`, `interim_summary`, and `probe`.

## Records

* Results gain `calls` (every model call, including retries and failures:
  messages sent, raw reply, reasoning text, cleaned text, score, settings,
  and tokens including reasoning tokens) and `routing` (every speaker pick:
  candidates, exclusions, scores, rule, and how a tie was broken).

* Session metadata records the response limit, seed, `on_desire_failure`,
  `reasoning_effort`, token limits, per-call settings, prompt templates,
  standing rules, and the FocusGroup and LLMR versions. Only allowlisted
  model parameters are recorded, so no key or endpoint enters a record.

## Other corrections

* Reply cleaning keeps discourse openings such as "As a result," and "As an
  aside,"; it still removes persona self-introductions such as "As a
  teacher,".

* `analyze_participation_balance()` lists every participant; one who never
  spoke has 0 messages. They used to be left out.

* The interim summary covers the messages the prompt window drops, instead
  of the last 30 messages, and is not made while the window holds the whole
  discussion.

* Moderator templates no longer assert history that may not exist, such as
  a completed icebreaker.

* In the Studio's continuation tab, the edited message is the one
  previewed; with a System row in the roster they used to be one message
  apart.

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
