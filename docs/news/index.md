# Changelog

## FocusGroup 0.5.1

Initial CRAN release.

- [`run_focus_group()`](https://asanaei.github.io/FocusGroup/reference/run_focus_group.md)
  is the single high-level session runner. It requires an explicit
  `config`, accepts `n_participants`, `guide`, `flow`, and
  `message_mode`, and performs a model-call cost preflight before live
  work.
- [`run_focus_group()`](https://asanaei.github.io/FocusGroup/reference/run_focus_group.md)
  returns a `focus_group_result` with the stable fields `focus_group`,
  `transcript`, `summary`, `participants`, `usage`, and `metadata`. Its
  print method gives a concise run summary.
- [`create_agents()`](https://asanaei.github.io/FocusGroup/reference/create_agents.md),
  [`create_agents_from_data()`](https://asanaei.github.io/FocusGroup/reference/create_agents_from_data.md),
  and
  [`create_agents_from_survey()`](https://asanaei.github.io/FocusGroup/reference/create_agents_from_survey.md)
  construct agent-ID-keyed rosters from supplied personas, in-memory
  records, labeled survey files, and pre-rendered persona panels.
- `ConversationFlow` is the public extension base. Built-in round-robin,
  probabilistic, and desire-based flows are constructed with
  [`create_conversation_flow()`](https://asanaei.github.io/FocusGroup/reference/create_conversation_flow.md).
- Model configuration arguments use `config`; group-level configuration
  uses `admin_config`. The `FGAgent` configuration field is `config`.
- The `.runner` seam accepts an experiments data frame with `config` and
  `messages` list-columns and returns those rows with at least
  `response_text`.
- Session logs give every row a unique integer `message_id` and record
  the moderator cycle as `round`, while retaining `phase`.
- [`analyze_focus_group()`](https://asanaei.github.io/FocusGroup/reference/analyze_focus_group.md)
  returns a `focus_group_analysis` with stable fields, typed empty
  components, and an `issues` table. Descriptive analysis runs offline.
  Thematic analysis and model summaries require an explicit `config`.
- [`focus_group_from_transcript()`](https://asanaei.github.io/FocusGroup/reference/focus_group_from_transcript.md)
  accepts `moderator_id`; when it is omitted, speaker identifiers
  containing `"mod"` provide the fallback.
- [`get_default_prompt_templates()`](https://asanaei.github.io/FocusGroup/reference/get_default_prompt_templates.md)
  contains only templates used by supported package operations.
- [`run_focus_studio()`](https://asanaei.github.io/FocusGroup/reference/run_focus_studio.md)
  starts the optional Shiny interface for running sessions, reading
  transcripts, and comparing continuations from edited histories.
