# Run a Focus Group Simulation

Constructs the agents, moderator guide, and turn-taking flow, then runs
one focus group session. The model configuration is explicit. Before a
live run, the function estimates the number of generated model outputs
and applies the \`max_calls\` limit unless \`confirm\` is \`TRUE\`.

## Usage

``` r
run_focus_group(
  topic,
  config,
  n_participants = 6,
  guide = c(Opening = 2, Icebreaker = 3, Engagement = 8, Exploration = 10, Closing = 2),
  demographics = NULL,
  survey_responses = NULL,
  flow = "desire_based",
  seed = NULL,
  message_mode = c("roleflip", "flat"),
  verbose = TRUE,
  max_participant_responses = NULL,
  max_calls = 100L,
  confirm = FALSE,
  .runner = NULL
)
```

## Arguments

- topic:

  Character. Focus group topic.

- config:

  An explicit \`LLMR::llm_config\` used by all agents and group-level
  model tasks.

- n_participants:

  Integer. Number of participants, excluding the moderator.

- guide:

  A named numeric vector or named list. Numeric values select that many
  moderator instructions from the phase banks. Character vectors supply
  the ordered instructions directly.

- demographics:

  Optional participant demographics.

- survey_responses:

  Optional participant survey responses.

- flow:

  Character. One of \`"round_robin"\`, \`"probabilistic"\`, or
  \`"desire_based"\`.

- seed:

  Optional integer governing in-package sampling.

- message_mode:

  Character. \`"roleflip"\` or \`"flat"\`.

- verbose:

  Logical. Print session progress.

- max_participant_responses:

  Optional integer maximum number of participant responses per moderator
  question.

- max_calls:

  Integer. Maximum estimated live model outputs allowed without
  confirmation.

- confirm:

  Logical. Permit a live run whose estimate exceeds \`max_calls\`.

- .runner:

  Optional experiments-frame runner. It receives a data frame with
  \`config\` and \`messages\` list-columns and returns those rows with
  at least \`response_text\`.

## Value

A \`focus_group_result\` with the group, transcript, summary,
participant table, token usage, and sanitized metadata.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
result <- run_focus_group(
  topic = "Library funding priorities",
  config = cfg,
  n_participants = 4,
  guide = list(
    Opening = "Welcome the participants and state the ground rules.",
    Exploration = "Which funding priority deserves attention first?",
    Closing = "Thank the participants and close the session."
  ),
  flow = "round_robin"
)
} # }
```
