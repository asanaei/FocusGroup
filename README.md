# FocusGroup: simulated moderated discussion for research design

<img src="man/figures/logo.png" alt="FocusGroup icon" width="120" class="d-none" />

[![R-CMD-check](https://github.com/asanaei/FocusGroup/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/FocusGroup/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/FocusGroup/)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## What FocusGroup is for

FocusGroup runs moderated discussions in which language models act as the
moderator and participants. A moderator guide sets the sequence of the
discussion, and a turn-taking rule selects each participant speaker. Model
requests use [LLMR](https://asanaei.github.io/LLMR/).

The package supports moderator-guide piloting, comparison of discussion
protocols, and studies of interaction under specified personas, histories, and
speaker-selection rules. These uses concern research design. A generated
transcript records model responses under the chosen guide, persona set, model
configuration, and turn-taking rule. It does not estimate public opinion.

FocusGroup is experimental. Its interface may change before a stable release.

## Install and configure a model

```r
install.packages("LLMR")
remotes::install_github("asanaei/FocusGroup")
```

API operations that generate model output require an explicit `config`. Set the
provider key in an environment variable before making a live call. For an
OpenAI model, LLMR reads `OPENAI_API_KEY`.

```r
library(FocusGroup)
library(LLMR)

config <- LLMR::llm_config(
  provider = "openai",
  model = "gpt-4o-mini",
  temperature = 0.7,
  max_tokens = 200
)
```

## Run a moderated session

`run_focus_group()` builds the moderator, participants, guide, and turn-taking
rule, then runs one session.

```r
moderator_guide <- list(
  Opening = "Welcome the group and state the ground rules.",
  Icebreaker = c(
    "What first comes to mind when you think about remote work?",
    "What experience has shaped that impression?"
  ),
  Engagement = "Where does remote work enter your daily routine?",
  Exploration = c(
    "What tensions arise between flexibility and coordination?",
    "Whose perspective has received too little attention?"
  ),
  Closing = "Thank the participants and close the session."
)

result <- run_focus_group(
  topic = "The impact of remote work on team collaboration",
  n_participants = 4,
  guide = moderator_guide,
  flow = "round_robin",
  config = config,
  seed = 110,
  message_mode = "roleflip",
  verbose = TRUE
)

print(result)
head(result$transcript, 5)
result$participants
result$usage
cat(result$summary)
```

The result contains the underlying `focus_group`, a row-oriented `transcript`,
the final `summary`, a `participants` data frame, token `usage`, and study
`metadata`.

## Design the moderator guide

The `guide` argument accepts either phase counts or ordered moderator
instructions. A named numeric vector selects that many built-in instructions
for each phase:

```r
count_guide <- c(
  Opening = 1,
  Icebreaker = 1,
  Engagement = 2,
  Exploration = 3,
  Closing = 1
)
```

A named list, such as `moderator_guide` above, supplies the instruction text.
A character vector within one phase supplies several moderator turns in order.
The supported phases are Opening, Icebreaker, Engagement, Exploration, and
Closing. A guide may omit phases, but phases that are present must follow this
order and each may appear once. Opening and Closing contain moderator turns;
Icebreaker, Engagement, and Exploration also collect participant responses.

## Choose a turn-taking rule

The `flow` argument controls how a participant is selected after a moderator
question. The choice changes both the study design and the number of model
calls.

| Mode | Speaker selection | Research use and cost |
|---|---|---|
| `"round_robin"` | It cycles through participant IDs in a fixed order. | Use it when equal speaking opportunity and predictable exposure are design requirements. It adds no speaker-selection model calls. |
| `"probabilistic"` | It samples from speaking propensities. The selected participant's propensity falls to zero, while the other propensities recover toward their baselines by `recovery_increment`, which defaults to `0.1`. | Use it when unequal, stochastic participation is part of the design but speaker choice should not depend on model-rated content. |
| `"desire_based"` | It asks eligible personas to rate their desire to speak from the current question and transcript. It excludes the previous speaker when possible, uses `min_desire_threshold = 3`, may relax the threshold after discussion begins, selects the highest score, breaks ties at random, and falls back to uniform selection after scoring failures. | Use it when content-responsive speaker emergence is the object of study. It adds model calls and makes speaker assignment model-dependent. |

`"desire_based"` is the default. Nondefault propensity, recovery, or desire
settings require `create_conversation_flow()` and direct construction, described
in the advanced section below.

## Build participants from personas and respondent data

Participant records can enter as direct descriptions, in-memory respondent
rows, a pre-rendered persona panel, or a labeled survey file. Each constructor
returns a named list of `FGAgent` objects containing the participants and a
moderator.

Use direct descriptions when the persona text has already been specified:

```r
direct_agents <- create_agents(
  n_participants = 2,
  direct_persona_descriptions = c(
    "A union representative who works on site five days a week.",
    "A software developer who works from home and coordinates across time zones."
  ),
  config = config
)
```

`create_agents_from_data()` accepts respondent rows already in memory.
Demographic columns are rendered as background and the remaining columns as
survey responses. Values are used as supplied, so coded values should be
decoded before this call.

```r
respondents <- data.frame(
  age = c(34, 61),
  occupation = c("accountant", "school administrator"),
  `How often do you work remotely?` = c("three days a week", "never"),
  check.names = FALSE
)

record_agents <- create_agents_from_data(
  respondents,
  n_participants = 2,
  demographic_cols = c("age", "occupation"),
  config = config
)
```

A data frame with `persona` and `persona_id` columns is treated as a
pre-rendered persona panel. This includes `LLMR::anes_2024_personas`:

```r
data(anes_2024_personas, package = "LLMR")

panel_agents <- create_agents_from_data(
  anes_2024_personas,
  n_participants = 6,
  config = config
)
```

`create_agents_from_survey()` reads Stata, SPSS, and SAS files through `haven`.
It decodes selected values from their value labels and uses variable labels as
question text unless names supplied by the caller replace them.

```r
survey_agents <- create_agents_from_survey(
  n_participants = 6,
  survey_path = "respondents.dta",
  config = config,
  demographic_vars = c(age = "AGE", education = "EDUC"),
  survey_vars = c("Remote-work preference" = "Q12")
)
```

When direct persona text is absent, the constructors render the supplied
demographic fields and survey responses. They do not infer dispositions from
demographic labels. `run_focus_group()` accepts in-memory demographics and
survey responses through its own arguments. Rosters returned by the
constructors above are used with direct `FocusGroup` construction.

## Import and analyze transcripts

`focus_group_from_transcript()` imports a simulated or observed transcript for
analysis. Importing and descriptive analysis make no model call. Pass
`moderator_id` when the moderator is known. If it is omitted, the importer
looks for `"mod"` within speaker identifiers.

```r
transcript <- data.frame(
  speaker = c("Facilitator", "P1", "P2"),
  text = c("What should change?", "Service frequency.", "Lower fares.")
)

imported <- focus_group_from_transcript(
  transcript,
  topic = "Public transit",
  moderator_id = "Facilitator"
)

offline_analysis <- analyze_focus_group(imported, include_plots = FALSE)
```

`analyze_focus_group()` returns `basic_stats`, `topics`, `tfidf`,
`readability`, `themes`, `model_summary`, `plots`, and `issues`. Components that
cannot be computed are returned as empty components with consistent columns
where they are tabular. `plots` is an empty list when `ggplot2` is unavailable,
and `issues` records each optional analysis that failed and its reason.

Descriptive analyses run offline. Thematic analysis and model summaries are
opt-in and run only when the caller supplies `config`:

```r
model_analysis <- analyze_focus_group(result, config = config)
```

A requested model analysis raises a provider condition if the provider call
fails. It does not convert that failure into a missing component. Additional
text analyses and plots use optional packages:

```r
install.packages(c(
  "ggplot2", "quanteda", "quanteda.textstats", "topicmodels", "tidytext"
))
```

FocusGroup does not export specialized report constructors. `LLMR::report()`
is the ecosystem reporting entry point.

## Use Focus Studio

`run_focus_studio()` starts the optional Shiny interface. Its Run tab starts
sessions with synthetic or ANES 2024 personas. Synthetic-persona sessions use
`run_focus_group()`; ANES sessions use respondent-derived agents with the same
guide, flow, and session classes. The Analyze tab reads participation and word
statistics offline. The Continuation experiment fixes the next participant
speaker and generates one next message under the original history and one under
a history with an earlier message edited. These paired continuations support a
comparison of the specified next turn; they do not continue the full session.

Session execution and continuation generation make live model calls. Install
the suggested `shiny`, `bslib`, `DT`, and `LLMR.shiny` packages before starting
the interface.

## Study records, cost, and interpretation

The transcript records each message with its phase and round. Generated rows
also carry response identifiers, finish reasons, token counts, provider and
model labels, duration, and failure metadata. Moderator questions and their
participant responses share a round.

The `participants` component is a data frame. `usage` uses LLMR's `sent`, `rec`,
and `total` token vocabulary. `metadata` records `topic`, `purpose`, `flow`,
`message_mode`, `n_participants`, `estimated_calls`, `provider`, and `model`.
Credentials are never included.

Before a live run, `run_focus_group()` estimates the number of model calls. A
request above `max_calls` stops unless `confirm = TRUE`. Retries and incomplete
outputs can increase the realized call count.

A generated transcript is evidence about the specified simulation, not a
population. Interpretation should retain the moderator guide, participant
records, model configuration, turn-taking rule, and message construction with
the study record.

## Advanced construction, testing, and extension

Direct construction creates an agent roster, builds a flow with
`create_conversation_flow()`, passes both to `FocusGroup$new()`, and calls
`run_simulation()`. A `FocusGroup` object represents one session and runs once.
`FGAgent$new()` also takes `config`, stored in its public field of the same name.
Built-in rules are created through `create_conversation_flow()`.
`ConversationFlow` is the base class for custom turn-taking rules.

The `.runner` argument accepts an optional function for offline tests or replay
instead of live model calls. It uses the same request and response data-frame
format as LLMRcontent archive replay: the function receives a data frame with
`config` and `messages` list-columns and returns those rows with at least
`response_text`. It handles moderator, participant, desire-scoring, and summary
requests. An explicit configuration remains required and is recorded with the
experiment so readers can see how it was run. Supplying `.runner` bypasses the
live cost stop.

`get_default_prompt_templates()` returns the supported participant, moderator,
desire-scoring, and thematic-analysis templates. Named replacements can be
passed as `prompt_templates` to `FocusGroup$new()`. See `?run_focus_group`,
`?FocusGroup`, `?FGAgent`, and `?ConversationFlow` for the direct API and return
values.

## Contributing

Report bugs and feature requests in the
[GitHub repository](https://github.com/asanaei/FocusGroup). Pull requests may
be submitted there.

## License

This project uses the MIT License; see `LICENSE`.
