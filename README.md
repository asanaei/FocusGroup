# FocusGroup: simulated moderated discussion for research design

<img src="man/figures/logo.png" alt="FocusGroup icon" width="120" class="d-none" />

[![R-CMD-check](https://github.com/asanaei/FocusGroup/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/FocusGroup/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/FocusGroup/)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

## Overview

FocusGroup runs a moderated discussion in which language models act as the
moderator and participants. A question script determines what the moderator
does. A conversation flow selects each participant. The package stores the
utterances, phase labels, response metadata, and token counts in a
`FocusGroup` object. Model requests are made through
[LLMR](https://asanaei.github.io/LLMR/).

FocusGroup is used to pilot moderator guides and discussion protocols before
fieldwork. Researchers can compare scripts and turn-taking rules or examine how
an edited turn changes the continuation of a discussion. `fg_quick()` and
`run_focus_group()` construct complete sessions. The exported R6 classes give
direct access to agents, scripts, prompts, and speaker selection.

## Main objects

- `FGAgent` stores one speaker's persona, model configuration, optional runner,
  utterance history, and token counts.
- `FocusGroup` stores the roster, moderator script, conversation flow, prompts,
  and conversation log.
- `RoundRobinFlow`, `ProbabilisticFlow`, and `DesireBasedFlow` select the next
  participant by order, propensity, or a model-produced score.
- `create_diverse_agents()`, `create_agents_from_data()`, and
  `create_agents_from_survey()` construct agents from direct descriptions or
  respondent data.
- `analyze_focus_group()` runs the main transcript analyses.
- `run_focus_studio()` starts the optional Shiny interface.

FocusGroup is experimental. Its public interface may change before a stable
release.

## Installation

```r
install.packages("LLMR")
remotes::install_github("asanaei/FocusGroup")
```

Text analysis and plotting use optional packages:

```r
install.packages(c("ggplot2", "quanteda", "quanteda.textstats",
                   "topicmodels", "tidytext"))
```

Set the provider key in an environment variable before making a provider call.
For an OpenAI model, LLMR reads `OPENAI_API_KEY`.

## Quick start

### Shiny interface

`run_focus_studio()` starts the optional Shiny application:

```r
library(FocusGroup)
run_focus_studio()
```

**Run a focus group** executes a session and saves the `FocusGroup` object as an
`.rds` file. **Analyze transcript** reads participation and word statistics
without making a model call. **Continuation experiment** compares the next
generated turn under an original and an edited history.

### Run a scripted set of phases

`run_focus_group()` constructs agents, a moderator script, and a conversation
flow. For `turns_per_phase`, numeric values select that many instructions from
the built-in phase banks. Character vectors provide instructions verbatim, one
moderator turn per element. Phases must be supplied in the order Opening,
Icebreaker, Engagement, Exploration, Closing.

```r
library(FocusGroup)
library(LLMR)

llm_config_agents <- LLMR::llm_config(
  provider = "openai",
  model = "gpt-4o-mini",
  temperature = 0.7,
  max_tokens = 200
)

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
  participants = 4,
  turns_per_phase = moderator_guide,
  llm_config = llm_config_agents,
  seed = 110,
  verbose = TRUE
)

head(result$conversation, 5)
result$basic_stats$speaker_stats
cat(result$summary)
```

The transcript records model responses under the supplied prompts and
configuration. It is not an estimate of public opinion.

### Run a session without a provider

`runner` is called with the active LLMR configuration and message list in place
of a provider call. The fixed response below permits the complete session and
its descriptive analyses to run offline.

```r
scripted_runner <- function(config, messages) {
  paste(
    "This scripted response states one position on library funding and gives",
    "a concrete reason that another participant could examine."
  )
}

offline_result <- fg_quick(
  topic = "Library funding priorities",
  participants = 2,
  flow = "round_robin",
  runner = scripted_runner,
  max_participant_responses = 1,
  verbose = FALSE
)

offline_result$focus_group$analyze()$speaker_stats
```

### Construct the R6 objects directly

`FGAgent`, a `ConversationFlow` subclass, and `FocusGroup` can be constructed
directly.

```r
library(FocusGroup)
library(LLMR)

llm_conf <- LLMR::llm_config(
  provider = "openai",
  model = "gpt-4o-mini",
  temperature = 0.7
)

all_agents_list <- create_diverse_agents(
  n_participants = 3,
  llm_config = llm_conf
)
all_agents_named <- setNames(
  all_agents_list,
  vapply(all_agents_list, function(agent) agent$id, character(1))
)
moderator_id <- "MOD"

custom_script <- list(
  list(phase = "opening"),
  list(
    phase = "icebreaker_question",
    text = "What is one word that describes your ideal weekend?"
  ),
  list(
    phase = "engagement_question",
    text = "How do you typically spend your weekends?"
  ),
  list(
    phase = "exploration_question",
    text = "What makes a weekend relaxing or fulfilling for you?"
  ),
  list(phase = "closing")
)

round_robin_flow <- RoundRobinFlow$new(
  agents = all_agents_named,
  moderator_id = moderator_id
)

fg_manual <- FocusGroup$new(
  topic = "Weekend preferences and activities",
  purpose = "Describe how participants prefer to spend their weekends.",
  agents = all_agents_named,
  moderator_id = moderator_id,
  turn_taking_flow = round_robin_flow,
  question_script = custom_script,
  llm_config_admin = llm_conf
)

fg_manual$run_simulation(verbose = TRUE)
conversation_df_manual <- dplyr::bind_rows(
  lapply(fg_manual$conversation_log, as.data.frame)
)
head(conversation_df_manual, 5)
cat(fg_manual$summarize(summary_level = 1))
```

## Core classes

### `FGAgent`

`FGAgent` represents one participant or moderator. `generate_utterance()`
returns text and call metadata. `get_need_to_talk()` returns the score used by
`DesireBasedFlow`.

```r
participant_details <- list(
  direct_persona_description = paste(
    "A 35-year-old software engineer who enjoys hiking and is concerned",
    "about data privacy."
  ),
  communication_style = "analytical and inclined to ask clarifying questions",
  demographics = list(age = 35, occupation = "Software Engineer")
)

agent1 <- FGAgent$new(
  id = "P1",
  agent_details = participant_details,
  llm_config = llm_conf
)
```

### `FocusGroup`

`FocusGroup` owns the roster, script, conversation flow, prompts, and log.
`run_simulation()` executes the script. `analyze()` and the other
`analyze_*()` methods read the log.

### `ConversationFlow`

`ConversationFlow` defines `select_next_speaker()`. Its three subclasses
implement the supplied selection rules. `create_conversation_flow()` constructs
a flow from a mode name.

## From a silicon panel to a focus group

`create_agents_from_data()` treats a data frame as a pre-rendered persona panel
when it inherits from `silicon_panel` or contains both `persona` and
`persona_id`. It uses `persona` as the direct persona description and excludes
`persona` and `persona_id` from survey responses. LLMRpanel need not be
installed.

```r
# panel is an LLMRpanel persona frame with class "silicon_panel"
panel_agents <- create_agents_from_data(
  panel,
  n_participants = nrow(panel),
  llm_config = llm_conf
)
panel_agents <- setNames(
  panel_agents,
  vapply(panel_agents, function(agent) agent$id, character(1))
)

panel_flow <- RoundRobinFlow$new(panel_agents, moderator_id = "MOD")
panel_session <- FocusGroup$new(
  topic = "Public transit priorities",
  purpose = "Examine points of agreement and disagreement.",
  agents = panel_agents,
  moderator_id = "MOD",
  turn_taking_flow = panel_flow,
  question_script = list(
    list(phase = "opening"),
    list(
      phase = "exploration_question",
      text = "Which transit priority should receive attention first?"
    ),
    list(phase = "closing")
  )
)
panel_session$run_simulation()
```

`create_agents_from_data()` also accepts `LLMR::anes_2024_personas`.

## Create agents from survey data

`create_agents_from_survey()` reads Stata, SPSS, and SAS files with `haven`. It
decodes selected variables from their value labels and uses variable labels as
question text. `create_agents_from_data()` accepts an in-memory data frame.
`create_diverse_agents()` accepts demographic and survey-response data frames
with one row per participant.

When a direct persona description is absent, the package renders the supplied
demographic fields and survey responses in the persona prompt. It does not
infer dispositions from demographic labels.

## Prompts

`get_default_prompt_templates()` returns the named templates used by
`FocusGroup`. Change the required entries and pass the list to
`FocusGroup$new(prompt_templates = ...)`.

```r
default_prompts <- get_default_prompt_templates()
default_prompts$moderator_opening <- paste(
  "Welcome the participants. State that the topic is {{topic}} and the",
  "purpose is {{focus_group_purpose}}. Explain the ground rules."
)

# fg <- FocusGroup$new(
#   ...,
#   prompt_templates = default_prompts
# )
```

## Analysis

`FocusGroup$analyze()` returns speaker counts, word counts, and a formatted
transcript. Other methods return topic, TF-IDF, readability, thematic,
participation, response, question, and key-phrase results.
`analyze_focus_group()` runs the main analyses in one call.

The four `plot_*()` methods return ggplot objects for participation and turn
length. When `include_plots = TRUE`, `analyze_focus_group()` places them in its
`plots` element if ggplot2 is installed.

## Contributing

Report bugs and feature requests in the
[GitHub repository](https://github.com/asanaei/FocusGroup). Pull requests may
be submitted there.

## License

This project uses the MIT License; see `LICENSE`.
