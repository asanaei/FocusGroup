# Create Focus Group Agents

Creates participant agents from supplied records, direct personas, or
generated profiles, and adds a moderator.

## Usage

``` r
create_agents(
  n_participants,
  config,
  demographics = NULL,
  survey_responses = NULL,
  direct_persona_descriptions = NULL,
  .runner = NULL
)
```

## Arguments

- n_participants:

  Integer number of participants to create

- config:

  An explicit \`LLMR::llm_config\` for all agents.

- demographics:

  Optional data frame with demographics. If NULL, generates diverse
  demographics.

- survey_responses:

  Optional data frame with survey responses. If NULL, generates
  responses.

- direct_persona_descriptions:

  Optional character vector of pre-rendered participant personas. When
  supplied, these descriptions are used directly and are recycled in
  order if necessary. Demographics and survey responses remain attached
  as raw reporting fields.

- .runner:

  Optional experiments-frame runner stored on every agent.

## Value

A named list of FGAgent objects (participants + 1 moderator), keyed by
agent ID.

## Examples

``` r
if (FALSE) { # \dontrun{
cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
agents <- create_agents(6, config = cfg)

# Create with custom demographics
demo_data <- data.frame(
  age = c(22, 35, 28, 41, 19, 33),
  gender = c("Female", "Male", "Male", "Female", "Male", "Female"),
  education = c("Bachelor's", "Master's", "High School", "PhD", "Some College", "Bachelor's")
)
agents <- create_agents(6, config = cfg, demographics = demo_data)
} # }
```
