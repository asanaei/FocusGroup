# Create agents from an in-memory data frame of respondents

Like \[create_agents_from_survey()\] but starting from a data frame
already in memory (for example \`LLMR::anes_2024_personas\`).
Demographic columns are rendered as background; the remaining columns
are rendered as survey responses, keyed by their question wording when
the frame carries a \`dictionary\` attribute (see
\[LLMR::llm_persona_split()\]), else by their column names. Values are
taken as-is (decode and clean them first if they are still coded).

## Usage

``` r
create_agents_from_data(
  data,
  n_participants,
  config,
  demographic_cols = NULL,
  rows = NULL,
  weights = NULL,
  .runner = NULL
)
```

## Arguments

- data:

  A data frame, one respondent per row.

- n_participants:

  Integer number of participants (excludes the moderator).

- config:

  An explicit \`LLMR::llm_config\` for all agents.

- demographic_cols:

  Character vector of columns to render as demographics. Defaults to the
  \`data\`'s \`"demographic_fields"\` attribute when present, else a
  small set of common demographic column names found in \`data\`.

- rows, weights:

  See \[create_agents_from_survey()\].

- .runner:

  Optional experiments-frame runner stored on every agent.

## Value

A named list of \`FGAgent\` objects (participants + moderator), keyed by
agent ID.

## Details

A frame of class \`silicon_panel\`, or a frame with both \`persona\` and
\`persona_id\` columns, is treated as a pre-rendered persona panel. Its
\`persona\` text becomes each participant's direct persona description;
\`persona\` and \`persona_id\` are not rendered as survey answers. This
bridge uses the frame's structure and does not require LLMRpanel.

## Examples

``` r
if (FALSE) { # \dontrun{
data(anes_2024_personas, package = "LLMR")
cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
agents <- create_agents_from_data(
  anes_2024_personas, n_participants = 6, config = cfg
)
} # }
```
