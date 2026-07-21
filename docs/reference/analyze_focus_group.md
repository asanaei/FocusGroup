# Analyze Focus Group Results

Runs descriptive analyses offline. Thematic analysis and a
model-generated summary are opt-in and run only when \`config\` is
supplied.

## Usage

``` r
analyze_focus_group(
  focus_group_result,
  num_topics = 5,
  include_plots = TRUE,
  config = NULL,
  .runner = NULL
)
```

## Arguments

- focus_group_result:

  A \`FocusGroup\` object or \`focus_group_result\`.

- num_topics:

  Integer number of topics for topic modeling.

- include_plots:

  Logical. Attempt the four descriptive plots.

- config:

  Optional explicit \`LLMR::llm_config\`. When supplied, thematic
  analysis and a model summary are generated.

- .runner:

  Optional experiments-frame runner for the two model analyses.

## Value

A \`focus_group_analysis\` with fixed fields and an \`issues\` table.

## Examples

``` r
transcript <- data.frame(
  speaker = c("Moderator", "P1", "P2"),
  text = c("What matters most?", "Cost matters.", "Access matters.")
)
fg <- focus_group_from_transcript(transcript)
analysis <- analyze_focus_group(fg, include_plots = FALSE)
analysis$basic_stats$speaker_stats
#> # A tibble: 3 × 4
#>   speaker_id utterance_count total_words avg_words_per_utterance
#>   <fct>                <int>       <int>                   <dbl>
#> 1 Moderator                1           3                       3
#> 2 P1                       1           2                       2
#> 3 P2                       1           2                       2
```
