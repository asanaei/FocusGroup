# Build a FocusGroup object from an imported transcript

Turns a transcript that the package did not generate – a human focus
group, a transcript exported from other software, or a saved CSV – into
a \[FocusGroup\] object whose \`conversation_log\` holds the imported
turns. The returned object supports the analysis methods (\`analyze()\`,
\`analyze_participation_balance()\`, \`analyze_topics()\`,
\`analyze_tfidf()\`, \`analyze_readability()\`, \`analyze_themes()\`),
the plotting methods, and \[analyze_focus_group()\]. No model call is
made: importing and the descriptive analyses run offline, and only the
model-dependent methods (\`analyze_themes()\`, \`summarize()\`) contact
a provider when you call them.

## Usage

``` r
focus_group_from_transcript(
  data,
  speaker_col = "speaker",
  text_col = "text",
  topic = NULL,
  moderator_id = NULL
)
```

## Arguments

- data:

  A data frame with one row per turn, in order.

- speaker_col:

  Character. Name of the column holding the speaker id. Default
  "speaker".

- text_col:

  Character. Name of the column holding the utterance text. Default
  "text".

- topic:

  Character or \`NULL\`. The discussion topic, used by the
  model-dependent analyses and stored on the object. \`NULL\` falls back
  to "Imported transcript".

- moderator_id:

  Character or \`NULL\`. Exact speaker id to treat as the moderator.
  When \`NULL\`, the documented "mod" substring fallback is used.

## Value

A \[FocusGroup\] object with a populated \`conversation_log\`.

## Details

One lightweight agent is created per speaker so that speaker-aware
methods have a roster to report on; these agents carry a minimal
placeholder persona and are not meant to continue the conversation. Set
\`moderator_id\` when the transcript identifies the moderator. When it
is \`NULL\`, the moderator is the first speaker whose id contains "mod"
(case-insensitive), e.g. "Moderator" or "MOD". When the fallback finds
no match, a non-speaking "MOD" agent is added so the object is complete,
and it appears in \`analyze()\` with zero utterances. Rows whose speaker
is missing or blank are dropped with a warning; rows with speaker
"System" are kept in the log but excluded from analyses, matching how
the simulator treats its own roster message.

## See also

\[analyze_focus_group()\] to run the descriptive analyses in one call;
\[run_focus_group()\] to simulate a discussion instead.

## Examples

``` r
transcript <- data.frame(
  speaker = c("Moderator", "Ana", "Ben", "Ana", "Moderator", "Ben"),
  text = c(
    "Welcome, everyone. What do you make of the new library hours?",
    "They help working parents like me; evenings matter most.",
    "I see it differently. Mornings are now far too crowded.",
    "Crowded mornings still beat being locked out after work.",
    "Ben, say more about what changed in the mornings.",
    "Staff are stretched thin before noon, so lines are longer."
  )
)
fg <- focus_group_from_transcript(transcript, topic = "library hours")
fg$analyze_participation_balance()$participation_stats
#> # A tibble: 3 × 6
#>   speaker_id messages words avg_words message_percentage word_percentage
#>   <chr>         <int> <int>     <dbl>              <dbl>           <dbl>
#> 1 Ana               2    18         9               33.3            31.0
#> 2 Ben               2    20        10               33.3            34.5
#> 3 Moderator         2    20        10               33.3            34.5
```
