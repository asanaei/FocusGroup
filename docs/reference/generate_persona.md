# Generate a participant persona from demographics and survey responses

Renders the persona text for a synthetic participant from the
demographics and survey responses the researcher supplied. It states
those facts and nothing more: what a given age, education, place of
residence, or survey answer implies about a person is left to the model,
not decided here. Mapping a demographic label to a fixed disposition is
the essentialism a research instrument should avoid, so the package does
not do it.

## Usage

``` r
generate_persona(
  demographics,
  survey_responses = NULL,
  style = getOption("focusgroup.persona_style", "labeled")
)
```

## Arguments

- demographics:

  A named list (or single-row data frame coerced to one) of demographic
  fields, e.g. \`list(age = 63, education = "High school", region =
  "South")\`. Field names are shown to the model as written.

- survey_responses:

  Optional named list of survey responses, keyed by the question text:
  \`list("Party identification" = "Strong Democrat")\`.

- style:

  One of \`"labeled"\` (default) or \`"paragraph"\`. \`"labeled"\` lists
  the demographics and then a Question/Answer block; \`"paragraph"\`
  fuses them into a single natural paragraph. Defaults to
  \`getOption("focusgroup.persona_style", "labeled")\`.

## Value

Character string with the persona description.

## Details

Both the demographics and the survey responses are rendered in full
(every supplied field), not a fixed subset. Pass survey responses keyed
by the question wording (for survey-file input, the variable label is
used as the key) so the model sees the item, not a code name.
