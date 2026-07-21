# Launch the FocusGroup Shiny GUI

A point-and-click front end with three tabs. Run a focus group starts a
fresh moderated session live from a topic and a handful of participants,
shows the transcript, and offers it as a downloadable `.rds`. Analyze
loads a transcript and reads its participation and word statistics
offline. The continuation experiment takes a saved focus group up to a
message, perturbs an earlier message, and generates the next message
under the original and perturbed histories, so the downstream message
can be compared. Running and the continuation experiment generate text
and need an API key; analysis works offline.

## Usage

``` r
run_focus_studio(...)
```

## Arguments

- ...:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly, the value of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html); called
for the side effect of starting the app.

## Details

The GUI is optional. It needs the suggested packages shiny, bslib, DT,
and LLMR.shiny; install them first. Keys are read from environment
variables only, never pasted into the app.

## Examples

``` r
if (interactive() &&
    all(vapply(c("shiny", "bslib", "DT", "LLMR.shiny"),
               requireNamespace, logical(1), quietly = TRUE))) {
  run_focus_studio()
}
```
