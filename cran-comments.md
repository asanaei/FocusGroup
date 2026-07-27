## Resubmission

Resubmission of FocusGroup 0.5.1. The incoming pretest of 2026-07-27 gave one
WARNING under "checking dependencies in R code": four objects used by the
optional GUI (LLMR.shiny::guess_column, LLMR.shiny::help_tip,
LLMR.shiny::llmr_theme, LLMR.shiny::text_block_output) are not exported by
LLMR.shiny 0.1.1, the version the pretest machines held. LLMR.shiny 0.1.2
exports them. The Suggests entry now states the version requirement,
LLMR.shiny (>= 0.1.2).

The accompanying NOTE flagged "LLM" in the Description as possibly
misspelled; it is the standard initialism for large language model.

## The package

FocusGroup simulates moderated focus group discussions in which the
participants are LLM agents: a moderator poses scripted questions,
participants take turns under a configurable turn-taking rule, and the
package records the conversation and analyzes it (participation statistics,
topic modeling, TF-IDF, readability, and LLM-assisted thematic analysis).

FocusGroup Imports LLMR (on CRAN) for all model calls. It Suggests
LLMR.shiny (>= 0.1.2) for the optional GUI; every use of LLMR.shiny and of
the other suggested packages is guarded with requireNamespace() and skipped
when the package is absent. The test suite is fully offline (all LLM entry
points are mocked); no test, example, or vignette makes a network call.

## Test environments

- local macOS (Darwin 25.5.0), R 4.4.3
- R CMD check --as-cran on the built tarball

## R CMD check results

0 errors | 0 warnings | 3 notes

- "checking CRAN incoming feasibility ... NOTE: New submission".
- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; the check machine had no access to a time service.
- "checking HTML version of manual ... NOTE": emitted by an older system
  `tidy` that does not recognize the HTML5 elements R generates; it does not
  reproduce on CRAN.
