## Resubmission

Resubmission of FocusGroup, bumped to 0.5.2 (0.5.1 was never published).
The July pretest declined 0.5.1 with a WARNING because four objects its GUI
uses were not exported by the LLMR.shiny then on CRAN; LLMR.shiny 0.1.2,
which exports them, is on CRAN now, and the Suggests entry states the
requirement (LLMR.shiny >= 0.1.2).

Version 0.5.2 also incorporates corrections from an external methodological
review: the untouched default now plans 93 model outputs and runs (the old
default planned 467 and tripped its own gate), participant frames are never
recycled and direct personas acquire no synthetic records, survey value
cleanup no longer alters substance, continuation comparisons cannot inherit
a post-cut summary, saved Studio sessions are sanitized of configurations
and runner closures, and a failed final summary returns the completed
transcript.

The pretest NOTE flagged "LLM" in the Description as possibly misspelled;
it is the standard initialism for large language model.

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
