## Submission

This is the first CRAN submission of FocusGroup (0.5.1). The package simulates
moderated focus group discussions in which the participants are LLM agents: a
moderator poses scripted questions, participants take turns under a
configurable turn-taking rule, and the package records the conversation and
analyzes it (participation statistics, topic modeling, TF-IDF, readability,
and LLM-assisted thematic analysis). It is intended for design-stage research,
such as piloting a moderator guide before fielding a study with human
participants.

FocusGroup Imports LLMR (on CRAN) for all model calls. It Suggests LLMR.shiny
(on CRAN) for the optional GUI; every use of LLMR.shiny
and of the other suggested packages is guarded with requireNamespace() and
skipped when the package is absent. The test suite is fully offline (all LLM
entry points are mocked); no test, example, or vignette makes a network call.

## Test environments

- local macOS (Darwin 25.5.0), R 4.4.3
- R CMD check --as-cran

## R CMD check results

0 errors | 0 warnings | 3 notes

- "checking CRAN incoming feasibility ... NOTE: New submission" and
  "Suggests or Enhances not in mainstream repositories: LLMR.shiny".
  This is a first submission; LLMR.shiny is submitted to CRAN in sequence and
  all uses are guarded as described above.
- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; does not reproduce on CRAN.
- "checking HTML version of manual ... NOTE": emitted by an older
  system `tidy` that does not recognize the HTML5 elements R generates;
  it does not reproduce on CRAN.
