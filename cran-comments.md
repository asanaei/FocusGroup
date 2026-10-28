## Update of FocusGroup 0.5.2

This is an update of FocusGroup 0.5.2, which is on CRAN. Versions 0.5.3 to
0.5.5 were development versions and never submitted; NEWS.md lists their
changes under their own headings. Among them are corrections of results that
were wrong without any warning:

- Survey answers without a value label were dropped from personas, missing-
  data labels such as "-9. Refused" were shown as answers, labels cut at
  Stata's 80 characters went unnoticed, and numbers were written as "1e+05".
- `create_agents_from_survey()` applied its `rows` selector after dropping
  respondents, and a sampling-weight column was rendered as an answer.
- Under desire-based turn-taking, the participant who closed one question
  could not open the next, and the participant a moderator addressed by name
  did not get the turn.
- The agents saw the identifiers a user gave them; they now see neutral
  labels.
- `summarize()` reused a session's scripted runner, and a provider's retries
  changed later random draws.

New in these versions: the moderator calls on participants who have not
spoken, `fg_player()` writes a session to a self-contained HTML file,
`summarize_focus_group()` checks every quoted claim against the transcript in
code, `format()` writes a session as a JSON Lines file that
`focus_group_from_transcript()` reads back, and `run_focus_group_arms()`
continues a recorded session under alternative versions of a turn.

## Data

The package ships `anes_2024_personas` (a 25 KB `.rda`): 100 respondents of
the ANES 2024 Time Series Study, public release, with the source and
citation given in its help page. The same data were bundled with LLMR under
the same name; they now live with the two packages that use them
(FocusGroup and LLMRpanel), rebuilt from the May 19, 2026 release, and LLMR
will drop its copy in its next release. The build script is in the package
sources (`data-raw/`, excluded from the build).

## Dependencies

FocusGroup Imports LLMR (>= 0.8.8) and, new in this release, jsonlite. It
Suggests LLMR.shiny (>= 0.1.3) for the optional Studio; LLMR.shiny 0.1.3 is
submitted to CRAN ahead of this package. Every use of LLMR.shiny and of the
other suggested packages is guarded with `requireNamespace()`.

## Examples and tests

The examples run offline, through the package's `.runner` argument or the
bundled data; `fg_player()` writes to `tempfile()` and does not open a
browser outside interactive use. No example or test makes a network call,
and the test suite mocks every model call.

## Test environments

- local macOS 26.7.1 (aarch64-apple-darwin20), R 4.4.3: `R CMD check
  --as-cran` on the built tarball, against LLMR 0.9.0 (the CRAN version) and
  LLMR.shiny 0.1.3.

## R CMD check results

0 errors | 0 warnings | 1 note

- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; the check machine had no access to a time service.

## Reverse dependencies

None on CRAN.
