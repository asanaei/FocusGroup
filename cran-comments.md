## Update of FocusGroup 0.5.2

This is an update of FocusGroup 0.5.2, which is on CRAN. Version 0.5.3
corrects results that were wrong without any warning:

- `create_agents_from_survey()` applied its `rows` selector after dropping
  respondents without demographic values, so it could return other
  respondents than the ones selected.
- A sampling-weight column was rendered into the personas as if it were one
  of the respondent's answers.
- Invalid sampling weights and row selectors were repaired silently, which
  changed the sample; they are now refused with an error that names the
  input.
- The documented default of two participant responses per question did not
  apply, so an untouched `run_focus_group()` call exceeded its own
  `max_calls` limit.
- A session that failed partway lost the turns it had completed; they are
  now returned with the error.
- Token usage that a provider did not report was recorded as zero; it is now
  unknown (`NA`), and results count the calls whose usage is unknown.
- Imported transcripts kept only speaker and text, and the optional Studio
  read uploaded CSV files with type inference, which merged speaker ids such
  as `001` and `01`.

NEWS.md lists each change in behavior and names each new error.

## Dependencies

FocusGroup Imports LLMR (>= 0.8.8) for all model calls. It Suggests
LLMR.shiny (>= 0.1.3) for the optional Studio, and so requires LLMR.shiny
0.1.3, which is submitted to CRAN ahead of this package. The Studio now uses
that version's usage counters and its options for reading a CSV file without
type inference. It checks the installed LLMR.shiny version and refuses to
start with an older one. Every use of LLMR.shiny and of the other suggested
packages is guarded with `requireNamespace()`.

## Examples and tests

The examples run offline, through the package's `.runner` argument or the
bundled ANES records. The `\donttest{}` block of 0.5.2, a live session behind
a check for `GROQ_API_KEY`, has been removed, so no example makes a network
call in any environment. The test suite mocks every model call.

## Test environments

- local macOS 26.7.1 (aarch64-apple-darwin20), R 4.4.3:
  `R CMD check --as-cran` on the built tarball, once against LLMR 0.8.11
  (the CRAN version) and once against LLMR 0.8.14, which is submitted ahead
  of this package, both with LLMR.shiny 0.1.3.

## R CMD check results

0 errors | 0 warnings | 1 note

- "checking for future file timestamps ... NOTE: unable to verify current
  time". Environmental; the check machine had no access to a time service.

## Reverse dependencies

None on CRAN.
