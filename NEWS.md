# FocusGroup 0.3.0

* Added an optional Shiny GUI, launched with `run_focus_studio()`, with three
  tabs. Run a focus group starts a fresh moderated session live and offers it as
  a downloadable `.rds`. Analyze loads a transcript and reads its participation
  and word statistics offline. The continuation experiment takes a saved focus
  group up to a turn, perturbs an earlier turn's text, and generates the next
  turn under the original and perturbed histories, so the downstream turn can be
  compared as a dependent variable. The experiment is built on the existing
  public API (`generate_utterance()` on a supplied history string, with the
  target agent cloned per branch); no existing behavior changes. The GUI's
  dependencies (`shiny`, `bslib`, `DT`, and the shared `LLMR.shiny` substrate)
  are Suggests, and the launcher guards on all four, so non-GUI users are
  unaffected.
