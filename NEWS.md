# FocusGroup 0.4.0

* Bundled example data `anes_2024_personas`: 100 participant profiles derived
  from the ANES 2024 Time Series public release, selected by diversity sampling,
  with demographics coarsened and each respondent's attitude bundle intact. It is
  an ordinary data frame: filter rows and select columns with `dplyr` (column
  names use `demo_` and `att_<block>_` prefixes), then pass the result to the new
  `create_agents_from_data()`. An `ideology_score` column places each respondent
  on a single liberal-to-conservative dimension (a graded-response IRT ideal
  point; rows are sorted by it). The data is a derived product (no respondent
  identifiers, no restricted-use variables, orientation/identity items excluded);
  see `inst/NOTICE` and cite ANES.
* The Run tab of the Shiny GUI can draw participants from `anes_2024_personas`:
  a selectable table lists the profiles from most liberal to most conservative,
  and the chosen rows become the focus group (select none to draw a diverse
  sample).
* `generate_persona()` rewritten. It renders every supplied demographic field
  AND the survey responses (previously only age/gender/education reached the
  prompt; survey answers were dropped). It states those facts and draws no
  inferences -- mapping a demographic label to a fixed disposition is removed
  entirely. New `style` argument / `options(focusgroup.persona_style=)` selects a
  `"labeled"` block (default) or a one-paragraph `"paragraph"` rendering.
* `create_agents_from_survey()` reworked: survey answers are keyed by each
  variable's question wording (its label in the file), new `rows` (predicate or
  index) and `weights` arguments select and weight respondents, a configurable
  `na_strings` controls the missing-value vocabulary, and the dataset-specific
  detection heuristics are gone (it works on ANES, GSS, WVS, or any labeled
  file). A row-misalignment bug between demographics and survey answers is fixed,
  as is a single-column frame collapse that dropped one-variable selections.
* The participant rule for self-disclosure is now configurable. By default a
  participant asked whether it is an AI answers truthfully that this is a
  simulated exercise; `options(focusgroup.disclosure = "conceal")` restores the
  previous "do not reveal" instruction, and `"silent"` omits the line. The full
  participant and moderator rule blocks can be replaced with
  `options(focusgroup.participant_rules=)` / `options(focusgroup.moderator_rules=)`.
* An utterance is retried only when the model was cut off (`finish_reason ==
  "length"`), returned nothing, or trailed off with an ellipsis. A short,
  well-formed reply is no longer padded.
* `run_focus_group()` records the provider and model it actually used in
  `config_meta` even when the caller relied on the default configuration.
* The "Simulation Concluded" console line now prints on normal termination
  (script exhausted or moderator close), which previously it did not.

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
