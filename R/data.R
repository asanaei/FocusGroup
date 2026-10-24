# data.R ----------------------------------------------------------------------
# Documentation for the bundled dataset `anes_2024_personas`, stored in
# data/anes_2024_personas.rda. It is built by
# data-raw/anes_2024_personas/make_personas.R in the FocusGroup sources, which
# writes the same file into LLMRpanel; tests/testthat/test-anes-personas.R is
# shared by both packages.

#' Persona profiles of 100 respondents to the ANES 2024 Time Series Study
#'
#' A data frame describing 100 respondents to the American National Election
#' Studies (ANES) 2024 Time Series Study by their demographics and their
#' political attitudes, written as text a language model can read. It serves
#' as ready-made persona material: [create_agents_from_data()] builds
#' participants from any subset of its rows and columns. LLMRpanel ships an
#' identical copy.
#'
#' @format A data frame with 100 rows and 123 columns. Each row is one
#'   respondent; no ANES case identifier is kept. Column names are handles
#'   whose prefix names a block, so `startsWith(names(x), "att_iss_")` selects
#'   the issue positions:
#'   \describe{
#'     \item{`demo_`}{17 demographic fields: age band, sex, education,
#'       race/ethnicity, marital status, household income, religion and its
#'       importance, religious attendance, census region, rural-urban
#'       self-identification, employment status, home ownership, children in
#'       the household, union household, military service, and attention to
#'       politics.}
#'     \item{`att_id_`}{9 fields of party identification, ideology, and vote
#'       choice.}
#'     \item{`att_aff_`}{18 feeling thermometers toward candidates, parties,
#'       institutions, and groups.}
#'     \item{`att_eval_`}{14 evaluations: approval of the President, Congress,
#'       and the Supreme Court, the economy, personal finances, feelings about
#'       the country, and which party would better handle the economy and
#'       immigration.}
#'     \item{`att_iss_`}{47 issue positions, among them taxes and spending,
#'       immigration, guns, climate, health care, abortion, policing, race,
#'       and foreign policy.}
#'     \item{`att_val_`}{17 fields of trust, values, and views of democracy.}
#'     \item{`ideology_score`}{A number placing the respondent on one
#'       liberal-conservative dimension (see Details); the rows are sorted by
#'       it.}
#'   }
#'   Every column but `ideology_score` holds character values:
#'   \itemize{
#'     \item An answer with a value label is written as that label without its
#'       code ("Strong Democrat", "Favor a great deal"). Parenthetical
#'       qualifiers are kept, the interviewer notes "\{VOL\}" and
#'       "\{SPECIFY\}" are removed, and the four labels of the immigration
#'       policy item (V241386) that the Stata file cuts at 80 characters are
#'       restored in full from the codebook.
#'     \item The six 7-point scales that ANES labels only at their ends are
#'       written as sentences, endpoints included: "5 on a scale from 1
#'       (Government should provide many fewer services) to 7 (Government
#'       should provide many more services)".
#'     \item Feeling thermometers are written as "70 on a 0-100 feeling
#'       thermometer".
#'     \item "Haven't thought much about this" is kept as an answer where ANES
#'       offered it.
#'     \item Age is given in six bands and children in the household as "Yes"
#'       or "No". Employment status is written from the full ANES labels
#'       without their "R " prefix ("Retired (if also working, working < 20
#'       hrs/wk)"). Religious attendance is "Never" for respondents who said
#'       they never attend services (V241439 = 2), who were not asked how
#'       often (V241440).
#'     \item `NA` marks a missing-data code: refused, don't know,
#'       inapplicable, no post-election interview, and so on, and a
#'       thermometer rating outside 0-100 ("don't know where to rate", "don't
#'       recognize"). The 100 rows hold 246 such cells, most of them in the
#'       vote items, which do not apply to everyone.
#'   }
#'
#'   The data frame carries these attributes:
#'   \describe{
#'     \item{`dictionary`}{A data frame with one row per column except
#'       `ideology_score`: `handle`; `question`, the short phrase shown with
#'       an answer ("Abortion position"); `anes_variable`, the ANES variable;
#'       `domain`, "demographic" or the attitude block; `wording`, the
#'       question as printed in the ANES codebook; `wording_variable` and
#'       `wording_page`, the variable whose question that is and the codebook
#'       page where it begins; and `wording_status`. A status of "verbatim"
#'       means the wording is the variable's own question; "from_source_item"
#'       means the variable is a summary without a question of its own and
#'       the wording is that of the item it summarizes (for party
#'       identification, V241221); "stem_only" means the codebook prints only
#'       a shared stem ("Where would you place yourself on this scale, or
#'       haven't you thought much about this?") and the subject of the scale
#'       is in its end labels; "none" means no single question exists (census
#'       region, age), and `wording` is `NA`.}
#'     \item{`demographic_fields`}{The handles of the demographic columns.}
#'     \item{`n_demographics`, `n_attitudes`}{17 and 105.}
#'     \item{`source`, `citation`}{The citation of the ANES release.}
#'     \item{`build`}{The R and package versions, the md5 sums of the two ANES
#'       files, the seed, and the label overrides used to build the data.}
#'     \item{`build_audit`}{For each column, the number of `NA` cells and the
#'       number of missing-data codes in the raw file for the same
#'       respondents; the two are equal.}
#'   }
#'
#' @details
#' The respondents were drawn from the 4,710 of 5,521 who completed both the
#' pre-election and the post-election interview, neither of them on paper,
#' and answered at least 75 percent of the 122 fields. For each code of party
#' identification, race/ethnicity, and liberal-conservative self-placement
#' not yet covered, one respondent with that code was drawn at random (seed
#' 110); the remaining places were filled by greedy maximin selection, each
#' time adding the respondent farthest from those already chosen, under a
#' Gower distance in which the demographic, identity, thermometer,
#' evaluation, issue, and value blocks carry weights 0.25, 0.25, 0.20, 0.15,
#' 0.15, and 0.15. Each respondent's answers are kept together.
#'
#' The sample spans unusual combinations of answers and does not reproduce
#' population proportions. It carries no survey weights and supports no
#' population inference. Maximin selection over-represents people with no
#' 2024 presidential vote recorded (38 of the 100, against 18.5 percent of
#' the eligible respondents), and within each party it favors members whose
#' other answers depart from the party's usual positions.
#'
#' `ideology_score` comes from a one-dimensional graded-response item response
#' model fitted to 89 attitude items over the 4,710 eligible respondents.
#' Categories enter in the order of their ANES codes (thermometers grouped
#' into five bands, off-scale answers such as "Haven't thought much about
#' this" left out), and items are reversed where they correlate negatively
#' with party identification and ideology. The expected a posteriori scores
#' are standardized over the eligible respondents and oriented so that low is
#' liberal and high is conservative. They correlate 0.89 with party
#' identification and ideology among the eligible respondents and 0.58 among
#' the 100, whose scores have a standard deviation of 0.55.
#'
#' Items on sexual orientation and gender identity are excluded by design, as
#' are restricted-use variables, geography below the census region,
#' open-ended answers, and items asked only of the spouse/partner sample, only
#' on paper, or only of the GSS re-interview sample, which this release does
#' not include.
#'
#' @source American National Election Studies. 2026. ANES 2024 Time Series
#'   Study Full Release \[dataset and documentation\]. May 19, 2026 version.
#'   \url{https://www.electionstudies.org}. Work that uses these profiles
#'   should cite ANES. ANES bears no responsibility for the analyses or
#'   interpretations presented here.
#' @keywords datasets
#' @examples
#' dim(anes_2024_personas)
#' dictionary <- attr(anes_2024_personas, "dictionary")
#' dictionary[dictionary$handle == "att_iss_abortion_position",
#'            c("question", "wording", "wording_status")]
#' table(anes_2024_personas$att_id_party_identification)
#' issues <- anes_2024_personas[, startsWith(names(anes_2024_personas), "att_iss_")]
#' ncol(issues)
"anes_2024_personas"
