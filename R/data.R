#' Example participant profiles derived from ANES 2024
#'
#' A ready-to-use set of 100 participant profiles for building example focus
#' groups without downloading any data. Each row is one respondent from the
#' American National Election Studies (ANES) 2024 Time Series Study, with
#' demographics and a broad battery of political and social attitudes decoded to
#' their value labels. [generate_persona()] turns a row into persona text, and
#' [create_agents_from_data()] turns the whole frame into a set of agents.
#'
#' @format A data frame with 100 rows and 125 columns. It is an ordinary data
#'   frame: select columns and filter rows with `dplyr` or base R, then pass the
#'   result to [create_agents_from_data()].
#'
#'   Column names are short, tidy-select-friendly handles. Demographics use a
#'   `demo_` prefix (`demo_age`, `demo_race_ethnicity`, ...). Attitudes use an
#'   `att_<block>_` prefix grouping them into a few blocks, so
#'   `dplyr::select(starts_with("att_iss_"))` grabs a whole block:
#'   \itemize{
#'     \item `att_id_` -- political identity (party id, ideology, vote)
#'     \item `att_aff_` -- feeling thermometers (candidates, parties, groups)
#'     \item `att_eval_` -- approval, the economy, personal finances, national mood
#'     \item `att_iss_` -- issue positions (taxes, immigration, guns, climate,
#'       health care, abortion, crime, race, foreign policy, trade, ...)
#'     \item `att_val_` -- trust, social trust, values, democracy
#'   }
#'   One numeric column, `ideology_score`, is a single conservative--liberal
#'   dimension (see Details); the rows are sorted by it. Attitude values are
#'   character labels (`NA` when missing or not applicable). The data frame
#'   carries a `"dictionary"` attribute (a data frame mapping each handle to its
#'   question wording, ANES variable code, and block) and a `"demographic_fields"`
#'   attribute. [create_agents_from_data()] reads the dictionary so personas show
#'   the question wording, not the handle.
#'
#' @details
#' The 100 respondents were chosen by diversity sampling (a greedy maximin pass
#' over a Gower distance, after dropping respondents missing more than a quarter
#' of the fields), so the set spans the range of demographic and attitudinal
#' profiles rather than reproducing population proportions. It is example
#' material for instrument piloting and turn-level studies. It is NOT a
#' representative sample of the United States, carries no survey weights, and
#' should not be used for population inference.
#'
#' `ideology_score` is a unidimensional ideal point estimated from a graded-
#' response item-response model over the ordinal attitude items, standardized to
#' roughly mean 0 and unit scale, and oriented so that low is liberal and high is
#' conservative (it agrees with the first principal component at r ~= 0.99). It is
#' provided to make ordering and filtering easy (for example, the most
#' conservative quartile is `ideology_score > quantile(ideology_score, 0.75)`).
#' Like the rest of the data, it is a property of this 100-person example, not a
#' calibrated population measure.
#'
#' Each respondent's own bundle of answers is kept intact (answers are not
#' shuffled or perturbed across people), which is what makes a profile read as a
#' coherent person. Demographics are coarsened (age in bands, broad income and
#' race or ethnicity categories, census region rather than state); no respondent
#' identifiers, granular geography, open-ended text, or restricted-use variables
#' are included. Items concerning sexual orientation and gender identity are
#' excluded by design.
#'
#' @source Derived from the American National Election Studies. 2025. *ANES 2024
#'   Time Series Study Full Release* \[dataset and documentation\]. August 8,
#'   2025. \url{https://electionstudies.org/data-center/2024-time-series-study/}.
#'   This is a derived product of the ANES public release and is distributed for
#'   example use; it contains no ANES respondent identifiers and no restricted-use
#'   data. Work that uses these personas should cite ANES as above. The ANES bears
#'   no responsibility for the analyses or interpretations presented here.
#' @examples
#' data(anes_2024_personas)
#' nrow(anes_2024_personas)
#' # the data frame is the interface: filter rows, select columns, then build agents
#' \dontrun{
#'   library(dplyr)
#'   # six conservative-leaning participants, immigration and economy items only
#'   anes_2024_personas |>
#'     filter(ideology_score > 0.5) |>
#'     select(starts_with("demo_"), starts_with("att_iss_imm"), starts_with("att_eval_")) |>
#'     head(6) |>
#'     create_agents_from_data(n_participants = 6)
#'
#'   # or just take the whole thing
#'   create_agents_from_data(anes_2024_personas, n_participants = 8)
#' }
"anes_2024_personas"
