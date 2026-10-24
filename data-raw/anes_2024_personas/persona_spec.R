# persona_spec.R --------------------------------------------------------------
# Which ANES 2024 Time Series variables become fields of the bundled dataset
# `anes_2024_personas`, and how each one is decoded. Sourced by
# make_personas.R; see README.md in this folder for the reasons behind each
# choice.
#
# One row per field:
#   handle     column name in the shipped data frame
#   variable   ANES variable read from the Stata file
#   question   short phrase shown with the answer (the dictionary's `question`)
#   domain     "demographic", or the attitude block: "identity", "affect",
#              "evaluation", "issue", "values"
#   type       how values are written:
#                "label"       the value label, code prefix and {VOL}/{SPECIFY}
#                              removed, parentheticals kept
#                "scale"       a 7-point scale labeled only at its ends:
#                              "5 on a scale from 1 (<label 1>) to 7 (<label 7>)"
#                "thermometer" "70 on a 0-100 feeling thermometer"
#                "age_band"    age in years coarsened to six bands
#                "any_children" number of children coarsened to Yes/No
#                "recode"      an explicit table of codes (`anes_recodes`)
#                "attendance"  V241440, with V241439 = 2 (never attends) as
#                              "Never"
#   level      how the field enters the diversity distance and the IRT model:
#                "ordinal" (categories in the order of their ANES codes, or of
#                `order`), "numeric" (scales and thermometers, by value), or
#                "nominal" (categories only match or differ)
#   order      for an ordinal field whose codes are not in scale order, the
#              codes from one end of the scale to the other, e.g. "1,3,2" for
#              Favor (1), Neither (3), Oppose (2); empty otherwise
#   off_scale  codes that are real answers but lie off the scale ("Haven't
#              thought much about this", "Side with neither", "Neither",
#              "Something else"); they are shown as answers and left out of
#              the distance and the IRT model
#   irt        whether the item enters the IRT model of `ideology_score`

.spec_row <- function(handle, variable, question, domain, type = "label",
                      level = "ordinal", order = "", off_scale = "", irt = FALSE) {
  data.frame(handle = handle, variable = variable, question = question,
             domain = domain, type = type, level = level, order = order,
             off_scale = off_scale, irt = irt, stringsAsFactors = FALSE)
}

anes_fields <- do.call(rbind, list(
  # Demographics ---------------------------------------------------------------
  .spec_row("demo_age", "V241458x", "age", "demographic", "age_band"),
  .spec_row("demo_sex", "V241550", "sex", "demographic", level = "nominal"),
  .spec_row("demo_education", "V241465x", "education", "demographic"),
  .spec_row("demo_race_ethnicity", "V241501x", "race/ethnicity", "demographic", level = "nominal"),
  .spec_row("demo_marital_status", "V241461x", "marital status", "demographic", level = "nominal"),
  .spec_row("demo_household_income", "V241567x", "household income", "demographic"),
  .spec_row("demo_religion", "V241445x", "religion", "demographic", level = "nominal"),
  .spec_row("demo_religion_importance", "V241420", "religion importance", "demographic"),
  .spec_row("demo_religious_attendance", "V241440", "religious attendance", "demographic", "attendance"),
  .spec_row("demo_census_region", "V243007", "census region", "demographic", level = "nominal"),
  .spec_row("demo_rural_urban_identity", "V242341", "rural-urban self-identification",
            "demographic", off_scale = "5"),
  .spec_row("demo_employment_status", "V241488x", "employment status", "demographic",
            "recode", level = "nominal"),
  .spec_row("demo_home_ownership", "V241530", "home ownership", "demographic", level = "nominal"),
  .spec_row("demo_children_in_household", "V241521", "children in household", "demographic",
            "any_children", level = "nominal"),
  .spec_row("demo_union_household", "V241497", "union household", "demographic", level = "nominal"),
  .spec_row("demo_military_service", "V241470", "military service", "demographic", level = "nominal"),
  .spec_row("demo_attention_to_politics", "V241004", "attention to politics", "demographic"),

  # Party, ideology, vote --------------------------------------------------------
  .spec_row("att_id_party_identification", "V241227x", "Party identification", "identity", irt = TRUE),
  .spec_row("att_id_importance_of_party_identity", "V241228", "Importance of party identity", "identity"),
  .spec_row("att_id_liberal_conservative_self_pl", "V241177", "Liberal-conservative self-placement",
            "identity", off_scale = "99", irt = TRUE),
  .spec_row("att_id_2024_presidential_vote", "V242096x", "2024 presidential vote", "identity",
            level = "nominal"),
  .spec_row("att_id_presidential_vote_intent", "V241043", "Presidential vote intent", "identity",
            level = "nominal"),
  .spec_row("att_id_harris_vs_trump_preference", "V241049", "Harris vs Trump preference", "identity",
            level = "nominal"),
  .spec_row("att_id_likelihood_of_voting", "V241100", "Likelihood of voting", "identity"),
  .spec_row("att_id_party_of_presidential_vote_i", "V241075x", "Party of presidential vote/intent",
            "identity", level = "nominal"),
  .spec_row("att_id_party_of_us_house_vote_inten", "V241076x", "Party of US House vote/intent",
            "identity", level = "nominal"),

  # Feeling thermometers -----------------------------------------------------------
  .spec_row("att_aff_feeling_toward_kamala_harris", "V241156", "Feeling toward Kamala Harris",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_donald_trump", "V241157", "Feeling toward Donald Trump",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_the_democrati", "V241166", "Feeling toward the Democratic Party",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_the_republica", "V241167", "Feeling toward the Republican Party",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_the_maga_move", "V242148", "Feeling toward the MAGA movement",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_the_supreme_c", "V242143", "Feeling toward the Supreme Court",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_congress", "V242145", "Feeling toward Congress",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_liberals", "V242139", "Feeling toward liberals",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_conservatives", "V242142", "Feeling toward conservatives",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_labor_unions", "V242140", "Feeling toward labor unions",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_big_business", "V242141", "Feeling toward big business",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_the_police", "V242150", "Feeling toward the police",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_black_lives_m", "V242152", "Feeling toward Black Lives Matter",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_the_nra", "V242153", "Feeling toward the NRA",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_muslims", "V242146", "Feeling toward Muslims",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_christians", "V242147", "Feeling toward Christians",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_jews", "V242149", "Feeling toward Jews",
            "affect", "thermometer", "numeric", irt = TRUE),
  .spec_row("att_aff_feeling_toward_planned_paren", "V242156", "Feeling toward Planned Parenthood",
            "affect", "thermometer", "numeric", irt = TRUE),

  # Approval -------------------------------------------------------------------------
  .spec_row("att_eval_approval_of_the_president", "V241137x", "Approval of the President",
            "evaluation", irt = TRUE),
  .spec_row("att_eval_approval_of_congress", "V241129x", "Approval of Congress", "evaluation", irt = TRUE),
  .spec_row("att_eval_approval_of_the_supreme_cour", "V241133x", "Approval of the Supreme Court",
            "evaluation", irt = TRUE),
  .spec_row("att_eval_approval_of_the_president_on", "V241143x",
            "Approval of the President on the economy", "evaluation", irt = TRUE),
  .spec_row("att_eval_approval_of_the_president_on_2", "V241152x",
            "Approval of the President on immigration", "evaluation", irt = TRUE),
  .spec_row("att_eval_approval_of_the_president_on_3", "V241149x",
            "Approval of the President on abortion", "evaluation", irt = TRUE),

  # Economy and personal finances ------------------------------------------------------
  .spec_row("att_eval_national_economy_over_the_pa", "V241294x", "National economy over the past year",
            "evaluation", irt = TRUE),
  .spec_row("att_eval_national_economy_over_the_ne", "V241297x", "National economy over the next year",
            "evaluation", irt = TRUE),
  .spec_row("att_eval_personal_finances_vs_a_year_", "V241451", "Personal finances vs a year ago",
            "evaluation"),
  .spec_row("att_eval_worry_about_personal_finance", "V241539", "Worry about personal finances",
            "evaluation"),

  # Taxes, spending, redistribution --------------------------------------------------------
  .spec_row("att_iss_government_spending_and_serv", "V241239", "Government spending and services",
            "issue", "scale", "numeric", off_scale = "99", irt = TRUE),
  .spec_row("att_iss_federal_spending_on_social_s", "V241263x", "Federal spending on Social Security",
            "issue", irt = TRUE),
  .spec_row("att_iss_federal_spending_on_aid_to_t", "V241281x", "Federal spending on aid to the poor",
            "issue", irt = TRUE),
  .spec_row("att_iss_federal_spending_on_welfare", "V241275x", "Federal spending on welfare",
            "issue", irt = TRUE),
  .spec_row("att_iss_tax_on_millionaires", "V242316a", "Tax on millionaires", "issue",
            order = "1,3,2", irt = TRUE),
  .spec_row("att_iss_government_reducing_income_i", "V242253x", "Government reducing income inequality",
            "issue", irt = TRUE),

  # Immigration ---------------------------------------------------------------------------
  .spec_row("att_iss_policy_toward_unauthorized_i", "V241386", "Policy toward unauthorized immigrants",
            "issue", irt = TRUE),
  .spec_row("att_iss_border_wall", "V241395x", "Border wall", "issue", irt = TRUE),
  .spec_row("att_iss_ending_birthright_citizenshi", "V241389x", "Ending birthright citizenship",
            "issue", irt = TRUE),
  .spec_row("att_iss_path_to_citizenship", "V242234x", "Path to citizenship", "issue", irt = TRUE),
  .spec_row("att_iss_immigrants_and_the_economy", "V242235", "Immigrants and the economy",
            "issue", irt = TRUE),

  # Guns ------------------------------------------------------------------------------------
  .spec_row("att_iss_gun_background_checks", "V242328x", "Gun background checks", "issue", irt = TRUE),
  .spec_row("att_iss_difficulty_of_buying_a_gun", "V242325", "Difficulty of buying a gun", "issue",
            order = "1,3,2", irt = TRUE),

  # Climate and environment -------------------------------------------------------------------
  .spec_row("att_iss_regulating_greenhouse_emissi", "V242324x", "Regulating greenhouse emissions",
            "issue", irt = TRUE),
  .spec_row("att_iss_climate_change_and_severe_we", "V242321", "Climate change and severe weather",
            "issue", irt = TRUE),
  .spec_row("att_iss_environment_vs_business_trad", "V241258", "Environment vs business tradeoff",
            "issue", "scale", "numeric", off_scale = "99", irt = TRUE),

  # Health care -------------------------------------------------------------------------------
  .spec_row("att_iss_government_help_paying_for_h", "V242353x", "Government help paying for health care",
            "issue", irt = TRUE),
  .spec_row("att_iss_government_vs_private_medica", "V241245", "Government vs private medical insurance",
            "issue", "scale", "numeric", off_scale = "99", irt = TRUE),

  # Abortion ----------------------------------------------------------------------------------
  .spec_row("att_iss_abortion_position", "V241248", "Abortion position", "issue", "scale", "numeric",
            off_scale = "99", irt = TRUE),
  .spec_row("att_iss_importance_of_the_abortion_i", "V241303", "Importance of the abortion issue", "issue"),

  # Crime and policing ------------------------------------------------------------------------
  .spec_row("att_iss_death_penalty", "V241308x", "Death penalty", "issue", irt = TRUE),
  .spec_row("att_iss_police_use_of_force", "V242336", "Police use of force", "issue", irt = TRUE),
  .spec_row("att_iss_whether_police_treat_blacks_", "V242525x",
            "Whether police treat blacks or whites better", "issue", irt = TRUE),

  # Racial policy and attitudes -----------------------------------------------------------------
  .spec_row("att_iss_preferential_hiring_of_black", "V242241x", "Preferential hiring of blacks",
            "issue", irt = TRUE),
  .spec_row("att_iss_affirmative_action_in_univer", "V242245x", "Affirmative action in universities",
            "issue", irt = TRUE),
  .spec_row("att_iss_government_assistance_to_bla", "V241255", "Government assistance to blacks",
            "issue", "scale", "numeric", off_scale = "99", irt = TRUE),
  .spec_row("att_iss_blacks_should_work_their_way", "V242300",
            "Blacks should work their way up without favors", "issue", irt = TRUE),
  .spec_row("att_iss_slavery_makes_it_harder_for_", "V242301", "Slavery makes it harder for blacks",
            "issue", irt = TRUE),
  .spec_row("att_iss_blacks_have_gotten_less_than", "V242302", "Blacks have gotten less than they deserve",
            "issue", irt = TRUE),
  .spec_row("att_iss_if_blacks_tried_harder_they_", "V242303",
            "If blacks tried harder they would be as well off", "issue", irt = TRUE),
  .spec_row("att_iss_discrimination_against_black", "V242549", "Discrimination against blacks",
            "issue", irt = TRUE),
  .spec_row("att_iss_discrimination_against_white", "V242552", "Discrimination against whites",
            "issue", irt = TRUE),
  .spec_row("att_iss_discrimination_against_women", "V242554", "Discrimination against women",
            "issue", irt = TRUE),

  # Foreign policy, defense, trade -----------------------------------------------------------------
  .spec_row("att_iss_us_weapons_to_ukraine", "V241400x", "US weapons to Ukraine", "issue", irt = TRUE),
  .spec_row("att_iss_us_military_aid_to_israel", "V241403x", "US military aid to Israel", "issue",
            irt = TRUE),
  .spec_row("att_iss_side_with_israelis_or_palest", "V241409x", "Side with Israelis or Palestinians",
            "issue", off_scale = "8", irt = TRUE),
  .spec_row("att_iss_free_trade_agreements", "V242346x", "Free trade agreements", "issue", irt = TRUE),
  .spec_row("att_iss_china_as_a_threat", "V242365", "China as a threat", "issue", irt = TRUE),
  .spec_row("att_iss_russia_as_a_threat", "V242366", "Russia as a threat", "issue", irt = TRUE),
  .spec_row("att_iss_defense_spending", "V241242", "Defense spending", "issue", "scale", "numeric",
            off_scale = "99", irt = TRUE),

  # Trust, populism, authoritarianism ------------------------------------------------------------------
  .spec_row("att_val_trust_in_government_in_washi", "V241229", "Trust in government in Washington",
            "values", irt = TRUE),
  .spec_row("att_val_government_run_for_a_few_big", "V241231", "Government run for a few big interests",
            "values", irt = TRUE),
  .spec_row("att_val_whether_the_political_system", "V242304",
            "Whether the political system works for insiders", "values", irt = TRUE),
  .spec_row("att_val_compromise_vs_sticking_to_pr", "V241338x", "Compromise vs sticking to principles",
            "values", irt = TRUE),
  .spec_row("att_val_a_strong_leader_is_good_for_", "V242411", "A strong leader is good for government",
            "values", irt = TRUE),
  .spec_row("att_val_leaving_decisions_to_indepen", "V242414", "Leaving decisions to independent experts",
            "values", irt = TRUE),
  .spec_row("att_val_child_trait_independence_vs_", "V242260", "Child trait: independence vs respect",
            "values", order = "1,3,2", off_scale = "4", irt = TRUE),
  .spec_row("att_val_child_trait_obedience_vs_sel", "V242262", "Child trait: obedience vs self-reliance",
            "values", order = "1,3,2", off_scale = "4", irt = TRUE),

  # Social trust, wellbeing, trust in institutions ----------------------------------------------------
  .spec_row("att_val_whether_people_can_be_truste", "V241234", "Whether people can be trusted", "values"),
  .spec_row("att_val_satisfaction_with_life", "V241621", "Satisfaction with life", "values"),
  .spec_row("att_val_trust_in_the_judiciary", "V242419", "Trust in the judiciary", "values", irt = TRUE),
  .spec_row("att_val_trust_in_scientists", "V242420", "Trust in scientists", "values", irt = TRUE),
  .spec_row("att_val_trust_in_traditional_media", "V242422", "Trust in traditional news media",
            "values", irt = TRUE),

  # Moral traditionalism and gender roles ----------------------------------------------------------------
  .spec_row("att_val_adjusting_morals_to_a_changi", "V242258", "Adjusting morals to a changing world",
            "values", irt = TRUE),
  .spec_row("att_val_traditional_family_values", "V242259", "Traditional family values", "values",
            irt = TRUE),
  .spec_row("att_val_whether_men_should_work_and_", "V242279x",
            "Whether men should work and women keep home", "values", irt = TRUE),

  # Democracy, mobility, mood, party competence, education ------------------------------------------------
  .spec_row("att_val_satisfaction_with_democracy", "V242439", "Satisfaction with democracy", "values",
            irt = TRUE),
  .spec_row("att_iss_economic_mobility_vs_20_year", "V242314x", "Economic mobility vs 20 years ago",
            "issue", irt = TRUE),
  .spec_row("att_iss_income_gap_vs_20_years_ago", "V241360", "Income gap vs 20 years ago", "issue",
            order = "1,3,2", irt = TRUE),
  .spec_row("att_iss_whether_rural_areas_get_thei", "V242269x", "Whether rural areas get their fair share",
            "issue", irt = TRUE),
  .spec_row("att_eval_hopefulness_about_the_countr", "V241118", "Hopefulness about the country",
            "evaluation"),
  .spec_row("att_eval_anger_about_the_country", "V241121", "Anger about the country", "evaluation"),
  .spec_row("att_eval_which_party_better_handles_t", "V241236", "Which party better handles the economy",
            "evaluation", irt = TRUE),
  .spec_row("att_eval_which_party_better_handles_i", "V241237", "Which party better handles immigration",
            "evaluation", irt = TRUE),
  .spec_row("att_iss_importance_of_what_public_sc", "V242179", "Importance of what public schools teach",
            "issue"),
  .spec_row("att_iss_approval_of_diversity_equity", "V241290x", "Approval of diversity, equity and inclusion",
            "issue", irt = TRUE),
  .spec_row("att_iss_whether_sexual_harassment_wo", "V241341", "Whether sexual harassment would affect the vote",
            "issue"),
  .spec_row("att_iss_whether_attention_to_sexual_", "V242361x",
            "Whether attention to sexual harassment has gone too far", "issue", irt = TRUE)
))
rownames(anes_fields) <- NULL

# Variables read only to derive or filter a field, or to decide eligibility.
anes_helper_variables <- c(
  attendance = "V241439",   # ever attend religious services (V241439 = 2: never)
  mode_pre   = "V240002a",  # mode of the pre-election interview (3 = paper)
  mode_post  = "V240002b",  # mode of the post-election interview (3 = paper)
  completion = "V240002c",  # 2 = completed both interviews
  sample     = "V240003"    # sample type (shown in the build log only)
)

# Codes of V241488x written as the persona reads them. `anes_label` is the
# full value label (after any verified override in label_overrides.csv)
# without its code prefix; the build stops unless it equals the label in the
# file. The shown text drops the "R " prefix and spells out one abbreviation
# ("incl nonworkg rs").
anes_recodes <- list(
  V241488x = data.frame(
    code = c(1, 2, 4, 5, 6, 7, 8),
    anes_label = c(
      "R working now (if also retired, disabled, homemaker or student, working 20 or more hrs/wk)",
      "R temporarily laid off",
      "R unemployed",
      "R retired (if also working, working < 20 hrs/wk)",
      "R permanently disabled (if also working, working < 20 hrs/wk)",
      "R homemaker (if also working, working <20 hrs/wk/incl nonworkg rs both homemaker and student)",
      "R student (if also working, working <20 hrs/wk)"),
    text = c(
      "Working now (if also retired, disabled, homemaker or student, working 20 or more hrs/wk)",
      "Temporarily laid off",
      "Unemployed",
      "Retired (if also working, working < 20 hrs/wk)",
      "Permanently disabled (if also working, working < 20 hrs/wk)",
      "Homemaker (if also working, working <20 hrs/wk; includes nonworking respondents who are both homemaker and student)",
      "Student (if also working, working <20 hrs/wk)"),
    stringsAsFactors = FALSE)
)

# Age bands (years on Election Day; 80 means 80 or older).
anes_age_breaks <- c(-Inf, 24, 34, 44, 54, 64, Inf)
anes_age_labels <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")

# Thermometer bands used only inside the IRT model, which needs ordered
# categories; the shipped values are the ratings themselves.
anes_thermometer_irt_breaks <- c(-Inf, 15, 40, 60, 85, Inf)

# Labels that mark a variable the build refuses to read: spouse/partner
# survey items (SPS), paper-only items (PAPER), items asked only of the GSS
# re-interview sample, which this release does not include (GSS), and
# restricted-use variables (RESTRICTED). The check is made on the variable
# label after an optional "PRE:", "POST:" or "PRE-POST:" prefix.
anes_refused_label <- "^((PRE|POST|PRE-POST):\\s*)?(SPS|PAPER|GSS|RESTRICTED)\\b"

# Fields of the earlier build (the copy shipped in LLMR) that were dropped or
# replaced, with the reason; the README gives the codebook evidence.
anes_dropped <- data.frame(
  handle = c("demo_community_type", "att_val_general_happiness",
             "att_val_confidence_in_the_supreme_co", "att_val_confidence_in_the_scientific",
             "att_val_confidence_in_the_press", "att_val_importance_of_remaining_a_de"),
  variable = c("V242340", "V242614", "V242617", "V242620", "V242619", "V241731"),
  reason = c(
    "paper-only item (POST: PAPER:); replaced by V242341 under demo_rural_urban_identity",
    "GSS re-interview item; the GSS sample is not in this release",
    "GSS re-interview item; replaced by the CSES item V242419 (trust in the judiciary)",
    "GSS re-interview item; replaced by the CSES item V242420 (trust in scientists)",
    "GSS re-interview item; replaced by the CSES item V242422 (trust in traditional news media)",
    "spouse/partner survey item (SPS:); the respondent's own interview has no equivalent"),
  stringsAsFactors = FALSE)

# Variables whose source changed but whose handle and meaning did not.
anes_replaced <- data.frame(
  handle = c("att_aff_feeling_toward_the_maga_move", "att_val_satisfaction_with_democracy",
             "demo_home_ownership", "demo_religious_attendance"),
  old_variable = c("V241168", "V241711", "V241531", "V241440"),
  new_variable = c("V242148", "V242439", "V241530", "V241440"),
  reason = c(
    "V241168 is the paper-only pre-election thermometer; V242148 is the post-election thermometer",
    "V241711 is a spouse/partner survey item; V242439 is the respondent's CSES item",
    "V241531 asks only owners (mortgage or owned outright); V241530 asks everyone",
    "V241440 asks only people who attend; V241439 = 2 (never attends) is now 'Never'"),
  stringsAsFactors = FALSE)

# Items about sexual orientation and gender identity are left out by design;
# recorded here so the list can be audited. None of them is read.
anes_excluded_identity <- c(
  "V241370", "V241371", "V241372x", "V241373", "V241374", "V241375x",
  "V241376", "V241377", "V241378x", "V241379", "V241380", "V241381x",
  "V241382", "V241383", "V241384", "V241385x", "V241552", "V241553", "V241553z",
  "V241719", "V241742", "V242144", "V242151", "V242211", "V242362", "V242363",
  "V242364x", "V242457", "V242458x", "V242508", "V242510", "V242553", "V242558",
  "V242359", "V242360")

# Shipped cell texts of 76, 77, or 80 characters, the lengths a value label
# cut by Stata at 80 characters has with and without its code prefix. Each
# entry here is complete: the V241386 label is the verified full text from
# label_overrides.csv, which happens to be 80 characters long, and the scale
# sentences are built by the script. The build stops on any other cell of
# these lengths.
anes_length_allowlist <- c(
  "Make all unauthorized immigrants felons and send them back to their home country",
  sprintf("%d on a scale from 1 (Government insurance plan) to 7 (Private insurance plan)", 1:7))
