# Tests of the bundled dataset `anes_2024_personas`. The same file ships in
# FocusGroup and LLMRpanel, whose copies of the dataset are identical; both
# are written by FocusGroup's data-raw/anes_2024_personas/make_personas.R,
# which also writes the fingerprint in fixtures/anes_2024_personas.md5.

anes_test_package <- function() {
  pkg <- testthat::testing_package()
  if (!nzchar(pkg)) {
    desc <- file.path(testthat::test_path(), "..", "..", "DESCRIPTION")
    if (file.exists(desc)) pkg <- unname(read.dcf(desc, fields = "Package")[1, 1])
  }
  if (!nzchar(pkg)) stop("Cannot tell which package is under test.", call. = FALSE)
  pkg
}

# The package's own copy, never another package's dataset of the same name.
anes_personas <- function() {
  ns <- asNamespace(anes_test_package())
  if (exists("anes_2024_personas", envir = ns, inherits = FALSE)) {
    return(get("anes_2024_personas", envir = ns, inherits = FALSE))
  }
  lazy <- ns[[".__NAMESPACE__."]][["lazydata"]]
  if (!is.null(lazy) && exists("anes_2024_personas", envir = lazy, inherits = FALSE)) {
    return(get("anes_2024_personas", envir = lazy, inherits = FALSE))
  }
  stop("anes_2024_personas is not in the package under test.", call. = FALSE)
}

anes_handles <- c(
  "demo_age", "demo_sex", "demo_education", "demo_race_ethnicity", "demo_marital_status",
  "demo_household_income", "demo_religion", "demo_religion_importance",
  "demo_religious_attendance", "demo_census_region", "demo_rural_urban_identity",
  "demo_employment_status", "demo_home_ownership", "demo_children_in_household",
  "demo_union_household", "demo_military_service", "demo_attention_to_politics",
  "att_id_party_identification", "att_id_importance_of_party_identity",
  "att_id_liberal_conservative_self_pl", "att_id_2024_presidential_vote",
  "att_id_presidential_vote_intent", "att_id_harris_vs_trump_preference",
  "att_id_likelihood_of_voting", "att_id_party_of_presidential_vote_i",
  "att_id_party_of_us_house_vote_inten", "att_aff_feeling_toward_kamala_harris",
  "att_aff_feeling_toward_donald_trump", "att_aff_feeling_toward_the_democrati",
  "att_aff_feeling_toward_the_republica", "att_aff_feeling_toward_the_maga_move",
  "att_aff_feeling_toward_the_supreme_c", "att_aff_feeling_toward_congress",
  "att_aff_feeling_toward_liberals", "att_aff_feeling_toward_conservatives",
  "att_aff_feeling_toward_labor_unions", "att_aff_feeling_toward_big_business",
  "att_aff_feeling_toward_the_police", "att_aff_feeling_toward_black_lives_m",
  "att_aff_feeling_toward_the_nra", "att_aff_feeling_toward_muslims",
  "att_aff_feeling_toward_christians", "att_aff_feeling_toward_jews",
  "att_aff_feeling_toward_planned_paren", "att_eval_approval_of_the_president",
  "att_eval_approval_of_congress", "att_eval_approval_of_the_supreme_cour",
  "att_eval_approval_of_the_president_on", "att_eval_approval_of_the_president_on_2",
  "att_eval_approval_of_the_president_on_3", "att_eval_national_economy_over_the_pa",
  "att_eval_national_economy_over_the_ne", "att_eval_personal_finances_vs_a_year_",
  "att_eval_worry_about_personal_finance", "att_iss_government_spending_and_serv",
  "att_iss_federal_spending_on_social_s", "att_iss_federal_spending_on_aid_to_t",
  "att_iss_federal_spending_on_welfare", "att_iss_tax_on_millionaires",
  "att_iss_government_reducing_income_i", "att_iss_policy_toward_unauthorized_i",
  "att_iss_border_wall", "att_iss_ending_birthright_citizenshi",
  "att_iss_path_to_citizenship", "att_iss_immigrants_and_the_economy",
  "att_iss_gun_background_checks", "att_iss_difficulty_of_buying_a_gun",
  "att_iss_regulating_greenhouse_emissi", "att_iss_climate_change_and_severe_we",
  "att_iss_environment_vs_business_trad", "att_iss_government_help_paying_for_h",
  "att_iss_government_vs_private_medica", "att_iss_abortion_position",
  "att_iss_importance_of_the_abortion_i", "att_iss_death_penalty",
  "att_iss_police_use_of_force", "att_iss_whether_police_treat_blacks_",
  "att_iss_preferential_hiring_of_black", "att_iss_affirmative_action_in_univer",
  "att_iss_government_assistance_to_bla", "att_iss_blacks_should_work_their_way",
  "att_iss_slavery_makes_it_harder_for_", "att_iss_blacks_have_gotten_less_than",
  "att_iss_if_blacks_tried_harder_they_", "att_iss_discrimination_against_black",
  "att_iss_discrimination_against_white", "att_iss_discrimination_against_women",
  "att_iss_us_weapons_to_ukraine", "att_iss_us_military_aid_to_israel",
  "att_iss_side_with_israelis_or_palest", "att_iss_free_trade_agreements",
  "att_iss_china_as_a_threat", "att_iss_russia_as_a_threat", "att_iss_defense_spending",
  "att_val_trust_in_government_in_washi", "att_val_government_run_for_a_few_big",
  "att_val_whether_the_political_system", "att_val_compromise_vs_sticking_to_pr",
  "att_val_a_strong_leader_is_good_for_", "att_val_leaving_decisions_to_indepen",
  "att_val_child_trait_independence_vs_", "att_val_child_trait_obedience_vs_sel",
  "att_val_whether_people_can_be_truste", "att_val_satisfaction_with_life",
  "att_val_trust_in_the_judiciary", "att_val_trust_in_scientists",
  "att_val_trust_in_traditional_media", "att_val_adjusting_morals_to_a_changi",
  "att_val_traditional_family_values", "att_val_whether_men_should_work_and_",
  "att_val_satisfaction_with_democracy", "att_iss_economic_mobility_vs_20_year",
  "att_iss_income_gap_vs_20_years_ago", "att_iss_whether_rural_areas_get_thei",
  "att_eval_hopefulness_about_the_countr", "att_eval_anger_about_the_country",
  "att_eval_which_party_better_handles_t", "att_eval_which_party_better_handles_i",
  "att_iss_importance_of_what_public_sc", "att_iss_approval_of_diversity_equity",
  "att_iss_whether_sexual_harassment_wo", "att_iss_whether_attention_to_sexual_",
  "ideology_score"
)

anes_scale_handles <- c(
  "att_iss_government_spending_and_serv", "att_iss_environment_vs_business_trad",
  "att_iss_government_vs_private_medica", "att_iss_abortion_position",
  "att_iss_government_assistance_to_bla", "att_iss_defense_spending")

anes_citation <- paste(
  "American National Election Studies. 2026. ANES 2024 Time Series Study",
  "Full Release [dataset and documentation]. May 19, 2026 version.",
  "www.electionstudies.org")

# A copy of FocusGroup's fg_default_na_strings(): the labels ANES and other
# releases give missing-data codes.
anes_na_vocabulary <- c(
  "inapplicable", "missing", "refused", "don't know", "don.t know", "not asked",
  "legitimate skip", "no answer", "not applicable", "not available", "\\bdk/rf\\b",
  "^\\s*(-?[0-9]+\\.)?\\s*restricted\\b", "^\\s*(-?[0-9]+\\.)?\\s*error\\s*$",
  "technical error", "no post[- ](election )?interview", "interview deleted",
  "^\\s*-[0-9]+\\..*\\bbreak ?off\\b", "^\\s*-[0-9]+\\..*\\binsuffici?ent\\b",
  "separate file", "released separately", "will be (released|available)",
  "not reported")

# Complete texts that happen to have the length of a cut label (see
# anes_length_allowlist in persona_spec.R).
anes_length_allowlist <- c(
  "Make all unauthorized immigrants felons and send them back to their home country",
  sprintf("%d on a scale from 1 (Government insurance plan) to 7 (Private insurance plan)", 1:7))

anes_scale_form <- "^[1-7] on a scale from 1 \\(.+\\) to 7 \\(.+\\)$"
anes_thermometer_form <- "^([0-9]|[1-9][0-9]|100) on a 0-100 feeling thermometer$"

anes_cells <- function(x) {
  cells <- unlist(lapply(x[setdiff(names(x), "ideology_score")], as.character), use.names = FALSE)
  cells[!is.na(cells)]
}

anes_is_ascii <- function(x) {
  x <- x[!is.na(x)]
  !any(grepl("[^\x01-\x7f]", x, useBytes = TRUE))
}

# A plain-text dump of everything the dataset ships except its build
# metadata; its md5 is the fingerprint. The build script writes the dump the
# same way.
anes_dump_lines <- function(x) {
  cell <- function(v) {
    v <- if (is.numeric(v)) sprintf("%.3f", v) else as.character(v)
    v[is.na(v)] <- "NA"
    v
  }
  frame_lines <- function(d) {
    cols <- lapply(d, cell)
    c(paste(names(d), collapse = "\t"), do.call(paste, c(cols, sep = "\t")))
  }
  dict <- attr(x, "dictionary")
  dict$wording_page <- as.character(dict$wording_page)
  c("[data]", frame_lines(as.data.frame(unclass(x), stringsAsFactors = FALSE, check.names = FALSE)),
    "[dictionary]", frame_lines(dict),
    "[demographic_fields]", attr(x, "demographic_fields"),
    "[source]", attr(x, "source"),
    "[citation]", attr(x, "citation"),
    "[n_demographics]", as.character(attr(x, "n_demographics")),
    "[n_attitudes]", as.character(attr(x, "n_attitudes")),
    "[build_audit]", frame_lines(attr(x, "build_audit")))
}

test_that("the dataset has its shape, handles, and attributes", {
  x <- anes_personas()
  expect_s3_class(x, "data.frame")
  expect_identical(dim(x), c(100L, 123L))
  expect_identical(names(x), anes_handles)
  expect_type(x$ideology_score, "double")
  expect_false(anyNA(x$ideology_score))
  expect_false(is.unsorted(x$ideology_score))
  for (h in setdiff(anes_handles, "ideology_score")) expect_type(x[[h]], "character")

  dict <- attr(x, "dictionary")
  expect_identical(names(dict), c("handle", "question", "anes_variable", "domain", "wording",
                                  "wording_variable", "wording_page", "wording_status"))
  expect_identical(dict$handle, setdiff(anes_handles, "ideology_score"))
  expect_false(anyNA(dict$question))
  expect_false(anyDuplicated(dict$question) > 0)
  expect_true(all(grepl("^V24[0-9]{4}[a-z]?$", dict$anes_variable)))
  expect_true(all(dict$domain %in% c("demographic", "identity", "affect", "evaluation", "issue", "values")))
  demo <- dict$handle[dict$domain == "demographic"]
  expect_identical(attr(x, "demographic_fields"), demo)
  expect_true(all(startsWith(demo, "demo_")))
  expect_identical(attr(x, "n_demographics"), 17L)
  expect_identical(attr(x, "n_attitudes"), 105L)

  build <- attr(x, "build")
  expect_type(build, "list")
  expect_true(all(c("R", "packages", "inputs", "seed", "label_overrides") %in% names(build)))
  expect_identical(build$seed, 110L)
  expect_true(all(c("haven", "mirt") %in% names(build$packages)))
  expect_identical(build$inputs$file, c("anes_timeseries_2024_stata_20260519.dta",
                                        "anes_timeseries_2024_userguidecodebook_20260519.pdf"))
  expect_true(all(grepl("^[0-9a-f]{32}$", build$inputs$md5)))
})

test_that("the fields dropped for using other samples are gone", {
  x <- anes_personas()
  gone <- c("demo_community_type", "att_val_general_happiness",
            "att_val_confidence_in_the_supreme_co", "att_val_confidence_in_the_scientific",
            "att_val_confidence_in_the_press", "att_val_importance_of_remaining_a_de")
  expect_false(any(gone %in% names(x)))
  dict <- attr(x, "dictionary")
  refused <- c("V242340", "V241168", "V242614", "V242617", "V242620", "V242619",
               "V241711", "V241731", "V241531")
  expect_false(any(refused %in% dict$anes_variable))
  expect_identical(dict$anes_variable[dict$handle == "att_aff_feeling_toward_the_maga_move"], "V242148")
  expect_identical(dict$anes_variable[dict$handle == "att_val_satisfaction_with_democracy"], "V242439")
  expect_identical(dict$anes_variable[dict$handle == "demo_home_ownership"], "V241530")
  expect_identical(dict$anes_variable[dict$handle == "demo_rural_urban_identity"], "V242341")
  expect_identical(dict$anes_variable[match(c("att_val_trust_in_the_judiciary", "att_val_trust_in_scientists",
                                               "att_val_trust_in_traditional_media"), dict$handle)],
                   c("V242419", "V242420", "V242422"))
  # no field is mostly empty
  na_share <- vapply(x[dict$handle], function(v) mean(is.na(v)), numeric(1))
  expect_true(all(na_share < 0.5))
  expect_true("Never" %in% x$demo_religious_attendance)
  expect_true(all(c("Owned or being bought by a household member", "Rented for cash") %in%
                    x$demo_home_ownership))
})

test_that("cells carry no missing-data labels, codes, braces, or interviewer notation", {
  cells <- anes_cells(anes_personas())
  sentence <- grepl(anes_scale_form, cells) | grepl(anes_thermometer_form, cells)
  missing_label <- grepl(paste(anes_na_vocabulary, collapse = "|"), cells, ignore.case = TRUE)
  expect_identical(cells[missing_label], character())
  expect_identical(cells[grepl("^\\s*-?[0-9]+\\.?\\s", cells) & !sentence], character())
  expect_identical(cells[grepl("[{}]", cells)], character())
  expect_identical(cells[grepl("^R [a-z]", cells)], character())
  expect_identical(cells[grepl("^(998|999)$", cells)], character())
  # the vocabulary is FocusGroup's, when FocusGroup is the package under test
  fg_na <- get0("fg_default_na_strings", envir = asNamespace(anes_test_package()), mode = "function")
  if (!is.null(fg_na)) expect_identical(fg_na(), anes_na_vocabulary)
  # the checks above reject what they exist to reject
  probes <- c("-9. Refused", "-3. RESTRICTED", "Another candidate {SPECIFY}",
              "R homemaker", "1. Male", "999")
  expect_true(all(grepl(paste(anes_na_vocabulary, collapse = "|"), probes[1:2], ignore.case = TRUE)))
  expect_true(grepl("[{}]", probes[3]))
  expect_true(grepl("^R [a-z]", probes[4]))
  expect_true(grepl("^\\s*-?[0-9]+\\.?\\s", probes[5]))
  expect_true(grepl("^(998|999)$", probes[6]))
})

test_that("no cell has the length of a label cut at 80 characters, outside the allowlist", {
  cells <- unique(anes_cells(anes_personas()))
  flagged <- cells[nchar(cells) %in% c(76L, 77L, 80L)]
  expect_identical(setdiff(flagged, anes_length_allowlist), character())
  x <- anes_personas()
  full <- c(
    "Make all unauthorized immigrants felons and send them back to their home country",
    "Have guest worker program that allows unauthorized immigrants to remain in the US to work but only for a limited amount of time",
    "Allow unauthorized immigrants to remain in US & qualify for citizenship if they meet certain requirements",
    "Allow unauthorized immigrants to remain in US & qualify for citizenship without penalties")
  policy <- x$att_iss_policy_toward_unauthorized_i
  expect_true(all(policy[!is.na(policy)] %in% full))
  employment <- x$demo_employment_status
  expect_false(any(grepl("\\b(homem|20 or)$", employment)))
  overrides <- attr(x, "build")$label_overrides
  expect_identical(paste(overrides$variable, overrides$code),
                   c(paste("V241386", 1:4), "V241488x 1", "V241488x 7"))
})

test_that("endpoint-labeled scales are written as sentences and keep their middle values", {
  x <- anes_personas()
  for (h in anes_scale_handles) {
    v <- x[[h]][!is.na(x[[h]])]
    expect_true(all(grepl(anes_scale_form, v) | v == "Haven't thought much about this"), info = h)
    on_scale <- v[grepl(anes_scale_form, v)]
    points <- as.integer(substr(on_scale, 1L, 1L))
    expect_true(any(points %in% 2:6), info = h)
    ends <- unique(sub("^[1-7] ", "", on_scale))
    expect_length(ends, 1L)
  }
  other <- setdiff(names(x), c(anes_scale_handles, "ideology_score"))
  expect_false(any(vapply(x[other], function(v) any(grepl(anes_scale_form, v)), logical(1))))
})

test_that("feeling thermometers are written as ratings from 0 to 100", {
  x <- anes_personas()
  thermometers <- grep("^att_aff_", names(x), value = TRUE)
  expect_length(thermometers, 18L)
  for (h in thermometers) {
    v <- x[[h]][!is.na(x[[h]])]
    expect_true(all(grepl(anes_thermometer_form, v)), info = h)
    expect_gt(length(unique(v)), 5L)
  }
  expect_false(grepl(anes_thermometer_form, "200 on a 0-100 feeling thermometer"))
  expect_false(grepl(anes_thermometer_form, "998 on a 0-100 feeling thermometer"))
})

test_that("NA cells are exactly the raw missing-data codes", {
  x <- anes_personas()
  audit <- attr(x, "build_audit")
  expect_identical(names(audit), c("handle", "anes_variable", "n_na", "n_raw_missing"))
  expect_identical(audit$handle, names(x))
  expect_identical(audit$n_na, audit$n_raw_missing)
  expect_identical(audit$n_na, unname(vapply(x, function(v) sum(is.na(v)), integer(1))))
  dict <- attr(x, "dictionary")
  expect_identical(audit$anes_variable, c(dict$anes_variable, NA_character_))
})

test_that("dictionary wordings follow their status", {
  dict <- attr(anes_personas(), "dictionary")
  statuses <- c("verbatim", "from_source_item", "stem_only", "none")
  expect_true(all(dict$wording_status %in% statuses))
  none <- dict$wording_status == "none"
  expect_true(all(is.na(dict$wording[none])))
  expect_true(all(is.na(dict$wording_variable[none])))
  expect_true(all(is.na(dict$wording_page[none])))
  expect_false(anyNA(dict$wording[!none]))
  expect_true(all(nzchar(dict$wording[!none])))
  expect_type(dict$wording_page, "integer")
  expect_true(all(dict$wording_page[!none] >= 21L))
  expect_true(all(grepl("^V24[0-9]{4}[a-z]?$", dict$wording_variable[!none])))
  own <- dict$wording_status %in% c("verbatim", "stem_only")
  expect_identical(dict$wording_variable[own], dict$anes_variable[own])
  source_item <- dict$wording_status == "from_source_item"
  expect_true(all(dict$wording_variable[source_item] != dict$anes_variable[source_item]))
  expect_true(all(grepl("^Where would you place yourself on this scale",
                        dict$wording[dict$wording_status == "stem_only"])))
  expect_identical(dict$wording[dict$handle == "att_iss_policy_toward_unauthorized_i"], paste(
    "Which comes closest to your view about what government policy should be toward",
    "unauthorized immigrants now living in the United States? You can just tell me the",
    "number of your choice."))
  expect_identical(dict$wording_page[dict$handle == "att_iss_policy_toward_unauthorized_i"], 164L)
  expect_false(any(grepl("\\s{2}|^\\s|\\s$|[{}]", dict$wording[!none])))
})

test_that("the dataset matches its fingerprint", {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  writeLines(anes_dump_lines(anes_personas()), con, sep = "\n")
  close(con)
  fixture <- testthat::test_path("fixtures", "anes_2024_personas.md5")
  expect_true(file.exists(fixture))
  expect_identical(unname(tools::md5sum(path)), readLines(fixture, warn = FALSE)[[1]])
})

test_that("source and citation name the ANES release", {
  x <- anes_personas()
  expect_identical(attr(x, "source"), anes_citation)
  expect_identical(attr(x, "citation"), anes_citation)
})

test_that("all shipped text is ASCII", {
  x <- anes_personas()
  dict <- attr(x, "dictionary")
  text <- c(names(x), anes_cells(x), unlist(lapply(dict, as.character)),
            attr(x, "demographic_fields"), attr(x, "source"), attr(x, "citation"),
            unlist(lapply(attr(x, "build_audit"), as.character)))
  expect_true(anes_is_ascii(text))
  expect_false(anes_is_ascii("Don\u2019t know"))
})

# Developer check: the wordings and the full labels re-verified against the
# raw ANES files. It runs when ANES_DTA and ANES_CODEBOOK are set and fails,
# rather than skipping, if they are set but the check cannot run.
anes_codebook_lines <- function(pdf) {
  pdftotext <- Sys.getenv("PDFTOTEXT", "")
  if (!nzchar(pdftotext)) pdftotext <- Sys.which("pdftotext")
  if (!nzchar(pdftotext) || !file.exists(pdftotext)) {
    stop("ANES_CODEBOOK is set but pdftotext was not found; set PDFTOTEXT.", call. = FALSE)
  }
  out <- tempfile(fileext = ".txt")
  on.exit(unlink(out), add = TRUE)
  status <- system2(pdftotext, c("-raw", "-enc", "UTF-8", shQuote(pdf), shQuote(out)))
  if (!identical(as.integer(status), 0L)) stop("pdftotext failed.", call. = FALSE)
  pages <- strsplit(paste(readLines(out, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
                    "\f", fixed = TRUE)[[1]]
  rows <- lapply(pages, function(p) {
    lines <- strsplit(p, "\n", fixed = TRUE)[[1]]
    keep <- nzchar(trimws(lines))
    lines <- lines[keep]
    if (!length(lines)) return(NULL)
    num <- regmatches(lines[[1]], regexpr("(^[0-9]+\\b)|(\\b[0-9]+\\s*$)", lines[[1]]))
    data.frame(text = lines[-1], page = if (length(num)) as.integer(num) else NA_integer_,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

anes_ascii <- function(x) {
  map <- c("\ufb00" = "ff", "\ufb01" = "fi", "\ufb02" = "fl", "\ufb03" = "ffi",
           "\ufb04" = "ffl", "\u2018" = "'", "\u2019" = "'", "\u201a" = "'",
           "\u2032" = "'", "\u201c" = "\"", "\u201d" = "\"", "\u201e" = "\"",
           "\u2033" = "\"", "\u2010" = "-", "\u2011" = "-", "\u2012" = "-",
           "\u2013" = "-", "\u2212" = "-", "\u2014" = "--", "\u2015" = "--",
           "\u2026" = "...", "\u00a0" = " ")
  for (from in names(map)) x <- gsub(from, map[[from]], x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

# The block of one variable: from the line that starts with its name to the
# next line that starts with another variable's name; joined as the build
# joins it, with the page of each line's start.
anes_block <- function(lines, start, variable, names_in_file) {
  first <- sub("^\\s*(\\S+).*$", "\\1", lines$text)
  rest <- sub("^\\s*\\S+\\s*", "", lines$text)
  head <- first %in% names_in_file & (!nzchar(rest) | grepl("^[A-Z]{3}", rest))
  from <- which(seq_along(first) >= start & first == variable & head)[1]
  if (is.na(from)) stop("Variable ", variable, " not found in the codebook.", call. = FALSE)
  nxt <- which(seq_along(first) > from & head)
  to <- if (length(nxt)) nxt[[1]] - 1L else nrow(lines)
  joined <- ""
  starts <- integer()
  pages <- integer()
  for (i in from:to) {
    piece <- anes_ascii(lines$text[[i]])
    if (!nzchar(piece)) next
    tight <- nzchar(joined) && grepl("[A-Za-z]-$", joined) && grepl("^[A-Za-z]", piece)
    starts <- c(starts, if (!nzchar(joined)) 1L else nchar(joined) + if (tight) 1L else 2L)
    pages <- c(pages, lines$page[[i]])
    joined <- if (!nzchar(joined)) piece else paste0(joined, if (tight) "" else " ", piece)
  }
  list(text = joined, starts = starts, pages = pages)
}

anes_page_of <- function(block, text) {
  pos <- regexpr(text, block$text, fixed = TRUE)[[1]]
  if (pos < 0) return(NA_integer_)
  block$pages[[max(which(block$starts <= pos))]]
}

test_that("wordings and full labels re-verify against the raw ANES files (developer check)", {
  dta <- Sys.getenv("ANES_DTA", "")
  pdf <- Sys.getenv("ANES_CODEBOOK", "")
  skip_if(!nzchar(dta) || !nzchar(pdf), "ANES_DTA and ANES_CODEBOOK are not set")
  if (!file.exists(dta) || !file.exists(pdf)) stop("ANES_DTA or ANES_CODEBOOK does not exist.", call. = FALSE)
  # haven is named through a variable: LLMRpanel does not declare it, and
  # only this developer check uses it.
  reader <- "haven"
  if (!requireNamespace(reader, quietly = TRUE)) stop("The developer check needs haven.", call. = FALSE)
  read_dta <- getExportedValue(reader, "read_dta")
  x <- anes_personas()
  build <- attr(x, "build")
  expect_identical(unname(tools::md5sum(c(dta, pdf))), build$inputs$md5)

  meta <- read_dta(dta, n_max = 0)
  lines <- anes_codebook_lines(pdf)
  norm <- anes_ascii(lines$text)
  start <- which(norm == "CODEBOOK: VARIABLES" &
                   c(norm[-1], "") == "GENERAL NON-SURVEY VARIABLES" &
                   startsWith(c(norm[-(1:2)], "", ""), "version"))[1]
  expect_false(is.na(start))

  dict <- attr(x, "dictionary")
  worded <- dict[dict$wording_status != "none", ]
  for (i in seq_len(nrow(worded))) {
    block <- anes_block(lines, start, worded$wording_variable[[i]], names(meta))
    expect_identical(anes_page_of(block, worded$wording[[i]]), worded$wording_page[[i]],
                     info = worded$handle[[i]])
  }
  # a wording from another item does not verify in this item's block
  wrong <- anes_block(lines, start, "V241387", names(meta))
  expect_true(is.na(anes_page_of(wrong, worded$wording[worded$anes_variable == "V241386"])))

  overrides <- build$label_overrides
  for (i in seq_len(nrow(overrides))) {
    o <- overrides[i, ]
    lab <- attr(meta[[o$variable]], "labels")
    cut <- names(lab)[lab == o$code]
    expect_identical(nchar(cut), 80L)
    body <- trimws(sub("^\\s*-?[0-9]+\\.?\\s+", "", cut))
    expect_true(startsWith(o$full_label, body), info = paste(o$variable, o$code))
    block <- anes_block(lines, start, o$variable, names(meta))
    expect_identical(anes_page_of(block, paste0(o$code, ". ", o$full_label)), o$page,
                     info = paste(o$variable, o$code))
  }
})
