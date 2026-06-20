# Offline tests for survey -> persona ingestion. No network, no LLM calls, no
# real survey file: a small in-memory haven_labelled frame stands in for an
# arbitrary labeled survey (ANES/GSS/WVS shape).

make_labeled <- function(n = 12) {
  lab <- function(values, labels, varlab) {
    x <- haven::labelled(values, labels)
    attr(x, "label") <- varlab
    x
  }
  set.seed(110)
  data.frame(
    AGE = lab(sample(c(1L, 2L, 3L), n, TRUE),
              c("18-34" = 1, "35-54" = 2, "55+" = 3), "Age band of respondent"),
    EDU = lab(sample(c(1L, 2L, -9L), n, TRUE),
              c("High school" = 1, "College" = 2, "Refused" = -9), "Education level"),
    PARTY = lab(sample(c(1L, 2L, 3L), n, TRUE),
                c("Democrat" = 1, "Independent" = 2, "Republican" = 3),
                "Party identification of the respondent"),
    WT = runif(n, 0.5, 2),
    stringsAsFactors = FALSE
  )
}

test_that(".fg_decode_col decodes labels and NAs the missing vocabulary", {
  x <- haven::labelled(c(1L, 2L, -9L), c("Yes" = 1, "No" = 2, "Refused" = -9))
  v <- FocusGroup:::.fg_decode_col(x, .fg_default_na_strings)
  expect_equal(v, c("Yes", "No", NA))
})

test_that(".fg_col_key prefers an explicit name, then the variable label", {
  d <- make_labeled()
  expect_equal(FocusGroup:::.fg_col_key(d, "PARTY"),
               "Party identification of the respondent")          # from label
  expect_equal(FocusGroup:::.fg_col_key(d, "PARTY", "Party ID"),
               "Party ID")                                        # explicit wins
})

test_that("create_agents_from_survey renders the question wording, not the code", {
  d <- make_labeled()
  f <- tempfile(fileext = ".dta"); on.exit(unlink(f), add = TRUE)
  haven::write_dta(d, f)
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  ag <- create_agents_from_survey(
    n_participants = 3, survey_path = f,
    demographic_vars = c(age = "AGE", education = "EDU"),
    survey_vars = c("PARTY"),
    llm_config = cfg
  )
  expect_length(ag, 4L)                       # 3 participants + moderator
  p <- Filter(function(a) !isTRUE(a$is_moderator), ag)[[1]]
  txt <- p$persona_description
  # the survey answer is keyed by the variable label, and an answer label shows
  expect_true(grepl("Party identification of the respondent", txt))
  expect_true(grepl("Democrat|Independent|Republican", txt))
  # a decoded demographic shows; no raw code names leak
  expect_false(grepl("PARTY|AGE|EDU", txt))
})

test_that("the `rows` predicate restricts the eligible pool", {
  d <- make_labeled(30)
  f <- tempfile(fileext = ".dta"); on.exit(unlink(f), add = TRUE)
  haven::write_dta(d, f)
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  ag <- create_agents_from_survey(
    n_participants = 4, survey_path = f,
    demographic_vars = c(age = "AGE"),
    survey_vars = c("PARTY"),
    rows = function(df) df$age == "55+",
    llm_config = cfg
  )
  parts <- Filter(function(a) !isTRUE(a$is_moderator), ag)
  ages <- vapply(parts, function(a) {
    v <- a$demographics$age; if (is.null(v)) NA_character_ else as.character(v)
  }, "")
  expect_true(all(ages == "55+"))
})

test_that("create_agents_from_data works on the shipped ANES personas", {
  skip_if_not(exists("anes_2024_personas"))
  data(anes_2024_personas, package = "FocusGroup")
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  ag <- create_agents_from_data(anes_2024_personas, n_participants = 5, llm_config = cfg)
  expect_length(ag, 6L)
  p <- Filter(function(a) !isTRUE(a$is_moderator), ag)[[1]]
  # the persona carries both demographic background and survey answers
  expect_true(nchar(p$persona_description) > 200)
  expect_true(grepl("Background:", p$persona_description))
  expect_true(grepl("Question:", p$persona_description))
})

test_that("the shipped dataset is well-formed and carries no respondent ids", {
  skip_if_not(exists("anes_2024_personas"))
  data(anes_2024_personas, package = "FocusGroup")
  expect_s3_class(anes_2024_personas, "data.frame")
  expect_equal(nrow(anes_2024_personas), 100L)
  expect_false(any(grepl("V240001|case ?id|respondent ?id",
                         names(anes_2024_personas), ignore.case = TRUE)))
  expect_true(length(attr(anes_2024_personas, "demographic_fields")) >= 10)
  # rows are ordered by the ideology score (low = liberal, high = conservative)
  expect_true("ideology_score" %in% names(anes_2024_personas))
  expect_false(is.unsorted(anes_2024_personas$ideology_score))
  # no leftover ANES admin codes survived the decode (the numeric score column,
  # whose negative values look like "-1.6", is not a label and is excluded)
  label_cols <- setdiff(names(anes_2024_personas), "ideology_score")
  vals <- unique(unlist(lapply(anes_2024_personas[label_cols], unique)))
  vals <- vals[!is.na(vals)]
  expect_false(any(grepl("^-[0-9]+\\.\\s|inapplicable|refused|dk/rf", vals,
                         ignore.case = TRUE)))
})
