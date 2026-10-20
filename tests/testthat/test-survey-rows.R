# `rows` in create_agents_from_survey() selects the file's own rows, even when
# some respondents are left out for having no demographic values.

write_survey <- function() {
  lab <- function(values, labels, varlab) {
    x <- haven::labelled(values, labels)
    attr(x, "label") <- varlab
    x
  }
  d <- data.frame(
    AGE = lab(c(-9L, 2L, 3L, 4L),
              c("Refused" = -9, "25-34" = 2, "35-44" = 3, "45-54" = 4),
              "Age band"),
    PARTY = lab(c(1L, 2L, 1L, 2L), c("Democrat" = 1, "Republican" = 2),
                "Party identification")
  )
  f <- tempfile(fileext = ".dta")
  haven::write_dta(d, f)
  f
}

ages_of <- function(agents) {
  parts <- Filter(function(a) !isTRUE(a$is_moderator), agents)
  sort(vapply(parts, function(a) as.character(a$demographics$age), ""), method = "radix")
}

survey_agents <- function(f, ...) {
  create_agents_from_survey(survey_path = f, config = LLMR::llm_config("openai", "m"),
                            demographic_vars = c(age = "AGE"),
                            survey_vars = "PARTY",
                            question_wording = c(PARTY = "Party identification"), ...)
}

test_that("integer and logical rows refer to the file's rows", {
  f <- write_survey(); on.exit(unlink(f), add = TRUE)
  expect_identical(unname(ages_of(survey_agents(f, n_participants = 2, rows = c(2, 3)))),
                   c("25-34", "35-44"))
  expect_identical(unname(ages_of(survey_agents(f, n_participants = 2,
                                                rows = c(FALSE, TRUE, TRUE, FALSE)))),
                   c("25-34", "35-44"))
})

test_that("selected respondents without demographics are reported", {
  f <- write_survey(); on.exit(unlink(f), add = TRUE)
  expect_message(a <- survey_agents(f, n_participants = 1, rows = c(1, 2)),
                 "1 selected respondent\\(s\\) have no demographic values")
  expect_identical(unname(ages_of(a)), "25-34")
})

test_that("a respondent cannot be selected twice", {
  f <- write_survey(); on.exit(unlink(f), add = TRUE)
  expect_identical(unname(ages_of(survey_agents(f, n_participants = 2, rows = c(3, 3, 4)))),
                   c("35-44", "45-54"))
})

test_that("invalid row selectors are refused", {
  f <- write_survey(); on.exit(unlink(f), add = TRUE)
  expect_error(survey_agents(f, n_participants = 1, rows = 2.5), "whole numbers")
  expect_error(survey_agents(f, n_participants = 1, rows = 99), "whole numbers")
  expect_error(survey_agents(f, n_participants = 1, rows = c(TRUE, FALSE)),
               "one logical value per respondent")
})

test_that("a predicate over a variable with missing values selects where it is TRUE", {
  f <- write_survey(); on.exit(unlink(f), add = TRUE)
  expect_no_message(a <- survey_agents(f, n_participants = 2,
                                       rows = function(df) df$age != "45-54"))
  expect_identical(unname(ages_of(a)), c("25-34", "35-44"))
})

test_that("a file without demographic values is refused with a message that fits", {
  f <- write_survey(); on.exit(unlink(f), add = TRUE)
  expect_error(suppressMessages(survey_agents(f, n_participants = 1, rows = 1)),
               "`rows` selected no respondents with demographic values")
  d <- data.frame(AGE = haven::labelled(c(-9L, -9L), c("Refused" = -9)))
  g <- tempfile(fileext = ".dta"); on.exit(unlink(g), add = TRUE)
  haven::write_dta(d, g)
  msg <- testthat::capture_messages(expect_error(
    create_agents_from_survey(1, g, LLMR::llm_config("openai", "m"), demographic_vars = c(age = "AGE")),
    "No respondent in the file has a value for any of `demographic_vars`"))
  expect_match(msg, "^2 respondent\\(s\\) have no demographic values")
})

