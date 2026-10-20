# A sampling-weight column steers who is drawn; it is not part of any persona.

personas_of <- function(agents) {
  parts <- Filter(function(a) !isTRUE(a$is_moderator), agents)
  vapply(parts, function(a) a$persona_description, "")
}

test_that("the survey weight variable is not rendered into personas", {
  lab <- function(values, labels, varlab) {
    x <- haven::labelled(values, labels)
    attr(x, "label") <- varlab
    x
  }
  wt <- c(1.7, 0.4, 2.9, 1.1)
  attr(wt, "label") <- "Full sample weight"
  d <- data.frame(
    AGE = lab(c(1L, 2L, 3L, 1L), c("18-34" = 1, "35-54" = 2, "55+" = 3), "Age band"),
    PARTY = lab(c(1L, 2L, 1L, 2L), c("Democrat" = 1, "Republican" = 2),
                "Party identification"))
  d$WT <- wt
  f <- tempfile(fileext = ".dta"); on.exit(unlink(f), add = TRUE)
  haven::write_dta(d, f)
  set.seed(110)
  expect_warning(txt <- personas_of(create_agents_from_survey(
    n_participants = 3, survey_path = f, config = LLMR::llm_config("openai", "m"),
    demographic_vars = c(age = "AGE"), weights = "WT")),
    "PARTY \\(\"Party identification\"\\)")
  expect_false(any(grepl("Full sample weight|WT", txt)))
  expect_true(all(grepl("Party identification", txt)))
})

test_that("the data-frame weight column is not rendered into personas", {
  d <- data.frame(age = c("18-34", "35-54", "55+", "18-34"),
                  party = c("Democrat", "Republican", "Democrat", "Republican"),
                  wt_full = c(1.7, 0.4, 2.9, 1.1))
  set.seed(110)
  txt <- personas_of(create_agents_from_data(
    d, n_participants = 3, config = LLMR::llm_config("openai", "m"),
    demographic_cols = "age", weights = "wt_full"))
  expect_false(any(grepl("wt_full", txt)))
  expect_true(all(grepl("party", txt)))
})
