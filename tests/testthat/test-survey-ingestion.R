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
    config = cfg
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
    config = cfg
  )
  parts <- Filter(function(a) !isTRUE(a$is_moderator), ag)
  ages <- vapply(parts, function(a) {
    v <- a$demographics$age; if (is.null(v)) NA_character_ else as.character(v)
  }, "")
  expect_true(all(ages == "55+"))
})

test_that("create_agents_from_data works on the shared LLMR persona dataset", {
  skip_if_not_installed("LLMR")
  data(anes_2024_personas, package = "LLMR")
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  ag <- create_agents_from_data(anes_2024_personas, n_participants = 5, config = cfg)
  expect_length(ag, 6L)
  p <- Filter(function(a) !isTRUE(a$is_moderator), ag)[[1]]
  # the persona carries both demographic background and survey answers, with the
  # question wording (from the frame's dictionary), not the tidy handles
  expect_true(nchar(p$persona_description) > 200)
  expect_true(grepl("Background:", p$persona_description))
  expect_true(grepl("Question:", p$persona_description))
})

test_that("silicon panel personas pass through as direct descriptions", {
  panel <- data.frame(
    persona_id = c("persona-1", "persona-2"),
    persona = c(
      "A renter who depends on the evening bus and tracks monthly fares.",
      "A retired driver who favors frequent service over route expansion."
    ),
    age = c("34", "68"),
    stringsAsFactors = FALSE
  )
  class(panel) <- c("silicon_panel", class(panel))
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")

  agents <- create_agents_from_data(
    panel,
    n_participants = 2,
    demographic_cols = "age",
    rows = c(2, 1),
    config = cfg
  )

  expect_identical(
    unname(vapply(agents[1:2], function(agent) agent$persona_description,
                  character(1))),
    rev(panel$persona)
  )
  for (agent in agents[1:2]) {
    expect_false(any(c("persona", "persona_id") %in% names(agent$survey_responses)))
  }

  unclassed <- panel
  class(unclassed) <- "data.frame"
  duck_typed <- create_agents_from_data(
    unclassed,
    n_participants = 2,
    demographic_cols = "age",
    config = cfg
  )
  expect_identical(
    unname(vapply(duck_typed[1:2], function(agent) agent$persona_description,
                  character(1))),
    panel$persona
  )

  minimal <- unclassed[c("persona_id", "persona")]
  minimal_agents <- create_agents_from_data(
    minimal,
    n_participants = 2,
    config = cfg
  )
  expect_identical(
    unname(vapply(minimal_agents[1:2], function(agent) agent$persona_description,
                  character(1))),
    panel$persona
  )
})

# (The dataset's own shape/provenance is tested in LLMR, its home package.)
