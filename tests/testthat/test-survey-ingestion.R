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

test_that(".fg_render_column decodes labels and leaves out the missing vocabulary", {
  x <- haven::labelled(c(1L, 2L, -9L), c("Yes" = 1, "No" = 2, "Refused" = -9))
  v <- FocusGroup:::.fg_render_column(x, "x", .fg_default_na_strings)
  expect_equal(v$text, c("Yes", "No", NA))
  expect_equal(v$status, c("rendered", "rendered", "dropped"))
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
  expect_warning(ag <- suppressMessages(create_agents_from_survey(
    n_participants = 3, survey_path = f,
    demographic_vars = c(age = "AGE", education = "EDU"),
    survey_vars = c("PARTY"),
    config = cfg
  )), "No question wording was supplied for 1 survey item")
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
    survey_vars = "PARTY",
    question_wording = c(PARTY = "Which party do you identify with?"),
    rows = function(df) df$age == "55+",
    config = cfg
  )
  parts <- Filter(function(a) !isTRUE(a$is_moderator), ag)
  ages <- vapply(parts, function(a) {
    v <- a$demographics$age; if (is.null(v)) NA_character_ else as.character(v)
  }, "")
  expect_true(all(ages == "55+"))
})

test_that("create_agents_from_data works on the bundled persona dataset", {
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

test_that("create_agents() and the Studio draw from the package's own ANES dataset", {
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  expect_identical(FocusGroup:::.fg_personas(), anes_2024_personas)

  agents <- create_agents(nrow(anes_2024_personas), config = cfg)
  report <- persona_rendering_report(agents)
  expect_setequal(unique(report$variable), attr(anes_2024_personas, "demographic_fields"))
  # The 2026-05-19 release replaced the community-type field.
  expect_true("demo_rural_urban_identity" %in% report$variable)
  expect_false("demo_community_type" %in% report$variable)
  # Every value shown is the dataset's value for the row drawn.
  rows <- attr(agents, "source_rows")
  shown <- report[report$status == "rendered", ]
  expected <- mapply(function(id, column) anes_2024_personas[[column]][[rows[[id]]]],
                     shown$agent_id, shown$variable, USE.NAMES = FALSE)
  expect_identical(shown$text, expected)
})

test_that("personas from the bundled dataset state scales and thermometers in full", {
  cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
  agents <- create_agents_from_data(anes_2024_personas, nrow(anes_2024_personas), cfg)
  personas <- vapply(Filter(function(a) !isTRUE(a$is_moderator), agents),
                     function(a) a$persona_description, character(1))
  lines <- unlist(strsplit(personas, "\n"))
  scale <- "^  Answer: [1-7] on a scale from 1 \\([^()]+\\) to 7 \\([^()]+\\)$"
  expect_gt(sum(grepl(scale, lines)), 100L)
  expect_gt(sum(grepl("^  Answer: [0-9]+ on a 0-100 feeling thermometer$", lines)), 1000L)
  answers <- sub("^  Answer: ", "", grep("^  Answer: ", lines, value = TRUE))
  expect_false(any(grepl("^-?[0-9]+\\. |^NA$|[{}]|^\\s*$", answers)))
  expect_false(any(.fg_is_missing_label(answers, fg_default_na_strings())))
  expect_false(any(grepl("Background:[^\n]*(on a scale|thermometer)", personas)))
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
