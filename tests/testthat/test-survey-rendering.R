# Rendering of survey records into personas, and the rendering report. A small
# labeled file written to tempdir() stands in for a survey release with the
# shapes the ANES 2024 Stata file has: seven-point scales labeled only at their
# endpoints, an off-scale code 99, labels cut at 80 characters, a numeric age
# with a top code, and a category code without a label.

.lab <- function(values, labels, varlab = NULL) {
  x <- haven::labelled(values, labels)
  if (!is.null(varlab)) attr(x, "label") <- varlab
  x
}

.cut80 <- function(x) substr(x, 1L, 80L)

.immig_full <- paste("1. Make all unauthorized immigrants felons and send them",
                     "back to their home country")
.employ_full <- paste("1. R working now (if also retired, disabled, homemaker or",
                      "student, working 20 or more hrs/wk)")

.render_file <- function() {
  scale_labels <- c("-9. Refused" = -9, "-8. Don't know" = -8, "-4. Error" = -4,
                    "1. Government should provide many fewer services" = 1,
                    "7. Government should provide many more services" = 7,
                    "99. Haven't thought much about this" = 99)
  d <- data.frame(id = c(101, 102, 103, 104))
  d$AGE <- .lab(c(25, 80, 47, -2),
                c("-2. Missing, exact birth date not given" = -2,
                  "80. Eighty years old or older" = 80),
                "PRE: SUMMARY: Respondent age on Election Day")
  d$RELIG <- .lab(c(9, 1, 2, -1),
                  c("-1. Inapplicable" = -1, "1. Mainline Protestant" = 1,
                    "2. Evangelical Protestant" = 2, "3. Black Protestant" = 3),
                  "PRE: SUMMARY: Major group religion summary")
  d$EMPLOY <- .lab(c(1, 1, 2, 2),
                   stats::setNames(c(1, 2), c(.cut80(.employ_full), "2. R temporarily laid off")),
                   "PRE: SUMMARY: R occupation status 1 digit")
  d$SERV <- .lab(c(5, 7, 99, -9), scale_labels,
                 "PRE: 7pt scale spending & services: self-placement")
  d$IMMIG <- .lab(c(1, 2, 1, 2),
                  stats::setNames(c(-9, 1, 2), c("-9. Refused", .cut80(.immig_full),
                                                 "2. Have guest worker program")),
                  "PRE: US government policy toward unauthorized immigrants")
  d$THERM <- .lab(c(85, 0, 999, 100),
                  c("-9. Refused" = -9, "998. Don't know where to rate" = 998,
                    "999. Don't recognize" = 999),
                  "POST: Feeling thermometer: Democratic Party")
  d$POSTQ <- .lab(c(1, -1, -6, 1), c("-6. No post interview" = -6, "1. Yes" = 1, "2. No" = 2),
                  "POST: Did R vote for governor")
  path <- tempfile(fileext = ".dta")
  haven::write_dta(d, path)
  path
}

.wording <- c(SERV = "Where would you place yourself on this scale of government services?",
              IMMIG = "Which comes closest to your view about unauthorized immigrants?",
              THERM = "How would you rate the Democratic Party on a thermometer from 0 to 100?",
              POSTQ = "Did you vote for governor?")

.render_agents <- function(path, rows, ..., survey_vars = names(.wording),
                           question_wording = .wording) {
  create_agents_from_survey(
    n_participants = length(rows), survey_path = path,
    config = LLMR::llm_config("openai", "example-model"),
    demographic_vars = c(age = "AGE", religion = "RELIG", employment = "EMPLOY"),
    survey_vars = survey_vars, rows = rows,
    question_wording = question_wording, ...)
}

.quiet <- function(expr) suppressWarnings(suppressMessages(expr))

test_that("an unlabeled answer between labeled endpoints is rendered on its scale", {
  path <- .render_file(); on.exit(unlink(path))
  agents <- .quiet(.render_agents(path, rows = 1))
  persona <- agents$P1$persona_description
  sentence <- paste("On a scale of 1 to 7, where 1 is Government should provide many",
                    "fewer services and 7 is Government should provide many more",
                    "services, you place yourself at 5.")
  expect_true(grepl(paste0("Answer: ", sentence), persona, fixed = TRUE))
  report <- persona_rendering_report(agents)
  serv <- report[report$variable == "SERV", ]
  expect_identical(serv$rendered_as, "scale")
  expect_identical(serv$status, "rendered")
  expect_identical(serv$text, sentence)

  # The endpoint and the off-scale code keep their labels; a refusal is left out.
  agents <- .quiet(.render_agents(path, rows = 2:4))
  report <- persona_rendering_report(agents)
  serv <- report[report$variable == "SERV", ]
  expect_identical(serv$text, c("Government should provide many more services",
                                "Haven't thought much about this", NA))
  expect_identical(serv$status, c("rendered", "rendered", "dropped"))
  expect_identical(serv$reason[3], "missing value: -9. Refused")
})

test_that("the scale rule refuses code sets that are not endpoint-labeled scales", {
  na <- fg_default_na_strings()
  render <- function(values, labels) {
    .fg_render_column(.lab(values, labels), "x", na)[, c("text", "rendered_as", "status")]
  }
  # A nominal item with a gap in its codes is not a scale.
  nominal <- render(4, c("1. Democrat" = 1, "2. Republican" = 2, "3. Other" = 3, "5. None" = 5))
  expect_identical(nominal$rendered_as, "code")
  expect_identical(nominal$status, "fallback")
  # A labeled midpoint means the labels are not the scale's only ones.
  expect_identical(render(5, c("1. Liberal" = 1, "4. Moderate" = 4,
                               "7. Conservative" = 7))$rendered_as, "code")
  # A count with a bottom code and a top code is not a scale.
  expect_identical(render(3, c("0. None" = 0, "96. 96 or more" = 96))$rendered_as, "code")
  expect_identical(render(45, c("17. Under 18" = 17, "89. 89 or older" = 89))$rendered_as,
                   "value")
  # A code just past the upper endpoint is not an off-scale code.
  expect_identical(render(3, c("1. Agree" = 1, "5. Disagree" = 5, "8. No opinion" = 8))$rendered_as,
                   "code")
  # A thermometer labeled with a negative error code and off-scale codes holds numbers.
  therm <- render(c(50, 0), c("-4. Error" = -4, "999. Don't recognize" = 999))
  expect_identical(therm$text, c("50", "0"))
  expect_identical(therm$rendered_as, c("value", "value"))
  # Scales labeled only at their endpoints, centered on zero or running 0 to 10.
  expect_identical(render(-1, c("-3. Strongly disagree" = -3, "3. Strongly agree" = 3))$text,
                   paste("On a scale of -3 to 3, where -3 is Strongly disagree and 3 is",
                         "Strongly agree, you place yourself at -1."))
  expect_identical(render(6, c("0. Left" = 0, "10. Right" = 10, "95. Don't know where" = 95,
                               "96. Do not know enough" = 96))$rendered_as, "scale")
  # Missing-value labels are never endpoints, nor off-scale codes that block a scale.
  expect_identical(render(3, c("1. Agree" = 1, "5. Disagree" = 5, "8. Don't know" = 8))$rendered_as,
                   "scale")
  expect_identical(render(3, c("1. Agree" = 1, "9. Refused" = 9))$rendered_as, "value")
})

test_that("unlabeled codes left in a persona or left out are named in warnings", {
  path <- .render_file(); on.exit(unlink(path))
  expect_warning(
    expect_warning(
      expect_warning(.render_agents(path, rows = c(1, 2)),
                     "Shown as the bare code.*RELIG code 9 \\(P1\\)"),
      "80 characters long.*EMPLOY code 1 \\(P1, P2\\); IMMIG code 1 \\(P1\\)"),
    "Negative codes without a value label were taken as missing.*POSTQ code -1 \\(P2\\)")
  agents <- .quiet(.render_agents(path, rows = c(1, 2)))
  report <- persona_rendering_report(agents)
  postq <- report[report$variable == "POSTQ", ]
  expect_identical(postq$status, c("rendered", "dropped"))
  expect_match(postq$reason[2], "code -1 is negative and has no value label")
  expect_false(grepl("Did you vote for governor", agents$P2$persona_description))
})

test_that("the missing-data labels of ANES negative codes are left out by default", {
  path <- .render_file(); on.exit(unlink(path))
  expect_message(agents <- suppressWarnings(.render_agents(path, rows = 3)),
                 "left out of the personas as missing values \\(POSTQ\\)")
  report <- persona_rendering_report(agents)
  expect_identical(report$reason[report$variable == "POSTQ"], "missing value: -6. No post interview")
  expect_false(grepl("No post interview", agents$P1$persona_description))

  na <- .fg_default_na_strings
  anes <- c("-9. Refused", "-8. Don't know", "-7. Insufficient partial, interview deleted",
            "-6. No post interview", "-5. Sufficient partial, breakoff",
            "-5. Break off, sufficient partial", "-4. Error", "-3. RESTRICTED",
            "-3. RESTRICTED [STAFF ONLY]", "-2. DK/RF in V241288 or V241289",
            "-2. Open-end responses available in separate file",
            "-2. Data will be released separately when available",
            "-2. Data will be available in the 2024 ANES Methodology file",
            paste("-2. Due to a technical error variables from the RADPROGS",
                  "submodule will be relea"),
            "-2. Insufficent information to code other/specify open-ended response",
            "-2. Not reported", "-1. Inapplicable", "-1. No answer", "-3. Not applicable",
            "-5. Missing; Not available")
  expect_true(all(.fg_is_missing_label(anes, na)))
  # Each is missing by the one rule, as a plain value and as the label of its
  # negative code.
  expect_true(all(.fg_missing_text(anes, na)))
  codes <- as.numeric(sub("^(-[0-9]+)\\..*$", "\\1", anes))
  expect_true(all(.fg_missing_text(anes, na, code = codes)))
  # Answers that merely mention such words are kept.
  kept <- c("2. There is an error", "1. Abortion should be permitted with restrictions",
            "3. Terrorism", "1. Restrict immigration", "4. Postpone the vote",
            "2. Break off relations", "3. Insufficient funding", "Breakoff")
  expect_false(any(.fg_is_missing_label(kept, na)))
})

test_that("labels of negative codes outside the missing vocabulary are shown but flagged", {
  d <- data.frame(
    AGE = c(30, 40, 50),
    ISOL = .lab(c(-1, 2, 3), c("-2. DK/RF in V1 or V2" = -2, "-1. Applicable" = -1,
                               "1. Agree strongly" = 1, "2. Agree somewhat" = 2,
                               "3. Disagree somewhat" = 3)),
    AGREE = .lab(c(-3, 0, 2), c("-3. Strongly disagree" = -3, "-2. Disagree" = -2,
                                "-1. Slightly disagree" = -1, "0. Neither" = 0,
                                "1. Slightly agree" = 1, "2. Agree" = 2,
                                "3. Strongly agree" = 3)))
  path <- tempfile(fileext = ".dta"); on.exit(unlink(path))
  haven::write_dta(d, path)
  w <- testthat::capture_warnings(agents <- create_agents_from_survey(
    3, path, LLMR::llm_config("openai", "example-model"), demographic_vars = c(age = "AGE"),
    survey_vars = c("ISOL", "AGREE"),
    question_wording = c(ISOL = "Better off staying home?", AGREE = "Do you agree?")))
  expect_match(w, "Labels of negative codes are shown as answers.*ISOL code -1 \\(P1\\); AGREE code -3 \\(P1\\)",
               all = FALSE)
  report <- persona_rendering_report(agents)
  p1 <- report[report$agent_id == "P1" & report$kind == "item", ]
  expect_identical(p1$text, c("-1. Applicable", "-3. Strongly disagree"))
  expect_identical(p1$status, c("fallback", "fallback"))
  expect_true(grepl("Answer: -3. Strongly disagree", agents$P1$persona_description, fixed = TRUE))
})

test_that("question wording is shown, and its absence is warned", {
  path <- .render_file(); on.exit(unlink(path))
  agents <- .quiet(.render_agents(path, rows = 1))
  persona <- agents$P1$persona_description
  expect_true(grepl("Question: Where would you place yourself on this scale of government services?",
                    persona, fixed = TRUE))
  expect_false(grepl("PRE: 7pt scale", persona, fixed = TRUE))

  # The wording takes precedence over a name given to the variable.
  agents <- .quiet(.render_agents(path, rows = 1,
                                  survey_vars = c(scale_services = "SERV", "IMMIG")))
  report <- persona_rendering_report(agents)
  expect_identical(report$question_source[report$kind == "item"], c("wording", "wording"))
  expect_false(grepl("scale_services", agents$P1$persona_description))

  # Without it, the variable label is shown, even over a name given to the
  # variable, with one warning naming the items.
  expect_warning(
    agents <- suppressMessages(.render_agents(
      path, rows = 4, survey_vars = c("SERV", Immigration = "IMMIG"),
      question_wording = NULL)),
    "No question wording was supplied for 2 survey item.*SERV \\(\"PRE: 7pt scale spending")
  report <- persona_rendering_report(agents)
  expect_identical(report$question_source[report$kind == "item"], c("label", "label"))
  expect_identical(report$question[report$kind == "item"][1],
                   "PRE: 7pt scale spending & services: self-placement")
  expect_false(grepl("Question: Immigration", agents$P1$persona_description, fixed = TRUE))
  expect_no_warning(suppressMessages(.render_agents(
    path, rows = 4, survey_vars = c("SERV", "IMMIG"), question_wording = .wording,
    value_labels = data.frame(variable = "IMMIG", code = 2, label = "Guest workers"))))

  for (bad in list(c("Where?"), c(SERV = NA), c(SERV = " "), list(SERV = "Where?"),
                   c(SERV = "A", SERV = "B"))) {
    expect_error(.render_agents(path, rows = 1, question_wording = bad),
                 "`question_wording` must be a character vector")
  }
})

test_that("supplied value labels replace cut labels and label stray codes", {
  path <- .render_file(); on.exit(unlink(path))
  full <- data.frame(variable = c("IMMIG", "EMPLOY", "RELIG"), code = c(1, 1, 9),
                     label = c(.immig_full, .employ_full, "9. Other Christian"))
  expect_no_warning(agents <- suppressMessages(
    .render_agents(path, rows = 1, value_labels = full)))
  persona <- agents$P1$persona_description
  expect_true(grepl("Answer: Make all unauthorized immigrants felons and send them back to their home country",
                    persona, fixed = TRUE))
  expect_true(grepl("employment: R working now (if also retired, disabled, homemaker or student, working 20 or more hrs/wk)",
                    persona, fixed = TRUE))
  expect_true(grepl("religion: Other Christian", persona, fixed = TRUE))
  report <- persona_rendering_report(agents)
  supplied <- report[match(c("EMPLOY", "IMMIG", "RELIG"), report$variable), ]
  expect_identical(supplied$rendered_as, rep("supplied_label", 3))
  expect_identical(supplied$status, rep("rendered", 3))
  expect_identical(supplied$text[1:2], .fg_tidy_value(c(.employ_full, .immig_full)))
  expect_true(all(nchar(supplied$text[1:2]) > nchar(.fg_tidy_value(.cut80(c(.employ_full, .immig_full))))))

  # The list form takes haven's labels form, label = code.
  agents <- suppressMessages(suppressWarnings(.render_agents(
    path, rows = 1, value_labels = list(IMMIG = stats::setNames(1, .immig_full)))))
  expect_identical(persona_rendering_report(agents)$rendered_as[
    persona_rendering_report(agents)$variable == "IMMIG"], "supplied_label")

  # A supplied label that reads as missing leaves the answer out.
  agents <- suppressMessages(suppressWarnings(.render_agents(
    path, rows = 2, value_labels = data.frame(variable = "POSTQ", code = -1,
                                              label = "-1. Inapplicable"))))
  postq <- persona_rendering_report(agents)
  postq <- postq[postq$variable == "POSTQ", ]
  expect_identical(postq$reason, "missing value: -1. Inapplicable")

  expect_error(.render_agents(path, rows = 1, value_labels = data.frame(variable = "IMMIG", code = 1)),
               "columns variable, code, and label; missing: label")
  expect_error(.render_agents(path, rows = 1, value_labels = data.frame(
    variable = c("IMMIG", "IMMIG"), code = c(1, 1), label = c("A", "B"))),
    "more than one label for IMMIG code 1")
  expect_error(.render_agents(path, rows = 1, value_labels = data.frame(
    variable = "IMMIG", code = "one", label = "A")), "code\\(s\\) 'one' for IMMIG, which stores numbers")
  expect_error(.render_agents(path, rows = 1, value_labels = data.frame(
    variable = "IMMIG", code = 1, label = "")), "missing or empty variable, code, or label in row\\(s\\) 1")
  expect_error(.render_agents(path, rows = 1, value_labels = list(IMMIG = 1)),
               "must be a named vector")
  expect_error(.render_agents(path, rows = 1, value_labels = "IMMIG"),
               "must be a data frame with columns variable, code")
})

test_that("numeric demographics render as values and unlabeled codes as codes", {
  path <- .render_file(); on.exit(unlink(path))
  expect_warning(agents <- suppressMessages(.render_agents(
    path, rows = 1:2,
    value_labels = data.frame(variable = c("IMMIG", "EMPLOY", "POSTQ"), code = c(1, 1, -1),
                              label = c(.immig_full, .employ_full, "Inapplicable")))),
    "Shown as the bare code because the file has no value label for it: RELIG code 9 \\(P1\\)\\.")
  expect_true(grepl("Background: age: 25; religion: 9; employment:",
                    agents$P1$persona_description, fixed = TRUE))
  expect_true(grepl("age: Eighty years old or older", agents$P2$persona_description, fixed = TRUE))
  report <- persona_rendering_report(agents)
  demo <- report[report$kind == "demographic" & report$agent_id == "P1", ]
  expect_identical(demo$variable, c("AGE", "RELIG", "EMPLOY"))
  expect_identical(demo$rendered_as, c("value", "code", "supplied_label"))
  expect_identical(demo$status, c("rendered", "fallback", "rendered"))
  expect_identical(demo$reason[2], "code 9 has no value label")

  # A missing age is left out and listed; the respondent keeps the other fields.
  expect_message(agents <- suppressWarnings(.render_agents(path, rows = 4)),
                 "3 answer\\(s\\) were left out of the personas as missing values \\(AGE; RELIG; SERV\\)")
  report <- persona_rendering_report(agents)
  expect_identical(report$status[report$variable %in% c("AGE", "RELIG")], c("dropped", "dropped"))
  expect_identical(report$reason[report$variable == "AGE"],
                   "missing value: -2. Missing, exact birth date not given")
})

test_that("the rendering report records every requested field for every persona", {
  path <- .render_file(); on.exit(unlink(path))
  agents <- .quiet(.render_agents(path, rows = c(1, 3), survey_vars = c(names(.wording), "NOPE")))
  report <- persona_rendering_report(agents)
  expect_identical(names(report), c("agent_id", "kind", "variable", "question",
                                    "question_source", "code", "text", "rendered_as",
                                    "status", "reason", "template"))
  expect_identical(nrow(report), 2L * 8L)
  expect_identical(unique(report$agent_id), c("P1", "P2"))
  expect_identical(report$variable[1:8],
                   c("AGE", "RELIG", "EMPLOY", "SERV", "IMMIG", "THERM", "POSTQ", "NOPE"))
  expect_true(all(report$status %in% c("rendered", "fallback", "dropped")))
  expect_identical(report$reason[report$variable == "NOPE"],
                   rep("the variable is not in the file", 2))
  expect_identical(report$text[report$agent_id == "P2" & report$variable == "THERM"],
                   "Don't recognize")
  expect_identical(report$text[report$agent_id == "P1" & report$variable == "THERM"], "85")
  # Each answer the report records as shown is in that persona, under its question.
  shown <- report[report$status != "dropped" & report$kind == "item", ]
  for (i in seq_len(nrow(shown))) {
    expect_true(grepl(paste0("Question: ", shown$question[i], "\n  Answer: ", shown$text[i]),
                      agents[[shown$agent_id[i]]]$persona_description, fixed = TRUE))
  }
  w <- testthat::capture_warnings(.render_agents(path, rows = 1, survey_vars = c("SERV", "NOPE")))
  expect_match(w, "not in the file and are left out of every persona: NOPE\\.", all = FALSE)

  # The report travels with the roster into a FocusGroup; a subset loses it.
  fg <- suppressMessages(FocusGroup$new(
    topic = "Services", purpose = "Test the report.", agents = agents,
    moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow("round_robin", agents, "MOD")))
  expect_identical(persona_rendering_report(fg), report)
  expect_error(persona_rendering_report(agents[1:2]), "No rendering report is attached")
  direct <- create_agents(1, LLMR::llm_config("openai", "example-model"),
                          direct_persona_descriptions = "A retired teacher.")
  expect_error(persona_rendering_report(direct), "No rendering report is attached")
})

test_that("rosters built from data frames carry a report too", {
  cfg <- LLMR::llm_config("openai", "example-model")
  data <- data.frame(age = c("34", NA), party = c("Democrat", "Republican"),
                     stringsAsFactors = FALSE)
  agents <- create_agents_from_data(data, 2, cfg, demographic_cols = "age", rows = c(2, 1))
  report <- persona_rendering_report(agents)
  expect_identical(report$agent_id, c("P1", "P1", "P2", "P2"))
  expect_identical(report$variable, c("age", "party", "age", "party"))
  expect_identical(report$status, c("dropped", "rendered", "rendered", "rendered"))
  expect_identical(report$text, c(NA, "Republican", "34", "Democrat"))

  agents <- create_agents(2, cfg, demographics = data.frame(age = c(40, NA)))
  expect_identical(persona_rendering_report(agents)$status, c("rendered", "dropped"))

  result <- run_focus_group(
    "Services", cfg, n_participants = 2, guide = c(Opening = 1, Closing = 1),
    demographics = data.frame(age = c(40, 52)), flow = "round_robin", verbose = FALSE,
    .runner = function(experiments, ...) {
      experiments$response_text <- "A reply."
      experiments
    })
  expect_identical(persona_rendering_report(result)$text, c("40", "52"))

  bundled <- create_agents(2, cfg)
  expect_true(all(persona_rendering_report(bundled)$kind == "demographic"))
})

test_that("variables shown under the same text are told apart", {
  d <- data.frame(A = .lab(c(1, 2), c("1. Yes" = 1, "2. No" = 2), "Same label"),
                  B = .lab(c(2, 1), c("1. Yes" = 1, "2. No" = 2), "Same label"),
                  AGE = c(30, 40))
  path <- tempfile(fileext = ".dta"); on.exit(unlink(path))
  haven::write_dta(d, path)
  w <- testthat::capture_warnings(agents <- create_agents_from_survey(
    2, path, LLMR::llm_config("openai", "example-model"),
    demographic_vars = c(age = "AGE"), survey_vars = c("A", "B")))
  expect_match(w, "These survey items share the text shown for them.*A \\(\"Same label\"\\), B",
               all = FALSE)
  persona <- agents$P1$persona_description
  expect_true(grepl("Question: Same label [A]", persona, fixed = TRUE))
  expect_true(grepl("Question: Same label [B]", persona, fixed = TRUE))
})

test_that("a scale whose endpoint label may have been cut is flagged by its endpoint", {
  long <- .cut80(paste("1. Government should see to it that every person has a job",
                       "and a good standard of living"))
  d <- data.frame(AGE = c(30, 40),
                  JOBS = .lab(c(3, 7), stats::setNames(c(1, 7), c(long, "7. Let each person get ahead"))))
  path <- tempfile(fileext = ".dta"); on.exit(unlink(path))
  haven::write_dta(d, path)
  build <- function(...) create_agents_from_survey(
    2, path, LLMR::llm_config("openai", "example-model"), demographic_vars = c(age = "AGE"),
    survey_vars = "JOBS", question_wording = c(JOBS = "Jobs and living standards?"), ...)
  expect_warning(agents <- build(),
                 "80 characters long may have been cut short: JOBS scale endpoint 1 \\(P1\\)\\.")
  report <- persona_rendering_report(agents)
  expect_identical(report$status[report$variable == "JOBS"], c("fallback", "rendered"))
  expect_match(report$reason[report$variable == "JOBS"][1], "scale endpoint 1 is 80 characters")
  full <- data.frame(variable = "JOBS", code = 1, label = paste(
    "Government should see to it that every person has a job and a good standard of living"))
  expect_no_warning(agents <- build(value_labels = full))
  expect_match(persona_rendering_report(agents)$text[2],
               "where 1 is Government should see to it that every person has a job and a good standard of living and 7")
})

test_that("records in list form keep building, one value per field", {
  cfg <- LLMR::llm_config("openai", "example-model")
  agents <- create_agents(1, cfg, demographics = list(list(age = 30, kids = list(c(1, 2)))))
  report <- persona_rendering_report(agents)
  expect_identical(report$variable, c("age", "kids"))
  expect_identical(report$status, c("rendered", "dropped"))
  expect_identical(report$reason[2], "the field does not hold a single value")
  expect_true(grepl("age: 30", agents$P1$persona_description, fixed = TRUE))

  agents <- create_agents(1, cfg, demographics = list(list(30, gender = "F")))
  report <- persona_rendering_report(agents)
  expect_identical(report$variable, "gender")
  expect_identical(report$text, "F")

  agents <- create_agents(1, cfg, demographics = list(list(
    languages = c("Persian", "English"), frame = data.frame(a = 1:2))))
  report <- persona_rendering_report(agents)
  expect_identical(report$status, c("fallback", "dropped"))
  expect_identical(report$text, c("Persian", NA))
  expect_identical(report$reason[1], "the field holds 2 values; the persona shows the first")
})

test_that("a labeled midpoint rules out the scale form; endpoint-only scales keep it", {
  na <- .fg_default_na_strings
  render <- function(values, labels) .fg_render_column(.lab(values, labels), "x", na)
  expect_identical(render(2, c("0. Dissatisfied" = 0, "5. Neither" = 5,
                               "10. Satisfied" = 10))$rendered_as, "code")
  expect_identical(render(2, c("1. Liberal" = 1, "4. Moderate" = 4,
                               "7. Conservative" = 7))$rendered_as, "code")
  anchors <- stats::setNames(seq(0, 100, by = 15)[1:7], paste0(seq(0, 90, by = 15), ". Anchor"))
  anchors <- c(anchors, "100. Very warm" = 100)
  expect_identical(render(c(10, 20, 92), anchors)$rendered_as, rep("code", 3))
  expect_identical(render(5, c("1. Fewer" = 1, "7. More" = 7, "99. Haven't thought" = 99))$text,
                   "On a scale of 1 to 7, where 1 is Fewer and 7 is More, you place yourself at 5.")
  expect_identical(render(3, c("0. Left" = 0, "10. Right" = 10, "96. Do not know enough" = 96))$rendered_as,
                   "scale")
})

test_that("scale_template sets the sentence for each item and is recorded", {
  path <- .render_file(); on.exit(unlink(path))
  own <- "From {lo} ({lo_label}) to {hi} ({hi_label}), you chose {value}."
  agents <- .quiet(.render_agents(path, rows = 1, scale_template = own))
  report <- persona_rendering_report(agents)
  serv <- report[report$variable == "SERV", ]
  expect_identical(serv$text, paste("From 1 (Government should provide many fewer services) to 7",
                                    "(Government should provide many more services), you chose 5."))
  expect_identical(serv$template, own)
  expect_true(all(is.na(report$template[report$variable != "SERV"])))

  per_item <- c("Default {value}.", SERV = "Your placement of the candidate is {value}.")
  agents <- .quiet(.render_agents(path, rows = 1, scale_template = per_item))
  report <- persona_rendering_report(agents)
  expect_identical(report$text[report$variable == "SERV"], "Your placement of the candidate is 5.")
  agents <- .quiet(.render_agents(path, rows = 1, scale_template = c(THERM = "T {value}.")))
  report <- persona_rendering_report(agents)
  expect_identical(report$template[report$variable == "SERV"], .fg_default_scale_template)

  w <- testthat::capture_warnings(.render_agents(path, rows = 1,
                                                 scale_template = c(NOPE = "N {value}.")))
  expect_match(w, "`scale_template` names variables that are not requested.*NOPE", all = FALSE)
  expect_error(.render_agents(path, rows = 1, scale_template = "At {val}."),
               "unknown placeholder\\(s\\) \\{val\\}")
  expect_error(.render_agents(path, rows = 1, scale_template = "From {lo} to {hi}."),
               "must contain \\{value\\}")
  for (bad in list(c("A {value}", "B {value}"), NA_character_, "", 1,
                   c(SERV = "A {value}", SERV = "B {value}"))) {
    expect_error(.render_agents(path, rows = 1, scale_template = bad),
                 "`scale_template` must be one template")
  }
})

test_that("the report check matches whole fields, not substrings", {
  cfg <- LLMR::llm_config("openai", "example-model")
  agents <- create_agents(1, cfg, demographics = data.frame(age = 25),
                          survey_responses = data.frame(often = "Yes, always"))
  report <- persona_rendering_report(agents)
  report$issue <- NA_character_
  expect_silent(.fg_check_report(report, agents))
  short <- report
  short$text[short$variable == "age"] <- "2"
  expect_error(.fg_check_report(short, agents), "does not match the persona text for P1 age")
  partial <- report
  partial$text[partial$variable == "often"] <- "Yes"
  expect_error(.fg_check_report(partial, agents), "does not match the persona text for P1 often")
  hidden <- report
  hidden$status[hidden$variable == "age"] <- "dropped"
  expect_error(.fg_check_report(hidden, agents), "does not match the persona text for P1 age")
  withr::local_options(focusgroup.persona_style = "paragraph")
  agents <- create_agents(1, cfg, demographics = data.frame(age = 25),
                          survey_responses = data.frame(often = "Yes, always"))
  short$agent_id <- "P1"
  expect_error(.fg_check_report(short, agents), "does not match")
  expect_silent(.fg_check_report(persona_rendering_report(agents), agents))
})

test_that("numbers are written out in full, and stray requests are named", {
  d <- data.frame(AGE = c(30, 40), LOAN = c(100000, 2.5e6),
                  ANS = .lab(c(1, 2), c("1. Yes" = 1, "2. No" = 2), strrep("Q", 80)))
  path <- tempfile(fileext = ".dta"); on.exit(unlink(path))
  haven::write_dta(d, path)
  cfg <- LLMR::llm_config("openai", "example-model")
  w <- testthat::capture_warnings(agents <- create_agents_from_survey(
    2, path, cfg, demographic_vars = c(age = "AGE", years = "AGE", loan = "LOAN"),
    survey_vars = c("ANS", "ANS"),
    value_labels = data.frame(variable = c("ANS", "TYPO"), code = 1, label = "Yes")))
  expect_match(w, "requested more than once and are rendered once, under their first entry: AGE, ANS",
               all = FALSE)
  expect_match(w, "`value_labels` names variables that are not requested.*: TYPO\\.", all = FALSE)
  expect_match(w, "Variable labels exactly 80 characters long.*may have been cut short: ANS", all = FALSE)
  report <- persona_rendering_report(agents)
  expect_identical(report$text[report$variable == "LOAN"], c("100000", "2500000"))
  expect_true(grepl("loan: 100000", agents$P1$persona_description, fixed = TRUE))
  expect_identical(sum(report$variable == "AGE" & report$agent_id == "P1"), 1L)

  plain <- create_agents(1, cfg, demographics = data.frame(income = 100000))
  expect_true(grepl("income: 100000", plain$P1$persona_description, fixed = TRUE))
  expect_identical(persona_rendering_report(plain)$text, "100000")
})

test_that("a numeric column labeled only at its tagged missing values keeps its numbers", {
  x <- haven::labelled(c(12, haven::tagged_na("d"), 250000),
                       c("Don't know" = haven::tagged_na("d")))
  r <- .fg_render_column(x, "x", .fg_default_na_strings)
  expect_identical(r$text, c("12", NA, "250000"))
  expect_identical(r$reason[2], "missing value: Don't know")
})

# The same records as an in-memory frame: the file read back with haven, the
# question wording and field names in a dictionary.
.render_frame <- function(path) {
  frame <- haven::read_dta(path)
  attr(frame, "dictionary") <- data.frame(
    handle = c("AGE", "RELIG", "EMPLOY", names(.wording)),
    question = c("age", "religion", "employment", unname(.wording)),
    stringsAsFactors = FALSE)
  frame
}

test_that("a labelled frame renders as the same records read from the file", {
  path <- .render_file(); on.exit(unlink(path))
  cfg <- LLMR::llm_config("openai", "example-model")
  from_file <- .quiet(.render_agents(path, rows = 1:4))
  frame <- .render_frame(path)
  w <- testthat::capture_warnings(m <- testthat::capture_messages(
    from_data <- create_agents_from_data(frame, 4, cfg,
                                         demographic_cols = c("AGE", "RELIG", "EMPLOY"))))
  for (id in paste0("P", 1:4)) {
    expect_identical(from_data[[id]]$persona_description, from_file[[id]]$persona_description)
  }
  file_report <- persona_rendering_report(from_file)
  data_report <- persona_rendering_report(from_data)
  # The identifier column is reported as dropped, and only on the data path.
  ids <- data_report[data_report$variable == "id", ]
  expect_identical(unique(ids$reason), "identifier column")
  data_report <- data_report[data_report$variable != "id", ]
  rownames(data_report) <- NULL
  shared <- c("agent_id", "kind", "variable", "question", "code", "text",
              "rendered_as", "status", "reason", "template")
  expect_identical(data_report[shared], file_report[shared])
  expect_true(any(data_report$rendered_as %in% "scale"))
  expect_identical(data_report$reason[data_report$variable == "POSTQ" & data_report$agent_id == "P3"],
                   "missing value: -6. No post interview")

  # The warnings and the message name the same codes, with the data path's remedy.
  expect_match(w, "bare code because the column has no value label.*RELIG code 9 \\(P1\\).*column's value labels",
               all = FALSE)
  expect_match(w, "80 characters long.*EMPLOY code 1 \\(P1, P2\\); IMMIG code 1 \\(P1, P3\\)",
               all = FALSE)
  expect_match(w, "Negative codes without a value label.*POSTQ code -1 \\(P2\\)", all = FALSE)
  expect_false(any(grepl("value_labels", w, fixed = TRUE)))
  expect_match(m, "4 answer\\(s\\) were left out of the personas as missing values \\(POSTQ; AGE; RELIG; SERV\\)",
               all = FALSE)

  # scale_template applies by column, as it does by variable for a file.
  own <- c(SERV = "From {lo} ({lo_label}) to {hi} ({hi_label}), you chose {value}.")
  agents <- .quiet(create_agents_from_data(frame, 4, cfg, demographic_cols = "AGE",
                                           scale_template = own))
  report <- persona_rendering_report(agents)
  expect_identical(report$text[report$variable == "SERV" & report$agent_id == "P1"],
                   paste("From 1 (Government should provide many fewer services) to 7",
                         "(Government should provide many more services), you chose 5."))
  w <- testthat::capture_warnings(suppressMessages(create_agents_from_data(
    frame, 4, cfg, demographic_cols = "AGE", scale_template = c(NOPE = "At {value}."))))
  expect_match(w, "`scale_template` names columns that are not in `data`.*NOPE", all = FALSE)
})

test_that("missing-data labels in text and factor columns are left out of a frame's personas", {
  cfg <- LLMR::llm_config("openai", "example-model")
  long_answer <- "My landlord refused to fix the heat all winter, so I moved."
  frame <- data.frame(
    age = c("30", "40", "50", "60", "70"),
    view = c("-9. Refused", "Strongly agree", "-3. RESTRICTED", "There is an error", "Terrorism"),
    reason = c(long_answer, "Don't know", "Higher rents downtown", "No answer", "-5. Missing; Not available"),
    party = factor(c("Don't know", "Democrat", "Republican", "Democrat", "Refused")),
    income = c(100000, 2.5, -9, 1e6, NA),
    stringsAsFactors = FALSE)
  expect_message(agents <- create_agents_from_data(frame, 5, cfg, demographic_cols = "age"),
                 "7 answer\\(s\\) were left out of the personas as missing values \\(view; party; reason\\)")
  report <- persona_rendering_report(agents)
  cell <- function(id, variable) report[report$agent_id == id & report$variable == variable, ]
  expect_identical(cell("P1", "view")$reason, "missing value: -9. Refused")
  expect_identical(cell("P3", "view")$reason, "missing value: -3. RESTRICTED")
  expect_identical(cell("P1", "party")$reason, "missing value: Don't know")
  expect_identical(cell("P5", "party")$reason, "missing value: Refused")
  expect_identical(cell("P2", "reason")$status, "dropped")
  expect_identical(cell("P5", "reason")$status, "dropped")
  expect_false(grepl("Refused", agents$P1$persona_description))
  expect_false(grepl("RESTRICTED", agents$P3$persona_description))

  # Answers that merely mention missing-data words are kept, and so is longer
  # text that uses them; numbers are shown as given; an NA is not announced.
  expect_identical(cell("P4", "view")$text, "There is an error")
  expect_identical(cell("P5", "view")$text, "Terrorism")
  expect_identical(cell("P1", "reason")$text, long_answer)
  expect_identical(cell("P3", "reason")$text, "Higher rents downtown")
  expect_identical(report$text[report$variable == "income"], c("100000", "2.5", "-9", "1000000", NA))
  expect_identical(cell("P5", "income")$reason, "no value")

  # `na_strings` can be replaced or emptied, and does not reach numbers.
  expect_message(mine <- create_agents_from_data(frame, 5, cfg, demographic_cols = "age",
                                                 na_strings = c("^-9$", "terrorism")),
                 "1 answer\\(s\\) were left out.*\\(view\\)")
  report <- persona_rendering_report(mine)
  expect_identical(report$text[report$variable == "income"][[3]], "-9")
  expect_identical(report$status[report$variable == "view"],
                   c("rendered", "rendered", "rendered", "rendered", "dropped"))
  none <- create_agents_from_data(frame, 5, cfg, demographic_cols = "age", na_strings = character())
  expect_identical(persona_rendering_report(none)$text[1:2], c("30", "-9. Refused"))
  expect_error(create_agents_from_data(frame, 5, cfg, na_strings = c("refused", "")),
               "non-empty patterns")
  expect_error(create_agents_from_data(frame, 5, cfg, na_strings = 9), "non-empty patterns")
})

test_that("a plain frame of real answers renders as before", {
  cfg <- LLMR::llm_config("openai", "example-model")
  frame <- data.frame(age = c(34, 61), occupation = c("accountant", "school administrator"),
                      `How often do you work remotely?` = c("three days a week", "never"),
                      check.names = FALSE, stringsAsFactors = FALSE)
  expect_silent(agents <- create_agents_from_data(frame, 2, cfg,
                                                  demographic_cols = c("age", "occupation")))
  expect_identical(agents$P1$persona_description, generate_persona(
    list(age = "34", occupation = "accountant"),
    list(`How often do you work remotely?` = "three days a week")))
  expect_identical(agents$P2$persona_description, generate_persona(
    list(age = "61", occupation = "school administrator"),
    list(`How often do you work remotely?` = "never")))
  expect_true(all(persona_rendering_report(agents)$status == "rendered"))
})

test_that("SPSS user-defined missing values in a frame are left out", {
  cfg <- LLMR::llm_config("openai", "example-model")
  frame <- data.frame(age = c(30, 40, 50))
  frame$favor <- haven::labelled_spss(c(1, 8, 2), c(Favor = 1, Oppose = 2, "No opinion" = 8),
                                      na_values = 8)
  expect_message(agents <- create_agents_from_data(frame, 3, cfg, demographic_cols = "age"),
                 "1 answer\\(s\\) were left out.*\\(favor\\)")
  report <- persona_rendering_report(agents)
  favor <- report[report$variable == "favor", ]
  expect_identical(favor$text, c("Favor", NA, "Oppose"))
  expect_identical(favor$reason[[2]], "missing value: No opinion (user-defined missing value)")
  expect_false(grepl("No opinion", agents$P2$persona_description))
})

test_that("a pre-rendered persona panel keeps its text and its fields as given", {
  cfg <- LLMR::llm_config("openai", "example-model")
  panel <- data.frame(persona_id = c("a", "b"),
                      persona = c("A renter who rides the evening bus.", "A retired driver."),
                      view = c("-9. Refused", "Agree"), stringsAsFactors = FALSE)
  panel$party <- haven::labelled(c(1, -8), c("1. Democrat" = 1, "-8. Don't know" = -8))
  expect_silent(agents <- create_agents_from_data(panel, 2, cfg, demographic_cols = character()))
  expect_identical(agents$P1$persona_description, "A renter who rides the evening bus.")
  expect_identical(agents$P1$survey_responses$view, "-9. Refused")
  expect_null(attr(agents, "rendering_report"))
})

test_that("open-ended text in a survey file that mentions missing-data words is kept", {
  d <- data.frame(AGE = c(30, 40, 50),
                  WHY = c("My landlord refused to fix the heat all winter, so I moved.",
                          "Refused", "-9. Refused"),
                  stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".dta"); on.exit(unlink(path))
  haven::write_dta(d, path)
  agents <- suppressMessages(create_agents_from_survey(
    3, path, LLMR::llm_config("openai", "example-model"), demographic_vars = c(age = "AGE"),
    survey_vars = "WHY", question_wording = c(WHY = "Why did you move?")))
  why <- persona_rendering_report(agents)
  why <- why[why$variable == "WHY", ]
  expect_identical(why$status, c("rendered", "dropped", "dropped"))
  expect_identical(why$text[[1]], d$WHY[[1]])
})

test_that("text without a code is missing only when it says nothing but missing-data wording", {
  na <- fg_default_na_strings()
  missing <- c("Refused", "Don't know", "I don't know", "Refused to answer",
               "Not applicable (skipped)", "No answer", "Missing; Not available",
               "-9. Refused", "97. Refused", "Legitimate skip", "DK/RF",
               "-3. RESTRICTED", "Inapplicable", "Not asked", "-1. Inapplicable",
               "Technical error")
  answers <- c("I keep missing the bus", "My landlord refused to fix the heat all winter",
               "Missing person case", "There is an error", "Terrorism", "Not sure",
               "Don't know enough to say", "I refused the offer", "Evening hours",
               "The kids are missing out")
  expect_true(all(.fg_missing_text(missing, na)))
  expect_false(any(.fg_missing_text(answers, na)))
  expect_false(any(.fg_missing_text(answers, character())))
})

# The cases the one missing-data rule decides, with the code each shows (NA
# when the text shows none). The rule must give the same answer for a plain
# value and for the value label of that code in a labelled column.
.missing_cases <- data.frame(text = c(
  "-9. Refused", "-9 Refused", "[-8] Don't know", "(-8) Don't know",
  "{VOL} Don't know", "Don't know (VOL)", "Don't know/Refused (VOL.)",
  "Refused (VOL)", "Refused / don't know", "Not asked in survey", "97. Refused",
  "998. Don't know (where to rate)", "999. Don't recognize (don't know who this is)",
  "9. Missing", "-7. Insufficient partial, interview deleted",
  "-2. DK/RF in V241288 or V241289", "-5. Sufficient partial, breakoff",
  "Inapplicable, legitimate skip", "Refused to answer", "I don't know", "No answer",
  "Missing; Not available", "Error", "2. Error",
  as.character(haven::as_factor(haven::labelled(-9, c(Refused = -9)), levels = "both")),
  "5. Missing the old system", "2. Refused to take sides", "Missing the old system",
  "I keep missing the bus", "My landlord refused to fix the heat",
  "Missing person case", "There is an error", "Terrorism", "-3. Strongly disagree",
  "70 on a 0-100 feeling thermometer", "5 on a scale from 1 (A) to 7 (B)", "Not sure"),
  code = c(-9, -9, -8, -8, NA, NA, NA, NA, NA, NA, 97, 998, 999, 9, -7, -2, -5,
           NA, NA, NA, NA, NA, NA, 2, -9, 5, 2, NA, NA, NA, NA, NA, NA, -3, 70, 5, NA),
  missing = rep(c(TRUE, FALSE), c(25, 12)), stringsAsFactors = FALSE)

test_that("one rule decides whether plain values and value labels are missing", {
  na <- fg_default_na_strings()
  cases <- .missing_cases
  expect_identical(cases$text[[25]], "[-9] Refused")
  decided <- .fg_missing_text(cases$text, na)
  expect_identical(cases$text[decided & !cases$missing], character())
  expect_identical(cases$text[!decided & cases$missing], character())

  # As value labels of the codes they show, in labelled columns.
  coded <- cases[!is.na(cases$code), ]
  status <- vapply(seq_len(nrow(coded)), function(i) {
    col <- haven::labelled(coded$code[[i]], stats::setNames(coded$code[[i]], coded$text[[i]]))
    .fg_render_column(col, "v", na)$status
  }, character(1))
  expect_identical(coded$text[(status == "dropped") != coded$missing], character())
  expect_identical(.fg_missing_text(coded$text, na, code = coded$code), coded$missing)

  # A known code decides over the text: a label without a code prefix is
  # missing under a negative code or one of 90 and above, and an answer that
  # uses missing-data words under a substantive code is kept.
  labels <- c("Refused to take sides", "Don't know enough to say", "Refused")
  expect_identical(.fg_missing_text(labels, na, code = c(-9, 98, 3)), c(TRUE, TRUE, TRUE))
  expect_identical(.fg_missing_text(labels, na, code = c(2, 3, NA)), c(FALSE, FALSE, TRUE))
  expect_error(.fg_missing_text(labels, na, code = 1:2), "one code per text")
  # A decimal is not a code prefix, and nothing matched means nothing missing.
  expect_false(.fg_missing_text("1.5 refused rides", na))
  expect_identical(.fg_missing_text(c(NA, ""), na), c(FALSE, FALSE))
  expect_identical(.fg_missing_text(cases$text, character()), rep(FALSE, nrow(cases)))
})

test_that("a frame decoded with as_factor() and the labelled frame give the same personas", {
  cfg <- LLMR::llm_config("openai", "example-model")
  party <- haven::labelled(c(1, 2, -9, -8, 98, 3),
                           c(Democrat = 1, Republican = 2, Refused = -9, "Don't know" = -8,
                             "Don't know/Refused (VOL.)" = 98, "Refused to take sides" = 3))
  d <- data.frame(age = c("30-44", "45-64", "65+", "18-29", "30-44", "45-64"))
  d$both <- haven::as_factor(party, levels = "both")
  d$labels <- haven::as_factor(party, levels = "labels")
  d$coded <- party
  a <- suppressWarnings(suppressMessages(
    create_agents_from_data(d, 6, cfg, demographic_cols = "age", rows = 1:6)))
  r <- persona_rendering_report(a)
  for (v in c("both", "labels", "coded")) {
    expect_identical(r$status[r$variable == v] == "dropped",
                     c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE), info = v)
  }
  expect_false(grepl("[-9] Refused", a$P3$persona_description, fixed = TRUE))
  expect_true(grepl("Refused to take sides", a$P6$persona_description, fixed = TRUE))
})

test_that("create_agents() leaves out missing-data labels of the frames it is given", {
  cfg <- LLMR::llm_config("openai", "example-model")
  demo <- data.frame(age = c(34, -9, 51),
                     occupation = c("Refused", "I keep missing the bus", "nurse"),
                     party = factor(c("-9. Refused", "Democrat", "Don't know")),
                     stringsAsFactors = FALSE)
  survey <- data.frame(`Where do you get news?` = c("Don't know", "The radio",
                                                   "My landlord refused to say"),
                       check.names = FALSE)
  expect_message(agents <- create_agents(3, cfg, demographics = demo,
                                         survey_responses = survey),
                 "4 answer\\(s\\) were left out.*occupation; party; Where do you get news\\?")
  personas <- vapply(agents, function(a) a$persona_description, character(1))
  expect_false(grepl("Refused|Don't know", personas[["P1"]]))
  # Answers that use the words are kept, and numbers are never read as labels.
  expect_match(personas[["P2"]], "occupation: I keep missing the bus", fixed = TRUE)
  expect_match(personas[["P2"]], "age: -9", fixed = TRUE)
  expect_match(personas[["P3"]], "Answer: My landlord refused to say", fixed = TRUE)
  report <- persona_rendering_report(agents)
  dropped <- report[report$status == "dropped", ]
  expect_identical(paste(dropped$agent_id, dropped$variable),
                   c("P1 occupation", "P1 party", "P1 Where do you get news?", "P3 party"))
  expect_identical(dropped$reason, c("missing value: Refused", "missing value: -9. Refused",
                                     "missing value: Don't know", "missing value: Don't know"))
  # The raw record keeps every value.
  expect_identical(agents$P1$demographics$occupation, "Refused")

  # character() turns the screening off; an NA is left out without a message.
  expect_silent(off <- create_agents(3, cfg, demographics = demo, na_strings = character()))
  expect_match(off$P1$persona_description, "occupation: Refused", fixed = TRUE)
  expect_silent(create_agents(2, cfg, demographics = data.frame(job = c("baker", NA))))
  expect_error(create_agents(3, cfg, demographics = demo, na_strings = ""), "na_strings")

  # A frame the other constructors screened is not screened again.
  marked <- demo
  attr(marked, "focusgroup_screened") <- TRUE
  expect_silent(kept <- create_agents(3, cfg, demographics = marked))
  expect_match(kept$P1$persona_description, "occupation: Refused", fixed = TRUE)
})
