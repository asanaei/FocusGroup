# Offline tests for persona construction and the prompt-rule options. These run
# inside the package namespace (see tests/testthat.R), so the internal helpers
# generate_persona() and .fg_standing_rules() are visible by bare name.

test_that("the persona states demographics without inferring dispositions", {
  demo <- list(age = 22, gender = "female", education = "PhD",
               income = "high", urban_rural = "rural")
  p <- generate_persona(demo)
  expect_type(p, "character")
  expect_length(p, 1L)
  # the supplied facts appear...
  expect_true(grepl("22", p))
  expect_true(grepl("female", p))
  expect_true(grepl("rural", p))
  # ...but none of the old scripted dispositions are invented
  expect_false(grepl("digitally native", p))
  expect_false(grepl("traditional values", p))
  expect_false(grepl("think analytically", p))
  expect_false(grepl("rural-specific", p))
})

test_that("the persona renders the FULL row, not just age/gender/education", {
  demo <- list(age = 40, race = "Black", income = "Under $30k", region = "South")
  resp <- list("Party identification" = "Strong Democrat",
               "How worried about crime" = "Very worried")
  p <- generate_persona(demo, resp)
  # demographics beyond the old three-field subset are present
  expect_true(grepl("Black", p))
  expect_true(grepl("Under \\$30k", p))
  expect_true(grepl("South", p))
  # survey answers, keyed by question text, are present
  expect_true(grepl("Party identification", p))
  expect_true(grepl("Strong Democrat", p))
  expect_true(grepl("Very worried", p))
  # race is still not mapped to a disposition
  expect_false(grepl("racial justice", p))
})

test_that("the paragraph style renders the same facts in prose", {
  demo <- list(age = 63, education = "High school", region = "rural Midwest")
  resp <- list("Ideology" = "Conservative")
  p <- generate_persona(demo, resp, style = "paragraph")
  expect_false(grepl("Question:", p))          # no Q/A scaffolding
  expect_true(grepl("63", p))
  expect_true(grepl("Conservative", p))
})

test_that("focusgroup.persona_style switches the default style", {
  demo <- list(age = 30); resp <- list("Ideology" = "Moderate")
  withr::with_options(list(focusgroup.persona_style = "paragraph"), {
    expect_false(grepl("Question:", generate_persona(demo, resp)))
  })
  withr::with_options(list(focusgroup.persona_style = "labeled"), {
    expect_true(grepl("Question:", generate_persona(demo, resp)))
  })
})

test_that("participant standing rules default to transparent disclosure", {
  r <- .fg_standing_rules(is_moderator = FALSE)
  expect_true(grepl("simulated research exercise", r))
  expect_false(grepl("Never reveal you are an AI", r))
})

test_that("the disclosure option switches the participant rule", {
  withr::with_options(list(focusgroup.disclosure = "conceal"), {
    expect_true(grepl("Never reveal you are an AI model", .fg_standing_rules(FALSE)))
  })
  withr::with_options(list(focusgroup.disclosure = "silent"), {
    r <- .fg_standing_rules(FALSE)
    expect_false(grepl("Never reveal you are an AI", r))
    expect_false(grepl("simulated research exercise", r))
  })
})

test_that("participant rules can be replaced wholesale via an option", {
  withr::with_options(list(focusgroup.participant_rules = c("Rule one.", "Rule two.")), {
    expect_identical(.fg_standing_rules(FALSE), "Rule one.\nRule two.")
  })
})
