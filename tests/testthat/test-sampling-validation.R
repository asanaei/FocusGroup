.sampling_data <- function() data.frame(
  id = factor(c("a", "b", "b", NA)), respondent_id = c(10L, 20L, 30L, 40L),
  persona_id = c("x", "y", "z", "w"), age = c(25, 35, 45, 55), weight = 1:4)

.sampling_agents <- function(data = .sampling_data(), n = 1L, ...) {
  create_agents_from_data(data, n, LLMR::llm_config("openai", "example-model"),
                          demographic_cols = "age", ...)
}

test_that("weights reject invalid types and original rows before selection", {
  expect_error(.sampling_agents(weights = "absent"), "Weight column 'absent'")
  invalid <- list(factor(1:4), as.character(1:4), c(TRUE, FALSE, TRUE, FALSE),
                  as.complex(1:4), matrix(1:4, 4), 1:3, 1:5)
  for (w in invalid) expect_error(.sampling_agents(weights = w), "weights.*vector|weights.*length")
  for (value in c(-1, NA, NaN, Inf, -Inf)) {
    d <- .sampling_data(); d$weight[4] <- value
    expect_error(.sampling_agents(d, rows = 1, weights = "weight"), "column 'weight'.*source row.*4")
  }
  d <- .sampling_data(); d$weight <- factor(1:4)
  expect_error(.sampling_agents(d, weights = "weight"), "column 'weight'.*vector")
  expect_error(.sampling_agents(weights = rep(0, 4)), "positive weight")
})

test_that("eligibility cannot conceal zero weights or duplicate respondents", {
  expect_error(.sampling_agents(rows = 2:4, weights = c(1, 0, 0, 0)), "eligible respondents")
  expect_error(.sampling_agents(n = 2, weights = c(1, 0, 0, 0)), "positive weight; found 1")
  expect_error(.sampling_agents(n = 3, rows = c(2, 2, 1)), "only 2 distinct")
  expect_error(.sampling_agents(.sampling_data()[FALSE, ]), "only 0 distinct")
  expect_error(.sampling_agents(rows = integer()), "only 0 distinct")
  census <- .sampling_agents(n = 4, weights = c(1, 0, 0, 0))
  expect_identical(unname(attr(census, "source_rows")), 1:4)
})

test_that("row selectors validate positions and evaluate predicates once", {
  for (rows in list(c(1, 2.5), c(1, NA), c(1, Inf), c(1, 5), c(1, 0))) {
    expect_error(.sampling_agents(rows = rows), "position.*2")
  }
  expect_error(.sampling_agents(rows = c(TRUE, FALSE)), "logical value per respondent")
  for (f in list(function(d) 1:4, function(d) TRUE, function(d) c("TRUE", "TRUE", "TRUE", "TRUE"))) {
    expect_error(.sampling_agents(rows = f), "predicate.*position")
  }
  # NA in a logical selector or a predicate's result counts as not selected.
  na_rows <- .sampling_agents(n = 2, rows = c(TRUE, NA, FALSE, TRUE))
  expect_identical(unname(attr(na_rows, "source_rows")), c(1L, 4L))
  d <- .sampling_data(); d$age[2] <- NA
  na_pred <- .sampling_agents(d, n = 2, rows = function(df) df[[1]] != "25")
  expect_identical(unname(attr(na_pred, "source_rows")), c(3L, 4L))
  expect_error(.sampling_agents(d, n = 3, rows = function(df) df[[1]] != "25"),
               "only 2 distinct")
  calls <- 0L
  d <- .sampling_data()
  attr(d, "dictionary") <- data.frame(handle = "age", question = "Age in years")
  a <- .sampling_agents(d, n = 2, rows = function(df) {
    calls <<- calls + 1L
    expect_named(df, "Age in years")
    df[[1]] %in% c("35", "55")
  })
  expect_identical(calls, 1L)
  expect_identical(unname(attr(a, "source_rows")), c(2L, 4L))
  a <- .sampling_agents(n = 3, rows = c(4, 2, 4, 1))
  expect_identical(unname(attr(a, "source_rows")), c(4L, 2L, 1L))
})

test_that("valid weighted draws retain normalization, ordering, and RNG state", {
  withr::local_seed(110)
  before <- .Random.seed
  withr::local_options(list(focusgroup.seed = 110L))
  a <- .sampling_agents(n = 2, rows = c(4, 2, 1, 3), weights = "weight")
  expect_identical(attr(a, "source_rows"), c(P1 = 3L, P2 = 4L))
  expect_identical(.Random.seed, before)
  b <- .sampling_agents(n = 2, rows = c(4, 2, 1, 3), weights = 1:4)
  c <- .sampling_agents(n = 2, rows = c(4, 2, 1, 3), weights = haven::labelled(1:4, c(first = 1)))
  expect_identical(attr(a, "source_rows"), attr(b, "source_rows"))
  expect_identical(attr(a, "source_rows"), attr(c, "source_rows"))
  expect_no_warning(.sampling_agents(n = 2, weights = rep(.Machine$double.xmax, 4)))
  expect_identical(attr(a, "source_ids"),
    structure(.sampling_data()[c(3, 4), c("id", "respondent_id", "persona_id")], row.names = c("P1", "P2")))
  expect_null(attr(.sampling_agents(data.frame(age = 20:23)), "source_ids"))
  repeated <- .sampling_agents(n = 2, rows = 2:3)
  expect_identical(as.character(attr(repeated, "source_ids")$id), c("b", "b"))
  expect_s3_class(attr(repeated, "source_ids")$id, "factor")
})

test_that("survey weights are excluded and provenance maps through missing demographics", {
  d <- data.frame(id = 11:15, age = c(NA, 20, 30, 40, 50), weight = 1:5, answer = letters[1:5])
  path <- tempfile(fileext = ".dta"); on.exit(unlink(path))
  haven::write_dta(d, path)
  cfg <- LLMR::llm_config("openai", "example-model")
  call <- function(...) create_agents_from_survey(2, path, cfg, weights = "weight", ...)
  withr::local_options(list(focusgroup.seed = 110L))
  expect_warning(expect_message(a <- call(demographic_vars = c(age = "age", w = "weight"),
    survey_vars = c("weight", "answer"), rows = c(5, 1, 3, 2)), "1 selected respondent"),
    "answer \\(no label\\)")
  expect_identical(unname(attr(a, "source_rows")), c(3L, 5L))
  expect_identical(as.numeric(attr(a, "source_ids")$id), c(13, 15))
  expect_false(any(grepl("weight|w:", vapply(a[1:2], function(x) x$persona_description, ""))))
  expect_error(call(demographic_vars = "weight"), "No usable.*weight column")
  b <- call(demographic_vars = "age", rows = c(5, 2))
  expect_identical(unname(attr(b, "source_rows")), c(2L, 5L))
})
