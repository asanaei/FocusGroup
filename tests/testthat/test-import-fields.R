.import_fields <- function() {
  d <- data.frame(speaker = c("Chair", "", "Ana", "Ben"), text = c("Hours?", NA, "Evenings.", NA),
    message_id = factor(c("10", "15", "20", "30")), round = c("1", "2", NA, "3"),
    is_moderator = c("TRUE", "FALSE", "FALSE", "FALSE"), phase = c("opening", "dropped", NA, "discussion"),
    response_id = factor(letters[1:4]), finish_reason = c("stop", "length", NA, "stop"),
    sent_tokens = c("0", "1", NA, "3"), rec_tokens = c(2L, 3L, NA, 5L),
    total_tokens = c(7, 8, NA, 10), duration_s = c(0.1, 0.2, NA, 0.4),
    provider = factor(c("one", "two", NA, "four")), model = c("a", "b", NA, "d"),
    timestamp = c("2026-01-01T12:00:00Z", "invalid", "2026-01-01T14:00:00+02:00", NA))
  d$metadata <- I(list(list(value = 1L), list(value = 2L), NULL, list(value = 4L)))
  d
}

test_that("import preserves fields after dropping rows and normalizes canonical output", {
  warnings <- character()
  d <- .import_fields()
  fg <- withCallingHandlers(focus_group_from_transcript(d), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
  })
  expect_length(warnings, 2L)
  expect_match(warnings[1], "Dropped 1 row")
  expect_match(warnings[2], "1 utterance.*missing text")
  out <- .fg_transcript_data(fg$conversation_log)
  expect_identical(out$message_id, c(10L, 20L, 30L))
  expect_identical(out$round, c(1L, NA_integer_, 3L))
  expect_identical(out$text, c("Hours?", "Evenings.", ""))
  expect_identical(out$phase, c("opening", NA_character_, "discussion"))
  expect_identical(out$response_id, c("a", "c", "d"))
  expect_identical(out$finish_reason, c("stop", NA_character_, "stop"))
  expect_identical(out$sent_tokens, c(0L, NA_integer_, 3L))
  expect_identical(out$rec_tokens, c(2L, NA_integer_, 5L))
  expect_identical(out$total_tokens, c(7L, NA_integer_, 10L))
  expect_equal(out$duration_s, c(0.1, NA, 0.4))
  expect_identical(out$provider, c("one", NA_character_, "four"))
  expect_identical(out$model, c("a", NA_character_, "d"))
  expect_identical(out$metadata, I(list(list(value = 1L), list(), list(value = 4L))))
  expect_equal(as.numeric(out$timestamp[1:2]), rep(as.numeric(as.POSIXct("2026-01-01 12:00:00", tz = "UTC")), 2))
  expect_identical(attr(out$timestamp, "tzone"), "UTC")
  expect_identical(fg$moderator_id, "Chair"); expect_true(fg$agents$Chair$is_moderator)
  expect_equal(fg$total_tokens, 0); expect_equal(fg$calls_usage_unknown, 0)
  prepared <- suppressWarnings(.fg_prepare_analysis_source(d))
  expect_identical(prepared$transcript, out)
  expect_identical(prepared$focus_group$conversation_log, fg$conversation_log)
  names(d)[1:2] <- c("who", "said")
  prepared <- suppressWarnings(.fg_prepare_analysis_source(d, "who", "said"))
  expect_identical(prepared$transcript, out)
  csv <- .fg_transcript_csv(out)
  expect_false("metadata" %in% names(csv)); expect_true(is.na(csv$sent_tokens[2]))
})

test_that("import refuses invalid conversions with original source row positions", {
  d <- .import_fields(); d$text[4] <- "Reply."
  for (name in c("message_id", "round", "sent_tokens", "rec_tokens", "total_tokens")) {
    for (bad in c("1.5", "-1", "Inf", "2147483648", "abc")) {
      x <- d; x[[name]] <- as.character(x[[name]]); x[[name]][4] <- bad
      expect_error(suppressWarnings(focus_group_from_transcript(x)), paste0("Column '", name, "'.*source row.*4"))
    }
  }
  for (ids in list(c(10, 15, 20, 20), c(10, 15, 20, 5), c(10, 15, 20, NA))) {
    x <- d; x$message_id <- ids
    expect_error(suppressWarnings(focus_group_from_transcript(x)), "message_id.*source row.*4")
  }
  for (bad in c("-0.5", "Inf", "text")) {
    x <- d; x$duration_s[4] <- bad
    expect_error(suppressWarnings(focus_group_from_transcript(x)), "duration_s.*source row.*4")
  }
  x <- d; x$metadata[[4]] <- "invalid"
  expect_error(suppressWarnings(focus_group_from_transcript(x)), "metadata.*source row.*4")
  x <- d; x$is_moderator[4] <- "false"
  expect_error(suppressWarnings(focus_group_from_transcript(x)), "is_moderator.*source row.*4")
})

test_that("moderator roles follow explicit flags and avoid placeholder collisions", {
  d <- data.frame(speaker = c("MOD", "Ana", "MOD_2"), text = "Reply.", is_moderator = FALSE)
  expect_message(g <- focus_group_from_transcript(d), "marks no speaker.*\"MOD_3\" placeholder")
  expect_identical(g$moderator_id, "MOD_3"); expect_false(g$agents$MOD$is_moderator)
  expect_true(all(vapply(g$conversation_log, function(x) x$round == 0L, TRUE)))
  expect_error(focus_group_from_transcript(d, moderator_id = "MOD"), "conflicts")
  d$is_moderator[2] <- TRUE
  g <- focus_group_from_transcript(d)
  expect_identical(g$moderator_id, "Ana")
  expect_false(g$agents$MOD$is_moderator)
  expect_no_error(focus_group_from_transcript(d, moderator_id = "Ana"))
  d$is_moderator[1] <- TRUE
  expect_error(focus_group_from_transcript(d), "at most one")
  d <- data.frame(speaker = c("Ana", "Ana"), text = "Reply.", is_moderator = c(TRUE, FALSE))
  expect_error(focus_group_from_transcript(d), "conflicting roles.*Ana")
  d$speaker[1] <- "System"
  expect_error(focus_group_from_transcript(d), "System.*cannot")
  expect_error(focus_group_from_transcript(d, moderator_id = "System"), "cannot be 'System'")
  d$is_moderator[1] <- NA
  expect_error(focus_group_from_transcript(d), "is_moderator.*source row.*1")
})

test_that("timestamps accept defined ISO formats and preserve instants in UTC", {
  strings <- c("2026-01-01", "2026-01-01 12:00:00", "2026-01-01T12:00:00.25Z",
    "2026-01-01T14:00:00+02:00", "2026-01-01T07:00:00-0500")
  d <- data.frame(speaker = "MOD", text = "Reply.", timestamp = strings)
  x <- .fg_transcript_data(focus_group_from_transcript(d)$conversation_log)$timestamp
  expect_equal(as.numeric(x) - as.numeric(x[1]), c(0, 43200, 43200.25, 43200, 43200))
  d$timestamp <- c("invalid", "2026-02-31", "2026-01-01 rubbish", NA, "2026-01-01T12:00:00+99:00")
  expect_warning(x <- focus_group_from_transcript(d), "4 nonmissing timestamp")
  expect_true(all(is.na(.fg_transcript_data(x$conversation_log)$timestamp)))
  for (stamp in list(as.POSIXct("2026-01-01 07:00:00", tz = "America/New_York"),
                    as.Date("2026-01-01"), 1767268800)) {
    d <- data.frame(speaker = "MOD", text = "Reply.", timestamp = stamp)
    out <- focus_group_from_transcript(d)$conversation_log[[1]]$timestamp
    expected <- if (inherits(stamp, "Date")) as.numeric(stamp) * 86400 else as.numeric(stamp)
    expect_equal(as.numeric(out), expected); expect_identical(attr(out, "tzone"), "UTC")
  }
})

test_that("missing imported tokens remain unknown without inventing model calls", {
  g <- focus_group_from_transcript(data.frame(speaker = "MOD", text = "Reply."))
  expect_true(is.na(g$conversation_log[[1]]$sent_tokens))
  expect_equal(g$calls_usage_unknown, 0); expect_equal(g$total_tokens, 0)
  csv <- .fg_transcript_csv(.fg_transcript_data(g$conversation_log))
  path <- tempfile(fileext = ".csv"); on.exit(unlink(path))
  utils::write.csv(csv, path, na = "", row.names = FALSE)
  expect_true(is.na(utils::read.csv(path)$sent_tokens))
})

test_that("explicit missing numeric fields accept untyped NA without converting booleans", {
  d <- data.frame(speaker = "MOD", text = "Reply.", round = NA, sent_tokens = NA,
                  rec_tokens = NA, total_tokens = NA, duration_s = NA)
  expect_no_warning(g <- focus_group_from_transcript(d))
  expect_true(is.na(g$conversation_log[[1]]$round))
  expect_true(is.na(g$conversation_log[[1]]$sent_tokens))
  d$sent_tokens <- FALSE
  expect_error(focus_group_from_transcript(d), "sent_tokens.*source row.*1")
})
