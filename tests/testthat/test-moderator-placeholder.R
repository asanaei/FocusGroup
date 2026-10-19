# When no speaker id is a moderator alias, the importer says so instead of
# quietly analyzing the moderator's speech as a participant's.

transcript_with <- function(mod_label) {
  data.frame(
    speaker = c(mod_label, "Ana", "Ben", mod_label, "Ana"),
    text = c("Welcome. What do you think of the new hours?",
             "They help working parents.", "Mornings are crowded now.",
             "Ben, say more.", "Evenings matter most to me.")
  )
}

test_that("an unrecognized moderator label draws a message and a placeholder", {
  msg <- testthat::capture_messages(
    fg <- focus_group_from_transcript(transcript_with("Moderator 1")))
  expect_length(msg, 1L)
  expect_match(msg, "No speaker id is a moderator alias", fixed = TRUE)
  expect_match(msg, "all 3 speakers (Moderator 1, Ana, Ben)", fixed = TRUE)
  expect_match(msg, "\"MOD\" placeholder", fixed = TRUE)
  expect_match(msg, "Set `moderator_id`", fixed = TRUE)
  expect_identical(fg$moderator_id, "MOD")
  expect_false(fg$agents[["Moderator 1"]]$is_moderator)
})

test_that("the placeholder message truncates a long speaker list visibly", {
  many <- data.frame(speaker = paste("Speaker", 1:8), text = "A turn.")
  expect_message(focus_group_from_transcript(many),
                 "all 8 speakers \\(Speaker 1, .*Speaker 6, \\.\\.\\.\\)")
})

test_that("an all-false is_moderator column is named as the reason", {
  d <- transcript_with("Moderator")
  d$is_moderator <- FALSE
  expect_message(fg <- focus_group_from_transcript(d),
                 "`is_moderator` column marks no speaker.*Set `moderator_id`")
  expect_identical(fg$moderator_id, "MOD")
})

test_that("an exact alias is recognized without a message", {
  expect_no_message(fg <- focus_group_from_transcript(transcript_with("Moderator")))
  expect_identical(fg$moderator_id, "Moderator")
})

test_that("moderator_id names the moderator without a message", {
  expect_no_message(fg <- focus_group_from_transcript(transcript_with("Moderator 1"),
                                                      moderator_id = "Moderator 1"))
  expect_identical(fg$moderator_id, "Moderator 1")
})
