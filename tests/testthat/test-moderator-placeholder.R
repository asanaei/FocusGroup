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
  expect_message(fg <- focus_group_from_transcript(transcript_with("Moderator 1")),
                 "No speaker id is a moderator alias")
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
