# focus_group_from_transcript(): a FocusGroup built from an imported (human)
# transcript data frame. Everything here is offline; construction and the
# descriptive analyses make no model call.

imported_transcript <- function() {
  data.frame(
    speaker = c("Moderator", "Ana", "Ben", "Ana", "Moderator", "Ben", "Cara"),
    text = c(
      "Welcome, everyone. What do you make of the new library hours?",
      "They help working parents like me; evenings matter most for us.",
      "I see it differently. Mornings are now far too crowded to browse.",
      "Crowded mornings still beat being locked out after work entirely.",
      "Ben, say more about what changed in the mornings.",
      "Staff are stretched thin before noon, so lines are much longer.",
      "For me the weekend hours are the real improvement this year."),
    stringsAsFactors = FALSE)
}

test_that("a transcript data frame becomes an analyzable FocusGroup", {
  fg <- focus_group_from_transcript(imported_transcript(), topic = "library hours")
  expect_s3_class(fg, "FocusGroup")
  expect_identical(fg$topic, "library hours")
  expect_length(fg$conversation_log, 7L)
  expect_identical(fg$conversation_log[[2]]$speaker_id, "Ana")
  expect_identical(fg$conversation_log[[2]]$message_id, 2L)
  expect_identical(fg$conversation_log[[2]]$round, 1L)
  expect_false("turn" %in% names(fg$conversation_log[[2]]))

  # the moderator is detected from the speaker id and no synthetic MOD is added
  expect_identical(fg$moderator_id, "Moderator")
  expect_setequal(names(fg$agents), c("Moderator", "Ana", "Ben", "Cara"))
  expect_true(fg$agents[["Moderator"]]$is_moderator)
  expect_false(fg$agents[["Ana"]]$is_moderator)

  # participation balance runs and reports every actual speaker
  pb <- fg$analyze_participation_balance()
  expect_setequal(pb$participation_stats$speaker_id,
                  c("Moderator", "Ana", "Ben", "Cara"))
  expect_identical(sum(pb$participation_stats$messages), 7L)

  # basic analysis agrees on utterance counts
  stats <- fg$analyze()$speaker_stats
  expect_identical(
    stats$utterance_count[as.character(stats$speaker_id) == "Ana"], 2L)
})

test_that("at least one text analysis runs on an imported transcript", {
  skip_if_not_installed("tidytext")
  fg <- focus_group_from_transcript(imported_transcript(), topic = "library hours")
  expect_no_warning(tfidf <- fg$analyze_tfidf(top_n_terms = 3))
  expect_s3_class(tfidf, "tbl_df")
  expect_true(nrow(tfidf) > 0)
  expect_true(all(tfidf$speaker_id %in% c("Moderator", "Ana", "Ben", "Cara")))
})

test_that("System rows stay out of analyses but remain reachable on request", {
  df <- rbind(
    data.frame(speaker = "System", text = "Participants: Ana, Ben; Moderator: Moderator",
               stringsAsFactors = FALSE),
    imported_transcript())
  fg <- focus_group_from_transcript(df, topic = "library hours")

  # no agent is created for System, and default analyses exclude it
  expect_false("System" %in% names(fg$agents))
  stats <- fg$analyze()$speaker_stats
  expect_false("System" %in% as.character(stats$speaker_id))
  pb <- fg$analyze_participation_balance()
  expect_false("System" %in% pb$participation_stats$speaker_id)
  expect_false(identical(pb$balance_metrics$least_active_speaker, "System"))

  # the row is still in the log, and an explicit request reaches it
  expect_identical(fg$conversation_log[[1]]$speaker_id, "System")
  sys_stats <- fg$analyze(speaker_ids = "System")$speaker_stats
  expect_true("System" %in% as.character(sys_stats$speaker_id))
})

test_that("custom column names are honored", {
  df <- data.frame(
    who = c("MOD", "P1", "P2", "P1"),
    said = c("Welcome. First impressions?", "Positive overall.",
             "Mixed, honestly.", "The cost worries me a little."),
    stringsAsFactors = FALSE)
  fg <- focus_group_from_transcript(df, speaker_col = "who", text_col = "said")
  expect_identical(fg$moderator_id, "MOD")
  expect_identical(fg$topic, "Imported transcript")
  expect_length(fg$conversation_log, 4L)
  expect_identical(fg$conversation_log[[3]]$text, "Mixed, honestly.")
  expect_error(
    focus_group_from_transcript(df, speaker_col = "nope", text_col = "said"),
    "must both be present")
  expect_error(focus_group_from_transcript(df, speaker_col = character()),
               "must name one column")
  expect_error(focus_group_from_transcript(df, speaker_col = "who", text_col = "said",
                                           topic = ""),
               "non-empty character string")
})

test_that("missing text becomes an empty imported utterance", {
  df <- data.frame(speaker = c("Moderator", "Ana"),
                   text = c("Welcome.", NA_character_),
                   stringsAsFactors = FALSE)
  fg <- focus_group_from_transcript(df)
  expect_identical(fg$conversation_log[[2]]$text, "")
  expect_identical(fg$conversation_log[[2]]$total_tokens, 0L)
  expect_true(is.na(fg$conversation_log[[2]]$provider))
})

test_that("moderator_id overrides the substring fallback", {
  df <- data.frame(
    speaker = c("Facilitator", "Modest", "P2"),
    text = c("Welcome.", "My surname is Modest.", "My response."),
    stringsAsFactors = FALSE
  )
  fg <- focus_group_from_transcript(df, moderator_id = "Facilitator")
  expect_identical(fg$moderator_id, "Facilitator")
  expect_true(fg$agents[["Facilitator"]]$is_moderator)
  expect_false(fg$agents[["Modest"]]$is_moderator)

  expect_error(
    focus_group_from_transcript(df, moderator_id = "Absent"),
    "moderator_id"
  )
})

test_that("a transcript without a recognizable moderator gets a silent MOD", {
  df <- data.frame(
    speaker = c("Ana", "Ben", "Ana", "Ben"),
    text = c("Shall we start with costs?", "Fine by me; fares rose twice.",
             "And service was cut back in spring.", "Exactly, twice the wait."),
    stringsAsFactors = FALSE)
  fg <- focus_group_from_transcript(df)
  expect_identical(fg$moderator_id, "MOD")
  expect_true(fg$agents[["MOD"]]$is_moderator)
  # the synthetic moderator never spoke: zero utterances, standard treatment
  stats <- fg$analyze()$speaker_stats
  expect_identical(stats$utterance_count[as.character(stats$speaker_id) == "MOD"], 0L)
})

test_that("rows with a missing or blank speaker are dropped with a warning", {
  df <- data.frame(
    speaker = c("Moderator", NA, "Ana", ""),
    text = c("Welcome.", "stray note", "Thanks for having me.", "another note"),
    stringsAsFactors = FALSE)
  expect_warning(fg <- focus_group_from_transcript(df), "missing or blank speaker")
  expect_length(fg$conversation_log, 2L)
  expect_setequal(names(fg$agents), c("Moderator", "Ana"))
  expect_error(
    suppressWarnings(focus_group_from_transcript(
      data.frame(speaker = c(NA, ""), text = c("a", "b"), stringsAsFactors = FALSE))),
    "no attributable turns")
})
