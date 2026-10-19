# An uploaded transcript CSV keeps its identifiers and text as written. Type
# inference reads the speakers 001 and 01 as the same number, rounds a
# 20-digit id, and turns the text "NA" into a missing value.

.write_upload <- function() {
  path <- tempfile(fileext = ".csv")
  writeLines(c(
    "speaker,text,round,sent_tokens,response_id,provider",
    "001,Which hours matter to you?,1,12,000123,NA",
    "01,Evenings matter after work.,1,7,12345678901234567890,groq",
    "1,NA,1,NA,0007,groq",
    "12345678901234567890,Mornings are crowded.,1,,,groq"
  ), path)
  list(name = "transcript.csv", datapath = path)
}

test_that("default type inference would collapse the identifiers", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  inferred <- LLMR.shiny::read_csv_upload(.write_upload())
  expect_length(unique(inferred$speaker[1:3]), 1L)
  expect_false(identical(format(inferred$speaker[4], scientific = FALSE),
                         "12345678901234567890"))
  expect_true(is.na(inferred$text[3]))
  expect_equal(inferred$response_id[1], 123)
})

test_that("uploaded transcripts keep identifiers and text exactly", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  raw <- .fg_read_transcript_csv(.write_upload())
  expect_true(all(vapply(raw, is.character, logical(1))))
  converted <- .fg_convert_upload_columns(raw, c("speaker", "text"))
  expect_identical(converted$speaker,
                   c("001", "01", "1", "12345678901234567890"))
  expect_identical(converted$text[3], "NA")
  expect_identical(converted$response_id,
                   c("000123", "12345678901234567890", "0007", ""))
  expect_identical(converted$round, rep(1L, 4))
  expect_identical(converted$sent_tokens, c(12L, 7L, NA, NA))
  expect_identical(converted$provider, c(NA, "groq", "groq", "groq"))
  expect_null(attr(converted, "focusgroup_csv_upload"))
  plain <- data.frame(speaker = "001", round = "1")
  expect_identical(.fg_convert_upload_columns(plain, "speaker"), plain)

  fg <- focus_group_from_transcript(converted, moderator_id = "001")
  log <- .fg_transcript_data(fg$conversation_log)
  expect_identical(log$speaker_id, converted$speaker)
  expect_identical(log$text[3], "NA")
  expect_length(setdiff(names(fg$agents), "001"), 3L)
})

test_that("the Analyze tab reads an uploaded CSV without collapsing speakers", {
  skip_if_not_installed("LLMR.shiny", "0.1.3")
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  shared <- fake_shared("demo", FALSE, active_tab = "analyze")
  shiny::testServer(.fg_analyze_server, args = list(shared = shared), {
    session$setInputs(file = .write_upload())
    expect_true(all(vapply(raw(), is.character, logical(1))))
    session$setInputs(speaker_col = "speaker", text_col = "text",
                      include_plots = FALSE, analyze = 1L)
    expect_identical(transcript()$speaker_id,
                     c("001", "01", "1", "12345678901234567890"))
    expect_identical(transcript()$text[3], "NA")
    expect_identical(transcript()$round, rep(1L, 4))
    expect_identical(transcript()$sent_tokens, c(12L, 7L, NA, NA))
    expect_identical(transcript()$response_id[2], "12345678901234567890")
    expect_length(unique(transcript()$speaker_id), 4L)
  })
})
