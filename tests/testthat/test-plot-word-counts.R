# The two plots count words with the same rule as the analysis tables, so
# imported text with surrounding or blank whitespace gives the same counts
# in both. Splitting raw text on whitespace counts a leading space as a word.

.spacing_transcript <- function() {
  data.frame(
    speaker = c("Moderator", "Ana", "Ben", "Ana", "Ben"),
    text = c("Which hours matter?", " hello  world ", "   ", "a,b", "\thi there")
  )
}

test_that("the message length timeline counts words as the tables do", {
  skip_if_not_installed("ggplot2")
  fg <- focus_group_from_transcript(.spacing_transcript())
  p <- fg$plot_message_length_timeline()
  expect_identical(as.integer(p$data$word_count), c(3L, 2L, 0L, 1L, 2L))
  expect_identical(as.integer(p$data$word_count),
                   .fg_word_count(.spacing_transcript()$text))
})

test_that("participation by agent totals words as the tables do", {
  skip_if_not_installed("ggplot2")
  fg <- focus_group_from_transcript(.spacing_transcript())
  plotted <- fg$plot_participation_by_agent()$data
  table <- fg$analyze()$speaker_stats
  words <- function(x, id) x$total_words[x[[1]] == id]
  for (id in c("Ana", "Ben")) {
    expect_equal(words(plotted, id), words(table, id))
  }
  expect_equal(words(plotted, "Ana"), 3)
  expect_equal(words(plotted, "Ben"), 2)
})
