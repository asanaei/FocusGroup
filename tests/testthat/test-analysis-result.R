.analysis_fixture <- function(theme_error = NULL, summary_error = NULL,
                              tfidf_error = FALSE, empty_tables = FALSE) {
  calls <- new.env(parent = emptyenv())
  calls$basic <- 0L
  calls$topics <- 0L
  calls$tfidf <- 0L
  calls$readability <- 0L
  calls$themes <- 0L
  calls$summary <- 0L
  calls$theme_config <- NULL
  calls$summary_config <- NULL

  fg <- list(
    conversation_log = list(
      list(message_id = 1L, round = 1L, speaker_id = "P1",
           is_moderator = FALSE, phase = "imported", text = "Library evenings matter.")
    ),
    analyze = function(...) {
      calls$basic <- calls$basic + 1L
      list(
        speaker_stats = dplyr::tibble(
          speaker_id = "P1", utterance_count = 1L, total_words = 3L,
          avg_words_per_utterance = 3
        ),
        full_transcript = "P1: Library evenings matter."
      )
    },
    analyze_topics = function(...) {
      calls$topics <- calls$topics + 1L
      stop("topicmodels is unavailable")
    },
    analyze_tfidf = function(...) {
      calls$tfidf <- calls$tfidf + 1L
      if (tfidf_error) stop("tfidf package is unavailable")
      if (empty_tables) return(dplyr::tibble())
      dplyr::tibble(speaker_id = "P1", term = "library", tf_idf = 1)
    },
    analyze_readability = function(...) {
      calls$readability <- calls$readability + 1L
      if (empty_tables) return(dplyr::tibble())
      stop("readability package is unavailable")
    },
    analyze_themes = function(config, ...) {
      calls$themes <- calls$themes + 1L
      calls$theme_config <- config
      if (!is.null(theme_error)) stop(theme_error)
      "Theme: evening access"
    },
    summarize = function(config, ...) {
      calls$summary <- calls$summary + 1L
      calls$summary_config <- config
      if (!is.null(summary_error)) stop(summary_error)
      "Participants emphasized evening access."
    },
    plot_participation_timeline = function() stop("plot should not run"),
    plot_word_count_distribution = function() stop("plot should not run"),
    plot_participation_by_agent = function() stop("plot should not run"),
    plot_message_length_timeline = function() stop("plot should not run")
  )
  class(fg) <- "FocusGroup"
  list(focus_group = fg, calls = calls)
}

test_that("analysis runs descriptive work offline and has fixed typed fields", {
  fixture <- .analysis_fixture()
  result <- analyze_focus_group(fixture$focus_group, include_plots = FALSE)

  expect_s3_class(result, "focus_group_analysis")
  expect_named(
    result,
    c("basic_stats", "topics", "tfidf", "readability", "themes",
      "model_summary", "plots", "issues"),
    ignore.order = FALSE
  )
  expect_identical(fixture$calls$basic, 1L)
  expect_identical(fixture$calls$topics, 1L)
  expect_identical(fixture$calls$tfidf, 1L)
  expect_identical(fixture$calls$readability, 1L)
  expect_identical(fixture$calls$themes, 0L)
  expect_identical(fixture$calls$summary, 0L)

  expect_type(result$topics, "list")
  expect_length(result$topics, 0L)
  expect_s3_class(result$tfidf, "data.frame")
  expect_named(result$tfidf, c("speaker_id", "term", "tf_idf"))
  expect_s3_class(result$readability, "data.frame")
  expect_identical(nrow(result$readability), 0L)
  expect_true("speaker_id" %in% names(result$readability))
  expect_type(result$readability$speaker_id, "character")
  expect_true(is.character(result$themes) && length(result$themes) == 0L)
  expect_true(is.character(result$model_summary) &&
                length(result$model_summary) == 0L)
  expect_type(result$plots, "list")
  expect_length(result$plots, 0L)
  expect_s3_class(result$issues, "data.frame")
  expect_named(result$issues, c("component", "reason"), ignore.order = FALSE)
  expect_true(all(c("topics", "readability") %in% result$issues$component))
  expect_true(all(nzchar(result$issues$reason)))
})

test_that("a failed table analysis returns a typed empty table and an issue", {
  result <- analyze_focus_group(
    .analysis_fixture(tfidf_error = TRUE)$focus_group,
    include_plots = FALSE
  )
  expect_s3_class(result$tfidf, "data.frame")
  expect_identical(nrow(result$tfidf), 0L)
  expect_named(result$tfidf, c("speaker_id", "term", "tf_idf"),
               ignore.order = FALSE)
  expect_type(result$tfidf$speaker_id, "character")
  expect_type(result$tfidf$term, "character")
  expect_type(result$tfidf$tf_idf, "double")
  expect_true("tfidf" %in% result$issues$component)
})

test_that("method-returned empty tables are normalized to typed prototypes", {
  result <- analyze_focus_group(
    .analysis_fixture(empty_tables = TRUE)$focus_group,
    include_plots = FALSE
  )
  expect_named(result$tfidf, c("speaker_id", "term", "tf_idf"),
               ignore.order = FALSE)
  expect_named(result$readability, "speaker_id", ignore.order = FALSE)
  expect_type(result$tfidf$tf_idf, "double")
  expect_type(result$readability$speaker_id, "character")
})

test_that("an explicit config opts into themes and a model summary", {
  fixture <- .analysis_fixture()
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  result <- analyze_focus_group(
    fixture$focus_group,
    config = config,
    include_plots = FALSE
  )

  expect_identical(fixture$calls$themes, 1L)
  expect_identical(fixture$calls$summary, 1L)
  expect_identical(fixture$calls$theme_config, config)
  expect_identical(fixture$calls$summary_config, config)
  expect_identical(result$themes, "Theme: evening access")
  expect_identical(result$model_summary,
                   "Participants emphasized evening access.")
})

test_that("model analyses accept the experiments-frame runner seam", {
  transcript <- data.frame(
    speaker = c("Moderator", "P1", "P2"),
    text = c("Which hours matter?", "Evenings matter for work schedules.",
             "Weekend hours matter for family visits."),
    stringsAsFactors = FALSE
  )
  fg <- focus_group_from_transcript(transcript, moderator_id = "Moderator")
  seen <- list()
  scripted_runner <- function(experiments, ...) {
    seen[[length(seen) + 1L]] <<- experiments
    prompts <- vapply(experiments$messages, function(messages) {
      paste(vapply(messages, function(message) {
        as.character(message$content %||% "")
      }, character(1)), collapse = "\n")
    }, character(1))
    experiments$response_text <- ifelse(
      grepl("thematic analysis", prompts, ignore.case = TRUE),
      "Theme: access outside working hours.",
      "Participants emphasized evening and weekend access."
    )
    experiments$success <- TRUE
    experiments
  }
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  result <- analyze_focus_group(
    fg,
    config = config,
    include_plots = FALSE,
    .runner = scripted_runner
  )

  expect_length(seen, 2L)
  expect_true(all(vapply(seen, function(experiments) {
    is.data.frame(experiments) &&
      is.list(experiments$config) &&
      is.list(experiments$messages)
  }, logical(1))))
  expect_true(nzchar(result$themes))
  expect_true(nzchar(result$model_summary))
})

test_that("empty requested model output raises an error", {
  transcript <- data.frame(
    speaker = c("Moderator", "P1"),
    text = c("What matters?", "Evening access matters."),
    stringsAsFactors = FALSE
  )
  fg <- focus_group_from_transcript(transcript, moderator_id = "Moderator")
  empty_runner <- function(experiments, ...) {
    experiments$response_text <- ""
    experiments$success <- TRUE
    experiments
  }
  config <- LLMR::llm_config("openai", "gpt-4o-mini")

  expect_error(
    fg$analyze_themes(config = config, .runner = empty_runner),
    "empty model response"
  )
  expect_error(
    fg$summarize(config = config, .runner = empty_runner),
    "empty model response"
  )
})

test_that("thematic provider conditions propagate", {
  provider_condition <- structure(
    list(message = "thematic provider failed", call = NULL),
    class = c("scripted_theme_provider_error", "error", "condition")
  )
  fixture <- .analysis_fixture(theme_error = provider_condition)
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  expect_error(
    analyze_focus_group(fixture$focus_group, config = config,
                        include_plots = FALSE),
    class = "scripted_theme_provider_error"
  )
})

test_that("model-summary provider conditions propagate", {
  provider_condition <- structure(
    list(message = "summary provider failed", call = NULL),
    class = c("scripted_summary_provider_error", "error", "condition")
  )
  fixture <- .analysis_fixture(summary_error = provider_condition)
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  expect_error(
    analyze_focus_group(fixture$focus_group, config = config,
                        include_plots = FALSE),
    class = "scripted_summary_provider_error"
  )
})

test_that("analysis print is concise and reports issues", {
  result <- analyze_focus_group(.analysis_fixture()$focus_group,
                                include_plots = FALSE)
  printed <- capture.output(print(result))
  expect_lte(length(printed), 12L)
  expect_match(paste(printed, collapse = "\n"), "focus_group_analysis")
  expect_match(paste(printed, collapse = "\n"), "Issues")
})
