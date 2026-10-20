# Details of a session's text and records: the cleaning of a leading role
# preamble, participation balance over the whole roster, the interim summary's
# coverage, and template wording that holds whatever happened earlier.

.detail_agent <- function(reply) {
  FGAgent$new(
    id = "P1",
    agent_details = list(direct_persona_description = "A participant."),
    config = LLMR::llm_config("openai", "example-model"),
    .runner = function(experiments, ...) {
      experiments$response_text <- reply
      experiments
    })
}

.detail_utterance <- function(agent) {
  agent$generate_utterance(
    topic = "library hours", conversation_history_string = "",
    utterance_prompt_template = "State your view.", conversation_log = list())$text
}

test_that("discourse openings survive cleaning and persona introductions do not", {
  kept <- c(
    "As a result, the evening hours matter more than the morning ones.",
    "As an aside, the branch on Elm Street already opens late on Fridays.",
    "As a matter of fact, the library closed early twice last month.",
    "As a rule, I visit on weekends because that is when I am free.",
    "As a whole, the proposed schedule serves working families well.",
    "As a consequence, fewer students can study there after school.",
    "As an example, take the Monday closures that began this spring.",
    "As the saying goes, you do not know what you have until it is gone.",
    "As the moderator said, cost is only one part of the question here.",
    "As a society, we decide what a public library is worth funding."
  )
  for (text in kept) {
    expect_identical(.detail_utterance(.detail_agent(text)), text)
  }

  stripped <- c(
    "As a teacher, I see students who need a quiet place after school.",
    "As a retired nurse who worked nights, I value the late opening hours.",
    "As an immigrant, I relied on the library to learn about my new city.",
    "As the mother of two, I need weekend hours more than weekday ones."
  )
  expected <- c(
    "I see students who need a quiet place after school.",
    "I value the late opening hours.",
    "I relied on the library to learn about my new city.",
    "I need weekend hours more than weekday ones."
  )
  for (i in seq_along(stripped)) {
    expect_identical(.detail_utterance(.detail_agent(stripped[[i]])), expected[[i]])
  }
})

test_that("participation balance lists participants who never spoke", {
  config <- LLMR::llm_config("openai", "example-model")
  agents <- create_agents(3, config = config,
                          direct_persona_descriptions = c("A.", "B.", "C."))
  fg <- FocusGroup$new(
    topic = "library hours", purpose = "Test the balance.", agents = agents,
    moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow("round_robin", agents, "MOD"),
    question_script = list(list(phase = "closing", text = "Close.")))
  fg$conversation_log <- list(
    list(message_id = 1L, round = 1L, speaker_id = "MOD", is_moderator = TRUE,
         text = "Which hours matter most to you?", phase = "engagement_question"),
    list(message_id = 2L, round = 1L, speaker_id = "P1", is_moderator = FALSE,
         text = "Evening hours matter most to me.", phase = "engagement_question"),
    list(message_id = 3L, round = 1L, speaker_id = "P1", is_moderator = FALSE,
         text = "Weekends matter as well.", phase = "engagement_question")
  )

  balance <- fg$analyze_participation_balance()
  stats <- balance$participation_stats
  expect_setequal(stats$speaker_id, c("MOD", "P1", "P2", "P3"))
  expect_identical(stats$messages[stats$speaker_id == "P2"], 0L)
  expect_identical(stats$messages[stats$speaker_id == "P3"], 0L)
  expect_true(is.na(stats$avg_words[stats$speaker_id == "P3"]))
  expect_equal(sum(stats$message_percentage), 100)
  expect_true(balance$balance_metrics$least_active_speaker %in% c("P2", "P3"))
  expect_identical(balance$balance_metrics$most_active_speaker, "P1")
  expect_equal(balance$balance_metrics$message_percentage_range,
               100 * 2 / 3)

  # A speaker filter limits the roster as well as the log.
  filtered <- fg$analyze_participation_balance(speaker_ids = c("P1", "P2"))
  expect_setequal(filtered$participation_stats$speaker_id, c("P1", "P2"))
})

test_that("the interim summary covers the messages the prompt window drops", {
  config <- LLMR::llm_config("openai", "example-model")
  summary_prompts <- character()
  newest_at_summary <- integer()
  turn <- 0L
  runner <- function(experiments, ...) {
    prompt <- paste(vapply(experiments$messages[[1]], function(m) m$content,
                           character(1)), collapse = "\n")
    if (grepl("Summary:$", prompt)) {
      if (grepl("very short, bulleted", prompt, fixed = TRUE)) {
        summary_prompts <<- c(summary_prompts, prompt)
        newest_at_summary <<- c(newest_at_summary, turn)
      }
      experiments$response_text <- "A short summary of the earlier discussion."
    } else {
      turn <<- turn + 1L
      # About 7,500 estimated tokens per message, so the 64,000-token
      # window holds eight messages.
      experiments$response_text <- paste0(
        "Marker", sprintf("%03d", turn), " ",
        paste(rep("The evening hours matter for working families.", 640),
              collapse = " "))
    }
    experiments
  }
  result <- run_focus_group(
    topic = "library hours", config = config, n_participants = 2,
    guide = list(Engagement = c("Q1?", "Q2?", "Q3?", "Q4?")),
    flow = "round_robin", max_participant_responses = 2, verbose = FALSE,
    .runner = runner)

  expect_gte(length(summary_prompts), 1L)
  first <- summary_prompts[[1]]
  # The oldest discussion messages are summarized; the newest, which the
  # window still shows, are not.
  expect_match(first, "Marker001", fixed = TRUE)
  expect_false(grepl(sprintf("Marker%03d", newest_at_summary[[1]]), first,
                     fixed = TRUE))
  expect_s3_class(result, "focus_group_result")
})

test_that("no interim summary is made while the window holds the whole discussion", {
  config <- LLMR::llm_config("openai", "example-model")
  summaries <- 0L
  runner <- function(experiments, ...) {
    prompt <- paste(vapply(experiments$messages[[1]], function(m) m$content,
                           character(1)), collapse = "\n")
    if (grepl("very short, bulleted", prompt, fixed = TRUE)) summaries <<- summaries + 1L
    experiments$response_text <- "Evening hours matter to working families."
    experiments
  }
  # More than 200 short messages: the count trigger fires, but the window
  # drops nothing, so a summary would only repeat what every prompt shows.
  result <- run_focus_group(
    topic = "library hours", config = config, n_participants = 2,
    guide = list(Engagement = sprintf("Question %d?", 1:70)),
    flow = "round_robin", max_participant_responses = 2, verbose = FALSE,
    .runner = runner)
  expect_gt(nrow(result$transcript), 200L)
  expect_identical(summaries, 0L)
})

test_that("default moderator templates do not assert what happened earlier", {
  templates <- get_default_prompt_templates()
  moderator <- templates[grepl("^moderator_", names(templates))]
  claims <- c("has been completed", "are complete", "is complete",
              "just completed", "just said", "as discussed in opening",
              "nearing the end", "most recent messages first",
              "after icebreaker")
  for (name in names(moderator)) {
    for (claim in claims) {
      expect_false(grepl(claim, moderator[[name]], ignore.case = TRUE),
                   info = paste(name, claim))
    }
  }
})
