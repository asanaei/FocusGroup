# Session files: format() writes a result as JSON Lines, and
# focus_group_from_transcript() reads them back. Offline, with stub runners.

.sf_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""), character(1)),
        collapse = "\n")
}

.sf_runner <- function(experiments, ...) {
  prompt <- .sf_prompt(experiments$messages[[1]])
  h <- sum(utf8ToInt(prompt))
  experiments$response_text <- if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
    as.character(h %% 11L)
  } else if (grepl("Summary:$", prompt)) {
    "A short summary: caf\u00e9 hours, \u062f\u0631 the evening."
  } else if (grepl("private question", prompt, fixed = TRUE)) {
    "I would say 5."
  } else if (grepl("In this discussion you are", prompt, fixed = TRUE)) {
    sprintf("Reply %d: evening hours matter \u2014 \"really\" \\ to me.\nA second line.",
            h %% 9973L)
  } else if (h %% 3L == 0L) {
    "P2, what would you add to that?"
  } else {
    sprintf("Moderator line %d: what do you all think?", h %% 9973L)
  }
  experiments
}

.sf_result <- function(flow = "desire_based", ...) {
  run_focus_group(
    topic = "library hours",
    config = LLMR::llm_config("openai", "example-model", temperature = 0.3),
    n_participants = 3,
    demographics = data.frame(age = c("34", "51", "68"),
                              occupation = c("nurse", "teacher", "baker")),
    guide = list(Opening = "Welcome, everyone.",
                 Engagement = c("Which hours matter most?", "Who is left out?"),
                 Closing = "Thank you all."),
    flow = flow, max_participant_responses = 2, moderator_verbatim = c(1, 4),
    probes = fg_probe("From 1 to 7, how strongly do you favor longer hours?",
                      at = list(fg_after_item(2), "end"), scale = c(1, 7)),
    injections = list(fg_injection("A note from the board.", fg_after_item(2),
                                   from = "Board member"),
                      fg_injection("Speaking for myself: evenings.", fg_before_turn(3),
                                   from = "P3")),
    seed = 110, verbose = FALSE, .runner = .sf_runner, ...)
}

test_that("format() writes one JSON object per line, one row each, in plain ASCII", {
  result <- .sf_result()
  lines <- format(result)
  expect_type(lines, "character")
  expect_false(any(grepl("\n", lines, fixed = TRUE)))
  expect_false(any(grepl("[^\\x20-\\x7e]", lines, perl = TRUE)))
  parsed <- lapply(lines, jsonlite::parse_json)
  types <- vapply(parsed, `[[`, character(1), "type")
  expect_identical(types, c(
    "session", rep("participant", 4), rep("item", 4),
    rep("message", nrow(result$transcript)), rep("routing", nrow(result$routing)),
    rep("probe", nrow(result$probes)), rep("injection", nrow(result$injections)),
    rep("call", nrow(result$calls))))
  # Field names are the columns of the tables; a call line has no messages.
  message <- parsed[[which(types == "message")[1]]]
  expect_identical(names(message), c("type", names(result$transcript)))
  call <- parsed[[which(types == "call")[1]]]
  expect_identical(names(call), c("type", setdiff(names(result$calls), "messages")))
  session <- parsed[[1]]
  expect_identical(session$format, "fg-session")
  expect_identical(session$version, 1L)
  expect_false(any(c("guide", "handles") %in% names(session)))
  items <- parsed[types == "item"]
  expect_identical(vapply(items, `[[`, integer(1), "item"), 1:4)
  expect_true(items[[1]]$verbatim)
  # The text survives escaping.
  replies <- result$transcript$text[result$transcript$speaker_id == "P1"]
  written <- vapply(parsed[types == "message"], `[[`, character(1), "text")
  expect_true(all(replies %in% written))
  expect_match(replies[[1]], "\u2014 \"really\" \\ to me.\nA second", fixed = TRUE)
})

test_that("the writing is canonical for numbers, missing values, names, and times", {
  num <- FocusGroup:::.fg_json_number
  expect_identical(num(5), "5.0")
  expect_identical(num(0.1 + 0.2), "0.30000000000000004")
  expect_identical(num(0.3), "0.3")
  expect_identical(num(1e-20), "1e-20")
  expect_identical(num(1761300000.123456), "1761300000.123456")
  val <- FocusGroup:::.fg_json_value
  expect_identical(val(5L), "5")
  expect_identical(val(c(5L, NA)), "[5,null]")
  expect_identical(val(NA_character_), "null")
  expect_identical(val(NULL), "null")
  expect_identical(val(list()), "{}")
  expect_identical(val(c(P1 = 6, P2 = NA)), "{\"P1\":6.0,\"P2\":null}")
  expect_identical(val("P1", array = TRUE), "[\"P1\"]")
  expect_identical(val(character()), "[]")
  expect_identical(val(list(a = 1L, b = list())), "{\"a\":1,\"b\":{}}")
  expect_identical(FocusGroup:::.fg_json_string("a\u00e9\U0001F600\t\"\\"),
                   "\"a\\u00e9\\ud83d\\ude00\\t\\\"\\\\\"")
  time <- FocusGroup:::.fg_json_time
  t0 <- as.POSIXct("2026-10-24 14:30:00", tz = "UTC")
  expect_identical(time(t0), "\"2026-10-24T14:30:00Z\"")
  expect_identical(time(t0 + 0.25), "\"2026-10-24T14:30:00.25Z\"")
  expect_identical(time(t0 + 0.9999999), "\"2026-10-24T14:30:01Z\"")
  expect_identical(time(as.POSIXct(NA)), "null")
  expect_error(val(Inf, field = "x"), "infinite")
  expect_error(val(function() 1, field = "x"), "cannot carry")
})

test_that("a session file never holds a configuration, a key, or an endpoint", {
  config <- suppressWarnings(LLMR::llm_config(
    "openai", "example-model", api_key = "sk-FIXTURE-SECRET-123",
    api_url = "https://proxy.example.org/v1?key=URL-SECRET-456", temperature = 0.3))
  result <- run_focus_group(
    topic = "library hours", config = config, n_participants = 2,
    guide = list(Engagement = "Which hours matter?", Closing = "Close."),
    flow = "round_robin", max_participant_responses = 1, verbose = FALSE,
    .runner = .sf_runner)
  text <- paste(format(result), collapse = "\n")
  expect_false(grepl("SECRET", text, fixed = TRUE))
  expect_false(grepl("proxy.example.org", text, fixed = TRUE))
  expect_true(grepl("example-model", text, fixed = TRUE))
  planted <- result
  planted$metadata$config <- config
  expect_error(format(planted), "model configuration")
  planted$metadata$config <- NULL
  planted$metadata$runner <- .sf_runner
  expect_error(format(planted), "cannot carry")
})

test_that("a result saved by 0.5.5 takes the guide, flow parameters, and styles from its session object", {
  result <- .sf_result()
  old <- result
  old$metadata$guide <- NULL
  old$metadata$flow_params <- NULL
  old$participants$communication_style <- NULL
  expect_identical(format(old), format(result))
  # Without the session object they are left out or missing.
  old$focus_group <- NULL
  lines <- format(old)
  types <- vapply(lapply(lines, jsonlite::parse_json), `[[`, character(1), "type")
  expect_false("item" %in% types)
  participant <- jsonlite::parse_json(lines[[2]])
  expect_null(participant$communication_style)
})
