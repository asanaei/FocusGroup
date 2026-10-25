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

.sf_result <- function(flow = "desire_based", runner = .sf_runner, ...) {
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
    seed = 110, verbose = FALSE, .runner = runner, ...)
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

# ---- Reading ----------------------------------------------------------------

.sf_reread <- function(x) FocusGroup:::.fg_object_result(focus_group_from_transcript(x))

.sf_expect_same <- function(a, b, label = "") {
  for (component in setdiff(names(a), "focus_group")) {
    x <- a[[component]]
    y <- b[[component]]
    if (identical(component, "calls")) {
      x$messages <- NULL
      y$messages <- NULL
    }
    expect_equal(y, x, label = paste(label, component))
  }
}

test_that("a written session reads back whole, and writes the same file again", {
  for (flow in c("round_robin", "probabilistic", "desire_based")) {
    result <- .sf_result(flow = flow)
    lines <- format(result)
    path <- tempfile(fileext = ".fg.jsonl")
    writeLines(lines, path)
    from_file <- .sf_reread(path)
    expect_identical(format(from_file), lines, label = flow)
    .sf_expect_same(result, from_file, flow)
    # The result itself and its lines read the same. Its session object
    # does not know the estimate, the seed, or the failure rule given to
    # run_focus_group(), and is otherwise the same.
    expect_identical(format(.sf_reread(result)), lines, label = flow)
    expect_identical(format(.sf_reread(lines)), lines, label = flow)
    from_object <- format(.sf_reread(result$focus_group))
    expect_identical(from_object[-1], lines[-1], label = flow)
    head <- jsonlite::parse_json(from_object[[1]])
    expected <- jsonlite::parse_json(lines[[1]])
    unknown <- c("estimated_calls", "seed", if (flow != "desire_based") "on_desire_failure")
    expect_identical(head[setdiff(names(head), unknown)],
                     expected[setdiff(names(expected), unknown)], label = flow)
  }
  # The object read is new, with no configuration or runner.
  fg <- focus_group_from_transcript(result)
  expect_false(identical(fg, result$focus_group))
  expect_true(all(vapply(fg$agents, function(a) is.null(a$config) && is.null(a$.runner),
                         logical(1))))
  expect_identical(fg$agents$P1$persona_description,
                   result$focus_group$agents$P1$persona_description)
  expect_identical(fg$handles, result$focus_group$handles)
  expect_identical(fg$question_script, result$focus_group$question_script)
  expect_identical(fg$call_on_silent, result$focus_group$call_on_silent)
  expect_identical(class(fg$turn_taking_flow), class(result$focus_group$turn_taking_flow))
})

test_that("a partial session, an arm, and a session with permuted handles read back whole", {
  calls <- 0L
  failing <- function(experiments, ...) {
    calls <<- calls + 1L
    if (calls == 9L) stop("The provider is unavailable.")
    .sf_runner(experiments, ...)
  }
  partial <- tryCatch(.sf_result(flow = "round_robin", runner = failing),
                      focusgroup_incomplete_session = function(e) e$partial)
  expect_false(partial$metadata$complete)
  lines <- format(partial)
  again <- .sf_reread(lines)
  expect_identical(format(again), lines)
  expect_identical(again$metadata$failure, partial$metadata$failure)

  arms <- run_focus_group_arms(
    topic = "library hours", config = LLMR::llm_config("openai", "example-model"),
    n_participants = 3, arms = list(note = fg_injection("A note.", fg_after_item(2),
                                                        from = "Guest"), control = NULL),
    guide = list(Engagement = c("Which hours?", "Who is left out?"), Closing = "Close."),
    flow = "desire_based", seed = 110, verbose = FALSE, .runner = .sf_runner)
  arm <- arms$arms$note
  lines <- format(arm)
  again <- .sf_reread(lines)
  expect_identical(format(again), lines)
  expect_identical(again$metadata$branch, arm$metadata$branch)

  permuted <- run_focus_group(
    topic = "library hours", config = LLMR::llm_config("openai", "example-model"),
    n_participants = 3, guide = list(Engagement = "Which hours?", Closing = "Close."),
    flow = "round_robin", handles = c(P1 = "P3", P3 = "P1"), verbose = FALSE,
    .runner = .sf_runner)
  again <- .sf_reread(permuted)
  expect_identical(again$participants$handle, permuted$participants$handle)
  expect_identical(format(again), format(permuted))
})

.sf_problems <- function(lines) {
  e <- tryCatch(focus_group_from_transcript(lines), focusgroup_format_error = identity)
  expect_s3_class(e, "focusgroup_format_error")
  e$problems
}

test_that("every problem of a malformed file is reported at once, by line and field", {
  ok <- c('{"type":"participant","id":"Chair","is_moderator":true}',
          '{"type":"participant","id":"Ana"}',
          '{"type":"message","speaker_id":"Chair","text":"Which hours matter?"}',
          '{"type":"message","speaker_id":"Ana","text":"Evenings."}')
  expect_s3_class(focus_group_from_transcript(ok), "FocusGroup")

  p <- .sf_problems(c(ok[1:2], '{"type":"message","speaker_id":"Ana","text":"Evenings."',
                      '{"type":"mesage","speaker_id":"Ana","text":"x"}',
                      '{"type":"message","speaker_id":"Ana"}',
                      '{"type":"message","speaker_id":"Ben","text":"x","speeker":"Ben"}'))
  expect_identical(p$row, c(3L, 4L, 6L, 5L, 6L))
  expect_identical(p$field, c("", "type", "speeker", "text", "speaker_id"))
  expect_match(p$problem[[1]], "not valid JSON")
  expect_match(p$problem[[2]], "'mesage' is not a line type")
  expect_match(p$problem[[3]], "not a field of a message line")
  expect_match(p$problem[[4]], "required")
  expect_match(p$problem[[5]], "'Ben' is not declared")

  ids <- c(ok[1:2], '{"type":"message","message_id":5,"speaker_id":"Chair","text":"Q?"}',
           '{"type":"message","message_id":3,"speaker_id":"Ana","text":"A."}',
           '{"type":"message","speaker_id":"Ana","text":"B."}')
  p <- .sf_problems(ids)
  expect_identical(p$row, c(5L, 4L))
  expect_match(p$problem[[1]], "missing, while other messages give one")
  expect_match(p$problem[[2]], "3 does not follow 5")

  two <- c(ok[1], '{"type":"participant","id":"Ana","is_moderator":true}', ok[3:4])
  p <- .sf_problems(two)
  expect_identical(p$row, 2L)
  expect_match(p$problem, "second moderator")

  types <- c(ok[1:2], '{"type":"message","speaker_id":"Ana","text":5,"round":-1,"metadata":[1]}')
  p <- .sf_problems(types)
  expect_identical(p$field, c("text", "metadata", "text", "round"))

  # A pretty-printed object is one JSON value over several lines.
  pretty <- strsplit(as.character(jsonlite::prettify(ok[[3]])), "\n")[[1]]
  p <- .sf_problems(pretty)
  expect_identical(nrow(p), 1L)
  expect_match(p$problem, "one JSON value written over")
  expect_error(focus_group_from_transcript(character()), "data")
  p <- .sf_problems(c("", "  "))
  expect_match(p$problem, "no line")
})

test_that("a byte-order mark, Windows line endings, and blank lines are accepted", {
  ok <- c('{"type":"participant","id":"Chair","is_moderator":true}',
          '{"type":"participant","id":"Ana"}',
          '{"type":"message","speaker_id":"Chair","text":"Which hours matter?"}',
          '{"type":"message","speaker_id":"Ana","text":"Evenings, café."}')
  path <- tempfile(fileext = ".jsonl")
  con <- file(path, "wb")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)),
             charToRaw(enc2utf8(paste0(paste(c(ok[1:2], "", ok[3:4]), collapse = "\r\n"),
                                       "\r\n")))), con)
  close(con)
  fg <- focus_group_from_transcript(path)
  expect_identical(fg$moderator_id, "Chair")
  expect_identical(vapply(fg$conversation_log, `[[`, character(1), "text"),
                   c("Which hours matter?", "Evenings, café."))
  expect_identical(focus_group_from_transcript(paste(ok, collapse = "\r\n"))$conversation_log,
                   fg$conversation_log)
  # Only a .jsonl path is read; other files are read into R first.
  csv <- tempfile(fileext = ".csv")
  writeLines("speaker,text", csv)
  expect_error(focus_group_from_transcript(csv), "utils::read.csv")
  expect_error(focus_group_from_transcript(tempfile(fileext = ".jsonl")), "no session file")
  expect_error(focus_group_from_transcript(ok, speaker_col = "who"), "data frame")
})

test_that("a transcript file is read with the defaults for a human group", {
  lines <- c(
    '{"type":"session","topic":"library hours"}',
    '{"type":"message","speaker_id":"Moderator","text":"Welcome. Which hours matter?"}',
    '{"type":"message","speaker_id":"Ana","text":"Evenings."}',
    '{"type":"message","speaker_id":"Ben","text":"Mornings."}',
    '{"type":"message","speaker_id":"Ana","text":"Weekends too."}',
    '{"type":"message","speaker_id":"Moderator","text":"Now, a different topic."}',
    '{"type":"message","speaker_id":"Moderator","text":"Ben, who is left out?"}',
    '{"type":"message","speaker_id":"Ben","text":"Shift workers."}',
    '{"type":"message","speaker_id":"Guest","text":"A fact.","metadata":{"injected":true}}',
    '{"type":"message","speaker_id":"Moderator","text":"Thank you all."}')
  fg <- focus_group_from_transcript(lines,
                                    personas = c(Ana = "A nurse.", Ben = "A baker."))
  expect_identical(fg$moderator_id, "Moderator")
  expect_identical(names(fg$agents), c("Moderator", "Ana", "Ben"))
  expect_false("Guest" %in% names(fg$agents))
  expect_identical(vapply(fg$question_script, `[[`, character(1), "phase"),
                   c("generic_discussion", "transition", "generic_discussion", "closing"))
  expect_true(all(vapply(fg$question_script, function(i) isTRUE(i$verbatim), logical(1))))
  expect_identical(fg$question_script[[2]]$text, "Now, a different topic.")
  expect_identical(vapply(fg$conversation_log, `[[`, integer(1), "item"),
                   c(1L, 1L, 1L, 1L, 2L, 3L, 3L, 3L, 4L))
  expect_identical(fg$max_participant_responses, 3L)
  expect_identical(fg$call_on_silent$items, integer())
  expect_identical(unname(fg$handles), c("Moderator", "Ana", "Ben"))
  expect_identical(fg$agents$Ana$persona_description, "A nurse.")
  expect_identical(fg$agents$Moderator$persona_description,
                   FocusGroup:::.fg_placeholder_persona("Moderator"))
  expect_s3_class(fg$turn_taking_flow, "RoundRobinFlow")
  expect_identical(fg$topic, "library hours")
  expect_error(focus_group_from_transcript(lines, personas = c(Cleo = "x")), "Cleo")
  expect_error(focus_group_from_transcript(lines, personas = c(Ana = "")), "personas")
  # The analyses run on it, and so do the turn table and the player's input.
  expect_identical(nrow(fg_turns(fg)), 9L)
  # Written and read again, the transcript keeps its defaults.
  again <- FocusGroup:::.fg_object_result(fg)
  expect_identical(format(.sf_reread(format(again))), format(again))
})

test_that("the examples of the format description read as described", {
  spec <- readLines(system.file("fg-session-spec.md", package = "FocusGroup"))
  starts <- which(spec == "The session file:")
  expect_length(starts, 2L)
  read <- lapply(starts, function(i) {
    fence <- which(spec == "```")
    open <- fence[fence > i][1]
    close <- fence[fence > open][1]
    focus_group_from_transcript(spec[(open + 1L):(close - 1L)])
  })
  expect_identical(read[[1]]$moderator_id, "Moderator")
  expect_identical(length(read[[1]]$conversation_log), 5L)
  expect_identical(read[[1]]$topic, "library hours")
  expect_identical(read[[1]]$question_script[[2]]$text,
                   "Ben, say more about what changed in the mornings.")
  expect_identical(read[[2]]$moderator_id, "Rosa")
  expect_identical(read[[2]]$agents$P2$persona_description, "retired nurse")
  stamps <- vapply(read[[2]]$conversation_log, function(m) as.numeric(m$timestamp), numeric(1))
  expect_identical(diff(stamps), c(16, 19, 622))
  expect_identical(vapply(read[[2]]$question_script, `[[`, character(1), "phase"),
                   c("generic_discussion", "closing"))
})

test_that("an analysis rebuilds the session object of a result saved without it", {
  result <- .sf_result(flow = "round_robin")
  saved <- result
  saved$focus_group <- NULL
  analysis <- analyze_focus_group(saved, include_plots = FALSE)
  expect_identical(analysis$basic_stats$speaker_stats$speaker_id,
                   analyze_focus_group(result, include_plots = FALSE)$basic_stats$speaker_stats$speaker_id)
  prepared <- FocusGroup:::.fg_prepare_analysis_source(saved)
  expect_s3_class(prepared$focus_group, "FocusGroup")
  expect_identical(prepared$transcript, saved$transcript)
})

test_that("a nested data frame of metadata is refused naming the source rows", {
  d <- data.frame(speaker = c("MOD", "", "Ana", "Ben"), text = c("Q?", "x", "A.", "B."))
  d$metadata <- data.frame(flag = c(TRUE, FALSE, TRUE, FALSE), note = c("a", "b", "c", "d"))
  expect_error(suppressWarnings(focus_group_from_transcript(d)),
               "metadata.*not a list.*source row\\(s\\) 1, 3, 4")
})
