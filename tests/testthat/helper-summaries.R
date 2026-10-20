# Helpers for the summary tests: sessions made with stub runners on the
# current engine, the record fields of the contract added where a test needs
# them, and a scripted summarizer that answers each prompt with claims built
# from the messages the prompt shows.

.sum_config <- function() LLMR::llm_config("openai", "example-model")

# A session whose participants give distinct replies, so that quotes can be
# told apart.
.sum_session <- function(n = 3, guide = c(Opening = 1, Engagement = 2, Closing = 1),
                         ...) {
  count <- 0L
  lines <- c("Evening hours matter most to working parents like me.",
             "I disagree, the mornings are when retirees can come in.",
             "Weekend hours would help students who work during the week.",
             "Staff costs worry me more than the schedule itself.",
             "I changed my mind, evenings should come first after all.",
             "Nobody asked the branch staff what they can handle.")
  runner <- function(experiments, ...) {
    prompt <- paste(vapply(experiments$messages[[1]], function(m) as.character(m$content),
                           character(1)), collapse = "\n")
    experiments$response_text <- if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      "6"
    } else if (grepl("private question", prompt, fixed = TRUE)) {
      "Privately I would rate the longer hours a five out of seven overall."
    } else if (grepl("Summary:$", prompt)) {
      "A short summary."
    } else if (grepl("MODERATOR", prompt, fixed = TRUE)) {
      "What should change about the library hours?"
    } else {
      count <<- count + 1L
      lines[(count - 1L) %% length(lines) + 1L]
    }
    experiments
  }
  run_focus_group("library hours", .sum_config(), n_participants = n, guide = guide,
                  flow = "round_robin", max_participant_responses = 2,
                  verbose = FALSE, .runner = runner, ...)
}

# The 0.5.5 record fields of the contract, added to a result made by the
# current engine.
.sum_contract <- function(result, handles = NULL) {
  tr <- result$transcript
  ids <- result$participants$id
  map <- stats::setNames(ids, ids)
  map[result$participants$is_moderator] <- "Moderator"
  if (!is.null(handles)) map[names(handles)] <- handles
  tr$handle <- ifelse(tr$speaker_id %in% names(map), unname(map[tr$speaker_id]), tr$speaker_id)
  tr$handle[tr$speaker_id == "System"] <- "System"
  tr$item <- tr$round
  result$transcript <- tr
  result$participants$handle <- unname(map[result$participants$id])
  result$routing$trigger_message_id <- rep(NA_integer_, nrow(result$routing))
  result
}

.sum_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content), character(1)), collapse = "\n")
}

# The messages a summary prompt shows.
.sum_prompt_messages <- function(prompt) {
  lines <- strsplit(prompt, "\n", fixed = TRUE)[[1]]
  hits <- regmatches(lines, regexec("^\\[#([0-9]+)\\] ([^:(]+?)(?: \\([^)]*\\))?: (.*)$", lines))
  hits <- Filter(length, hits)
  data.frame(id = vapply(hits, function(h) as.integer(h[2]), integer(1)),
             label = vapply(hits, `[`, character(1), 3),
             text = vapply(hits, `[`, character(1), 4), stringsAsFactors = FALSE)
}

.sum_prompt_by <- function(prompt) {
  if (grepl("contributions of one participant", prompt, fixed = TRUE)) "participant"
  else if (grepl("one guide item at a time", prompt, fixed = TRUE)) "item"
  else if (grepl("where a focus group discussion divided", prompt, fixed = TRUE)) "disagreements"
  else if (grepl("developed over time", prompt, fixed = TRUE)) "trajectory"
  else if (grepl("turn by turn", prompt, fixed = TRUE)) "turn"
  else "session"
}

.sum_prompt_levels <- function(prompt) {
  shape <- regmatches(prompt, regexpr("\"levels\": \\{[^}]*\\}", prompt))
  regmatches(shape, gregexpr("(headline|brief|detailed)", shape))[[1]]
}

.sum_first_words <- function(text, n = 4L) {
  paste(utils::head(strsplit(text, " ", fixed = TRUE)[[1]], n), collapse = " ")
}

.sum_claim <- function(id, unit, label, message, kind = "position", act = NULL) {
  claim <- list(id = id, unit = unit, kind = kind,
                text = sprintf("%s spoke about the hours.", label),
                status = "stated", speakers = list(label), evidence = list(message$id),
                context = list(),
                quotes = list(list(message_id = message$id, speaker = label,
                                   text = .sum_first_words(message$text))))
  if (!is.null(act)) claim$act <- act
  claim
}

# A reply that passes the checks, built from the prompt alone.
.sum_honest_reply <- function(prompt) {
  by <- .sum_prompt_by(prompt)
  msgs <- .sum_prompt_messages(prompt)
  levels <- .sum_prompt_levels(prompt)
  participants <- msgs[grepl("^P[0-9]+$", msgs$label), , drop = FALSE]
  claims <- list()
  level_ids <- list()
  if (by == "participant") {
    unit <- sub(".*one participant, (P[0-9]+)\\..*", "\\1", prompt)
    own <- participants[participants$label == unit, , drop = FALSE]
    for (i in seq_len(min(2L, nrow(own)))) claims[[i]] <- .sum_claim(i, unit, unit, own[i, ])
  } else if (by == "item") {
    unit <- sub(".*Summarize guide item ([0-9]+)\\..*", "\\1", prompt)
    first <- participants[!duplicated(participants$label), , drop = FALSE]
    for (i in seq_len(nrow(first))) claims[[i]] <- .sum_claim(i, unit, first$label[i], first[i, ])
  } else if (by == "turn") {
    act <- if ("detailed" %in% levels) "answer" else NULL
    for (i in seq_len(nrow(msgs))) {
      claims[[i]] <- .sum_claim(i, as.character(msgs$id[i]), msgs$label[i], msgs[i, ],
                                kind = "gist", act = if (is.null(act)) NULL else
                                  if (msgs$label[i] == "Moderator") "procedure" else act)
      if (msgs$label[i] == "Moderator") claims[[i]]$text <- "The moderator asked a question."
    }
  } else if (by == "trajectory") {
    stages <- regmatches(prompt, gregexpr("(third|item)_[0-9]+ \\(messages #[0-9]+ to #[0-9]+\\)",
                                          prompt))[[1]]
    stages <- unique(stages)
    for (s in stages) {
      key <- sub(" .*", "", s)
      bounds <- sub("^[a-z]+_[0-9]+", "", s)
      range <- as.integer(regmatches(bounds, gregexpr("[0-9]+", bounds))[[1]])
      inside <- participants[participants$id >= range[1] & participants$id <= range[2], , drop = FALSE]
      if (!nrow(inside)) next
      id <- length(claims) + 1L
      claims[[id]] <- .sum_claim(id, key, inside$label[1], inside[1, ], kind = "stage")
    }
  } else if (by == "session") {
    first <- participants[!duplicated(participants$label), , drop = FALSE]
    for (i in seq_len(nrow(first))) claims[[i]] <- .sum_claim(i, "session", first$label[i], first[i, ])
  }
  ids <- vapply(claims, function(cl) as.integer(cl$id), integer(1))
  units <- vapply(claims, function(cl) cl$unit, character(1))
  for (level in levels) {
    keep <- if (by == "trajectory") {
      if (level == "headline") grepl("^third_", units) else grepl("^item_", units)
    } else rep(TRUE, length(ids))
    level_ids[[level]] <- as.list(ids[keep])
  }
  if (!length(level_ids)) level_ids <- structure(list(), names = character())
  as.character(jsonlite::toJSON(list(claims = claims, levels = level_ids), auto_unbox = TRUE,
                                null = "null"))
}

# A runner for summaries: `reply(prompt, n, messages)` gives the reply text
# of call n. It keeps every prompt it receives.
.sum_runner <- function(reply = function(prompt, n, messages) .sum_honest_reply(prompt),
                        finish = function(n) "stop") {
  env <- new.env()
  env$n <- 0L
  env$prompts <- character()
  env$messages <- list()
  env$tokens <- integer()
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    messages <- experiments$messages[[1]]
    prompt <- .sum_prompt(messages)
    env$prompts <- c(env$prompts, prompt)
    env$messages[[env$n]] <- messages
    env$tokens <- c(env$tokens, as.integer(experiments$config[[1]]$model_params$max_tokens %||% NA))
    experiments$response_text <- reply(messages[[1]]$content, env$n, messages)
    experiments$finish_reason <- finish(env$n)
    experiments
  }
  env
}
