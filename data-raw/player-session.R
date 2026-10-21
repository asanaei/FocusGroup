# Builds inst/extdata/player-session.rds, the synthetic session that the
# fg_player() example, its tests, and the README screenshot use. No model is
# called: a stub runner writes every reply, desire score, probe answer, and
# the final summary. Run from the package root:
#
#   Rscript data-raw/player-session.R
#
# The session: four invented participants, a desire-based flow with two
# responses per question, a guide whose opening and closing are read as
# written, a private question asked at the start and before the end, and an
# injected objection from a "Guest expert" after guide item 3. The
# moderator's line for item 4 addresses P2, and at item 2 the moderator calls
# on the participants who have not yet spoken.
#
# FocusGroup 0.5.4 records neither call-ons nor addressed turns, and has no
# handle or item column. When the engine lacks them, the block marked
# "Interim" below adds them by hand, as the 0.5.5 record contract describes
# them. With an engine that records them, that block does not run and the
# session itself produces them.

pkgload::load_all(".", quiet = TRUE)

topic <- "Library opening hours"
questions <- c(
  "Which hours do you use the library now, and why those?",
  "Who do you think the current hours leave out?",
  "If the library could open longer on only one day, which day should it be?")
guide <- list(
  Opening = paste(
    "Welcome, and thank you for coming. Tonight we are talking about the",
    "library's opening hours: when you use the branch, and when you cannot.",
    "There are no right or wrong answers."),
  Engagement = questions[1:2],
  Exploration = questions[3],
  Closing = "Thank you all. That is the end of our discussion; your views will go to the library board.")

people <- data.frame(
  age = c(67, 38, 45, 20),
  occupation = c("retired schoolteacher", "night-shift nurse", "bakery owner",
                 "community-college student"),
  `library use` = c(
    "walks to the branch most mornings and runs its Thursday reading hour for children",
    "can reach the branch only on weekends, with two children in primary school",
    "rents the meeting room for evening classes on starting a small business",
    "studies there between classes and an afternoon job at a hardware store"),
  check.names = FALSE, stringsAsFactors = FALSE)
speaker_keys <- c(P1 = "retired schoolteacher", P2 = "night-shift nurse",
                  P3 = "bakery owner", P4 = "community-college student")

moderator_lines <- c(
  "Let's start with how you use the library today. Which hours do you use it now, and why those?",
  "Thank you. Now I'd like to look at who might be missing. Who do you think the current hours leave out?",
  "Thanks, everyone, for those views. P2, if the library could open longer on only one day, which day should it be?")

replies <- list(
  P1 = c(
    "I come in at nine most mornings, right when the doors open. The building is quiet, and by ten I am setting up the reading corner for the Thursday group.",
    "I take the guest expert's point seriously: the morning programs are the ones young families depend on. But I would not protect them by shutting out everyone who works days.",
    "Thursday, so the reading hour could move later and working parents could bring their children. I would give up my quiet mornings for that."),
  P2 = c(
    "Saturdays, and only Saturdays. After a night shift I sleep until early afternoon, so on weekdays the branch has closed by the time I could get there with the kids.",
    "People who work shifts, plainly. It is **weekends or nothing** for us, and the Sunday closure means my children lose half of what they could use.",
    "Sunday. One afternoon a week would let my family use the library together, which right now never happens."),
  P3 = c(
    "Mostly Tuesday and Wednesday evenings, for the business classes I run in the meeting room. We have to pack up by a quarter to eight, which cuts every session short.",
    "Anyone who works nine to five. The morning programs matter, but cutting two of them to buy evening hours is a trade I would make, since evenings serve more adults.",
    "Wednesday evening. My classes run then, and a later close would let us finish without rushing people out the door."),
  P4 = c(
    "Between classes, so late mornings and early afternoons. It is the only quiet place I have, and I *always* wish it stayed open after my shift ends at six.",
    "Students with jobs. Half my classmates study at a kitchen table because the branch closes at six, and the morning programs do nothing for them.",
    "Friday. It is the one evening I am free, and the study rooms are full of people like me whenever they are open."))

# Desire scores by participant, question, and pick within the question. The
# first pick of question 1 is a tie between P1 and P3, drawn at random; the
# second pick of question 2 is a tie between P3 and P4.
desire <- list(
  P1 = list(c(7, 6), c(5, 4), c(5, 7)),
  P2 = list(c(4, 4), c(8, 8), c(9, 9)),
  P3 = list(c(7, 6), c(6, 6), c(4, 5)),
  P4 = list(c(3, 5), c(6, 6), c(6, 6)))

probe_answers <- list(
  P1 = c("4. Mornings suit me, but I can see why others want evenings.",
         "5. Hearing the others, a later Thursday would help families more than it costs me."),
  P2 = c("7. Anything later than six would change what my family can do.",
         "7. Nothing in the discussion changed my mind."),
  P3 = c("6. Later evenings would let my classes run their full length.",
         "6. The objection about the morning programs gave me pause, but not enough to change my answer."),
  P4 = c("6. I study best in the evening.",
         "7. The evening is when students like me can come."))

final_summary <- paste(
  "The participants agreed that the current hours suit people who are free",
  "during the day and leave out shift workers, people with day jobs, and",
  "working students. They differed on the guest expert's objection: P1",
  "wanted to keep the two morning programs for young children, while P3",
  "would give them up for evening hours. Asked for one longer day, they",
  "named Thursday, Sunday, Wednesday, and Friday, each for a reason tied to",
  "their own week.")

# The stub runner. It tells the call types apart by their prompts, the
# speaker by the persona in the system message, and the question by its text.
stub <- local({
  turns <- list()
  probes_asked <- list()
  moderator_calls <- 0L
  function(experiments, ...) {
    messages <- experiments$messages[[1]]
    contents <- vapply(messages, function(m) as.character(m$content %||% ""), character(1))
    roles <- vapply(messages, function(m) as.character(m$role %||% ""), character(1))
    prompt <- paste(contents, collapse = "\n")
    system <- paste(contents[roles == "system"], collapse = "\n")
    speaker <- names(speaker_keys)[vapply(speaker_keys, grepl, logical(1), x = system,
                                          fixed = TRUE)]
    question <- which(vapply(questions, grepl, logical(1), x = prompt, fixed = TRUE))
    question <- if (length(question)) max(question) else NA_integer_
    text <- if (grepl("Summary:$", prompt)) {
      final_summary
    } else if (grepl("private question", prompt, fixed = TRUE)) {
      k <- (probes_asked[[speaker]] %||% 0L) + 1L
      probes_asked[[speaker]] <<- k
      probe_answers[[speaker]][[k]]
    } else if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      k <- (turns[[paste(question)]] %||% 0L) + 1L
      as.character(desire[[speaker]][[question]][[min(k, 2L)]])
    } else if (!length(speaker)) {
      moderator_calls <<- moderator_calls + 1L
      moderator_lines[[moderator_calls]]
    } else {
      turns[[paste(question)]] <<- (turns[[paste(question)]] %||% 0L) + 1L
      replies[[speaker]][[question]]
    }
    if (length(text) != 1L || is.na(text)) stop("The stub has no reply for this call.")
    experiments$response_text <- text
    experiments
  }
})

supports <- function(name) name %in% names(formals(run_focus_group))
end_point <- tryCatch({ fg_probe("x", at = "before_closing"); "before_closing" },
                      error = function(e) "end")
args <- list(
  topic = topic, config = LLMR::llm_config("openai", "example-model"),
  n_participants = 4, guide = guide, demographics = people,
  flow = "desire_based", seed = 110, verbose = FALSE,
  max_participant_responses = 2, moderator_verbatim = c(1, 5),
  injections = fg_injection(
    paste("A reasoned objection: staying open two more evening hours on weekdays",
          "would cost the branch its two morning programs for young children,",
          "because the staff budget does not grow."),
    at = fg_after_item(3), from = "Guest expert"),
  probes = fg_probe(
    "From 1 to 7, how strongly do you favor keeping the branch open later in the evening?",
    at = list("start", end_point), label = "evening_hours"),
  .runner = stub)
if (supports("call_on_silent")) args$call_on_silent <- "opening"
result <- do.call(run_focus_group, args)

# ---- Interim: the 0.5.5 record fields, added by hand -------------------------
if (!"handle" %in% names(result$transcript)) {
  moderator_handle <- "Moderator"
  call_on_text <- "{participant}, I'd like to hear from you as well. What are your thoughts?"
  tr <- result$transcript
  routing <- result$routing

  # Item 4: the moderator's line names P2, who answers next.
  item4 <- which(tr$is_moderator & tr$round == 4L)
  stopifnot(length(item4) == 1L, grepl("^Thanks, everyone, for those views\\. P2,", tr$text[[item4]]),
            identical(tr$speaker_id[[item4 + 1L]], "P2"))
  pick <- which(routing$message_id == tr$message_id[[item4 + 1L]])
  routing$rule[[pick]] <- "addressed"
  routing$candidates[[pick]] <- "P2"
  routing$excluded[[pick]] <- character()
  routing$scores[pick] <- list(NULL)
  routing$tied[[pick]] <- character()
  routing$tie_break[[pick]] <- "none"
  routing$threshold[[pick]] <- NA_real_
  routing$scoring_mode[[pick]] <- NA_character_
  routing$trigger_message_id <- NA_integer_
  routing$trigger_message_id[[pick]] <- tr$message_id[[item4]]
  tr$metadata[[item4 + 1L]]$addressed <- TRUE

  # Item 3: FocusGroup 0.5.4 kept the closer of item 2 from opening item 3;
  # from 0.5.5 a moderator line clears that exclusion, so the first pick of
  # item 3 asks everyone. The stub's score for the participant left out is
  # added; it does not change the pick.
  item3 <- which(tr$is_moderator & !vapply(tr$metadata, function(m) isTRUE(m$injected), logical(1)) &
                   tr$round == 3L)
  first3 <- min(which(!tr$is_moderator & tr$round == 3L & tr$speaker_id %in% names(replies)))
  pick <- which(routing$message_id == tr$message_id[[first3]])
  left_out <- names(routing$excluded[[pick]])[routing$excluded[[pick]] == "previous_speaker"]
  stopifnot(length(item3) == 1L, length(left_out) == 1L)
  scores <- routing$scores[[pick]]
  scores[[left_out]] <- desire[[left_out]][[2]][[1]]
  routing$scores[[pick]] <- scores[names(replies)]
  routing$candidates[[pick]] <- names(replies)
  routing$excluded[[pick]] <- character()
  stopifnot(identical(names(which.max(routing$scores[[pick]])), routing$selected_id[[pick]]))

  # Item 2: the call-ons, after the last turn of the item, for the
  # participants who have not yet spoken, in roster order.
  ids <- result$participants$id[!result$participants$is_moderator]
  item2 <- which(tr$round == 2L)
  after <- max(item2)
  silent <- setdiff(ids, tr$speaker_id[seq_len(after)])
  stopifnot(length(silent) == 2L)
  shift <- 2L * length(silent)
  cut <- tr$message_id[[after]]
  bump <- function(x) ifelse(!is.na(x) & x > cut, x + shift, x)
  turn_cut <- max(routing$turn[which(routing$message_id <= cut)])
  bump_turn <- function(x) ifelse(!is.na(x) & x > turn_cut, x + length(silent), x)

  tr$message_id <- bump(tr$message_id)
  tr$metadata <- I(lapply(tr$metadata, function(m) {
    if (!is.null(m$injection$cut_after_message_id)) {
      m$injection$cut_after_message_id <- bump(m$injection$cut_after_message_id)
    }
    m
  }))
  routing$message_id <- bump(routing$message_id)
  routing$trigger_message_id <- bump(routing$trigger_message_id)
  routing$turn <- bump_turn(routing$turn)
  result$probes$after_message_id <- bump(result$probes$after_message_id)
  result$probes$turn <- bump_turn(result$probes$turn)
  result$injections$message_id <- bump(result$injections$message_id)
  result$injections$cut_after_message_id <- bump(result$injections$cut_after_message_id)
  result$injections$turn <- bump_turn(result$injections$turn)
  result$calls$message_id <- bump(result$calls$message_id)

  new_rows <- list()
  new_routing <- list()
  for (k in seq_along(silent)) {
    id <- silent[[k]]
    line_id <- cut + 2L * k - 1L
    line <- tr[after, , drop = FALSE]
    line$message_id <- line_id
    line$speaker_id <- "MOD"
    line$is_moderator <- TRUE
    line$text <- sub("{participant}", id, call_on_text, fixed = TRUE)
    line$response_id <- NA_character_
    line$finish_reason <- NA_character_
    line$sent_tokens <- NA_integer_
    line$rec_tokens <- NA_integer_
    line$total_tokens <- NA_integer_
    line$duration_s <- NA_real_
    line$provider <- NA_character_
    line$model <- NA_character_
    line$metadata <- I(list(list(verbatim = TRUE,
                                 call_on = list(point = "opening", participant = id,
                                                handle = id))))
    reply <- line
    reply$message_id <- line_id + 1L
    reply$speaker_id <- id
    reply$is_moderator <- FALSE
    reply$text <- replies[[id]][[1]]
    reply$provider <- "openai"
    reply$model <- "example-model"
    reply$metadata <- I(list(list(called_on = TRUE, usage_unknown = 1)))
    new_rows <- c(new_rows, list(line, reply))

    row <- routing[routing$message_id <= cut, , drop = FALSE]
    row <- row[nrow(row), , drop = FALSE]
    row$turn <- turn_cut + k
    row$rule <- "called_on"
    row$selected_id <- id
    row$message_id <- line_id + 1L
    row$threshold <- NA_real_
    row$tie_break <- "none"
    row$scoring_mode <- NA_character_
    row$candidates <- I(list(id))
    row$excluded <- I(list(character()))
    row$scores <- I(list(NULL))
    row$tied <- I(list(character()))
    row$trigger_message_id <- line_id
    new_routing <- c(new_routing, list(row))
  }
  before <- seq_len(after)
  tr <- rbind(tr[before, , drop = FALSE], do.call(rbind, new_rows),
              tr[-before, , drop = FALSE])
  rownames(tr) <- NULL
  routing_before <- routing$turn <= turn_cut
  routing <- rbind(routing[routing_before, , drop = FALSE], do.call(rbind, new_routing),
                   routing[!routing_before, , drop = FALSE])
  routing$pick <- seq_len(nrow(routing))
  rownames(routing) <- NULL

  # The handle and item columns, the participants' handles, and the 0.5.5
  # metadata. In FocusGroup 0.5.4 every round is one guide item.
  tr$handle <- ifelse(tr$speaker_id == "MOD", moderator_handle, tr$speaker_id)
  tr$item <- tr$round
  result$transcript <- tr
  result$routing <- routing
  result$participants$handle <- ifelse(result$participants$is_moderator,
                                       moderator_handle, result$participants$id)
  result$metadata$handles <- data.frame(agent_id = result$participants$id,
                                        handle = result$participants$handle,
                                        stringsAsFactors = FALSE)
  result$metadata$call_on_silent <- list(points = "opening", items = 2L)
  result$metadata$call_on_text <- call_on_text
  result$metadata$final_summary <- TRUE
}

# The FocusGroup object is left out: it holds the stub runner and the model
# configuration, and the player reads only the records.
result$focus_group <- NULL
dir.create(file.path("inst", "extdata"), showWarnings = FALSE, recursive = TRUE)
saveRDS(result, file.path("inst", "extdata", "player-session.rds"), version = 3)
cat("Wrote inst/extdata/player-session.rds:", nrow(result$transcript), "messages,",
    nrow(result$routing), "speaker selections,", nrow(result$probes), "probe answers.\n")
