# File: R/player.R
# Purpose: fg_player(), a self-contained HTML replay of a finished session,
# built from the session's records alone.

#' Replay a finished session in the browser
#'
#' Writes one self-contained HTML file that steps through a finished focus
#' group from its saved records: a title page, one page per participant, then
#' every logged message in order, with the record of each speaker selection
#' before the turn it chose and each private answer where it was asked, and
#' an end page. No model is called and nothing is reconstructed: a record the
#' page needs but the session lacks stops the function with an error naming
#' it.
#'
#' @param x A `focus_group_result` from [run_focus_group()], a
#'   `focus_group_arms` from [run_focus_group_arms()], a [FocusGroup] object
#'   after `run_simulation()`, or the list that `FocusGroup$run_arms()`
#'   returns.
#' @param file Path of the HTML file to write. `NULL` (the default) writes a
#'   temporary file. When `x` holds several arms and `arm` is `NULL`, one file
#'   is written per arm, each holding the shared messages and that arm's
#'   continuation; `file` then names a directory (a new temporary one when
#'   `NULL`), and the files are named `<arm>.html`.
#' @param arm `NULL`, or the name of the one arm to replay.
#' @param annotations Optional named list, by participant ID, of named
#'   character vectors shown on that participant's page and sidebar card, for
#'   example `list(P1 = c(stake = "high"))`.
#' @param mode `"step"` (the default) shows one step at a time; `"static"`
#'   shows every step at once, for printing and archives.
#' @param strip_speaker_prefix `FALSE` (the default) shows every message as
#'   recorded. `TRUE` drops a speaker label that the model wrote at the start
#'   of its own line, such as `"**MOD:**"` in a moderator line or `"P3:"` in
#'   the reply of the participant whose handle is P3, never another speaker's
#'   label; a speaker's own label is its handle, not its ID. The title page
#'   says so and counts the lines changed.
#' @param open Open the file with [utils::browseURL()]. The default opens it
#'   in an interactive session.
#'
#' @details
#' The page reads these records: `transcript`, with each message's
#' `metadata` (verbatim moderator lines, injected comments, call-ons, and
#' addressed turns); `routing`, for the scores, exclusions, tie draws, and
#' rule behind each participant turn; `probes`, placed after the message
#' named by `after_message_id`; `participants`, for each persona as sent,
#' without the standing rules; `summary`; and `metadata`. Results saved by
#' FocusGroup 0.5.3 and earlier lack `routing` and `probes`, so the function
#' stops for them. For results without the `handle` column of 0.5.5, each
#' speaker's handle is its ID.
#'
#' The steps, in order:
#' \itemize{
#'   \item a title page: topic and purpose, the number of participants,
#'     whether the moderator's lines were read as written or written by the
#'     model, the turn-taking rule, responses per question, model, date,
#'     package versions, and arm;
#'   \item one page per participant: ID and handle, the persona as sent, and
#'     any `annotations`, with that participant's sidebar card highlighted;
#'   \item one step per logged message: the roster line, moderator lines,
#'     injected comments marked with their source, and participant turns;
#'   \item before each participant turn, a panel with the selection record:
#'     under desire-based selection, the score of every participant asked,
#'     those not asked with the reason, the participant chosen, and whether a
#'     tie was drawn at random; under probabilistic selection, each
#'     participant's chance of being drawn; for a call-on or a participant
#'     the moderator addressed, that fact. Round robin gets no panel;
#'   \item each private question, as a panel with every answer, placed where
#'     it was asked; no agent sees these answers;
#'   \item an end page with the turns of every participant, zeros included,
#'     and the final summary, which the model wrote after the session.
#' }
#'
#' In `"step"` mode the right arrow, space, Page Down, Enter, or a click
#' shows the next step; the left arrow, Page Up, or Backspace goes back; Home
#' returns to the first page; End shows every step; S shows or hides the
#' selection panels; and F toggles full screen. A presentation clicker sends
#' Page Down and Page Up. Adding `#step=12` to the address opens the page at
#' step 12.
#'
#' Each participant has one color, used in the sidebar, the turns, and the
#' score bars: the Okabe-Ito palette for up to 7 participants and Paul Tol's
#' muted scheme for 8 or 9, both readable with the common forms of color
#' blindness. Beyond 9 the colors come from `grDevices::hcl.colors(n, "Dark
#' 3")`, with a message, and are harder to tell apart. The moderator is gray.
#'
#' The file needs no network connection: styles, script, and data are
#' inline. Model text is inserted as text, never as markup; only `**bold**`
#' and `*italics*` are rendered.
#'
#' @return The path of the file written, invisibly. For several arms, a
#'   character vector of paths named by arm.
#' @seealso [run_focus_group()] and [run_focus_group_arms()] for the records
#'   the page reads.
#' @examples
#' session <- readRDS(system.file("extdata", "player-session.rds",
#'                                package = "FocusGroup"))
#' page <- fg_player(session, file = tempfile(fileext = ".html"),
#'                   open = FALSE)
#' file.exists(page)
#'
#' # Every step at once, with annotations on two participants' pages.
#' fg_player(session, file = tempfile(fileext = ".html"), mode = "static",
#'           annotations = list(P1 = c(recruited = "library newsletter"),
#'                              P2 = c(recruited = "hospital notice board")),
#'           open = FALSE)
#' @export
fg_player <- function(x, file = NULL, arm = NULL, annotations = NULL,
                      mode = c("step", "static"), strip_speaker_prefix = FALSE,
                      open = interactive()) {
  mode <- match.arg(mode)
  .fg_player_flag(strip_speaker_prefix, "strip_speaker_prefix")
  .fg_player_flag(open, "open")
  if (!is.null(arm) && !.fg_player_string(arm)) {
    stop("`arm` must be `NULL` or one arm name.", call. = FALSE)
  }
  if (!is.null(file) && !.fg_player_string(file)) {
    stop("`file` must be `NULL` or one path.", call. = FALSE)
  }

  sessions <- .fg_player_inputs(x)
  arms <- names(sessions)
  if (!is.null(arm)) {
    if (is.null(arms)) {
      own <- sessions[[1]]$metadata$branch$arm
      if (!identical(own, arm)) {
        stop("`x` holds one session, not the arm '", arm, "'; leave `arm` as `NULL`.",
             call. = FALSE)
      }
    } else {
      if (!arm %in% arms) {
        stop("No arm named '", arm, "'; the arms are ",
             paste0("'", arms, "'", collapse = ", "), ".", call. = FALSE)
      }
      sessions <- sessions[arm]
    }
  }
  # Annotations are checked against the roster before any file is written.
  pages <- lapply(sessions, function(s) {
    session <- .fg_player_session(s)
    payload <- .fg_player_payload(session, annotations = annotations, mode = mode,
                                  strip_speaker_prefix = strip_speaker_prefix)
    .fg_player_html(payload)
  })

  paths <- .fg_player_paths(file, names(sessions), length(sessions))
  for (i in seq_along(pages)) .fg_player_write(pages[[i]], paths[[i]])
  if (open) for (path in paths) utils::browseURL(path)
  invisible(paths)
}

.fg_player_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  invisible(value)
}

.fg_player_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

# ---- Inputs -----------------------------------------------------------------

# The sessions to replay: an unnamed list of one result, or a list of arm
# results named by arm.
.fg_player_inputs <- function(x) {
  if (inherits(x, "focus_group_result")) return(list(x))
  if (inherits(x, "focus_group_arms")) {
    if (!is.list(x$arms) || !length(x$arms)) {
      stop("The focus_group_arms object holds no arm.", call. = FALSE)
    }
    .fg_player_arm_names(names(x$arms))
    return(x$arms)
  }
  if (inherits(x, "FocusGroup")) return(list(.fg_player_from_object(x)))
  is_group <- function(v) inherits(v, "FocusGroup")
  branches <- NULL
  if (is.list(x) && is.list(x$arms) && length(x$arms) &&
      all(vapply(x$arms, is_group, logical(1)))) {
    branches <- x$arms
  } else if (is.list(x) && length(x) && all(vapply(x, is_group, logical(1)))) {
    branches <- x
  }
  if (is.null(branches)) {
    stop("`x` must be a focus_group_result, a focus_group_arms, a FocusGroup ",
         "after run_simulation(), or the list FocusGroup$run_arms() returns.",
         call. = FALSE)
  }
  .fg_player_arm_names(names(branches))
  out <- lapply(names(branches), function(name) {
    .fg_player_from_object(branches[[name]], arm = name)
  })
  names(out) <- names(branches)
  out
}

.fg_player_arm_names <- function(arms) {
  if (is.null(arms) || anyNA(arms) || any(!nzchar(arms)) || anyDuplicated(arms)) {
    stop("Every arm must have a distinct, non-empty name.", call. = FALSE)
  }
  invisible(arms)
}

# A result built from a FocusGroup object, with the records the object keeps.
.fg_player_from_object <- function(fg, arm = NULL) {
  if (!length(fg$conversation_log)) {
    stop("The FocusGroup object holds no session; call run_simulation() first.",
         call. = FALSE)
  }
  if (is.null(fg$records) || !length(fg$records$info) ||
      identical(fg$records$info$source, "transcript")) {
    stop("The FocusGroup object has no `records` of a session it ran, which ",
         "fg_player() needs; imported transcripts and objects saved by ",
         "FocusGroup 0.5.3 and earlier have none.", call. = FALSE)
  }
  flow <- fg$turn_taking_flow
  flow_name <- if (inherits(flow, "DesireBasedFlow")) "desire_based" else
    if (inherits(flow, "ProbabilisticFlow")) "probabilistic" else
      if (inherits(flow, "RoundRobinFlow")) "round_robin" else "custom"
  n <- length(setdiff(names(fg$agents), fg$moderator_id))
  branch <- if (!is.null(arm)) {
    c(list(arm = arm, role = "arm"), fg$records$info$branch %||% list())
  }
  # A session read from a file keeps the provenance its configurations
  # would give.
  if (!is.null(fg$records$info$recorded)) {
    result <- .fg_object_result(fg)
    if (!is.null(branch)) result$metadata$branch <- branch
    return(result)
  }
  .fg_focus_group_result(
    fg, flow_name, fg$message_mode %||% NA_character_, n, NA_integer_,
    run_meta = list(branch = branch))
}

# ---- The session record -----------------------------------------------------

.fg_player_missing <- function(what, note = NULL) {
  stop("The session has no `", what, "` record, which fg_player() needs",
       if (!is.null(note)) paste0("; ", note) else "",
       ". The player reads the saved records and reconstructs nothing.",
       call. = FALSE)
}

.fg_player_require_columns <- function(x, name, columns, note = NULL) {
  table <- x[[name]]
  if (!is.data.frame(table)) .fg_player_missing(name, note)
  for (column in columns) {
    if (!column %in% names(table)) .fg_player_missing(paste0(name, "$", column), note)
  }
  table
}

# The records the page is built from, checked and with the 0.5.4 fallbacks
# of the record contract applied: handle = ID, item = round, and no
# trigger message.
.fg_player_session <- function(x) {
  old <- "results saved by FocusGroup 0.5.3 and earlier lack it"
  transcript <- .fg_player_require_columns(
    x, "transcript",
    c("message_id", "round", "speaker_id", "is_moderator", "text", "phase", "metadata"))
  participants <- .fg_player_require_columns(
    x, "participants", c("id", "is_moderator", "persona"))
  routing <- .fg_player_require_columns(
    x, "routing",
    c("pick", "rule", "selected_id", "message_id", "threshold", "tie_break",
      "candidates", "excluded", "scores", "tied"), old)
  probes <- .fg_player_require_columns(
    x, "probes",
    c("probe", "point", "agent_id", "question", "answer", "after_message_id"), old)
  if (!"summary" %in% names(x)) .fg_player_missing("summary")
  metadata <- x$metadata
  if (!is.list(metadata)) .fg_player_missing("metadata")
  for (field in c("topic", "purpose", "flow", "max_participant_responses",
                  "model", "versions", "started")) {
    if (!field %in% names(metadata)) .fg_player_missing(paste0("metadata$", field))
  }

  if (!nrow(transcript)) stop("The session's transcript has no message.", call. = FALSE)
  ids <- transcript$message_id
  if (anyNA(ids) || anyDuplicated(ids) || is.unsorted(ids, strictly = TRUE)) {
    stop("The transcript's message ids must be distinct and increasing.",
         call. = FALSE)
  }
  transcript$handle <- if ("handle" %in% names(transcript)) {
    ifelse(is.na(transcript$handle), transcript$speaker_id, transcript$handle)
  } else transcript$speaker_id
  transcript$item <- if ("item" %in% names(transcript)) transcript$item else transcript$round
  transcript$text <- ifelse(is.na(transcript$text), "", as.character(transcript$text))
  transcript$injected <- vapply(transcript$metadata, function(m) isTRUE(m$injected),
                                logical(1))

  moderator_rows <- which(participants$is_moderator %in% TRUE)
  if (length(moderator_rows) != 1L) {
    stop("The session's participants record must have exactly one moderator row.",
         call. = FALSE)
  }
  handle_of <- .fg_player_handles(participants, metadata)
  moderator <- list(id = participants$id[[moderator_rows]],
                    handle = unname(handle_of[[participants$id[[moderator_rows]]]]))
  people <- participants[-moderator_rows, , drop = FALSE]
  if (!nrow(people)) stop("The session has no participant.", call. = FALSE)
  people$handle <- unname(handle_of[people$id])

  # Every speaker is the System row, the moderator, an injected voice, or a
  # participant on the roster. A participant's injected message is that
  # participant's turn.
  turn <- !transcript$is_moderator & transcript$speaker_id != "System" &
    (!transcript$injected | transcript$speaker_id %in% people$id)
  stray <- turn & !transcript$speaker_id %in% people$id
  if (any(stray)) {
    i <- which(stray)[[1]]
    stop("Message ", transcript$message_id[[i]], " is by '", transcript$speaker_id[[i]],
         "', who is not in the participants record.", call. = FALSE)
  }
  moderator_stray <- transcript$is_moderator & !transcript$injected &
    transcript$speaker_id != moderator$id
  if (any(moderator_stray)) {
    i <- which(moderator_stray)[[1]]
    stop("Message ", transcript$message_id[[i]], " is marked as the moderator's but is by '",
         transcript$speaker_id[[i]], "'.", call. = FALSE)
  }
  transcript$turn <- turn

  if (!"trigger_message_id" %in% names(routing)) {
    routing$trigger_message_id <- rep(NA_integer_, nrow(routing))
  }
  .fg_player_check_routing(routing, transcript)
  bad_probe <- is.na(probes$after_message_id) |
    !probes$after_message_id %in% transcript$message_id
  if (any(bad_probe)) {
    stop("Probe '", probes$probe[which(bad_probe)[[1]]], "' names no message of the ",
         "transcript in `after_message_id`.", call. = FALSE)
  }
  bad_agent <- !probes$agent_id %in% people$id
  if (any(bad_agent)) {
    stop("Probe '", probes$probe[which(bad_agent)[[1]]], "' names '",
         probes$agent_id[which(bad_agent)[[1]]], "' in `agent_id`, who is not a ",
         "participant of the session.", call. = FALSE)
  }
  no_persona <- is.na(people$persona)
  if (any(no_persona)) {
    stop("The `participants$persona` record is missing for ",
         paste(people$id[no_persona], collapse = ", "), ".", call. = FALSE)
  }

  list(transcript = transcript, people = people, moderator = moderator,
       routing = routing, probes = probes, summary = x$summary, metadata = metadata)
}

# Handles by agent ID: the participants' `handle` column, else
# `metadata$handles`, else the IDs.
.fg_player_handles <- function(participants, metadata) {
  ids <- participants$id
  handles <- stats::setNames(ids, ids)
  map <- metadata$handles
  if (is.data.frame(map) && all(c("agent_id", "handle") %in% names(map))) {
    known <- map$agent_id %in% ids & !is.na(map$handle)
    handles[map$agent_id[known]] <- map$handle[known]
  }
  if ("handle" %in% names(participants)) {
    known <- !is.na(participants$handle)
    handles[ids[known]] <- participants$handle[known]
  }
  handles
}

# Every participant turn has one routing row naming it, by the speaker who
# took it; every routing row names a turn, except a pick that produced none.
.fg_player_check_routing <- function(routing, transcript) {
  linked <- routing[!is.na(routing$message_id), , drop = FALSE]
  if (anyDuplicated(linked$message_id)) {
    stop("The routing record links two speaker selections to message ",
         linked$message_id[anyDuplicated(linked$message_id)], ".", call. = FALSE)
  }
  at <- match(linked$message_id, transcript$message_id)
  for (k in seq_len(nrow(linked))) {
    i <- at[[k]]
    if (is.na(i) || !transcript$turn[[i]] ||
        !identical(transcript$speaker_id[[i]], linked$selected_id[[k]])) {
      stop("Routing pick ", linked$pick[[k]], " names message ", linked$message_id[[k]],
           ", which is not a turn by ", linked$selected_id[[k]], ".", call. = FALSE)
    }
    trigger <- linked$trigger_message_id[[k]]
    if (!is.na(trigger) && !trigger %in% transcript$message_id) {
      stop("Routing pick ", linked$pick[[k]], " names trigger message ", trigger,
           ", which is not in the transcript.", call. = FALSE)
    }
  }
  unrouted <- transcript$turn & !transcript$message_id %in% linked$message_id
  if (any(unrouted)) {
    i <- which(unrouted)[[1]]
    stop("The routing record has no speaker selection for message ",
         transcript$message_id[[i]], ", a turn by ", transcript$speaker_id[[i]],
         ".", call. = FALSE)
  }
  orphans <- sum(is.na(routing$message_id))
  if (orphans) {
    warning(orphans, " speaker selection(s) in the routing record produced no turn ",
            "and are not shown.", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- The page data ----------------------------------------------------------

.fg_player_phase_labels <- c(
  setup = "setup", opening = "opening", icebreaker_question = "icebreaker",
  engagement_question = "engagement question",
  exploration_question = "exploration question", probing_focused = "follow-up",
  ending_question = "ending question", generic_discussion = "discussion",
  closing = "closing", summarizing = "summary",
  manage_participation = "participation")

.fg_player_phase <- function(phase) {
  phase <- as.character(phase)
  label <- unname(.fg_player_phase_labels[phase])
  ifelse(is.na(label), gsub("_", " ", phase), label)
}

.fg_player_moderator_color <- "#6B6B6B"
.fg_player_voice_color <- "#1F1F1F"

# One color per participant. Okabe-Ito (without black) for up to 7, ordered so
# that yellow, the weakest on white, comes last; Paul Tol's muted scheme for
# 8 or 9; HCL "Dark 3" beyond that.
.fg_player_colors <- function(n) {
  okabe_ito <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                 "#56B4E9", "#F0E442")
  tol_muted <- c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933",
                 "#DDCC77", "#CC6677", "#882255", "#AA4499")
  if (n <= length(okabe_ito)) return(okabe_ito[seq_len(n)])
  if (n <= length(tol_muted)) return(tol_muted[seq_len(n)])
  message("The page has ", n, " participants; beyond 9 the colors come from ",
          "grDevices::hcl.colors(n, \"Dark 3\") and are harder to tell apart.")
  substr(grDevices::hcl.colors(n, "Dark 3"), 1L, 7L)
}

.fg_player_number <- function(n, capital = TRUE) {
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight",
             "nine", "ten")
  if (is.na(n)) return(if (capital) "An unrecorded number of" else "an unrecorded number of")
  word <- if (n >= 1 && n <= 10 && n == floor(n)) words[[n]] else format(n)
  if (capital) paste0(toupper(substring(word, 1, 1)), substring(word, 2)) else word
}

.fg_player_and <- function(x) {
  x <- as.character(x)
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  if (length(x) == 2L) return(paste(x, collapse = " and "))
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[[length(x)]])
}

# A date in English, whatever the locale, in the time zone the record
# gives (UTC for the package's own records). With `zone = TRUE` the zone is
# named after it.
.fg_player_date <- function(started, zone = FALSE) {
  if (!length(started) || is.na(started[[1]])) return(NA_character_)
  tz <- attr(started, "tzone")
  tz <- if (is.null(tz) || is.na(tz[[1]])) "" else tz[[1]]
  t <- as.POSIXct(started[[1]])
  out <- sprintf("%s %d, %s", month.name[[as.integer(format(t, "%m", tz = tz))]],
                 as.integer(format(t, "%d", tz = tz)), format(t, "%Y", tz = tz))
  if (zone && nzchar(tz)) paste0(out, " (", tz, ")") else out
}

# A point of the session in words: "fg_after_item(3)" is "after the
# moderator's line for guide item 3".
.fg_player_point_label <- function(point) {
  point <- as.character(.fg_player_scalar(point))
  if (is.na(point) || !nzchar(point)) return("at a point not recorded")
  item <- regmatches(point, regexec("^fg_after_item\\(([0-9]+)\\)$", point))[[1]]
  if (length(item)) return(paste("after the moderator's line for guide item", item[[2]]))
  turn <- regmatches(point, regexec("^fg_before_turn\\(([0-9]+)\\)$", point))[[1]]
  if (length(turn)) return(paste("before participant turn", turn[[2]]))
  switch(point, start = "at the start of the session", end = "at the end of the session",
         before_closing = "before the closing", paste("at", point))
}

.fg_player_scalar <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[[1]])) NA else x[[1]]
}

.fg_player_regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x)
}

# Remove a label naming the speaker itself from the start of a line: "MOD:",
# "**MOD:**", "**P3**:", in any case. Another speaker's label stays.
.fg_player_strip_label <- function(text, labels) {
  labels <- unique(labels[!is.na(labels) & nzchar(labels)])
  if (!length(labels) || !nzchar(text)) return(text)
  alternatives <- paste(.fg_player_regex_escape(labels), collapse = "|")
  pattern <- paste0("^\\s*(?:\\*{1,2}|_{1,2})?\\s*(?:", alternatives,
                    ")\\s*(?:\\*{1,2}|_{1,2})?\\s*:\\s*(?:\\*{1,2}|_{1,2})?\\s*")
  out <- sub(pattern, "", text, perl = TRUE, ignore.case = TRUE)
  if (nzchar(trimws(out))) out else text
}

# The persona for a sidebar card, without the opening sentence and label
# that the package writes into every persona it renders. The participant's
# page shows the persona exactly as sent.
.fg_player_preview <- function(persona) {
  preview <- sub("^\\s*You are a participant in a focus group\\.\\s*", "", persona)
  preview <- sub("^Background:\\s*", "", preview)
  if (nzchar(trimws(preview))) preview else persona
}

.fg_player_rule_sentence <- function(flow) {
  switch(as.character(flow),
    round_robin = "Round robin: the participants speak in a fixed rotation.",
    probabilistic = paste(
      "Probabilistic selection: each turn goes to a participant drawn at random,",
      "with a chance that falls to zero after speaking and recovers over the",
      "following turns."),
    desire_based = paste(
      "Desire-based selection: before each turn, every participant except the",
      "one who spoke last rates its desire to speak from 0 to 10, and the",
      "highest score takes the floor, with ties drawn at random."),
    "A custom turn-taking flow chooses each speaker.")
}

.fg_player_rule_short <- function(flow) {
  switch(as.character(flow),
    round_robin = "Round robin", probabilistic = "Probabilistic selection",
    desire_based = "Desire-based selection", "Custom turn-taking flow")
}

# The payload the template renders: the roster with colors, and the steps.
.fg_player_payload <- function(session, annotations = NULL, mode = "step",
                               strip_speaker_prefix = FALSE) {
  tr <- session$transcript
  people <- session$people
  meta <- session$metadata
  moderator <- session$moderator
  notes <- .fg_player_annotations(annotations, people$id)
  colors <- stats::setNames(.fg_player_colors(nrow(people)), people$id)
  handle_of <- stats::setNames(people$handle, people$id)

  agents <- lapply(seq_len(nrow(people)), function(i) {
    id <- people$id[[i]]
    persona <- as.character(people$persona[[i]] %|NA|% "")
    list(id = id, handle = people$handle[[i]], color = unname(colors[[id]]),
         persona = persona, preview = .fg_player_preview(persona),
         annotations = notes[[id]] %||% list())
  })

  # Messages, with the self-label removed when asked.
  verbatim <- vapply(tr$metadata, function(m) isTRUE(m$verbatim), logical(1))
  call_on <- lapply(tr$metadata, function(m) m$call_on)
  guide_line <- tr$is_moderator & !tr$injected & vapply(call_on, is.null, logical(1))
  stripped <- 0L
  if (strip_speaker_prefix) {
    for (i in which((tr$turn | (guide_line & !verbatim)))) {
      # The speaker's own label is its handle; its ID can be another
      # speaker's handle.
      labels <- tr$handle[[i]]
      if (tr$is_moderator[[i]]) labels <- c(labels, moderator$handle, "MOD", "Moderator")
      new <- .fg_player_strip_label(tr$text[[i]], labels)
      if (!identical(new, tr$text[[i]])) {
        stripped <- stripped + 1L
        tr$text[[i]] <- new
      }
    }
  }
  mixed_verbatim <- any(verbatim[guide_line]) && !all(verbatim[guide_line])
  item_verbatim <- if (any(guide_line)) {
    tapply(verbatim[guide_line], tr$item[guide_line], all)
  } else logical()
  called <- which(!vapply(call_on, is.null, logical(1)))
  call_ons <- data.frame(
    item = tr$item[called],
    id = vapply(call_on[called], function(x) as.character(.fg_player_scalar(x$participant)),
                character(1)),
    stringsAsFactors = FALSE)
  engine_stripped <- sum(vapply(tr$metadata, function(m) isTRUE(m$self_label_removed),
                                logical(1)))

  message_step <- function(i) {
    m <- tr$metadata[[i]]
    base <- list(message_id = tr$message_id[[i]], item = tr$item[[i]],
                 round = tr$round[[i]], phase = .fg_player_phase(tr$phase[[i]]),
                 speaker = tr$speaker_id[[i]], handle = tr$handle[[i]],
                 text = tr$text[[i]])
    if (identical(tr$speaker_id[[i]], "System") && !tr$injected[[i]]) {
      return(c(list(kind = "system"), base))
    }
    if (tr$injected[[i]]) {
      inj <- m$injection %||% list()
      from <- as.character(.fg_player_scalar(inj$from) %|NA|% tr$speaker_id[[i]])
      if (identical(from, "moderator")) from <- "the moderator"
      own_turn <- tr$turn[[i]]
      if (own_turn) from <- tr$handle[[i]]
      visibility <- as.character(.fg_player_scalar(inj$visibility))
      arm <- .fg_player_scalar(inj$arm)
      step <- c(list(kind = "injected"), base, list(
        source = from, is_moderator = isTRUE(tr$is_moderator[[i]]),
        point = as.character(.fg_player_scalar(inj$point) %|NA|% ""),
        point_label = .fg_player_point_label(inj$point),
        arm = if (is.na(arm)) NULL else as.character(arm),
        visibility = if (is.na(visibility)) NULL else visibility,
        color = if (isTRUE(tr$is_moderator[[i]])) .fg_player_moderator_color else
          if (own_turn) unname(colors[[tr$speaker_id[[i]]]]) else
            .fg_player_voice_color))
      if (own_turn) step$as_turn <- TRUE
      return(step)
    }
    if (tr$is_moderator[[i]]) {
      called <- m$call_on
      tag <- NULL
      if (!is.null(called)) {
        who <- as.character(.fg_player_scalar(called$participant))
        if (is.na(who) || !who %in% names(handle_of)) {
          stop("The call-on line in message ", tr$message_id[[i]],
               " names no participant of the session.", call. = FALSE)
        }
        tag <- paste("calls on", handle_of[[who]])
      } else if (mixed_verbatim && verbatim[[i]]) {
        tag <- "read as written"
      }
      return(c(list(kind = "moderator"), base, list(tag = tag)))
    }
    tag <- if (isTRUE(m$called_on)) "called on" else if (isTRUE(m$addressed)) "addressed"
    c(list(kind = "turn"), base, list(tag = tag))
  }

  steps <- list(.fg_player_title_step(session, nrow(people), item_verbatim, call_ons,
                                      handle_of, strip_speaker_prefix, stripped,
                                      engine_stripped))
  for (i in seq_len(nrow(people))) {
    steps[[length(steps) + 1L]] <- list(kind = "persona", agent = people$id[[i]],
                                        index = i, n = nrow(people))
  }
  routing_at <- match(tr$message_id, session$routing$message_id)
  probe_groups <- .fg_player_probe_groups(session$probes)
  for (i in seq_len(nrow(tr))) {
    if (tr$turn[[i]]) {
      panel <- .fg_player_routing_step(session$routing[routing_at[[i]], , drop = FALSE],
                                       people, handle_of, meta$flow)
      if (!is.null(panel)) steps[[length(steps) + 1L]] <- panel
    }
    steps[[length(steps) + 1L]] <- message_step(i)
    for (group in probe_groups) {
      if (isTRUE(group$after_message_id == tr$message_id[[i]])) {
        steps[[length(steps) + 1L]] <- group
      }
    }
  }
  steps[[length(steps) + 1L]] <- .fg_player_end_step(session, tr)

  list(title = as.character(meta$topic),
       subtitle = .fg_player_subtitle(session),
       mode = mode,
       moderator = list(id = moderator$id, handle = moderator$handle,
                        color = .fg_player_moderator_color),
       agents = agents,
       steps = steps)
}

.fg_player_annotations <- function(annotations, ids) {
  if (is.null(annotations)) return(list())
  if (!is.list(annotations) || is.null(names(annotations)) ||
      any(!nzchar(names(annotations))) || anyDuplicated(names(annotations))) {
    stop("`annotations` must be a list named by participant ID.", call. = FALSE)
  }
  unknown <- setdiff(names(annotations), ids)
  if (length(unknown)) {
    stop("`annotations` names no participant of this session: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  lapply(stats::setNames(names(annotations), names(annotations)), function(id) {
    fields <- annotations[[id]]
    if (is.list(fields)) fields <- unlist(fields)
    if (!is.atomic(fields) || !length(fields) || is.null(names(fields)) ||
        any(!nzchar(names(fields)))) {
      stop("`annotations$", id, "` must be a named character vector.", call. = FALSE)
    }
    lapply(seq_along(fields), function(k) {
      list(name = names(fields)[[k]], value = as.character(fields[[k]]))
    })
  })
}

.fg_player_title_step <- function(session, n, item_verbatim, call_ons, handle_of,
                                  strip, stripped, engine_stripped) {
  meta <- session$metadata
  versions <- meta$versions %||% list()
  lines <- character()
  purpose <- .fg_player_scalar(meta$purpose)
  if (!is.na(purpose) && nzchar(purpose)) {
    lines <- c(lines, paste0("Purpose: ", purpose, if (!grepl("[.!?]$", purpose)) "."))
  }

  people <- if (n == 1L) "One participant" else paste(.fg_player_number(n), "participants")
  delivery <- if (!length(item_verbatim) || !any(item_verbatim)) {
    "a moderator whose lines the model writes from each item of the guide"
  } else if (all(item_verbatim)) {
    "a moderator who reads each item of the guide as written"
  } else {
    sprintf(paste("a moderator who reads %d of the %d guide items as written, while the",
                  "model writes the lines for the others"),
            sum(item_verbatim), length(item_verbatim))
  }
  lines <- c(lines, paste0(people, " and ", delivery, "."))
  lines <- c(lines, .fg_player_rule_sentence(meta$flow))

  # Call-ons as the transcript records them; the setting is mentioned only
  # when none took place.
  points <- if (is.list(meta$call_on_silent)) meta$call_on_silent$items
  if (nrow(call_ons)) {
    items <- unique(call_ons$item)
    parts <- vapply(items, function(k) {
      sprintf("on %s at guide item %s",
              .fg_player_and(unname(handle_of[call_ons$id[call_ons$item == k]])), k)
    }, character(1))
    lines <- c(lines, paste0("The moderator called by name ", .fg_player_and(parts), "."))
  } else if (length(points)) {
    lines <- c(lines, sprintf("Call-ons were set for guide %s %s; none took place.",
                              if (length(points) == 1L) "item" else "items",
                              .fg_player_and(as.integer(points))))
  }
  if (any(session$routing$rule %in% "addressed")) {
    lines <- c(lines, paste("When the moderator's question names one participant,",
                            "that participant answers next."))
  }
  responses <- as.integer(.fg_player_scalar(meta$max_participant_responses))
  lines <- c(lines, sprintf("%s %s per question%s.", .fg_player_number(responses),
                            if (identical(responses, 1L)) "response" else "responses",
                            if (nrow(call_ons)) ", plus the call-ons" else ""))

  model <- .fg_player_scalar(meta$model)
  provider <- .fg_player_scalar(meta$provider)
  date <- .fg_player_date(meta$started, zone = TRUE)
  fg_version <- .fg_player_scalar(versions$FocusGroup)
  llmr_version <- .fg_player_scalar(versions$LLMR)
  lines <- c(lines, paste0(
    if (is.na(model)) "Model not recorded" else
      paste0("Model ", model, if (!is.na(provider)) paste0(" (", provider, ")") else ""),
    if (is.na(date)) ", run date not recorded," else paste0(", run on ", date, ","),
    " with FocusGroup ", fg_version %|NA|% "(version not recorded)",
    if (!is.na(llmr_version)) paste0(" and LLMR ", llmr_version) else "", "."))

  branch <- meta$branch
  arm <- .fg_player_scalar(branch$arm)
  if (!is.na(arm)) {
    shared <- .fg_player_scalar(branch$prefix_messages)
    point <- .fg_player_scalar(branch$point)
    lines <- c(lines, paste0(
      "Arm \"", arm, "\": ",
      if (is.na(shared)) "its first messages are" else paste("its first", shared, "messages are"),
      " shared with the other arms, and it continues on its own ",
      if (is.na(point)) "from the branch point" else .fg_player_point_label(point), "."))
  }
  if (isFALSE(meta$complete)) {
    failure <- meta$failure %||% list()
    lines <- c(lines, paste0(
      "The session stopped during phase '", failure$phase %||% "unknown", "'",
      if (!is.null(failure$message)) paste0(" (", failure$message, ")") else "",
      "; the page shows the messages recorded before then."))
  }
  if (strip) {
    lines <- c(lines, if (stripped) {
      sprintf(paste("A speaker label the model wrote at the start of its own line is",
                    "removed here; %d %s had one."), stripped,
              if (stripped == 1L) "line" else "lines")
    } else {
      paste("A speaker label the model wrote at the start of its own line would be",
            "removed here; no line had one.")
    })
  }
  if (engine_stripped) {
    lines <- c(lines, sprintf(paste(
      "The package removed a leading speaker label from %d %s as it recorded",
      "them; the raw replies are in the calls record."), engine_stripped,
      if (engine_stripped == 1L) "reply" else "replies"))
  }
  lines <- c(lines, "Replayed from the saved record; no model is called.")
  list(kind = "title", kicker = if (!is.na(arm)) "A recorded session, one arm" else
         "A recorded session",
       heading = as.character(meta$topic), lines = as.list(lines))
}

.fg_player_subtitle <- function(session) {
  meta <- session$metadata
  responses <- as.integer(.fg_player_scalar(meta$max_participant_responses))
  parts <- c(
    paste0(.fg_player_rule_short(meta$flow), ", ",
           .fg_player_number(responses, capital = FALSE),
           if (identical(responses, 1L)) " response" else " responses", " per question"),
    as.character(.fg_player_scalar(meta$model) %|NA|% "model not recorded"),
    .fg_player_date(meta$started) %|NA|% "date not recorded",
    paste("FocusGroup", .fg_player_scalar(meta$versions$FocusGroup) %|NA|% "(version not recorded)"))
  arm <- .fg_player_scalar(meta$branch$arm)
  if (!is.na(arm)) parts <- c(parts, paste("arm", arm))
  paste(parts, collapse = "; ")
}

.fg_player_desire_rules <- c("highest_score", "threshold_lowered",
                             "highest_below_threshold", "uniform_after_failure")

# The selection panel shown before a turn, or NULL for a rule that shows
# none (round robin, and a custom flow that recorded no scores).
.fg_player_routing_step <- function(row, people, handle_of, flow) {
  rule <- row$rule[[1]]
  chosen <- row$selected_id[[1]]
  scores <- row$scores[[1]]
  base <- list(kind = "routing", pick = row$pick[[1]], rule = rule,
               message_id = row$message_id[[1]], chosen = chosen)
  trigger <- row$trigger_message_id[[1]]
  desire_note <- if (identical(flow, "desire_based")) " No desire scores were asked for this turn." else ""
  if (identical(rule, "injected")) {
    return(c(base, list(
      scale = "none", title = "An injected turn",
      entries = list(), tie = FALSE, tied = list(),
      note = paste0("This turn's text was injected as written for ", handle_of[[chosen]],
                    "; no speaker was chosen and no model was called."))))
  }
  if (rule %in% c("called_on", "addressed")) {
    where <- if (is.na(trigger)) "" else paste0(" in message #", trigger)
    text <- if (identical(rule, "called_on")) {
      paste0("Called on by name", where, ".", desire_note)
    } else {
      paste0("Named in the moderator's question", where, ".", desire_note)
    }
    return(c(base, list(
      scale = "none",
      title = if (identical(rule, "called_on")) "Called on by the moderator" else
        "Addressed by the moderator",
      entries = list(), tie = FALSE, tied = list(), note = text,
      trigger_message_id = if (is.na(trigger)) NULL else trigger)))
  }
  if (identical(rule, "rotation") || (!length(scores) && !rule %in% .fg_player_desire_rules)) {
    return(NULL)
  }
  candidates <- as.character(row$candidates[[1]])
  excluded <- row$excluded[[1]]
  excluded <- stats::setNames(as.character(excluded), names(excluded))
  chance <- identical(rule, "propensity_draw")
  entries <- lapply(people$id, function(id) {
    if (id %in% names(scores)) {
      value <- unname(scores[[id]])
      if (is.na(value)) {
        return(list(id = id, value = NULL, status = "failed", reason = "no valid score"))
      }
      return(list(id = id, value = as.numeric(value), status = "asked", reason = NULL))
    }
    reason <- if (id %in% names(excluded)) excluded[[id]] else
      if (id %in% candidates) "not scored" else "not asked"
    label <- switch(reason, previous_speaker = "spoke last; not asked",
                    score_failed = "no valid score", `not asked` = "not asked", reason)
    list(id = id, value = NULL, status = "not_asked", reason = label)
  })
  tied <- as.character(row$tied[[1]])
  tie <- identical(row$tie_break[[1]], "random") && length(tied) > 1L
  threshold <- row$threshold[[1]]
  handles <- function(ids) .fg_player_and(unname(handle_of[ids]))
  note <- if (chance) {
    "The speaker is drawn at random with these chances."
  } else if (identical(rule, "uniform_after_failure")) {
    "No valid score came back; the speaker was drawn at random among those asked."
  } else if (!any(!is.na(scores))) {
    "No valid score is recorded for this pick."
  } else {
    top <- if (tie) {
      sprintf("%s tied at the top; %s was drawn at random.", handles(tied), handle_of[[chosen]])
    } else {
      sprintf("%s has the highest score and takes the floor.", handle_of[[chosen]])
    }
    if (identical(rule, "threshold_lowered") && !is.na(threshold)) {
      top <- paste(sprintf("No score reached the usual threshold, so it was lowered to %s.",
                           format(threshold)), top)
    } else if (identical(rule, "highest_below_threshold") && !is.na(threshold)) {
      top <- paste(sprintf("No score reached the threshold of %s.", format(threshold)), top)
    }
    top
  }
  c(base, list(
    scale = if (chance) "chance" else if (rule %in% .fg_player_desire_rules) "desire" else "value",
    title = if (chance) "Who speaks next? Each participant's chance of being drawn" else
      if (rule %in% .fg_player_desire_rules) "Who speaks next? Each participant's desire to speak, 0 to 10" else
        paste("Who speaks next? Scores recorded by the flow (rule", rule, ")"),
    entries = entries, tie = tie, tied = as.list(tied), note = note))
}

# One panel per private question asked at one point: the question and every
# answer, placed after the message the probe followed.
.fg_player_probe_groups <- function(probes) {
  if (!nrow(probes)) return(list())
  key <- paste(probes$probe, probes$point, probes$after_message_id, sep = "\r")
  lapply(unique(key), function(k) {
    rows <- probes[key == k, , drop = FALSE]
    list(kind = "probe", probe = rows$probe[[1]], point = rows$point[[1]],
         point_label = .fg_player_point_label(rows$point[[1]]),
         question = rows$question[[1]],
         after_message_id = rows$after_message_id[[1]],
         answers = lapply(seq_len(nrow(rows)), function(j) {
           answer <- rows$answer[[j]]
           list(id = rows$agent_id[[j]],
                answer = if (is.na(answer)) NULL else as.character(answer))
         }))
  })
}

.fg_player_end_step <- function(session, tr) {
  people <- session$people
  counts <- vapply(people$id, function(id) sum(tr$turn & tr$speaker_id == id), integer(1))
  summary <- session$summary
  meta <- session$metadata
  text <- NULL
  note <- NULL
  if (length(summary) && !is.na(summary[[1]]) && nzchar(summary[[1]])) {
    text <- as.character(summary[[1]])
  } else if (length(summary) && is.na(summary[[1]])) {
    note <- "The final summary call failed; no summary was saved."
  } else if (isFALSE(meta$final_summary)) {
    note <- "No final summary was requested."
  } else if (isFALSE(meta$complete)) {
    note <- "The session stopped before the final summary."
  } else {
    note <- "The session saved no final summary."
  }
  list(kind = "end", heading = "End of the session",
       counts = lapply(seq_along(counts), function(i) {
         list(id = people$id[[i]], turns = unname(counts[[i]]))
       }),
       summary = text,
       summary_label = if (is.null(text)) "Final summary" else
         "Final summary, written by the model after the session; no agent read it.",
       summary_note = note)
}

# ---- The page ---------------------------------------------------------------

.fg_player_placeholder <- "__FG_PLAYER_DATA__"

.fg_player_template <- function() {
  path <- system.file("player", "template.html", package = "FocusGroup")
  if (!nzchar(path) || !file.exists(path)) {
    stop("The player template inst/player/template.html is missing from the ",
         "FocusGroup installation.", call. = FALSE)
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# The payload as JSON that is safe inside a script element: "</" is written
# "<\/" so that no "</script>" can close it, "<!--" is written with an
# escaped "<", and "://" is written ":\/\/" so that no text reads as an
# address. JSON.parse() restores every character.
.fg_player_json <- function(payload) {
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null",
                                        na = "null", digits = NA))
  json <- gsub("</", "<\\/", json, fixed = TRUE)
  json <- gsub("<!--", "\\u003c!--", json, fixed = TRUE)
  gsub("://", ":\\/\\/", json, fixed = TRUE)
}

.fg_player_html <- function(payload) {
  template <- .fg_player_template()
  at <- gregexpr(.fg_player_placeholder, template, fixed = TRUE)[[1]]
  if (length(at) != 1L || at[[1]] < 0) {
    stop("The player template must hold the data placeholder exactly once.",
         call. = FALSE)
  }
  start <- at[[1]]
  end <- start + nchar(.fg_player_placeholder)
  paste0(substr(template, 1L, start - 1L), .fg_player_json(payload),
         substr(template, end, nchar(template)), "\n")
}

.fg_player_paths <- function(file, arms, n) {
  if (n == 1L) {
    if (is.null(file)) return(tempfile("fg_player_", fileext = ".html"))
    if (dir.exists(file)) {
      stop("`file` is a directory; with one session it names the HTML file to write.",
           call. = FALSE)
    }
    if (!dir.exists(dirname(file))) {
      stop("The directory of `file` does not exist: ", dirname(file), call. = FALSE)
    }
    out <- file
    if (!is.null(arms)) names(out) <- arms
    return(out)
  }
  dir <- file %||% tempfile("fg_player_")
  if (grepl("\\.html?$", dir, ignore.case = TRUE)) {
    stop("`x` holds ", n, " arms, so `file` names a directory for one file per ",
         "arm; give a directory, or choose one arm with `arm`.", call. = FALSE)
  }
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE)) {
    stop("Cannot create the directory ", dir, ".", call. = FALSE)
  }
  names <- gsub("[^A-Za-z0-9._-]+", "_", arms)
  if (anyDuplicated(tolower(names))) {
    stop("The arm names give the same file name twice after unsafe characters ",
         "are replaced: ", paste(arms, collapse = ", "), ".", call. = FALSE)
  }
  stats::setNames(file.path(dir, paste0(names, ".html")), arms)
}

.fg_player_write <- function(html, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(html)), con)
  invisible(path)
}
