# File: R/transcript_import.R
# Purpose: Build a FocusGroup object from an imported (e.g. human) transcript,
# so the package's analysis methods run on conversations it did not simulate.

#' Build a FocusGroup object from an imported transcript
#'
#' Turns a transcript that the package did not generate -- a human focus group,
#' a transcript exported from other software, or a saved CSV -- into a
#' [FocusGroup] object whose `conversation_log` holds the imported turns. The
#' returned object supports the analysis methods (`analyze()`,
#' `analyze_participation_balance()`, `analyze_topics()`, `analyze_tfidf()`,
#' `analyze_readability()`, `analyze_themes()`), the plotting methods, and
#' [analyze_focus_group()]. No model call is made: importing and the
#' descriptive analyses run offline, and only the model-dependent methods
#' (`analyze_themes()`, `summarize()`) contact a provider when you call them.
#'
#' One lightweight agent is created per speaker so that speaker-aware methods
#' have a roster to report on; these agents carry a minimal placeholder persona
#' and are not meant to continue the conversation. Set `moderator_id` when the
#' transcript identifies the moderator. When it is `NULL` and `data` has no
#' `is_moderator` column, the moderator is the speaker whose id, ignoring case
#' and surrounding spaces, is exactly one of "mod", "moderator",
#' "facilitator", or "interviewer"; an id such as "Moderator 1" or "Modesto"
#' does not match, and two matching speakers are an error. When no speaker is
#' the moderator, a non-speaking "MOD" agent is added so the object is
#' complete, a message says so, and every speaker is analyzed as a
#' participant. Rows whose speaker is missing or blank are dropped with a
#' warning; rows with speaker "System" are kept in the log but excluded from
#' analyses, matching how the simulator treats its own roster message.
#'
#' @param data A data frame with one row per turn, in order.
#' @param speaker_col Character. Name of the column holding the speaker id.
#'   Default "speaker".
#' @param text_col Character. Name of the column holding the utterance text.
#'   Default "text". Missing text becomes an empty utterance, with a warning
#'   giving the number of such turns.
#' @param topic Character or `NULL`. The discussion topic, used by the
#'   model-dependent analyses and stored on the object. `NULL` falls back to
#'   "Imported transcript".
#' @param moderator_id Character or `NULL`. Exact speaker id to treat as the
#'   moderator. When `NULL`, an `is_moderator` column or the exact aliases
#'   described above are used.
#'
#' @details
#' Columns that a transcript exported by this package carries are kept when
#' `data` has them, so a saved transcript can be read back without losing its
#' record: `message_id`, `round`, `is_moderator`, `phase`, `response_id`,
#' `finish_reason`, `sent_tokens`, `rec_tokens`, `total_tokens`,
#' `duration_s`, `provider`, `model`, `timestamp`, and the list-column
#' `metadata`. A field that is absent takes the value an imported turn has
#' always had: messages are numbered in order, `round` counts the moderator's
#' turns (0 throughout when there is no moderator), `phase` is `"imported"`,
#' and the other fields are `NA`. In particular, token counts that the
#' transcript does not supply are unknown (`NA`), not zero, and importing adds
#' no model calls to the object's usage counters.
#'
#' Supplied fields are checked rather than coerced. `message_id` must hold
#' distinct positive whole numbers increasing down the rows; `round` and the
#' token counts nonnegative whole numbers; `duration_s` nonnegative numbers;
#' and `metadata` lists or `NULL`. Numbers stored as text are accepted, and
#' `NA` is allowed except in `message_id`. A value that fails these checks is
#' an error naming the column and its rows.
#'
#' An `is_moderator` column must hold `TRUE` or `FALSE` in every row, as
#' logical values or as the text "TRUE" and "FALSE"; it must give each speaker
#' a single role and mark at most one
#' speaker; it must agree with `moderator_id` when both are given. A column
#' that marks no speaker means the transcript has no moderator, and the
#' aliases are not consulted. "System" cannot be the moderator. If a
#' participant is already named "MOD", the placeholder moderator is named
#' "MOD_2", or the next free suffix.
#'
#' Timestamps may be date-times, dates, numbers of seconds since 1970-01-01
#' UTC, or ISO 8601 text such as "2026-03-01", "2026-03-01 14:30:00", or
#' "2026-03-01T14:30:00.5-05:00". Text without an offset is read as UTC, and
#' all timestamps are stored in UTC. A timestamp that cannot be read becomes
#' `NA`, with one warning giving the number of such values.
#'
#' @return A [FocusGroup] object with a populated `conversation_log`.
#' @seealso [analyze_focus_group()] to run the descriptive analyses in one
#'   call; [run_focus_group()] to simulate a discussion instead.
#' @examples
#' transcript <- data.frame(
#'   speaker = c("Chair", "Ana", "Ben", "Ana", "Chair", "Ben"),
#'   text = c(
#'     "Welcome, everyone. What do you make of the new library hours?",
#'     "They help working parents like me; evenings matter most.",
#'     "I see it differently. Mornings are now far too crowded.",
#'     "Crowded mornings still beat being locked out after work.",
#'     "Ben, say more about what changed in the mornings.",
#'     "Staff are stretched thin before noon, so lines are longer."
#'   )
#' )
#' # "Chair" is not one of the moderator aliases, so name the moderator.
#' fg <- focus_group_from_transcript(transcript, topic = "library hours",
#'                                   moderator_id = "Chair")
#' fg$analyze_participation_balance()$participation_stats
#' @export
focus_group_from_transcript <- function(data,
                                        speaker_col = "speaker",
                                        text_col = "text",
                                        topic = NULL,
                                        moderator_id = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame with one row per turn.", call. = FALSE)
  }
  if (!is.character(speaker_col) || length(speaker_col) != 1L ||
      is.na(speaker_col) || !nzchar(speaker_col)) {
    stop("`speaker_col` must name one column in `data`.", call. = FALSE)
  }
  if (!is.character(text_col) || length(text_col) != 1L ||
      is.na(text_col) || !nzchar(text_col)) {
    stop("`text_col` must name one column in `data`.", call. = FALSE)
  }
  if (!all(c(speaker_col, text_col) %in% names(data))) {
    stop("Columns '", speaker_col, "' and '", text_col,
         "' must both be present in `data`.", call. = FALSE)
  }
  if (!is.null(topic) && (!is.character(topic) || length(topic) != 1L ||
                          is.na(topic) || !nzchar(trimws(topic)))) {
    stop("`topic` must be `NULL` or a non-empty character string.", call. = FALSE)
  }
  if (!is.null(moderator_id) &&
      (!is.character(moderator_id) || length(moderator_id) != 1L ||
       is.na(moderator_id) || !nzchar(trimws(moderator_id)))) {
    stop("`moderator_id` must be `NULL` or a non-empty character string.",
         call. = FALSE)
  }

  log <- .fg_import_log(data, speaker_col, text_col, moderator_id)

  # One placeholder agent per speaker. "System" stays out of the roster: it is
  # the simulator's bookkeeping label, filtered from analyses by convention.
  speakers <- unique(vapply(log, function(m) m$speaker_id, character(1)))
  speakers <- setdiff(speakers, "System")
  if (!length(speakers)) {
    stop("The transcript contains no speakers other than 'System'.", call. = FALSE)
  }
  is_mod <- vapply(speakers, function(s) {
    any(vapply(log, function(m)
      identical(m$speaker_id, s) && isTRUE(m$is_moderator), logical(1)))
  }, logical(1))

  agents <- lapply(seq_along(speakers), function(i) {
    FGAgent$new(
      id = speakers[i],
      agent_details = list(direct_persona_description = paste0(
        "Speaker '", speakers[i], "' in an imported focus group transcript. ",
        "This placeholder stands in for the original speaker for analysis; ",
        "it carries no persona of its own.")),
      config = NULL,
      is_moderator = is_mod[i]
    )
  })
  names(agents) <- speakers

  resolved_moderator_id <- if (any(is_mod)) speakers[which(is_mod)[1]] else "MOD"
  if (!any(is_mod)) {
    suffix <- 2L
    while (resolved_moderator_id %in% speakers) {
      resolved_moderator_id <- paste0("MOD_", suffix)
      suffix <- suffix + 1L
    }
  }
  if (!resolved_moderator_id %in% names(agents)) {
    reason <- if (is.null(moderator_id) && "is_moderator" %in% names(data)) {
      "The `is_moderator` column marks no speaker as the moderator"
    } else {
      paste("No speaker id is a moderator alias (mod, moderator, facilitator,",
            "interviewer)")
    }
    shown <- paste(utils::head(speakers, 6L), collapse = ", ")
    if (length(speakers) > 6L) shown <- paste0(shown, ", ...")
    message(sprintf(paste0(
      "%s, so all %d speakers (%s) are analyzed as participants and a ",
      "non-speaking \"%s\" placeholder was added. Set `moderator_id` to name the ",
      "moderator."),
      reason, length(speakers), shown, resolved_moderator_id))
    agents[[resolved_moderator_id]] <- FGAgent$new(
      id = resolved_moderator_id,
      agent_details = list(direct_persona_description = paste(
        "Placeholder moderator for an imported transcript in which no speaker",
        "was identifiable as the moderator.")),
      config = NULL,
      is_moderator = TRUE
    )
  }

  the_topic <- topic %||% "Imported transcript"
  fg <- FocusGroup$new(
    topic = the_topic,
    purpose = paste("Analyze an imported focus group transcript on:", the_topic),
    agents = agents,
    moderator_id = resolved_moderator_id,
    turn_taking_flow = RoundRobinFlow$new(agents, resolved_moderator_id),
    # A one-entry placeholder script: the conversation already happened, so
    # nothing here is meant to run (and the empty-script message is not useful).
    question_script = list(list(phase = "generic_discussion", text = the_topic))
  )
  fg$conversation_log <- log
  fg
}

.fg_import_error <- function(column, rows) {
  stop(sprintf("Column '%s' contains invalid values at source row(s) %s.",
               column, paste(utils::head(rows, 6L), collapse = ", ")), call. = FALSE)
}

.fg_import_timestamps <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(as.numeric(as.POSIXct(x)), origin = "1970-01-01", tz = "UTC"))
  }
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = "UTC"))
  missing <- is.na(x)
  values <- rep(NA_real_, length(x))
  if (is.numeric(x) && !is.complex(x) && is.null(dim(x))) {
    values[is.finite(x)] <- x[is.finite(x)]
  } else if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    pattern <- paste0("^([0-9]{4}-[0-9]{2}-[0-9]{2})",
      "(?:[ T]([0-9]{2}:[0-9]{2}:[0-9]{2}(?:[.][0-9]+)?)",
      "(Z|[+-][0-9]{2}:?[0-9]{2})?)?$")
    for (i in which(!missing)) {
      hit <- regmatches(x[i], regexec(pattern, x[i], perl = TRUE))[[1]]
      if (!length(hit)) next
      clock <- if (nzchar(hit[3])) hit[3] else "00:00:00"
      parsed <- strptime(paste(hit[2], clock), "%Y-%m-%d %H:%M:%OS", tz = "UTC")
      if (is.na(parsed) || format(parsed, "%Y-%m-%d", tz = "UTC") != hit[2]) next
      offset <- 0
      if (nzchar(hit[4]) && hit[4] != "Z") {
        zone <- gsub(":", "", hit[4], fixed = TRUE)
        hours <- as.integer(substr(zone, 2, 3))
        minutes <- as.integer(substr(zone, 4, 5))
        if (hours > 23 || minutes > 59) next
        offset <- (hours * 60 + minutes) * 60 * if (substr(zone, 1, 1) == "-") -1 else 1
      }
      values[i] <- as.numeric(as.POSIXct(parsed)) - offset
    }
  }
  invalid <- !missing & is.na(values)
  if (any(invalid)) warning(sprintf(
    "%d nonmissing timestamp value(s) could not be parsed and were imported as NA.",
    sum(invalid)), call. = FALSE)
  as.POSIXct(values, origin = "1970-01-01", tz = "UTC")
}

.fg_import_log <- function(data, speaker_col, text_col, moderator_id) {
  if (anyNA(c(speaker_col, text_col)) ||
      !all(c(speaker_col, text_col) %in% names(data))) {
    stop("Could not find speaker and text columns.", call. = FALSE)
  }
  speakers <- as.character(data[[speaker_col]])
  keep <- !is.na(speakers) & nzchar(trimws(speakers))
  if (any(!keep)) warning(sprintf("Dropped %d row(s) with a missing or blank speaker.",
                                  sum(!keep)), call. = FALSE)
  source_rows <- which(keep)
  data <- data[keep, , drop = FALSE]
  speakers <- speakers[keep]
  n <- nrow(data)
  if (!n) stop("The transcript has no attributable turns.", call. = FALSE)
  invalid <- function(column, bad) .fg_import_error(column, source_rows[bad])
  number <- function(column, default, integer = TRUE, positive = FALSE) {
    if (!column %in% names(data)) return(rep(default, n))
    x <- data[[column]]
    if (is.factor(x)) x <- as.character(x)
    if (is.logical(x) && all(is.na(x))) x <- as.numeric(x)
    if ((!is.numeric(x) && !is.character(x)) || is.complex(x) || !is.null(dim(x))) {
      invalid(column, seq_len(n))
    }
    value <- suppressWarnings(as.numeric(x))
    bad <- !is.na(x) & (!is.finite(value) | value < if (positive) 1 else 0)
    bad <- bad | is.nan(value)
    if (integer) bad <- bad | (!is.na(value) & (value != floor(value) | value > .Machine$integer.max))
    if (positive) bad <- bad | is.na(value)
    if (any(bad)) invalid(column, which(bad))
    if (integer) as.integer(value) else value
  }
  character_field <- function(column, default = NA_character_) {
    if (!column %in% names(data)) return(rep(default, n))
    x <- data[[column]]
    if (!is.atomic(x) || !is.null(dim(x))) invalid(column, seq_len(n))
    as.character(x)
  }
  flags <- NULL
  if ("is_moderator" %in% names(data)) {
    x <- data$is_moderator
    if (is.factor(x)) x <- as.character(x)
    if (!is.logical(x) && !is.character(x)) invalid("is_moderator", seq_len(n))
    bad <- is.na(x) | !as.character(x) %in% c("TRUE", "FALSE")
    if (any(bad)) invalid("is_moderator", which(bad))
    flags <- as.character(x) == "TRUE"
    for (speaker in unique(speakers)) {
      if (length(unique(flags[speakers == speaker])) > 1L) {
        stop(sprintf("`is_moderator` assigns conflicting roles to speaker '%s'.", speaker), call. = FALSE)
      }
    }
    if (length(unique(speakers[flags])) > 1L) {
      stop("`is_moderator` must identify at most one moderator speaker.", call. = FALSE)
    }
  }
  if (!is.null(moderator_id)) {
    if (identical(moderator_id, "System")) stop("`moderator_id` cannot be 'System'.", call. = FALSE)
    if (!moderator_id %in% speakers) stop("`moderator_id` must identify a speaker in `data`.", call. = FALSE)
    expected <- speakers == moderator_id
    if (!is.null(flags) && any(flags != expected)) stop("`is_moderator` conflicts with `moderator_id`.", call. = FALSE)
    flags <- expected
  } else if (is.null(flags)) {
    flags <- tolower(trimws(speakers)) %in% c("mod", "moderator", "facilitator", "interviewer")
    if (length(unique(speakers[flags])) > 1L) {
      stop("More than one speaker matches a moderator alias; supply `moderator_id`.", call. = FALSE)
    }
  }
  if (any(flags & speakers == "System")) stop("'System' cannot be the moderator.", call. = FALSE)
  text <- as.character(data[[text_col]])
  if (anyNA(text)) {
    warning(sprintf("%d utterance(s) with missing text were imported as empty text.", sum(is.na(text))), call. = FALSE)
    text[is.na(text)] <- ""
  }
  message_id <- number("message_id", NA_integer_, positive = TRUE)
  if (!"message_id" %in% names(data)) message_id <- seq_len(n)
  bad_id <- which(duplicated(message_id) | c(FALSE, diff(message_id) <= 0))
  if (length(bad_id)) invalid("message_id", bad_id)
  if ("timestamp" %in% names(data) &&
      (!is.null(dim(data$timestamp)) ||
       (is.list(data$timestamp) && !inherits(data$timestamp, "POSIXt")))) {
    invalid("timestamp", seq_len(n))
  }
  fields <- list(message_id = message_id,
    round = if ("round" %in% names(data)) number("round", NA_integer_) else as.integer(cumsum(flags)),
    speaker_id = speakers, is_moderator = flags, text = text,
    phase = character_field("phase", "imported"),
    response_id = character_field("response_id"), finish_reason = character_field("finish_reason"),
    sent_tokens = number("sent_tokens", NA_integer_), rec_tokens = number("rec_tokens", NA_integer_),
    total_tokens = number("total_tokens", NA_integer_), duration_s = number("duration_s", NA_real_, integer = FALSE),
    provider = character_field("provider"), model = character_field("model"),
    timestamp = if ("timestamp" %in% names(data)) .fg_import_timestamps(data$timestamp) else
      as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC"))
  metadata <- if ("metadata" %in% names(data)) data$metadata else rep(list(list()), n)
  if (!is.list(metadata)) invalid("metadata", seq_len(n))
  bad <- which(!vapply(metadata, function(x) is.null(x) || is.list(x), logical(1)))
  if (length(bad)) invalid("metadata", bad)
  lapply(seq_len(n), function(i) {
    row <- lapply(fields, function(x) x[i])
    row$metadata <- metadata[[i]] %||% list()
    row
  })
}
