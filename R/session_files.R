# File: R/session_files.R
# Purpose: The session file: a focus_group_result written as JSON Lines by
# format(), and read back by focus_group_from_transcript().

#' Write a Session as JSON Lines
#'
#' Returns the lines of a session file: one JSON object per line, each one
#' row of a table of the result, so that `writeLines(format(result),
#' "session.fg.jsonl")` saves the session and [focus_group_from_transcript()]
#' reads it back.
#'
#' Each line has a `type`, and its other fields are the columns of the table
#' it comes from:
#' \describe{
#'   \item{`session`}{One line, first: the fields of `metadata` (topic,
#'     purpose, the turn-taking rule and its parameters, the settings, the
#'     status, the versions), with `summary` and `usage`, and `format` and
#'     `version`, which name the file format. `metadata$handles` and
#'     `metadata$guide` are written in the lines below.}
#'   \item{`participant`}{One line per row of `participants`, the moderator's
#'     included.}
#'   \item{`item`}{One line per guide item, in order: `item` (its number) and
#'     the item's `phase`, `text`, and `verbatim`, where set.}
#'   \item{`message`}{One line per row of `transcript`.}
#'   \item{`routing`, `probe`, `injection`, `call`}{One line per row of
#'     `routing`, `probes`, `injections`, and `calls`. A call line leaves out
#'     `messages`, the messages the call sent, which grow with the square of
#'     the session's length and are not needed to read or continue it.}
#' }
#'
#' The writing is canonical, so that a file read back and written again is
#' the same file. A missing value is `null`. A whole number stored as a
#' double is written with a decimal point (`160.0`) and an integer without
#' one, and every double is written with as many digits as it needs to be
#' read back exactly. A named vector, such as the desire `scores` of a pick,
#' is an object; `candidates` and `tied` are always arrays. An empty list
#' is `{}`. Times are written in UTC to the microsecond
#' (`"2026-10-24T14:30:00.25Z"`). Every character outside ASCII is written as
#' a `\\uXXXX` escape, so the file is plain ASCII.
#'
#' The file never holds a model configuration, an API key, an endpoint, or a
#' runner: `metadata` holds none, and the recorded call settings are the
#' allowlisted model parameters. [saveRDS()] of the result, by contrast,
#' saves `focus_group`, which holds the configuration (and so any literal
#' key or private endpoint in it) and the runner; set `result$focus_group <-
#' NULL` first to save the record alone. [analyze_focus_group()] and the
#' Studio rebuild the session object of such a result from its tables.
#'
#' For a result saved by FocusGroup 0.5.5 or earlier, the guide, the flow
#' parameters, and the communication styles, which it lacks, are read from
#' the fields of its `focus_group`, when it has one.
#'
#' @param x A `focus_group_result`.
#' @param ... Unused.
#' @return A character vector of JSON lines, one per row, in the order
#'   session, participants, items, messages, routing, probes, injections,
#'   calls.
#' @seealso [focus_group_from_transcript()] to read the lines back, and
#'   `vignette("architecture", package = "FocusGroup")`.
#' @examples
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                         api_key = LLMR::llm_api_key_env("GROQ_API_KEY"))
#' scripted <- function(experiments, ...) {
#'   experiments$response_text <- "Evening hours matter most to me."
#'   experiments
#' }
#' result <- run_focus_group("Library hours", cfg, n_participants = 2,
#'                           guide = list(Engagement = "Which hours matter?",
#'                                        Closing = "Close the session."),
#'                           flow = "round_robin", verbose = FALSE,
#'                           .runner = scripted)
#' path <- tempfile(fileext = ".fg.jsonl")
#' writeLines(format(result), path)
#' substr(readLines(path)[1:3], 1, 70)
#' fg <- focus_group_from_transcript(path)
#' fg$conversation_log[[3]]$text
#' @export
format.focus_group_result <- function(x, ...) {
  meta <- x$metadata %||% list()
  fg <- x$focus_group
  live <- inherits(fg, "FocusGroup")
  # A result saved by 0.5.5 or earlier lacks these; the session object, when
  # it has one, holds them as fields.
  if (!"guide" %in% names(meta) && live) meta$guide <- fg$question_script
  if (!"flow_params" %in% names(meta) && live) {
    meta$flow_params <- .fg_flow_params(fg$turn_taking_flow)
  }
  participants <- x$participants
  if (is.data.frame(participants) && !"communication_style" %in% names(participants)) {
    participants$communication_style <- vapply(as.character(participants$id), function(id) {
      agent <- if (live) fg$agents[[id]]
      as.character(agent$communication_style_instruction %||% NA_character_)[1]
    }, character(1), USE.NAMES = FALSE)
  }

  session <- c(list(type = "session", format = "fg-session", version = 1L),
               meta[setdiff(names(meta), c("guide", "handles"))])
  if (length(x$summary)) session["summary"] <- list(as.character(x$summary)[[1]])
  if (!is.null(x$usage)) session["usage"] <- list(x$usage)
  lines <- .fg_json_object(session)

  if (is.data.frame(participants)) {
    lines <- c(lines, .fg_table_lines("participant", participants,
                                      .fg_participant_columns()))
  }
  guide <- meta$guide
  for (k in seq_along(guide)) {
    item <- guide[[k]]
    lines <- c(lines, .fg_json_object(c(list(type = "item", item = k),
                                        item[setdiff(names(item), c("type", "item"))])))
  }
  lines <- c(lines,
    .fg_table_lines("message", x$transcript, .fg_transcript_data(list())),
    .fg_table_lines("routing", x$routing, .fg_routing_data(list())),
    .fg_table_lines("probe", x$probes, .fg_probes_data(list())),
    .fg_table_lines("injection", x$injections, .fg_injections_data(list())),
    .fg_table_lines("call", x$calls, .fg_calls_data(list())[
      setdiff(names(.fg_calls_data(list())), "messages")]))
  lines
}

# The columns of `participants`, as an empty table.
.fg_participant_columns <- function() {
  out <- data.frame(id = character(), handle = character(), role = character(),
                    is_moderator = logical(), persona = character(),
                    communication_style = character(), stringsAsFactors = FALSE)
  out$demographics <- I(list())
  out$survey_responses <- I(list())
  out$provider <- character()
  out$model <- character()
  out
}

# One line per row of `table`, with the columns of `empty` in its order. A
# column the table lacks, as in a result saved by an earlier version, is
# written as missing.
.fg_table_lines <- function(type, table, empty) {
  if (!is.data.frame(table) || !nrow(table)) return(character())
  columns <- names(empty)
  vapply(seq_len(nrow(table)), function(i) {
    fields <- list(type = type)
    for (column in columns) {
      value <- if (column %in% names(table)) {
        table[[column]][[i]]
      } else if (is.list(empty[[column]])) NULL else empty[[column]][NA_integer_]
      fields[column] <- list(value)
    }
    .fg_json_object(fields)
  }, character(1))
}

# ---- Canonical JSON -------------------------------------------------------------

# The fields always written as arrays, whatever their length.
.fg_json_arrays <- c("candidates", "tied", "points", "items", "failed")

# One JSON object from a named list, its fields in order.
.fg_json_object <- function(fields) {
  keys <- names(fields)
  values <- vapply(seq_along(fields), function(i) {
    .fg_json_value(fields[[i]], array = keys[[i]] %in% .fg_json_arrays,
                   field = keys[[i]])
  }, character(1))
  paste0("{", paste0(.fg_json_string(keys), ":", values, collapse = ","), "}")
}

# The JSON text of one R value. A named list or vector is an object, an
# unnamed one an array, and an unnamed vector of length one a scalar unless
# `array`; an empty list is {}. A value that has no place in a session file
# (a model configuration, a function, an environment) stops the writing.
.fg_json_value <- function(x, array = FALSE, field = "") {
  if (is.null(x)) return("null")
  if (inherits(x, "llm_config")) {
    stop("The field '", field, "' holds a model configuration, which a session ",
         "file never carries.", call. = FALSE)
  }
  if (is.function(x) || is.environment(x) || inherits(x, "R6")) {
    stop("The field '", field, "' holds a ", class(x)[1], ", which a session file ",
         "cannot carry.", call. = FALSE)
  }
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(i) {
      row <- lapply(names(x), function(column) x[[column]][[i]])
      names(row) <- names(x)
      .fg_json_object(row)
    })
    return(paste0("[", paste(unlist(rows), collapse = ","), "]"))
  }
  if (is.list(x)) {
    if (!length(x)) return("{}")
    keys <- names(x)
    if (!is.null(keys)) {
      values <- vapply(seq_along(x), function(i) {
        .fg_json_value(x[[i]], array = keys[[i]] %in% .fg_json_arrays,
                       field = paste0(field, "$", keys[[i]]))
      }, character(1))
      return(paste0("{", paste0(.fg_json_string(keys), ":", values, collapse = ","), "}"))
    }
    values <- vapply(x, .fg_json_value, character(1), field = field)
    return(paste0("[", paste(values, collapse = ","), "]"))
  }
  if (is.factor(x)) x <- as.character(x)
  if (!is.atomic(x) || is.complex(x) || is.raw(x)) {
    stop("The field '", field, "' holds a value of class ", class(x)[1],
         ", which a session file cannot carry.", call. = FALSE)
  }
  values <- .fg_json_scalars(x, field)
  keys <- names(x)
  if (!is.null(keys)) {
    if (!length(x)) return("{}")
    return(paste0("{", paste0(.fg_json_string(keys), ":", values, collapse = ","), "}"))
  }
  if (length(x) == 1L && !array) return(values)
  paste0("[", paste(values, collapse = ","), "]")
}

# The JSON text of each element of an atomic vector.
.fg_json_scalars <- function(x, field = "") {
  if (inherits(x, "POSIXct")) return(.fg_json_time(x))
  if (inherits(x, "Date")) x <- as.character(x)
  out <- rep("null", length(x))
  ok <- !is.na(x)
  if (is.logical(x)) {
    out[ok] <- ifelse(x[ok], "true", "false")
  } else if (is.integer(x)) {
    out[ok] <- as.character(x[ok])
  } else if (is.double(x)) {
    if (any(is.infinite(x))) {
      stop("The field '", field, "' holds an infinite number, which JSON cannot ",
           "write.", call. = FALSE)
    }
    out[ok] <- vapply(x[ok], .fg_json_number, character(1))
  } else if (is.character(x)) {
    out[ok] <- .fg_json_string(x[ok])
  }
  out
}

# The shortest decimal form that reads back as the same double, with a
# decimal point or an exponent, so that it reads back as a double and not
# as an integer.
.fg_json_number <- function(x) {
  for (digits in 15:17) {
    text <- sprintf(paste0("%.", digits, "g"), x)
    if (as.numeric(text) == x) break
  }
  if (!grepl("[.eE]", text)) text <- paste0(text, ".0")
  text
}

# A time as ISO 8601 text in UTC, to the microsecond, without trailing zeros.
.fg_json_time <- function(x) {
  value <- as.numeric(x)
  out <- rep("null", length(value))
  ok <- !is.na(value)
  if (!any(ok)) return(out)
  seconds <- floor(value[ok])
  micro <- round((value[ok] - seconds) * 1e6)
  carry <- micro >= 1e6
  seconds[carry] <- seconds[carry] + 1
  micro[carry] <- 0
  clock <- format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"),
                  "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  fraction <- ifelse(micro > 0, paste0(".", sub("0+$", "", sprintf("%06d", as.integer(micro)))),
                     "")
  out[ok] <- paste0("\"", clock, fraction, "Z\"")
  out
}

# JSON string literals, with every character outside printable ASCII
# escaped, so that the text is plain ASCII.
.fg_json_string <- function(x) {
  x <- enc2utf8(as.character(x))
  plain <- !grepl("[^ !#-\\[\\]-~]", x, perl = TRUE)
  out <- character(length(x))
  out[plain] <- paste0("\"", x[plain], "\"")
  for (i in which(!plain)) {
    codes <- utf8ToInt(x[[i]])
    if (anyNA(codes)) {
      stop("A string is not valid UTF-8 and cannot be written.", call. = FALSE)
    }
    chars <- character(length(codes))
    printable <- codes >= 32L & codes < 127L & codes != 34L & codes != 92L
    chars[printable] <- intToUtf8(codes[printable], multiple = TRUE)
    named <- c(`34` = "\\\"", `92` = "\\\\", `8` = "\\b", `12` = "\\f",
               `10` = "\\n", `13` = "\\r", `9` = "\\t")
    short <- !printable & as.character(codes) %in% names(named)
    chars[short] <- named[as.character(codes[short])]
    rest <- !printable & !short
    bmp <- rest & codes < 65536L
    chars[bmp] <- sprintf("\\u%04x", codes[bmp])
    astral <- which(rest & codes >= 65536L)
    if (length(astral)) {
      v <- codes[astral] - 65536L
      chars[astral] <- sprintf("\\u%04x\\u%04x", 55296L + v %/% 1024L, 56320L + v %% 1024L)
    }
    out[[i]] <- paste0("\"", paste(chars, collapse = ""), "\"")
  }
  out
}
