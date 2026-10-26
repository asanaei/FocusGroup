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
#'     `metadata$guide` are written in the lines below. A field of
#'     `metadata` the format does not define, such as a researcher's
#'     `metadata$condition`, is written with the prefix `x_`
#'     (`"x_condition"`), and the reader restores it under its name.}
#'   \item{`participant`}{One line per row of `participants`, the moderator's
#'     included.}
#'   \item{`item`}{One line per guide item, in order: `item` (its number) and
#'     the item's `phase`, `text`, and `verbatim`, where set; any other field
#'     of the item, such as a `label`, with the prefix `x_`.}
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
#' a `\uXXXX` escape, so the file is plain ASCII.
#'
#' The file holds no model configuration and no runner, and so no key or
#' endpoint from a configuration: `metadata` holds none, and the recorded
#' call settings are the allowlisted model parameters. The message of a
#' failed call is written as recorded, except that key-like values in it
#' (the values of query parameters such as `key=`, bearer tokens, and
#' strings of the form of an API key) are replaced by `<redacted>`. [saveRDS()] of the result, by contrast,
#' saves `focus_group`, which holds the configuration (and so any literal
#' key or private endpoint in it) and the runner; set `result$focus_group <-
#' NULL` first to save the record alone. [analyze_focus_group()] and the
#' Studio rebuild the session object of such a result from its tables.
#'
#' For a result saved by FocusGroup 0.5.5 or earlier, the guide, the flow
#' parameters, and the communication styles, which it lacks, are read from
#' the fields of its `focus_group`, when it has one; without one,
#' [focus_group_from_transcript()] rebuilds them from the record as its
#' help page describes.
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

  # An error message can quote a request; key-like values in it are not
  # written.
  if (is.list(meta$failure) && is.character(meta$failure$message)) {
    meta$failure$message <- .fg_redact_secrets(meta$failure$message)
  }
  if (is.data.frame(x$calls) && is.character(x$calls$error)) {
    x$calls$error <- .fg_redact_secrets(x$calls$error)
  }
  session <- c(list(type = "session", format = "fg-session", version = 1L),
               .fg_mark_extra(meta[setdiff(names(meta), c("guide", "handles"))],
                              "session"))
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
                                        .fg_mark_extra(item[setdiff(names(item),
                                                                    c("type", "item"))],
                                                       "item"))))
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

# Fields of a `type` line that the format does not define, such as a
# researcher's `metadata$condition` or a guide item's `label`, are written
# with the prefix "x_", which the reader accepts and removes.
.fg_mark_extra <- function(fields, type) {
  if (!length(fields)) return(fields)
  extra <- !names(fields) %in% names(.fg_line_kinds(type))
  names(fields)[extra] <- paste0("x_", names(fields)[extra])
  fields
}

# Text with key-like values replaced by "<redacted>": the values of query
# parameters such as `key=` and `api_key=`, bearer tokens, and strings of the
# forms of OpenAI and Google API keys.
.fg_redact_secrets <- function(x) {
  x <- gsub(paste0("(?i)([?&](?:api[_-]?key|key|token|access[_-]?token|auth|",
                   "signature|sig)=)[^&\\s\"'#]+"), "\\1<redacted>", x, perl = TRUE)
  x <- gsub("(?i)(bearer\\s+)[A-Za-z0-9._~+/=-]{8,}", "\\1<redacted>", x, perl = TRUE)
  x <- gsub("\\bsk-[A-Za-z0-9_-]{8,}", "<redacted>", x, perl = TRUE)
  gsub("\\bAIza[0-9A-Za-z_-]{20,}", "<redacted>", x, perl = TRUE)
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

# ---- Reading a session file -------------------------------------------------------

# The placeholder persona of a speaker read from a transcript that gives none.
# A session cannot continue while a speaker who would speak has it.
.fg_placeholder_persona <- function(id) {
  paste0("Speaker '", id, "' in an imported focus group transcript. ",
         "This placeholder stands in for the original speaker for analysis; ",
         "it carries no persona of its own.")
}

.fg_placeholder_moderator <- paste(
  "Placeholder moderator for an imported transcript in which no speaker",
  "was identifiable as the moderator.")

.fg_placeholder_topic <- "Imported transcript"

# The lines of a session given as a path to a .jsonl file or as JSON lines.
.fg_session_text <- function(data) {
  if (anyNA(data)) stop("The session lines must not be NA.", call. = FALSE)
  if (length(data) == 1L && !grepl("[\n{]", data)) {
    if (grepl("\\.jsonl$", data, ignore.case = TRUE)) {
      if (!file.exists(data)) {
        stop("There is no session file at '", data, "'.", call. = FALSE)
      }
      return(readLines(data, warn = FALSE, encoding = "UTF-8"))
    }
    if (file.exists(data)) {
      stop("'", data, "' is not a session file (.jsonl). Read a CSV file with ",
           "utils::read.csv() or an RDS file with readRDS(), and pass what it returns.",
           call. = FALSE)
    }
    stop("`data` is one string that is neither the path of a .jsonl file nor a ",
         "JSON line.", call. = FALSE)
  }
  unlist(strsplit(enc2utf8(data), "\n", fixed = TRUE), use.names = FALSE)
}

# Problems found in a session file, gathered so that all are reported at once.
.fg_problems <- function() {
  env <- new.env(parent = emptyenv())
  env$rows <- list()
  env$add <- function(row, field, problem) {
    env$rows[[length(env$rows) + 1L]] <- list(row = as.integer(row), field = field,
                                              problem = problem)
    invisible(NULL)
  }
  env$table <- function() {
    data.frame(row = vapply(env$rows, `[[`, integer(1), "row"),
               field = vapply(env$rows, `[[`, character(1), "field"),
               problem = vapply(env$rows, `[[`, character(1), "problem"),
               stringsAsFactors = FALSE)
  }
  env$stop_if_any <- function() {
    if (!length(env$rows)) return(invisible(NULL))
    table <- env$table()
    shown <- utils::head(table, 10L)
    where <- ifelse(is.na(shown$row), "file", paste("line", shown$row))
    field <- ifelse(nzchar(shown$field), paste0(", field '", shown$field, "'"), "")
    rlang::abort(
      c(sprintf("The session file has %d problem(s).", nrow(table)),
        stats::setNames(paste0(where, field, ": ", shown$problem), rep("x", nrow(shown))),
        if (nrow(table) > 10L) c(i = "See the `problems` field of the error for the rest.")),
      class = "focusgroup_format_error", problems = table)
  }
  env
}

# The fields of each line type and how each is read. A table's columns are
# read by their type in the result; the list columns by their content.
.fg_line_kinds <- function(type) {
  from_table <- function(empty, lists = character()) {
    kinds <- vapply(names(empty), function(column) {
      x <- empty[[column]]
      if (column %in% names(lists)) lists[[column]]
      else if (identical(column, "metadata")) "metadata"
      else if (inherits(x, "POSIXct")) "time"
      else if (is.list(x)) "object"
      else if (is.integer(x)) "int"
      else if (is.double(x)) "num"
      else if (is.logical(x)) "bool"
      else "string"
    }, character(1))
    kinds
  }
  switch(type,
    session = c(format = "string", version = "int", topic = "string", purpose = "string",
                flow = "string", message_mode = "string", n_participants = "int",
                estimated_calls = "int", provider = "string", model = "string",
                complete = "bool", failure = "strings_object",
                max_participant_responses = "int", seed = "int",
                on_desire_failure = "string", reasoning_effort = "effort",
                max_tokens = "numbers_object", call_settings = "settings_table",
                prompt_templates = "strings_object", standing_rules = "strings_object",
                disclosure = "string", versions = "strings_object", started = "time",
                branch = "branch", call_on_silent = "call_on", call_on_text = "string",
                final_summary = "bool", flow_params = "flow_params", summary = "string",
                usage = "numbers_object"),
    participant = from_table(.fg_participant_columns()),
    item = c(item = "int", phase = "string", text = "string", verbatim = "bool"),
    message = from_table(.fg_transcript_data(list())),
    routing = from_table(.fg_routing_data(list()),
                         c(candidates = "strings", tied = "strings",
                           excluded = "named_strings", scores = "named_numbers")),
    probe = from_table(.fg_probes_data(list())),
    injection = from_table(.fg_injections_data(list())),
    call = from_table(.fg_calls_data(list())[setdiff(names(.fg_calls_data(list())),
                                                     "messages")]),
    NULL)
}

.fg_line_types <- c("session", "participant", "item", "message", "routing", "probe",
                    "injection", "call")

# A JSON value read without simplification, with arrays of scalars as
# vectors, objects as named lists, an empty object or array as list(), and
# null as NA.
.fg_json_decode <- function(x) {
  if (is.null(x)) return(NA)
  if (!is.list(x)) return(x)
  if (!length(x)) return(list())
  if (!is.null(names(x))) return(lapply(x, .fg_json_decode))
  scalar <- vapply(x, function(v) is.null(v) || (is.atomic(v) && length(v) == 1L),
                   logical(1))
  if (all(scalar)) {
    present <- Filter(Negate(is.null), x)
    classes <- unique(vapply(present, function(v) class(v)[1], character(1)))
    if (length(classes) == 1L || all(classes %in% c("integer", "numeric"))) {
      type <- if (!length(classes)) "logical" else
        if (all(classes %in% c("integer", "numeric")) && length(classes) > 1L) "numeric" else
          classes[[1]]
      na <- switch(type, character = NA_character_, integer = NA_integer_,
                   numeric = NA_real_, NA)
      return(vapply(x, function(v) if (is.null(v)) na else as.vector(v, type),
                    na, USE.NAMES = FALSE))
    }
  }
  lapply(x, .fg_json_decode)
}

# The fields of message metadata that the package writes, read back with
# their types: a JSON null is NA of the field's type.
.fg_metadata_types <- function(md) {
  if (!is.list(md) || !length(md)) return(md)
  as_type <- function(value, type) {
    if (is.null(value)) return(value)
    switch(type,
      character = if (is.character(value)) value else
        if (length(value) && all(is.na(value))) rep(NA_character_, length(value)) else value,
      integer = if (is.numeric(value) && all(is.na(value) | value == round(value))) {
        as.integer(value)
      } else if (length(value) && all(is.na(value))) rep(NA_integer_, length(value)) else value,
      double = if (is.numeric(value) || (length(value) && all(is.na(value)))) {
        as.double(value)
      } else value,
      logical = if (length(value) && all(is.na(value))) as.logical(value) else value)
  }
  fix <- function(x, types) {
    if (!is.list(x)) return(x)
    for (name in intersect(names(types), names(x))) {
      x[[name]] <- as_type(x[[name]], types[[name]])
    }
    x
  }
  md <- fix(md, c(usage_unknown = "double", flow = "character", fallback = "character",
                  threshold = "double", selected_id = "character"))
  if (is.list(md[["injection"]])) {
    md[["injection"]] <- fix(md[["injection"]], c(label = "character", arm = "character",
                                        point = "character", from = "character",
                                        visibility = "character",
                                        cut_after_message_id = "integer",
                                        as_turn = "logical", replaced = "character"))
  }
  if (is.list(md[["call_on"]])) {
    md[["call_on"]] <- fix(md[["call_on"]], c(point = "character", participant = "character",
                                    handle = "character"))
  }
  if (is.list(md[["desire_scoring_failure"]])) {
    failure <- fix(md[["desire_scoring_failure"]], c(reason = "character",
                                                fallback = "character"))
    for (name in intersect(c("candidates", "failed"), names(failure))) {
      value <- failure[[name]]
      failure[[name]] <- if (is.list(value) && !length(value)) character() else
        as_type(value, "character")
    }
    md[["desire_scoring_failure"]] <- failure
  }
  if (is.list(md[["scores"]])) {
    md[["scores"]] <- lapply(md[["scores"]], function(v) as_type(v, "double"))
  }
  md
}

# Read one field of a line by its kind. Returns the value, or calls `bad`
# with the problem and returns NULL.
.fg_read_field <- function(value, kind, bad) {
  is_scalar <- function(v) is.atomic(v) && length(v) == 1L
  whole <- function(v) is.numeric(v) && is_scalar(v) && is.finite(v) && v == round(v) &&
    abs(v) <= .Machine$integer.max
  switch(kind,
    string = {
      if (is.null(value)) return(NA_character_)
      if (!is.character(value) || !is_scalar(value)) return(bad("must be a string"))
      value
    },
    int = {
      if (is.null(value)) return(NA_integer_)
      if (!whole(value)) return(bad("must be a whole number"))
      as.integer(value)
    },
    num = {
      if (is.null(value)) return(NA_real_)
      if (!is.numeric(value) || !is_scalar(value) || !is.finite(value)) {
        return(bad("must be a finite number"))
      }
      as.double(value)
    },
    bool = {
      if (is.null(value)) return(NA)
      if (!is.logical(value) || !is_scalar(value)) return(bad("must be true or false"))
      value
    },
    time = {
      if (is.null(value)) return(as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"))
      if (!(is.character(value) || is.numeric(value)) || !is_scalar(value)) {
        return(bad("must be an ISO 8601 time or a number of seconds"))
      }
      out <- suppressWarnings(.fg_import_timestamps(value))
      if (is.na(out)) return(bad("is not a time this reader can read"))
      out
    },
    strings = {
      if (is.null(value)) return(character())
      if (is.character(value) && is_scalar(value)) return(value)
      if (!is.list(value) || !is.null(names(value)) ||
          !all(vapply(value, function(v) is.character(v) && is_scalar(v), logical(1)))) {
        return(bad("must be an array of strings"))
      }
      as.character(unlist(value))
    },
    named_strings = {
      if (is.null(value) || (is.list(value) && !length(value))) return(character())
      if (!is.list(value) || is.null(names(value)) ||
          !all(vapply(value, function(v) is.character(v) && is_scalar(v), logical(1)))) {
        return(bad("must be an object of strings"))
      }
      vapply(value, identity, character(1))
    },
    named_numbers = {
      if (is.null(value)) return(NULL)
      if (!is.list(value) || is.null(names(value)) ||
          !all(vapply(value, function(v) is.null(v) || (is.numeric(v) && is_scalar(v) &&
                                                         is.finite(v)), logical(1)))) {
        return(bad("must be an object of finite numbers"))
      }
      vapply(value, function(v) if (is.null(v)) NA_real_ else as.double(v), numeric(1))
    },
    numbers_object = {
      if (is.null(value)) return(NULL)
      if (!is.list(value) || (length(value) && is.null(names(value))) ||
          !all(vapply(value, function(v) is.null(v) || (is.numeric(v) && is_scalar(v) &&
                                                         is.finite(v)), logical(1)))) {
        return(bad("must be an object of finite numbers"))
      }
      lapply(value, function(v) if (is.null(v)) NA_real_ else as.double(v))
    },
    strings_object = {
      if (is.null(value)) return(NULL)
      if (!is.list(value) || (length(value) && is.null(names(value))) ||
          !all(vapply(value, function(v) is.null(v) || (is.character(v) && is_scalar(v)),
                      logical(1)))) {
        return(bad("must be an object of strings"))
      }
      lapply(value, function(v) if (is.null(v)) NA_character_ else v)
    },
    effort = {
      if (is.null(value)) return(NULL)
      if (is.character(value) && is_scalar(value)) return(value)
      .fg_read_field(value, "strings_object", bad)
    },
    object = , metadata = {
      if (is.null(value)) return(list())
      if (!is.list(value) || (length(value) && is.null(names(value)))) {
        return(bad("must be an object"))
      }
      out <- .fg_json_decode(value)
      if (identical(kind, "metadata")) .fg_metadata_types(out) else out
    },
    branch = {
      if (is.null(value)) return(NULL)
      if (!is.list(value) || (length(value) && is.null(names(value)))) {
        return(bad("must be an object"))
      }
      out <- list()
      for (name in names(value)) {
        kind <- if (name %in% c("prefix_messages", "prefix_last_message_id", "prefix_calls")) {
          "int"
        } else if (name %in% c("arm", "role", "point", "prefix_hash")) "string" else NA
        if (is.na(kind)) return(bad(paste0("has an unknown element '", name, "'")))
        failed <- FALSE
        v <- .fg_read_field(value[[name]], kind, function(problem) {
          failed <<- TRUE
          NULL
        })
        if (failed) return(bad(paste0("element '", name, "' ",
                                      if (kind == "int") "must be a whole number" else
                                        "must be a string")))
        out[name] <- list(v)
      }
      out
    },
    call_on = {
      if (is.null(value)) return(list(points = character(), items = integer()))
      if (!is.list(value) || (length(value) && is.null(names(value))) ||
          length(setdiff(names(value), c("points", "items"))) ||
          (length(value) && !all(c("points", "items") %in% names(value)))) {
        return(bad("must be an object with `points` and `items`"))
      }
      failed <- FALSE
      points <- .fg_read_field(value[["points"]], "strings", function(problem) {
        failed <<- TRUE
        NULL
      })
      if (failed) return(bad("`points` must be an array of strings"))
      items <- value[["items"]]
      items <- if (is.null(items) || (is.list(items) && !length(items))) integer() else
        if (is.list(items) && is.null(names(items)) &&
            all(vapply(items, function(v) whole(v) && v >= 1, logical(1)))) {
          as.integer(unlist(items))
        } else if (whole(items) && items >= 1) as.integer(items) else
          return(bad("`items` must be guide item numbers"))
      if (length(points) != length(items)) {
        return(bad("must give one point for each item"))
      }
      list(points = points, items = items)
    },
    flow_params = {
      if (is.null(value) || (is.list(value) && !length(value))) return(list())
      if (!is.list(value) || is.null(names(value))) return(bad("must be an object"))
      out <- list()
      for (name in names(value)) {
        v <- value[[name]]
        out[[name]] <- switch(name,
          initial_propensities = .fg_read_field(v, "named_numbers", bad),
          recovery_increment = , min_desire_threshold = .fg_read_field(v, "num", bad),
          on_desire_failure = .fg_read_field(v, "string", bad),
          return(bad(paste0("has an unknown parameter '", name, "'"))))
      }
      out
    },
    settings_table = {
      empty <- data.frame(call_type = character(), agent_id = character(),
                          provider = character(), model = character(),
                          stringsAsFactors = FALSE)
      empty$settings <- I(list())
      if (is.null(value) || (is.list(value) && !length(value))) return(empty)
      if (!is.list(value) || !is.null(names(value)) ||
          !all(vapply(value, function(r) is.list(r) && !is.null(names(r)), logical(1)))) {
        return(bad("must be an array of objects"))
      }
      text <- function(name) vapply(value, function(r) {
        v <- r[[name]]
        if (is.character(v) && length(v) == 1L) v else NA_character_
      }, character(1))
      out <- data.frame(call_type = text("call_type"), agent_id = text("agent_id"),
                        provider = text("provider"), model = text("model"),
                        stringsAsFactors = FALSE)
      out$settings <- I(lapply(value, function(r) {
        s <- r$settings
        if (is.list(s) && length(s)) .fg_json_decode(s) else list()
      }))
      out
    },
    stop("Unknown field kind: ", kind, call. = FALSE))
}

# Parse and check the lines of a session file. Returns the lines read, by
# type, each a list of its fields with `.row`, the line number; every
# problem is reported at once.
.fg_parse_session <- function(lines) {
  problems <- .fg_problems()
  if (length(lines)) lines[[1]] <- sub("^\ufeff", "", lines[[1]])
  lines <- sub("\r$", "", lines)
  rows <- which(nzchar(trimws(lines)))
  if (!length(rows)) {
    problems$add(NA, "", "The file holds no line.")
    problems$stop_if_any()
  }
  parsed <- vector("list", length(lines))
  failed <- integer()
  for (i in rows) {
    value <- tryCatch(jsonlite::parse_json(lines[[i]], simplifyVector = FALSE),
                      error = function(e) e)
    if (inherits(value, "error")) {
      failed <- c(failed, i)
      next
    }
    if (!is.list(value) || (length(value) && is.null(names(value))) || !length(value)) {
      problems$add(i, "", "is not a JSON object with a `type`.")
      next
    }
    twice <- unique(names(value)[duplicated(names(value))])
    for (name in twice) problems$add(i, name, "is given twice.")
    parsed[[i]] <- value
  }
  if (length(failed)) {
    whole <- tryCatch(jsonlite::parse_json(paste(lines, collapse = "\n")),
                      error = function(e) NULL)
    if (length(failed) > 1L && !is.null(whole)) {
      problems$add(NA, "", paste(
        "The file is one JSON value written over", length(lines), "lines; a session",
        "file has one JSON object on each line."))
      problems$stop_if_any()
    }
    for (i in failed) {
      problems$add(i, "", paste0("is not valid JSON: ", substr(trimws(lines[[i]]), 1, 60)))
    }
  }
  out <- stats::setNames(rep(list(list()), length(.fg_line_types)), .fg_line_types)
  for (i in rows) {
    value <- parsed[[i]]
    if (is.null(value)) next
    type <- value$type
    if (!is.character(type) || length(type) != 1L || !type %in% .fg_line_types) {
      problems$add(i, "type", if (is.null(type)) "is missing." else paste0(
        "'", paste(format(type), collapse = " "), "' is not a line type; the types are ",
        paste(.fg_line_types, collapse = ", "), "."))
      next
    }
    kinds <- .fg_line_kinds(type)
    fields <- value[setdiff(names(value), "type")]
    unknown <- setdiff(names(fields), names(kinds))
    extra <- unknown[startsWith(unknown, "x_")]
    unknown <- unknown[!startsWith(unknown, "x_")]
    for (name in unknown) {
      problems$add(i, name, paste0("is not a field of a ", type, " line."))
    }
    record <- list()
    # A field the format does not define, written with the prefix "x_", is
    # kept under its name, or under the prefixed name when that would be a
    # field of the line.
    if (length(extra)) {
      restored <- sub("^x_", "", extra)
      restored[restored %in% names(kinds) | !nzchar(restored)] <-
        extra[restored %in% names(kinds) | !nzchar(restored)]
      record$.extra <- stats::setNames(lapply(fields[extra], .fg_json_decode), restored)
    }
    for (name in intersect(names(kinds), names(fields))) {
      bad <- FALSE
      value <- .fg_read_field(fields[[name]], kinds[[name]], function(problem) {
        bad <<- TRUE
        problems$add(i, name, paste0(problem, "."))
        NULL
      })
      if (!bad) record[name] <- list(value)
    }
    record$.row <- i
    out[[type]][[length(out[[type]]) + 1L]] <- record
  }

  # Required fields and their values.
  need <- function(type, field, ok = function(v) TRUE, what = "is required.") {
    for (r in out[[type]]) {
      v <- r[[field]]
      if (is.null(v) || (is.atomic(v) && length(v) == 1L && is.na(v))) {
        problems$add(r$.row, field, "is required.")
      } else if (!ok(v)) problems$add(r$.row, field, what)
    }
  }
  need("message", "speaker_id", function(v) nzchar(trimws(v)), "must not be empty.")
  need("message", "text")
  need("participant", "id", function(v) nzchar(trimws(v)), "must not be empty.")
  need("item", "item", function(v) v >= 1L, "must be 1 or more.")
  need("item", "phase", function(v) nzchar(v), "must not be empty.")
  nonnegative <- function(type, fields) {
    for (r in out[[type]]) for (field in intersect(fields, names(r))) {
      v <- r[[field]]
      if (is.numeric(v) && length(v) == 1L && !is.na(v) && v < 0) {
        problems$add(r$.row, field, "must not be negative.")
      }
    }
  }
  nonnegative("message", c("round", "item", "sent_tokens", "rec_tokens", "total_tokens",
                           "duration_s"))
  for (r in out$message) {
    if (isTRUE(r$message_id < 1L)) problems$add(r$.row, "message_id", "must be 1 or more.")
  }
  if (length(out$session) > 1L) {
    for (r in out$session[-1]) problems$add(r$.row, "type", "is a second session line.")
  }
  version <- if (length(out$session)) out$session[[1]][["version"]] else NA
  if (isTRUE(version > 1L)) {
    problems$add(out$session[[1]]$.row, "version", paste0(
      "is ", version, "; this version of FocusGroup reads version 1."))
  }
  mode <- if (length(out$session)) out$session[[1]][["message_mode"]] else NA
  if (!is.null(mode) && !is.na(mode) && !mode %in% c("roleflip", "flat")) {
    problems$add(out$session[[1]]$.row, "message_mode",
                 "must be \"roleflip\" or \"flat\".")
  }
  if (!length(out$message)) problems$add(NA, "", "The file has no message line.")

  # Message ids: all given, distinct, and increasing; or none given.
  ids <- vapply(out$message, function(r) r$message_id %||% NA_integer_, integer(1))
  if (any(!is.na(ids)) && anyNA(ids)) {
    for (r in out$message[is.na(ids)]) {
      problems$add(r$.row, "message_id", "is missing, while other messages give one.")
    }
  }
  given_ids <- which(!is.na(ids))
  for (j in given_ids[-1][diff(ids[given_ids]) <= 0L]) {
    before <- ids[given_ids][match(j, given_ids) - 1L]
    problems$add(out$message[[j]]$.row, "message_id", sprintf(
      "%d does not follow %d; message ids must increase.", ids[[j]], before))
  }

  # The roster: distinct ids and at most one moderator.
  pids <- vapply(out$participant, function(r) r$id %||% NA_character_, character(1))
  for (j in which(duplicated(pids) & !is.na(pids))) {
    problems$add(out$participant[[j]]$.row, "id", paste0("'", pids[[j]], "' is declared twice."))
  }
  for (j in which(pids %in% "System")) {
    problems$add(out$participant[[j]]$.row, "id", "'System' labels the roster row.")
  }
  moderators <- Filter(function(r) isTRUE(r$is_moderator) || identical(r$role, "moderator"),
                       out$participant)
  if (length(moderators) > 1L) {
    for (r in moderators[-1]) {
      problems$add(r$.row, "is_moderator", paste0(
        "'", r$id, "' is a second moderator; a session has one."))
    }
  }
  # Every speaker is declared, when the roster is, except the roster row and
  # injected voices.
  if (length(out$participant)) {
    for (r in out$message) {
      speaker <- r$speaker_id
      if (is.null(speaker) || is.na(speaker) || speaker %in% c(pids, "System")) next
      md <- r$metadata %||% list()
      if (isTRUE(md$injected) && !isTRUE(md$as_turn)) next
      problems$add(r$.row, "speaker_id", paste0(
        "'", speaker, "' is not declared in a participant line."))
    }
  }
  # The guide: items 1 to K, each once.
  items <- vapply(out$item, function(r) r$item %||% NA_integer_, integer(1))
  for (j in which(duplicated(items) & !is.na(items))) {
    problems$add(out$item[[j]]$.row, "item", paste0("item ", items[[j]], " is given twice."))
  }
  if (length(items) && !anyNA(items)) {
    gaps <- setdiff(seq_len(max(items)), items)
    if (length(gaps)) {
      problems$add(out$item[[1]]$.row, "item", paste0(
        "the guide has no item ", paste(gaps, collapse = ", "), "."))
    }
    for (r in out$message) {
      if (isTRUE(r$item > max(items))) {
        problems$add(r$.row, "item", sprintf("%d is not an item of the %d-item guide.",
                                             r$item, max(items)))
      }
    }
  }
  problems$stop_if_any()
  out
}

# The personas given to the reader: NULL, or strings named by speaker.
.fg_check_personas <- function(personas) {
  if (is.null(personas)) return(NULL)
  if (is.list(personas) && all(vapply(personas, function(p) is.character(p) &&
                                        length(p) == 1L, logical(1)))) {
    personas <- unlist(personas)
  }
  if (!is.character(personas) || !length(personas) || anyNA(personas) ||
      any(!nzchar(trimws(personas))) || is.null(names(personas)) ||
      anyNA(names(personas)) || any(!nzchar(names(personas))) ||
      anyDuplicated(names(personas))) {
    stop("`personas` must be `NULL` or non-empty strings named by speaker, one per speaker.",
         call. = FALSE)
  }
  personas
}

# Give speakers the personas supplied for them.
.fg_apply_personas <- function(fg, personas) {
  if (is.null(personas)) return(fg)
  unknown <- setdiff(names(personas), names(fg$agents))
  if (length(unknown)) {
    stop("`personas` names speakers who are not in the session: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  for (id in names(personas)) fg$agents[[id]]$persona_description <- unname(personas[[id]])
  fg
}

# A fresh FocusGroup from the lines of a session file, parsed and checked.
# Without participant lines the speakers are the roster; without item lines
# each moderator line is an item of the guide, delivered as written. A file
# that records a run keeps its settings and records; one that does not is a
# transcript, read with the defaults described in
# focus_group_from_transcript().
.fg_build_session <- function(parsed, topic = NULL, moderator_id = NULL, personas = NULL) {
  problems <- .fg_problems()
  session <- if (length(parsed$session)) parsed$session[[1]] else list()
  given <- function(name) {
    v <- session[[name]]
    !is.null(v) && !(is.atomic(v) && length(v) == 1L && is.na(v))
  }
  msgs <- parsed$message
  n <- length(msgs)
  speaker <- vapply(msgs, function(r) r$speaker_id, character(1))
  md <- lapply(msgs, function(r) r$metadata %||% list())
  flagged <- vapply(msgs, function(r) isTRUE(r$is_moderator), logical(1))
  as_turn <- vapply(md, function(m) isTRUE(m$as_turn), logical(1))
  voice <- vapply(md, function(m) isTRUE(m$injected), logical(1)) & !as_turn & !flagged

  # The roster and its moderator.
  declared <- length(parsed$participant) > 0L
  aliases <- c("mod", "moderator", "facilitator", "interviewer")
  if (declared) {
    roster <- parsed$participant
    ids <- vapply(roster, function(r) r$id, character(1))
    is_mod <- vapply(roster, function(r) isTRUE(r$is_moderator) ||
                       identical(r$role, "moderator"), logical(1))
    mod <- if (any(is_mod)) ids[is_mod][[1]] else NA_character_
    if (is.na(mod) && is.null(moderator_id)) {
      # A roster that marks no moderator: the messages marked as the
      # moderator's, or a speaker named as one.
      marked <- unique(speaker[flagged])
      found <- if (length(marked) == 1L) marked else ids[tolower(trimws(ids)) %in% aliases]
      if (length(found) > 1L) {
        problems$add(NA, "", paste("More than one speaker matches a moderator alias;",
                                   "supply `moderator_id`."))
      }
      if (length(found)) mod <- found[[1]]
    }
    if (!is.null(moderator_id)) {
      if (!is.na(mod) && !identical(mod, moderator_id)) {
        problems$add(roster[[which(is_mod)[1]]]$.row, "is_moderator", paste0(
          "declares '", mod, "' the moderator, but `moderator_id` is '", moderator_id, "'."))
      } else if (!moderator_id %in% ids) {
        problems$add(NA, "", paste0("`moderator_id` '", moderator_id,
                                    "' is not declared in a participant line."))
      }
      mod <- moderator_id
    }
  } else {
    roster <- list()
    ids <- unique(speaker[!voice & speaker != "System"])
    marked <- unique(speaker[flagged])
    if (length(marked) > 1L) {
      for (j in which(flagged & speaker != marked[[1]])) {
        problems$add(msgs[[j]]$.row, "is_moderator", paste0(
          "marks '", speaker[[j]], "' as the moderator, but '", marked[[1]],
          "' is marked too; a session has one moderator."))
      }
    }
    mod <- if (!is.null(moderator_id)) {
      if (!moderator_id %in% ids) {
        problems$add(NA, "", paste0("`moderator_id` '", moderator_id,
                                    "' speaks no message."))
      }
      if (length(marked) && !identical(marked[[1]], moderator_id)) {
        problems$add(NA, "", paste0("`moderator_id` '", moderator_id, "' conflicts with ",
                                    "the messages marked as the moderator's."))
      }
      moderator_id
    } else if (length(marked)) {
      marked[[1]]
    } else {
      found <- ids[tolower(trimws(ids)) %in% aliases]
      if (length(found) > 1L) {
        problems$add(NA, "", paste("More than one speaker matches a moderator alias;",
                                   "supply `moderator_id`."))
      }
      if (length(found)) found[[1]] else NA_character_
    }
  }
  if (identical(mod, "System")) problems$add(NA, "", "'System' cannot be the moderator.")
  problems$stop_if_any()
  placeholder <- is.na(mod)
  if (placeholder) {
    mod <- "MOD"
    suffix <- 2L
    while (mod %in% ids) {
      mod <- paste0("MOD_", suffix)
      suffix <- suffix + 1L
    }
    shown <- paste(utils::head(ids, 6L), collapse = ", ")
    if (length(ids) > 6L) shown <- paste0(shown, ", ...")
    message(sprintf(paste0(
      "No speaker is marked or named as the moderator, so all %d speakers (%s) are ",
      "read as participants and a non-speaking \"%s\" placeholder was added. Set ",
      "`moderator_id` to name the moderator."), length(ids), shown, mod))
  }
  for (j in which(flagged & speaker != mod)) {
    problems$add(msgs[[j]]$.row, "is_moderator", paste0(
      "marks a message by '", speaker[[j]], "', who is not the moderator, as the moderator's."))
  }
  unflagged <- vapply(msgs, function(r) identical(r$is_moderator, FALSE), logical(1))
  for (j in which(unflagged & speaker == mod)) {
    problems$add(msgs[[j]]$.row, "is_moderator", paste0(
      "marks a message by the moderator '", mod, "' as not the moderator's."))
  }
  participant_ids <- setdiff(ids, mod)
  for (j in which(as_turn & !speaker %in% participant_ids)) {
    problems$add(msgs[[j]]$.row, "metadata", paste0(
      "marks an injected turn by '", speaker[[j]], "', who is not a participant."))
  }
  problems$stop_if_any()

  # Items and rounds: a new item at each moderator line when none are given.
  is_mod_msg <- speaker == mod
  guide_line <- is_mod_msg & !vapply(md, function(m) isTRUE(m$injected) ||
                                       !is.null(m$call_on), logical(1))
  item <- vapply(msgs, function(r) r$item %||% NA_integer_, integer(1))
  round <- vapply(msgs, function(r) r$round %||% NA_integer_, integer(1))
  has_item <- vapply(msgs, function(r) "item" %in% names(r), logical(1))
  if (!any(has_item)) {
    item <- as.integer(cumsum(guide_line))
    if (!any(guide_line)) item <- rep(1L, n)
    item[speaker == "System"] <- 0L
  }
  # A message without an item, among messages with one, belongs to the item
  # before it.
  for (j in which(is.na(item))) item[j] <- if (j == 1L) 0L else item[j - 1L]
  item[speaker == "System" & !has_item] <- 0L
  has_round <- vapply(msgs, function(r) "round" %in% names(r), logical(1))
  if (!any(has_round)) round <- item
  for (j in which(is.na(round))) round[j] <- if (j == 1L) 0L else round[j - 1L]

  # The guide: the item lines, or one verbatim item for each moderator line.
  # The phase of an item is that of its moderator line, as a recorded run
  # gives it; a transcript without phases has them inferred. The record of
  # a run made by FocusGroup 0.5.5 or earlier has no item lines, and the
  # guide texts of its generated moderator lines are unknown: those items
  # are delivered as recorded, with a warning.
  the_topic <- topic %||% (if (given("topic")) session$topic) %||% .fg_placeholder_topic
  recorded_run <- length(parsed$session) > 0L &&
    any(c("versions", "started") %in% names(session))
  guide <- lapply(parsed$item[order(vapply(parsed$item, function(r) r$item, integer(1)))],
                  function(r) c(r[setdiff(names(r), c("item", ".row", ".extra"))], r$.extra))
  if (!length(guide)) {
    k_max <- max(c(0L, item), na.rm = TRUE)
    turn <- speaker %in% participant_ids
    own_phase <- vapply(msgs, function(r) {
      p <- r$phase
      if (is.character(p) && length(p) == 1L && !is.na(p) &&
          p %in% .fg_guide_phases) p else NA_character_
    }, character(1))
    unknown_text <- integer()
    guide <- lapply(seq_len(k_max), function(k) {
      line <- which(guide_line & item %in% k)
      replied <- any(turn & item %in% k)
      phase <- if (replied) "generic_discussion" else if (k == k_max) "closing" else
        "transition"
      if (length(line)) {
        j <- line[[1]]
        if (!is.na(own_phase[[j]])) {
          phase <- own_phase[[j]]
          if (recorded_run && !isTRUE(md[[j]]$verbatim)) unknown_text <<- c(unknown_text, k)
        }
        list(phase = phase, text = msgs[[j]]$text, verbatim = TRUE)
      } else list(phase = phase)
    })
    if (!length(guide)) guide <- list(list(phase = "generic_discussion", text = the_topic))
    if (length(unknown_text)) {
      warning(structure(class = c("focusgroup_guide_rebuilt", "warning", "condition"),
        list(message = sprintf(paste(
          "The record has no guide (files written by FocusGroup 0.5.5 and earlier",
          "keep none), so guide item(s) %s are rebuilt from the moderator's recorded",
          "lines, whose guide texts are unknown, and marked verbatim: a continuation",
          "delivers those moderator lines as recorded instead of generating them."),
          paste(unknown_text, collapse = ", ")), call = NULL, items = unknown_text)))
    }
  }
  for (j in which(item > length(guide))) {
    problems$add(msgs[[j]]$.row, "item", sprintf(
      "is %d, beyond the %d item(s) of the guide; give each message its item.",
      item[[j]], length(guide)))
  }
  beyond <- setdiff(session$call_on_silent$items, seq_along(guide))
  if (length(beyond)) {
    problems$add(session$.row, "call_on_silent", paste0(
      "names item(s) ", paste(beyond, collapse = ", "), " beyond the ", length(guide),
      " item(s) of the guide."))
  }
  problems$stop_if_any()

  # The log.
  handle_of <- stats::setNames(ids, ids)
  for (r in roster) if (!is.null(r$handle) && !is.na(r$handle)) handle_of[[r$id]] <- r$handle
  if (!declared) {
    for (j in seq_len(n)) {
      h <- msgs[[j]]$handle
      if (!is.null(h) && !is.na(h) && speaker[[j]] %in% ids &&
          identical(handle_of[[speaker[[j]]]], speaker[[j]])) {
        handle_of[[speaker[[j]]]] <- h
      }
    }
  }
  ids_given <- vapply(msgs, function(r) r$message_id %||% NA_integer_, integer(1))
  if (anyNA(ids_given)) ids_given <- seq_len(n)
  field <- function(r, name, na) {
    v <- r[[name]]
    if (is.null(v)) na else v
  }
  log <- lapply(seq_len(n), function(j) {
    r <- msgs[[j]]
    s <- speaker[[j]]
    handle <- r$handle
    if (is.null(handle) || is.na(handle)) handle <- if (s %in% ids) unname(handle_of[[s]]) else s
    list(message_id = ids_given[[j]], round = round[[j]], item = item[[j]],
         speaker_id = s, handle = handle,
         is_moderator = if (is.na(field(r, "is_moderator", NA))) s == mod else r$is_moderator,
         text = r$text,
         timestamp = field(r, "timestamp", as.POSIXct(NA_real_, origin = "1970-01-01",
                                                      tz = "UTC")),
         phase = if ("phase" %in% names(r)) r$phase else "imported",
         response_id = field(r, "response_id", NA_character_),
         finish_reason = field(r, "finish_reason", NA_character_),
         sent_tokens = field(r, "sent_tokens", NA_integer_),
         rec_tokens = field(r, "rec_tokens", NA_integer_),
         total_tokens = field(r, "total_tokens", NA_integer_),
         duration_s = field(r, "duration_s", NA_real_),
         provider = field(r, "provider", NA_character_),
         model = field(r, "model", NA_character_),
         metadata = md[[j]])
  })

  # The agents, with their personas and styles as recorded.
  line_of <- stats::setNames(roster, ids[seq_along(roster)])
  agents <- lapply(c(participant_ids, mod)[order(match(c(participant_ids, mod),
                                                       c(ids, mod)))], function(id) {
    r <- line_of[[id]] %||% list()
    is_moderator <- identical(id, mod)
    persona <- (if (id %in% names(personas)) personas[[id]]) %||% (if (!is.null(r$persona) && !is.na(r$persona) &&
                                       nzchar(trimws(r$persona))) r$persona) %||%
      (if (placeholder && is_moderator) .fg_placeholder_moderator else
        .fg_placeholder_persona(id))
    agent <- FGAgent$new(id = id, agent_details = list(
      direct_persona_description = persona,
      demographics = r$demographics %||% list(),
      survey_responses = r$survey_responses %||% list()),
      config = NULL, is_moderator = is_moderator)
    agent$persona_description <- unname(persona)
    style <- r$communication_style
    if (!is.null(style) && !is.na(style)) agent$communication_style_instruction <- style
    if (!is.null(r$role) && !is.na(r$role)) agent$role <- r$role
    agent
  })
  names(agents) <- vapply(agents, function(a) a$id, character(1))
  unknown <- setdiff(names(personas), names(agents))
  if (length(unknown)) {
    stop("`personas` names speakers who are not in the session: ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }

  # The settings: as recorded, or the defaults for a transcript.
  flow_name <- if (given("flow")) session$flow else "round_robin"
  flow_params <- session$flow_params %||% list()
  # A session line written by FocusGroup 0.5.5 or earlier has no
  # `flow_params` and gives the failure rule of "desire_based" on its own.
  if (!"flow_params" %in% names(session) && identical(flow_name, "desire_based") &&
      given("on_desire_failure")) {
    flow_params$on_desire_failure <- session$on_desire_failure
  }
  built_in <- flow_name %in% c("round_robin", "probabilistic", "desire_based")
  flow <- tryCatch(
    if (built_in) {
      create_conversation_flow(flow_name, agents, mod, flow_params = flow_params)
    } else RoundRobinFlow$new(agents, mod),
    error = function(e) {
      problems$add(NA, "flow_params", conditionMessage(e))
      NULL
    })
  problems$stop_if_any()
  longest <- 0L
  run <- 0L
  for (j in seq_len(n)) {
    if (is_mod_msg[[j]]) run <- 0L else if (speaker[[j]] %in% participant_ids) {
      run <- run + 1L
      longest <- max(longest, run)
    }
  }
  limits <- Filter(function(v) !is.na(v), session$max_tokens %||% list())
  fg <- tryCatch(withCallingHandlers(
    FocusGroup$new(
      topic = the_topic,
      purpose = if (given("purpose")) session$purpose else
        paste("Analyze an imported focus group transcript on:", the_topic),
      agents = agents, moderator_id = mod, turn_taking_flow = flow,
      question_script = guide,
      prompt_templates = session$prompt_templates %||% list(),
      max_tokens_config = limits,
      max_participant_responses = if (given("max_participant_responses")) {
        session$max_participant_responses
      } else max(1L, longest),
      reasoning_effort = session$reasoning_effort,
      handles = if (all(handle_of == names(handle_of))) "ids" else handle_of,
      call_on_silent = "none",
      call_on_text = if (given("call_on_text")) session$call_on_text),
    focusgroup_handles_ids = function(m) invokeRestart("muffleMessage")),
    error = function(e) {
      problems$add(NA, "", conditionMessage(e))
      NULL
    })
  problems$stop_if_any()
  if (!is.null(session$call_on_silent)) fg$call_on_silent <- session$call_on_silent
  fg$conversation_log <- log
  fg$message_mode <- if (given("message_mode")) session$message_mode
  if ("summary" %in% names(session)) fg$final_summary <- session$summary
  if (given("complete") || !is.null(session$failure)) {
    attr(fg, "session_status") <- list(complete = session$complete %||% NA,
                                       failure = session$failure)
  }
  usage <- session$usage
  if (is.list(usage)) {
    fg$total_tokens_sent <- usage$sent %||% NA_real_
    fg$total_tokens_received <- usage$rec %||% NA_real_
    fg$total_tokens <- usage$total %||% NA_real_
    fg$calls_usage_unknown <- usage$unknown %||% NA_real_
  }
  strip <- function(records) lapply(records, function(r) r[setdiff(names(r), c(".row", ".extra"))])
  fg$records$routing <- strip(parsed$routing)
  fg$records$probes <- strip(parsed$probe)
  fg$records$injections <- strip(parsed$injection)
  fg$records$calls <- strip(parsed$call)
  # What the file records beyond the session object: the provenance of a
  # run, or, for a transcript that records none, the settings chosen for it.
  run <- recorded_run
  branch <- session$branch
  line_field <- function(id, name) {
    v <- line_of[[id]][[name]]
    if (is.null(v)) NA_character_ else v
  }
  info <- list(
    started = session$started %||% as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"),
    versions = session$versions,
    message_mode = fg$message_mode,
    standing_rules = session$standing_rules,
    extra = session$.extra,
    disclosure = session$disclosure %||% NA_character_,
    final_summary = session$final_summary %||% TRUE,
    branch = if (!is.null(branch)) branch[setdiff(names(branch), c("arm", "role"))],
    recorded = list(
      flow = flow_name,
      n_participants = session$n_participants %||% length(participant_ids),
      estimated_calls = session$estimated_calls %||% NA_integer_,
      seed = session$seed %||% NA_integer_,
      on_desire_failure = session$on_desire_failure %||% NA_character_,
      provider = session$provider %||% NA_character_,
      model = session$model %||% NA_character_,
      call_settings = session$call_settings,
      branch = branch,
      participants = data.frame(
        id = ids,
        provider = vapply(ids, line_field, character(1), name = "provider", USE.NAMES = FALSE),
        model = vapply(ids, line_field, character(1), name = "model", USE.NAMES = FALSE),
        stringsAsFactors = FALSE)))
  if (!run) {
    info <- c(list(source = "transcript"),
              info[setdiff(names(info), .fg_run_fields)])
  }
  fg$records$info <- info
  fg
}

# The session object of a result saved without one, for analysis: rebuilt
# from its tables, without the warning about continuing a record that has
# no guide, which analysis does not do.
.fg_rebuilt_for_analysis <- function(result) {
  withCallingHandlers(focus_group_from_transcript(result),
                      focusgroup_guide_rebuilt = function(w) invokeRestart("muffleWarning"))
}

# The metadata that describe a run: absent from a transcript.
.fg_run_fields <- c("started", "versions", "standing_rules", "disclosure")

# A FocusGroup as a focus_group_result, for writing. The settings and
# provenance a session read from a file keeps in `records$info$recorded`
# stand in for those its agents' configurations would give. An object
# imported from a data frame is written as a transcript: its topic and
# purpose, roster, and messages; one read from a transcript file keeps the
# settings it was read with.
.fg_object_result <- function(fg) {
  info <- fg$records$info %||% list()
  rec <- info$recorded %||% list()
  participant_ids <- setdiff(names(fg$agents), fg$moderator_id)
  flow_params <- .fg_flow_params(fg$turn_taking_flow)
  result <- .fg_focus_group_result(
    fg, rec$flow %||% .fg_flow_name(fg$turn_taking_flow),
    fg$message_mode %||% NA_character_,
    rec$n_participants %||% length(participant_ids),
    rec$estimated_calls %||% NA_integer_,
    run_meta = list(seed = rec$seed,
                    on_desire_failure = rec$on_desire_failure %||%
                      flow_params$on_desire_failure,
                    branch = rec$branch))
  # The result is for writing: its tables are the record.
  result["focus_group"] <- list(NULL)
  if (!length(info)) {
    result$metadata <- result$metadata[c("topic", "purpose")]
    return(result)
  }
  # A transcript read from a file records no run.
  if (identical(info$source, "transcript")) {
    result$metadata <- result$metadata[setdiff(names(result$metadata), .fg_run_fields)]
  }
  unconfigured <- vapply(fg$agents, function(a) is.null(a$config), logical(1))
  if (all(unconfigured) && length(rec)) {
    result$metadata$provider <- rec$provider %||% NA_character_
    result$metadata$model <- rec$model %||% NA_character_
    if (!is.null(rec$call_settings)) result$metadata$call_settings <- rec$call_settings
  }
  if (is.data.frame(rec$participants)) {
    at <- match(result$participants$id, rec$participants$id)
    keep <- unconfigured[result$participants$id] & !is.na(at)
    result$participants$provider[keep] <- rec$participants$provider[at[keep]]
    result$participants$model[keep] <- rec$participants$model[at[keep]]
  }
  # Metadata fields the format does not define, as the file gave them.
  for (name in setdiff(names(info$extra), names(result$metadata))) {
    result$metadata[name] <- list(info$extra[[name]])
  }
  result
}
