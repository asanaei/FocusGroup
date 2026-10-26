# File: R/interventions.R
# Purpose: Injected messages, private probes, and sessions branched from one
# shared prefix.

#' Points in a Session
#'
#' A point names a moment in a session at which a message can be injected or
#' a probe asked. `fg_after_item(k)` is the moment just after the moderator's
#' line for item `k` of the guide, before any participant answers it.
#' `fg_before_turn(t)` is the moment just before the `t`-th participant turn
#' of the session, counting from 1 across all questions, before the speaker
#' of that turn is chosen. A probe may also be asked at `"start"`, before the
#' moderator's first line, or at `"end"`, after the last message, the
#' closing line included, and before the final summary. A probe or an
#' injection may be placed at `"before_closing"`, after the last discussion
#' item and the call-ons that follow it and before the closing line; in a
#' guide without a closing item it comes just before `"end"`.
#'
#' Guide items are numbered in the order the session runs them: with
#' `guide = c(Opening = 1, Icebreaker = 1, Engagement = 2, Exploration = 3,
#' Closing = 1)`, item 4 is the second engagement question.
#'
#' Participant turns count every reply, those that answer the moderator's
#' call-on lines among them (see `call_on_silent` in [run_focus_group()]).
#' When `fg_before_turn(t)` falls on the reply to a call-on line, it comes
#' after that line. A custom turn-taking flow can end the discussion of an
#' item by choosing no participant (`NULL`) or the moderator; that choice is
#' not a turn and has no routing record. The point has then already come, so
#' when call-ons follow, `fg_before_turn(t)` falls before the call-on line
#' whose reply is turn `t`.
#'
#' @param k One positive whole number, the guide item.
#' @param t One positive whole number, the participant turn.
#' @return An object of class `fg_point`.
#' @seealso [fg_injection()], [fg_probe()], [run_focus_group_arms()].
#' @examples
#' fg_after_item(4)
#' fg_before_turn(7)
#' @export
fg_after_item <- function(k) {
  .fg_point("after_item", .fg_positive_whole(k, "k"))
}

#' @rdname fg_after_item
#' @export
fg_before_turn <- function(t) {
  .fg_point("before_turn", .fg_positive_whole(t, "t"))
}

.fg_point <- function(type, value) {
  structure(list(type = type, value = as.integer(value)), class = "fg_point")
}

.fg_positive_whole <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < 1 || x != floor(x) || x > .Machine$integer.max) {
    stop("`", name, "` must be one positive whole number.", call. = FALSE)
  }
  as.integer(x)
}

#' @export
format.fg_point <- function(x, ...) {
  if (x$type %in% .fg_named_points) x$type else
    sprintf("fg_%s(%d)", x$type, x$value)
}

#' @export
print.fg_point <- function(x, ...) {
  cat("<fg_point> ", format(x), "\n", sep = "")
  invisible(x)
}

# The points named by a word rather than a number.
.fg_named_points <- c("start", "before_closing", "end")

# A point as given to a probe or an injection: an fg_point, or one of the
# named points a probe ("start", "before_closing", "end") or an injection
# ("before_closing") accepts.
.fg_as_point <- function(at, allow_ends = FALSE, what = "`at`") {
  if (inherits(at, "fg_point")) return(at)
  allowed <- if (allow_ends) .fg_named_points else "before_closing"
  if (is.character(at) && length(at) == 1L && !is.na(at) && at %in% allowed) {
    return(.fg_point(at, NA_integer_))
  }
  stop(what, " must be ", paste0("\"", allowed, "\", ", collapse = ""),
       "fg_after_item(k), or fg_before_turn(t).", call. = FALSE)
}

# The checkpoint a point refers to inside the session engine.
.fg_point_event <- function(point) {
  if (point$type %in% .fg_named_points) point$type else
    paste0(point$type, ":", point$value)
}

# The label of a checkpoint, as a user would write the point.
.fg_event_label <- function(event) {
  if (event %in% .fg_named_points) return(event)
  parts <- strsplit(event, ":", fixed = TRUE)[[1]]
  sprintf("fg_%s(%s)", parts[[1]], parts[[2]])
}

.fg_check_string <- function(x, name, allow_null = FALSE) {
  if (allow_null && is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop("`", name, "` must be one non-empty string.", call. = FALSE)
  }
  invisible(x)
}

#' A Message Injected into a Session
#'
#' Describes one message to be placed into a running session at a stated
#' point, as an experimental manipulation: an objection, a correction, a
#' piece of news. The message is logged like any other, with
#' `metadata$injected = TRUE` and an `injection` record giving its label, arm,
#' point, sender, visibility, and the `message_id` it follows.
#'
#' A message `from = "moderator"` is logged under the moderator's id, so the
#' moderator reads it as its own line; so is a message from the moderator's
#' id or handle. A message from a participant's id or handle (the handle in
#' any case) is that participant's turn: at `fg_before_turn(t)` the
#' participant says `text` as turn `t`, with no model call. It is logged
#' under the participant's id with `metadata$as_turn = TRUE`, recorded in the
#' routing record with the rule `"injected"`, and counted as a participant
#' turn, and it takes the place of the turn the session was about to give:
#' the flow's pick, an addressed turn, or the reply to a call-on line, after
#' which the moderator calls on the next silent participant. A line from
#' someone other than the participant called on takes no one's place: the
#' participant called on still answers next. The record's `replaced` names
#' which turn was taken, or `"none"`. Such a message is allowed only at
#' [fg_before_turn()], with `visibility = "shared"`, and once per point. A
#' `from` that is one participant's id and another's handle, as under
#' permuted handles, could mean either and stops the run before any call. Any
#' other `from` names an outside voice, such as `"Guest expert"`, logged
#' under that name and shown to the agents as written; it may not be
#' `"System"`, nor a name of the form `"P4"`, which would read as a
#' participant's handle. A moderator message that puts its last question to
#' one participant by handle ("P2, what do you make of that?") gives that
#' participant the next turn, except at a turn that answers the moderator's
#' call-on line, which stays with the participant called on.
#'
#' With `visibility = "shared"` the message enters the shared log and every
#' agent sees it from then on. With `visibility = "participants"` it is kept
#' out of the moderator's prompts and out of interim summaries, which the
#' moderator also reads; the participants see it as part of the discussion.
#'
#' @param text One non-empty string, the message.
#' @param at Where the message goes: [fg_after_item()], [fg_before_turn()],
#'   or `"before_closing"`. When an injection and a probe share a point, the
#'   probe is asked first.
#' @param from `"moderator"`, a participant's id or handle, or the name of
#'   an outside speaker.
#' @param visibility `"shared"` or `"participants"`.
#' @param label Optional name for the injection in the records; by default
#'   the arm's name in [run_focus_group_arms()], and `"injection_1"`,
#'   `"injection_2"`, ... in [run_focus_group()].
#' @return An object of class `fg_injection`.
#' @seealso [run_focus_group()], [run_focus_group_arms()].
#' @examples
#' fg_injection(
#'   "One study found that extended hours cost more than they return.",
#'   at = fg_after_item(4), from = "Guest expert")
#' @export
fg_injection <- function(text, at, from = "moderator",
                         visibility = c("shared", "participants"),
                         label = NULL) {
  .fg_check_string(text, "text")
  at <- .fg_as_point(at)
  .fg_check_string(from, "from")
  visibility <- match.arg(visibility)
  .fg_check_string(label, "label", allow_null = TRUE)
  structure(list(text = text, at = at, from = trimws(from),
                 visibility = visibility, label = label),
            class = "fg_injection")
}

#' @export
print.fg_injection <- function(x, ...) {
  cat("<fg_injection> ", format(x$at), " from ", x$from, " (", x$visibility,
      ")\n", sep = "")
  cat("  ", x$text, "\n", sep = "")
  invisible(x)
}

#' A Private Question Put to Each Participant
#'
#' Describes a question asked of each participant at stated points, whose
#' answers are saved but never enter the shared log: no agent sees a probe
#' or its answers afterwards, so asking it does not change the discussion.
#' Asked before and after an injection, the same question measures a
#' position on both sides of it; asked at `"start"` and `"end"`, it gives
#' ratings before and after a session.
#'
#' Each participant answers with its persona, the standing rules, and the
#' discussion so far, as for a turn, and the probe as the closing
#' instruction, worded by the `participant_probe_instruction` template of
#' [get_default_prompt_templates()]. The answer is the reply's final answer,
#' without any reasoning. A reply that is empty or cut off is retried once
#' with a larger budget; one still empty is recorded as `NA`, with a
#' warning. The answers are returned in the `probes` component of the result,
#' with the raw reply beside each.
#'
#' With `scale = c(lo, hi)`, each answer is also read as a number, recorded
#' in the `value` column. The reading is strict. Markup ("**4**"), a
#' restatement of the scale ("1-7:", "on a scale of 1 to 7", "from 1
#' (strongly against) to 7 (strongly in favor)"), and a denominator equal to
#' `hi` ("5/7", "6 out of 7") are set aside, as are digits joined to letters
#' ("P3", "V241", "2024s") and, on a scale whose ends lie between -999 and
#' 999, numbers of four or more digits, such as years. What remains must be
#' one whole number from `lo` to `hi`. An answer with no number, with two
#' different numbers ("3 or 4"), with a fraction of a point ("4.5"), with a
#' number outside the scale ("-2"), or on another scale ("6/10" on a 1 to 7
#' scale) is not read, and neither are numbers written in words ("seven");
#' Arabic-Indic and Persian digits are read as digits. Nor is an answer that
#' negates, compares, or reports someone else's number, whose number need
#' not be the answer: one with "minus", "negative", "not", "never", "no",
#' "except", "at least", "at most", "more than", "less than", "fewer than",
#' "higher", "lower", "above", "below", "over", "under", or "between"
#' (outside the scale's own restatement and the description of its ends),
#' a number followed by "+" ("2+"), "others said", "they said", "everyone
#' said", "someone said", or "P3 said", or a fraction written as one
#' character, such as the half sign.
#' Such an answer is followed by one re-ask in the same private exchange,
#' worded by the `participant_probe_reask` template, asking for one number
#' from `lo` to `hi` alone. An empty reply to the re-ask is not retried, and
#' the earlier answer is kept. A value still unread is `NA`, and the
#' participants concerned are named in one warning for the probe at that
#' point. The column `attempts` counts the calls made for each answer, and
#' every call, the re-ask included, is in the result's `calls`.
#'
#' @param question One non-empty string, the question.
#' @param at One point, or a list of points, at which to ask it:
#'   [fg_after_item()], [fg_before_turn()], `"start"`, `"before_closing"`,
#'   or `"end"`. `"end"` comes after the closing line; `"before_closing"`
#'   comes before it.
#' @param label Optional name for the probe in the records; by default
#'   `"probe_1"`, `"probe_2"`, ... in the order the probes are given.
#' @param participants Optional character vector of participant ids to ask;
#'   `NULL` asks every participant.
#' @param scale Optional two whole numbers, `c(lo, hi)` with `lo < hi`, the
#'   ends of the scale on which the question asks for a number. `NULL`, the
#'   default, records the answers as text only.
#' @return An object of class `fg_probe`.
#' @seealso [run_focus_group()], [run_focus_group_arms()].
#' @examples
#' fg_probe(paste("Where do you stand on extending library hours, from 1",
#'                "(strongly against) to 7 (strongly in favor)?",
#'                "Answer with the number first."),
#'          at = list("start", fg_after_item(4), "end"), label = "position",
#'          scale = c(1, 7))
#' @export
fg_probe <- function(question, at, label = NULL, participants = NULL, scale = NULL) {
  .fg_check_string(question, "question")
  points <- if (inherits(at, "fg_point")) list(at) else
    if (is.character(at)) as.list(at) else at
  if (!is.list(points) || !length(points)) {
    stop("`at` must be one point or a list of points.", call. = FALSE)
  }
  points <- lapply(points, .fg_as_point, allow_ends = TRUE)
  events <- vapply(points, .fg_point_event, character(1))
  if (anyDuplicated(events)) {
    stop("`at` names the same point more than once.", call. = FALSE)
  }
  .fg_check_string(label, "label", allow_null = TRUE)
  if (!is.null(participants) &&
      (!is.character(participants) || !length(participants) ||
       anyNA(participants) || anyDuplicated(participants))) {
    stop("`participants` must be `NULL` or distinct participant ids.",
         call. = FALSE)
  }
  scale <- .fg_check_scale(scale)
  structure(list(question = question, at = points, label = label,
                 participants = participants, scale = scale),
            class = "fg_probe")
}

#' @export
print.fg_probe <- function(x, ...) {
  cat("<fg_probe> at ", paste(vapply(x$at, format, character(1)), collapse = ", "),
      if (!is.null(x$scale)) sprintf(", scale %s to %s", .fg_number_text(x$scale[[1]]),
                                     .fg_number_text(x$scale[[2]])),
      "\n", sep = "")
  cat("  ", x$question, "\n", sep = "")
  invisible(x)
}

# A list of injections from NULL, one injection, or a list of them.
.fg_injection_list <- function(x, what = "`injections`") {
  if (is.null(x)) return(list())
  if (inherits(x, "fg_injection")) return(list(x))
  if (!is.list(x) || !all(vapply(x, inherits, logical(1), "fg_injection"))) {
    stop(what, " must be `NULL`, an fg_injection(), or a list of them.",
         call. = FALSE)
  }
  unname(x)
}

# The whole number from `lo` to `hi` that a probe answer states, or NA. The
# reading is strict, so that a number is recorded only when the answer gives
# one and no other:
# * markup ("**4**") is removed;
# * a restatement of the scale is removed: "1-7", "1 to 7", "between 1 and
#   7", "from 1 (strongly against) to 7 (strongly in favor)", and the pair
#   "1 is ... 7 is ..." (or "means", "=");
# * a fraction or "out of" whose denominator is `hi` ("5/7", "6 out of 7")
#   counts as its numerator, and "out of 7" alone is removed; any other
#   denominator ("6/10" on a 1-7 scale) means the answer was given on
#   another scale, and no number is read;
# * Arabic-Indic and Persian digits are digits, and a comma between digits
#   is a thousands separator ("1,000") or a decimal comma ("4,5");
# * digits joined to letters are not numbers ("P3", "V241", "2024s",
#   "7-point", "9pm"), nor, on a scale whose ends lie between -999 and 999,
#   are numbers of four or more digits, which are years or counts ("2024",
#   "1,000");
# * what remains must be one number, perhaps repeated, that is whole and lies
#   from `lo` to `hi`. No number, two different numbers ("3 or 4", "3-4"), a
#   fraction of a point ("4.5"), or a number outside the scale ("-2", "8")
#   gives NA.
# * an answer that negates, compares, or reports another's number is not
#   read, since the number it states need not be the answer: "minus",
#   "negative", "not", "n't", "never", "no", "except", "at least", "at
#   most", "more than", "less than", "fewer than", "higher", "lower",
#   "above", "below", "over", "under", or "between" left once the scale's
#   own restatement is removed; a number followed by "+" ("2+"); "others
#   said", "they said", "everyone said", "someone said", "P3 said" and the
#   like; and fractions written as one character (the half, quarters, and
#   the other vulgar fractions).
# Numbers written in words ("seven") are not read.
.fg_number_cues <- paste0(
  "\\b(?:minus|negative|not|never|no|except|between|higher|lower|above|below|",
  "over|under)\\b|n['\u2019]t\\b|",
  "\\b(?:at\\s+least|at\\s+most|more\\s+than|less\\s+than|fewer\\s+than)\\b|",
  "\\b(?:others|other\\s+people|they|everyone|everybody|someone|somebody|",
  "p[0-9]+)\\s+(?:said|say|says|gave|give|gives|chose|picked|answered|rated)\\b|",
  "[0-9]\\s*\\+|[\u00bc\u00bd\u00be\u2150-\u215e]")

.fg_parse_number <- function(text, lo, hi) {
  if (length(text) != 1L || is.na(text)) return(NA_real_)
  # Dashes and the minus sign as "-", and Arabic-Indic and Persian digits as
  # digits; fixed matching works in every locale.
  x <- enc2utf8(as.character(text))
  for (dash in c("\u2010", "\u2011", "\u2012", "\u2013", "\u2014", "\u2015", "\u2212")) {
    x <- gsub(dash, "-", x, fixed = TRUE)
  }
  for (d in 0:9) {
    x <- gsub(intToUtf8(0x0660 + d), d, x, fixed = TRUE)
    x <- gsub(intToUtf8(0x06F0 + d), d, x, fixed = TRUE)
  }
  x <- gsub(intToUtf8(0x066B), ".", x, fixed = TRUE)
  x <- gsub(intToUtf8(0x066C), ",", x, fixed = TRUE)
  x <- tolower(x)
  x <- gsub("[*_`~#>|]", " ", x)
  # A comma between digits groups thousands ("1,000") or, otherwise, marks
  # a decimal ("4,5").
  x <- gsub("(?<=[0-9]),(?=[0-9]{3}\\b)", "", x, perl = TRUE)
  x <- gsub("(?<=[0-9]),(?=[0-9])", ".", x, perl = TRUE)
  num <- "[0-9]+(?:\\.[0-9]+)?"
  start <- "(?<![a-z0-9.])"
  end <- "(?![a-z0-9]|\\.[0-9])"
  as_pattern <- function(v) gsub("-", "\\-", .fg_number_text(v), fixed = TRUE)
  lo_re <- paste0(start, as_pattern(lo), end)
  hi_re <- paste0(start, as_pattern(hi), end)
  aside <- "(?:\\s*\\([^()]*\\))?"
  range <- paste0("(?:\\b(?:between|from)\\s+)?", lo_re, aside,
                  "\\s*(?:-|to|through|thru|and)\\s*", hi_re, aside)
  x <- gsub(range, " ", x, perl = TRUE)
  anchor <- "(?=\\s*(?:is\\b|means\\b|=))"
  # The words that describe the ends ("0 is no trust and 10 is full trust")
  # are the scale's, not the answer's, and are not read for cues.
  cued <- x
  if (grepl(paste0(lo_re, anchor), x, perl = TRUE) &&
      grepl(paste0(hi_re, anchor), x, perl = TRUE)) {
    cued <- gsub(paste0("(?:", lo_re, "|", hi_re, ")\\s*(?:is|means|=)\\s*",
                        "[^,;.()]*?(?=\\s+and\\b|[,;.()]|$)"), " ", x, perl = TRUE)
    x <- gsub(paste0("(", lo_re, "|", hi_re, ")", anchor), " ", x, perl = TRUE)
  }
  if (grepl(.fg_number_cues, cued, perl = TRUE)) return(NA_real_)
  fraction <- paste0(start, "(-?", num, ")\\s*(?:/|out\\s+of)\\s*(", num, ")", end)
  found <- regmatches(x, gregexpr(fraction, x, perl = TRUE))[[1]]
  if (length(found)) {
    parts <- regmatches(found, regexec(fraction, found, perl = TRUE))
    if (any(vapply(parts, function(p) as.numeric(p[[3]]) != hi, logical(1)))) {
      return(NA_real_)
    }
    regmatches(x, gregexpr(fraction, x, perl = TRUE)) <-
      list(paste0(" ", vapply(parts, `[[`, character(1), 2L), " "))
  }
  x <- gsub(paste0("(?:out\\s+of|/)\\s*", hi_re), " ", x, perl = TRUE)
  if (grepl(paste0("(?:out\\s+of|/)\\s*", num), x, perl = TRUE)) return(NA_real_)
  tokens <- gregexpr("[a-z0-9]+(?:[-'.][a-z0-9]+)*", x, perl = TRUE)
  words <- regmatches(x, tokens)[[1]]
  mixed <- grepl("[a-z]", words) & grepl("[0-9]", words)
  regmatches(x, tokens) <- list(ifelse(mixed, " ", words))
  x <- gsub("([0-9])(?:-|\\s+-\\s+)(?=[0-9])", "\\1 to ", x, perl = TRUE)
  values <- as.numeric(regmatches(x, gregexpr(paste0("(?<![a-z0-9])-?", num), x,
                                              perl = TRUE))[[1]])
  if (max(abs(c(lo, hi))) < 1000) {
    values <- values[!grepl("^-?[0-9]{4,}", .fg_number_text(trunc(values)))]
  }
  values <- unique(values)
  if (length(values) != 1L || values != round(values) || values < lo || values > hi) {
    return(NA_real_)
  }
  values
}

# Validate a probe's `scale`: NULL, or two whole numbers, the lower first.
.fg_check_scale <- function(scale) {
  if (is.null(scale)) return(NULL)
  if (!is.numeric(scale) || length(scale) != 2L || anyNA(scale) ||
      any(!is.finite(scale)) || any(scale != round(scale)) || scale[[1]] >= scale[[2]]) {
    stop("`scale` must be `NULL` or two whole numbers, the lower end first, as in c(1, 7).",
         call. = FALSE)
  }
  as.double(unname(scale))
}

# A list of probes from NULL, one probe, or a list of them.
.fg_probe_list <- function(x) {
  if (is.null(x)) return(list())
  if (inherits(x, "fg_probe")) return(list(x))
  if (!is.list(x) || !all(vapply(x, inherits, logical(1), "fg_probe"))) {
    stop("`probes` must be `NULL`, an fg_probe(), or a list of them.",
         call. = FALSE)
  }
  unname(x)
}

# Arms: a named list whose elements are NULL (continue unchanged), one
# injection, or a list of injections. Returns a named list of injection
# lists, with labels defaulting to the arm's name.
.fg_arm_list <- function(arms) {
  if (!is.list(arms) || inherits(arms, c("fg_injection", "fg_probe")) ||
      length(arms) < 2L) {
    stop("`arms` must be a named list of at least two arms.", call. = FALSE)
  }
  arm_names <- names(arms)
  if (is.null(arm_names) || anyNA(arm_names) || any(!nzchar(trimws(arm_names))) ||
      anyDuplicated(arm_names)) {
    stop("Every arm must have a distinct, non-empty name.", call. = FALSE)
  }
  out <- lapply(arm_names, function(name) {
    injections <- .fg_injection_list(
      arms[[name]], what = sprintf("Arm '%s'", name))
    lapply(injections, function(inj) {
      inj$label <- inj$label %||% name
      inj
    })
  })
  names(out) <- arm_names
  if (all(lengths(out) == 0L)) {
    stop("At least one arm must inject a message.", call. = FALSE)
  }
  out
}

#' Run Arms of a Focus Group from One Shared Prefix
#'
#' Runs a session up to an injection point once, then continues the full
#' remainder of the session (every later question and turn, and the final
#' summary) once per arm, so that every arm shares each message before the
#' injection. Comparing the arms then compares sessions that differ only in
#' what was injected and in what followed from it.
#'
#' Each arm is `NULL`, which continues the prefix with nothing injected, one
#' [fg_injection()], or a list of injections. The first injection of every
#' arm that has one must name the same point; that point is where the
#' session branches. An arm's later injections may name later points. A
#' neutral injection at the branch point, rather than `NULL`, gives a control
#' arm that also receives a message there.
#'
#' The prefix is run once, with its probes, and a copy of the whole session
#' (agents, turn-taking state, records, and token counters) is taken at the
#' branch point; each arm continues its own copy. Each arm also starts from
#' the random-number state at the branch point, so the random draws that
#' follow, such as tie-breaks and probabilistic speaker choices, use the same
#' stream in every arm; a provider's retries, which wait a random time, do
#' not draw from it. With `seed`, the whole run is reproducible; without it,
#' the arms still share the stream from the branch point on.
#'
#' A branch point must lie within the guide: `fg_after_item(k)` needs `k` no
#' larger than the number of guide items, and `fg_before_turn(t)` needs `t`
#' no larger than the number of participant turns the guide allows. A point
#' the session still does not reach, as when a custom flow chooses no
#' speaker, stops the run with an error of class
#' `focusgroup_injection_unreached`.
#'
#' The estimate checked against `max_calls` is that of one session times the
#' number of arms, an upper bound, since the prefix is paid once.
#'
#' @section Continuing a recorded session:
#' When `topic` is a recorded session, a `FocusGroup` or a
#' `focus_group_result` (for example one read from a session file by
#' [focus_group_from_transcript()]), no prefix is run: each arm continues the
#' record from a participant turn. The branch point must be
#' `fg_before_turn(t)`, where turn `t` is the `t`-th message by a
#' participant. The messages before it are the shared prefix, taken from the
#' record as they are, and the record's turn `t` and everything after it are
#' replaced by each arm's continuation. An arm `fg_injection(text, at =
#' fg_before_turn(t), from = "P2")` has P2 say `text` as turn `t` (see
#' [fg_injection()]); a `NULL` arm regenerates turn `t` and the rest; an
#' outside voice or a moderator line can also be injected there.
#'
#' The session is rebuilt as it stood just before turn `t`: the guide item
#' and its stage, the turns already given on it, the call-on queue, a
#' participant the moderator's last line addressed, the question in force,
#' the interim summary in force, and the turn-taking rule's state, with the
#' record's personas, handles, guide, settings, templates, standing rules,
#' and message construction. `config` and `.runner` are given to every agent;
#' `reasoning_effort` and `max_tokens_config`, when given, replace the
#' record's. The arguments that the record fixes (`n_participants`, `guide`,
#' `demographics`, `survey_responses`, `flow`, `message_mode`,
#' `max_participant_responses`, `on_desire_failure`, `moderator_verbatim`,
#' `call_on_silent`, `call_on_text`, and `handles`) must be left out. A probe
#' at the branch point itself is asked once, on the rebuilt session, before
#' the arms, so that every arm shares its answers, as a probe there is asked
#' in the prefix of a new session. A probe or an injection at a point the
#' record has already passed is an error, and so is a transcript without
#' moderator lines, which has no guide item to continue, as
#' are a custom turn-taking flow, whose state cannot be rebuilt, a record
#' without a topic, and a participant who has only the placeholder persona of
#' an imported transcript; give personas with `focus_group_from_transcript(x,
#' personas = ...)`. Everything is checked before any call. Under round robin
#' with a model that replies the same way to the same messages, a `NULL` arm
#' repeats the record. Each arm's `calls` and `usage` hold the record's calls
#' before the cut and the arm's own after it. With `seed`, the caller's
#' random-number state is restored afterwards.
#'
#' A continuation of a human transcript is a model's continuation of a human
#' discussion: the later moderator lines are the recorded ones, delivered as
#' written, and the participants' turns are generated from the personas
#' given. Arms are comparable with one another, not with the people who took
#' part.
#'
#' @inheritParams run_focus_group
#' @param topic Character. Focus group topic. Or a recorded session to
#'   continue, a `FocusGroup` or a `focus_group_result`; see "Continuing a
#'   recorded session".
#' @param arms A named list of at least two arms, as described in Details.
#' @param probes Optional [fg_probe()] or list of probes. A probe at or
#'   before the branch point is asked once, in the prefix; a later one is
#'   asked in every arm.
#' @return A `focus_group_arms` object, a list with:
#' \describe{
#'   \item{arms}{A named list of `focus_group_result` objects, one per arm,
#'     each a complete session as returned by [run_focus_group()]. Its
#'     transcript, calls, and records begin with the shared prefix; its
#'     `usage` counts the whole session, prefix included; and its
#'     `metadata$branch` records the arm, the branch point, the number of
#'     prefix messages and calls, and `prefix_hash`, a hash of the prefix
#'     transcript that is the same in every arm.}
#'   \item{prefix}{The shared prefix as a `focus_group_result` with
#'     `metadata$complete` set to `FALSE` and no summary.}
#'   \item{branch_point}{The branch point, as written, such as
#'     `"fg_after_item(4)"`.}
#'   \item{injections}{A data frame of every arm's injections, as in
#'     [run_focus_group()].}
#'   \item{probes}{A data frame of every probe answer: those asked in the
#'     prefix once, with `arm` set to `NA`, and those asked in each arm.}
#'   \item{usage}{The token usage of all the calls made: the prefix once and
#'     each arm's remainder. Adding up the arms' own `usage` would count the
#'     prefix once per arm.}
#'   \item{metadata}{A list with `topic`, `branch_point`, `arms` (the arm
#'     names), `estimated_calls`, `seed`, `prefix_messages`, `prefix_hash`,
#'     `complete`, and `failure`.}
#' }
#' When a call fails, the run stops with an error of class
#' `focusgroup_incomplete_arms` whose `partial` field holds the
#' `focus_group_arms` object built from what was completed, with the arm
#' that failed, if any, as a partial session; its `arm` field names that arm
#' and its `parent` field holds the original error.
#' @seealso [fg_injection()], [fg_probe()], [run_focus_group()].
#' @examples
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                         api_key = LLMR::llm_api_key_env("GROQ_API_KEY"))
#' scripted <- function(experiments, ...) {
#'   experiments$response_text <- "Evening hours matter most to people who work."
#'   experiments
#' }
#' guide <- list(Opening = "Welcome the participants.",
#'               Engagement = c("Which hours matter most?",
#'                              "Who is left out by the current hours?"),
#'               Exploration = "What would you change first?",
#'               Closing = "Thank the participants and close.")
#' arms <- list(
#'   objection = fg_injection(
#'     "Extended hours would cost the branch two weekday programs.",
#'     at = fg_after_item(3), from = "Guest expert"),
#'   control = NULL)
#' out <- run_focus_group_arms(
#'   "Library hours", cfg, arms = arms, n_participants = 2, guide = guide,
#'   flow = "round_robin", verbose = FALSE,
#'   probes = fg_probe("From 1 to 7, how strongly do you favor longer hours?",
#'                     at = list(fg_after_item(3), "end")),
#'   .runner = scripted)
#' out
#' out$probes[, c("arm", "point", "agent_id", "answer")]
#'
#' # Continue a recorded session from its third participant turn: as recorded,
#' # and with P2 saying something else there.
#' session <- run_focus_group("Library hours", cfg, n_participants = 2,
#'                            guide = guide, flow = "round_robin",
#'                            verbose = FALSE, .runner = scripted)
#' path <- tempfile(fileext = ".fg.jsonl")
#' writeLines(format(session), path)
#' again <- run_focus_group_arms(
#'   focus_group_from_transcript(path), cfg,
#'   arms = list(regenerated = NULL,
#'               changed = fg_injection("Mornings matter more to me.",
#'                                      at = fg_before_turn(3), from = "P2")),
#'   verbose = FALSE, .runner = scripted)
#' again$arms$changed$transcript[, c("speaker_id", "text")]
#' @export
run_focus_group_arms <- function(topic, config, arms, n_participants = 6,
                                 guide = c(Opening = 1, Icebreaker = 1,
                                           Engagement = 2, Exploration = 3,
                                           Closing = 1),
                                 demographics = NULL, survey_responses = NULL,
                                 flow = "desire_based", seed = NULL,
                                 message_mode = c("roleflip", "flat"),
                                 verbose = TRUE,
                                 max_participant_responses = NULL,
                                 max_calls = 120L, confirm = FALSE,
                                 max_tokens_config = NULL,
                                 reasoning_effort = NULL,
                                 on_desire_failure = c("error", "random"),
                                 probes = NULL, moderator_verbatim = FALSE,
                                 call_on_silent = c("opening", "midpoint", "end"),
                                 call_on_text = NULL, handles = NULL,
                                 final_summary = TRUE, .runner = NULL) {
  if (inherits(topic, c("FocusGroup", "focus_group_result"))) {
    # A recorded session fixes these; they must be left out.
    fixed <- c("n_participants", "guide", "demographics", "survey_responses", "flow",
               "message_mode", "max_participant_responses", "on_desire_failure",
               "moderator_verbatim", "call_on_silent", "call_on_text", "handles")
    frame <- environment()
    given <- fixed[!vapply(fixed, function(name) {
      eval(call("missing", as.name(name)), frame)
    }, logical(1))]
    if (length(given)) {
      stop("A recorded session fixes ", paste0("`", given, "`", collapse = ", "),
           "; leave ", if (length(given) == 1L) "it" else "them",
           " out when continuing it.", call. = FALSE)
    }
    if (missing(config)) {
      stop("`config` must be an explicit `llm_config` object.", call. = FALSE)
    }
    out <- .fg_continue_arms(
      record = topic, config = config, arms = arms, probes = probes, seed = seed,
      verbose = verbose, max_calls = max_calls, confirm = confirm,
      final_summary = final_summary, reasoning_effort = reasoning_effort,
      max_tokens_config = max_tokens_config, .runner = .runner, seed_env = frame)
    if (verbose) cat("Arms complete.\n")
    return(out)
  }
  message_mode <- match.arg(message_mode)
  on_desire_failure <- match.arg(on_desire_failure)
  arms <- .fg_arm_list(arms)
  setup <- .fg_session_setup(
    topic = topic, config = config, n_participants = n_participants,
    guide = guide, demographics = demographics,
    survey_responses = survey_responses, flow = flow, seed = seed,
    message_mode = message_mode, verbose = verbose,
    max_participant_responses = max_participant_responses,
    max_calls = max_calls, confirm = confirm,
    max_tokens_config = max_tokens_config, reasoning_effort = reasoning_effort,
    on_desire_failure = on_desire_failure, probes = probes,
    moderator_verbatim = moderator_verbatim, call_on_silent = call_on_silent,
    call_on_text = call_on_text, handles = handles,
    final_summary = final_summary, n_arms = length(arms),
    .runner = .runner, seed_env = environment())
  setup$final_summary <- final_summary
  out <- .fg_execute_arms(setup, arms, verbose)
  if (verbose) cat("Arms complete.\n")
  out
}

# Run the prefix and the arms, and build the focus_group_arms object. A
# failure anywhere becomes an error carrying what was completed. `run`, when
# given, runs the arms in place of FocusGroup$run_arms(), as for a recorded
# session continued from a cut.
.fg_execute_arms <- function(setup, arms, verbose, run = NULL) {
  fg <- setup$fg
  build <- function(prefix, branches, failure = NULL) {
    .fg_arms_result(prefix, branches, setup, failure)
  }
  run <- tryCatch(
    withr::with_options(list(focusgroup.message_mode = setup$message_mode),
                        if (is.function(run)) run() else
                          fg$run_arms(arms, probes = setup$probes, verbose = verbose,
                                      final_summary = setup$final_summary %||% TRUE)),
    error = function(e) e)
  # An error raised before the session began, such as an arm whose point
  # lies outside the guide, carries no partial result.
  if (inherits(run, "error") && !length(fg$conversation_log)) stop(run)
  if (inherits(run, "focusgroup_branch_failure")) {
    failed <- run$branch
    attr(failed, "session_status") <- list(complete = FALSE,
      failure = list(phase = run$phase, message = conditionMessage(run$parent)))
    completed <- lapply(run$completed, function(b) {
      attr(b, "session_status") <- list(complete = TRUE, failure = NULL)
      b
    })
    branches <- c(completed, stats::setNames(list(failed), run$arm))
    rlang::abort(sprintf("Arm '%s' stopped during phase '%s'.", run$arm, run$phase),
                 class = "focusgroup_incomplete_arms",
                 partial = build(fg, branches,
                                 list(arm = run$arm, phase = run$phase,
                                      message = conditionMessage(run$parent))),
                 arm = run$arm, phase = run$phase, parent = run$parent)
  }
  if (inherits(run, "error")) {
    phase <- attr(fg, "execution_phase") %||% "setup"
    if (inherits(run, "focusgroup_injection_unreached")) phase <- "branch_point"
    attr(fg, "session_status") <- list(complete = FALSE,
      failure = list(phase = phase, message = conditionMessage(run)))
    rlang::abort(sprintf("The shared prefix stopped during phase '%s'.", phase),
                 class = "focusgroup_incomplete_arms",
                 partial = build(fg, list(), list(arm = NA_character_, phase = phase,
                                                  message = conditionMessage(run))),
                 arm = NA_character_, phase = phase, parent = run)
  }
  for (name in names(run$arms)) {
    attr(run$arms[[name]], "session_status") <- list(complete = TRUE, failure = NULL)
  }
  build(run$prefix, run$arms)
}

# Continue a recorded session from a participant turn, once per arm. The
# record is read into a fresh FocusGroup, put in its state just before the
# turn of the branch point, given `config` and `.runner`, and checked with
# the arms and probes before any call; the arms then run as in
# FocusGroup$run_arms(). The standing rules, the disclosure setting, and the
# message construction of the record are in force while they run.
.fg_continue_arms <- function(record, config, arms, probes, seed, verbose, max_calls,
                              confirm, final_summary, reasoning_effort,
                              max_tokens_config, .runner, seed_env) {
  .fg_validate_config(config)
  .fg_check_seed(seed)
  reasoning_effort <- .fg_validate_reasoning_effort(reasoning_effort)
  probes <- .fg_probe_list(probes)
  .fg_check_flag(final_summary, "final_summary")
  .fg_check_flag(verbose, "verbose")
  .fg_check_flag(confirm, "confirm")
  if (!is.numeric(max_calls) || length(max_calls) != 1L || is.na(max_calls) ||
      !is.finite(max_calls) || max_calls < 1 || max_calls != floor(max_calls)) {
    stop("`max_calls` must be one finite positive integer.", call. = FALSE)
  }
  if (!is.null(.runner) && !is.function(.runner)) {
    stop("`.runner` must be `NULL` or a function.", call. = FALSE)
  }
  if (!is.null(max_tokens_config) && !is.list(max_tokens_config)) {
    stop("`max_tokens_config` must be `NULL` or a named list.", call. = FALSE)
  }
  arms <- .fg_arm_list(arms)
  firsts <- Filter(Negate(is.null), lapply(arms, function(a) if (length(a)) a[[1]]))
  events <- unique(vapply(firsts, function(inj) .fg_point_event(inj$at), character(1)))
  if (length(events) != 1L) {
    stop("The first injection of every arm must name the same point; found ",
         paste(vapply(events, .fg_event_label, character(1)), collapse = ", "), ".",
         call. = FALSE)
  }
  branch_event <- events[[1]]
  if (!identical(firsts[[1]]$at$type, "before_turn")) {
    stop("A recorded session is continued from a participant turn; name the branch ",
         "point with fg_before_turn(), not ", .fg_event_label(branch_event), ".",
         call. = FALSE)
  }
  t <- firsts[[1]]$at$value

  fg <- focus_group_from_transcript(record)
  info <- fg$records$info %||% list()
  flow_name <- info$recorded$flow %||% .fg_flow_name(fg$turn_taking_flow)
  if (!flow_name %in% c("round_robin", "probabilistic", "desire_based")) {
    stop("A session with the custom turn-taking flow '", flow_name, "' cannot be ",
         "continued: its state cannot be rebuilt from the record.", call. = FALSE)
  }
  if (identical(fg$topic, .fg_placeholder_topic)) {
    stop("The record has no topic. Give one with ",
         "focus_group_from_transcript(x, topic = ...) and continue that.", call. = FALSE)
  }
  participant_ids <- setdiff(names(fg$agents), fg$moderator_id)
  stand_ins <- participant_ids[vapply(participant_ids, function(id) {
    identical(fg$agents[[id]]$persona_description, .fg_placeholder_persona(id))
  }, logical(1))]
  if (length(stand_ins)) {
    stop("Participant(s) ", paste(stand_ins, collapse = ", "), " have no persona, only ",
         "the placeholder of an imported transcript, and could speak in the ",
         "continuation. Give their personas with ",
         "focus_group_from_transcript(x, personas = ...) and continue that.", call. = FALSE)
  }
  # Every guide item begins with a moderator line; a transcript without one
  # has no item to continue.
  guide_lines <- vapply(fg$conversation_log, function(m) {
    identical(m$speaker_id, fg$moderator_id) && !isTRUE(m$metadata$injected)
  }, logical(1))
  if (!any(guide_lines)) {
    stop("The transcript has no moderator lines, so it has no guide items to ",
         "continue: each guide item begins with a moderator line. If one of the ",
         "speakers moderated, name them with focus_group_from_transcript(x, ",
         "moderator_id = ...); otherwise add the moderator's questions to the ",
         "transcript as the moderator's messages, in their places, and read it again.",
         call. = FALSE)
  }
  priv <- fg$.__enclos_env__$private
  turns <- sum(vapply(fg$conversation_log, function(m) {
    m$speaker_id %in% participant_ids
  }, logical(1)))
  if (t > turns) {
    stop("The record has ", turns, " participant turn(s); the branch point ",
         .fg_event_label(branch_event), " lies beyond them.", call. = FALSE)
  }

  # The settings the continuation runs under: the record's, with the
  # model configuration, the runner, and any new limits given here.
  for (agent in fg$agents) {
    agent$config <- config
    agent$.runner <- .runner
  }
  fg$admin_config <- config
  if (!is.null(reasoning_effort)) fg$reasoning_effort <- reasoning_effort
  caps <- c(utterance = "max_tokens_utterance", moderator = "max_tokens_moderator",
            desire = "max_tokens_desire", desire_retry = "max_tokens_desire_retry",
            summary = "max_tokens_summary", interim_summary = "max_tokens_interim_summary",
            probe = "max_tokens_probe")
  unknown <- setdiff(names(max_tokens_config %||% list()), names(caps))
  if (length(unknown)) {
    stop("Unknown `max_tokens_config` element(s): ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  for (cap in names(max_tokens_config %||% list())) {
    value <- max_tokens_config[[cap]]
    if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) ||
        value < 1 || value != floor(value)) {
      stop("`max_tokens_config$", cap, "` must be one positive whole number.", call. = FALSE)
    }
    fg[[caps[[cap]]]] <- value
  }
  message_mode <- fg$message_mode %||% .fg_message_mode()
  options <- list(focusgroup.message_mode = message_mode)
  rules <- info$standing_rules
  if (is.character(rules$participant) && !is.na(rules$participant)) {
    options$focusgroup.participant_rules <- rules$participant
  }
  if (is.character(rules$moderator) && !is.na(rules$moderator)) {
    options$focusgroup.moderator_rules <- rules$moderator
  }
  if (is.character(info$disclosure) && !is.na(info$disclosure)) {
    options$focusgroup.disclosure <- info$disclosure
  }
  withr::local_options(options)
  fg$message_mode <- message_mode
  priv$position_at(t, final_summary = final_summary)

  # Everything is checked before any call: each arm's injections against the
  # guide and the points already passed, and the probes likewise.
  passed <- setdiff(priv$fired, branch_event)
  for (name in names(arms)) {
    plan <- priv$prepare_plan(lapply(arms[[name]], function(inj) {
      inj$label <- inj$label %||% name
      inj
    }), probes)
    late <- vapply(plan$injections, function(inj) inj$event %in% passed, logical(1))
    if (any(late)) {
      stop("Arm '", name, "' places an injection at ",
           .fg_event_label(plan$injections[[which(late)[1]]]$event),
           ", which comes before the branch point.", call. = FALSE)
    }
  }
  plan <- priv$prepare_plan(list(), probes)
  for (probe in plan$probes) {
    early <- intersect(probe$events, setdiff(priv$fired, branch_event))
    if (length(early)) {
      stop("Probe '", probe$label, "' is asked at ", .fg_event_label(early[[1]]),
           ", which a continued session has already passed; ask it at a later point.",
           call. = FALSE)
    }
  }
  moderator <- fg$agents[[fg$moderator_id]]
  k <- priv$state$item
  later <- fg$question_script[setdiff(seq_along(fg$question_script), seq_len(k))]
  if (identical(moderator$persona_description, .fg_placeholder_persona(fg$moderator_id)) &&
      any(!vapply(later, function(item) isTRUE(item$verbatim), logical(1)))) {
    stop("The moderator has no persona and would write the lines of later guide items. ",
         "Give one with focus_group_from_transcript(x, personas = ...).", call. = FALSE)
  }

  estimated <- .fg_continuation_estimate(fg, probes, final_summary) * as.double(length(arms))
  estimated <- .fg_validate_estimate(estimated)
  if (is.null(.runner) && estimated > max_calls && !confirm) {
    stop("Estimated model outputs (", estimated, ") exceed `max_calls` (",
         as.integer(max_calls), "). Increase `max_calls` or set `confirm = TRUE`.",
         call. = FALSE)
  }
  if (!is.null(seed)) {
    withr::local_preserve_seed(.local_envir = seed_env)
    withr::local_seed(as.integer(seed), .local_envir = seed_env)
  }

  log <- fg$conversation_log
  fg$records$info$branch <- list(
    point = .fg_event_label(branch_event),
    prefix_messages = length(log),
    prefix_last_message_id = as.integer(log[[length(log)]]$message_id),
    prefix_calls = length(fg$records$calls),
    prefix_hash = .fg_prefix_hash(log))
  if (verbose) {
    cat("Continuing the recorded session from ", .fg_event_label(branch_event),
        " after ", length(log), " message(s)\n", sep = "")
  }
  setup <- list(fg = fg, flow = flow_name, message_mode = message_mode,
                n_participants = length(participant_ids), estimated_calls = estimated,
                probes = probes, final_summary = final_summary,
                run_meta = list(seed = seed,
                                on_desire_failure = .fg_flow_params(
                                  fg$turn_taking_flow)$on_desire_failure))
  .fg_execute_arms(setup, arms, verbose, run = function() {
    # A probe at the branch point is asked once, on the rebuilt state, so
    # that every arm shares its answers, as run_focus_group_arms() asks it
    # in the prefix of a new session.
    at_branch <- Filter(function(probe) branch_event %in% probe$events, plan$probes)
    if (length(at_branch)) {
      priv$with_records(for (probe in at_branch) priv$run_probe(probe, branch_event, verbose))
      fg$records$info$branch$prefix_calls <- length(fg$records$calls)
    }
    priv$branch_out(arms, probes, branch_event, verbose)
  })
}

# The model outputs one arm of a continued session can make, at most: the
# rest of the current item's turns, every later item's moderator line and
# turns, the call-ons from the current item on, the probes, and the final
# summary, with a desire score per participant before each turn the flow
# chooses.
.fg_continuation_estimate <- function(fg, probes, final_summary) {
  priv <- fg$.__enclos_env__$private
  state <- priv$state
  k <- state$item
  script <- fg$question_script
  later <- setdiff(seq_along(script), seq_len(k))
  phases <- vapply(script, function(item) as.character(item$phase %||% ""), character(1))
  verbatim <- vapply(script, function(item) isTRUE(item$verbatim), logical(1))
  n <- length(setdiff(names(fg$agents), fg$moderator_id))
  limit <- as.double(fg$max_participant_responses)
  now <- if (identical(state$stage, "turn")) max(0, limit - state$responses) else 0
  turns <- now + sum(phases[later] %in% .fg_response_phases) * limit
  desire <- if (inherits(fg$turn_taking_flow, "DesireBasedFlow")) turns * n else 0
  items <- fg$call_on_silent$items %||% integer()
  call_ons <- sum(items >= k) * as.double(n)
  probe_outputs <- sum(vapply(probes, function(p) {
    length(p$at) * as.double(length(p$participants %||% seq_len(n))) *
      (if (is.null(p$scale)) 1 else 2)
  }, numeric(1)))
  sum(!verbatim[later]) + turns + desire + call_ons + probe_outputs +
    (if (isTRUE(final_summary)) 1 else 0)
}

# The hash of a transcript prefix: the message ids, speakers, and texts.
.fg_prefix_hash <- function(log) {
  LLMR::llm_hash(lapply(log, function(m) {
    list(as.integer(m$message_id), as.character(m$speaker_id), as.character(m$text))
  }))
}

.fg_arms_result <- function(prefix, branches, setup, failure = NULL) {
  info <- prefix$records$info$branch %||% list()
  point <- info$point %||% NA_character_
  prefix_status <- attr(prefix, "session_status") %||%
    list(complete = FALSE, failure = NULL)
  attr(prefix, "session_status") <- prefix_status
  result <- function(fg, branch) {
    .fg_focus_group_result(fg, setup$flow, setup$message_mode,
                           setup$n_participants, setup$estimated_calls,
                           c(setup$run_meta, list(branch = branch)))
  }
  prefix_result <- result(prefix, c(list(arm = NA_character_, role = "prefix"), info))
  arm_results <- lapply(names(branches), function(name) {
    result(branches[[name]], c(list(arm = name, role = "arm"), info))
  })
  names(arm_results) <- names(branches)

  # Each arm's records begin with the prefix's; the rows after them are the
  # arm's own.
  rows <- function(component) {
    shared <- nrow(prefix_result[[component]])
    parts <- lapply(arm_results, function(r) {
      x <- r[[component]]
      x[setdiff(seq_len(nrow(x)), seq_len(shared)), , drop = FALSE]
    })
    do.call(rbind, c(list(prefix_result[[component]]), unname(parts)))
  }
  injections <- rows("injections")
  probes <- rows("probes")
  rownames(injections) <- NULL
  rownames(probes) <- NULL

  usage_of <- function(fg) c(sent = fg$total_tokens_sent %||% NA_real_,
                            rec = fg$total_tokens_received %||% NA_real_,
                            total = fg$total_tokens %||% NA_real_,
                            unknown = fg$calls_usage_unknown %||% NA_real_)
  base <- usage_of(prefix)
  total <- base
  for (b in branches) total <- total + (usage_of(b) - base)

  structure(list(
    arms = arm_results,
    prefix = prefix_result,
    branch_point = point,
    injections = injections,
    probes = probes,
    usage = as.list(total),
    metadata = list(
      topic = prefix$topic,
      branch_point = point,
      arms = names(branches),
      estimated_calls = as.integer(setup$estimated_calls),
      seed = if (is.null(setup$run_meta$seed)) NA_integer_ else as.integer(setup$run_meta$seed),
      prefix_messages = as.integer(info$prefix_messages %||% length(prefix$conversation_log)),
      prefix_hash = info$prefix_hash %||% NA_character_,
      complete = is.null(failure),
      failure = failure
    )
  ), class = c("focus_group_arms", "list"))
}

#' Print Focus Group Arms
#'
#' Prints the topic, the branch point, each arm's message count, and the
#' total token count.
#' @param x A `focus_group_arms` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.focus_group_arms <- function(x, ...) {
  cat("<focus_group_arms>\n")
  cat("Topic: ", x$metadata$topic, "\n", sep = "")
  cat("Branch point: ", x$branch_point, " after ", x$metadata$prefix_messages,
      " shared message(s)\n", sep = "")
  for (name in names(x$arms)) {
    complete <- isTRUE(x$arms[[name]]$metadata$complete)
    cat("  ", name, ": ", nrow(x$arms[[name]]$transcript), " messages",
        if (!complete) " (incomplete)", "\n", sep = "")
  }
  cat("Tokens: ", .fg_usage_text(x$usage$total, x$usage$unknown), "\n", sep = "")
  cat(.fg_usage_note(x$usage$unknown))
  invisible(x)
}
