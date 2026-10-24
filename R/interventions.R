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
#' id or handle. Any other `from` names an outside voice, such as
#' `"Guest expert"`, logged under that name and shown to the agents as
#' written; it may not be a participant id, a participant's handle, or
#' `"System"`. A moderator message that puts its last question to one
#' participant by handle ("P2, what do you make of that?") gives that
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
#' @param from `"moderator"` or the name of an outside speaker.
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
#' scale) is not read, and neither are numbers written in words ("seven").
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
# * digits joined to letters are not numbers ("P3", "V241", "2024s",
#   "7-point", "9pm"), nor, on a scale whose ends lie between -999 and 999,
#   are numbers of four or more digits, which are years or counts ("2024",
#   "1,000");
# * what remains must be one number, perhaps repeated, that is whole and lies
#   from `lo` to `hi`. No number, two different numbers ("3 or 4", "3-4"), a
#   fraction of a point ("4.5"), or a number outside the scale ("-2", "8")
#   gives NA.
# Numbers written in words ("seven") are not read.
.fg_parse_number <- function(text, lo, hi) {
  if (length(text) != 1L || is.na(text)) return(NA_real_)
  x <- tolower(as.character(text))
  x <- gsub("[‐-―−]", "-", x)
  x <- gsub("[*_`~#>|]", " ", x)
  x <- gsub("(?<=[0-9]),(?=[0-9]{3}\\b)", "", x, perl = TRUE)
  num <- "[0-9]+(?:\\.[0-9]+)?"
  start <- "(?<![a-z0-9.])"
  end <- "(?![a-z0-9]|\\.[0-9])"
  as_pattern <- function(v) gsub("-", "\\-", .fg_number_text(v), fixed = TRUE)
  lo_re <- paste0(start, as_pattern(lo), end)
  hi_re <- paste0(start, as_pattern(hi), end)
  aside <- "(?:\\s*\\([^()]*\\))?"
  range <- paste0(lo_re, aside, "\\s*(?:-|to|through|thru|and)\\s*", hi_re, aside)
  x <- gsub(range, " ", x, perl = TRUE)
  anchor <- "(?=\\s*(?:is\\b|means\\b|=))"
  if (grepl(paste0(lo_re, anchor), x, perl = TRUE) &&
      grepl(paste0(hi_re, anchor), x, perl = TRUE)) {
    x <- gsub(paste0("(", lo_re, "|", hi_re, ")", anchor), " ", x, perl = TRUE)
  }
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
#' @inheritParams run_focus_group
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
# failure anywhere becomes an error carrying what was completed.
.fg_execute_arms <- function(setup, arms, verbose) {
  fg <- setup$fg
  build <- function(prefix, branches, failure = NULL) {
    .fg_arms_result(prefix, branches, setup, failure)
  }
  run <- tryCatch(
    withr::with_options(list(focusgroup.message_mode = setup$message_mode),
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

  rows <- function(component, keep) {
    parts <- lapply(arm_results, function(r) {
      x <- r[[component]]
      x[keep(x, r), , drop = FALSE]
    })
    do.call(rbind, c(list(prefix_result[[component]][0, , drop = FALSE]),
                     unname(parts)))
  }
  injections <- rows("injections", function(x, r) rep(TRUE, nrow(x)))
  probes <- rbind(prefix_result$probes,
                  rows("probes", function(x, r) !is.na(x$arm)))
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
