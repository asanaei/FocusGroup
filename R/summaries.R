# File: R/summaries.R
# Purpose: Summaries of a discussion by participant, guide item,
# disagreement, trajectory, turn, or whole session, at several lengths. The
# model returns claims as JSON; each claim is checked against the transcript
# before any prose is rendered from it. fg_turns() is the deterministic turn
# table the summaries read.

.fg_summary_by <- c("participant", "item", "disagreements", "trajectory",
                    "turn", "session")
.fg_summary_levels <- c("headline", "brief", "detailed")
.fg_summary_statuses <- c("stated", "implied", "contradictory", "not_stated")
.fg_summary_kinds <- list(
  participant = c("position", "reason", "change", "reaction", "question",
                  "not_addressed", "other"),
  item = c("position", "agreement", "disagreement", "minority", "question",
           "not_addressed", "other"),
  disagreements = c("disagreement", "minority"),
  trajectory = c("stage", "change"),
  turn = "gist",
  session = c("position", "agreement", "disagreement", "minority", "change",
              "question", "silence", "other"))
.fg_summary_acts <- c("question", "answer", "agreement", "disagreement",
                      "elaboration", "experience", "concession", "procedure",
                      "other")
.fg_summary_endings <- c("concession", "moved_on", "unresolved")
# Claims that on_fail = "drop" may not drop: losing one would hide dissent or
# change.
.fg_summary_protected <- c("disagreement", "minority", "change")
.fg_summary_template_names <- c("summary_participant", "summary_item",
  "summary_disagreements", "summary_trajectory", "summary_turn",
  "summary_session", "summary_reask")
.fg_summary_budgets <- list(
  default = c(headline = "one sentence", brief = "100-120 words",
              detailed = "longer, with quotes"),
  turn = c(headline = "20 words per turn", brief = "40 words per turn",
           detailed = "a quote and an act code per turn"))
.fg_finish_truncated <- c("length", "max_tokens", "max_output_tokens")

#' Summarize a Discussion by Participant, Item, Disagreement, Trajectory,
#' Turn, or Session
#'
#' Summarizes a finished or imported discussion with a model, in one of six
#' units and at one or more lengths. The model is asked for claims as JSON:
#' each claim states one point, its status, the messages that support it, and
#' any words quoted from them. Every claim is checked against the transcript
#' before it is used, and the prose of the summary is rendered from the claims
#' that pass. No message is added to the session and no agent sees the
#' summary.
#'
#' @section Units:
#' \describe{
#'   \item{`"participant"`}{One call per participant: positions and reasons,
#'     changes of position, reactions to others, and guide items not
#'     addressed. A participant with no turns gets a row with `n_turns = 0`
#'     and no call.}
#'   \item{`"item"`}{One call per guide item with participant turns, sending
#'     only that item's messages: positions and their holders, agreement and
#'     disagreement. Who spoke, and how often, is counted in code and given to
#'     the model.}
#'   \item{`"disagreements"`}{One call per session: one entry per
#'     disagreement or position held by one participant alone, with its
#'     holders, its messages, and how it ended (a concession, the moderator
#'     moving on, or unresolved).}
#'   \item{`"trajectory"`}{One call per session. The stages are fixed by the
#'     package: thirds of the participant turns at `"headline"`, one stage
#'     per guide item at `"brief"` and `"detailed"`.}
#'   \item{`"turn"`}{One call per guide item (an item without participant
#'     turns joins the next one): a gist of every message, with an act code at
#'     `"detailed"`.}
#'   \item{`"session"`}{One call per session for the whole group.}
#' }
#' For a `focus_group_arms` object the shared prefix is summarized once and
#' each arm from the branch point on; an arm's call sees only the messages
#' after the branch.
#'
#' @section Lengths:
#' `compression` names the lengths: `"headline"` (one sentence per unit, 20
#' words per turn), `"brief"` (a paragraph of about 100 to 120 words; 40 words
#' per turn), and `"detailed"` (longer, with quotes). All the lengths asked for
#' come back in one call per unit, as lists of claims drawn from one set, so
#' they agree with each other. The word budgets are recorded beside each
#' summary; they are not enforced.
#'
#' @section What the model sees:
#' The transcript, with each message's id and its speaker's handle, the label
#' the agents saw. No user ID labels a speaker: a speaker whose handle is its
#' user ID (an imported transcript, `handles = "ids"`, or a session made
#' before handles existed) is shown as `P1`, `P2`, ... in roster order, unless
#' that ID already has this form or another voice uses it, and the moderator
#' as `Moderator`. The messages themselves are sent as recorded: the speakers
#' wrote them under the handles they saw, and a name in them ("Ana, why
#' evenings?", "Will the library stay open in May?") is left as written.
#' Personas are never sent, nor is the System roster row.
#' Injected messages are marked as injected, and those shown to participants
#' only are marked as such. Private probe answers are sent only with
#' `include_probes = TRUE`, as a separate block with ids `Q1`, `Q2`, ...
#'
#' @section Checks:
#' Each reply is checked in code, with no model involved. The JSON must parse
#' (the reply is the first complete JSON object with a `claims` field, in a
#' code block or in the text; only the final answer counts, so reasoning a
#' model writes into its reply is ignored) and the reply must not have been
#' cut off at the token limit. Every message a claim cites must exist, lie
#' within the unit, and not be the System row. The messages given as evidence
#' must be by the speakers the claim names, and a claim other than
#' `"not_stated"` must cite at least one message by each speaker it names.
#'
#' A quote must appear in the text of the cited message,
#' ignoring only differences of case, whitespace, and quotation marks (and,
#' for Persian, the zero-width non-joiner and the Arabic forms of yeh and
#' kaf). It must have at least 3 words and start and end on word boundaries,
#' an apostrophe or hyphen between letters counting as part of a word. The
#' parts of a quote joined by `"..."` must appear in order in the same
#' message, each with at least 2 words, and the words left out between them
#' may not include a negation such as "not" or "n't". Words a claim puts in
#' double quotation marks in its text must likewise appear in a message it
#' cites as evidence.
#'
#' A claim of change must cite two messages of one speaker, earlier first.
#' Every handle of the form `P` and a number in a claim must be on the
#' roster; a participant the text names must be among the claim's speakers
#' or the speaker of a message it cites as context; a claim that is not
#' `"not_stated"` may not name a participant with no message in the unit; a
#' claim that a participant said nothing fails when that participant spoke;
#' and a user ID or name the model was not shown, as a speaker's label or in
#' the text of a message, may not appear. A claim that
#' speaks for the whole group ("everyone", "all of them", "no one",
#' "consensus", "unanimous", "the whole group", and similar wording) must cite
#' a message from every participant who spoke on each item it cites. Unless
#' `include_probes = TRUE`, no claim may share a run of 6 words with a probe
#' answer that does not also occur in the transcript.
#'
#' Coverage is counted over the claims that pass: every participant, every
#' participant who spoke on the item, every message, or every stage, as the
#' unit requires, and every length asked for. A participant counts as covered
#' only by a claim that reports what they said, with a message of theirs as
#' evidence. The prose shows the status of a claim that is not `"stated"` at
#' every length.
#'
#' A reply that fails is sent back once, with each failure stated (for
#' instance, that a quote is not in message 14, followed by the text of
#' message 14); after a reply cut off at the token limit, the limit is
#' doubled for the re-ask. If the second reply fails too, `on_fail =
#' "error"` raises an error of class `focusgroup_summary_check_failure` with
#' fields `checks` (the checks of both replies), `replies` (both raw
#' replies), `segment`, `unit`, and `partial` (the summary of the units
#' completed before it). `on_fail = "drop"` instead moves the failing claims
#' to `rejected` and renders the summary from the rest, but still raises the
#' error when a dropped claim reports a disagreement, a position held by one
#' participant alone, or a change, or when the reply as a whole fails
#' (invalid JSON, a cut-off reply, missing lengths, or coverage that the
#' claims kept do not give).
#'
#' @section Calls and structured output:
#' Calls are made one at a time, with call types `"summary_<by>"`, and are
#' recorded in the `calls` component of the result, never in the session's
#' records; the random-number state is the same after the call as before.
#' The estimate checked against `max_calls` counts one re-ask per unit, an
#' upper bound. A unit whose prompt is longer than the prompt window (64,000
#' estimated tokens, counting bytes for text outside ASCII) stops the run
#' before any call, with an error of class
#' `focusgroup_summary_input_too_long`; `by = "item"` sends one guide item at
#' a time. The token limit is written into the field the configuration uses
#' (`max_completion_tokens`, `max_output_tokens`, or `max_tokens`). When the
#' installed LLMR provides [LLMR::enable_structured_output()], the request
#' also carries the JSON schema of the reply, unless the configuration already
#' sets a response format or is an Anthropic configuration with thinking
#' enabled, which cannot take a forced tool call; set
#' `options(focusgroup.summary_structured_output = FALSE)` for a model that
#' rejects a schema. Otherwise the format is stated in the prompt alone.
#' Either way every reply is validated locally. The schema and the mode are
#' recorded in `metadata$structured_output`. No temperature or seed is set.
#'
#' @param x A `focus_group_result` from [run_focus_group()], a
#'   `focus_group_arms` object from [run_focus_group_arms()], a
#'   [FocusGroup], including one made by [focus_group_from_transcript()], or
#'   the list that `FocusGroup$run_arms()` returns (or its named list of
#'   arms), summarized as a `focus_group_arms` object is.
#' @param config An explicit `llm_config` object for the summarizing model.
#'   The session's own models and runner are never used.
#' @param by The unit: one of `"participant"`, `"item"`, `"disagreements"`,
#'   `"trajectory"`, `"turn"`, or `"session"`.
#' @param compression One or more of `"headline"`, `"brief"`, and
#'   `"detailed"`.
#' @param include_probes Logical. Send private probe answers to the model as
#'   a separate, labeled block.
#' @param on_fail `"error"` or `"drop"`; see Checks.
#' @param max_calls Maximum estimated model calls allowed without `confirm`.
#' @param confirm Logical. Permit a run whose estimate exceeds `max_calls`.
#' @param max_tokens Optional token limit for each reply. By default, the
#'   configuration's own limit, or else 1000, 2000, or 3000 tokens for the
#'   longest length asked for, with 120 more per message under `by = "turn"`
#'   and 300 more per stage under `by = "trajectory"`.
#' @param reasoning_effort Optional reasoning effort for every summary call,
#'   one string, written over the configuration's setting.
#' @param templates Optional named list of templates replacing the defaults
#'   of the same name: `summary_participant`, `summary_item`,
#'   `summary_disagreements`, `summary_trajectory`, `summary_turn`,
#'   `summary_session`, and `summary_reask` (see
#'   [get_default_prompt_templates()]). A unit template must contain
#'   `\{\{transcript\}\}` and `\{\{format\}\}`, and `summary_reask` must
#'   contain `\{\{failures\}\}`; a placeholder without a value is an error.
#' @param verbose Logical. Print one line per call.
#' @param .runner Optional function used instead of live model calls. It
#'   receives a data frame with `config` and `messages` list-columns and
#'   returns the rows with at least `response_text`.
#' @return A `focus_group_summary`, a list with:
#' \describe{
#'   \item{summaries}{One row per unit and length: `segment` (`"session"`,
#'     or `"prefix"` and the arm names), `unit`, `level`, `text` (rendered
#'     from the claims kept; `NA` when there is none), `n_claims`, `words`,
#'     `budget`, `n_turns`, `speaker_id` (the user ID, for participant and
#'     turn units), and `note`.}
#'   \item{claims}{The claims kept, one row each: `segment`, `unit`,
#'     `claim`, `kind`, `status`, `text`, `speakers` (handles as the model
#'     saw them), `speaker_ids`, `evidence`, `context`, `probe_evidence`,
#'     `act`, `ending`, `ending_message_id`, `levels`, and `attempt`.}
#'   \item{quotes}{The quotes of the claims kept, each cut from its source:
#'     `text` as the model gave it, `verbatim` as it appears in the message,
#'     and `start` and `end`, character positions in the message text, with
#'     `spans` giving each part of a quote joined by "...".}
#'   \item{checks}{Every check run, one row per check, claim, and attempt,
#'     with `passed` and, on failure, `detail`.}
#'   \item{rejected}{Claims dropped under `on_fail = "drop"`, with the
#'     reasons.}
#'   \item{calls}{One row per model call, in the format of the `calls`
#'     component of [run_focus_group()], with `segment` and `unit`.}
#'   \item{turns}{The table from [fg_turns()], with `label`, the handle the
#'     model saw.}
#'   \item{metadata}{`by`, `compression`, `include_probes`, `on_fail`,
#'     `provider`, `model`, `settings`, `max_tokens`, `reasoning_effort`,
#'     `structured_output`, `templates`, `handles` (user ID, handle, and the
#'     label sent to the model), `segments`, `source_hash`, `units`,
#'     `estimated_calls`, `max_calls`, `budgets`, `usage`, `versions`,
#'     `created`, `complete`, and `failure`.}
#' }
#' `as.data.frame()` returns `summaries`, or another table named by its
#' `component` argument.
#' @seealso [fg_turns()]; the `summarize()` method of [FocusGroup] for the
#'   interim summary that agents read during a session.
#' @examples
#' transcript <- data.frame(
#'   speaker = c("Moderator", "Ana", "Ben"),
#'   text = c("What should change about the library hours?",
#'            "Evening hours would help working parents like me.",
#'            "I disagree, mornings are what retirees need most.")
#' )
#' fg <- focus_group_from_transcript(transcript, topic = "Library hours")
#' # A scripted model reply for each participant, as a live model would give.
#' scripted <- function(experiments, ...) {
#'   prompt <- experiments$messages[[1]][[1]]$content
#'   unit <- if (grepl("participant, P1", prompt, fixed = TRUE)) "P1" else "P2"
#'   claim <- if (unit == "P1") {
#'     list(id = 1, unit = "P1", kind = "position", status = "stated",
#'          text = "P1 wants evening hours for working parents.",
#'          speakers = list("P1"), evidence = list(2), context = list(1),
#'          quotes = list(list(message_id = 2, speaker = "P1",
#'                             text = "Evening hours would help")))
#'   } else {
#'     list(id = 1, unit = "P2", kind = "position", status = "stated",
#'          text = "P2 disagrees and puts retirees' mornings first.",
#'          speakers = list("P2"), evidence = list(3), context = list(2),
#'          quotes = list(list(message_id = 3, speaker = "P2",
#'                             text = "mornings are what retirees need")))
#'   }
#'   experiments$response_text <- as.character(jsonlite::toJSON(
#'     list(claims = list(claim), levels = list(headline = list(1))),
#'     auto_unbox = TRUE))
#'   experiments
#' }
#' cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
#' out <- summarize_focus_group(fg, cfg, compression = "headline",
#'                              .runner = scripted)
#' out
#' out$quotes[, c("unit", "message_id", "verbatim")]
#' @export
summarize_focus_group <- function(x, config, by = "participant",
                                  compression = "brief",
                                  include_probes = FALSE,
                                  on_fail = c("error", "drop"),
                                  max_calls = 30L, confirm = FALSE,
                                  max_tokens = NULL, reasoning_effort = NULL,
                                  templates = NULL, verbose = FALSE,
                                  .runner = NULL) {
  if (missing(config)) config <- NULL
  .fg_validate_config(config)
  if (!is.character(by) || length(by) != 1L || is.na(by) ||
      !by %in% .fg_summary_by) {
    stop("`by` must be one of ", paste0("\"", .fg_summary_by, "\"", collapse = ", "),
         ".", call. = FALSE)
  }
  if (!is.character(compression) || !length(compression) || anyNA(compression) ||
      any(!compression %in% .fg_summary_levels) || anyDuplicated(compression)) {
    stop("`compression` must name distinct lengths among \"headline\", ",
         "\"brief\", and \"detailed\".", call. = FALSE)
  }
  levels <- .fg_summary_levels[.fg_summary_levels %in% compression]
  on_fail <- match.arg(on_fail)
  for (flag in c("include_probes", "confirm", "verbose")) {
    value <- get(flag)
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", flag, "` must be `TRUE` or `FALSE`.", call. = FALSE)
    }
  }
  if (!is.numeric(max_calls) || length(max_calls) != 1L || is.na(max_calls) ||
      !is.finite(max_calls) || max_calls < 1 || max_calls != floor(max_calls) ||
      max_calls > .Machine$integer.max) {
    stop("`max_calls` must be one finite positive integer.", call. = FALSE)
  }
  if (!is.null(max_tokens) &&
      (!is.numeric(max_tokens) || length(max_tokens) != 1L || is.na(max_tokens) ||
       !is.finite(max_tokens) || max_tokens < 1 || max_tokens != floor(max_tokens))) {
    stop("`max_tokens` must be `NULL` or one positive whole number.", call. = FALSE)
  }
  if (!is.null(reasoning_effort) &&
      (!is.character(reasoning_effort) || length(reasoning_effort) != 1L ||
       is.na(reasoning_effort) || !nzchar(trimws(reasoning_effort)))) {
    stop("`reasoning_effort` must be `NULL` or one non-empty string.", call. = FALSE)
  }
  if (!is.null(.runner) && !is.function(.runner)) {
    stop("`.runner` must be `NULL` or a function.", call. = FALSE)
  }
  used_templates <- .fg_summary_templates(templates)

  source <- .fg_summary_source(x)
  plan <- .fg_summary_plan(source, by, levels, include_probes, used_templates,
                           max_tokens, config)
  called <- Filter(function(u) isTRUE(u$call), plan)

  # The prompt window and the budget are checked before any call.
  for (unit in called) {
    # Characters outside ASCII take more tokens; bytes bound them better.
    tokens <- max(estimate_tokens(unit$prompt), ceiling(nchar(unit$prompt, "bytes") / 4))
    if (tokens > .fg_prompt_window_tokens) {
      advice <- if (by %in% c("item", "turn")) {
        "A single guide item is too long to send; shorten the transcript before summarizing it."
      } else {
        "Use `by = \"item\"` to send one guide item at a time."
      }
      rlang::abort(sprintf(paste0(
        "The prompt for %s is about %s tokens, longer than the prompt window ",
        "of %s tokens. %s"), .fg_unit_name(unit),
        format(tokens, big.mark = ","), format(.fg_prompt_window_tokens, big.mark = ","),
        advice), class = "focusgroup_summary_input_too_long",
        segment = unit$segment, unit = unit$key, tokens = tokens,
        window = .fg_prompt_window_tokens)
    }
  }
  estimated_calls <- 2L * length(called)
  if (is.null(.runner) && estimated_calls > max_calls && !confirm) {
    stop(
      "Estimated model calls (", estimated_calls, ": ", length(called),
      " unit(s), each with at most one re-ask) exceed `max_calls` (",
      as.integer(max_calls), "). Increase `max_calls` or set `confirm = TRUE`.",
      call. = FALSE
    )
  }

  call_config <- .fg_with_params(config, if (is.null(reasoning_effort)) list() else
    list(reasoning_effort = reasoning_effort))
  schema <- .fg_summary_schema(by, levels, include_probes)
  structured <- .fg_summary_structured(call_config, schema)

  meta <- list(
    by = by, compression = levels, include_probes = include_probes,
    on_fail = on_fail,
    provider = config$provider %||% NA_character_,
    model = config$model %||% NA_character_,
    settings = .fg_call_settings(call_config),
    max_tokens = vapply(plan, function(u) as.numeric(u$max_tokens %||% NA_real_), numeric(1)),
    reasoning_effort = reasoning_effort,
    structured_output = list(mode = structured$mode, schema = schema),
    templates = used_templates,
    handles = source$handles,
    segments = vapply(source$segments, `[[`, character(1), "name"),
    source_hash = source$hash,
    units = length(plan),
    estimated_calls = as.integer(estimated_calls),
    max_calls = as.integer(max_calls),
    budgets = .fg_summary_budgets[[if (identical(by, "turn")) "turn" else "default"]][levels],
    versions = .fg_versions(),
    created = as.POSIXct(Sys.time(), tz = "UTC"))
  names(meta$max_tokens) <- vapply(plan, .fg_unit_name, character(1))

  store <- new.env(parent = emptyenv())
  store$calls <- list()
  outcomes <- vector("list", length(plan))
  build <- function(complete = TRUE, failure = NULL) {
    .fg_summary_result(plan, outcomes, store, source, meta, complete, failure)
  }
  n_called <- 0L
  for (i in seq_along(plan)) {
    unit <- plan[[i]]
    if (!isTRUE(unit$call)) next
    n_called <- n_called + 1L
    if (verbose) {
      cat(sprintf("Summarizing %s (%d of %d)...\n", .fg_unit_name(unit), n_called,
                  length(called)))
    }
    run <- tryCatch(
      .fg_summary_run_unit(unit, structured$config, .runner, store,
                           used_templates$summary_reask),
      error = function(e) e)
    if (inherits(run, "error")) {
      rlang::abort(sprintf("The summary call for %s failed: %s", .fg_unit_name(unit),
                           conditionMessage(run)),
                   class = "focusgroup_summary_call_failure",
                   segment = unit$segment, unit = unit$key,
                   partial = build(FALSE, list(segment = unit$segment, unit = unit$key,
                                                message = conditionMessage(run))),
                   parent = run)
    }
    final <- run$attempts[[length(run$attempts)]]$verdict
    if (!isTRUE(final$passed)) {
      dropped_kinds <- unique(unlist(lapply(final$claims, function(cl) {
        if (length(cl$failures)) cl$kind else NULL
      })))
      protected <- intersect(dropped_kinds, .fg_summary_protected)
      if (identical(on_fail, "error") || isTRUE(final$fatal) || length(protected)) {
        checks <- do.call(rbind, lapply(seq_along(run$attempts), function(k) {
          .fg_checks_frame(run$attempts[[k]]$verdict$checks, unit, k)
        }))
        reason <- if (identical(on_fail, "drop") && !isTRUE(final$fatal) && length(protected)) {
          sprintf(" A failing claim reports %s, which on_fail = \"drop\" does not drop.",
                  paste(protected, collapse = " and "))
        } else ""
        shown <- utils::head(final$failures, 3L)
        rlang::abort(
          sprintf("The summary of %s failed %d check(s) after a re-ask: %s%s%s",
                  .fg_unit_name(unit), length(final$failures),
                  paste(shown, collapse = " "),
                  if (length(final$failures) > 3L) " ..." else "", reason),
          class = "focusgroup_summary_check_failure",
          checks = checks,
          replies = vapply(run$attempts, `[[`, character(1), "raw"),
          segment = unit$segment, unit = unit$key,
          partial = build(FALSE, list(segment = unit$segment, unit = unit$key,
                                       failures = final$failures)))
      }
    }
    outcomes[[i]] <- run
  }
  tryCatch(build(), error = function(e) {
    rlang::abort(paste("The summary could not be assembled from its replies:",
                       conditionMessage(e)),
                 class = "focusgroup_summary_build_failure",
                 calls = .fg_calls_data(store$calls), parent = e)
  })
}

#' Turn Table of a Discussion
#'
#' One row per logged message, in order, with what the records say about how
#' the speaker came to have the floor. No model is called. The System roster
#' row is left out.
#'
#' `link` is `"addressed"` when the moderator's preceding line named the
#' participant, `"called_on"` when the moderator called on a participant who
#' had been silent, and otherwise `"sequence"`: the message simply follows the
#' one before it, which says nothing about whom it answers. For the first two,
#' `linked_message_id` is the moderator's line (the routing record's
#' `trigger_message_id`); for `"sequence"`, the previous message.
#'
#' @param x A `focus_group_result`, a `focus_group_arms` object, a
#'   [FocusGroup], or the list that `FocusGroup$run_arms()` returns, as for
#'   [summarize_focus_group()].
#' @return A data frame with `segment` (`"session"`, or `"prefix"` and the
#'   arm names; an arm's rows start after the branch point), `message_id`,
#'   `item`, `round`, `phase`, `speaker_id`, `handle`, `is_moderator`,
#'   `injected`, `visibility` (`"shared"` or `"participants"`), `verbatim`,
#'   `words`, `rule` (the routing rule that chose a participant turn),
#'   `candidates` and `excluded` (list-columns from the routing record),
#'   `trigger_message_id`, `link`, and `linked_message_id`.
#'
#'   Results made before guide items and handles were recorded take `item`
#'   from `round` (or, for an imported transcript without rounds, start a new
#'   item at each moderator message, with item 0 before the first) and
#'   `handle` from `speaker_id`.
#' @seealso [summarize_focus_group()].
#' @examples
#' transcript <- data.frame(
#'   speaker = c("Moderator", "Ana", "Ben", "Moderator", "Ana"),
#'   text = c("What should change?", "Longer evening hours.",
#'            "More weekend staff.", "Ana, why evenings?",
#'            "I work days.")
#' )
#' fg_turns(focus_group_from_transcript(transcript))
#' @export
fg_turns <- function(x) {
  source <- .fg_summary_source(x)
  out <- do.call(rbind, lapply(source$segments, .fg_segment_turns))
  rownames(out) <- NULL
  out
}

# ---- Reading the records --------------------------------------------------------

# The sessions a summary reads: one segment for a single session, or the
# shared prefix and each arm's continuation after the branch point.
.fg_summary_source <- function(x) {
  branches <- if (!inherits(x, c("focus_group_arms", "focus_group_result", "FocusGroup"))) {
    .fg_branch_list(x)
  }
  if (inherits(x, "focus_group_arms")) {
    m <- x$metadata$prefix_messages
    if (!is.numeric(m) || length(m) != 1L || is.na(m) || m < 0) {
      stop("`x$metadata$prefix_messages` must give the number of messages ",
           "before the branch point.", call. = FALSE)
    }
    if (!inherits(x$prefix, "focus_group_result") || !length(x$arms) ||
        !all(vapply(x$arms, inherits, logical(1), "focus_group_result"))) {
      stop("`x` must hold a `prefix` result and a named list of `arms` results.",
           call. = FALSE)
    }
    prefix <- .fg_source_session(x$prefix)
    arms <- x$arms
  } else if (!is.null(branches)) {
    m <- branches$prefix_messages
    arms <- branches$arms
    prefix <- if (is.null(branches$prefix)) {
      .fg_session_head(.fg_source_session(arms[[1]]), m)
    } else .fg_source_session(branches$prefix)
  }
  if (inherits(x, "focus_group_arms") || !is.null(branches)) {
    arm_sessions <- lapply(arms, .fg_source_session)
    cut <- if (m >= 1 && m <= nrow(prefix$transcript)) prefix$transcript$message_id[m] else
      as.integer(m)
    segments <- list(.fg_source_segment(prefix, "prefix", NA_character_,
                                        seq_len(nrow(prefix$transcript)),
                                        prefix$probes))
    for (name in names(arms)) {
      arm <- arm_sessions[[name]]
      rows <- seq_len(nrow(arm$transcript))
      probes <- arm$probes[!is.na(arm$probes$arm) & arm$probes$arm == name, , drop = FALSE]
      segments[[length(segments) + 1L]] <- .fg_source_segment(
        arm, name, name, rows[rows > m], probes,
        note = sprintf(paste0("These messages continue a discussion after message #%d. ",
                              "The earlier messages are summarized separately."),
                       as.integer(cut)))
    }
    sessions <- c(list(prefix), unname(arm_sessions))
  } else if (inherits(x, c("focus_group_result", "FocusGroup"))) {
    session <- .fg_source_session(x)
    segments <- list(.fg_source_segment(session, "session", NA_character_,
                                        seq_len(nrow(session$transcript)),
                                        session$probes))
    sessions <- list(session)
  } else {
    stop("`x` must be a focus_group_result, a focus_group_arms object, a ",
         "FocusGroup, or the list FocusGroup$run_arms() returns.", call. = FALSE)
  }
  handles <- unique(do.call(rbind, lapply(segments, function(s) s$handles)))
  rownames(handles) <- NULL
  all_messages <- do.call(rbind, lapply(sessions, function(s) {
    s$transcript[!s$transcript$system, c("message_id", "text"), drop = FALSE]
  }))
  all_probes <- unlist(lapply(sessions, function(s) s$probes$answer))
  list(segments = segments, handles = handles,
       hash = LLMR::llm_hash(lapply(segments, function(s) {
         list(s$name, s$transcript$message_id, s$transcript$speaker_id, s$transcript$text)
       })),
       transcript_ngrams = unique(unlist(lapply(all_messages$text, .fg_ngrams))),
       probe_ngrams = unique(unlist(lapply(all_probes[!is.na(all_probes)], .fg_ngrams))))
}

# The list FocusGroup$run_arms() returns (`prefix`, a FocusGroup stopped at
# the branch point, and `arms`, the FocusGroup of each arm, named by arm), or
# its `arms` alone, as fg_player() takes them; NULL for anything else. The
# arms must name one branch point and one prefix. Returns the prefix (NULL
# when only the arms are given), the arms, and the number of messages before
# the branch point.
.fg_branch_list <- function(x) {
  if (!is.list(x) || is.object(x) || !length(x)) return(NULL)
  is_group <- function(v) inherits(v, "FocusGroup")
  prefix <- NULL
  if (is.list(x$arms) && length(x$arms) && all(vapply(x$arms, is_group, logical(1))) &&
      (is.null(x$prefix) || is_group(x$prefix))) {
    prefix <- x$prefix
    arms <- x$arms
  } else if (all(vapply(x, is_group, logical(1)))) {
    arms <- x
  } else {
    return(NULL)
  }
  names <- names(arms)
  if (is.null(names) || anyNA(names) || any(!nzchar(names)) || anyDuplicated(names)) {
    stop("Every arm must have a distinct, non-empty name.", call. = FALSE)
  }
  branch <- lapply(c(if (!is.null(prefix)) list(prefix), arms), function(fg) {
    fg$records$info$branch %||% list()
  })
  counts <- vapply(branch, function(b) as.numeric(b$prefix_messages %||% NA_real_), numeric(1))
  hashes <- vapply(branch, function(b) as.character(b$prefix_hash %||% NA_character_),
                   character(1))
  if (anyNA(counts) || length(unique(counts)) != 1L || length(unique(hashes)) != 1L) {
    stop("The arms do not record one shared prefix; pass the list that ",
         "FocusGroup$run_arms() returns.", call. = FALSE)
  }
  list(prefix = prefix, arms = arms, prefix_messages = counts[[1]])
}

# The first `m` messages of a session read from an arm, with the probes asked
# before the branch point: the shared prefix.
.fg_session_head <- function(session, m) {
  session$transcript <- session$transcript[seq_len(min(m, nrow(session$transcript))), ,
                                           drop = FALSE]
  probes <- session$probes
  session$probes <- probes[is.na(probes$arm), , drop = FALSE]
  session
}

# One session's records, read through the record contract: the transcript
# with `item` and `handle`, the roster with handles, routing with
# `trigger_message_id`, and probes. Records made before these fields existed
# fall back as the contract states.
.fg_source_session <- function(x) {
  if (inherits(x, "FocusGroup")) {
    log <- x$conversation_log
    transcript <- .fg_transcript_data(log)
    if (!"item" %in% names(transcript) &&
        any(vapply(log, function(m) !is.null(m$item), logical(1)))) {
      transcript$item <- vapply(log, function(m) as.integer(m$item %||% NA_integer_)[1],
                                integer(1))
    }
    if (!"handle" %in% names(transcript) &&
        any(vapply(log, function(m) !is.null(m$handle), logical(1)))) {
      transcript$handle <- vapply(log, function(m) as.character(m$handle %||% NA_character_)[1],
                                  character(1))
    }
    records <- x$records %||% .fg_empty_records()
    routing <- .fg_routing_data(records$routing)
    if (!"trigger_message_id" %in% names(routing)) {
      routing$trigger_message_id <- .fg_record_column(records$routing,
                                                      "trigger_message_id", "integer")
    }
    probes <- .fg_probes_data(records$probes)
    ids <- names(x$agents)
    roster <- data.frame(
      id = ids,
      is_moderator = ids == (x$moderator_id %||% ""),
      handle = vapply(x$agents, function(a) {
        value <- if ("handle" %in% names(a)) a$handle else NULL
        as.character(value %||% NA_character_)[1]
      }, character(1)),
      stringsAsFactors = FALSE)
    topic <- x$topic %||% NA_character_
  } else {
    transcript <- x$transcript
    if (!is.data.frame(transcript)) {
      stop("`x$transcript` must be a data frame.", call. = FALSE)
    }
    routing <- x$routing
    if (!is.data.frame(routing)) routing <- .fg_routing_data(list())
    if (!"trigger_message_id" %in% names(routing)) {
      routing$trigger_message_id <- rep(NA_integer_, nrow(routing))
    }
    probes <- x$probes
    if (!is.data.frame(probes)) probes <- .fg_probes_data(list())
    participants <- x$participants
    if (!is.data.frame(participants) || !all(c("id", "is_moderator") %in% names(participants))) {
      stop("`x$participants` must be a data frame with `id` and `is_moderator`.",
           call. = FALSE)
    }
    handle <- if ("handle" %in% names(participants)) {
      as.character(participants$handle)
    } else {
      map <- x$metadata$handles
      if (is.data.frame(map) && all(c("agent_id", "handle") %in% names(map))) {
        as.character(map$handle[match(participants$id, map$agent_id)])
      } else rep(NA_character_, nrow(participants))
    }
    roster <- data.frame(id = as.character(participants$id),
                         is_moderator = as.logical(participants$is_moderator),
                         handle = handle, stringsAsFactors = FALSE)
    topic <- x$metadata$topic %||% NA_character_
  }
  missing <- setdiff(c("message_id", "speaker_id", "text", "is_moderator"),
                     names(transcript))
  if (length(missing)) {
    stop("The transcript lacks the column(s) ", paste(missing, collapse = ", "), ".",
         call. = FALSE)
  }
  list(transcript = .fg_source_transcript(transcript, roster), roster = roster,
       routing = routing, probes = probes, topic = as.character(topic)[1])
}

# The transcript in the columns the summaries use. `item` falls back to
# `round`; when rounds are missing, a new item starts at each moderator
# message, with item 0 before the first. `handle` falls back to the roster's
# handle and then to `speaker_id`.
.fg_source_transcript <- function(transcript, roster) {
  n <- nrow(transcript)
  speaker <- as.character(transcript$speaker_id)
  system <- speaker == "System"
  moderator <- as.logical(transcript$is_moderator) %in% TRUE
  round <- if ("round" %in% names(transcript)) as.integer(transcript$round) else
    rep(NA_integer_, n)
  item <- if ("item" %in% names(transcript)) as.integer(transcript$item) else NULL
  if (is.null(item)) {
    item <- if (!anyNA(round[!system])) round else as.integer(cumsum(moderator & !system))
  }
  item[system] <- 0L
  if (anyNA(item)) {
    # A message without an item belongs to the item before it.
    for (i in which(is.na(item))) item[i] <- if (i == 1L) 0L else item[i - 1L]
  }
  metadata <- if ("metadata" %in% names(transcript)) transcript$metadata else
    rep(list(list()), n)
  metadata <- lapply(metadata, function(m) if (is.list(m)) m else list())
  injected <- vapply(metadata, function(m) isTRUE(m$injected), logical(1))
  visibility <- vapply(metadata, function(m) {
    value <- m$injection$visibility
    if (is.character(value) && length(value) == 1L && !is.na(value)) value else "shared"
  }, character(1))
  handle <- if ("handle" %in% names(transcript)) as.character(transcript$handle) else
    rep(NA_character_, n)
  from_roster <- roster$handle[match(speaker, roster$id)]
  handle[is.na(handle)] <- from_roster[is.na(handle)]
  handle[is.na(handle)] <- speaker[is.na(handle)]
  handle[system] <- "System"
  text <- as.character(transcript$text)
  text[is.na(text)] <- ""
  data.frame(
    message_id = as.integer(transcript$message_id),
    item = item, round = round,
    phase = if ("phase" %in% names(transcript)) as.character(transcript$phase) else
      rep(NA_character_, n),
    speaker_id = speaker, handle = handle, is_moderator = moderator,
    text = text, injected = injected, visibility = visibility,
    verbatim = vapply(metadata, function(m) isTRUE(m$verbatim), logical(1)),
    system = system, stringsAsFactors = FALSE)
}

# The labels the summarizing model sees. A handle that differs from the
# speaker's user ID is what the agents saw and is kept. Where the handle is
# the user ID itself, the moderator is "Moderator" and a participant keeps an
# ID of the form P1, P2, ... and is otherwise given the next free one, in
# roster order, so that no user ID labels a speaker. `reserved` holds the
# names of other voices, such as an injection's sender, which keep their
# label and which no new label may take.
.fg_summary_labels <- function(roster, reserved = character()) {
  label <- roster$handle
  own <- is.na(label) | label == roster$id
  label[roster$is_moderator & own] <- "Moderator"
  participants <- which(!roster$is_moderator)
  neutral <- grepl("^P[0-9]+$", roster$id)
  keep <- participants[own[participants] & neutral[participants]]
  label[keep] <- roster$id[keep]
  taken <- c(label[!is.na(label) & !(own & !roster$is_moderator & !neutral)], reserved)
  k <- 0L
  for (i in participants[own[participants] & !neutral[participants]]) {
    repeat {
      k <- k + 1L
      candidate <- paste0("P", k)
      if (!candidate %in% taken) break
    }
    label[i] <- candidate
    taken <- c(taken, candidate)
  }
  clash <- c(label[duplicated(label)], intersect(label, reserved))
  if (length(clash)) {
    stop("Two speakers would share the label ", clash[1],
         "; give them distinct handles.", call. = FALSE)
  }
  label
}

# The names that label no speaker in the summary: user IDs and handles that
# differ from the label the speaker is shown under, each mapped to that
# label. Message text is never rewritten; a claim that uses one of these
# names is checked against the text the model saw.
.fg_other_names <- function(handles) {
  names <- c(handles$speaker_id, handles$handle)
  labels <- c(handles$label, handles$label)
  keep <- !is.na(names) & names != labels & names != "System" & nzchar(names)
  map <- labels[keep]
  names(map) <- names[keep]
  map[!duplicated(names(map))]
}

# Whether `name` occurs as a whole word, in any case, in one of `texts`.
.fg_name_in_texts <- function(name, texts) {
  texts <- texts[!is.na(texts) & nzchar(texts)]
  if (!length(texts)) return(FALSE)
  pattern <- paste0("(?i)", .fg_label_pattern(name))
  any(grepl(pattern, texts, perl = TRUE))
}

# One segment: the rows of a session in it, with the labels, roster, routing,
# and probes for those rows, and the names that label no speaker.
.fg_source_segment <- function(session, name, arm, rows, probes, note = NULL) {
  roster <- session$roster
  transcript <- session$transcript
  others_all <- setdiff(unique(transcript$speaker_id[!transcript$system]), roster$id)
  roster$label <- .fg_summary_labels(roster, reserved = others_all)
  label <- roster$label[match(transcript$speaker_id, roster$id)]
  label[is.na(label)] <- transcript$handle[is.na(label)]
  label[transcript$system] <- "System"
  transcript$label <- label
  transcript$role <- ifelse(transcript$system, "system",
    ifelse(transcript$speaker_id %in% roster$id[!roster$is_moderator] &
             !transcript$is_moderator, "participant",
           ifelse(transcript$is_moderator, "moderator", "other")))
  kept_rows <- rows
  others <- unique(transcript$speaker_id[kept_rows][transcript$role[kept_rows] == "other"])
  handles <- data.frame(
    speaker_id = c(roster$id, others),
    handle = c(ifelse(is.na(roster$handle), roster$id, roster$handle),
               transcript$handle[match(others, transcript$speaker_id)]),
    label = c(roster$label, transcript$label[match(others, transcript$speaker_id)]),
    role = c(ifelse(roster$is_moderator, "moderator", "participant"),
             rep("other", length(others))),
    stringsAsFactors = FALSE)
  kept <- transcript[rows, , drop = FALSE]
  routing <- session$routing
  routing <- routing[!is.na(routing$message_id) &
                       routing$message_id %in% kept$message_id, , drop = FALSE]
  probes <- probes[!is.na(probes$answer), , drop = FALSE]
  probes$label <- roster$label[match(probes$agent_id, roster$id)]
  probes$label[is.na(probes$label)] <- probes$agent_id[is.na(probes$label)]
  probes$key <- if (nrow(probes)) paste0("Q", seq_len(nrow(probes))) else character()
  list(name = name, arm = arm, all = transcript, transcript = kept,
       roster = roster, routing = routing, probes = probes,
       topic = session$topic, note = note, handles = handles,
       other_names = .fg_other_names(handles))
}

# The turn table of one segment.
.fg_segment_turns <- function(segment) {
  tr <- segment$transcript[!segment$transcript$system, , drop = FALSE]
  routing <- segment$routing
  at <- match(tr$message_id, routing$message_id)
  pick <- function(column, empty) {
    values <- routing[[column]]
    if (is.null(values)) return(rep(list(empty), length(at)))
    lapply(at, function(i) if (is.na(i)) empty else values[[i]] %||% empty)
  }
  rule <- if (nrow(routing)) as.character(routing$rule[at]) else rep(NA_character_, nrow(tr))
  trigger <- if (nrow(routing)) as.integer(routing$trigger_message_id[at]) else
    rep(NA_integer_, nrow(tr))
  link <- ifelse(!is.na(rule) & rule %in% c("addressed", "called_on"), rule, "sequence")
  previous <- c(NA_integer_, utils::head(tr$message_id, -1L))
  out <- data.frame(
    segment = rep(segment$name, nrow(tr)),
    message_id = tr$message_id, item = tr$item, round = tr$round, phase = tr$phase,
    speaker_id = tr$speaker_id, handle = tr$handle, is_moderator = tr$is_moderator,
    injected = tr$injected, visibility = tr$visibility, verbatim = tr$verbatim,
    words = .fg_word_count(tr$text), rule = rule, stringsAsFactors = FALSE)
  out$candidates <- I(pick("candidates", character()))
  out$excluded <- I(pick("excluded", character()))
  out$trigger_message_id <- trigger
  out$link <- link
  out$linked_message_id <- ifelse(link == "sequence", previous, trigger)
  out
}

# ---- The units and their prompts ------------------------------------------------

# What the checks of a segment need: the messages with their labels, the ids
# that exist, the probe answers, and the names a claim may not use: those
# that label no speaker and occur in no message (nor, with `include_probes`,
# in a probe answer) of the session.
.fg_summary_context <- function(segment, source, include_probes) {
  all <- segment$all
  msgs <- segment$transcript[!segment$transcript$system, , drop = FALSE]
  ids <- as.character(all$message_id)
  probes <- segment$probes
  other <- segment$other_names
  read <- c(all$text[!all$system], if (isTRUE(include_probes)) as.character(probes$answer))
  labels <- unique(c(segment$roster$label, all$label[!all$system]))
  hidden <- other[!names(other) %in% labels &
                    !vapply(names(other), .fg_name_in_texts, logical(1), texts = read)]
  list(
    msgs = msgs,
    all_ids = all$message_id,
    system_ids = all$message_id[all$system],
    label_of = stats::setNames(all$label, ids),
    text_of = stats::setNames(all$text, ids),
    item_of = stats::setNames(all$item, ids),
    role_of = stats::setNames(all$role, ids),
    participant_labels = segment$roster$label[!segment$roster$is_moderator],
    speaker_labels = unique(c(segment$roster$label, msgs$label)),
    label_to_id = stats::setNames(segment$handles$speaker_id, segment$handles$label),
    hidden_names = hidden,
    probes = probes,
    include_probes = include_probes,
    transcript_ngrams = unique(c(source$transcript_ngrams,
                                 unlist(lapply(all$text[!all$system], .fg_ngrams)))),
    probe_ngrams = unique(c(source$probe_ngrams,
                            unlist(lapply(as.character(probes$answer), .fg_ngrams)))))
}

# Every unit of the summary, with its prompt; units that need no call (a
# participant with no turns, an item without participant turns) are kept so
# that the summary reports them.
.fg_summary_plan <- function(source, by, levels, include_probes, templates,
                             max_tokens, config) {
  plan <- list()
  for (segment in source$segments) {
    ctx <- .fg_summary_context(segment, source, include_probes)
    msgs <- ctx$msgs
    is_turn <- msgs$role == "participant"
    base <- list(by = by, levels = levels, segment = segment$name, arm = segment$arm,
                 ctx = ctx, include_probes = include_probes)
    units <- switch(by,
      participant = lapply(which(!segment$roster$is_moderator), function(i) {
        id <- segment$roster$id[i]
        label <- segment$roster$label[i]
        n <- sum(is_turn & msgs$speaker_id == id)
        list(key = label, speaker_id = id, n_turns = n, call = n > 0L,
             scope = msgs$message_id, input = msgs$message_id, rows = label)
      }),
      item = lapply(unique(msgs$item), function(k) {
        in_item <- msgs$item == k
        spoke <- unique(msgs$label[in_item & is_turn])
        list(key = as.character(k), item = k, n_turns = sum(in_item & is_turn),
             call = any(in_item & is_turn), scope = msgs$message_id[in_item],
             input = msgs$message_id[in_item], item_speakers = spoke,
             rows = as.character(k))
      }),
      turn = lapply(.fg_item_groups(msgs), function(ids) {
        list(key = sprintf("%d-%d", min(ids), max(ids)),
             n_turns = sum(is_turn & msgs$message_id %in% ids), call = TRUE,
             scope = ids, input = ids, chunk = ids, rows = as.character(ids))
      }),
      trajectory = {
        stages <- .fg_trajectory_stages(msgs)
        list(list(key = "trajectory", n_turns = sum(is_turn), call = any(is_turn),
                  scope = msgs$message_id, input = msgs$message_id, stages = stages,
                  rows = NULL))
      },
      list(list(key = by, n_turns = sum(is_turn), call = any(is_turn),
                scope = msgs$message_id, input = msgs$message_id,
                rows = if (identical(by, "session")) "session" else NULL)))
    for (unit in units) {
      unit <- c(base, unit)
      if (isTRUE(unit$call) && !length(unit$input)) unit$call <- FALSE
      if (isTRUE(unit$call)) {
        unit$max_tokens <- .fg_summary_max_tokens(unit, max_tokens, config)
        unit$prompt <- .fg_summary_prompt(unit, segment, templates)
      }
      plan[[length(plan) + 1L]] <- unit
    }
  }
  plan
}

# A name for a unit in messages to the user.
.fg_unit_name <- function(unit) {
  what <- switch(unit$by,
    participant = unit$key,
    item = paste("guide item", unit$key),
    turn = paste("messages", unit$key),
    paste("the", unit$by))
  if (identical(unit$segment, "session")) what else
    sprintf("%s (%s)", what, if (identical(unit$segment, "prefix")) "shared prefix" else
      paste0("arm '", unit$segment, "'"))
}

# Groups of messages, one per guide item with participant turns; the messages
# of an item without participant turns join the next group, or the last one
# when no later item has turns.
.fg_item_groups <- function(msgs) {
  if (!nrow(msgs)) return(list())
  items <- unique(msgs$item)
  has_turn <- vapply(items, function(k) any(msgs$role[msgs$item == k] == "participant"),
                     logical(1))
  if (!any(has_turn)) return(list(msgs$message_id))
  groups <- list()
  pending <- integer()
  for (j in seq_along(items)) {
    ids <- msgs$message_id[msgs$item == items[j]]
    if (has_turn[j]) {
      groups[[length(groups) + 1L]] <- c(pending, ids)
      names(groups)[length(groups)] <- as.character(items[j])
      pending <- integer()
    } else {
      pending <- c(pending, ids)
    }
  }
  if (length(pending)) {
    last <- length(groups)
    groups[[last]] <- c(groups[[last]], pending)
  }
  groups
}

# The stages of a trajectory, fixed in code: thirds of the participant turns
# for "headline" (fewer stages when there are fewer than 3 turns), and one
# stage per guide item with participant turns for "brief" and "detailed".
# A message between participant turns belongs to the stage of the next turn,
# and messages after the last turn to the last stage.
.fg_trajectory_stages <- function(msgs) {
  turns <- which(msgs$role == "participant")
  n <- length(turns)
  if (!n) return(list(thirds = list(), items = list()))
  k <- min(3L, n)
  stage_of_turn <- ceiling(seq_len(n) * k / n)
  next_turn <- vapply(seq_len(nrow(msgs)), function(i) {
    later <- turns[turns >= i]
    if (length(later)) later[1] else turns[n]
  }, integer(1))
  third <- stage_of_turn[match(next_turn, turns)]
  thirds <- lapply(seq_len(k), function(s) msgs$message_id[third == s])
  names(thirds) <- paste0("third_", seq_len(k))
  items <- .fg_item_groups(msgs)
  names(items) <- paste0("item_", names(items))
  list(thirds = thirds, items = items)
}

# The stages a level uses.
.fg_level_stages <- function(stages, level) {
  if (identical(level, "headline")) stages$thirds else stages$items
}

# The token-limit fields a configuration may carry, in the order a provider
# reads them; the summary writes its limit into those the configuration
# uses, or into `max_tokens`.
.fg_token_keys <- c("max_completion_tokens", "max_output_tokens", "max_tokens")

.fg_token_key <- function(config) {
  keys <- intersect(.fg_token_keys, names(config$model_params %||% list()))
  if (length(keys)) keys else "max_tokens"
}

# The default token limit of a unit's reply. A thinking budget the
# configuration sets is added, since the reply must fit after it.
.fg_summary_max_tokens <- function(unit, max_tokens, config) {
  if (!is.null(max_tokens)) return(as.integer(max_tokens))
  configured <- config$model_params[[.fg_token_key(config)[1]]]
  if (is.numeric(configured) && length(configured) == 1L && !is.na(configured)) {
    return(as.integer(configured))
  }
  base <- c(headline = 1000L, brief = 2000L, detailed = 3000L)[[unit$levels[length(unit$levels)]]]
  budget <- config$model_params$thinking_budget
  if (is.numeric(budget) && length(budget) == 1L && !is.na(budget) && budget > 0) {
    base <- base + as.integer(budget)
  }
  if (identical(unit$by, "turn")) base <- base + 120L * length(unit$chunk)
  if (identical(unit$by, "trajectory")) {
    base <- base + 300L * (length(unit$stages$thirds) + length(unit$stages$items))
  }
  as.integer(base)
}

# The transcript block of a prompt: one line per message with its id and
# label, under a header for each guide item.
.fg_summary_transcript <- function(ctx, ids, note = NULL) {
  msgs <- ctx$msgs[ctx$msgs$message_id %in% ids, , drop = FALSE]
  lines <- character()
  if (!is.null(note)) lines <- c(lines, note, "")
  current <- NULL
  for (i in seq_len(nrow(msgs))) {
    m <- msgs[i, ]
    if (!identical(current, m$item)) {
      phase <- m$phase
      phase <- if (is.na(phase) || phase %in% c("imported", "unknown", "")) "" else
        paste0(": ", gsub("_", " ", phase, fixed = TRUE))
      lines <- c(lines, sprintf("-- Item %d%s --", m$item, phase))
      current <- m$item
    }
    marks <- character()
    if (isTRUE(m$injected)) {
      marks <- c(marks, if (identical(m$visibility, "participants"))
        "injected, shown to participants only" else "injected")
    }
    label <- if (length(marks)) sprintf("%s (%s)", m$label, paste(marks, collapse = "; ")) else
      m$label
    lines <- c(lines, sprintf("[#%d] %s: %s", m$message_id, label, m$text))
  }
  paste(lines, collapse = "\n")
}

# Who took part, counted in code, for the prompt.
.fg_summary_roster <- function(unit, segment) {
  ctx <- unit$ctx
  msgs <- ctx$msgs[ctx$msgs$message_id %in% unit$input, , drop = FALSE]
  participants <- segment$roster$label[!segment$roster$is_moderator]
  count <- function(label) sum(msgs$label == label & msgs$role == "participant")
  describe <- function(labels) {
    vapply(labels, function(l) {
      n <- count(l)
      sprintf("%s (%s)", l, if (n == 0L) "no messages" else
        if (n == 1L) "1 message" else paste(n, "messages"))
    }, character(1))
  }
  moderator <- segment$roster$label[segment$roster$is_moderator]
  others <- unique(msgs$label[msgs$role == "other"])
  tail <- c(if (length(moderator)) sprintf("The moderator is %s.", moderator[1]),
            if (length(others)) sprintf("Other voices: %s.", paste(others, collapse = ", ")))
  if (identical(unit$by, "item")) {
    spoke <- participants[vapply(participants, count, integer(1)) > 0L]
    silent <- setdiff(participants, spoke)
    paste(c(sprintf("Spoke on this item: %s.", paste(describe(spoke), collapse = ", ")),
            if (length(silent)) sprintf("Did not speak on this item: %s.",
                                        paste(silent, collapse = ", ")),
            tail), collapse = " ")
  } else {
    paste(c(sprintf("Participants: %s.", paste(describe(participants), collapse = ", ")),
            tail), collapse = " ")
  }
}

.fg_summary_probes_block <- function(unit) {
  probes <- unit$ctx$probes
  if (!isTRUE(unit$include_probes) || !nrow(probes)) return("")
  lines <- vapply(seq_len(nrow(probes)), function(i) {
    after <- probes$after_message_id[i]
    sprintf("[%s] %s, asked %s: \"%s\" Answer: %s", probes$key[i], probes$label[i],
            if (is.na(after)) "before the discussion" else sprintf("after message #%d", after),
            probes$question[i], probes$answer[i])
  }, character(1))
  paste0("\nPrivate probe answers. Each participant answered these questions alone: ",
         "neither the other participants nor the moderator saw the answers, and they ",
         "did not enter the discussion. Cite them only in \"probe_evidence\", by their ",
         "ids, and say in the claim that the answer was private.\n",
         paste(lines, collapse = "\n"), "\n")
}

.fg_summary_stages_text <- function(unit) {
  if (!identical(unit$by, "trajectory")) return("")
  describe <- function(stages) {
    vapply(names(stages), function(key) {
      ids <- stages[[key]]
      sprintf("%s (messages #%d to #%d)", key, min(ids), max(ids))
    }, character(1))
  }
  parts <- character()
  if ("headline" %in% unit$levels) {
    parts <- c(parts, sprintf("For the headline: %s.",
                              paste(describe(unit$stages$thirds), collapse = ", ")))
  }
  if (any(c("brief", "detailed") %in% unit$levels)) {
    parts <- c(parts, sprintf("For brief and detailed: %s.",
                              paste(describe(unit$stages$items), collapse = ", ")))
  }
  paste(parts, collapse = "\n")
}

.fg_summary_unit_text <- function(unit) {
  switch(unit$by,
    participant = unit$key,
    item = unit$key,
    turn = sprintf("messages #%d to #%d", min(unit$chunk), max(unit$chunk)),
    if (identical(unit$segment, "session") || identical(unit$segment, "prefix"))
      "the discussion" else sprintf("arm '%s' of the discussion", unit$segment))
}

.fg_summary_prompt <- function(unit, segment, templates) {
  values <- list(
    topic = segment$topic %|NA|% "not recorded",
    unit = .fg_summary_unit_text(unit),
    roster = .fg_summary_roster(unit, segment),
    transcript = .fg_summary_transcript(unit$ctx, unit$input, segment$note),
    probes = .fg_summary_probes_block(unit),
    stages = .fg_summary_stages_text(unit),
    format = .fg_summary_format(unit))
  name <- paste0("summary_", unit$by)
  .fg_render_template(templates[[name]], values, name)
}

# ---- Templates ------------------------------------------------------------------

# Fill `{{key}}` placeholders in one pass. A placeholder without a value is
# an error, and text inside the values, such as a transcript that contains
# "{{topic}}", is never read for placeholders.
.fg_render_template <- function(template, values, name = "template") {
  if (!is.character(template) || length(template) != 1L || is.na(template)) {
    stop("Template `", name, "` must be one string.", call. = FALSE)
  }
  odd <- regmatches(template, gregexpr("\\{\\{[^{}]*\\}\\}", template, perl = TRUE))[[1]]
  odd <- odd[!grepl("^\\{\\{[A-Za-z0-9_]+\\}\\}$", odd)]
  if (length(odd)) {
    stop("Template `", name, "` has a malformed placeholder: ", odd[1], ".", call. = FALSE)
  }
  hits <- gregexpr("\\{\\{[A-Za-z0-9_]+\\}\\}", template, perl = TRUE)[[1]]
  if (hits[1] == -1L) return(template)
  starts <- as.integer(hits)
  ends <- starts + attr(hits, "match.length") - 1L
  keys <- substring(template, starts + 2L, ends - 2L)
  missing <- unique(keys[vapply(keys, function(k) is.null(values[[k]]), logical(1))])
  if (length(missing)) {
    stop("Template `", name, "` uses ", paste0("{{", missing, "}}", collapse = ", "),
         ", which has no value.", call. = FALSE)
  }
  pieces <- character(2L * length(keys) + 1L)
  position <- 1L
  for (i in seq_along(keys)) {
    pieces[2L * i - 1L] <- substr(template, position, starts[i] - 1L)
    pieces[2L * i] <- as.character(values[[keys[i]]])[1]
    position <- ends[i] + 1L
  }
  pieces[length(pieces)] <- substr(template, position, nchar(template))
  paste(pieces, collapse = "")
}

# The summary templates in use: the defaults, with any the caller replaced.
.fg_summary_templates <- function(templates) {
  defaults <- get_default_prompt_templates()[.fg_summary_template_names]
  if (is.null(templates)) return(defaults)
  if (is.character(templates)) templates <- as.list(templates)
  if (!is.list(templates) || !length(templates) || is.null(names(templates)) ||
      anyNA(names(templates)) || any(!nzchar(names(templates))) ||
      anyDuplicated(names(templates))) {
    stop("`templates` must be `NULL` or a list of strings named by template.",
         call. = FALSE)
  }
  unknown <- setdiff(names(templates), .fg_summary_template_names)
  if (length(unknown)) {
    stop("Unknown summary template(s): ", paste(unknown, collapse = ", "),
         ". The summary templates are ", paste(.fg_summary_template_names, collapse = ", "),
         ".", call. = FALSE)
  }
  for (name in names(templates)) {
    value <- templates[[name]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
      stop("Template `", name, "` must be one non-empty string.", call. = FALSE)
    }
    required <- if (identical(name, "summary_reask")) "failures" else c("transcript", "format")
    absent <- required[!vapply(required, function(k) grepl(paste0("{{", k, "}}"), value,
                                                           fixed = TRUE), logical(1))]
    if (length(absent)) {
      stop("Template `", name, "` must contain ", paste0("{{", absent, "}}", collapse = " and "),
           ".", call. = FALSE)
    }
  }
  utils::modifyList(defaults, templates)
}

# ---- The reply format -----------------------------------------------------------

# The JSON instructions of a unit's prompt.
.fg_summary_format <- function(unit) {
  by <- unit$by
  levels <- unit$levels
  probes <- isTRUE(unit$include_probes)
  example_unit <- switch(by,
    participant = unit$key, item = unit$key,
    turn = as.character(unit$chunk[1]),
    trajectory = names(.fg_level_stages(unit$stages, levels[1]))[1],
    disagreements = "D1", session = "session")
  kind <- switch(by, trajectory = "stage", turn = "gist", disagreements = "disagreement",
                 "position")
  # The example speaker is the unit's own participant in participant mode.
  who <- if (identical(by, "participant")) unit$key else "P2"
  quote <- sprintf(if (probes) {
    "{\"message_id\": 14, \"probe\": null, \"speaker\": \"%s\", \"text\": \"words copied from message 14\"}"
  } else {
    "{\"message_id\": 14, \"speaker\": \"%s\", \"text\": \"words copied from message 14\"}"
  }, who)
  extra <- c(
    if (probes) "\"probe_evidence\": []",
    if (identical(by, "turn")) paste0("\"act\": ", if ("detailed" %in% levels) "\"answer\"" else "null"),
    if (identical(by, "disagreements")) "\"ending\": {\"how\": \"unresolved\", \"message_id\": null}")
  level_example <- paste0("{", paste(sprintf("\"%s\": [1]", levels), collapse = ", "), "}")
  speakers <- if (identical(by, "disagreements")) "[\"P2\", \"P3\"]" else
    sprintf("[\"%s\"]", who)
  shape <- paste0(
    "{\n  \"claims\": [\n    {\"id\": 1, \"unit\": \"", example_unit, "\", \"kind\": \"", kind,
    "\", \"text\": \"One sentence.\", \"status\": \"stated\", \"speakers\": ", speakers,
    ", \"evidence\": [14], \"context\": [12], \"quotes\": [", quote, "]",
    if (length(extra)) paste0(", ", paste(extra, collapse = ", ")), "}\n  ],\n",
    "  \"levels\": ", level_example, "\n}")
  unit_rule <- switch(by,
    participant = sprintf("\"%s\" in every claim.", unit$key),
    item = sprintf("\"%s\" in every claim.", unit$key),
    turn = "the id of the message the claim is about, as a string; every message shown needs at least one claim.",
    trajectory = "the stage the claim is about, such as \"third_1\" or \"item_3\"; the stages are listed above.",
    disagreements = "a key for the entry, \"D1\", \"D2\", and so on, one per disagreement or position held alone.",
    session = "\"session\" in every claim.")
  kinds <- .fg_summary_kinds[[by]]
  lines <- c(
    "Reply with one JSON object and nothing else, in this shape (the values are examples):",
    shape, "",
    "Fields of each claim:",
    "- \"id\": a whole number, different for each claim.",
    paste("- \"unit\":", unit_rule),
    paste0("- \"kind\": one of ", paste0("\"", kinds, "\"", collapse = ", "), ".",
           switch(by,
             disagreements = " \"disagreement\" names the participants on every side; \"minority\" a position held by one participant alone.",
             item = " \"minority\" is a position held by one participant alone.",
             session = " \"minority\" is a position held by one participant alone.",
             "")),
    "- \"text\": one sentence stating the claim, naming speakers by their labels.",
    paste("- \"status\": \"stated\" when said outright; \"implied\" when it follows from what",
          "was said but was not said outright; \"contradictory\" when the speaker's messages",
          "conflict on it; \"not_stated\" when the transcript does not show it."),
    if (identical(by, "turn")) "- \"speakers\": the label of the message's speaker." else
      "- \"speakers\": the labels of the speakers whose words or positions the claim reports.",
    paste0("- \"evidence\": ids of messages by those speakers that support the claim, at ",
           "least one unless the status is \"not_stated\".",
           if ("change" %in% kinds) paste(" A claim of kind \"change\" cites the earlier",
                                          "message first and the later one second, both by",
                                          "the same speaker.")),
    paste("- \"context\": ids of other messages the claim refers to, such as the moderator's",
          "question or the message replied to; [] when there are none."),
    paste("- \"quotes\": words copied exactly from one cited message, at least 3 words, with",
          "\"...\" where words are left out; \"speaker\" is the label of that message's",
          "speaker; [] when there are none."),
    if (probes) paste("- \"probe_evidence\": ids of private probe answers, such as \"Q2\", that",
                      "support the claim; [] when there are none. A quote from a probe answer",
                      "gives \"probe\": \"Q2\" and \"message_id\": null; a quote from a message",
                      "gives \"probe\": null."),
    if (identical(by, "turn")) {
      if ("detailed" %in% levels) {
        paste0("- \"act\": what the message does, one of ",
               paste0("\"", .fg_summary_acts, "\"", collapse = ", "),
               ". \"procedure\" is the moderator's management, such as a welcome or a transition.")
      } else "- \"act\": null."
    },
    if (identical(by, "disagreements")) {
      paste("- \"ending\": {\"how\": \"concession\", \"message_id\": 20} when a holder gave",
            "ground in message 20; {\"how\": \"moved_on\", \"message_id\": 18} when the",
            "moderator moved on in message 18; {\"how\": \"unresolved\", \"message_id\": null}",
            "otherwise.")
    },
    "",
    "\"levels\" gives, for each length, the ids of the claims to show, in order:",
    vapply(levels, function(level) .fg_level_rule(by, level), character(1)))
  paste(lines, collapse = "\n")
}

.fg_level_rule <- function(by, level) {
  rule <- if (identical(by, "turn")) {
    switch(level,
      headline = "for each message, one claim of at most 20 words.",
      brief = "for each message, the claims that make one or two sentences, at most 40 words.",
      detailed = "for each message, the claims with a quote and an act code.")
  } else if (identical(by, "trajectory")) {
    switch(level,
      headline = "at least one claim for each of the headline stages.",
      brief = "for each guide-item stage, the claims that make a few sentences.",
      detailed = "for each guide-item stage, a longer account with quotes.")
  } else if (identical(by, "disagreements")) {
    switch(level,
      headline = "the most consequential entries, one sentence each.",
      brief = "the entries that make a paragraph of about 100 to 120 words.",
      detailed = "every entry, with quotes.")
  } else {
    switch(level,
      headline = "the one claim that best states the unit, in one sentence.",
      brief = "the claims that together make a paragraph of about 100 to 120 words.",
      detailed = "a longer account, with quotes.")
  }
  sprintf("- \"%s\": %s", level, rule)
}

# The JSON schema of a reply, written so that strict providers accept it:
# every property required, no others allowed, and absent values as null.
.fg_summary_schema <- function(by, levels, include_probes) {
  ids <- list(type = "array", items = list(type = "integer"))
  strings <- list(type = "array", items = list(type = "string"))
  object <- function(properties) {
    list(type = "object", properties = properties,
         required = as.list(names(properties)), additionalProperties = FALSE)
  }
  quote <- c(
    list(message_id = list(type = list("integer", "null"))),
    if (include_probes) list(probe = list(type = list("string", "null"))),
    list(speaker = list(type = "string"), text = list(type = "string")))
  claim <- list(
    id = list(type = "integer"),
    unit = list(type = "string"),
    kind = list(type = "string", enum = as.list(.fg_summary_kinds[[by]])),
    text = list(type = "string"),
    status = list(type = "string", enum = as.list(.fg_summary_statuses)),
    speakers = strings,
    evidence = ids,
    context = ids,
    quotes = list(type = "array", items = object(quote)))
  if (include_probes) claim$probe_evidence <- strings
  if (identical(by, "turn")) {
    claim$act <- list(anyOf = list(list(type = "string", enum = as.list(.fg_summary_acts)),
                                   list(type = "null")))
  }
  if (identical(by, "disagreements")) {
    claim$ending <- object(list(how = list(type = "string", enum = as.list(.fg_summary_endings)),
                                message_id = list(type = list("integer", "null"))))
  }
  level_props <- stats::setNames(rep(list(ids), length(levels)), levels)
  object(list(claims = list(type = "array", items = object(claim)),
              levels = object(level_props)))
}

# The configuration a summary call uses for structured output, and the mode:
# "schema" when LLMR::enable_structured_output() added the schema, "config"
# when the configuration already set a response format and is left as it is,
# and "prompt" when the format is stated in the prompt alone.
.fg_summary_structured <- function(config, schema) {
  params <- names(config$model_params %||% list())
  if (any(c("response_format", "json_schema", "response_mime_type",
            "response_json_schema", "tools") %in% params)) {
    return(list(config = config, mode = "config"))
  }
  # Anthropic's schema mode forces a tool call, which its API refuses while
  # extended thinking is on.
  thinking <- identical(config$provider, "anthropic") &&
    any(c("thinking", "thinking_budget", "reasoning_effort", "include_thoughts") %in% params)
  if (!isTRUE(getOption("focusgroup.summary_structured_output", TRUE)) || thinking ||
      !"enable_structured_output" %in% getNamespaceExports("LLMR")) {
    return(list(config = config, mode = "prompt"))
  }
  enable <- getExportedValue("LLMR", "enable_structured_output")
  list(config = enable(config, schema = schema, name = "focusgroup_summary"),
       mode = "schema")
}

# ---- Calls ----------------------------------------------------------------------

# Evaluate `expr`, keeping the records of the model calls made inside it in
# `store$calls` (an environment's list) rather than in a session's records.
# `fields` are added to each record. The notes .fg_note_call() signals, such
# as the cleaned text, are added to the record they refer to.
.fg_collect_calls <- function(expr, store, fields = list()) {
  withCallingHandlers(expr,
    focusgroup_call = function(cnd) {
      id <- length(store$calls) + 1L
      store$calls[[id]] <- c(list(call_id = id), cnd$record, fields)
      cnd$handle$index <- id
    },
    focusgroup_call_note = function(cnd) {
      id <- cnd$handle$index
      if (is.null(id) || id > length(store$calls)) return(invisible(NULL))
      for (field in names(cnd$fields)) store$calls[[id]][[field]] <- cnd$fields[[field]]
    })
}

# Summarize one unit: a call, the checks, and at most one re-ask stating each
# failure, with the token limit doubled after a reply cut off at the limit.
.fg_summary_run_unit <- function(unit, config, .runner, store, reask_template) {
  messages <- list(list(role = "user", content = unit$prompt))
  tokens <- unit$max_tokens
  attempts <- list()
  fields <- list(round = NA_integer_, phase = NA_character_, message_id = NA_integer_,
                 arm = unit$arm, segment = unit$segment, unit = unit$key)
  for (attempt in 1:2) {
    cfg <- config
    for (key in .fg_token_key(config)) cfg$model_params[[key]] <- tokens
    reply <- .fg_collect_calls({
      # Retries draw random waits; the caller's random-number stream is left
      # as it was.
      response <- withr::with_preserve_seed(.fg_call_llm(
        config = cfg, messages = messages, .runner = .runner,
        tries = 5, wait_seconds = 2, backoff_factor = 3,
        .call = list(type = paste0("summary_", unit$by), agent_id = NA_character_,
                     attempt = attempt)))
      text <- .fg_response_text(response)
      .fg_note_call(response, cleaned_text = text)
      list(text = text, raw = .fg_raw_text(response),
           finish = .fg_response_finish(response))
    }, store, fields)
    verdict <- .fg_verify_unit(reply$text, reply$finish, unit)
    attempts[[attempt]] <- c(reply, list(verdict = verdict, max_tokens = tokens))
    if (isTRUE(verdict$passed) || attempt == 2L) break
    failures <- paste0("- ", verdict$failures, collapse = "\n")
    reask <- .fg_render_template(reask_template, list(failures = failures), "summary_reask")
    messages <- c(messages, list(
      list(role = "assistant", content = if (nzchar(reply$text)) reply$text else "(empty reply)"),
      list(role = "user", content = reask)))
    if (isTRUE(verdict$truncated)) tokens <- as.integer(min(2 * tokens, .Machine$integer.max))
  }
  list(attempts = attempts)
}

# ---- The checks -----------------------------------------------------------------

# The complete JSON objects in a text, outermost first, found by matching
# braces outside strings. Text around them, and an object cut off before its
# closing brace, are left out.
.fg_json_objects <- function(text) {
  chars <- strsplit(text, "")[[1]]
  n <- length(chars)
  out <- character()
  i <- 1L
  while (i <= n) {
    if (chars[i] != "{") {
      i <- i + 1L
      next
    }
    depth <- 0L
    in_string <- FALSE
    escaped <- FALSE
    j <- i
    while (j <= n) {
      ch <- chars[j]
      if (in_string) {
        if (escaped) escaped <- FALSE
        else if (ch == "\\") escaped <- TRUE
        else if (ch == "\"") in_string <- FALSE
      } else if (ch == "\"") {
        in_string <- TRUE
      } else if (ch == "{") {
        depth <- depth + 1L
      } else if (ch == "}") {
        depth <- depth - 1L
        if (depth == 0L) break
      }
      j <- j + 1L
    }
    if (j > n) break
    out <- c(out, paste(chars[i:j], collapse = ""))
    i <- j + 1L
  }
  out
}

# Parse a reply's final answer as one JSON object. Fenced code blocks are
# read first, then the whole text; within each, every complete object in
# turn, and the first with a "claims" field is the reply.
.fg_summary_parse <- function(text) {
  value <- trimws(text %||% "")
  fences <- regmatches(value, gregexpr("(?s)```[A-Za-z]*\\s*(.*?)```", value, perl = TRUE))[[1]]
  fences <- sub("(?s)^```[A-Za-z]*\\s*(.*?)```$", "\\1", fences, perl = TRUE)
  first_error <- NULL
  fallback <- NULL
  for (candidate in c(fences, value)) {
    for (object in .fg_json_objects(candidate)) {
      parsed <- tryCatch(jsonlite::fromJSON(object, simplifyVector = FALSE),
                         error = function(e) e)
      if (inherits(parsed, "error")) {
        first_error <- first_error %||% trimws(gsub("\\s+", " ", conditionMessage(parsed)))
        next
      }
      if (!is.list(parsed) || is.null(names(parsed))) next
      if ("claims" %in% names(parsed)) return(list(ok = TRUE, value = parsed))
      fallback <- fallback %||% parsed
    }
  }
  if (!is.null(fallback)) return(list(ok = TRUE, value = fallback))
  list(ok = FALSE, error = first_error %||% "no complete JSON object was found")
}

# A list of message ids, as whole numbers or strings such as "14" or "#14".
.fg_summary_ids <- function(value, field) {
  if (is.null(value)) return(list(ids = integer(), errors = character()))
  if (!is.list(value)) value <- as.list(value)
  ids <- integer()
  errors <- character()
  for (v in value) {
    if (is.numeric(v) && length(v) == 1L && !is.na(v) && v == floor(v) &&
        abs(v) <= .Machine$integer.max) {
      ids <- c(ids, as.integer(v))
    } else if (is.character(v) && length(v) == 1L && grepl("^\\s*#?\\s*[0-9]{1,9}\\s*$", v)) {
      ids <- c(ids, as.integer(gsub("[^0-9]", "", v)))
    } else {
      errors <- c(errors, sprintf("`%s` must list message ids as whole numbers", field))
    }
  }
  list(ids = ids, errors = unique(errors))
}

.fg_summary_string <- function(value) {
  if (is.character(value) && length(value) == 1L && !is.na(value)) trimws(value) else
    if (is.numeric(value) && length(value) == 1L && !is.na(value)) format(value) else NA_character_
}

.fg_summary_strings <- function(value) {
  if (is.null(value)) return(character())
  if (!is.list(value)) value <- as.list(value)
  out <- vapply(value, function(v) .fg_summary_string(v), character(1))
  out[!is.na(out)]
}

# One claim of a reply, in a fixed shape, with the problems of its fields.
.fg_summary_claim <- function(raw, index) {
  errors <- character()
  if (!is.list(raw) || is.null(names(raw))) {
    return(list(index = index, id = NA_integer_, label = sprintf("Claim %d", index),
                kind = NA_character_, unit = NA_character_, text = NA_character_,
                status = NA_character_, speakers = character(), evidence = integer(),
                context = integer(), probe_evidence = character(), quotes = list(),
                act = NA_character_, ending = NULL,
                errors = "it is not a JSON object"))
  }
  id <- raw$id
  id <- if (is.numeric(id) && length(id) == 1L && !is.na(id) && id == floor(id)) as.integer(id) else
    if (is.character(id) && length(id) == 1L && grepl("^\\s*[0-9]{1,9}\\s*$", id)) as.integer(id) else NA_integer_
  if (is.na(id)) errors <- c(errors, "`id` must be a whole number")
  text <- .fg_summary_string(raw$text)
  if (is.na(text) || !nzchar(text)) errors <- c(errors, "`text` must be a non-empty string")
  kind <- .fg_summary_string(raw$kind)
  if (is.na(kind)) errors <- c(errors, "`kind` is missing")
  status <- .fg_summary_string(raw$status)
  if (is.na(status) || !status %in% .fg_summary_statuses) {
    errors <- c(errors, "`status` must be \"stated\", \"implied\", \"contradictory\", or \"not_stated\"")
  }
  unit <- .fg_summary_string(raw$unit)
  if (is.na(unit)) errors <- c(errors, "`unit` is missing")
  if (!is.null(raw$speakers) && !is.list(raw$speakers) && !is.character(raw$speakers)) {
    errors <- c(errors, "`speakers` must be a list of labels")
  }
  evidence <- .fg_summary_ids(raw$evidence, "evidence")
  context <- .fg_summary_ids(raw$context, "context")
  errors <- c(errors, evidence$errors, context$errors)
  quotes <- raw$quotes %||% list()
  if (!is.list(quotes) || (length(quotes) && !is.null(names(quotes)))) {
    errors <- c(errors, "`quotes` must be a list of quote objects")
    quotes <- list()
  }
  quotes <- lapply(quotes, function(q) {
    if (!is.list(q)) return(list(message_id = NA_integer_, probe = NA_character_,
                                 speaker = NA_character_, text = NA_character_, bad = TRUE))
    mid <- .fg_summary_ids(if (is.null(q$message_id)) NULL else list(q$message_id), "message_id")
    list(message_id = if (length(mid$ids)) mid$ids[1] else NA_integer_,
         probe = .fg_summary_string(q$probe),
         speaker = .fg_summary_string(q$speaker),
         text = .fg_summary_string(q$text),
         bad = length(mid$errors) > 0L)
  })
  ending <- NULL
  if (!is.null(raw$ending)) {
    if (is.list(raw$ending)) {
      mid <- .fg_summary_ids(if (is.null(raw$ending$message_id)) NULL else
        list(raw$ending$message_id), "ending")
      ending <- list(how = tolower(.fg_summary_string(raw$ending$how)),
                     message_id = if (length(mid$ids)) mid$ids[1] else NA_integer_)
    } else {
      errors <- c(errors, "`ending` must be an object")
    }
  }
  list(index = index, id = id,
       label = sprintf("Claim %s", if (is.na(id)) index else id),
       kind = kind, unit = unit, text = text, status = status,
       speakers = unique(.fg_summary_strings(raw$speakers)),
       evidence = unique(evidence$ids), evidence_order = evidence$ids,
       context = unique(context$ids),
       probe_evidence = unique(.fg_summary_strings(raw$probe_evidence)),
       quotes = quotes, act = .fg_summary_string(raw$act), ending = ending,
       errors = errors)
}

# Check one reply of a unit. Returns whether it passed, whether a failure
# concerns the reply as a whole (and so cannot be dropped), the claims with
# their failures, the verified quotes, every check run, and the failures
# stated for a re-ask.
.fg_verify_unit <- function(text, finish_reason = NA_character_, unit) {
  ctx <- unit$ctx
  rows <- list()
  failures <- character()
  add <- function(claim, check, passed, detail = NA_character_) {
    rows[[length(rows) + 1L]] <<- list(claim = as.integer(claim), check = check,
                                       passed = isTRUE(passed),
                                       detail = if (isTRUE(passed)) NA_character_ else detail)
    if (!isTRUE(passed)) failures <<- c(failures, detail)
    invisible(passed)
  }
  done <- function(fatal, claims = list(), levels = list(), quotes = NULL) {
    checks <- if (length(rows)) {
      data.frame(claim = vapply(rows, `[[`, integer(1), "claim"),
                 check = vapply(rows, `[[`, character(1), "check"),
                 passed = vapply(rows, `[[`, logical(1), "passed"),
                 detail = vapply(rows, `[[`, character(1), "detail"),
                 stringsAsFactors = FALSE)
    } else data.frame(claim = integer(), check = character(), passed = logical(),
                      detail = character(), stringsAsFactors = FALSE)
    list(passed = !length(failures), fatal = fatal, truncated = truncated,
         claims = claims, levels = levels, quotes = quotes, checks = checks,
         failures = unique(failures))
  }

  # A reply cut off at the token limit is incomplete as a whole.
  truncated <- !is.na(finish_reason) && tolower(finish_reason) %in% .fg_finish_truncated
  add(NA, "complete", !truncated, paste(
    "The reply was cut off at the token limit. Give the whole JSON object, with",
    "each claim in one sentence."))
  parsed <- .fg_summary_parse(text)
  add(NA, "json", parsed$ok, sprintf(
    "The reply is not a valid JSON object (%s). Reply with the JSON object alone.",
    parsed$error %||% ""))
  if (!parsed$ok) return(done(TRUE))
  value <- parsed$value
  raw_claims <- value$claims
  if (is.null(raw_claims)) raw_claims <- list()
  shape_ok <- is.list(raw_claims) && (!length(raw_claims) || is.null(names(raw_claims)))
  add(NA, "claims", shape_ok, "`claims` must be a list of claim objects.")
  if (!shape_ok) return(done(TRUE))
  claims <- lapply(seq_along(raw_claims), function(i) .fg_summary_claim(raw_claims[[i]], i))
  # How a disagreement ended belongs to the disagreements register alone.
  if (!identical(unit$by, "disagreements")) {
    claims <- lapply(claims, function(cl) {
      cl$ending <- NULL
      cl
    })
  }
  claim_ids <- vapply(claims, `[[`, integer(1), "id")
  dup <- unique(claim_ids[!is.na(claim_ids) & duplicated(claim_ids)])
  add(NA, "claim_ids", !length(dup), sprintf(
    "Claim ids must be distinct; %s is used more than once.", paste(dup, collapse = ", ")))
  fatal <- truncated || length(dup) > 0L

  quotes <- list()
  for (i in seq_along(claims)) {
    result <- .fg_check_claim(claims[[i]], unit, ctx)
    for (r in result$rows) add(claims[[i]]$id %|NA|% NA_integer_, r$check, r$passed, r$detail)
    claims[[i]]$failures <- result$failures
    quotes[[i]] <- result$quotes
  }

  # Lengths: every length asked for, listing claims of the reply.
  levels <- value$levels
  levels_ok <- is.list(levels) && !is.null(names(levels))
  add(NA, "levels", levels_ok, paste0(
    "`levels` must be an object with a list of claim ids for each of ",
    paste0("\"", unit$levels, "\"", collapse = ", "), "."))
  parsed_levels <- list()
  if (levels_ok) {
    for (level in unit$levels) {
      ids <- .fg_summary_ids(levels[[level]], paste0("levels$", level))
      present <- !is.null(levels[[level]]) && !length(ids$errors)
      add(NA, "levels", present, sprintf(
        "`levels` has no list of claim ids for \"%s\".", level))
      unknown <- setdiff(ids$ids, claim_ids)
      add(NA, "levels", !length(unknown), sprintf(
        "The \"%s\" list cites claim %s, which is not in the reply.", level,
        paste(unknown, collapse = ", ")))
      parsed_levels[[level]] <- intersect(ids$ids, claim_ids)
      if (!present || length(unknown)) fatal <- TRUE
    }
  } else {
    fatal <- TRUE
  }
  # Coverage counts only the claims that pass, so a gap left by a failing
  # claim is a failure of the reply as a whole.
  if (!fatal) {
    passing <- Filter(function(cl) !length(cl$failures), claims)
    passing_ids <- vapply(passing, `[[`, integer(1), "id")
    passing_levels <- lapply(parsed_levels, function(ids) ids[ids %in% passing_ids])
    covered <- .fg_check_coverage(passing, passing_levels, unit)
    for (r in covered) add(NA, r$check, r$passed, r$detail)
    if (!all(vapply(covered, function(r) isTRUE(r$passed), logical(1)))) fatal <- TRUE
  }
  done(fatal, claims, parsed_levels, quotes)
}

# A pattern matching `label` as a whole word.
.fg_label_pattern <- function(label) {
  paste0("(?<![\\p{L}\\p{N}_])\\Q", label, "\\E(?![\\p{L}\\p{N}_])")
}

# The checks of one claim. Returns the check rows, the failures, and the
# verified quotes with their positions in the source message.
.fg_check_claim <- function(claim, unit, ctx) {
  rows <- list()
  failures <- character()
  quotes <- list()
  check <- function(name, passed, detail) {
    rows[[length(rows) + 1L]] <<- list(check = name, passed = isTRUE(passed),
                                       detail = if (isTRUE(passed)) NA_character_ else detail)
    if (!isTRUE(passed)) failures <<- c(failures, detail)
    isTRUE(passed)
  }
  out <- function() list(rows = rows, failures = failures, quotes = quotes)
  label <- claim$label
  if (!check("schema", !length(claim$errors),
             sprintf("%s: %s.", label, paste(claim$errors, collapse = "; ")))) {
    return(out())
  }
  by <- unit$by
  kinds <- .fg_summary_kinds[[by]]
  check("kind", claim$kind %in% kinds, sprintf(
    "%s has kind \"%s\"; use one of %s.", label, claim$kind,
    paste0("\"", kinds, "\"", collapse = ", ")))

  # The unit the claim belongs to.
  unit_ok <- switch(by,
    participant = identical(claim$unit, unit$key),
    item = identical(gsub("[^0-9]", "", claim$unit), unit$key) && nzchar(unit$key),
    turn = gsub("[^0-9]", "", claim$unit) %in% as.character(unit$chunk),
    trajectory = tolower(claim$unit) %in% c(names(unit$stages$thirds), names(unit$stages$items)),
    disagreements = nzchar(claim$unit),
    session = TRUE)
  unit_hint <- switch(by,
    participant = sprintf("\"%s\"", unit$key),
    item = sprintf("\"%s\"", unit$key),
    turn = "the id of one message shown",
    trajectory = paste0("one of ", paste0("\"", c(names(unit$stages$thirds),
                                                    names(unit$stages$items)), "\"",
                                          collapse = ", ")),
    "a non-empty key")
  check("unit", unit_ok, sprintf("%s has unit \"%s\"; use %s.", label, claim$unit, unit_hint))
  if (identical(by, "turn")) claim$unit <- gsub("[^0-9]", "", claim$unit)
  if (identical(by, "item")) claim$unit <- unit$key
  if (identical(by, "trajectory")) claim$unit <- tolower(claim$unit)

  # The messages cited: they exist, are not the System row, and lie in the
  # unit (for a trajectory, in the claim's stage).
  quote_ids <- vapply(claim$quotes, function(q) q$message_id, integer(1))
  ending_id <- if (is.null(claim$ending)) NA_integer_ else claim$ending$message_id
  cited <- unique(c(claim$evidence, claim$context, quote_ids[!is.na(quote_ids)],
                    ending_id[!is.na(ending_id)]))
  unknown <- setdiff(cited, ctx$all_ids)
  check("ids_exist", !length(unknown), sprintf(
    "%s cites message %s, which does not exist.", label, paste(unknown, collapse = ", ")))
  system <- intersect(cited, ctx$system_ids)
  check("not_system", !length(system), sprintf(
    "%s cites message %s, the roster line, which is not part of the discussion.",
    label, paste(system, collapse = ", ")))
  scope <- unit$scope
  if (identical(by, "trajectory") && unit_ok) {
    scope <- c(unit$stages$thirds, unit$stages$items)[[claim$unit]]
  }
  outside <- setdiff(cited, c(scope, unknown, system))
  if (identical(by, "trajectory") && length(outside)) {
    # Context may reach back across stages, and so may the earlier message of
    # a change.
    allowed <- intersect(claim$context, unit$scope)
    if (identical(claim$kind, "change") && length(claim$evidence_order)) {
      allowed <- c(allowed, utils::head(claim$evidence_order, -1L))
    }
    outside <- setdiff(outside, allowed)
  }
  check("in_scope", !length(outside), sprintf(
    "%s cites message %s, which is outside %s (messages #%d to #%d).", label,
    paste(outside, collapse = ", "),
    if (identical(by, "trajectory")) paste("stage", claim$unit) else "this unit",
    min(scope), max(scope)))
  valid <- function(ids) ids[ids %in% ctx$all_ids & !ids %in% ctx$system_ids]
  in_unit <- function(ids) ids[ids %in% unit$scope]
  # Participants with a message in the unit.
  spoke_here <- unique(ctx$msgs$label[ctx$msgs$role == "participant" &
                                        ctx$msgs$message_id %in% unit$scope])

  # Speakers: known labels, matched by the evidence.
  unknown_speakers <- setdiff(claim$speakers, ctx$speaker_labels)
  check("speakers", !length(unknown_speakers), sprintf(
    "%s names %s, who is not a speaker in this discussion; the speakers are %s.", label,
    paste(unknown_speakers, collapse = ", "), paste(ctx$speaker_labels, collapse = ", ")))
  has_probe <- isTRUE(ctx$include_probes) && length(claim$probe_evidence) > 0L
  stated <- !identical(claim$status, "not_stated")
  if (stated) {
    check("evidence", length(claim$evidence) > 0L || has_probe, sprintf(
      "%s is \"%s\" but cites no message as evidence; cite the messages, or mark it \"not_stated\".",
      label, claim$status))
  }
  evidence <- valid(claim$evidence)
  by_whom <- unname(ctx$label_of[as.character(evidence)])
  wrong <- evidence[!by_whom %in% claim$speakers]
  check("speaker_match", !length(wrong), paste(vapply(wrong, function(id) sprintf(
    "%s gives message %d as evidence for %s, but message %d is by %s.", label, id,
    if (length(claim$speakers)) paste(claim$speakers, collapse = " and ") else "no speaker",
    id, ctx$label_of[[as.character(id)]]), character(1)), collapse = " "))
  probe_by <- character()
  if (isTRUE(ctx$include_probes) && length(claim$probe_evidence)) {
    unknown_probes <- setdiff(claim$probe_evidence, ctx$probes$key)
    check("probe_ids", !length(unknown_probes), sprintf(
      "%s cites probe answer %s, which does not exist.", label,
      paste(unknown_probes, collapse = ", ")))
    probe_by <- ctx$probes$label[match(intersect(claim$probe_evidence, ctx$probes$key),
                                       ctx$probes$key)]
    wrong_probes <- setdiff(probe_by, claim$speakers)
    check("speaker_match", !length(wrong_probes), sprintf(
      "%s cites a probe answer by %s, who is not among its speakers.", label,
      paste(wrong_probes, collapse = ", ")))
  }
  if (stated && length(claim$speakers)) {
    lacking <- setdiff(intersect(claim$speakers, ctx$speaker_labels), c(by_whom, probe_by))
    check("speaker_evidence", !length(lacking), paste(vapply(lacking, function(s) {
      if (s %in% spoke_here) sprintf("%s reports what %s said but cites no message by %s.", label, s, s)
      else sprintf(paste0("%s reports what %s said, but %s has no message here; ",
                          "describe silence with the status \"not_stated\"."), label, s, s)
    }, character(1)), collapse = " "))
  }

  # A turn's claim is about that message and its speaker.
  if (identical(by, "turn") && unit_ok) {
    id <- as.integer(claim$unit)
    speaker <- ctx$label_of[[as.character(id)]]
    check("turn", id %in% claim$evidence && identical(claim$speakers, speaker), sprintf(
      "%s is about message %d; give \"speakers\": [\"%s\"] and include %d in \"evidence\".",
      label, id, speaker, id))
    if ("detailed" %in% unit$levels) {
      check("act", claim$act %in% .fg_summary_acts, sprintf(
        "%s needs an \"act\", one of %s.", label,
        paste0("\"", .fg_summary_acts, "\"", collapse = ", ")))
    }
  }

  # Quotes: from a cited message, by its speaker, verbatim.
  allowed_sources <- c(claim$evidence, claim$context)
  for (k in seq_along(claim$quotes)) {
    q <- claim$quotes[[k]]
    what <- sprintf("Quote %d of %s", k, sub("^Claim", "claim", label))
    if (isTRUE(q$bad) || is.na(q$text) || is.na(q$speaker) ||
        (is.na(q$message_id) && (is.na(q$probe) || !isTRUE(ctx$include_probes)))) {
      check("quote_schema", FALSE, sprintf(
        "%s needs \"message_id\", \"speaker\", and \"text\".", what))
      next
    }
    if (!is.na(q$message_id)) {
      id <- q$message_id
      if (!id %in% valid(id)) next
      key <- as.character(id)
      source_text <- ctx$text_of[[key]]
      source_label <- ctx$label_of[[key]]
      check("quote_source", id %in% allowed_sources, sprintf(
        "%s is from message %d, which the claim does not cite in \"evidence\" or \"context\".",
        what, id))
      where <- sprintf("message %d", id)
    } else {
      p <- match(q$probe, ctx$probes$key)
      if (!check("quote_source", !is.na(p) && q$probe %in% claim$probe_evidence, sprintf(
        "%s is from probe answer %s, which the claim does not cite in \"probe_evidence\".",
        what, q$probe))) next
      source_text <- as.character(ctx$probes$answer[[p]])
      source_label <- ctx$probes$label[[p]]
      where <- sprintf("probe answer %s", q$probe)
    }
    check("quote_speaker", identical(q$speaker, source_label), sprintf(
      "%s is attributed to %s, but %s is by %s.", what, q$speaker, where, source_label))
    match <- .fg_match_quote(q$text, source_text)
    check("quote_length", match$words >= 3L, sprintf(
      "%s has %d word(s); a quote needs at least 3.", what, match$words))
    found <- check("quote_text", match$found, sprintf(
      "%s is not in %s. %s reads: \"%s\"", what, where,
      paste0(toupper(substr(where, 1, 1)), substring(where, 2)), source_text))
    whole <- !found || check("quote_omission", is.na(match$problem), sprintf(
      "%s %s; quote the words as they stand.", what, match$problem))
    if (found && whole && match$words >= 3L) {
      quotes[[length(quotes) + 1L]] <- list(
        quote = k, message_id = q$message_id, probe = q$probe, speaker = q$speaker,
        text = q$text, verbatim = match$verbatim, start = match$start, end = match$end,
        spans = match$spans)
    }
  }

  # Words in quotation marks inside the text must come from a message the
  # claim cites as evidence, by one of its speakers.
  normalized <- .fg_normalize_span(claim$text)$text
  marked <- regmatches(normalized, gregexpr("\"[^\"]+\"", normalized))[[1]]
  marked <- gsub("^\"|\"$", "", marked)
  marked <- marked[grepl("[\\p{L}\\p{N}]", marked, perl = TRUE)]
  sources <- unname(ctx$text_of[as.character(evidence)])
  if (isTRUE(ctx$include_probes)) {
    for (key in intersect(claim$probe_evidence, ctx$probes$key)) {
      sources <- c(sources, as.character(ctx$probes$answer[[match(key, ctx$probes$key)]]))
    }
  }
  for (words in marked) {
    found <- any(vapply(sources, function(src) {
      m <- .fg_match_quote(words, src)
      isTRUE(m$found) && is.na(m$problem)
    }, logical(1)))
    check("text_quote", found, sprintf(paste0(
      "%s puts \"%s\" in quotation marks, but those words are not in a message it cites ",
      "as evidence by %s. Give quotes in \"quotes\", each with its message."), label, words,
      if (length(claim$speakers)) paste(claim$speakers, collapse = " or ") else "its speakers"))
  }

  # A change cites two messages of one speaker, earlier first.
  if (identical(claim$kind, "change")) {
    order <- valid(claim$evidence_order)
    speakers <- unique(unname(ctx$label_of[as.character(order)]))
    check("change", length(order) >= 2L && length(speakers) == 1L &&
            identical(claim$speakers, speakers) && !is.unsorted(order, strictly = TRUE),
          sprintf(paste0("%s is a change; it must cite two messages by the same speaker, ",
                         "the earlier first, and name that speaker alone."), label))
  }
  if (identical(claim$kind, "disagreement")) {
    check("holders", length(claim$speakers) >= 2L, sprintf(
      "%s is a disagreement; name the participants on every side.", label))
  }
  if (identical(claim$kind, "minority")) {
    check("holders", length(claim$speakers) == 1L, sprintf(
      "%s is a position held alone; name exactly one speaker.", label))
  }
  if (identical(by, "disagreements") && claim$kind %in% kinds) {
    .fg_check_ending(claim, ctx, scope, check)
  }

  # Handles in the text: every P-number on the roster; every participant
  # named among the claim's speakers or the speakers of the messages it cites
  # as context; a view given only to a participant who spoke; no user ID or
  # name the model was not shown as a label or in a message.
  named <- unique(regmatches(claim$text, gregexpr("\\bP[0-9]+\\b", claim$text, perl = TRUE))[[1]])
  off <- setdiff(named, ctx$participant_labels)
  check("roster", !length(off), sprintf(
    "%s names %s, who is not in this discussion; the participants are %s.", label,
    paste(off, collapse = ", "), paste(ctx$participant_labels, collapse = ", ")))
  mentioned <- ctx$participant_labels[vapply(ctx$participant_labels, function(l) {
    grepl(.fg_label_pattern(l), claim$text, perl = TRUE)
  }, logical(1))]
  context_by <- unname(ctx$label_of[as.character(valid(claim$context))])
  loose <- setdiff(mentioned, c(claim$speakers, context_by))
  check("text_speakers", !length(loose), sprintf(paste0(
    "%s names %s in its text but neither lists %s among its speakers nor cites a ",
    "message by %s."), label, paste(loose, collapse = " and "),
    if (length(loose) > 1L) "them" else "that participant",
    if (length(loose) > 1L) "them" else "that participant"))
  if (stated) {
    silent <- setdiff(mentioned, spoke_here)
    check("silent", !length(silent), sprintf(paste0(
      "%s gives a view to %s, who has no message here; report silence in a separate ",
      "claim with the status \"not_stated\"."), label, paste(silent, collapse = " and ")))
  } else {
    # A claim of silence about a participant who spoke is false.
    silence <- paste0("\\b(silent|silence|did not speak|didn't speak|never spoke|",
                      "said nothing|did not say anything|didn't say anything|stayed quiet|",
                      "remained quiet|did not take part|didn't take part|no messages?)\\b",
                      "(?!\\s+(about|on|regarding|concerning|of|to)\\b)")
    if (grepl(silence, tolower(normalized), perl = TRUE)) {
      spoke <- intersect(unique(c(mentioned, claim$speakers)), spoke_here)
      check("silence", !length(spoke), paste(vapply(spoke, function(s) sprintf(
        "%s says %s did not speak, but %s spoke in message %s.", label, s, s,
        paste0("#", ctx$msgs$message_id[ctx$msgs$label == s &
                                          ctx$msgs$message_id %in% unit$scope],
               collapse = ", ")), character(1)), collapse = " "))
    }
  }
  if (length(ctx$hidden_names)) {
    hidden <- names(ctx$hidden_names)[vapply(names(ctx$hidden_names), function(n) {
      grepl(.fg_label_pattern(n), claim$text, perl = TRUE)
    }, logical(1))]
    check("labels", !length(hidden), sprintf(
      "%s uses the name %s; refer to that speaker as %s.", label, paste(hidden, collapse = ", "),
      paste(unname(ctx$hidden_names[hidden]), collapse = ", ")))
  }

  # A claim for the whole group cites every participant who spoke on each
  # item it cites. When the claim is about the moderator or another voice
  # alone, an invitation such as "asked everyone" does not count.
  others_only <- length(claim$speakers) > 0L &&
    !any(claim$speakers %in% ctx$participant_labels)
  if (.fg_universal_claim(claim$text, invitations = others_only)) {
    cited_ok <- valid(c(claim$evidence, claim$context))
    items <- unique(unname(ctx$item_of[as.character(cited_ok)]))
    msgs <- ctx$msgs[ctx$msgs$message_id %in% unit$scope, , drop = FALSE]
    missing <- character()
    for (k in items) {
      spoke <- unique(msgs$label[msgs$item == k & msgs$role == "participant"])
      cited_by <- unique(unname(ctx$label_of[as.character(
        evidence[ctx$item_of[as.character(evidence)] == k])]))
      lacking <- setdiff(spoke, cited_by)
      if (length(lacking)) {
        missing <- c(missing, sprintf("%s on item %d", paste(lacking, collapse = ", "), k))
      }
    }
    check("universal", length(items) > 0L && !length(missing), if (!length(items)) {
      sprintf(paste0("%s speaks for the whole group but cites no message; cite a message ",
                     "by every participant who spoke, say who held the view, or reword it ",
                     "if it is not about the participants."), label)
    } else {
      sprintf(paste0("%s speaks for the whole group but cites no message by %s; cite them, ",
                     "say who held the view, or reword it if it is not about the ",
                     "participants."), label, paste(missing, collapse = "; "))
    })
  }

  # Without include_probes, no wording from a private answer.
  if (!isTRUE(ctx$include_probes) && length(ctx$probe_ngrams)) {
    runs <- .fg_ngrams(claim$text)
    leaked <- runs[runs %in% ctx$probe_ngrams & !runs %in% ctx$transcript_ngrams]
    check("probe_leak", !length(leaked), sprintf(
      "%s repeats words from a private probe answer that were not said in the discussion (\"%s\").",
      label, leaked[1]))
  }
  out()
}

.fg_check_ending <- function(claim, ctx, scope, check) {
  ending <- claim$ending
  label <- claim$label
  if (!check("ending", !is.null(ending) && ending$how %in% .fg_summary_endings, sprintf(
    "%s needs an \"ending\" whose \"how\" is \"concession\", \"moved_on\", or \"unresolved\".",
    label))) return(invisible())
  if (identical(ending$how, "unresolved")) return(invisible())
  id <- ending$message_id
  if (!check("ending", !is.na(id) && id %in% scope, sprintf(
    "%s ends with \"%s\" but gives no message of this discussion for it.", label,
    ending$how))) return(invisible())
  first <- suppressWarnings(min(claim$evidence))
  after <- !is.finite(first) || id > first
  who <- ctx$label_of[[as.character(id)]]
  if (identical(ending$how, "concession")) {
    check("ending", who %in% claim$speakers && after, sprintf(
      "%s ends with a concession in message %d, but that message is by %s and must be a later message by one of %s.",
      label, id, who, paste(claim$speakers, collapse = ", ")))
  } else {
    check("ending", identical(ctx$role_of[[as.character(id)]], "moderator") && after, sprintf(
      "%s ends with the moderator moving on in message %d, which must be a later message by the moderator.",
      label, id))
  }
  invisible()
}

# Coverage of a reply, counted over the claims that pass: every participant,
# item speaker, message, or stage the unit requires, in every length asked
# for. A participant counts as covered only by a claim that reports what
# they said, with a message of theirs as evidence.
.fg_check_coverage <- function(claims, levels, unit) {
  rows <- list()
  add <- function(check, passed, detail) {
    rows[[length(rows) + 1L]] <<- list(check = check, passed = isTRUE(passed),
                                       detail = if (isTRUE(passed)) NA_character_ else detail)
  }
  ctx <- unit$ctx
  ids <- vapply(claims, `[[`, integer(1), "id")
  unit_of <- vapply(claims, function(cl) {
    u <- cl$unit %|NA|% NA_character_
    if (unit$by %in% c("turn", "item")) gsub("[^0-9]", "", u) else
      if (identical(unit$by, "trajectory")) tolower(u) else u
  }, character(1))
  in_level <- function(level) unit_of[match(levels[[level]], ids)]
  reports <- function(cl, s) {
    own <- intersect(cl$evidence, unit$scope)
    !identical(cl$status, "not_stated") && s %in% cl$speakers &&
      any(unname(ctx$label_of[as.character(own)]) == s)
  }
  by <- unit$by
  if (identical(by, "participant")) {
    ok <- any(vapply(claims, function(cl) identical(cl$unit, unit$key) && reports(cl, unit$key),
                     logical(1)))
    add("coverage", ok, sprintf(paste0(
      "The reply has no claim that reports what %s said; give at least one claim with ",
      "\"unit\": \"%s\", %s among its speakers, and a message by %s as evidence."),
      unit$key, unit$key, unit$key, unit$key))
  }
  if (identical(by, "item")) {
    for (s in unit$item_speakers) {
      own <- ctx$msgs$message_id[ctx$msgs$label == s & ctx$msgs$message_id %in% unit$scope]
      add("coverage", any(vapply(claims, reports, logical(1), s = s)), sprintf(paste0(
        "The reply does not report %s, who spoke on this item (message %s); give a claim ",
        "with %s among its speakers and a message by %s as evidence."), s,
        paste0("#", own, collapse = ", "), s, s))
    }
  }
  if (identical(by, "turn")) {
    lacking <- setdiff(as.character(unit$chunk), unit_of)
    add("coverage", !length(lacking), sprintf("Message %s has no claim.",
                                              paste(lacking, collapse = ", ")))
    for (level in unit$levels) {
      lacking <- setdiff(as.character(unit$chunk), in_level(level))
      add("coverage", !length(lacking), sprintf(
        "The \"%s\" list has no claim for message %s.", level, paste(lacking, collapse = ", ")))
    }
  }
  if (identical(by, "trajectory")) {
    for (level in unit$levels) {
      stages <- names(.fg_level_stages(unit$stages, level))
      listed <- in_level(level)
      lacking <- setdiff(stages, listed)
      foreign <- setdiff(listed, stages)
      add("coverage", !length(lacking), sprintf(
        "The \"%s\" list has no claim for stage %s.", level, paste(lacking, collapse = ", ")))
      add("coverage", !length(foreign), sprintf(
        "The \"%s\" list includes claims for stage %s; it uses the stages %s.", level,
        paste(foreign, collapse = ", "), paste(stages, collapse = ", ")))
    }
  }
  if (!identical(by, "disagreements")) {
    for (level in unit$levels) {
      add("coverage", length(levels[[level]]) > 0L,
          sprintf("The \"%s\" list has no claim that passed the checks.", level))
    }
  }
  rows
}

# ---- Quotes and words -----------------------------------------------------------

.fg_quote_singles <- c("\u2018", "\u2019", "\u201a", "\u201b", "\u2032", "\u02bc")
.fg_quote_doubles <- c("\u201c", "\u201d", "\u201e", "\u201f", "\u2033", "\u00ab", "\u00bb")
.fg_space_marks <- c("\u00a0", "\u1680", "\u2000", "\u2001", "\u2002", "\u2003", "\u2004",
                     "\u2005", "\u2006", "\u2007", "\u2008", "\u2009", "\u200a", "\u2028",
                     "\u2029", "\u202f", "\u205f", "\u3000", "\u200c")
# Arabic letter forms written for their Persian counterparts: yeh, alef
# maksura, and kaf.
.fg_letter_forms <- c("\u064a" = "\u06cc", "\u0649" = "\u06cc", "\u0643" = "\u06a9")

# Text with case, whitespace, and quotation marks normalized, and nothing
# else beyond the Arabic and Persian forms of yeh and kaf (the zero-width
# non-joiner counts as a space); `pos` gives the position in the original
# text of each character.
.fg_normalize_span <- function(text) {
  chars <- strsplit(enc2utf8(as.character(text %|NA|% "")), "")[[1]]
  if (!length(chars)) return(list(text = "", pos = integer()))
  chars[chars %in% .fg_quote_singles] <- "'"
  chars[chars %in% .fg_quote_doubles] <- "\""
  forms <- chars %in% names(.fg_letter_forms)
  chars[forms] <- .fg_letter_forms[chars[forms]]
  space <- chars %in% .fg_space_marks | grepl("^\\s$", chars, perl = TRUE)
  chars[space] <- " "
  chars <- tolower(chars)
  pos <- seq_along(chars)
  keep <- !(space & c(FALSE, utils::head(space, -1L)))
  chars <- chars[keep]
  pos <- pos[keep]
  filled <- which(chars != " ")
  if (!length(filled)) return(list(text = "", pos = integer()))
  span <- filled[1]:filled[length(filled)]
  list(text = paste(chars[span], collapse = ""), pos = pos[span])
}

.fg_words <- function(text) {
  normalized <- .fg_normalize_span(text)$text
  regmatches(normalized, gregexpr("[\\p{L}\\p{N}]+(?:'[\\p{L}\\p{N}]+)*", normalized,
                                  perl = TRUE))[[1]]
}

# The runs of `n` consecutive words in a text.
.fg_ngrams <- function(text, n = 6L) {
  words <- .fg_words(text)
  if (length(words) < n) return(character())
  vapply(seq_len(length(words) - n + 1L), function(i) {
    paste(words[i:(i + n - 1L)], collapse = " ")
  }, character(1))
}

.fg_group_verbs <- paste0(
  "(agreed|agree|agrees|said|say|says|felt|feel|feels|thought|think|thinks|",
  "favored|favor|favors|favoured|favour|supported|support|supports|opposed|oppose|",
  "opposes|wanted|want|wants|shared|share|shares|accepted|accept|accepts|endorsed|",
  "liked|like|disliked|preferred|prefer|prefers|rejected|reject|backed|objected|",
  "object|disagreed|disagree|in favou?r|in agreement)")

# Whether a claim speaks for the whole group: "everyone", "all of them",
# "consensus", "unanimous", "no one", "the whole group", and the like. Words
# inside double quotation marks are the speaker's, and are checked as quotes.
# With `invitations = TRUE`, a phrase such as "asked everyone" or "thanked
# all" is not read as a claim about the group.
.fg_universal_claim <- function(text, invitations = FALSE) {
  value <- .fg_normalize_span(text)$text
  value <- gsub("\"[^\"]*\"", " ", value)
  if (invitations) {
    value <- gsub(paste0(
      "\\b(asks?|asked|asking|invites?|invited|inviting|welcomes?|welcomed|welcoming|",
      "thanks?|thanked|thanking|greets?|greeted|greeting|tells?|told|telling|reminds?|",
      "reminded|encourages?|encouraged|addresses|addressed|calls? on|called on)\\b",
      "(\\s+[\\p{L}']+){0,3}?\\s+(everyone|everybody|all)\\b"), " ", value, perl = TRUE)
  }
  people <- paste0("(participants|members|speakers|respondents|people|group|others|",
                   "them|us|[0-9]+|two|three|four|five|six|seven|eight|nine|ten|",
                   "eleven|twelve)")
  patterns <- c(
    "\\b(everyone|everybody|consensus|unanimous|unanimously|nobody)\\b",
    "\\bno one\\b",
    "\\bnone of (them|us|the)\\b",
    "\\b(every|each) (participant|member|speaker|respondent|person)s?\\b",
    "\\b(each|every one|any) of (them|us|the participants|the group)\\b",
    "\\b(whole|entire) group\\b",
    paste0("\\b(the group|the participants|participants|the others|both)( all| both)? ",
           .fg_group_verbs, "\\b"),
    "\\bboth (of them|of us|participants|speakers)\\b",
    "\\b(broad|general|wide|widespread|shared|common) (agreement|support|consensus|opposition)\\b",
    "\\b(they|we|you) (all|both)\\b",
    "\\ball (but|except)\\b",
    paste0("\\ball (of )?(the )?", people, "\\b"),
    paste0("\\ball ", .fg_group_verbs, "\\b"))
  any(vapply(patterns, grepl, logical(1), x = value, perl = TRUE))
}

# The position at or after `from` where `needle` occurs in `haystack` on word
# boundaries, or NA. An apostrophe or hyphen between letters is part of a
# word, so a match cannot start or end inside "don't" or "well-known".
.fg_find_span <- function(haystack, needle, from = 1L) {
  n <- nchar(needle)
  size <- nchar(haystack)
  word <- function(ch) grepl("[\\p{L}\\p{N}]", ch, perl = TRUE)
  joins <- c("'", "-")
  inside_left <- function(p) {
    if (p == 1L) return(FALSE)
    before <- substr(haystack, p - 1L, p - 1L)
    word(before) || (before %in% joins && p >= 3L && word(substr(haystack, p - 2L, p - 2L)))
  }
  inside_right <- function(e) {
    if (e >= size) return(FALSE)
    after <- substr(haystack, e + 1L, e + 1L)
    word(after) || (after %in% joins && e + 2L <= size && word(substr(haystack, e + 2L, e + 2L)))
  }
  first_word <- word(substr(needle, 1L, 1L))
  last_word <- word(substr(needle, n, n))
  start <- from
  while (n > 0L && start <= size - n + 1L) {
    hit <- regexpr(needle, substring(haystack, start), fixed = TRUE)
    if (hit < 0L) return(NA_integer_)
    p <- start + as.integer(hit) - 1L
    e <- p + n - 1L
    if ((!first_word || !inside_left(p)) && (!last_word || !inside_right(e))) return(p)
    start <- p + 1L
  }
  NA_integer_
}

.fg_count_words <- function(text) {
  length(regmatches(text, gregexpr("[\\p{L}\\p{N}]+(?:['-][\\p{L}\\p{N}]+)*", text,
                                   perl = TRUE))[[1]])
}

# Match a quote against its source: the parts of the quote joined by "..."
# must occur in order, each on word boundaries, after normalizing case,
# whitespace, and quotation marks. A quote of several parts is not whole when
# a part has fewer than 2 words or when the words it leaves out include a
# negation. `source` is the message or probe answer as recorded. Returns
# whether it was found, the problem if it is not whole, its word count, and
# the span cut from the source, with its character positions.
.fg_match_quote <- function(quote, source) {
  q <- .fg_normalize_span(quote)$text
  q <- gsub("^[\"']+|[\"']+$", "", q)
  parts <- strsplit(q, "\\s*[\\[(]?\\s*(\\.\\s?\\.\\s?\\.|\u2026)\\s*[\\])]?\\s*", perl = TRUE)[[1]]
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  counts <- vapply(parts, .fg_count_words, integer(1), USE.NAMES = FALSE)
  none <- list(found = FALSE, problem = NA_character_, words = as.integer(sum(counts)),
               verbatim = NA_character_, start = NA_integer_, end = NA_integer_, spans = NULL)
  if (!length(parts)) return(none)
  source <- enc2utf8(as.character(source))
  src <- .fg_normalize_span(source)
  from <- 1L
  first <- integer()
  last <- integer()
  gaps <- character()
  for (part in parts) {
    p <- .fg_find_span(src$text, part, from)
    if (is.na(p)) return(none)
    e <- p + nchar(part) - 1L
    if (length(last)) gaps <- c(gaps, substr(src$text, from, p - 1L))
    first <- c(first, p)
    last <- c(last, e)
    from <- e + 1L
  }
  problem <- NA_character_
  negation <- "(?<![\\p{L}])(not|no|never|nor|neither|none|without|cannot)(?![\\p{L}])|n't\\b"
  dropped <- gaps[grepl(negation, gaps, perl = TRUE)]
  if (length(parts) > 1L && any(counts < 2L)) {
    problem <- "has a part with fewer than 2 words between its \"...\" marks"
  } else if (length(dropped)) {
    problem <- sprintf("leaves out \"%s\", which changes its sense", trimws(dropped[1]))
  }
  starts <- src$pos[first]
  ends <- src$pos[last]
  list(found = TRUE, problem = problem, words = as.integer(sum(counts)),
       verbatim = paste(substring(source, starts, ends), collapse = " ... "),
       start = starts[1], end = ends[length(ends)],
       spans = data.frame(start = starts, end = ends))
}

# ---- The result -----------------------------------------------------------------

.fg_checks_frame <- function(checks, unit, attempt) {
  if (!nrow(checks)) return(NULL)
  cbind(data.frame(segment = unit$segment, unit = unit$key, attempt = as.integer(attempt),
                   stringsAsFactors = FALSE), checks)
}

.fg_summary_result <- function(plan, outcomes, store, source, meta, complete, failure) {
  summaries <- list()
  claims <- list()
  quotes <- list()
  checks <- list()
  rejected <- list()
  budgets <- meta$budgets
  for (i in seq_along(plan)) {
    unit <- plan[[i]]
    run <- outcomes[[i]]
    final <- if (is.null(run)) NULL else run$attempts[[length(run$attempts)]]
    if (!is.null(run)) {
      for (k in seq_along(run$attempts)) {
        checks[[length(checks) + 1L]] <- .fg_checks_frame(run$attempts[[k]]$verdict$checks, unit, k)
      }
    }
    kept <- list()
    if (!is.null(final)) {
      verdict <- final$verdict
      for (j in seq_along(verdict$claims)) {
        cl <- verdict$claims[[j]]
        if (length(cl$failures)) {
          rejected[[length(rejected) + 1L]] <- .fg_claim_row(cl, unit, verdict, length(run$attempts),
                                                             reasons = paste(cl$failures, collapse = " "))
        } else {
          kept[[length(kept) + 1L]] <- cl
          claims[[length(claims) + 1L]] <- .fg_claim_row(cl, unit, verdict, length(run$attempts))
          for (q in verdict$quotes[[j]]) {
            quotes[[length(quotes) + 1L]] <- .fg_quote_row(q, cl, unit)
          }
        }
      }
    }
    summaries[[length(summaries) + 1L]] <- .fg_summary_rows(unit, kept, final, budgets)
  }
  bind <- function(rows, empty) {
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) return(empty)
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }
  claim_template <- .fg_claim_row(NULL)
  calls <- .fg_calls_data(store$calls)
  calls$segment <- .fg_record_column(store$calls, "segment", "character")
  calls$unit <- .fg_record_column(store$calls, "unit", "character")
  turns <- do.call(rbind, lapply(source$segments, function(s) {
    t <- .fg_segment_turns(s)
    t$label <- s$transcript$label[match(t$message_id, s$transcript$message_id)]
    t
  }))
  rownames(turns) <- NULL
  meta$usage <- list(
    sent = sum(as.numeric(calls$sent_tokens), na.rm = TRUE),
    rec = sum(as.numeric(calls$rec_tokens), na.rm = TRUE),
    total = sum(as.numeric(calls$total_tokens), na.rm = TRUE),
    unknown = as.numeric(sum(is.na(calls$total_tokens))))
  meta$complete <- complete
  meta$failure <- failure
  structure(list(
    summaries = bind(summaries, .fg_summary_rows(NULL)),
    claims = bind(claims, claim_template),
    quotes = bind(quotes, .fg_quote_row(NULL)),
    checks = bind(checks, data.frame(segment = character(), unit = character(),
                                     attempt = integer(), claim = integer(),
                                     check = character(), passed = logical(),
                                     detail = character(), stringsAsFactors = FALSE)),
    rejected = bind(rejected, cbind(claim_template, reasons = character())),
    calls = calls,
    turns = turns,
    metadata = meta
  ), class = c("focus_group_summary", "list"))
}

.fg_claim_row <- function(cl, unit = NULL, verdict = NULL, attempt = NA_integer_,
                          reasons = NULL) {
  if (is.null(cl)) {
    out <- data.frame(segment = character(), unit = character(), claim = integer(),
                      kind = character(), status = character(), text = character(),
                      stringsAsFactors = FALSE)
    out$speakers <- I(list())
    out$speaker_ids <- I(list())
    out$evidence <- I(list())
    out$context <- I(list())
    out$probe_evidence <- I(list())
    out$act <- character()
    out$ending <- character()
    out$ending_message_id <- integer()
    out$levels <- I(list())
    out$attempt <- integer()
    return(out)
  }
  key <- cl$unit %|NA|% NA_character_
  if (unit$by %in% c("turn", "item")) key <- gsub("[^0-9]", "", key)
  if (identical(unit$by, "trajectory")) key <- tolower(key)
  ids <- unit$ctx$label_to_id
  in_levels <- names(Filter(function(v) cl$id %in% v, verdict$levels))
  out <- data.frame(segment = unit$segment, unit = key, claim = cl$id %|NA|% NA_integer_,
                    kind = cl$kind %|NA|% NA_character_,
                    status = cl$status %|NA|% NA_character_,
                    text = cl$text %|NA|% NA_character_, stringsAsFactors = FALSE)
  out$speakers <- I(list(cl$speakers))
  out$speaker_ids <- I(list(vapply(cl$speakers, function(s) {
    if (s %in% names(ids)) ids[[s]] else NA_character_
  }, character(1), USE.NAMES = FALSE)))
  out$evidence <- I(list(cl$evidence))
  out$context <- I(list(cl$context))
  out$probe_evidence <- I(list(cl$probe_evidence))
  out$act <- cl$act %|NA|% NA_character_
  out$ending <- if (is.null(cl$ending)) NA_character_ else cl$ending$how %|NA|% NA_character_
  out$ending_message_id <- if (is.null(cl$ending)) NA_integer_ else
    as.integer(cl$ending$message_id %|NA|% NA_integer_)
  out$levels <- I(list(in_levels))
  out$attempt <- as.integer(attempt)
  if (!is.null(reasons)) out$reasons <- reasons
  out
}

.fg_quote_row <- function(q, cl = NULL, unit = NULL) {
  if (is.null(q)) {
    out <- data.frame(segment = character(), unit = character(), claim = integer(),
                      quote = integer(), message_id = integer(), probe = character(),
                      speaker = character(), speaker_id = character(), text = character(),
                      verbatim = character(), start = integer(), end = integer(),
                      stringsAsFactors = FALSE)
    out$spans <- I(list())
    return(out)
  }
  key <- cl$unit %|NA|% NA_character_
  if (unit$by %in% c("turn", "item")) key <- gsub("[^0-9]", "", key)
  if (identical(unit$by, "trajectory")) key <- tolower(key)
  ids <- unit$ctx$label_to_id
  out <- data.frame(segment = unit$segment, unit = key, claim = cl$id,
                    quote = as.integer(q$quote), message_id = as.integer(q$message_id),
                    probe = as.character(q$probe %|NA|% NA_character_), speaker = q$speaker,
                    speaker_id = if (q$speaker %in% names(ids)) ids[[q$speaker]] else NA_character_,
                    text = q$text, verbatim = q$verbatim, start = as.integer(q$start),
                    end = as.integer(q$end), stringsAsFactors = FALSE)
  out$spans <- I(list(q$spans))
  out
}

# The prose of a unit, one row per summary unit and length, rendered from the
# claims kept.
.fg_summary_rows <- function(unit, kept = list(), final = NULL, budgets = NULL) {
  empty <- data.frame(segment = character(), by = character(), unit = character(),
                      level = character(), text = character(), n_claims = integer(),
                      words = integer(), budget = character(), n_turns = integer(),
                      speaker_id = character(), note = character(),
                      stringsAsFactors = FALSE)
  if (is.null(unit)) return(empty)
  ctx <- unit$ctx
  key_of <- function(cl) {
    u <- cl$unit %|NA|% NA_character_
    if (unit$by %in% c("turn", "item")) gsub("[^0-9]", "", u) else
      if (identical(unit$by, "trajectory")) tolower(u) else u
  }
  rows <- list()
  for (level in unit$levels) {
    targets <- switch(unit$by,
      trajectory = names(.fg_level_stages(unit$stages, level)),
      disagreements = unique(vapply(kept, key_of, character(1))),
      unit$rows)
    if (!length(targets)) targets <- NA_character_
    listed <- if (is.null(final)) integer() else final$verdict$levels[[level]]
    for (target in targets) {
      chosen <- Filter(function(cl) {
        identical(unit$by, "session") || identical(key_of(cl), target)
      }, kept)
      chosen <- chosen[order(match(vapply(chosen, `[[`, integer(1), "id"), listed))]
      chosen <- Filter(function(cl) cl$id %in% listed, chosen)
      text <- if (length(chosen)) {
        paste(vapply(chosen, .fg_render_claim, character(1), level = level, by = unit$by),
              collapse = " ")
      } else NA_character_
      id <- NA_character_
      n_turns <- unit$n_turns
      if (identical(unit$by, "participant")) id <- unit$speaker_id
      if (identical(unit$by, "turn")) {
        row <- ctx$msgs[ctx$msgs$message_id == as.integer(target), ]
        id <- row$speaker_id[1]
        n_turns <- as.integer(identical(row$role[1], "participant"))
      }
      if (identical(unit$by, "trajectory")) {
        ids <- .fg_level_stages(unit$stages, level)[[target]]
        n_turns <- sum(ctx$msgs$role == "participant" & ctx$msgs$message_id %in% ids)
      }
      note <- if (!isTRUE(unit$call)) {
        if (identical(unit$by, "participant")) "no turns in this segment; no call" else
          "no participant turns; no call"
      } else if (is.null(final)) "not summarized" else if (!length(chosen)) {
        if (identical(unit$by, "disagreements")) "no entries reported" else "no claim kept"
      } else NA_character_
      rows[[length(rows) + 1L]] <- data.frame(
        segment = unit$segment, by = unit$by,
        unit = if (is.na(target)) NA_character_ else target, level = level, text = text,
        n_claims = length(chosen), words = if (is.na(text)) 0L else .fg_word_count(text),
        budget = unname(budgets[[level]] %||% NA_character_), n_turns = as.integer(n_turns),
        speaker_id = id, note = note, stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

# One claim as prose at a length: its text, with its status when it is not
# "stated" at every length; at "brief" and "detailed" the ending of a
# disagreement; at "detailed" its quotes and a turn's act code.
.fg_render_claim <- function(cl, level, by) {
  text <- cl$text
  if (identical(level, "detailed") && identical(by, "turn") && !is.na(cl$act)) {
    text <- sprintf("[%s] %s", cl$act, text)
  }
  if (level %in% c("brief", "detailed") && !is.null(cl$ending) && !is.na(cl$ending$how)) {
    ending <- switch(cl$ending$how,
      concession = sprintf("ended with a concession at #%d", cl$ending$message_id),
      moved_on = sprintf("ended when the moderator moved on at #%d", cl$ending$message_id),
      unresolved = "unresolved",
      NULL)
    if (!is.null(ending)) {
      text <- sprintf("%s (%s)", sub("[.]$", "", text), ending)
      if (!grepl("[.!?]$", text)) text <- paste0(text, ".")
    }
  }
  if (!identical(cl$status, "stated")) {
    text <- sprintf("%s [%s]", text, gsub("_", " ", cl$status, fixed = TRUE))
  }
  if (identical(level, "detailed")) {
    for (q in cl$quotes) {
      if (is.na(q$text)) next
      where <- if (!is.na(q$message_id)) sprintf("#%d", q$message_id) else q$probe
      text <- sprintf("%s \"%s\" (%s, %s).", text, q$text, q$speaker, where)
    }
  }
  text
}

#' Print a Focus Group Summary
#'
#' Prints the unit, the lengths, the model, the numbers of calls, claims, and
#' quotes, and the summaries at the shortest length asked for.
#' @param x A `focus_group_summary`.
#' @param n Maximum number of summaries to print.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.focus_group_summary <- function(x, n = 12L, ...) {
  m <- x$metadata
  cat("<focus_group_summary>\n")
  cat("By: ", m$by, " | Lengths: ", paste(m$compression, collapse = ", "), "\n", sep = "")
  cat("Model: ", m$provider, "/", m$model, " | Calls: ", nrow(x$calls),
      " | Structured output: ", m$structured_output$mode, "\n", sep = "")
  cat("Claims kept: ", nrow(x$claims),
      if (nrow(x$rejected)) paste0(" | Dropped: ", nrow(x$rejected)),
      " | Quotes: ", nrow(x$quotes), "\n", sep = "")
  if (!isTRUE(m$complete)) {
    cat("Incomplete: stopped at ", m$failure$unit %||% "a unit", "\n", sep = "")
  }
  level <- m$compression[1]
  rows <- x$summaries[x$summaries$level == level, , drop = FALSE]
  width <- max(40L, getOption("width", 80L) - 4L)
  for (i in seq_len(min(nrow(rows), n))) {
    r <- rows[i, ]
    where <- if (identical(r$segment, "session")) "" else paste0("[", r$segment, "] ")
    what <- if (is.na(r$unit)) r$by else r$unit
    body <- if (is.na(r$text)) paste0("(", r$note %|NA|% "no text", ")") else r$text
    cat(paste(strwrap(paste0(where, what, ": ", body), width = width, exdent = 4L),
              collapse = "\n"), "\n", sep = "")
  }
  if (nrow(rows) > n) {
    cat("... and ", nrow(rows) - n, " more; see `$summaries`.\n", sep = "")
  }
  invisible(x)
}

#' Tables of a Focus Group Summary
#'
#' @param x A `focus_group_summary`.
#' @param row.names,optional Unused; for compatibility with the generic.
#' @param ... Unused.
#' @param component The table to return: `"summaries"` (the default),
#'   `"claims"`, `"quotes"`, `"checks"`, `"rejected"`, `"calls"`, or
#'   `"turns"`.
#' @return A data frame.
#' @keywords internal
#' @export
as.data.frame.focus_group_summary <- function(x, row.names = NULL, optional = FALSE, ...,
                                              component = c("summaries", "claims", "quotes",
                                                            "checks", "rejected", "calls",
                                                            "turns")) {
  component <- match.arg(component)
  x[[component]]
}
