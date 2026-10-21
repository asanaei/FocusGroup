#' 
NULL

# Default moderator scripts used when guide supplies numeric counts.
# Each public default count fits within its phase bank, so the standard session
# does not repeat a question.
.fg_phase_script_banks <- function(topic) {
  list(
    Opening = c(
      paste0("Welcome the participants, state the purpose of the discussion on ",
             topic, ", and explain the ground rules."),
      paste0("Explain how the discussion of ", topic,
             " will proceed, affirm the moderator's neutrality, and state that respectful disagreement is welcome.")
    ),
    Icebreaker = c(
      paste0("What first comes to mind when you think about ", topic, "?"),
      paste0("What recent experience has shaped your first impression of ", topic, "?"),
      paste0("How would you describe your connection to ", topic, " to someone new to it?")
    ),
    Engagement = c(
      paste0("Where does ", topic, " enter your daily life or work?"),
      paste0("Which part of ", topic, " matters most to you at present?"),
      paste0("What has changed most in your experience of ", topic, "?"),
      paste0("Who benefits most from current approaches to ", topic, "?"),
      paste0("Who bears the greatest costs associated with ", topic, "?"),
      paste0("What practical obstacle shapes how people respond to ", topic, "?"),
      paste0("Where do your own experiences differ from common accounts of ", topic, "?"),
      paste0("What aspect of ", topic, " deserves more public attention?")
    ),
    Exploration = c(
      paste0("What tensions or trade-offs are central to ", topic, "?"),
      paste0("Where does the group appear to agree about ", topic, "?"),
      paste0("Where do the sharpest differences about ", topic, " remain?"),
      paste0("What evidence would change your view of ", topic, "?"),
      paste0("Which proposed response to ", topic, " seems most credible, and why?"),
      paste0("What unintended consequence should be considered in decisions about ", topic, "?"),
      paste0("How do institutions shape people's options concerning ", topic, "?"),
      paste0("What important perspective on ", topic, " has not yet been heard?"),
      paste0("What would a fair outcome concerning ", topic, " look like?"),
      paste0("What question about ", topic, " should future research examine?")
    ),
    Closing = c(
      paste0("Summarize the central areas of agreement and disagreement about ",
             topic, " without adding a judgment."),
      "Thank the participants, state that the discussion is complete, and close the session."
    )
  )
}

.fg_extend_script_bank <- function(bank, n, phase, topic) {
  if (n == 0L) return(character(0))
  if (phase == "Closing") {
    final_close <- bank[[length(bank)]]
    if (n == 1L) return(final_close)
    pre_close <- bank[-length(bank)]
    if ((n - 1L) > length(pre_close)) {
      extra <- vapply(seq.int(length(pre_close) + 1L, n - 1L), function(i) {
        paste0("Synthesize one additional unresolved point about ", topic,
               " that has not appeared in the preceding closing remarks (point ",
               i, ").")
      }, character(1))
      pre_close <- c(pre_close, extra)
    }
    return(c(pre_close[seq_len(n - 1L)], final_close))
  }
  if (n <= length(bank)) return(bank[seq_len(n)])
  extra <- vapply(seq.int(length(bank) + 1L, n), function(i) {
    if (phase %in% c("Opening", "Closing")) {
      paste0("Add a distinct ", tolower(phase), " instruction for moderator turn ", i,
             " concerning ", topic, ".")
    } else {
      paste0("What additional ", tolower(phase), " perspective on ", topic,
             " has not yet been discussed? Focus on prompt ", i, ".")
    }
  }, character(1))
  c(bank, extra)
}

# Validate a guide's phase names, order, and values, and return the number of
# moderator turns each phase asks for. The counts let run_focus_group() check
# the model-output estimate before a large numeric count is expanded into a
# script.
.fg_guide_counts <- function(guide) {
  if (!(is.atomic(guide) || is.list(guide)) ||
      is.data.frame(guide)) {
    stop("`guide` must be a named numeric vector or named list.",
         call. = FALSE)
  }
  phase_names <- names(guide)
  if (is.null(phase_names) || anyNA(phase_names) || any(!nzchar(phase_names))) {
    stop("`guide` must have non-empty phase names.", call. = FALSE)
  }

  supported <- c("Opening", "Icebreaker", "Engagement", "Exploration", "Closing")
  matched <- match(tolower(phase_names), tolower(supported))
  if (anyNA(matched)) {
    stop("Unknown phase name(s): ", paste(phase_names[is.na(matched)], collapse = ", "),
         ". Supported phases are ", paste(supported, collapse = ", "), ".",
         call. = FALSE)
  }
  canonical <- supported[matched]
  if (anyDuplicated(canonical)) {
    stop("Each phase may appear only once in `guide`.", call. = FALSE)
  }
  if (is.unsorted(matched)) {
    stop("Phases in `guide` must follow the order Opening, Icebreaker, Engagement, Exploration, Closing.",
         call. = FALSE)
  }

  counts <- vapply(seq_along(guide), function(i) {
    value <- guide[[i]]
    phase <- canonical[[i]]
    if (is.character(value)) {
      if (anyNA(value) || any(!nzchar(trimws(value)))) {
        stop("Character scripts for phase '", phase,
             "' must be non-missing and non-empty.", call. = FALSE)
      }
      return(as.double(length(value)))
    }
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0 || value != floor(value)) {
      shown <- if (is.atomic(value) && length(value) == 1L) {
        format(value)
      } else {
        paste0("a ", class(value)[1], " of length ", length(value))
      }
      stop("The numeric count for phase '", phase,
           "' must be one non-negative whole number, not ", shown, ".",
           call. = FALSE)
    }
    as.double(value)
  }, numeric(1))
  stats::setNames(counts, canonical)
}

# Convert numeric phase counts or user-supplied phase scripts to the structure
# consumed by FocusGroup. Numeric values select distinct defaults; character
# vectors supply the exact ordered moderator questions or instructions.
.fg_build_question_script <- function(guide, topic) {
  if (is.null(guide)) return(list())
  counts <- .fg_guide_counts(guide)
  canonical <- names(counts)
  if (any(counts > .Machine$integer.max)) {
    phase <- canonical[which(counts > .Machine$integer.max)[1]]
    stop("The numeric count for phase '", phase,
         "' exceeds the largest supported count (", .Machine$integer.max, ").",
         call. = FALSE)
  }

  phase_codes <- c(
    Opening = "opening",
    Icebreaker = "icebreaker_question",
    Engagement = "engagement_question",
    Exploration = "exploration_question",
    Closing = "closing"
  )
  banks <- .fg_phase_script_banks(topic)
  out <- list()

  for (i in seq_along(guide)) {
    value <- guide[[i]]
    phase <- canonical[[i]]
    scripts <- if (is.character(value)) {
      unname(value)
    } else {
      .fg_extend_script_bank(banks[[phase]], as.integer(value), phase, topic)
    }
    if (length(scripts)) {
      out <- c(out, lapply(scripts, function(script) {
        list(phase = unname(phase_codes[[phase]]), text = script)
      }))
    }
  }

  if (!length(out)) {
    stop("`guide` must specify at least one moderator turn.",
         call. = FALSE)
  }
  out
}

#' Run a Focus Group Simulation
#'
#' Constructs the agents, moderator guide, and turn-taking flow, then runs one
#' focus group session. `config` has no default; every agent and group-level
#' call uses the one supplied. Before a live run, the function estimates the
#' number of generated model outputs and applies the `max_calls` limit unless
#' `confirm` is `TRUE`.
#'
#' @param topic Character. Focus group topic.
#' @param config An explicit `LLMR::llm_config` used by all agents and group-level
#'   model tasks.
#' @param n_participants Integer. Number of participants, excluding the moderator.
#' @param guide A named numeric vector or named list. The names are phases,
#'   among `"Opening"`, `"Icebreaker"`, `"Engagement"`, `"Exploration"`, and
#'   `"Closing"`, each at most once and in that order. Numeric values select that
#'   many moderator instructions from the phase banks. Character vectors supply
#'   the ordered instructions directly. The default is a compact session (one
#'   opening, one icebreaker, two engagement and three exploration questions,
#'   one closing); with the default flow, participants, response limit, and
#'   call-ons it plans at most 111 model outputs, inside the default
#'   `max_calls` of 120. Larger guides multiply quickly: each question round
#'   costs one output per response plus, under `"desire_based"`, one scoring
#'   output per participant. `NULL` runs a minimal script: an opening, one
#'   open discussion of `topic`, and a closing.
#' @param demographics Optional participant demographics, one row per
#'   participant. With this, `survey_responses`, and personas all absent, the
#'   participants are drawn from the bundled ANES 2024 records.
#' @param survey_responses Optional participant survey answers, one row per
#'   participant.
#' @param flow Character. One of `"round_robin"`, `"probabilistic"`, or
#'   `"desire_based"`.
#' @param seed Optional integer governing in-package sampling: the respondents
#'   drawn from the bundled records and the random choices of the turn-taking
#'   flow, such as the speaker draws of `"probabilistic"` and the tie-breaks of
#'   `"desire_based"`. The caller's random-number state is restored when the
#'   function returns. The option `focusgroup.seed`, when set, fixes the
#'   respondent draw instead.
#' @param message_mode Character. `"roleflip"` or `"flat"`.
#' @param verbose Logical. Print session progress.
#' @param max_participant_responses Optional integer maximum number of participant
#'   responses per moderator question. `NULL`, the default, uses the option
#'   `focusgroup.max_participant_responses`, which is 2 unless you set it.
#' @param max_calls Integer. Maximum estimated live model outputs allowed without
#'   confirmation; a finite whole number.
#' @param confirm Logical. Permit a live run whose estimate exceeds `max_calls`.
#' @param max_tokens_config Optional named list of token limits by call type,
#'   as for `FocusGroup$new()`: `utterance`, `moderator`, `desire`,
#'   `desire_retry`, `summary`, and `interim_summary`. A model that reasons
#'   before it answers needs `desire` and `desire_retry` large enough to hold
#'   its reasoning and the score.
#' @param reasoning_effort Optional reasoning-effort setting passed to the
#'   provider: one string, such as `"low"`, for every call, or a named vector
#'   by call type (`"moderator"`, `"participant"`, `"desire"`, `"summary"`,
#'   `"probe"`). It is written over the `reasoning_effort` of `config` for
#'   those calls; `NULL` leaves `config` as it is. LLMR forwards it on
#'   OpenAI-compatible providers.
#' @param on_desire_failure Character. Under `"desire_based"`, what happens
#'   when a participant's desire score cannot be obtained after its retry.
#'   `"error"` (the default) stops the session with an error of class
#'   `focusgroup_desire_failure`, returned as a partial session as described
#'   in Details. `"random"` chooses the speaker among the participants with
#'   valid scores by the usual rule, and uniformly among the eligible
#'   participants only when none has a valid score; the failure is recorded
#'   in the selected message's `metadata$desire_scoring_failure`.
#' @param injections Optional [fg_injection()] or list of injections, each
#'   placed into the session at its point. To compare sessions that share
#'   everything before an injection, use [run_focus_group_arms()].
#' @param probes Optional [fg_probe()] or list of probes, private questions
#'   asked of each participant at stated points. The answers are returned in
#'   `probes` and never enter the discussion.
#' @param moderator_verbatim `FALSE` (the default) has the moderator's model
#'   render each guide item as a line of its own. `TRUE` delivers every guide
#'   item exactly as written, with no model call, and a vector of item
#'   numbers delivers those items so. A verbatim message carries
#'   `metadata$verbatim = TRUE`. The opening and closing instructions of the
#'   numeric phase banks are instructions to the moderator, so verbatim
#'   delivery suits a guide written as the lines to be said.
#' @param call_on_silent Where the moderator calls on the participants who
#'   have not spoken since the previous such point, as described in Details:
#'   any of `"opening"`, `"midpoint"`, and `"end"` (the default is all
#'   three), guide item numbers, or `"none"` (or `FALSE`) for no call-ons.
#' @param call_on_text The call-on line. It must contain `{participant}`,
#'   which is replaced by the participant's handle; `NULL` uses
#'   "\{participant\}, I'd like to hear from you as well. What are your
#'   thoughts?".
#' @param handles The labels under which the agents see each other: `NULL`
#'   (the default) for `"P1"`, `"P2"`, ... and `"Moderator"`, `"ids"` for the
#'   agent IDs, or a character vector of handles named by agent ID, as for
#'   `FocusGroup$new()`.
#' @param final_summary Logical. `TRUE` (the default) ends the session with a
#'   model-written summary; `FALSE` makes no summary call and leaves
#'   `summary` empty.
#' @param .runner Optional function used instead of live model calls. It
#'   receives a data frame with `config` and `messages` list-columns and returns
#'   those rows with at least `response_text`. Token columns such as
#'   `sent_tokens`, `rec_tokens`, and `total_tokens` are optional; when they are
#'   absent, usage is unknown.
#'
#' @details
#' At the end of each call-on item's discussion the moderator calls on every
#' participant who has not spoken since the previous call-on point, or since
#' the start for the first, in roster order. `"opening"` is the first guide
#' item participants answer, `"midpoint"` the middle one, and `"end"` the
#' last; points that fall on the same item count once, and item numbers count
#' every guide item, as [fg_after_item()] does. Each call-on line is
#' delivered as written, with no model call, and carries
#' `metadata$call_on`; the participant then answers, with a cue in the turn
#' instruction and no desire score, and the reply carries
#' `metadata$called_on = TRUE`. With the default guide the points are items
#' 2, 4, and 7, so every participant speaks during the icebreaker. Krueger
#' (2002) describes the opening question of a focus group as a round robin.
#'
#' The participants and the moderator see each other under handles, never
#' under the IDs of the records: the IDs of [create_agents()] are already
#' `"P1"`, `"P2"`, ..., and the moderator appears as `"Moderator"`. When
#' the moderator's last question opens or closes with one participant's
#' handle ("P3, do you agree?", "Could you say more, P3?") and does not also
#' put it to the group, that participant takes the next turn, without a
#' desire score, and the reply carries `metadata$addressed = TRUE`. A label
#' a speaker writes for itself at the start of its reply (`**MOD:**`,
#' `P3:`) is removed, and the message carries
#' `metadata$self_label_removed = TRUE`; the reply as returned stays in
#' `calls`.
#'
#' The estimate counts one output for each moderator turn the model writes,
#' one for each participant response a question round allows, one for each
#' participant at each call-on point, and one for the final summary. Under
#' `"desire_based"` it adds one desire score per participant before each
#' response the flow chooses. With the default guide, six participants, and
#' the default flow, two responses per question give an estimate of 111
#' outputs (93 without call-ons) and three give 153. A session can make
#' fewer calls, since the desire-based flow does not score the previous
#' speaker, an addressed turn needs no scores, and a call-on point calls on
#' only the participants who have been silent, or more, since incomplete
#' replies are retried and a long discussion triggers interim summaries.
#' `max_calls` applies to the estimate, and only to live runs: with
#' `.runner` the session runs whatever the estimate.
#'
#' When a model call fails partway through the session (a provider error that
#' persists after retries, an error from `.runner`, a reply still incomplete
#' after its retry, or a failed interim summary), the turns already completed
#' are not lost. The function raises an error of class
#' `focusgroup_incomplete_session` whose `partial` field holds the
#' `focus_group_result` built from those turns, with `metadata$complete` set to
#' `FALSE`. The error's `phase` field names the script phase that was running,
#' such as `"engagement_question"`, or `"interim_summary"`, and its `parent`
#' field holds the original error. Errors raised before the session starts,
#' such as an invalid argument or the `max_calls` check, carry no partial
#' result. A failed final summary is not an error of this kind: the function
#' warns and returns the complete transcript with an `NA` summary.
#'
#' Token usage that a provider did not report is unknown, not zero. A call's
#' total is its reported total, or the sum of its sent and received counts when
#' both are known. The `usage` component adds the reported counts of every
#' call FocusGroup makes, including retries of incomplete replies, desire
#' scores, and interim summaries, and its `unknown` element counts the calls
#' whose usage was incomplete. When `unknown` is positive the subtotals are
#' lower bounds, and when it is `NA`, as after a call that raised an error, the
#' number of such calls cannot be determined. Retries made inside
#' `LLMR::call_llm_robust()` are not visible here and are not counted. A
#' reported count that is not a nonnegative whole number is treated as
#' unknown, with a warning.
#'
#' In the transcript, a message's token count is `NA` unless every call behind
#' the message reported it. The message's `metadata` element `usage_unknown`
#' gives the number of those calls whose usage was incomplete.
#'
#' @return A `focus_group_result`, a list with these components:
#' \describe{
#'   \item{focus_group}{The `FocusGroup` object that ran the session. It keeps
#'     the model configuration, so model analyses can be run on it later.}
#'   \item{transcript}{A data frame with one row per logged message, starting
#'     with the `"System"` message that lists the roster. Its columns are
#'     `message_id`, `round`, `item` (the guide item the message belongs
#'     to, 0 for the roster row), `speaker_id` (the agent ID), `handle` (the
#'     label the agents saw), `is_moderator`, `text`, `phase`,
#'     `response_id`, `finish_reason`, `sent_tokens`, `rec_tokens`,
#'     `total_tokens`, `duration_s`, `provider`, `model`, `timestamp` (in UTC),
#'     and the list-column `metadata`, which marks, among others, delivered
#'     lines (`verbatim`), injected messages (`injected`, `injection`),
#'     call-on lines (`call_on`, a list of `point`, `participant`, and
#'     `handle`), the replies to them (`called_on`), replies to a moderator
#'     line that addressed the speaker (`addressed`), and replies whose
#'     self-label was removed (`self_label_removed`).}
#'   \item{summary}{The final summary. It is `NA` when the final summary call
#'     failed, and `character(0)` in a partial result that stopped before it
#'     or in a session run with `final_summary = FALSE`.}
#'   \item{participants}{A data frame with one row per agent: `id`, `handle`, `role`,
#'     `is_moderator`, `persona`, `demographics`, `survey_responses`,
#'     `provider`, and `model`. When the participants were drawn from the
#'     bundled ANES records, the rows drawn are recorded in
#'     `attr(result$focus_group$agents, "source_rows")`.}
#'   \item{usage}{A list of the token subtotals `sent`, `rec`, and `total`,
#'     and the count `unknown` described in Details. It records the session as
#'     run; model analyses run later add to the counters of `focus_group` but
#'     not to this list.}
#'   \item{metadata}{A list with `topic`, `purpose`, `flow`, `message_mode`,
#'     `n_participants`, `estimated_calls`, `provider`, `model`, `complete`
#'     (`TRUE` for a finished session, `FALSE` for a partial one), and
#'     `failure` (`NULL`, or a list with the `phase` and `message` of the
#'     error that stopped the session); then the conditions of the run:
#'     `max_participant_responses`, `seed` (`NA` when none was given),
#'     `on_desire_failure`, `reasoning_effort`, `max_tokens` (the token
#'     limit of each call type), `call_settings` (a data frame giving, for
#'     each call type and agent, the provider, model, and model parameters
#'     the calls start from), `prompt_templates`, `standing_rules` (the
#'     system-message rules for participants and the moderator),
#'     `disclosure`, `versions` (of FocusGroup, LLMR, and R), `started`, and
#'     `branch` (`NULL`, or for a session run by [run_focus_group_arms()] its
#'     arm and branch point), `handles` (a data frame of `agent_id` and
#'     `handle`), `call_on_silent` (a list of the `points` and the guide
#'     `items` they fall on, both empty when call-ons are off),
#'     `call_on_text`, and `final_summary`. It holds no configuration
#'     object.}
#'   \item{calls}{A data frame with one row per model call the session
#'     made, in order, including retries and failed calls: `call_id`,
#'     `call_type` (`"moderator"`, `"participant"`, `"desire"`, `"summary"`,
#'     `"interim_summary"`, or `"probe"`), `agent_id`, `attempt` (2 for a
#'     retry), `round`, `phase`, `message_id` (the message the call produced,
#'     if any), `provider`, `model`, the list-columns `settings` (the model
#'     parameters sent) and `messages` (the messages sent), `raw_text` (the
#'     reply before any cleaning), `thinking` (reasoning the provider returned
#'     separately), `cleaned_text`, `score` (for desire calls), `finish_reason`,
#'     `response_id`, `sent_tokens`, `rec_tokens`, `total_tokens`,
#'     `reasoning_tokens`, `duration_s`, `error` (the error a failed call
#'     raised), `timestamp`, and `arm` (the arm of a session continued by
#'     [run_focus_group_arms()], otherwise `NA`). Only scalar sampling and
#'     length parameters enter `settings`; keys and endpoint addresses never
#'     do.}
#'   \item{routing}{A data frame with one row per speaker selection: `pick`,
#'     `round`, `phase`, `turn` (the participant turn it chose for),
#'     `flow`, `rule` (such as `"rotation"`, `"propensity_draw"`,
#'     `"highest_score"`, `"threshold_lowered"`, `"highest_below_threshold"`,
#'     `"uniform_after_failure"`, or, for turns the flow did not choose,
#'     `"called_on"` and `"addressed"`), `selected_id`, `message_id`,
#'     `trigger_message_id` (for `"called_on"` and `"addressed"`, the
#'     moderator line that gave the turn; otherwise `NA`),
#'     `threshold`, `tie_break` (`"none"`, `"random"`, or `"draw"`),
#'     `scoring_mode`, and the list-columns `candidates`, `excluded` (named
#'     by participant, giving the reason, such as `"previous_speaker"` or
#'     `"score_failed"`), `scores` (desire scores, or selection
#'     probabilities for `"probabilistic"`), and `tied`, followed by `arm`.}
#'   \item{probes}{A data frame with one row per probe answer: `probe` (its
#'     label), `point`, `agent_id`, `question`, `answer` (the final answer,
#'     or `NA` when none came), `raw_text`, `finish_reason`, `call_id` (its
#'     row in `calls`), `round`, `phase`, `turn` (the next participant turn),
#'     `after_message_id` (the last message before it), and `arm`.}
#'   \item{injections}{A data frame with one row per injection: `label`,
#'     `arm`, `point`, `delivered` (`FALSE` for one whose point the session
#'     never reached, which is also reported with a warning), `message_id`,
#'     `speaker_id`, `from`, `visibility`, `text`, `cut_after_message_id`
#'     (the message it follows), `round`, `phase`, and `turn`.}
#' }
#' Results saved by FocusGroup 0.5.2 and earlier lack `usage$unknown`,
#' `metadata$complete`, and `metadata$failure`; results saved by 0.5.3 and
#' earlier lack `calls`, `routing`, `probes`, `injections`, and the metadata
#' that follows `failure`; results saved by 0.5.4 and earlier lack the
#' `item` and `handle` columns, `trigger_message_id`, and the metadata that
#' follows `branch`.
#'
#' @references Krueger, R. A. (2002). *Designing and Conducting Focus Group
#'   Interviews*. University of Minnesota.
#'
#' @examples
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                         api_key = LLMR::llm_api_key_env("GROQ_API_KEY"))
#' guide <- list(
#'   Opening = "Welcome the participants and state the ground rules.",
#'   Exploration = "Which funding priority deserves attention first?",
#'   Closing = "Thank the participants and close the session."
#' )
#'
#' scripted <- function(experiments, ...) {
#'   experiments$response_text <- "Evening opening hours matter most to me."
#'   experiments
#' }
#' result <- run_focus_group(
#'   topic = "Library funding priorities",
#'   config = cfg,
#'   n_participants = 2,
#'   guide = guide,
#'   flow = "round_robin",
#'   verbose = FALSE,
#'   .runner = scripted
#' )
#' result$transcript[, c("speaker_id", "phase", "text")]
#'
#' # The scripted runner reports no token counts, so usage is unknown.
#' result$usage
#'
#' # A session that fails partway returns what it completed.
#' failing <- local({
#'   calls <- 0
#'   function(experiments, ...) {
#'     calls <<- calls + 1
#'     if (calls == 3) stop("The provider is unavailable.")
#'     scripted(experiments)
#'   }
#' })
#' partial <- tryCatch(
#'   run_focus_group("Library funding priorities", cfg, n_participants = 2,
#'                   guide = guide, flow = "round_robin", verbose = FALSE,
#'                   .runner = failing),
#'   focusgroup_incomplete_session = function(e) e$partial
#' )
#' partial$metadata[c("complete", "failure")]
#' @export
run_focus_group <- function(topic, config, n_participants = 6,
                            guide = c(Opening = 1, Icebreaker = 1,
                                      Engagement = 2, Exploration = 3,
                                      Closing = 1),
                            demographics = NULL, survey_responses = NULL,
                            flow = "desire_based", seed = NULL,
                            message_mode = c("roleflip", "flat"), verbose = TRUE,
                            max_participant_responses = NULL, max_calls = 120L,
                            confirm = FALSE, max_tokens_config = NULL,
                            reasoning_effort = NULL,
                            on_desire_failure = c("error", "random"),
                            injections = NULL, probes = NULL,
                            moderator_verbatim = FALSE,
                            call_on_silent = c("opening", "midpoint", "end"),
                            call_on_text = NULL, handles = NULL,
                            final_summary = TRUE, .runner = NULL) {
  message_mode <- match.arg(message_mode)
  on_desire_failure <- match.arg(on_desire_failure)
  injections <- .fg_injection_list(injections)
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
    final_summary = final_summary, n_arms = 1L, .runner = .runner,
    seed_env = environment())

  result <- .fg_execute_session(setup$fg, flow, message_mode,
                                setup$n_participants, setup$estimated_calls,
                                verbose, run_meta = setup$run_meta,
                                injections = injections, probes = setup$probes,
                                final_summary = final_summary)

  if (verbose) cat("Simulation complete.\n")
  result
}

# Validate the arguments of a run, build its agents, flow, and FocusGroup,
# and check the estimate. The seed, when given, is set in `seed_env`, the
# calling function's frame, so that it governs the whole run and the
# caller's random-number state is restored when that function returns.
.fg_session_setup <- function(topic, config, n_participants, guide,
                              demographics, survey_responses, flow, seed,
                              message_mode, verbose, max_participant_responses,
                              max_calls, confirm, max_tokens_config,
                              reasoning_effort, on_desire_failure, probes,
                              moderator_verbatim, call_on_silent,
                              call_on_text, handles, final_summary, n_arms,
                              .runner, seed_env) {
  .fg_validate_config(config)
  reasoning_effort <- .fg_validate_reasoning_effort(reasoning_effort)
  probes <- .fg_probe_list(probes)
  .fg_check_flag(final_summary, "final_summary")
  call_on_text <- .fg_check_call_on_text(call_on_text)
  if (!is.null(max_tokens_config) && !is.list(max_tokens_config)) {
    stop("`max_tokens_config` must be `NULL` or a named list.", call. = FALSE)
  }

  if (!is.character(topic) || length(topic) != 1L || is.na(topic) ||
      !nzchar(trimws(topic))) {
    stop("`topic` must be a non-empty character string.", call. = FALSE)
  }
  if (!is.numeric(n_participants) || length(n_participants) != 1L ||
      is.na(n_participants) || !is.finite(n_participants) ||
      n_participants > .Machine$integer.max || n_participants < 1 ||
      n_participants != floor(n_participants)) {
    stop("`n_participants` must be one positive integer.", call. = FALSE)
  }
  n_participants <- as.integer(n_participants)
  flow_types <- c("round_robin", "probabilistic", "desire_based")
  if (!is.character(flow) || length(flow) != 1L || !flow %in% flow_types) {
    stop("`flow` must be one of: ", paste(flow_types, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.numeric(max_calls) || length(max_calls) != 1L || is.na(max_calls) ||
      !is.finite(max_calls) || max_calls > .Machine$integer.max ||
      max_calls < 1 || max_calls != floor(max_calls)) {
    stop("`max_calls` must be one finite positive integer.", call. = FALSE)
  }
  if (!is.logical(confirm) || length(confirm) != 1L || is.na(confirm)) {
    stop("`confirm` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.null(.runner) && !is.function(.runner)) {
    stop("`.runner` must be `NULL` or a function.", call. = FALSE)
  }

  response_limit <- max_participant_responses %||%
    getOption("focusgroup.max_participant_responses", 2L)
  if (!is.numeric(response_limit) || length(response_limit) != 1L ||
      is.na(response_limit) || !is.finite(response_limit) ||
      response_limit > .Machine$integer.max || response_limit < 1 ||
      response_limit != floor(response_limit)) {
    stop("`max_participant_responses` must be `NULL` or one positive integer.",
         call. = FALSE)
  }
  response_limit <- as.integer(response_limit)

  # Check the estimate from the phase counts first, so that an enormous
  # numeric count is refused before it is expanded into a script. Each
  # call-on point adds at most one reply per participant.
  if (!is.null(guide)) {
    counts <- .fg_guide_counts(guide)
    questions <- sum(counts[names(counts) %in% c("Icebreaker", "Engagement", "Exploration")])
    points_bound <- if (is.numeric(call_on_silent)) length(call_on_silent) else
      if (isFALSE(call_on_silent) || identical(call_on_silent, "none") ||
          is.null(call_on_silent)) 0 else length(.fg_call_on_names)
    .fg_validate_estimate(sum(counts) + questions * as.double(response_limit) *
      (if (identical(flow, "desire_based")) (1 + as.double(n_participants)) else 1) +
      points_bound * as.double(n_participants) + 1)
  }
  question_script <- .fg_build_question_script(guide, topic)
  estimate_script <- question_script
  if (!length(estimate_script)) {
    estimate_script <- list(
      list(phase = "opening"),
      list(phase = "generic_discussion", text = topic),
      list(phase = "closing")
    )
  }
  response_phases <- c(
    "icebreaker_question", "engagement_question", "exploration_question",
    "probing_focused", "ending_question", "generic_discussion"
  )
  verbatim <- .fg_verbatim_items(moderator_verbatim, length(estimate_script))
  if (length(verbatim)) {
    if (!length(question_script)) question_script <- estimate_script
    for (i in verbatim) question_script[[i]]$verbatim <- TRUE
  }
  phases <- vapply(estimate_script, `[[`, character(1), "phase")
  question_rounds <- sum(phases %in% response_phases)
  moderator_outputs <- length(estimate_script) - length(verbatim)
  participant_outputs <- as.double(question_rounds) * response_limit
  desire_outputs <- if (identical(flow, "desire_based")) {
    participant_outputs * n_participants
  } else {
    0L
  }
  probe_outputs <- sum(vapply(probes, function(p) {
    length(p$at) * as.double(length(p$participants %||% seq_len(n_participants)))
  }, numeric(1)))
  # The moderator's call-on lines are delivered as written; the replies to
  # them need no desire score.
  call_on <- .fg_call_on_points(call_on_silent, phases)
  call_on_outputs <- length(call_on$items) * as.double(n_participants)
  summary_outputs <- if (isTRUE(final_summary)) 1 else 0
  estimated_calls <- .fg_validate_estimate(
    (moderator_outputs + participant_outputs + desire_outputs + call_on_outputs +
       probe_outputs + summary_outputs) * as.double(n_arms)
  )
  if (is.null(.runner) && estimated_calls > max_calls && !confirm) {
    stop(
      "Estimated model outputs (", estimated_calls, ") exceed `max_calls` (",
      as.integer(max_calls), "). Increase `max_calls` or set `confirm = TRUE` ",
      "after reviewing the guide and flow.", call. = FALSE
    )
  }

  if (!is.null(seed)) withr::local_seed(as.integer(seed), .local_envir = seed_env)
  if (verbose) {
    cat("Setting up focus group simulation...\n")
    cat("Topic:", topic, "\n")
    cat("Participants:", n_participants, "\n")
    cat("Conversation flow:", flow, "\n")
    cat("Estimated model outputs:", estimated_calls, "\n")
  }

  agents <- create_agents(
    n_participants = n_participants,
    config = config,
    demographics = demographics,
    survey_responses = survey_responses,
    .runner = .runner
  )
  moderator_id <- names(agents)[vapply(
    agents, function(agent) isTRUE(agent$is_moderator), logical(1)
  )][1]
  if (is.na(moderator_id) || !nzchar(moderator_id)) {
    stop("No moderator found in the agent roster.", call. = FALSE)
  }

  purpose <- paste("To explore perspectives and experiences related to", topic)
  fg <- FocusGroup$new(
    topic = topic,
    purpose = purpose,
    agents = agents,
    moderator_id = moderator_id,
    turn_taking_flow = create_conversation_flow(
      flow, agents, moderator_id,
      flow_params = list(on_desire_failure = on_desire_failure)),
    question_script = question_script,
    admin_config = config,
    max_tokens_config = max_tokens_config %||% list(),
    max_participant_responses = response_limit,
    reasoning_effort = reasoning_effort,
    handles = handles,
    call_on_silent = call_on_silent,
    call_on_text = call_on_text
  )

  list(fg = fg, flow = flow, message_mode = message_mode,
       n_participants = n_participants, estimated_calls = estimated_calls,
       probes = probes,
       run_meta = list(seed = seed, on_desire_failure = on_desire_failure))
}

# The guide items to deliver verbatim: none for FALSE, all for TRUE, or the
# item numbers given.
.fg_verbatim_items <- function(moderator_verbatim, n_items) {
  if (isFALSE(moderator_verbatim)) return(integer())
  if (isTRUE(moderator_verbatim)) return(seq_len(n_items))
  v <- moderator_verbatim
  if (!is.numeric(v) || !length(v) || anyNA(v) || any(!is.finite(v)) ||
      any(v != floor(v)) || any(v < 1) || any(v > n_items) || anyDuplicated(v)) {
    stop("`moderator_verbatim` must be TRUE, FALSE, or distinct guide item ",
         "numbers from 1 to ", n_items, ".", call. = FALSE)
  }
  sort(as.integer(v))
}

# The model-output estimate must be a whole number that fits in an integer.
.fg_validate_estimate <- function(value) {
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value < 0 || value != floor(value)) {
    stop("The model-output estimate is unavailable: ", format(value), ".",
         call. = FALSE)
  }
  if (value > .Machine$integer.max) {
    stop("The model-output estimate (", format(value, big.mark = ",", scientific = FALSE),
         ") exceeds the integer range; reduce the guide counts, ",
         "`n_participants`, or `max_participant_responses`.", call. = FALSE)
  }
  as.integer(value)
}

.fg_execute_session <- function(fg, flow, message_mode, n_participants,
                                estimated_calls, verbose = FALSE,
                                run_meta = list(), injections = NULL,
                                probes = NULL, final_summary = TRUE) {
  result <- function() .fg_focus_group_result(
    fg, flow, message_mode, n_participants, estimated_calls, run_meta)
  tryCatch(
    withr::with_options(list(focusgroup.message_mode = message_mode),
                       fg$run_simulation(verbose = verbose,
                                         injections = injections,
                                         probes = probes,
                                         final_summary = final_summary)),
    error = function(e) {
      # An error raised before the session began, such as an injection
      # point outside the guide, carries no partial result.
      if (!length(fg$conversation_log)) stop(e)
      phase <- attr(fg, "execution_phase") %||% "setup"
      attr(fg, "session_status") <- list(complete = FALSE,
        failure = list(phase = phase, message = conditionMessage(e)))
      rlang::abort(sprintf("The focus group stopped during phase '%s'.", phase),
                   class = "focusgroup_incomplete_session", partial = result(),
                   phase = phase, parent = e)
    })
  attr(fg, "session_status") <- list(complete = TRUE, failure = NULL)
  result()
}

.fg_validate_config <- function(config) {
  if (missing(config) || is.null(config) || !inherits(config, "llm_config")) {
    stop("`config` must be an explicit `llm_config` object.", call. = FALSE)
  }
  invisible(config)
}

.fg_transcript_data <- function(log) {
  out <- data.frame(
    message_id = integer(), round = integer(), item = integer(),
    speaker_id = character(), handle = character(),
    is_moderator = logical(), text = character(), phase = character(),
    response_id = character(), finish_reason = character(),
    sent_tokens = integer(), rec_tokens = integer(), total_tokens = integer(),
    duration_s = double(), provider = character(), model = character(),
    timestamp = as.POSIXct(character()), stringsAsFactors = FALSE
  )
  out$metadata <- I(list())
  if (!length(log)) return(out)

  scalar <- function(x, default) {
    value <- x %||% default
    if (!length(value) || is.na(value[[1]])) default else value[[1]]
  }
  token_column <- function(name) {
    values <- vapply(log, function(x) as.double(scalar(x[[name]], NA_real_)), numeric(1))
    if (all(is.na(values) | values <= .Machine$integer.max)) as.integer(values) else values
  }
  out <- data.frame(
    message_id = vapply(log, function(x) as.integer(scalar(x$message_id, NA_integer_)), integer(1)),
    round = vapply(log, function(x) as.integer(scalar(x$round, NA_integer_)), integer(1)),
    item = vapply(log, function(x) {
      as.integer(scalar(x$item, scalar(x$round, NA_integer_)))
    }, integer(1)),
    speaker_id = vapply(log, function(x) as.character(scalar(x$speaker_id, "")), character(1)),
    handle = vapply(log, function(x) {
      as.character(scalar(x$handle, scalar(x$speaker_id, "")))
    }, character(1)),
    is_moderator = vapply(log, function(x) isTRUE(x$is_moderator), logical(1)),
    text = vapply(log, function(x) as.character(scalar(x$text, "")), character(1)),
    phase = vapply(log, function(x) as.character(x$phase %||% "unknown"), character(1)),
    response_id = vapply(log, function(x) as.character(scalar(x$response_id, NA_character_)), character(1)),
    finish_reason = vapply(log, function(x) as.character(scalar(x$finish_reason, NA_character_)), character(1)),
    sent_tokens = token_column("sent_tokens"),
    rec_tokens = token_column("rec_tokens"),
    total_tokens = token_column("total_tokens"),
    duration_s = vapply(log, function(x) as.numeric(scalar(x$duration_s, NA_real_)), numeric(1)),
    provider = vapply(log, function(x) as.character(scalar(x$provider, NA_character_)), character(1)),
    model = vapply(log, function(x) as.character(scalar(x$model, NA_character_)), character(1)),
    timestamp = as.POSIXct(vapply(
      log, function(x) as.numeric(scalar(x$timestamp, as.POSIXct(NA))), numeric(1)
    ), origin = "1970-01-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  out$metadata <- I(lapply(log, function(x) x$metadata %||% list()))
  out
}

.fg_participant_data <- function(agents) {
  out <- data.frame(
    id = vapply(agents, function(agent) agent$id, character(1)),
    handle = vapply(agents, .fg_agent_label, character(1)),
    role = vapply(agents, function(agent) agent$role %||%
                    if (isTRUE(agent$is_moderator)) "moderator" else "participant",
                  character(1)),
    is_moderator = vapply(agents, function(agent) isTRUE(agent$is_moderator), logical(1)),
    persona = vapply(agents, function(agent) agent$persona_description, character(1)),
    stringsAsFactors = FALSE
  )
  out$demographics <- I(lapply(agents, function(agent) agent$demographics %||% list()))
  out$survey_responses <- I(lapply(agents, function(agent) agent$survey_responses %||% list()))
  out$provider <- vapply(agents, function(agent)
    agent$config$provider %||% NA_character_, character(1))
  out$model <- vapply(agents, function(agent)
    agent$config$model %||% NA_character_, character(1))
  out[c("id", "handle", "role", "is_moderator", "persona", "demographics",
        "survey_responses", "provider", "model")]
}

.fg_focus_group_result <- function(focus_group, flow, message_mode,
                                   n_participants, estimated_calls,
                                   run_meta = list()) {
  transcript <- .fg_transcript_data(focus_group$conversation_log)
  participant_data <- .fg_participant_data(focus_group$agents)
  records <- focus_group$records %||% .fg_empty_records()
  sent <- focus_group$total_tokens_sent %||% NA_real_
  rec <- focus_group$total_tokens_received %||% NA_real_
  status <- attr(focus_group, "session_status")
  moderator_config <- focus_group$agents[[focus_group$moderator_id]]$config
  structure(
    list(
      focus_group = focus_group,
      transcript = transcript,
      summary = focus_group$final_summary %||% character(),
      participants = participant_data,
      usage = list(sent = sent, rec = rec,
                   total = focus_group$total_tokens %||% NA_real_,
                   unknown = focus_group$calls_usage_unknown %||% NA_real_),
      metadata = list(
        topic = focus_group$topic,
        purpose = focus_group$purpose,
        flow = flow,
        message_mode = message_mode,
        n_participants = as.integer(n_participants),
        estimated_calls = as.integer(estimated_calls),
        provider = moderator_config$provider %||% NA_character_,
        model = moderator_config$model %||% NA_character_,
        complete = status$complete %||% NA,
        failure = status$failure,
        max_participant_responses = as.integer(
          focus_group$max_participant_responses %||% NA_integer_),
        seed = if (is.null(run_meta$seed)) NA_integer_ else as.integer(run_meta$seed),
        on_desire_failure = run_meta$on_desire_failure %||% NA_character_,
        reasoning_effort = focus_group$reasoning_effort,
        max_tokens = .fg_token_limits(focus_group),
        call_settings = .fg_settings_data(focus_group),
        prompt_templates = focus_group$prompt_templates,
        standing_rules = records$info$standing_rules,
        disclosure = records$info$disclosure %||% NA_character_,
        versions = records$info$versions %||% .fg_versions(),
        started = records$info$started %||% as.POSIXct(NA),
        branch = run_meta$branch,
        handles = .fg_handles_data(focus_group),
        call_on_silent = list(
          points = as.character(focus_group$call_on_silent$points %||% character()),
          items = as.integer(focus_group$call_on_silent$items %||% integer())),
        call_on_text = focus_group$call_on_text %||% NA_character_,
        final_summary = !isFALSE(records$info$final_summary)
      ),
      calls = .fg_calls_data(records$calls),
      routing = .fg_routing_data(records$routing),
      probes = .fg_probes_data(records$probes),
      injections = .fg_injections_data(records$injections)
    ),
    class = c("focus_group_result", "list")
  )
}

# The handle of every agent in roster order: the label the agents saw.
.fg_handles_data <- function(focus_group) {
  agents <- focus_group$agents
  data.frame(
    agent_id = vapply(agents, function(a) as.character(a$id), character(1), USE.NAMES = FALSE),
    handle = vapply(agents, .fg_agent_label, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE)
}

# A session's token limits by call type.
.fg_token_limits <- function(focus_group) {
  limit <- function(name, default) as.numeric(focus_group[[name]] %||% default)
  list(utterance = limit("max_tokens_utterance", NA_real_),
       moderator = limit("max_tokens_moderator", NA_real_),
       desire = limit("max_tokens_desire", NA_real_),
       desire_retry = limit("max_tokens_desire_retry", .fg_desire_retry_tokens),
       summary = limit("max_tokens_summary", 400),
       interim_summary = limit("max_tokens_interim_summary", 250),
       probe = limit("max_tokens_probe", NA_real_))
}

# The settings each call type starts from, by agent: the allowlisted model
# parameters after the session's limits and reasoning effort are applied. A
# participant's reply is at least 220 tokens, and the moderator's opening and
# closing use 300 and 250, when the configuration sets no limit. The
# settings of each call as made are in the `calls` record.
.fg_settings_data <- function(focus_group) {
  rows <- list()
  add <- function(type, agent_id, config, max_tokens = NULL, temperature = NULL) {
    if (is.null(config) || !inherits(config, "llm_config")) return(invisible())
    if (!is.null(max_tokens)) config$model_params$max_tokens <- max_tokens
    if (!is.null(temperature)) {
      config$model_params$temperature <- config$model_params$temperature %||% temperature
    }
    config <- .fg_with_params(config, .fg_call_params(focus_group, type))
    rows[[length(rows) + 1L]] <<- list(call_type = type, agent_id = agent_id,
      provider = config$provider %||% NA_character_,
      model = config$model %||% NA_character_,
      settings = .fg_call_settings(config))
  }
  limits <- .fg_token_limits(focus_group)
  moderator_id <- focus_group$moderator_id %||% NA_character_
  for (id in names(focus_group$agents)) {
    config <- focus_group$agents[[id]]$config
    configured <- config$model_params$max_tokens
    if (identical(id, moderator_id)) {
      add("moderator", id, config, configured %||% limits$moderator, 0.7)
    } else {
      add("participant", id, config,
          configured %||% max(limits$utterance, 220, na.rm = TRUE), 0.7)
      add("desire", id, config, limits$desire)
      if (!is.na(limits$probe)) add("probe", id, config, limits$probe)
    }
  }
  add("summary", NA_character_, focus_group$admin_config, limits$summary)
  add("interim_summary", NA_character_, focus_group$admin_config, limits$interim_summary)
  out <- data.frame(
    call_type = vapply(rows, `[[`, character(1), "call_type"),
    agent_id = vapply(rows, function(r) as.character(r$agent_id), character(1)),
    provider = vapply(rows, function(r) as.character(r$provider), character(1)),
    model = vapply(rows, function(r) as.character(r$model), character(1)),
    stringsAsFactors = FALSE)
  out$settings <- I(lapply(rows, `[[`, "settings"))
  out
}

# One column of a list of records: a scalar per record, with NA where a
# record lacks the field.
.fg_record_column <- function(records, name, type) {
  convert <- switch(type, character = as.character, integer = as.integer,
                    double = as.double, logical = as.logical)
  na <- convert(NA)
  vapply(records, function(x) {
    value <- x[[name]]
    if (is.null(value) || !length(value) || (is.atomic(value) && is.na(value[[1]]))) na
    else convert(value[[1]])
  }, na)
}

# The `calls` component of a result: one row per model call.
.fg_calls_data <- function(calls) {
  col <- function(name, type) .fg_record_column(calls, name, type)
  count <- function(name) {
    values <- col(name, "double")
    if (all(is.na(values) | values <= .Machine$integer.max)) as.integer(values) else values
  }
  out <- data.frame(
    call_id = col("call_id", "integer"), call_type = col("call_type", "character"),
    agent_id = col("agent_id", "character"), attempt = col("attempt", "integer"),
    round = col("round", "integer"), phase = col("phase", "character"),
    message_id = col("message_id", "integer"),
    provider = col("provider", "character"), model = col("model", "character"),
    stringsAsFactors = FALSE)
  out$settings <- I(lapply(calls, function(x) x$settings %||% list()))
  out$messages <- I(lapply(calls, function(x) x$messages %||% list()))
  out$raw_text <- col("raw_text", "character")
  out$thinking <- col("thinking", "character")
  out$cleaned_text <- col("cleaned_text", "character")
  out$score <- col("score", "double")
  out$finish_reason <- col("finish_reason", "character")
  out$response_id <- col("response_id", "character")
  out$sent_tokens <- count("sent_tokens")
  out$rec_tokens <- count("rec_tokens")
  out$total_tokens <- count("total_tokens")
  out$reasoning_tokens <- count("reasoning_tokens")
  out$duration_s <- col("duration_s", "double")
  out$error <- col("error", "character")
  out$timestamp <- as.POSIXct(col("timestamp", "double"), origin = "1970-01-01", tz = "UTC")
  out$arm <- col("arm", "character")
  out
}

# The `probes` component of a result: one row per probe answer.
.fg_probes_data <- function(probes) {
  col <- function(name, type) .fg_record_column(probes, name, type)
  data.frame(
    probe = col("probe", "character"), point = col("point", "character"),
    agent_id = col("agent_id", "character"), question = col("question", "character"),
    answer = col("answer", "character"), raw_text = col("raw_text", "character"),
    finish_reason = col("finish_reason", "character"),
    call_id = col("call_id", "integer"), round = col("round", "integer"),
    phase = col("phase", "character"), turn = col("turn", "integer"),
    after_message_id = col("after_message_id", "integer"),
    arm = col("arm", "character"), stringsAsFactors = FALSE)
}

# The `injections` component of a result: one row per planned injection.
.fg_injections_data <- function(injections) {
  col <- function(name, type) .fg_record_column(injections, name, type)
  data.frame(
    label = col("label", "character"), arm = col("arm", "character"),
    point = col("point", "character"), delivered = col("delivered", "logical"),
    message_id = col("message_id", "integer"),
    speaker_id = col("speaker_id", "character"), from = col("from", "character"),
    visibility = col("visibility", "character"), text = col("text", "character"),
    cut_after_message_id = col("cut_after_message_id", "integer"),
    round = col("round", "integer"), phase = col("phase", "character"),
    turn = col("turn", "integer"), stringsAsFactors = FALSE)
}

# The `routing` component of a result: one row per speaker selection.
.fg_routing_data <- function(routing) {
  col <- function(name, type) .fg_record_column(routing, name, type)
  out <- data.frame(
    pick = col("pick", "integer"), round = col("round", "integer"),
    phase = col("phase", "character"), turn = col("turn", "integer"),
    flow = col("flow", "character"), rule = col("rule", "character"),
    selected_id = col("selected_id", "character"),
    message_id = col("message_id", "integer"),
    trigger_message_id = col("trigger_message_id", "integer"),
    threshold = col("threshold", "double"),
    tie_break = col("tie_break", "character"),
    scoring_mode = col("scoring_mode", "character"),
    stringsAsFactors = FALSE)
  out$candidates <- I(lapply(routing, function(x) x$candidates %||% character()))
  out$excluded <- I(lapply(routing, function(x) x$excluded %||% character()))
  out$scores <- I(lapply(routing, function(x) x$scores))
  out$tied <- I(lapply(routing, function(x) x$tied %||% character()))
  out$arm <- col("arm", "character")
  out
}

#' Print a focus group result
#'
#' Prints the topic, the participant and message counts, and the total token
#' count. When some calls did not report their usage, the count is shown as a
#' lower bound ("at least ..."), or as "unknown" when no call reported it, and
#' a further line says how many calls are affected.
#' @param x A `focus_group_result`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.focus_group_result <- function(x, ...) {
  cat("<focus_group_result>\n")
  cat("Topic: ", x$metadata$topic, "\n", sep = "")
  cat("Participants: ", x$metadata$n_participants,
      " | Messages: ", nrow(x$transcript),
      " | Tokens: ", .fg_usage_text(x$usage$total, x$usage$unknown), "\n", sep = "")
  cat(.fg_usage_note(x$usage$unknown))
  invisible(x)
}

#' Create Focus Group Agents
#'
#' Creates participant agents from supplied records, direct personas, or
#' the bundled ANES 2024 records, and adds a moderator.
#'
#' @param n_participants Integer number of participants to create
#' @param config An explicit `LLMR::llm_config` for all agents.
#' @param demographics Optional data frame, one row per participant. When it,
#'   `survey_responses`, and `direct_persona_descriptions` are all NULL,
#'   participants are drawn from the bundled ANES 2024 records.
#' @param survey_responses Optional data frame of survey answers, one row per
#'   participant.
#' @param direct_persona_descriptions Optional character vector of pre-rendered
#'   participant personas. When supplied, these descriptions are used directly
#'   and are recycled in order if necessary. Demographics and survey responses
#'   remain attached as raw reporting fields.
#' @param .runner Optional function used instead of live model calls and stored
#'   on every agent.
#'
#' @return A named list of FGAgent objects (participants + 1 moderator), keyed
#'   by agent ID. When the participants are drawn from the bundled ANES
#'   records, the list carries the `source_rows` attribute described in
#'   [create_agents_from_survey()], giving the rows of
#'   `LLMR::anes_2024_personas` that were drawn. Unless the personas are
#'   given in `direct_persona_descriptions`, the list also carries a
#'   `rendering_report` attribute recording each field supplied for each
#'   participant and whether the persona shows it; see
#'   [persona_rendering_report()].
#'
#' @examples
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                         api_key = LLMR::llm_api_key_env("GROQ_API_KEY"))
#' agents <- create_agents(6, config = cfg)
#' names(agents)
#' agents$P1$persona_description
#'
#' demo_data <- data.frame(
#'   age = c(22, 35, 28, 41, 19, 33),
#'   gender = c("Female", "Male", "Male", "Female", "Male", "Female"),
#'   education = c("Bachelor's", "Master's", "High School", "PhD",
#'                 "Some College", "Bachelor's")
#' )
#' agents <- create_agents(6, config = cfg, demographics = demo_data)
#' agents$P2$persona_description
#'
#' @export
create_agents <- function(n_participants, config, demographics = NULL,
                          survey_responses = NULL,
                          direct_persona_descriptions = NULL, .runner = NULL) {

  .fg_validate_config(config)

  if (!is.numeric(n_participants) || length(n_participants) != 1L ||
      is.na(n_participants) || !is.finite(n_participants) ||
      n_participants > .Machine$integer.max || n_participants < 1 ||
      n_participants != floor(n_participants)) {
    stop("`n_participants` must be a single positive integer.", call. = FALSE)
  }
  n_participants <- as.integer(n_participants)

  # The easy route. With nothing supplied, participants are drawn from the
  # bundled ANES 2024 records rather than invented: real respondents, their
  # demographic profile rendered with the survey's own question wording. The
  # 100 rows are diversity-selected, not a representative sample. For the
  # respondents' stated answers as well, use create_agents_from_data() on the
  # full frame; for other populations, create_agents_from_survey(). The roster
  # records which rows were drawn.
  if (is.null(demographics) && is.null(survey_responses) &&
      is.null(direct_persona_descriptions)) {
    personas <- LLMR::anes_2024_personas
    if (n_participants > nrow(personas)) {
      stop(sprintf(paste(
        "%d participants requested but the bundled ANES example holds %d",
        "respondents. Supply your own `demographics`, a survey file",
        "(create_agents_from_survey()), or a larger frame",
        "(create_agents_from_data())."),
        n_participants, nrow(personas)), call. = FALSE)
    }
    fields <- intersect(LLMR::llm_persona_demographic_fields(personas),
                        names(personas))
    lean <- personas[, fields, drop = FALSE]
    attr(lean, "dictionary") <- attr(personas, "dictionary")
    attr(lean, "demographic_fields") <- fields
    agents <- create_agents_from_data(
      lean, n_participants = n_participants, config = config, .runner = .runner)
    return(.fg_roster_provenance(agents, personas, attr(agents, "source_rows")))
  }

  if (!is.null(direct_persona_descriptions)) {
    if (!is.character(direct_persona_descriptions) ||
        !length(direct_persona_descriptions) ||
        anyNA(direct_persona_descriptions) ||
        any(!nzchar(trimws(direct_persona_descriptions)))) {
      stop("`direct_persona_descriptions` must contain non-empty character strings.",
           call. = FALSE)
    }
  }

  # A frame with fewer rows than participants would have to be recycled, and
  # recycled rows are one research unit masquerading as several
  # (pseudoreplication). Refuse; the caller can sample explicitly.
  if (is.data.frame(demographics) && nrow(demographics) > 0 &&
      nrow(demographics) < n_participants) {
    stop(sprintf(paste(
      "`demographics` has %d rows for %d participants. Recycling rows would",
      "duplicate research units; supply at least `n_participants` rows or",
      "sample them yourself."),
      nrow(demographics), n_participants), call. = FALSE)
  }
  if (is.data.frame(survey_responses) && nrow(survey_responses) > 0 &&
      nrow(survey_responses) < n_participants) {
    stop(sprintf(paste(
      "`survey_responses` has %d rows for %d participants. Recycling rows",
      "would duplicate research units; supply at least `n_participants` rows",
      "or sample them yourself."),
      nrow(survey_responses), n_participants), call. = FALSE)
  }

  agents <- list()
  report <- list()

  # Create participants

  for (i in seq_len(n_participants)) {
    agent_demographics <- if (!is.null(demographics) && is.data.frame(demographics) && nrow(demographics) > 0) {
      .fg_row_fields(demographics, ((i - 1L) %% nrow(demographics)) + 1L)
    } else if (!is.null(demographics) && !is.data.frame(demographics) && is.list(demographics) && length(demographics) >= i) {
      demographics[[i]]
    } else {
      # No demographics were supplied (direct personas, or a short list):
      # attach nothing rather than inventing a record.
      list()
    }

    agent_survey <- if (!is.null(survey_responses) && is.data.frame(survey_responses) && nrow(survey_responses) > 0) {
      .fg_row_fields(survey_responses, ((i - 1L) %% nrow(survey_responses)) + 1L)
    } else if (!is.null(survey_responses) && !is.data.frame(survey_responses) && is.list(survey_responses) && length(survey_responses) >= i) {
      survey_responses[[i]]
    } else {
      NULL
    }

    # Identifier columns stay with the raw record but never enter the
    # persona, which the agents read.
    shown_demographics <- agent_demographics[
      !names(agent_demographics) %in% .fg_identifier_columns]
    shown_survey <- if (!is.null(agent_survey)) {
      agent_survey[!names(agent_survey) %in% .fg_identifier_columns]
    }
    persona <- if (!is.null(direct_persona_descriptions)) {
      direct_persona_descriptions[
        ((i - 1L) %% length(direct_persona_descriptions)) + 1L
      ]
    } else {
      generate_persona(shown_demographics, shown_survey)
    }
    if (is.null(direct_persona_descriptions)) {
      report[[i]] <- rbind(
        .fg_supplied_rows(paste0("P", i), "demographic",
                          .fg_supplied_record(demographics, i), shown_demographics),
        .fg_supplied_rows(paste0("P", i), "item",
                          .fg_supplied_record(survey_responses, i), shown_survey))
    }

    agents[[paste0("P", i)]] <- FGAgent$new(
      id = paste0("P", i),
      agent_details = list(
        direct_persona_description = persona,
        demographics = agent_demographics,
        survey_responses = agent_survey
      ),
      config = config,
      is_moderator = FALSE,
      .runner = .runner
    )
  }

  # Create moderator
  moderator_persona <- "You are an experienced focus group moderator. You facilitate discussion, ask follow-up questions, ensure all participants have opportunities to speak, and guide the conversation through different phases. You remain neutral and encouraging."

  agents[["MOD"]] <- FGAgent$new(
    id = "MOD",
    agent_details = list(
      direct_persona_description = moderator_persona,
      demographics = list(role = "professional_moderator")
    ),
    config = config,
    is_moderator = TRUE,
    .runner = .runner
  )

  if (is.null(direct_persona_descriptions)) {
    agents <- .fg_attach_report(
      agents, .fg_check_report(.fg_mark_identifiers(.fg_bind_report(report)), agents))
  }
  return(agents)
}

#' Analyze Focus Group Results
#'
#' Runs descriptive analyses offline. Thematic analysis and a model-generated
#' summary are opt-in and run only when `config` is supplied.
#'
#' @param focus_group_result A `FocusGroup` object or `focus_group_result`.
#' @param num_topics Integer number of topics for topic modeling.
#' @param speakers Whose turns the text statistics describe:
#'   `"participants"` (default) excludes the moderator's scripted turns;
#'   `"all"` keeps them. The transcript itself always retains every turn.
#' @param language Language of the transcript (default `"en"`). The
#'   TF-IDF stop-word list and the readability formula are English-specific,
#'   so those components are skipped, with a note in `issues`, for any other
#'   value.
#' @param include_plots Logical. Attempt the four descriptive plots.
#' @param config Optional explicit `LLMR::llm_config`. When supplied, thematic
#'   analysis and a model summary are generated.
#' @param .runner Optional function used instead of live model calls for the two
#'   model analyses.
#'
#' @details
#' The two model analyses add their usage to the token fields of the
#' `FocusGroup` object, including calls made before one of them fails. The
#' `usage` component of a `focus_group_result` passed in is not changed: it
#' records the session as run. An error from a model analysis is not caught.
#'
#' @return A `focus_group_analysis` containing the returned analysis
#'   components, `basic_stats`, `topics`, `tfidf`, `readability`, `themes`,
#'   `model_summary`, and `plots`, and an `issues` table with columns
#'   `component` and `reason` for the components that were skipped or failed.
#'
#' @examples
#' transcript <- data.frame(
#'   speaker = c("Moderator", "P1", "P2"),
#'   text = c("What matters most?", "Cost matters.", "Access matters.")
#' )
#' fg <- focus_group_from_transcript(transcript)
#' analysis <- analyze_focus_group(fg, include_plots = FALSE)
#' analysis$basic_stats$speaker_stats
#' @export
analyze_focus_group <- function(focus_group_result,
                                num_topics = 5,
                                include_plots = TRUE,
                                speakers = c("participants", "all"),
                                language = "en",
                                config = NULL,
                                .runner = NULL) {
  speakers <- match.arg(speakers)
  stopifnot(is.character(language), length(language) == 1L)
  if (inherits(focus_group_result, "FocusGroup")) {
    fg <- focus_group_result
  } else if (inherits(focus_group_result, "focus_group_result")) {
    fg <- focus_group_result$focus_group
  } else {
    stop("`focus_group_result` must be a FocusGroup or focus_group_result.",
         call. = FALSE)
  }
  if (!inherits(fg, "FocusGroup")) {
    stop("`focus_group_result$focus_group` must be a FocusGroup object.",
         call. = FALSE)
  }
  if (!length(fg$conversation_log)) {
    stop("No conversation data found.", call. = FALSE)
  }
  # Text statistics describe the discussion, so the moderator's scripted
  # turns are excluded unless asked for; the transcript itself keeps them.
  excluded_speakers <- character()
  if (identical(speakers, "participants")) {
    mod_ids <- names(fg$agents)[vapply(
      fg$agents, function(a) isTRUE(a$is_moderator), logical(1))]
    # "System" is the simulator's bookkeeping label, not a discussant.
    excluded_speakers <- unique(c(mod_ids, "System"))
    kept <- Filter(function(m) !(m$speaker_id %in% excluded_speakers),
                   fg$conversation_log)
    if (length(kept) && length(kept) < length(fg$conversation_log)) {
      if (is.environment(fg)) {
        original_log <- fg$conversation_log
        fg$conversation_log <- kept
        on.exit(fg$conversation_log <- original_log, add = TRUE)
      } else {
        fg$conversation_log <- kept
      }
    }
  }
  if (!is.numeric(num_topics) || length(num_topics) != 1L ||
      is.na(num_topics) || num_topics < 1 || num_topics != floor(num_topics)) {
    stop("`num_topics` must be one positive integer.", call. = FALSE)
  }
  if (!is.logical(include_plots) || length(include_plots) != 1L ||
      is.na(include_plots)) {
    stop("`include_plots` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.null(config)) .fg_validate_config(config)
  if (!is.null(.runner) && !is.function(.runner)) {
    stop("`.runner` must be `NULL` or a function.", call. = FALSE)
  }

  issues <- dplyr::tibble(component = character(), reason = character())
  add_issue <- function(component, reason) {
    issues <<- dplyr::bind_rows(
      issues,
      dplyr::tibble(component = as.character(component),
                    reason = as.character(reason))
    )
  }
  capture_component <- function(component, code, empty) {
    warnings <- character()
    failure <- NULL
    value <- tryCatch(
      withCallingHandlers(
        code(),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        failure <<- conditionMessage(e)
        NULL
      }
    )
    if (length(warnings)) {
      for (reason in unique(warnings)) add_issue(component, reason)
    }
    if (!is.null(failure)) {
      add_issue(component, failure)
      return(empty)
    }
    if (is.null(value)) {
      if (!length(warnings)) add_issue(component, "Analysis returned no result.")
      return(empty)
    }
    value
  }

  empty_basic <- list(
    speaker_stats = dplyr::tibble(
      speaker_id = character(), utterance_count = integer(),
      total_words = integer(), avg_words_per_utterance = double()
    ),
    full_transcript = character()
  )
  basic_stats <- capture_component("basic_stats", function() fg$analyze(),
                                   empty_basic)
  # fg$analyze() completes its table from the agent roster, so a moderator who
  # said nothing in the filtered log would still appear as a zero row.
  if (length(excluded_speakers) && is.data.frame(basic_stats$speaker_stats)) {
    keep_rows <- !(basic_stats$speaker_stats$speaker_id %in% excluded_speakers)
    basic_stats$speaker_stats <- basic_stats$speaker_stats[keep_rows, ,
                                                           drop = FALSE]
  }
  topics <- capture_component(
    "topics", function() fg$analyze_topics(num_topics = as.integer(num_topics)),
    list()
  )
  empty_tfidf <- dplyr::tibble(
    speaker_id = character(), term = character(), tf_idf = double()
  )
  english_only <- identical(tolower(language), "en")
  tfidf <- if (english_only) {
    capture_component("tfidf", function() fg$analyze_tfidf(), empty_tfidf)
  } else {
    add_issue("tfidf",
              "Skipped: the stop-word list is English; set language = 'en' to run it.")
    empty_tfidf
  }
  if (!is.data.frame(tfidf) || !nrow(tfidf)) tfidf <- empty_tfidf

  empty_readability <- dplyr::tibble(speaker_id = character())
  readability <- if (english_only) {
    capture_component("readability", function() fg$analyze_readability(),
                      empty_readability)
  } else {
    add_issue("readability",
              "Skipped: the readability formula is English; set language = 'en' to run it.")
    empty_readability
  }
  if (!is.data.frame(readability) || !nrow(readability)) {
    readability <- empty_readability
  }

  themes <- character()
  model_summary <- character()
  if (!is.null(config)) {
    themes <- .fg_legacy_usage(fg, fg$analyze_themes(config = config, .runner = .runner))
    model_summary <- .fg_legacy_usage(fg, fg$summarize(config = config, .runner = .runner))
  }

  plots <- list()
  if (include_plots) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      plot_calls <- list(
        participation_timeline = function() fg$plot_participation_timeline(),
        word_count_distribution = function() fg$plot_word_count_distribution(),
        participation_by_agent = function() fg$plot_participation_by_agent(),
        message_length_timeline = function() fg$plot_message_length_timeline()
      )
      for (component in names(plot_calls)) {
        value <- capture_component(
          paste0("plots.", component), plot_calls[[component]], NULL
        )
        if (!is.null(value)) plots[[component]] <- value
      }
    } else {
      add_issue("plots", "Package 'ggplot2' is not installed.")
    }
  }

  structure(
    list(
      basic_stats = basic_stats,
      topics = topics,
      tfidf = tfidf,
      readability = readability,
      themes = themes,
      model_summary = model_summary,
      plots = plots,
      issues = issues
    ),
    class = c("focus_group_analysis", "list")
  )
}

#' @export
print.focus_group_analysis <- function(x, ...) {
  cat("<focus_group_analysis>\n")
  n_topics <- if (!is.null(x$topics$num_topics)) x$topics$num_topics
              else if (!is.null(x$topics$topic_terms)) length(x$topics$topic_terms)
              else 0L
  cat("Speakers: ", nrow(x$basic_stats$speaker_stats),
      " | Topics: ", n_topics,
      " | Issues: ", nrow(x$issues), "\n", sep = "")
  cat("Model analysis: ",
      if (length(x$themes) || length(x$model_summary)) "included" else "not requested",
      "\n", sep = "")
  invisible(x)
}

#' Generate a participant persona from demographics and survey responses
#'
#' Renders the persona text for a simulated participant from the demographics
#' and survey responses the researcher supplied. It states those facts and
#' nothing more: what a given age, education, place of residence, or survey
#' answer implies about a person is left to the model, not decided here.
#' Mapping a demographic label to a fixed disposition is the essentialism a
#' research instrument should avoid, so the package does not do it.
#'
#' Both the demographics and the survey responses are rendered in full (every
#' supplied field), not a fixed subset; fields that are missing or empty are
#' left out rather than rendered as "NA". Pass survey responses keyed by the
#' question wording (for survey-file input, `question_wording`, the names of
#' `survey_vars`, or the variable label is used as the key) so the model sees
#' the item, not a code name.
#'
#' @param demographics A named list (or single-row data frame coerced to one) of
#'   demographic fields, e.g. `list(age = 63, education = "High school",
#'   region = "South")`. Field names are shown to the model as written.
#' @param survey_responses Optional named list of survey responses, keyed by the
#'   question text: `list("Party identification" = "Strong Democrat")`.
#' @param style One of `"labeled"` (default) or `"paragraph"`. `"labeled"` lists
#'   the demographics and then a Question/Answer block; `"paragraph"` fuses them
#'   into a single natural paragraph. Defaults to
#'   `getOption("focusgroup.persona_style", "labeled")`.
#' @return Character string with the persona description.
#'
#' @keywords internal
generate_persona <- function(demographics, survey_responses = NULL,
                             style = getOption("focusgroup.persona_style", "labeled")) {
  style <- match.arg(style, c("labeled", "paragraph"))

  # Missing and empty fields are absent rather than rendered as "NA", and the
  # code prefixes that some labeled surveys carry on a value label ("1. Male")
  # are stripped.
  demos <- lapply(.fg_persona_fields(demographics), .fg_tidy_value)
  resp  <- lapply(.fg_persona_fields(survey_responses), .fg_tidy_value)

  demo_text <- if (length(demos)) {
    paste(sprintf("%s: %s", names(demos), unlist(demos, use.names = FALSE)),
          collapse = "; ")
  } else NULL

  closing <- paste(
    "Speak from this background as the conversation gives you reason to.",
    "Stay engaged without dominating; respond to the moderator and to what others say.")

  if (identical(style, "paragraph")) {
    bits <- c(
      "You are a participant in a focus group.",
      if (!is.null(demo_text)) sprintf("Your background: %s.", demo_text),
      if (length(resp)) sprintf(
        "On a pre-session questionnaire you said: %s.",
        paste(sprintf("%s -- %s", names(resp), unlist(resp, use.names = FALSE)),
              collapse = "; ")),
      closing)
    return(paste(bits, collapse = " "))
  }

  # "labeled" (default): a structured block, reusing the shared renderers.
  bits <- c(
    "You are a participant in a focus group.",
    if (!is.null(demo_text)) sprintf("Background: %s.", demo_text),
    if (length(resp)) format_survey_responses(resp),
    closing)
  paste(bits, collapse = "\n")
}

# Default missing-value vocabulary for labeled surveys, as regular
# expressions matched against value labels ignoring case. Configurable per
# call. Beyond the common terms, it covers the labels the ANES and WVS
# releases give their negative missing-data codes: "-3. RESTRICTED",
# "-6. No post interview", "-7. Insufficient partial, interview deleted",
# "-5. Sufficient partial, breakoff", "-4. Error", "-2. DK/RF in V241288 or
# V241289", "-2. Open-end responses available in separate file", "-2. Data
# will be released separately when available", and WVS's "No answer", "Not
# applicable", and "Not available". "Error" and "RESTRICTED" count only as a
# whole label, and "break off" and "insufficient" only in a label with a
# negative code, so that answers such as "There is an error" or "Break off
# relations" are kept.
.fg_default_na_strings <- c("inapplicable", "missing", "refused",
                            "don't know", "don.t know", "not asked",
                            "legitimate skip", "no answer", "not applicable",
                            "not available", "\\bdk/rf\\b",
                            "^\\s*(-?[0-9]+\\.)?\\s*restricted\\b",
                            "^\\s*(-?[0-9]+\\.)?\\s*error\\s*$", "technical error",
                            "no post[- ](election )?interview", "interview deleted",
                            "^\\s*-[0-9]+\\..*\\bbreak ?off\\b",
                            "^\\s*-[0-9]+\\..*\\binsuffici?ent\\b", "separate file",
                            "released separately", "will be (released|available)",
                            "not reported")

#' Missing-value patterns used by default for survey files
#'
#' Returns the regular expressions that [create_agents_from_survey()] uses by
#' default to recognize value labels marking an answer as missing, such as
#' "Refused", "Don't know", "Inapplicable", and the labels the ANES and WVS
#' give their negative missing-data codes ("-3. RESTRICTED", "-6. No post
#' interview"). Patterns are matched ignoring case. Extend the vector to
#' match another file's vocabulary.
#'
#' @return A character vector of regular expressions.
#' @seealso [create_agents_from_survey()]
#' @examples
#' na <- c(fg_default_na_strings(), "prefer not to say")
#' head(na)
#' @export
fg_default_na_strings <- function() .fg_default_na_strings

# Choose the column key shown to the model: the human variable label (question
# wording) when present, else the column name. `named` overrides take priority.
.fg_col_key <- function(raw_lab, code, override = NULL) {
  if (!is.null(override) && nzchar(override)) return(override)
  lab <- tryCatch(attr(raw_lab[[code]], "label"), error = function(...) NULL)
  if (!is.null(lab) && nzchar(lab)) lab else code
}

# The text shown for each requested variable of one kind and where it comes
# from. A survey item is shown under its supplied question wording, else its
# label in the file (codebook shorthand, so this warns), else the name given
# to it, else the variable name. A demographic field is shown under the name
# given to it ("age"), else its label, else the variable name.
.fg_field_keys <- function(data, vars, kind, wording = NULL) {
  variables <- unname(as.character(vars))
  given <- names(vars) %||% rep("", length(vars))
  given[is.na(given)] <- ""
  label <- vapply(variables, function(v) {
    lab <- if (v %in% names(data)) attr(data[[v]], "label", exact = TRUE)
    if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) lab else ""
  }, character(1), USE.NAMES = FALSE)
  worded <- if (is.null(wording)) rep(NA_character_, length(variables)) else
    unname(wording[variables])
  source <- if (kind == "item") {
    ifelse(!is.na(worded), "wording",
           ifelse(nzchar(label), "label",
                  ifelse(nzchar(given), "name", "variable")))
  } else {
    ifelse(nzchar(given), "name", ifelse(nzchar(label), "label", "variable"))
  }
  question <- ifelse(source == "wording", worded,
                     ifelse(source == "name", given,
                            ifelse(source == "label", label, variables)))
  question <- .fg_distinct_keys(question, variables,
                                if (kind == "item") "survey items" else "demographic fields")
  data.frame(kind = rep(kind, length(variables)), variable = variables,
             question = as.character(question), question_source = source,
             stringsAsFactors = FALSE)
}

# Shared core: given a decoded demographics frame and survey-responses frame
# (already row-aligned, same nrow), select rows and build agents.
.fg_agents_from_frames <- function(demo_df, survey_df, n_participants,
                                   config, rows = NULL, weights = NULL,
                                   direct_persona_descriptions = NULL,
                                   .runner = NULL) {
  N <- nrow(demo_df)
  if (!is.null(direct_persona_descriptions) &&
      length(direct_persona_descriptions) != N) {
    stop("Direct persona descriptions must align with the respondent rows.",
         call. = FALSE)
  }
  if (!is.numeric(n_participants) || length(n_participants) != 1L ||
      !is.finite(n_participants) || n_participants < 1 ||
      n_participants != floor(n_participants) || n_participants > .Machine$integer.max) {
    stop("`n_participants` must be one positive integer.", call. = FALSE)
  }
  idx <- .fg_row_indices(rows, demo_df, N)
  if (n_participants > length(idx)) {
    stop(sprintf("%d participants were requested, but only %d distinct respondents are eligible.",
                 n_participants, length(idx)), call. = FALSE)
  }
  seed_val <- getOption("focusgroup.seed", NA_integer_)
  prob <- NULL
  if (!is.null(weights)) {
    w <- weights[idx]
    label <- attr(weights, "weight_label") %||% "`weights`"
    if (!any(w > 0)) {
      stop(label, " has no positive weight among the eligible respondents.", call. = FALSE)
    }
    if (length(idx) > n_participants && sum(w > 0) < n_participants) {
      stop(sprintf("A draw of %d respondents requires at least %d eligible respondents with positive weight; found %d.",
                   n_participants, n_participants, sum(w > 0)), call. = FALSE)
    }
    total <- sum(w)
    prob <- if (is.finite(total)) w / total else (w / max(w)) / sum(w / max(w))
  }

  take <- if (length(idx) > n_participants) {
    draw <- function() sample(idx, n_participants, prob = prob)
    # with_seed keeps the seeding contained: the respondent draw is
    # deterministic but the caller's RNG stream is restored afterwards.
    if (!is.null(seed_val) && !is.na(seed_val)) {
      withr::with_seed(as.integer(seed_val), draw())
    } else draw()
  } else idx

  demo_sample <- demo_df[take, , drop = FALSE]
  survey_sample <- if (!is.null(survey_df)) survey_df[take, , drop = FALSE] else NULL
  persona_sample <- if (!is.null(direct_persona_descriptions)) {
    direct_persona_descriptions[take]
  } else {
    NULL
  }

  agents <- create_agents(
    n_participants = n_participants,
    config = config,
    demographics = demo_sample,
    survey_responses = survey_sample,
    direct_persona_descriptions = persona_sample,
    .runner = .runner
  )
  attr(agents, "source_rows") <- stats::setNames(as.integer(take), paste0("P", seq_along(take)))
  agents
}

#' Create agents from a labeled survey file
#'
#' Reads a labeled survey file (Stata `.dta`, SPSS `.sav`, or SAS
#' `.sas7bdat`) and turns each selected respondent into an `FGAgent` whose
#' persona states that respondent's demographics and survey answers. Answers
#' are rendered from the file's own value labels, so the same call works on
#' ANES, GSS, WVS, or any other labeled file; nothing about a particular
#' dataset is hard-coded.
#'
#' Each survey answer is shown under its question: the wording supplied in
#' `question_wording`, else the variable's label in the file, else the name
#' given to the variable in `survey_vars`. A label is codebook shorthand
#' rather than the question asked, so items shown without a wording draw a
#' warning. Demographic fields are shown under the names given in
#' `demographic_vars` ("age: 25"), else their labels. The persona draws no
#' inferences from the answers; it states them and lets the model interpret.
#'
#' @param n_participants Integer number of participants (excludes the moderator).
#' @param survey_path Path to the survey file (`.dta`, `.sav`, or `.sas7bdat`).
#' @param config An explicit `LLMR::llm_config` for all agents.
#' @param demographic_vars Variable names (codes) to render as demographics.
#'   May be a named vector, in which case the names are shown as the field
#'   labels. If `NULL`, common demographic variables are auto-detected by their
#'   labels.
#' @param survey_vars Variable names (codes) to render as survey responses. May
#'   be named; a name is used as the question only for an item with neither a
#'   `question_wording` entry nor a label in the file. If `NULL`, labeled
#'   variables that are not demographics are used, except a case ID, which
#'   is left out with a message: a variable without value labels whose values
#'   are whole numbers or codes without spaces and differ in every row, in a
#'   file of at least 30 rows or under a name or label that reads as an
#'   identifier ("id", "case", "respondent", "serial"). Variables named `id`, `respondent_id`, or
#'   `persona_id` are never rendered.
#' @param rows Optional row selector restricting the eligible respondents before
#'   sampling: integer positions of rows in the file, a logical vector with one
#'   value per row in the file, or a predicate `function(df) -> logical` over the
#'   demographics frame, whose columns hold the text rendered for each field.
#'   The selector refers to the file's own rows and
#'   is applied before respondents without any demographic value are left out;
#'   a message says how many selected respondents were left out for that
#'   reason. Positions must be whole numbers within the file. In a logical
#'   selector or a predicate's result, `NA` counts as not selected, as in
#'   [subset()], so `function(df) df$age != "18-24"` leaves out respondents
#'   whose age is missing. A position given more than once selects its
#'   respondent once.
#' @param weights Optional sampling weights: a column name in the file, or a
#'   numeric vector aligned to the file's rows. Used only to weight which
#'   respondents are drawn; the weight column is not rendered into the
#'   personas, even when `demographic_vars` or `survey_vars` name it. Weights
#'   must be nonnegative and finite, with at least one positive; numeric
#'   values carrying haven value labels are accepted.
#' @param na_strings Character vector of value-label substrings, matched as
#'   regular expressions and ignoring case, that mark an answer as missing.
#'   The default covers common terms ("inapplicable", "missing", "refused",
#'   "don't know", "not asked", "legitimate skip", "no answer", "not
#'   applicable", "not available") and the labels the ANES 2024 release gives
#'   its negative missing-data codes, such as "-3. RESTRICTED", "-6. No post
#'   interview", "-5. Sufficient partial, breakoff", "-4. Error", "-2. DK/RF in
#'   V241288 or V241289", and "-2. Open-end responses available in separate
#'   file". "Error" and "RESTRICTED" count only as a whole label. Pass your own
#'   vector, or extend [fg_default_na_strings()], to match another file's
#'   missing-data vocabulary.
#' @param question_wording Optional named character vector giving the question
#'   as asked for survey items, keyed by variable name, as in
#'   `c(V241248 = "Where would you place yourself on abortion?")`. It takes
#'   precedence over the variable labels and the names of `survey_vars`;
#'   entries for variables not in `survey_vars` are ignored. An item without
#'   a wording is shown under its variable label (or, lacking one, its name),
#'   with a warning that lists such items: a variable label is often codebook
#'   shorthand ("PRE: 7pt scale abortion: self-placement") rather than the
#'   question.
#' @param value_labels Optional labels that replace or extend the file's value
#'   labels: a data frame with columns `variable`, `code`, and `label`, or a
#'   named list of named vectors in the form of haven value labels, as in
#'   `list(V241386 = c("Make all unauthorized immigrants felons and send them
#'   back to their home country" = 1))`. A supplied label replaces the file's
#'   label for its code, or labels a code the file leaves unlabeled; use it to
#'   restore labels the file cut short or to label stray codes. Supplied labels
#'   are matched against `na_strings` like the file's own. Entries for
#'   variables that are not requested are not used, with a warning.
#' @param scale_template The sentence that states an answer placed on a scale
#'   whose endpoints alone are labeled (see Rendering answers). The
#'   placeholders `{lo}` and `{hi}` are the endpoint codes, `{lo_label}` and
#'   `{hi_label}` their labels, and `{value}` the answer, which every template
#'   must contain; any other placeholder is an error. The default, "On a scale
#'   of \{lo\} to \{hi\}, where \{lo\} is \{lo_label\} and \{hi\} is
#'   \{hi_label\}, you place yourself at \{value\}.", fits self-placement
#'   items. Give one template for every item, or templates named by variable
#'   with at most one unnamed template as the default for the rest. Items that
#'   place someone else or rate a group need their own wording, as in
#'   `c(V241240 = "On a scale of {lo} to {hi}, where {lo} is {lo_label} and
#'   {hi} is {hi_label}, your placement of the Democratic candidate is
#'   {value}.")`. The template used is recorded in the rendering report.
#' @param .runner Optional function used instead of live model calls and stored
#'   on every agent.
#'
#' @details
#' Respondents are drawn without replacement, so a participant is never a
#' second copy of another. When the selection leaves more eligible respondents
#' than `n_participants`, that many are sampled, with probability proportional
#' to `weights` when supplied, and the draw needs at least `n_participants`
#' respondents with a positive weight. When the selection leaves exactly
#' `n_participants`, all of them are used, whatever their weights; when it
#' leaves fewer, the call is refused. The option `focusgroup.seed` makes the
#' draw reproducible without changing the caller's random-number state.
#'
#' Invalid weights are refused rather than repaired. The call stops when the
#' named weight column is not in the file, when the weights are not numeric
#' (a factor, text, or logical values), when their length differs from the
#' number of rows, when any weight is missing, negative, or not finite, when
#' no eligible respondent has a positive weight, or when a draw has fewer
#' respondents with a positive weight than participants to fill. The error
#' names the column or the offending rows.
#'
#' @section Rendering answers:
#' An answer in a variable with value labels is rendered by the first of the
#' following rules that applies, and the rule used is recorded in the
#' rendering report described under Value.
#'
#' * A code with a value label is shown as its label, without a leading code
#'   of zero or above such as "1. ". A label that matches `na_strings` marks
#'   a missing value, and the answer is left out. Two kinds of label from the
#'   file are shown but flagged, with a warning naming the variable and code:
#'   a label exactly 80 characters long, since some releases, the ANES 2024
#'   Stata file among them, cut longer labels at that length; and the label
#'   of a negative code that does not match `na_strings`, shown with its
#'   code, since the ANES and WVS releases use negative codes for missing
#'   data. Extend `na_strings` to leave the latter out.
#' * A number without a label that lies between the two labeled endpoints of
#'   a scale is shown on that scale, in the sentence given by
#'   `scale_template`: "On a scale of 1 to 7, where 1 is Government should
#'   provide many fewer services and 7 is Government should provide many more
#'   services, you place yourself at 5." The endpoints are the nearest labeled
#'   codes on either side of the answer, and they must be the only labels of
#'   the scale: no other label may lie below the lower endpoint, and labels
#'   above the upper endpoint must lie more than the scale's width beyond it.
#'   An off-scale code such as "99. Haven't thought much about this" after a
#'   1 to 7 scale is therefore not an endpoint, while a labeled midpoint, as
#'   in a 0 to 10 scale labeled at 0, 5, and 10, rules out the scale form.
#'   Negative codes below a scale that starts at zero or above are taken for
#'   missing-data codes. Labels matching `na_strings` are never endpoints, a
#'   scale with a negative endpoint must be centered on zero, and top or
#'   bottom codes such as "89 or older" do not make a scale.
#' * A negative number without a label that is not a point on a scale is
#'   taken for a missing-data code, as negative codes are in the ANES and WVS
#'   releases, and left out, with a warning. Label it in `value_labels` if it
#'   is an answer.
#' * A number without a label in a variable that holds numbers is shown as
#'   the number, as in "age: 25". A variable holds numbers when the file
#'   labels at most one of its substantive codes of zero or above, such as
#'   the top code "80. Eighty years old or older", or labels only top and
#'   bottom codes.
#' * Any other code without a label is shown as the code, with a warning
#'   naming the variable and code. Supply its label in `value_labels`.
#'
#' A variable without value labels is shown as stored, with numbers written
#' out in full ("100000"); an empty value, or one matching `na_strings`, is
#' left out.
#'
#' Answers left out as missing values are announced in a message. Requested
#' variables that are not in the file are left out of every persona, and a
#' variable requested more than once is rendered once, each with a warning.
#' When two variables would be shown under the same text, each is shown with
#' its variable name appended, with a warning. A variable label exactly 80
#' characters long shown as a question or field name is also named in a
#' warning.
#'
#' @return A named list of `FGAgent` objects (participants + moderator), keyed
#'   by agent ID. Two attributes record which respondents were drawn. The
#'   `source_rows` attribute is an integer vector of their row positions in the
#'   file, named by participant ID. When the file has columns named `id`,
#'   `respondent_id`, or `persona_id`, the `source_ids` attribute is a data
#'   frame of those columns for the drawn respondents, with participant IDs as
#'   row names. The moderator appears in neither. A third attribute,
#'   `rendering_report`, records for each participant and each requested field
#'   and item the text shown or the reason it was left out; read it with
#'   [persona_rendering_report()]. Subsetting the list drops the attributes,
#'   so read them from the list as returned.
#' @seealso [create_agents_from_data()] for an in-memory data frame,
#'   [persona_rendering_report()] for the record of each persona, and
#'   `LLMR::anes_2024_personas` for a ready-made example.
#' @examples
#' # A small labeled file stands in for a survey release; the call is the same
#' # against a full ANES, GSS, or WVS file.
#' toy <- data.frame(
#'   age = haven::labelled(c(1L, 2L, 3L, 2L),
#'                         c("18-24" = 1L, "25-34" = 2L, "35-44" = 3L)),
#'   pid = haven::labelled(c(1L, 2L, 2L, 1L),
#'                         c("Democrat" = 1L, "Republican" = 2L))
#' )
#' path <- file.path(tempdir(), "toy_survey.dta")
#' haven::write_dta(toy, path)
#'
#' agents <- create_agents_from_survey(
#'   n_participants = 2,
#'   survey_path = path,
#'   config = LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                             api_key = LLMR::llm_api_key_env("GROQ_API_KEY")),
#'   demographic_vars = c(age = "age"),
#'   survey_vars = c("Party identification" = "pid"),
#'   rows = function(df) df$age != "18-24"
#' )
#' agents$P1$persona_description
#' @export
create_agents_from_survey <- function(n_participants,
                                      survey_path,
                                      config,
                                      demographic_vars = NULL,
                                      survey_vars = NULL,
                                      rows = NULL,
                                      weights = NULL,
                                      na_strings = fg_default_na_strings(),
                                      question_wording = NULL,
                                      value_labels = NULL,
                                      scale_template = .fg_default_scale_template,
                                      .runner = NULL) {
  .fg_validate_config(config)
  question_wording <- .fg_check_wording(question_wording)
  value_labels <- .fg_check_value_labels(value_labels)
  scale_template <- .fg_check_template(scale_template)
  if (!file.exists(survey_path)) stop("Survey file not found at: ", survey_path)

  ext <- tolower(tools::file_ext(survey_path))
  raw_lab <- switch(ext,
    "dta" = haven::read_dta(survey_path),
    "sav" = haven::read_sav(survey_path),
    "sas7bdat" = haven::read_sas(survey_path),
    stop("Unsupported survey file type: .", ext)
  )

  weight_col <- if (is.character(weights) && length(weights) == 1L) weights else character()
  weights_vec <- .fg_resolve_weights(weights, raw_lab)

  label_of <- function(v) tryCatch(attr(raw_lab[[v]], "label") %||% "", error = function(...) "")
  detect_by_label <- function(patterns) {
    labs <- vapply(names(raw_lab), label_of, "")
    names(raw_lab)[which(Reduce(`|`, lapply(patterns, function(p)
      grepl(p, labs, ignore.case = TRUE))))]
  }

  # Demographics: explicit, else auto-detect common fields by label.
  if (is.null(demographic_vars)) {
    pick1 <- function(p) { hit <- detect_by_label(p); if (length(hit)) hit[[1]] else NA_character_ }
    demographic_vars <- c(age = pick1("age"), gender = pick1(c("sex", "gender")),
                          education = pick1("educ"), race = pick1("race"),
                          income = pick1("income"))
    demographic_vars <- demographic_vars[!is.na(demographic_vars)]
  }
  # Survey vars: explicit, else any labeled var that is neither a demographic
  # nor the sampling-weight column, which is not something a respondent said.
  auto_survey <- is.null(survey_vars)
  if (auto_survey) {
    has_label <- vapply(names(raw_lab), function(v) nzchar(label_of(v)), logical(1))
    survey_vars <- setdiff(names(raw_lab)[has_label],
                           c(unname(demographic_vars), weight_col))
  }

  had_demographics <- length(demographic_vars) > 0L
  demographic_vars <- demographic_vars[!unname(demographic_vars) %in% weight_col]
  survey_vars <- survey_vars[!unname(survey_vars) %in% weight_col]
  if (had_demographics && !length(demographic_vars)) {
    stop("No usable `demographic_vars` remain after excluding the sampling-weight column.", call. = FALSE)
  }
  repeated <- c(unique(unname(demographic_vars)[duplicated(unname(demographic_vars))]),
                unique(unname(survey_vars)[duplicated(unname(survey_vars))]))
  if (length(repeated)) {
    warning("These variables are requested more than once and are rendered once, ",
            "under their first entry: ", paste(unique(repeated), collapse = ", "), ".",
            call. = FALSE)
  }
  demographic_vars <- demographic_vars[!duplicated(unname(demographic_vars))]
  survey_vars <- survey_vars[!duplicated(unname(survey_vars))]
  if (!any(unname(demographic_vars) %in% names(raw_lab))) {
    stop("None of `demographic_vars` are in the file.")
  }
  absent <- setdiff(c(unname(demographic_vars), unname(survey_vars)), names(raw_lab))
  if (length(absent)) {
    warning("These variables are not in the file and are left out of every persona: ",
            paste(absent, collapse = ", "), ".", call. = FALSE)
  }
  requested <- c(unname(demographic_vars), unname(survey_vars))
  # Identifiers never enter a persona: the columns named as identifiers and,
  # among automatically chosen items, variables that look like a case ID.
  identifier_vars <- intersect(requested, .fg_identifier_columns)
  if (auto_survey) {
    distinct <- Filter(function(v) {
      v %in% names(raw_lab) && .fg_distinct_in_every_row(raw_lab[[v]], v)
    }, setdiff(unname(survey_vars), identifier_vars))
    if (length(distinct)) {
      message("Left out of the personas as respondent identifiers, since their ",
              "values differ in every row of the file: ",
              paste(unlist(distinct), collapse = ", "),
              ". Name them in `survey_vars` to render them.")
      identifier_vars <- c(identifier_vars, unlist(distinct))
    }
  }
  unused <- list(`value_labels` = setdiff(unique(value_labels$variable), requested),
                 `scale_template` = setdiff(names(scale_template)[nzchar(names(scale_template))],
                                            requested))
  for (argument in names(unused)) {
    if (length(unused[[argument]])) {
      warning(sprintf("`%s` names variables that are not requested, so its entries for them are not used: %s.",
                      argument, paste(unused[[argument]], collapse = ", ")), call. = FALSE)
    }
  }

  # Each requested field and item: the text shown for it and where that text
  # comes from, then the rendering of every respondent's answer.
  fields <- rbind(
    .fg_field_keys(raw_lab, demographic_vars, "demographic"),
    .fg_field_keys(raw_lab, survey_vars, "item", question_wording))
  rendered <- lapply(seq_len(nrow(fields)), function(j) {
    v <- fields$variable[j]
    if (!v %in% names(raw_lab)) {
      return(.fg_dropped_rows(nrow(raw_lab), "the variable is not in the file", NA_character_))
    }
    if (v %in% identifier_vars) {
      out <- .fg_dropped_rows(nrow(raw_lab), "identifier column", NA_character_)
      out$code <- .fg_value_text(unclass(raw_lab[[v]]))
      return(out)
    }
    supplied <- if (!is.null(value_labels)) {
      value_labels[value_labels$variable == v, , drop = FALSE]
    }
    .fg_render_column(raw_lab[[v]], v, na_strings, supplied,
                      .fg_template_for(scale_template, v))
  })
  present <- fields[fields$variable %in% names(raw_lab) &
                      !fields$variable %in% identifier_vars, , drop = FALSE]
  .fg_question_conditions(present)
  cut <- present[present$question_source == "label" & nchar(present$question) == 80L, ,
                 drop = FALSE]
  if (nrow(cut)) {
    warning("Variable labels exactly 80 characters long, shown as the text for these ",
            "variables, may have been cut short: ", .fg_list_items(cut$variable),
            ". Supply `question_wording` for items and names for demographic fields.",
            call. = FALSE)
  }

  frame_of <- function(kind) {
    j <- which(fields$kind == kind & fields$variable %in% names(raw_lab))
    if (!length(j)) return(NULL)
    out <- as.data.frame(lapply(rendered[j], `[[`, "text"), stringsAsFactors = FALSE)
    names(out) <- fields$question[j]
    out
  }
  demo_df <- frame_of("demographic")
  survey_df <- frame_of("item")

  # `rows` selects among the file's own rows, so it is resolved before
  # respondents without any demographic value are left out; demographics and
  # survey answers are filtered by the same mask and never misalign.
  has_demo <- rowSums(!is.na(demo_df)) > 0
  selected <- .fg_resolve_rows(rows, demo_df, nrow(demo_df))
  n_left_out <- sum(selected & !has_demo)
  if (n_left_out > 0L) {
    message(sprintf(
      "%d %srespondent(s) have no demographic values and were left out.",
      n_left_out, if (is.null(rows)) "" else "selected "))
  }
  keep <- selected & has_demo
  if (!any(keep)) {
    stop(if (is.null(rows)) {
      "No respondent in the file has a value for any of `demographic_vars`."
    } else {
      "`rows` selected no respondents with demographic values."
    }, call. = FALSE)
  }
  demo_df <- demo_df[keep, , drop = FALSE]
  if (!is.null(survey_df)) survey_df <- survey_df[keep, , drop = FALSE]
  if (!is.null(weights_vec)) {
    weight_label <- attr(weights_vec, "weight_label")
    weights_vec <- weights_vec[keep]
    attr(weights_vec, "weight_label") <- weight_label
  }

  agents <- .fg_agents_from_frames(
    demo_df, survey_df, n_participants, config,
    rows = NULL, weights = weights_vec, .runner = .runner
  )
  source_rows <- which(keep)[attr(agents, "source_rows")]
  agents <- .fg_roster_provenance(agents, raw_lab, source_rows)
  report <- .fg_rendering_report(names(attr(agents, "source_rows")), source_rows,
                                 fields, rendered)
  .fg_rendering_conditions(.fg_check_report(report, agents))
  .fg_attach_report(agents, report)
}

# Resolve a `rows` selector to row positions in `df`, which has `n` rows. A
# predicate is evaluated once; its result, like a logical selector, must give
# one value per row, and NA counts as not selected, as in subset(), so that a
# predicate over a variable with missing values selects the rows where it is
# TRUE. Integer positions must be whole numbers between 1 and `n`; their order
# is kept and a repeated position counts once, so a selector cannot draw the
# same respondent twice.
.fg_row_indices <- function(rows, df, n) {
  if (is.null(rows)) return(seq_len(n))
  predicate <- is.function(rows)
  if (predicate) rows <- rows(df)
  if (predicate || is.logical(rows)) {
    label <- if (predicate) "A `rows` predicate" else "A logical `rows` selector"
    if (!is.logical(rows) || !is.null(dim(rows)) || length(rows) != n) {
      bad <- if (!is.logical(rows) || !is.null(dim(rows))) seq_len(max(n, length(rows))) else
        seq.int(min(n, length(rows)) + 1L, max(n, length(rows)))
      stop(label, if (predicate) " must return" else " must have",
           " one logical value per respondent; invalid positions: ",
           paste(utils::head(bad, 6L), collapse = ", "),
           ".", call. = FALSE)
    }
    return(which(rows))
  }
  valid_type <- is.numeric(rows) && !is.complex(rows) && !is.factor(rows) && is.null(dim(rows))
  bad <- if (valid_type) which(!is.finite(rows) | rows != floor(rows) | rows < 1 | rows > n) else seq_along(rows)
  if (!valid_type || length(bad)) {
    stop("`rows` must be whole numbers between 1 and the number of respondents; invalid values at position(s) ",
         paste(utils::head(bad, 6L), collapse = ", "), ".", call. = FALSE)
  }
  unique(as.integer(rows))
}

# The same selector as a logical mask over the `n` rows, in file order.
.fg_resolve_rows <- function(rows, df, n) {
  seq_len(n) %in% .fg_row_indices(rows, df, n)
}

# Validate sampling weights against every row of `data`, before any selection,
# and return them as doubles. `weights` is a column name or a vector aligned to
# the rows. Anything that is not a usable weight is refused with an error that
# names the column or rows, never coerced: a factor would sample by its codes,
# and a missing or negative weight would silently exclude a respondent. The
# returned vector carries a label for later messages.
.fg_resolve_weights <- function(weights, data) {
  if (is.null(weights)) return(NULL)
  label <- "`weights`"
  if (is.character(weights) && length(weights) == 1L &&
      !is.na(weights) && nzchar(weights)) {
    if (!weights %in% names(data)) {
      stop(sprintf("Weight column '%s' is not present in the input data.", weights), call. = FALSE)
    }
    label <- sprintf("Weight column '%s'", weights)
    weights <- data[[weights]]
  }
  if (!(typeof(weights) %in% c("integer", "double")) || is.factor(weights) ||
      !is.null(dim(weights))) {
    stop(label, " must be an integer or double vector; factors, text, logical values, and arrays are not sampling weights.",
         call. = FALSE)
  }
  if (length(weights) != nrow(data)) {
    stop(sprintf("%s has length %d; expected one weight for each of the %d respondents.",
                 label, length(weights), nrow(data)), call. = FALSE)
  }
  bad <- which(!is.finite(weights) | weights < 0)
  if (length(bad)) {
    stop(sprintf("%s contains missing, negative, or nonfinite weights at source row(s) %s.",
                 label, paste(utils::head(bad, 6L), collapse = ", ")), call. = FALSE)
  }
  if (!any(weights > 0)) stop(label, " must contain at least one positive weight.", call. = FALSE)
  out <- as.double(weights)
  attr(out, "weight_label") <- label
  out
}

# Columns that identify a respondent. They are recorded in a roster's
# `source_ids` and never rendered into a persona.
.fg_identifier_columns <- c("id", "respondent_id", "persona_id")

# Whether a survey variable looks like a case identifier: no value labels, no
# missing value, a different value in every row, and values that are codes
# (whole numbers, or text without spaces, which leaves out open-ended
# answers). In a small file, where ages or amounts can differ in every row by
# chance, the variable's name or label must also read as an identifier.
.fg_distinct_in_every_row <- function(x, name = "") {
  labels <- attr(x, "labels", exact = TRUE)
  if (length(labels)) return(FALSE)
  v <- as.vector(unclass(x))
  if (length(v) < 2L || anyNA(v) || anyDuplicated(v)) return(FALSE)
  codes <- if (is.numeric(v)) all(is.finite(v) & v == floor(v)) else
    is.character(v) && !any(grepl("\\s", v))
  if (!codes) return(FALSE)
  label <- attr(x, "label", exact = TRUE)
  words <- tolower(paste(c(name, if (is.character(label)) label), collapse = " "))
  id_like <- grepl("(^|[^a-z])(id|ids|caseid|case|respondent|serial|uuid)([^a-z]|$)", words)
  length(v) >= 30L || id_like
}

# Report rows for identifier columns left out of the personas.
.fg_mark_identifiers <- function(report) {
  hit <- report$variable %in% .fg_identifier_columns & report$status == "dropped"
  report$reason[hit] <- "identifier column"
  report$issue[hit] <- NA_character_
  report
}

# Record on a roster which rows of `data` its participants were drawn from
# (`source_rows`, named by agent ID) and, when `data` has identifier columns,
# their identifiers (`source_ids`). The moderator is not a respondent and is
# left out of both.
.fg_roster_provenance <- function(agents, data, rows) {
  ids <- names(agents)[!vapply(agents, function(a) isTRUE(a$is_moderator), logical(1))]
  attr(agents, "source_rows") <- stats::setNames(as.integer(rows), ids)
  columns <- intersect(.fg_identifier_columns, names(data))
  if (length(columns)) {
    source_ids <- as.data.frame(data[rows, columns, drop = FALSE])
    rownames(source_ids) <- ids
    attr(agents, "source_ids") <- source_ids
  }
  agents
}

#' Create agents from an in-memory data frame of respondents
#'
#' Like [create_agents_from_survey()] but starting from a data frame already in
#' memory (for example `LLMR::anes_2024_personas`). Demographic columns are
#' rendered as background; the remaining columns are rendered as survey responses,
#' keyed by their question wording when the frame carries a `dictionary` attribute
#' (see [LLMR::llm_persona_split()]), else by their column names. Values are taken
#' as-is (decode and clean them first if they are still coded). Columns named
#' `id`, `respondent_id`, or `persona_id` identify respondents and are not
#' rendered: they are kept in the `source_ids` attribute and listed in the
#' rendering report as dropped. The same holds for those columns in a data
#' frame given to [create_agents()].
#'
#' A frame of class `silicon_panel`, or a frame with both `persona` and
#' `persona_id` columns, is treated as a pre-rendered persona panel. Its
#' `persona` text becomes each participant's direct persona description;
#' `persona` and `persona_id` are not rendered as survey answers. This bridge
#' uses the frame's structure and does not require LLMRpanel.
#'
#' @param data A data frame, one respondent per row.
#' @param n_participants Integer number of participants (excludes the moderator).
#' @param config An explicit `LLMR::llm_config` for all agents.
#' @param demographic_cols Character vector of columns to render as demographics.
#'   Defaults to the `data`'s `"demographic_fields"` attribute when present, else
#'   a small set of common demographic column names found in `data`.
#' @param rows,weights See [create_agents_from_survey()]. Here `rows` refers to
#'   the rows of `data`, and a predicate receives the demographics frame with
#'   its columns relabeled as they appear in the personas. Integer positions
#'   are kept in the order given.
#' @param .runner Optional function used instead of live model calls and stored
#'   on every agent.
#' @details
#' Sampling and the validation of `weights` follow
#' [create_agents_from_survey()].
#'
#' @return A named list of `FGAgent` objects (participants + moderator), keyed
#'   by agent ID, with the `source_rows` and `source_ids` attributes described
#'   in [create_agents_from_survey()]; here the positions refer to the rows of
#'   `data`. Unless the frame is a pre-rendered persona panel, the list also
#'   carries a `rendering_report` attribute recording, for each participant
#'   and column, the value shown or that it was missing; see
#'   [persona_rendering_report()].
#' @examples
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                         api_key = LLMR::llm_api_key_env("GROQ_API_KEY"))
#' agents <- create_agents_from_data(
#'   LLMR::anes_2024_personas, n_participants = 6, config = cfg
#' )
#' agents$P1$persona_description
#' @export
create_agents_from_data <- function(data, n_participants, config,
                                    demographic_cols = NULL,
                                    rows = NULL, weights = NULL,
                                    .runner = NULL) {
  stopifnot(is.data.frame(data))
  .fg_validate_config(config)

  wv <- .fg_resolve_weights(weights, data)

  is_silicon_panel <- inherits(data, "silicon_panel") ||
    all(c("persona", "persona_id") %in% names(data))
  if (is_silicon_panel && !"persona" %in% names(data)) {
    stop("A `silicon_panel` frame must contain a `persona` column.", call. = FALSE)
  }
  direct_personas <- if (is_silicon_panel) as.character(data$persona) else NULL
  if (!is.null(direct_personas) &&
      (anyNA(direct_personas) || any(!nzchar(trimws(direct_personas))))) {
    stop("The `persona` column must contain non-empty text for every row.",
         call. = FALSE)
  }
  reserved_panel_cols <- if (is_silicon_panel) c("persona", "persona_id") else character()

  if (is.null(demographic_cols)) {
    demographic_cols <- attr(data, "demographic_fields")
  }
  if (is.null(demographic_cols)) {
    common <- c("age", "sex", "gender", "education", "race/ethnicity", "race",
                "marital status", "household income", "income", "religion",
                "census region", "region", "community type", "employment status",
                "home ownership", "children in household", "union household",
                "military service", "attention to politics")
    demographic_cols <- intersect(common, names(data))
  }
  # a sampling-weight column and a score/sort column are not persona facts,
  # and an identifier column would show the respondent's ID to the agents
  weight_col <- if (is.character(weights) && length(weights) == 1L) weights else character(0)
  id_cols <- setdiff(intersect(.fg_identifier_columns, names(data)),
                     c(reserved_panel_cols, weight_col))
  id_kinds <- ifelse(id_cols %in% demographic_cols, "demographic", "item")
  demographic_cols <- setdiff(intersect(demographic_cols, names(data)),
                              c(reserved_panel_cols, weight_col, id_cols))
  drop_cols <- intersect(c("ideology_score", weight_col), names(data))
  survey_cols <- setdiff(names(data), c(demographic_cols, drop_cols,
                                        reserved_panel_cols, id_cols))

  to_char <- function(df) {
    if (!ncol(df)) return(data.frame(row.names = seq_len(nrow(df))))
    as.data.frame(lapply(df, function(x) {
      v <- .fg_value_text(x); v[!nzchar(v)] <- NA; v
    }), stringsAsFactors = FALSE, check.names = FALSE)
  }

  # Render with the human question wording, not the tidy column handle, when the
  # data carries a dictionary (handle -> question). Handles stay the column names
  # so dplyr select()/filter() work; the persona text reads naturally.
  dict <- attr(data, "dictionary")
  relabel <- function(cols) {
    if (is.null(dict) || !all(c("handle", "question") %in% names(dict))) return(cols)
    q <- dict$question[match(cols, dict$handle)]
    ifelse(is.na(q), cols, q)
  }

  field_keys <- function(cols, kind) {
    question <- relabel(cols)
    data.frame(kind = rep(kind, length(cols)), variable = cols,
               question = .fg_distinct_keys(question, cols, if (kind == "item")
                 "survey items" else "demographic fields"),
               question_source = ifelse(question == cols, "variable", "dictionary"),
               stringsAsFactors = FALSE)
  }
  fields <- rbind(field_keys(demographic_cols, "demographic"),
                  field_keys(survey_cols, "item"))

  demo_df <- to_char(data[, demographic_cols, drop = FALSE])
  names(demo_df) <- fields$question[fields$kind == "demographic"]
  survey_df <- if (length(survey_cols)) {
    s <- to_char(data[, survey_cols, drop = FALSE])
    names(s) <- fields$question[fields$kind == "item"]; s
  } else NULL

  agents <- .fg_agents_from_frames(
    demo_df, survey_df, n_participants, config,
    rows = rows, weights = wv,
    direct_persona_descriptions = direct_personas,
    .runner = .runner
  )
  agents <- .fg_roster_provenance(agents, data, attr(agents, "source_rows"))
  if (is_silicon_panel) return(agents)

  # The values are rendered as given, so the report records each one as shown
  # or, when it is missing or empty, as dropped. Identifier columns are
  # recorded as dropped.
  rendered <- lapply(c(as.list(demo_df), as.list(survey_df)), function(v) {
    none <- .fg_no_value(v)
    data.frame(code = v, text = ifelse(none, NA_character_, v),
               rendered_as = ifelse(none, NA_character_, "value"),
               status = ifelse(none, "dropped", "rendered"),
               reason = ifelse(none, "no value", NA_character_),
               template = NA_character_,
               issue = ifelse(none, "missing", NA_character_), flag = v,
               stringsAsFactors = FALSE)
  })
  if (length(id_cols)) {
    fields <- rbind(fields, data.frame(
      kind = id_kinds, variable = id_cols, question = id_cols,
      question_source = "variable", stringsAsFactors = FALSE))
    rendered <- c(rendered, lapply(id_cols, function(col) {
      out <- .fg_dropped_rows(nrow(data), "identifier column", NA_character_)
      out$code <- .fg_value_text(data[[col]])
      out
    }))
  }
  source_rows <- attr(agents, "source_rows")
  report <- .fg_rendering_report(names(source_rows), source_rows, fields, rendered)
  .fg_attach_report(agents, .fg_check_report(report, agents))
}

 
