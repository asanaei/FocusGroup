# File: R/FocusGroup.R
# Purpose: Defines the main FocusGroup R6 class for simulation management.

#' @import R6
#' @import dplyr
#' @import tidyr
#' @importFrom LLMR call_llm_robust llm_config tokens finish_reason
NULL

#' FocusGroup R6 Class
#'
#' @description
#' Main R6 class for managing and running a focus group simulation.
#' It orchestrates the simulation through distinct phases (Opening, Icebreaker,
#' Engagement, Exploration, Closing) guided by a question script, manages agent
#' interactions via a turn-taking flow, and provides methods for analysis and
#' visualization of the conversation.
#'
#' @field topic Character. The main topic of the focus group discussion.
#' @field purpose Character. The primary purpose or research objective of conducting the focus group.
#' @field agents Named list. A list of `FGAgent` objects participating in the simulation, indexed by their IDs.
#' @field moderator_id Character. The ID of the agent designated as the moderator.
#' @field conversation_log List. A chronological log of all messages. Each message is a list
#'   that includes at least `message_id`, `round`, `item`, `speaker_id`,
#'   `handle`, `is_moderator`, `text`, `timestamp`, `phase`, and `metadata`,
#'   along with call metadata: `response_id`, `finish_reason`, `sent_tokens`,
#'   `rec_tokens`, `total_tokens`, `duration_s`, `provider`, and `model`.
#'   `message_id` is unique message order; moderator and participant messages
#'   from one moderator cycle share `round`, and `item` is the guide item the
#'   message belongs to (0 for the roster row). `speaker_id` is the agent ID
#'   and `handle` the label the agents saw.
#' @field handles Named character vector. The label under which each agent
#'   appears to the other agents, named by agent ID; see the `handles`
#'   argument of `new()`.
#' @field call_on_silent List. The guide items at which the moderator calls on
#'   the participants who have not spoken, as `points` (the name of each
#'   point: `"opening"`, `"midpoint"`, `"end"`, or `"item:<k>"`) and `items`
#'   (the item numbers). Both are empty when call-ons are off.
#' @field call_on_text Character. The moderator's call-on line;
#'   `{participant}` is replaced by the participant's handle.
#' @field turn_taking_flow A `ConversationFlow` object dictating participant turn-taking.
#' @field prompt_templates List. Holds prompt templates for agent/moderator actions.
#' @field question_script List. A structured list defining phases and specific questions/actions for the moderator.
#'   Each element is a list with `phase` (e.g., "opening", "icebreaker_question") and optionally `text` (for specific questions).
#' @field current_question_text Character. Text of the current question being discussed.
#' @field current_conversation_summary Character. An LLM-generated summary of earlier parts of the conversation,
#'   used for managing context length in prompts.
#' @field final_summary Character. Final LLM-generated summary from the most recent simulation run.
#' @field message_mode Character. The message construction used by the most recent
#'   `run_simulation()` ("roleflip" or "flat"), recorded so a saved object can be
#'   replayed (e.g. in the GUI continuation experiment) with the same construction.
#' @field admin_config An `llm_config` object for group-level model tasks.
#' @field max_tokens_utterance Integer. Default max tokens for participant utterances.
#' @field max_tokens_moderator Integer. Default max tokens for moderator utterances.
#' @field max_tokens_desire Integer. Default max tokens for desire-to-talk queries.
#' @field max_tokens_desire_retry Integer. Max tokens for the one retry of a
#'   desire query whose reply was empty or cut off.
#' @field max_tokens_summary Integer. Max tokens for the final summary.
#' @field max_tokens_interim_summary Integer. Max tokens for an interim summary.
#' @field max_tokens_probe Integer or `NULL`. Max tokens for a probe answer;
#'   `NULL` uses the agent configuration's limit, or 300 when it sets none.
#' @field reasoning_effort `NULL`, one reasoning-effort setting for every call
#'   type, or a named list of settings by call type (`"moderator"`,
#'   `"participant"`, `"desire"`, `"summary"`, `"probe"`). A setting is written
#'   over the configuration's `reasoning_effort` for those calls; `NULL` leaves
#'   the configurations as they are.
#' @field max_participant_responses Integer. Maximum number of participant exchanges per round before the moderator can intervene. Can also be set globally via `options(focusgroup.max_participant_responses = N)`.
#' @field total_tokens_sent Numeric. Tokens sent across all LLM calls in the
#'   group, as reported; calls that reported no count add nothing.
#' @field total_tokens_received Numeric. Tokens received across all LLM calls,
#'   as reported.
#' @field total_tokens Numeric. Total tokens across all LLM calls: each call's
#'   reported total, or its sent and received counts when it reported no total.
#' @field calls_usage_unknown Numeric. Number of calls whose usage was
#'   incomplete, so that the three token fields are lower bounds when it is
#'   positive; `NA` when that number cannot be determined, as after a call that
#'   raised an error.
#' @field records List. The record of the most recent session: `calls`, one
#'   entry per model call the session made; `routing`, one entry per speaker
#'   selection; `probes` and `injections`, described in [run_focus_group()];
#'   and `info`, the versions, standing rules, and settings in force when the
#'   session started. [run_focus_group()] returns the first four as data
#'   frames. Objects saved by FocusGroup 0.5.3 and earlier lack it.
#'
#' @details
#' The group's token fields count every model call made on its behalf: the
#' moderator's and participants' turns with their retries, desire scores,
#' interim and final summaries, and model analyses run later, such as
#' `analyze_themes()`. [run_focus_group()] describes how unreported usage is
#' handled. A transcript imported with [focus_group_from_transcript()] adds
#' no calls. Objects saved by FocusGroup 0.5.2 and earlier lack
#' `total_tokens` and `calls_usage_unknown`.
#'
#' A session run by [run_focus_group()] carries a `session_status` attribute,
#' a list with `complete` and `failure` as in that function's `metadata`.
#'
#' Every prompt shows the agents under their handles, never under their IDs:
#' the participants' turns, the moderator's lines, desire scores, probes, and
#' the interim summary that stands in for messages the prompt window no
#' longer holds. The researcher's records, the log and the routing record
#' among them, keep the IDs. Each participant's system message names its
#' handle ("In this discussion you are P3.").
#'
#' The moderator calls on silent participants at the guide items given by
#' `call_on_silent`. When the discussion of such an item ends, each
#' participant who has not spoken since the previous call-on point (or the
#' start of the session) is asked, in roster order, by the line in
#' `call_on_text`, delivered as written with no model call; the participant
#' then answers, without a desire score, and the routing record gives the
#' turn the rule `"called_on"`. With the default guide of
#' [run_focus_group()] the points are items 2, 4, and 7. Krueger (2002)
#' describes the opening question of a focus group as a round robin; the
#' first point gives every participant a turn early in the session.
#'
#' A moderator line whose last question opens or closes with one
#' participant's handle ("P3, do you agree?", "P3: what do you think?",
#' "Could you say more, P3?"), or is a short follow-up naming one ("And
#' P3?", "What about P3?", "How about P3?"), and does not also ask the
#' group, gives that participant the next turn, with the rule `"addressed"`.
#'
#' @references Krueger, R. A. (2002). *Designing and Conducting Focus Group
#'   Interviews*. University of Minnesota.
#'
#' @return An R6 generator. `FocusGroup$new()` returns a session object;
#'   most analyses run through [run_focus_group()], which builds, runs, and
#'   returns one. Construction alone makes no model call.
#' @examples
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                         api_key = LLMR::llm_api_key_env("GROQ_API_KEY"))
#' agents <- create_agents(
#'   n_participants = 2, config = cfg,
#'   direct_persona_descriptions = c("A retired teacher.",
#'                                   "A night-shift nurse."))
#' flow <- create_conversation_flow("round_robin", agents, "MOD")
#' fg <- FocusGroup$new(topic = "Neighborhood safety",
#'                      purpose = "Pilot the moderator guide.",
#'                      agents = agents,
#'                      moderator_id = "MOD",
#'                      turn_taking_flow = flow)
#' fg$topic
#' @export
FocusGroup <- R6::R6Class("FocusGroup",
  public = list(
    topic = NULL,
    purpose = NULL,
    agents = list(),
    moderator_id = NULL,
    conversation_log = list(),
    turn_taking_flow = NULL,
    prompt_templates = list(),
    question_script = list(),
    current_question_text = NULL,
    current_conversation_summary = NULL,
    final_summary = NULL,
    message_mode = NULL,
    admin_config = NULL,
    max_tokens_utterance = 160,
    max_tokens_moderator = 400,
    max_tokens_desire = 16,
    max_tokens_desire_retry = 256L,
    max_tokens_summary = 400,
    max_tokens_interim_summary = 250L,
    max_tokens_probe = NULL,
    reasoning_effort = NULL,
    max_participant_responses = 2L,
    total_tokens_sent = 0,
    total_tokens_received = 0,
    total_tokens = 0,
    calls_usage_unknown = 0,
    records = NULL,
    handles = NULL,
    call_on_silent = NULL,
    call_on_text = NULL,

    #' @description Initialize a new FocusGroup simulation.
    #' @param topic Character. The main discussion topic.
    #' @param purpose Character. The primary purpose of the focus group.
    #' @param agents Named list of initialized `FGAgent` objects, each named
    #'   by its `id`.
    #' @param moderator_id Character. The ID of the agent acting as moderator.
    #' @param turn_taking_flow An initialized `ConversationFlow` object.
    #' @param question_script List. Moderator's script defining phases and questions.
    #'        If empty, a minimal default script (opening, generic discussion, closing) is used.
    #'        An item with `verbatim = TRUE` is delivered as its `text` reads,
    #'        with no model call; it must have a text.
    #' @param prompt_templates List. Custom prompt templates. Defaults are used if not provided.
    #' @param admin_config An `llm_config` object for administrative model tasks.
    #'   If `NULL`, the moderator's explicitly supplied `config` is used.
    #' @param max_tokens_config List. Optional. Named list with `utterance`,
    #'   `moderator`, `desire`, `desire_retry`, `summary`, `interim_summary`,
    #'   and `probe` to override the default max token limits (160, 400, 16,
    #'   256, 400, 250, and the configuration's own limit or 300) for these
    #'   call types. A model that reasons
    #'   before answering needs `desire` and `desire_retry` large enough to hold
    #'   its reasoning and the score.
    #' @param max_participant_responses Integer. Optional. Maximum participant exchanges per round before moderator intervention.
    #'        Defaults to `getOption("focusgroup.max_participant_responses", 2L)`.
    #'        It must be one positive whole number.
    #' @param reasoning_effort Optional reasoning-effort setting, as described
    #'   for the `reasoning_effort` field.
    #' @param handles The labels under which the agents see each other.
    #'   `NULL` (the default) shows the participants as `"P1"`, `"P2"`, ...
    #'   in roster order and the moderator as `"Moderator"`, so the IDs given
    #'   to the agents never reach a prompt; a participant whose ID is already
    #'   `"P1"` to `"P99"` keeps it, and the others take the numbers that are
    #'   no agent's ID, the moderator's included, so that a neutral handle
    #'   never names a different agent's ID. `"ids"` shows the agent IDs, with
    #'   a message listing them. A character vector named by agent ID sets
    #'   handles of your own, such as invented first names, and the agents it
    #'   does not name keep their neutral handles. A handle must be non-empty
    #'   and distinct, cannot be `"System"` or the name of an injected voice,
    #'   and cannot contain a standalone number from 0 to 10, which a desire
    #'   score could be mistaken for. No participant's ID may be the
    #'   moderator's handle (in any case), which would make the two
    #'   indistinguishable.
    #' @param call_on_silent Where the moderator calls on the participants
    #'   who have not spoken since the previous such point: any of
    #'   `"opening"` (the first discussion item), `"midpoint"` (discussion
    #'   item `ceiling(D / 2)` of `D`), and `"end"` (the last discussion
    #'   item), or guide item numbers, which count every item, as
    #'   [fg_after_item()] does, and must be items participants answer.
    #'   `TRUE` gives the three named points; `"none"`, `FALSE`, or `NULL`
    #'   turns call-ons off. Points falling on the same item count once.
    #' @param call_on_text The call-on line, delivered as written with no
    #'   model call. It must contain `{participant}`, which is replaced by the
    #'   participant's handle. `NULL` uses "\{participant\}, I'd like to hear
    #'   from you as well. What are your thoughts?".
    initialize = function(topic, purpose, agents, moderator_id, turn_taking_flow,
                          question_script = list(), prompt_templates = list(),
                          admin_config = NULL, max_tokens_config = list(),
                          max_participant_responses = NULL,
                          reasoning_effort = NULL, handles = NULL,
                          call_on_silent = c("opening", "midpoint", "end"),
                          call_on_text = NULL) {
      # Validations
      if (!is.character(topic) || length(topic) != 1 || nchar(topic) == 0) stop("'topic' must be a non-empty character string.")
      if (!is.character(purpose) || length(purpose) != 1 || nchar(purpose) == 0) stop("'purpose' must be a non-empty character string.")
      if (!is.list(agents) || length(agents) < 1 || !all(sapply(agents, inherits, "FGAgent"))) stop("'agents' must be a list of at least one FGAgent object.")
      agent_ids <- vapply(agents, function(a) as.character(a$id), character(1),
                          USE.NAMES = FALSE)
      if (is.null(names(agents)) || !identical(names(agents), agent_ids)) {
        stop("Each element of `agents` must be named by its agent's `id`; ",
             "found names ", paste(names(agents) %||% "(none)", collapse = ", "),
             " for IDs ", paste(agent_ids, collapse = ", "), ".", call. = FALSE)
      }
      if (!is.character(moderator_id) || !(moderator_id %in% names(agents))) stop("'moderator_id' must be a valid ID of an agent in 'agents'.")
      if (!inherits(turn_taking_flow, "ConversationFlow")) stop("'turn_taking_flow' must be a ConversationFlow object.")
      if (!is.list(question_script)) stop("'question_script' must be a list.")
      response_limit <- max_participant_responses %||%
        getOption("focusgroup.max_participant_responses", 2L)
      if (!is.numeric(response_limit) || length(response_limit) != 1L ||
          is.na(response_limit) || !is.finite(response_limit) || response_limit < 1 ||
          response_limit != floor(response_limit) ||
          response_limit > .Machine$integer.max) {
        stop("`max_participant_responses` (or the option ",
             "`focusgroup.max_participant_responses`) must be one positive whole number.",
             call. = FALSE)
      }
      self$handles <- .fg_resolve_handles(handles, agent_ids, moderator_id)
      self$call_on_text <- .fg_check_call_on_text(call_on_text)
      if (identical(handles, "ids")) {
        rlang::inform(paste0("The agents will see each other under their IDs: ",
                             paste(agent_ids, collapse = ", "), "."),
                      class = "focusgroup_handles_ids")
      }

      self$topic <- topic
      self$purpose <- purpose
      self$agents <- agents
      self$moderator_id <- moderator_id
      if (!is.null(self$agents[[moderator_id]])) {
        self$agents[[moderator_id]]$is_moderator <- TRUE
        self$agents[[moderator_id]]$role <- "moderator"
      }
      # An agent saved by FocusGroup 0.5.4 or earlier has no handle field and
      # matches its own turns by ID, so it keeps its ID as its handle, and
      # the other agents see that ID.
      legacy <- agent_ids[!vapply(self$agents, function(a) "handle" %in% names(a),
                                  logical(1))]
      legacy <- legacy[self$handles[legacy] != legacy]
      if (length(legacy)) {
        self$handles[legacy] <- legacy
        shared <- self$handles[duplicated(tolower(self$handles)) |
                                 duplicated(tolower(self$handles), fromLast = TRUE)]
        if (length(shared)) {
          stop("Agent(s) ", paste(legacy, collapse = ", "), " come from an earlier ",
               "version of FocusGroup and keep their IDs as handles, which then ",
               "would be shared: ", paste(unique(shared), collapse = ", "), ".",
               call. = FALSE)
        }
        warning("Agent(s) ", paste(legacy, collapse = ", "), " come from an earlier ",
                "version of FocusGroup and recognize their own turns by ID, so the ",
                "other agents see them under their IDs. Build them anew to use ",
                "handles.", call. = FALSE)
      }
      # A participant whose ID is the moderator's handle could not be told
      # from the moderator in an injection's `from` or in the records.
      moderator_handle <- unname(self$handles[[moderator_id]])
      clash <- setdiff(agent_ids, moderator_id)[
        tolower(setdiff(agent_ids, moderator_id)) == tolower(moderator_handle)]
      if (length(clash)) {
        stop("Participant ID '", clash[[1]], "' is the moderator's handle ('",
             moderator_handle, "'), so the participant could not be told from the ",
             "moderator. Give the participant another ID, or the moderator another ",
             "handle, as in `handles = c(", moderator_id, " = \"Host\")`.", call. = FALSE)
      }
      private$claim_handles()
      self$turn_taking_flow <- turn_taking_flow
      self$conversation_log <- list()
      self$final_summary <- NULL

      if (length(question_script) == 0) {
        message("No question_script provided. Using a minimal default script (opening, generic discussion, closing).")
        self$question_script <- list(
            list(phase = "opening"),
            list(phase = "generic_discussion", text = self$topic),
            list(phase = "closing")
        )
      } else {
        self$question_script <- question_script
      }
      for (i in seq_along(self$question_script)) {
        item <- self$question_script[[i]]
        if (isTRUE(item$verbatim) &&
            !(is.character(item$text) && length(item$text) == 1L &&
              !is.na(item$text) && nzchar(trimws(item$text)))) {
          stop("Guide item ", i, " is marked verbatim but has no text to deliver.",
               call. = FALSE)
        }
      }
      self$call_on_silent <- .fg_call_on_points(
        call_on_silent,
        vapply(self$question_script, function(item) as.character(item$phase %||% ""),
               character(1)))
      private$current_phase_index <- 0

      default_prompts_list <- get_default_prompt_templates()
      self$prompt_templates <- utils::modifyList(default_prompts_list, prompt_templates %||% list())

      self$admin_config <- admin_config %||% self$agents[[moderator_id]]$config
      if (!is.null(self$admin_config) &&
          !inherits(self$admin_config, "llm_config")) {
        stop("`admin_config` must be `NULL` or an `llm_config` object.",
             call. = FALSE)
      }

      self$max_tokens_utterance <- max_tokens_config$utterance %||% self$max_tokens_utterance
      self$max_tokens_moderator <- max_tokens_config$moderator %||% self$max_tokens_moderator
      self$max_tokens_desire <- max_tokens_config$desire %||% self$max_tokens_desire
      self$max_tokens_desire_retry <- max_tokens_config$desire_retry %||% self$max_tokens_desire_retry
      self$max_tokens_summary <- max_tokens_config$summary %||% self$max_tokens_summary
      self$max_tokens_interim_summary <- max_tokens_config$interim_summary %||%
        self$max_tokens_interim_summary
      self$max_tokens_probe <- max_tokens_config$probe
      unknown_caps <- setdiff(names(max_tokens_config %||% list()),
                              c("utterance", "moderator", "desire", "desire_retry",
                                "summary", "interim_summary", "probe"))
      if (length(unknown_caps)) {
        stop("Unknown `max_tokens_config` element(s): ",
             paste(unknown_caps, collapse = ", "), ".", call. = FALSE)
      }
      for (cap in names(max_tokens_config %||% list())) {
        value <- max_tokens_config[[cap]]
        if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
            !is.finite(value) || value < 1 || value != floor(value)) {
          stop("`max_tokens_config$", cap, "` must be one positive whole number.",
               call. = FALSE)
        }
      }
      self$reasoning_effort <- .fg_validate_reasoning_effort(reasoning_effort)
      self$max_participant_responses <- as.integer(response_limit)

      self$total_tokens_sent <- 0
      self$total_tokens_received <- 0
      self$total_tokens <- 0
      self$calls_usage_unknown <- 0
      self$records <- .fg_empty_records()
    },

    #' @description Run the full focus group simulation.
    #' Iterates through the `question_script` phases or a specified number of rounds.
    #' @param num_rounds Integer. Optional. Maximum number of moderator cycles to run. If `NULL` (default),
    #'   the simulation runs until the `question_script` is exhausted or the moderator
    #'   decides to end. If both `num_rounds` and `question_script` are provided,
    #'   the simulation stops at whichever condition is met first.
    #' @param verbose Logical. If `TRUE`, prints progress and utterances to the console.
    #' @param injections Optional [fg_injection()] or list of injections to
    #'   place into the session at their points.
    #' @param probes Optional [fg_probe()] or list of probes to ask at their
    #'   points; the answers go to `records$probes`, never to the log.
    #' @param final_summary Logical. `TRUE` (the default) ends the session
    #'   with a model-written summary in `final_summary`; `FALSE` makes no
    #'   summary call and leaves `final_summary` `NULL`.
    #' @return Invisibly returns the `conversation_log`. When a call fails,
    #'   the error propagates and the messages completed so far remain in
    #'   `conversation_log`. A failed final summary is not an error: it warns
    #'   and leaves `final_summary` as `NA`. Every model call the session
    #'   makes, and every speaker selection, is recorded in `records`. An
    #'   injection or probe whose point the session never reaches is reported
    #'   with a warning.
    run_simulation = function(num_rounds = NULL, verbose = FALSE,
                              injections = NULL, probes = NULL,
                              final_summary = TRUE) {
      private$check_unused()
      .fg_check_flag(final_summary, "final_summary")
      private$prepare_plan(.fg_injection_list(injections), .fg_probe_list(probes))
      # Record the active message construction so a saved object replays the same
      # way (e.g. the GUI continuation experiment).
      self$message_mode <- .fg_message_mode()
      private$start_records(final_summary)
      private$with_records(private$run_session(num_rounds, verbose))
      invisible(self$conversation_log)
    },

    #' @description Run the session up to a branch point once, then continue
    #'   a copy of it once per arm. [run_focus_group_arms()] describes the
    #'   design and builds results from the objects returned here.
    #' @param arms A named list of arms, each `NULL`, an [fg_injection()], or
    #'   a list of injections; the first injection of every arm that has one
    #'   must name the same point.
    #' @param probes Optional [fg_probe()] or list of probes.
    #' @param num_rounds Integer. Optional maximum number of moderator cycles.
    #' @param verbose Logical. Print progress.
    #' @param final_summary Logical. Whether each arm ends with a final
    #'   summary, as in `run_simulation()`.
    #' @return A list with `prefix`, this object, stopped at the branch point,
    #'   and `arms`, a named list of `FocusGroup` objects, each a copy of the
    #'   prefix continued to the end of the session under one arm. A failure
    #'   in an arm raises an error of class `focusgroup_branch_failure` whose
    #'   fields `arm`, `branch` (the failed arm's object), `completed`,
    #'   `phase`, and `parent` describe it. A session that ends before the
    #'   branch point raises an error of class
    #'   `focusgroup_injection_unreached`.
    run_arms = function(arms, probes = NULL, num_rounds = NULL, verbose = FALSE,
                        final_summary = TRUE) {
      private$check_unused()
      .fg_check_flag(final_summary, "final_summary")
      arms <- .fg_arm_list(arms)
      firsts <- Filter(Negate(is.null), lapply(arms, function(a) if (length(a)) a[[1]]))
      events <- unique(vapply(firsts, function(inj) .fg_point_event(inj$at), character(1)))
      if (length(events) != 1L) {
        stop("The first injection of every arm must name the same point; found ",
             paste(vapply(events, .fg_event_label, character(1)), collapse = ", "), ".",
             call. = FALSE)
      }
      branch_event <- events[[1]]
      # Each arm's injections are checked against the guide before any call.
      for (name in names(arms)) {
        private$prepare_plan(lapply(arms[[name]], function(inj) {
          inj$label <- inj$label %||% name
          inj
        }), .fg_probe_list(probes))
      }
      private$prepare_plan(list(), .fg_probe_list(probes), pause_at = branch_event)

      self$message_mode <- .fg_message_mode()
      private$start_records(final_summary)
      status <- private$with_records({
        private$session_begin(num_rounds, verbose)
        private$session_continue(verbose)
      })
      if (!identical(status, "paused")) {
        rlang::abort(sprintf("The session ended before the branch point %s; no arm was run.",
                             .fg_event_label(branch_event)),
                     class = "focusgroup_injection_unreached")
      }
      self$records$info$branch <- list(
        point = .fg_event_label(branch_event),
        prefix_messages = length(self$conversation_log),
        prefix_last_message_id = as.integer(
          self$conversation_log[[length(self$conversation_log)]]$message_id),
        prefix_calls = length(self$records$calls),
        prefix_hash = .fg_prefix_hash(self$conversation_log))
      if (verbose) {
        cat("
--- Branch point ", .fg_event_label(branch_event), " reached after ",
            length(self$conversation_log), " message(s) ---
", sep = "")
      }
      private$branch_out(arms, probes, branch_event, verbose)
    },


    #' @description Generate a summary of the conversation using an LLM.
    #' @param config An explicit `llm_config` object for the summarization model.
    #' @param summary_level Integer (1-3). 1: Prose overview, 2: Detailed bulleted, 3: Short bulleted takeaways.
    #' @param max_tokens Integer. Optional. Max tokens for the summary.
    #' @param internal_call Logical. Marks a summary made for context
    #'   management during the session. It does not change the call: the
    #'   call's usage is added to the group's token fields either way, and to
    #'   no agent's. A session's call record lists such a summary as
    #'   `"interim_summary"`.
    #' @param transcript_override Character. Optional. If provided, this transcript is summarized instead of `self$conversation_log`.
    #' @param .runner Optional function used instead of live model calls. It
    #'   receives a data frame with `config` and `messages` list-columns and
    #'   returns the rows with at least `response_text`. `NULL` makes a live
    #'   call through `config`, also on a session that ran with a runner.
    #' @return Character string containing the generated summary. The
    #'   transcript made from the log shows each speaker under its handle,
    #'   with the agent ID beside it when the two differ, and leaves out the
    #'   roster row; such a summary is for the researcher. The interim summary
    #'   the agents read is made from a `transcript_override` that shows
    #'   handles only. A log or transcript with nothing to summarize is an
    #'   error, as is an empty reply; its usage is recorded first.
    summarize = function(config, summary_level = 1, max_tokens = NULL,
                         internal_call = FALSE, transcript_override = NULL,
                         .runner = NULL) {
      if (!requireNamespace("LLMR", quietly = TRUE)) stop("LLMR package is required.")

      if (missing(config) || is.null(config)) {
        stop("`config` is required for summarization.", call. = FALSE)
      }
      if (!inherits(config, "llm_config")) {
        stop("`config` must be an `llm_config` object.", call. = FALSE)
      }
      if (!summary_level %in% 1:3) stop("'summary_level' must be 1, 2, or 3.")

      transcript_to_use <- transcript_override
      if (is.null(transcript_to_use)) {
          # Labeled by handle, the label the speakers' own text refers to,
          # with the agent ID beside it when they differ. This transcript is
          # for the researcher; the interim summary the agents read is
          # given as `transcript_override` and shows handles only.
          shown <- Filter(function(msg) !identical(msg$speaker_id, "System"),
                          self$conversation_log)
          if (!length(shown)) {
            stop("The conversation log has no messages to summarize.", call. = FALSE)
          }
          transcript_to_use <- paste(vapply(shown, function(msg) {
            handle <- private$handle_of(msg$speaker_id)
            id <- if (!identical(handle, msg$speaker_id)) {
              paste0("ID ", msg$speaker_id, ", ")
            } else ""
            paste0(handle, " (", id, "Message ", msg$message_id,
                   ", Round ", msg$round, ", Phase: ", msg$phase, "): ",
                   msg$text)
          }, character(1)), collapse = "\n\n")
      }
      if (!is.character(transcript_to_use) || length(transcript_to_use) != 1L ||
          is.na(transcript_to_use) || !nzchar(trimws(transcript_to_use))) {
          stop("The transcript to summarize is empty.", call. = FALSE)
      }


      topic_context <- paste0("The main topic of the discussion was: ", self$topic, "\n",
                              "The purpose of the focus group was: ", self$purpose, "\n\n")

      level_instructions <- ""
      estimated_tokens <- NULL # For LLM call

      if (summary_level == 1) {
        level_instructions <- "Provide a concise prose summary (approx. 200-300 words) of the focus group discussion. Focus on main themes, overall sentiment, key agreements/disagreements, and broad conclusions."
        estimated_tokens <- max_tokens %||% 400
      } else if (summary_level == 2) {
        level_instructions <- "Analyze the focus group transcript and provide a detailed, bulleted summary. Highlight key quotes (attributed), anecdotes, new arguments, surprising statements, and points of agreement/disagreement. Organize logically."
        estimated_tokens <- max_tokens %||% 1000
      } else if (summary_level == 3) { # Short, for context window or quick overview
        level_instructions <- "Provide a very short, bulleted overall summary (approx. 3-5 key bullet points) of the most critical takeaways. Focus on impactful points, arguments, and surprising insights. Keep it extremely concise."
        estimated_tokens <- max_tokens %||% 150
      }

      full_prompt <- paste0(
        topic_context,
        level_instructions,
        "\n\nHere is the transcript (or relevant portion):\n--------------------\n",
        transcript_to_use,
        "\n--------------------\nSummary:"
      )

      current_call_config <- config
      if (!is.null(estimated_tokens)) {
        current_call_config$model_params$max_tokens <- estimated_tokens
      }

      response_obj <- .fg_call_llm(
        config = current_call_config,
        .usage = function(u) .fg_record_group(self, u),
        messages = list(list(role = "user", content = full_prompt)),
        .runner = .runner,
        tries = 5,
        wait_seconds = 2,
        backoff_factor = 3,
        .call = list(type = if (isTRUE(internal_call)) "interim_summary" else "summary")
      )
      summary_text <- .fg_response_text(response_obj)
      .fg_note_call(response_obj, cleaned_text = summary_text)

      if (!nzchar(trimws(summary_text))) {
        stop("Summarization returned an empty model response.", call. = FALSE)
      }
      return(summary_text)
    },

    # --- Analysis Methods ---
    #' @description Basic analysis of the conversation log.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #'   If `NULL`, all messages are analyzed.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze. If `NULL`, analyzes all speakers.
    #' @return A list with `speaker_stats` (a tibble: speaker_id, utterance_count, total_words, avg_words_per_utterance)
    #'         and `full_transcript` (character string).
    analyze = function(message_ids = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)

      if (length(filtered_log) == 0) {
        warning("Filtered conversation log is empty for basic analysis.")
        # Return empty structure consistent with non-empty case
        all_agent_ids <- names(self$agents)
        speaker_stats_df <- dplyr::tibble(
          speaker_id = all_agent_ids,
          utterance_count = 0L,
          total_words = 0L,
          avg_words_per_utterance = 0.0
        )
        return(list(speaker_stats = speaker_stats_df, full_transcript = "Filtered conversation log is empty."))
      }

      log_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
        dplyr::tibble(
          speaker_id = x$speaker_id %||% NA_character_,
          text = x$text %||% ""
        )
      }))

      speaker_stats_df <- log_df %>%
        dplyr::mutate(word_count = .fg_word_count(.data$text)) %>%
        dplyr::group_by(.data$speaker_id) %>%
        dplyr::summarise(
          utterance_count = dplyr::n(),
          total_words = sum(.data$word_count, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(avg_words_per_utterance = ifelse(.data$utterance_count > 0, .data$total_words / .data$utterance_count, 0.0))

      # Ensure all agents are in the stats, even if they didn't speak in the filtered log
      all_agent_ids <- names(self$agents)
      current_speakers_in_stats <- unique(speaker_stats_df$speaker_id)
      missing_agents <- setdiff(all_agent_ids, current_speakers_in_stats)

      if (length(missing_agents) > 0) {
        missing_df <- dplyr::tibble(
          speaker_id = missing_agents, utterance_count = 0L, total_words = 0L, avg_words_per_utterance = 0.0
        )
        speaker_stats_df <- dplyr::bind_rows(speaker_stats_df, missing_df)
      }

      # Order by original agent order if possible, or alphabetically. Keep any
      # speaker that is not an agent (e.g. an explicitly requested "System"
      # row) as a trailing level rather than silently turning it into NA.
      ordered_agent_ids <- intersect(all_agent_ids, speaker_stats_df$speaker_id)
      if(length(ordered_agent_ids) > 0) {
          all_levels <- union(all_agent_ids, unique(as.character(speaker_stats_df$speaker_id)))
          speaker_stats_df <- speaker_stats_df %>%
            dplyr::mutate(speaker_id = factor(.data$speaker_id, levels = all_levels)) %>%
            dplyr::arrange(.data$speaker_id)
      }


      full_transcript_text <- paste(sapply(filtered_log, function(msg) {
        paste0(msg$speaker_id, " (Message ", msg$message_id,
               ", Round ", msg$round, ", Phase: ", msg$phase, "): ",
               msg$text)
      }), collapse = "\n\n")

      return(list(speaker_stats = speaker_stats_df, full_transcript = full_transcript_text))
    },

    #' @description Perform LDA topic modeling on the conversation.
    #' @param num_topics Integer. Number of topics to identify.
    #' @param min_doc_length Integer. Minimum words for a text to count as a
    #'   document.
    #' @param unit Character. What counts as a document: `"message"`
    #'   (default) treats each utterance as a document; `"speaker"` pools
    #'   every turn by the same speaker,
    #'   which leaves as many documents as there are speakers and tends to
    #'   recover who spoke rather than what was discussed.
    #' @param top_n_terms Integer. Number of top terms per topic to return.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @param seed Integer or NULL. Seed for the LDA control (default 110);
    #'   `NULL` leaves it unset.
    #' @param ... Additional arguments to `topicmodels::LDA()`.
    #' @return A list with LDA model, topic terms, and document-topic proportions. `NULL` on failure.
    analyze_topics = function(num_topics = 5, min_doc_length = 20,
                              top_n_terms = 10, message_ids = NULL,
                              speaker_ids = NULL, seed = 110,
                              unit = c("message", "speaker"), ...) {
      unit <- match.arg(unit)
      if (!all(sapply(c("topicmodels", "tidytext", "dplyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages topicmodels, tidytext, and dplyr are required for LDA. Please install them.")
      }

      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
          warning("Filtered log is empty for topic analysis.")
          return(NULL)
      }

      tryCatch({
        # Extract texts and speaker IDs
        texts <- sapply(filtered_log, function(x) x$text)
        speaker_ids_vec <- sapply(filtered_log, function(x) x$speaker_id)

        speaker_texts <- .fg_topic_documents(texts, speaker_ids_vec, unit)

        # Filter documents by minimum length
        word_counts <- .fg_word_count(speaker_texts)
        valid_docs <- names(speaker_texts)[word_counts >= min_doc_length]

        if (length(valid_docs) < 2) {
          warning("Not enough valid documents for topic modeling (minimum 2 required)")
          return(NULL)
        }

        if (length(valid_docs) < num_topics) {
          warning("Number of valid documents is less than num_topics. Reducing num_topics.")
          num_topics <- length(valid_docs)
        }
        if (length(valid_docs) < 3L * num_topics) {
          warning(sprintf(paste(
            "Fitting %d topic(s) on %d document(s); estimates from so few",
            "documents are unstable. Lower num_topics or min_doc_length, or",
            "read the term lists as description only."),
            num_topics, length(valid_docs)), call. = FALSE)
        }

        # Create document-term matrix using tidytext
        valid_texts <- speaker_texts[valid_docs]

        # Convert to tidy format and create DTM
        tidy_docs <- dplyr::tibble(
          document = names(valid_texts),
          text = valid_texts
        ) %>%
          tidytext::unnest_tokens(.data$word, .data$text) %>%
          dplyr::anti_join(tidytext::stop_words, by = "word") %>%
          dplyr::filter(nchar(.data$word) > 2) %>%
          dplyr::count(.data$document, .data$word, sort = TRUE)

        # Cast to DTM
        dtm <- tidy_docs %>%
          tidytext::cast_dtm(.data$document, .data$word, .data$n)

        # Check if DTM has sufficient terms
        if (ncol(dtm) < 5) {
          warning("Not enough unique terms for meaningful topic modeling")
          return(NULL)
        }

        # Run LDA
        lda_control <- if (is.null(seed)) list() else list(seed = as.integer(seed))
        lda_model <- topicmodels::LDA(dtm, k = num_topics, control = lda_control, ...)

        # Extract topic-term probabilities (beta)
        topics_beta <- tidytext::tidy(lda_model, matrix = "beta")

        # Get top terms for each topic
        top_terms <- topics_beta %>%
          dplyr::group_by(.data$topic) %>%
          dplyr::slice_max(.data$beta, n = top_n_terms, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::arrange(.data$topic, dplyr::desc(.data$beta))

        # Extract document-topic probabilities (gamma)
        topics_gamma <- tidytext::tidy(lda_model, matrix = "gamma")

        # Create topic labels based on top terms
        topic_labels <- top_terms %>%
          dplyr::group_by(.data$topic) %>%
          dplyr::slice_head(n = 3) %>%
          dplyr::summarise(label = paste(.data$term, collapse = ", "), .groups = 'drop')

        return(list(
          lda_model = lda_model,
          top_terms = top_terms,
          document_topics = topics_gamma,
          topic_labels = topic_labels,
          dtm = dtm,
          num_documents = length(valid_docs),
          num_topics = num_topics,
          unit = unit
        ))

      }, error = function(e) {
        warning(paste("Topic analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Calculate TF-IDF scores for terms per participant.
    #' @param top_n_terms Integer. Number of top TF-IDF terms per participant.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @param ... Additional arguments to `tidytext::unnest_tokens`.
    #' @return A tibble with TF-IDF scores.
    analyze_tfidf = function(top_n_terms = 10, message_ids = NULL,
                             speaker_ids = NULL, ...) {
      if (!all(sapply(c("tidytext", "dplyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages tidytext and dplyr are required for TF-IDF. Please install them.")
      }
      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
          warning("Filtered log is empty for TF-IDF analysis.")
          return(dplyr::tibble())
      }

      tryCatch({
        # Extract texts and speaker IDs
        texts <- sapply(filtered_log, function(x) x$text)
        speaker_ids_vec <- sapply(filtered_log, function(x) x$speaker_id)

        # Create a data frame for analysis
        text_df <- dplyr::tibble(
          speaker_id = speaker_ids_vec,
          text = texts
        )

        # Aggregate by speaker to create documents
        speaker_docs <- text_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(document = paste(.data$text, collapse = " "), .groups = 'drop')

        # Tokenize and calculate TF-IDF using tidytext approach
        tfidf_results <- speaker_docs %>%
          tidytext::unnest_tokens(.data$word, .data$document, ...) %>%
          dplyr::anti_join(tidytext::stop_words, by = "word") %>%
          dplyr::filter(nchar(.data$word) > 2) %>%
          dplyr::count(.data$speaker_id, .data$word, sort = TRUE) %>%
          tidytext::bind_tf_idf(.data$word, .data$speaker_id, .data$n) %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::slice_max(.data$tf_idf, n = top_n_terms, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            speaker_id = .data$speaker_id,
            term = .data$word,
            tf_idf = .data$tf_idf
          ) %>%
          dplyr::arrange(.data$speaker_id, dplyr::desc(.data$tf_idf))

        return(tfidf_results)
      }, error = function(e) {
        warning(paste("TF-IDF analysis failed:", e$message))
        return(dplyr::tibble())
      })
    },

    #' @description Calculate readability scores for each participant's aggregated text.
    #' @param measures Character vector. Readability measure(s) from `quanteda.textstats::textstat_readability`.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A tibble with readability scores.
    analyze_readability = function(measures = "Flesch", message_ids = NULL,
                                   speaker_ids = NULL) {
      if (!all(sapply(c("quanteda", "quanteda.textstats", "dplyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages quanteda, quanteda.textstats, dplyr are required for readability. Please install them.")
      }
      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
          warning("Filtered log is empty for readability analysis.")
          return(dplyr::tibble())
      }

      tryCatch({
        # Extract texts and speaker IDs
        texts <- sapply(filtered_log, function(x) x$text)
        speaker_ids_vec <- sapply(filtered_log, function(x) x$speaker_id)

        # Aggregate by speaker
        speaker_texts <- tapply(texts, speaker_ids_vec, paste, collapse = " ")

        # Filter out speakers with very short texts (less than 10 words)
        speaker_word_counts <- .fg_word_count(speaker_texts)
        valid_speakers <- names(speaker_texts)[speaker_word_counts >= 10]

        if (length(valid_speakers) == 0) {
          warning("No speakers have sufficient text for readability analysis (minimum 10 words required)")
          return(dplyr::tibble())
        }

        # Create corpus for valid speakers only
        valid_texts <- speaker_texts[valid_speakers]
        corpus <- quanteda::corpus(valid_texts)
        quanteda::docnames(corpus) <- valid_speakers

        # Calculate readability measures
        readability_stats <- quanteda.textstats::textstat_readability(corpus, measure = measures)

        # Convert to tibble and add speaker_id column. textstat_readability()
        # returns the corpus docnames (the speaker ids) in `document`; its
        # rownames are just "1","2",... and must not be used as ids.
        result <- dplyr::as_tibble(readability_stats)
        result$speaker_id <- as.character(result$document)
        result$document <- NULL

        # Reorder columns to put speaker_id first
        result <- result %>%
          dplyr::select("speaker_id", dplyr::everything()) %>%
          dplyr::arrange(.data$speaker_id)

        return(result)
      }, error = function(e) {
        warning(paste("Readability analysis failed:", e$message))
        return(dplyr::tibble())
      })
    },

    #' @description Perform LLM-assisted thematic analysis on the transcript.
    #' @param config An explicit `llm_config` object for thematic analysis.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @param .runner Optional function used instead of live model calls. It
    #'   receives a data frame with `config` and `messages` list-columns and
    #'   returns the rows with at least `response_text`. `NULL` makes a live
    #'   call through `config`, also on a session that ran with a runner.
    #' @return The thematic summary as a character string. An empty transcript
    #'   returns `character(0)`. Provider failures are propagated.
    analyze_themes = function(config, message_ids = NULL, speaker_ids = NULL,
                              .runner = NULL) {
      if (missing(config) || is.null(config)) {
        stop("`config` is required for thematic analysis.", call. = FALSE)
      }
      if (!inherits(config, "llm_config")) {
        stop("`config` must be an `llm_config` object.", call. = FALSE)
      }

      analysis_data <- self$analyze(message_ids = message_ids,
                                    speaker_ids = speaker_ids)
      if (nchar(trimws(analysis_data$full_transcript)) == 0 || analysis_data$full_transcript == "Filtered conversation log is empty.") {
          warning("Transcript for thematic analysis is empty.", call. = FALSE)
          return(character(0))
      }

      theme_prompt <- replace_placeholders(
        self$prompt_templates$thematic_analysis_prompt,
        list(
          topic = self$topic,
          focus_group_purpose = self$purpose,
          full_transcript = analysis_data$full_transcript
        )
      )

      current_call_config <- config
      current_call_config$model_params$max_tokens <- 800

      response_obj <- .fg_call_llm(
        config = current_call_config,
        .usage = function(u) .fg_record_group(self, u),
        messages = list(list(role = "user", content = theme_prompt)),
        .runner = .runner,
        tries = 5,
        .call = list(type = "themes")
      )

      themes_summary <- .fg_response_text(response_obj)
      if (!nzchar(trimws(themes_summary))) {
        stop("Thematic analysis returned an empty model response.", call. = FALSE)
      }
      themes_summary
    },

    # analyze_sentiment removed

    #' @description Perform statistical analysis on conversation patterns.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with ANOVA results, phase participation stats, and correlations.
    analyze_statistics = function(message_ids = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for statistical analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format for analysis
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            phase = x$phase,
            text = x$text,
            message_id = x$message_id,
            round = x$round
          )
        }))

        # Calculate basic statistics by speaker and phase
        stats <- conv_df %>%
          dplyr::group_by(.data$speaker_id, .data$phase) %>%
          dplyr::summarise(
            messages = dplyr::n(),
            words = sum(.fg_word_count(.data$text)),
            avg_words = .data$words / .data$messages,
            .groups = "drop"
          )

        # Perform ANOVA on word count by phase if multiple phases exist
        word_aov <- if(length(unique(stats$phase)) > 1) {
          stats::aov(words ~ phase, data = stats)
        } else NULL

        # Calculate phase participation statistics
        phase_stats <- stats %>%
          dplyr::group_by(.data$phase) %>%
          dplyr::summarise(
            mean_messages = mean(.data$messages),
            sd_messages = stats::sd(.data$messages),
            .groups = "drop"
          )

        # Calculate correlation between messages and words
        cor_test <- if(nrow(stats) > 2) {
          stats::cor.test(stats$messages, stats$words)
        } else NULL

        list(
          word_count_anova = if(!is.null(word_aov)) summary(word_aov) else "Not enough phases for ANOVA",
          phase_participation = phase_stats,
          messages_words_correlation = cor_test
        )
      }, error = function(e) {
        warning(paste("Statistical analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Analyze participation balance and dominance patterns.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with participation statistics and balance metrics. Every
    #'   agent in the roster has a row, an agent who did not speak in the
    #'   analyzed messages with zero messages and words and an `NA` average.
    #'   The `called_on` column counts the turns that answered the
    #'   moderator's call-on line.
    analyze_participation_balance = function(message_ids = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for participation balance analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            text = x$text,
            called_on = isTRUE(x$metadata$called_on)
          )
        }))

        # Calculate participation metrics
        participation <- conv_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(
            messages = dplyr::n(),
            words = sum(.fg_word_count(.data$text)),
            avg_words = .data$words / .data$messages,
            called_on = sum(.data$called_on),
            .groups = "drop"
          )

        # An agent who never spoke is part of the balance: it appears with
        # zero messages, so it can be the least active speaker. A
        # `speaker_ids` filter limits the roster as it limits the log.
        roster <- names(self$agents)
        if (!is.null(speaker_ids)) roster <- intersect(roster, speaker_ids)
        silent <- setdiff(roster, participation$speaker_id)
        if (length(silent)) {
          participation <- dplyr::bind_rows(participation, dplyr::tibble(
            speaker_id = silent, messages = 0L, words = 0L,
            avg_words = NA_real_, called_on = 0L)) %>%
            dplyr::arrange(.data$speaker_id)
        }

        # Calculate participation percentages
        total_messages <- sum(participation$messages)
        total_words <- sum(participation$words)

        participation <- participation %>%
          dplyr::mutate(
            message_percentage = (.data$messages / total_messages) * 100,
            word_percentage = (.data$words / total_words) * 100
          ) %>%
          dplyr::relocate("called_on", .after = "word_percentage")

        # Calculate dominance metrics
        max_message_pct <- max(participation$message_percentage)
        min_message_pct <- min(participation$message_percentage)
        message_range <- max_message_pct - min_message_pct

        list(
          participation_stats = participation,
          balance_metrics = list(
            most_active_speaker = participation$speaker_id[which.max(participation$message_percentage)],
            least_active_speaker = participation$speaker_id[which.min(participation$message_percentage)],
            message_percentage_range = message_range
          )
        )
      }, error = function(e) {
        warning(paste("Participation balance analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Analyze response patterns and interaction behaviors.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with response and interaction pattern metrics.
    analyze_response_patterns = function(message_ids = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for response pattern analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            text = x$text,
            message_id = x$message_id
          )
        }))

        # Calculate response patterns by speaker
        response_patterns <- conv_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(
            total_responses = dplyr::n(),
            avg_words = mean(.fg_word_count(.data$text)),
            question_count = sum(grepl("\\?", .data$text)),
            exclamation_count = sum(grepl("!", .data$text)),
            .groups = "drop"
          )

        # Calculate interaction patterns
        first_message <- min(conv_df$message_id)
        last_message <- max(conv_df$message_id)

        interaction_patterns <- conv_df %>%
          dplyr::group_by(.data$speaker_id) %>%
          dplyr::summarise(
            first_to_respond = sum(.data$message_id == first_message),
            last_to_respond = sum(.data$message_id == last_message),
            .groups = "drop"
          )

        list(
          response_metrics = response_patterns,
          interaction_metrics = interaction_patterns
        )
      }, error = function(e) {
        warning(paste("Response pattern analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Analyze question asking patterns during the conversation.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with question pattern analysis.
    analyze_question_patterns = function(message_ids = NULL, speaker_ids = NULL) {
      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for question pattern analysis.")
        return(NULL)
      }

      tryCatch({
        # Convert to data frame format and filter questions
        conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
          dplyr::tibble(
            speaker_id = x$speaker_id,
            phase = x$phase,
            text = x$text
          )
        }))

        # Extract questions and analyze patterns
        questions <- conv_df %>%
          dplyr::filter(grepl("\\?", .data$text)) %>%
          dplyr::group_by(.data$speaker_id, .data$phase) %>%
          dplyr::summarise(
            question_count = dplyr::n(),
            avg_words = mean(.fg_word_count(.data$text)),
            .groups = "drop"
          )

        # Analyze question distribution by phase
        question_dist <- questions %>%
          dplyr::group_by(.data$phase) %>%
          dplyr::summarise(
            total_questions = sum(.data$question_count),
            unique_questioners = dplyr::n_distinct(.data$speaker_id),
            .groups = "drop"
          )

        list(
          question_patterns = questions,
          question_distribution = question_dist
        )
      }, error = function(e) {
        warning(paste("Question pattern analysis failed:", e$message))
        return(NULL)
      })
    },

    #' @description Extract and analyze key phrases using n-grams.
    #' @param min_freq Integer. Minimum frequency for phrases to be considered key.
    #' @param message_ids Integer vector. Optional `message_id` values to analyze.
    #' @param speaker_ids Character vector. Optional. Specific speakers to analyze.
    #' @return A list with bigram and trigram analysis.
    analyze_key_phrases = function(min_freq = 2, message_ids = NULL,
                                   speaker_ids = NULL) {
      # Ensure tidytext and dplyr are available
      if (!all(sapply(c("tidytext", "dplyr", "tidyr"), requireNamespace, quietly = TRUE))) {
        stop("Packages tidytext, dplyr, and tidyr are required for key phrase analysis.")
      }

      filtered_log <- private$get_filtered_log(message_ids, speaker_ids)
      if (length(filtered_log) == 0) {
        warning("Filtered log is empty for key phrase analysis.")
        return(list(bigrams = dplyr::tibble(), trigrams = dplyr::tibble(),
                    total_unique_bigrams = 0, total_unique_trigrams = 0))
      }

      tryCatch({
        # Create a tibble from the conversation log
        conv_tibble <- dplyr::tibble(
          doc_id = seq_along(filtered_log), # Each message as a document for n-gram extraction
          text = sapply(filtered_log, function(x) x$text %||% "")
        )

        # --- Bigram Analysis ---
        bigrams_tokenized <- conv_tibble %>%
          tidytext::unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) %>%
          dplyr::filter(!is.na(bigram)) # Remove NA bigrams that might result from very short texts

        # Split each bigram and remove it when either word is a stop word.
        bigrams_separated <- bigrams_tokenized %>%
          tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE, fill = "right")

        bigrams_cleaned <- bigrams_separated %>%
          dplyr::anti_join(tidytext::stop_words, by = c("word1" = "word")) %>%
          dplyr::anti_join(tidytext::stop_words, by = c("word2" = "word")) %>%
          dplyr::select(doc_id, bigram) # Keep the original bigram column

        total_unique_bigrams <- dplyr::n_distinct(bigrams_cleaned$bigram)

        frequent_bigrams_df <- bigrams_cleaned %>%
          dplyr::count(bigram, sort = TRUE, name = "frequency") %>%
          dplyr::filter(frequency >= min_freq) %>%
          dplyr::rename(feature = bigram) # Match quanteda's output column name

        # --- Trigram Analysis ---
        trigrams_tokenized <- conv_tibble %>%
          tidytext::unnest_tokens(output = trigram, input = text, token = "ngrams", n = 3) %>%
          dplyr::filter(!is.na(trigram))

        trigrams_separated <- trigrams_tokenized %>%
          tidyr::separate(trigram, c("word1", "word2", "word3"), sep = " ", remove = FALSE, fill = "right")

        trigrams_cleaned <- trigrams_separated %>%
          dplyr::anti_join(tidytext::stop_words, by = c("word1" = "word")) %>%
          dplyr::anti_join(tidytext::stop_words, by = c("word2" = "word")) %>%
          dplyr::anti_join(tidytext::stop_words, by = c("word3" = "word")) %>% # Check all three words
          dplyr::select(doc_id, trigram)

        total_unique_trigrams <- dplyr::n_distinct(trigrams_cleaned$trigram)

        frequent_trigrams_df <- trigrams_cleaned %>%
          dplyr::count(trigram, sort = TRUE, name = "frequency") %>%
          dplyr::filter(frequency >= min_freq) %>%
          dplyr::rename(feature = trigram) # Match quanteda's output column name

        list(
          bigrams = frequent_bigrams_df,
          trigrams = frequent_trigrams_df,
          total_unique_bigrams_after_stopwords = total_unique_bigrams,
          total_unique_trigrams_after_stopwords = total_unique_trigrams
        )
      }, error = function(e) {
        warning(paste("Key phrase analysis with tidytext failed:", e$message))
        return(list(bigrams = dplyr::tibble(), trigrams = dplyr::tibble(),
                    total_unique_bigrams_after_stopwords = 0, total_unique_trigrams_after_stopwords = 0))
      })
    },



    #' @description Create a participation timeline of cumulative messages by participant across phases.
    #' @return ggplot object
    plot_participation_timeline = function() {
      private$require_ggplot2()

      # The shared helper drops the synthetic "System" roster row.
      filtered_log <- private$get_filtered_log()
      if (length(filtered_log) == 0) {
        warning("No conversation data available for plotting")
        # Return an empty plot with a message
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Participation Timeline",
                               subtitle = "No conversation data available") +
                 ggplot2::theme_void()) # Using theme_void for a completely empty plot
      }

      # Create a data frame from the conversation log
      # Ensure speaker_id and phase are handled if they could be NULL (though phase defaults to "unknown")
      conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
        dplyr::tibble(
          agent_id = x$speaker_id %||% NA_character_,
          phase = x$phase %||% "unknown"
        )
      }))

      actual_phase_order <- unique(conv_df$phase)
      all_participants <- unique(conv_df$agent_id) # Derived from filtered data

      conv_summary <- conv_df %>%
        dplyr::group_by(.data$agent_id, .data$phase) %>%
        dplyr::summarise(messages_in_phase = dplyr::n(), .groups = "drop") %>%
        dplyr::mutate(phase = factor(.data$phase, levels = actual_phase_order)) %>%
        tidyr::complete(agent_id = all_participants,
                        phase,
                        fill = list(messages_in_phase = 0)) %>%
        dplyr::arrange(.data$agent_id, .data$phase) %>%
        dplyr::group_by(.data$agent_id) %>%
        dplyr::mutate(cumulative_messages = cumsum(.data$messages_in_phase)) %>%
        dplyr::ungroup()


      ggplot2::ggplot(conv_summary, ggplot2::aes(x = .data$phase, y = .data$cumulative_messages, color = .data$agent_id, group = .data$agent_id)) +
        ggplot2::geom_line(linewidth = 1.2) +
        ggplot2::geom_point(size = 2.8) + ggplot2::geom_point(size = 1,color='white') + ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Participation Timeline",
          subtitle = "Cumulative messages by participant across phases",
          x = "Phase",
          y = "Cumulative Messages",
          color = "Participant"
        )
    },




    #' @description Create word count distribution plot showing message length patterns.
    #' @return ggplot object
    plot_word_count_distribution = function() {
      private$require_ggplot2()

      filtered_log <- private$get_filtered_log()
      if (length(filtered_log) == 0) {
        warning("No conversation data available for plotting")
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Word Count Distribution",
                               subtitle = "No conversation data available") +
                 ggplot2::theme_void())
      }

      conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
        dplyr::tibble(
          agent_id = x$speaker_id,
          phase = x$phase,
          message = x$text
        )
      }))

      word_counts <- .fg_word_count(conv_df$message)

      word_data <- data.frame(
        word_count = word_counts,
        agent_id = conv_df$agent_id,
        phase = conv_df$phase
      )

      ggplot2::ggplot(word_data, ggplot2::aes(x = .data$word_count)) +
        ggplot2::geom_histogram(bins = 30, fill = "steelblue", alpha = 0.8) +
        ggplot2::labs(
          title = "Word Count Distribution",
          subtitle = "Distribution of message lengths across all participants",
          x = "Words per Message",
          y = "Count"
        ) +
        ggplot2::facet_wrap(~ .data$phase, scales = "free_y")
    },

    #' @description Create a participation by agent plot showing total messages per participant.
    #' @return ggplot object
    plot_participation_by_agent = function() {
      private$require_ggplot2()

      filtered_log <- private$get_filtered_log()
      if (length(filtered_log) == 0) {
        warning("No conversation data available for plotting")
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Participation by Agent",
                               subtitle = "No conversation data available") +
                 ggplot2::theme_void())
      }

      conv_df <- dplyr::bind_rows(lapply(filtered_log, function(x) {
        dplyr::tibble(
          agent_id = x$speaker_id,
          message = x$text
        )
      }))

      participation <- conv_df %>%
        dplyr::group_by(.data$agent_id) %>%
        dplyr::summarise(
          total_messages = dplyr::n(),
          total_words = sum(.fg_word_count(.data$message)),
          avg_words_per_message = .data$total_words / .data$total_messages,
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(.data$total_messages))

      ggplot2::ggplot(participation, ggplot2::aes(x = stats::reorder(.data$agent_id, .data$total_messages))) +
        ggplot2::geom_bar(ggplot2::aes(y = .data$total_messages), stat = "identity", fill = "steelblue", alpha = 0.8) +
        ggplot2::labs(
          title = "Participation by Agent",
          subtitle = "Total messages by participant",
          x = "Participant",
          y = "Total Messages"
        )
    },

    #' @description Create a timeline showing message length over message order.
    #' @return ggplot object
    plot_message_length_timeline = function() {
      private$require_ggplot2()

      filtered_log <- private$get_filtered_log()
      if (length(filtered_log) == 0) {
        warning("No conversation data available for plotting")
        return(ggplot2::ggplot() +
                 ggplot2::labs(title = "Message Length Timeline",
                               subtitle = "No conversation data available") +
                 ggplot2::theme_void())
      }

      messages <- sapply(filtered_log, function(x) x$text)
      phases <- sapply(filtered_log, function(x) x$phase)

      word_counts <- .fg_word_count(messages)
      timeline_data <- data.frame(
        message_id = vapply(filtered_log, function(x) x$message_id, integer(1)),
        word_count = word_counts,
        phase = phases
      )

      window_size <- min(5, nrow(timeline_data))
      timeline_data$word_count_smooth <- stats::filter(timeline_data$word_count,
        filter = rep(1/window_size, window_size), sides = 2)

      ggplot2::ggplot(timeline_data, ggplot2::aes(x = .data$message_id)) +
        ggplot2::geom_line(ggplot2::aes(y = .data$word_count), alpha = 0.3, color = "gray") +
        ggplot2::geom_line(ggplot2::aes(y = .data$word_count_smooth), color = "blue", linewidth = 1.2) +
        ggplot2::labs(
          title = "Message Length Timeline",
          subtitle = "Evolution of message length throughout the conversation",
          x = "Message ID",
          y = "Words per Message"
        )
    }
  ),

  private = list(
    record_token_usage = function(sent = 0L, rec = 0L, total, unknown) {
      .fg_add_group_usage(self, sent, rec, total, unknown)
    },
    # Start the session record: no calls or picks yet, and the versions,
    # standing rules, and disclosure setting in force.
    start_records = function(final_summary = TRUE) {
      self$records <- .fg_empty_records()
      self$records$info <- list(
        started = as.POSIXct(Sys.time(), tz = "UTC"),
        versions = .fg_versions(),
        message_mode = self$message_mode,
        standing_rules = list(participant = .fg_standing_rules(FALSE),
                              moderator = .fg_standing_rules(TRUE)),
        disclosure = getOption("focusgroup.disclosure", "transparent"),
        rng_kind = RNGkind(),
        final_summary = isTRUE(final_summary)
      )
      private$pending_calls <- integer()
      private$participant_turns <- 0L
      private$current_round <- 0L
    },

    # Evaluate `expr` while collecting the session's call records.
    with_records = function(expr) {
      withCallingHandlers(expr,
        focusgroup_call = function(cnd) private$collect_call(cnd),
        focusgroup_call_note = function(cnd) private$collect_note(cnd))
    },

    collect_call = function(cnd) {
      id <- length(self$records$calls) + 1L
      record <- c(list(call_id = id), cnd$record,
                  list(round = as.integer(private$current_round %||% NA_integer_),
                       phase = as.character(attr(self, "execution_phase") %||% NA_character_),
                       message_id = NA_integer_,
                       arm = private$arm %||% NA_character_))
      self$records$calls[[id]] <- record
      cnd$handle$index <- id
      if (record$call_type %in% c("moderator", "participant")) {
        private$pending_calls <- c(private$pending_calls, id)
      }
    },

    collect_note = function(cnd) {
      id <- cnd$handle$index
      if (is.null(id) || id > length(self$records$calls)) return(invisible(NULL))
      for (field in names(cnd$fields)) {
        self$records$calls[[id]][[field]] <- cnd$fields[[field]]
      }
    },

    # Copy the flow's record of the pick just made into the session's routing
    # record; returns its position so the resulting message can be linked.
    record_routing = function(selected) {
      flow <- self$turn_taking_flow
      has_field <- "routing_record" %in% names(flow)
      record <- if (has_field) flow$routing_record else NULL
      selected_id <- if (is.null(selected)) NA_character_ else selected$id
      if (is.null(record)) {
        record <- .fg_routing_record(class(flow)[1], "custom",
                                     flow$participant_ids %||% character(),
                                     selected_id)
      }
      record$selected_id <- selected_id
      if (has_field) flow$routing_record <- NULL
      private$append_routing(record)
    },

    # Drop the flow's record of a choice that gave no participant the turn,
    # so that it is not copied for the next pick.
    discard_routing = function() {
      flow <- self$turn_taking_flow
      if ("routing_record" %in% names(flow)) flow$routing_record <- NULL
      invisible(NULL)
    },

    # Record a turn given without the flow: to a participant the moderator
    # called on ("called_on") or addressed ("addressed") in the message
    # `trigger`. No scores are asked for, and the flow's own record of its
    # last pick is left alone.
    record_forced_routing = function(agent_id, rule, trigger) {
      record <- .fg_routing_record(.fg_flow_name(self$turn_taking_flow), rule,
                                   agent_id, agent_id)
      private$append_routing(record, trigger)
    },

    append_routing = function(record, trigger = NA_integer_) {
      pick <- length(self$records$routing) + 1L
      self$records$routing[[pick]] <- c(
        list(pick = pick, round = as.integer(private$current_round),
             phase = as.character(attr(self, "execution_phase") %||% NA_character_),
             turn = as.integer(private$participant_turns + 1L)),
        record, list(message_id = NA_integer_,
                     trigger_message_id = as.integer(trigger),
                     arm = private$arm %||% NA_character_))
      pick
    },

    # ---- The session engine -------------------------------------------------
    # A session advances through stages, so that it can stop at a stated point
    # and later continue from it, as branching requires. "round_start" takes
    # the next guide item; "moderator" delivers its line; "after_item" handles
    # the point just after that line; "turn" runs one participant turn;
    # "call_on" has the moderator call on, one at a time, the participants who
    # have not spoken, when the discussion of a call-on item ends;
    # "round_end" and "round_end_last" check the prompt window; "loop_end"
    # closes the loop. The final summary follows. The checkpoints "start",
    # "after_item:k", "before_turn:t", "before_closing", and "end" run the
    # probes and injections planned for them, and a session can pause at one.
    # A stage pauses before it changes anything, or keeps in `private$state`
    # what it has already done, so that a resumed session neither repeats nor
    # skips a step.

    run_session = function(num_rounds, verbose) {
      private$session_begin(num_rounds, verbose)
      private$session_continue(verbose)
      private$session_finish(verbose)
    },

    session_begin = function(num_rounds, verbose) {
      if (verbose) {
        cat("Starting focus group simulation...\n")
        cat("Topic:", self$topic, "\n")
        cat("Purpose:", self$purpose, "\n")
      }
      private$current_phase_index <- 0
      private$current_round <- 0L
      private$last_summary_round <- -Inf
      private$fired <- character()
      private$claim_handles()
      # `call_on` holds the call-on stage in progress; `call_on_mark` is the
      # last message before the previous call-on point, so that a participant
      # is silent when no message of theirs follows it; `addressed` is the
      # participant a moderator line gave the next turn to.
      private$state <- list(stage = "round_start", num_rounds = num_rounds,
                            details = NULL, item = 0L, responses = 0L,
                            ended_by = NA_character_, call_on = NULL,
                            call_on_mark = 0L, addressed = NULL)

      # Log a roster system message so all agents "see" who is present
      participant_ids <- setdiff(names(self$agents), self$moderator_id)
      private$log_message("System", .fg_roster_text(participant_ids, self$moderator_id),
                          round = 0L, is_moderator = FALSE, phase = "setup",
                          meta = list(metadata = list(roster = TRUE)))
      private$checkpoint("start", verbose)
      invisible(NULL)
    },

    # Run stages until the session is done ("done") or pauses at the planned
    # branch point ("paused").
    session_continue = function(verbose) {
      private$claim_handles()
      repeat {
        stage <- private$state$stage
        if (identical(stage, "done")) return("done")
        paused <- switch(stage,
          round_start = private$step_round_start(verbose),
          moderator = private$step_moderator(verbose),
          after_item = private$step_after_item(verbose),
          turn = private$step_turn(verbose),
          call_on = private$step_call_on(verbose),
          round_end = private$step_round_end(verbose, last = FALSE),
          round_end_last = private$step_round_end(verbose, last = TRUE),
          loop_end = private$step_loop_end(verbose),
          stop("Unknown session stage: ", stage, call. = FALSE))
        if (isTRUE(paused)) return("paused")
      }
    },

    step_round_start = function(verbose) {
      if (private$closing_is_next() && private$checkpoint("before_closing", verbose)) {
        return(TRUE)
      }
      private$current_round <- private$current_round + 1L
      current_round <- private$current_round
      num_rounds <- private$state$num_rounds
      if (!is.null(num_rounds) && current_round > num_rounds) {
        if (verbose) cat("\n--- Reached num_rounds limit (", num_rounds, "). Simulation Ended ---\n")
        private$state$ended_by <- "num_rounds"
        private$state$stage <- "loop_end"
        return(FALSE)
      }
      current_phase_details <- private$get_next_phase_or_question()
      if (is.null(current_phase_details)) {
        if (verbose) cat("Moderator script exhausted. Ending simulation.\n")
        private$state$ended_by <- "script"
        private$state$stage <- "round_end_last"
        return(FALSE)
      }
      private$state$details <- current_phase_details
      private$state$item <- as.integer(private$current_phase_index)
      current_phase_name <- current_phase_details$phase
      attr(self, "execution_phase") <- current_phase_name
      # Determine the current question text appropriately per phase
      if (!is.null(current_phase_details$text) && nzchar(current_phase_details$text)) {
        self$current_question_text <- current_phase_details$text
      } else if (current_phase_name %in% .fg_question_phases) {
        # Fallback to the overall topic only for phases that ask a question or discussion
        self$current_question_text <- self$topic
      } else {
        self$current_question_text <- NULL
      }

      if (verbose) {
        cat(paste0("\n--- Round ", current_round, " | Phase: ", current_phase_name))
        if (!is.null(self$current_question_text) && nzchar(self$current_question_text) && current_phase_name != "opening" && current_phase_name != "closing") {
            cat(paste0(" | Current Question/Focus: ", substr(self$current_question_text,1,100), "..."))
        }
        cat(" ---\n")
      }
      private$state$stage <- "moderator"
      FALSE
    },

    # Whether the next guide item is a closing line that follows every
    # discussion item, the moment of the "before_closing" point.
    closing_is_next = function() {
      script <- self$question_script
      next_index <- as.integer(private$current_phase_index) + 1L
      if (next_index > length(script)) return(FALSE)
      if (!identical(script[[next_index]]$phase, "closing")) return(FALSE)
      num_rounds <- private$state$num_rounds
      if (!is.null(num_rounds) && private$current_round + 1L > num_rounds) return(FALSE)
      phases <- vapply(script, function(item) as.character(item$phase %||% ""), character(1))
      discussion <- which(phases %in% .fg_response_phases)
      !length(discussion) || max(discussion) < next_index
    },

    step_moderator = function(verbose) {
      current_phase_details <- private$state$details
      current_phase_name <- current_phase_details$phase
      current_round <- private$current_round

      if (isTRUE(current_phase_details$verbatim)) {
        # A verbatim item is the moderator's line as written; no model call.
        mod_text <- current_phase_details$text
        mod_meta <- list(metadata = list(verbatim = TRUE))
      } else {
        mod_res <- private$moderator_line(current_phase_details)
        mod_text <- if (is.list(mod_res)) mod_res$text else as.character(mod_res)
        mod_meta <- if (is.list(mod_res) && !is.null(mod_res$meta)) mod_res$meta else list()
      }
      private$log_message(self$moderator_id, mod_text, round = current_round,
                          is_moderator = TRUE, phase = current_phase_name,
                          meta = mod_meta)
      if (verbose) cat(paste0(self$moderator_id, " (Moderator): ", mod_text, "\n"))
      private$state$stage <- "after_item"
      FALSE
    },

    # The moderator's line for a guide item, generated from its template.
    # The moderator reads only the messages visible to it, under the
    # speakers' handles.
    moderator_line = function(current_phase_details) {
      moderator_agent <- self$agents[[self$moderator_id]]
      current_phase_name <- current_phase_details$phase
      question_phases <- .fg_question_phases
      raw_log <- private$moderator_view()
      moderator_log <- private$agent_view(raw_log)

      moderator_prompt_key <- paste0("moderator_", current_phase_name)
      if (!moderator_prompt_key %in% names(self$prompt_templates)) {
          # Fallback to a generic moderator prompt if phase-specific one is missing
          warning(paste("Prompt template for phase '", current_phase_name, "' (key: '", moderator_prompt_key, "') not found. Using generic moderator prompt.", sep=""))
          moderator_prompt_key <- "moderator_generic_utterance"
      }
      moderator_template <- self$prompt_templates[[moderator_prompt_key]]

      scripted_boundary_phase <- current_phase_name %in% c("opening", "closing") &&
        !is.null(current_phase_details$text) &&
        nzchar(current_phase_details$text)
      if (scripted_boundary_phase) {
        moderator_template <- paste(
          "You are the MODERATOR of a focus group.",
          "The focus group topic is: '{{topic}}'.",
          "The purpose of the focus group is: '{{focus_group_purpose}}'.",
          "Carry out only the following instruction for this moderator turn:",
          current_phase_details$text,
          "Do not repeat earlier opening or closing tasks, and do not advance to a later task unless this instruction calls for it.",
          sep = "\n"
        )
      }

      # Pre-fill moderator template placeholders to avoid unresolved tokens
      # in outputs. Counts are taken over agent IDs and shown as handles.
      no_message <- list(speaker_id = "N/A", text = "N/A")
      last_raw <- if (length(raw_log) > 0) raw_log[[length(raw_log)]] else no_message
      last_msg <- if (length(moderator_log) > 0) moderator_log[[length(moderator_log)]] else no_message
      participant_ids <- setdiff(names(self$agents), self$moderator_id)
      # Compute participation counts (exclude System and Moderator)
      non_system <- Filter(function(m) !is.null(m$speaker_id) && m$speaker_id != "System", raw_log)
      non_mod <- Filter(function(m) m$speaker_id != self$moderator_id, non_system)
      counts_raw <- if (length(non_mod) > 0) table(vapply(non_mod, function(m) m$speaker_id, "")) else integer(0)
      counts <- stats::setNames(rep(0L, length(participant_ids)), participant_ids)
      valid_count_names <- intersect(names(counts_raw), participant_ids)
      counts[valid_count_names] <- counts_raw[valid_count_names]
      dominant_id <- if (length(counts) > 0) names(counts)[which.max(counts)] else (participant_ids %||% character(1))[1] %||% ""
      quiet_id <- if (length(counts) > 0) names(counts)[which.min(counts)] else (participant_ids %||% character(1))[1] %||% ""
      other_id <- {
        cands <- setdiff(participant_ids, c(last_raw$speaker_id))
        if (length(cands) == 0) (participant_ids %||% character(1))[1] else cands[1]
      }
      moderator_template_filled <- replace_placeholders_known(
        moderator_template,
        list(
          topic = self$topic %||% "",
          focus_group_purpose = self$purpose %||% "",
          last_speaker_id = last_msg$speaker_id %||% "",
          last_utterance_text = last_msg$text %||% "",
          other_participant_id_placeholder = private$handle_of(other_id %||% ""),
          dominant_speaker_id_placeholder = private$handle_of(dominant_id %||% ""),
          quiet_speaker_id_placeholder = private$handle_of(quiet_id %||% ""),
          next_focus_group_question = self$current_question_text %||% ""
        )
      )

      if (!scripted_boundary_phase &&
          !current_phase_name %in% question_phases &&
          !is.null(current_phase_details$text) &&
          nzchar(current_phase_details$text)) {
        moderator_template_filled <- paste(
          moderator_template_filled,
          "\n\nScript for this moderator turn:\n",
          current_phase_details$text
        )
      }

      # Force-inject the scripted question into moderator prompt only for question-type phases
      if (current_phase_name %in% question_phases && !is.null(self$current_question_text) && nzchar(self$current_question_text)) {
        moderator_template_filled <- paste(
          moderator_template_filled,
          "\n\nQuestion to pose to participants:\n", self$current_question_text
        )
      }

      # Determine max tokens for this specific moderator action
      mod_tokens <- self$max_tokens_moderator
      if (current_phase_name == "opening") mod_tokens <- 300
      if (current_phase_name == "closing") mod_tokens <- 250
      if (current_phase_name == "summarizing") mod_tokens <- 200

      # Construct conversation history for the moderator
      history_for_moderator <- make_prompt_history(
        moderator_log,
        include_summary = self$current_conversation_summary,
        max_tokens_history = .fg_prompt_window_tokens
      )

      .fg_with_group_usage(self, moderator_agent$generate_utterance(
        topic = self$topic,
        conversation_history_string = history_for_moderator,
        utterance_prompt_template = moderator_template_filled,
        max_tokens_utterance = mod_tokens,
        current_moderator_question = self$current_question_text, # This is the question *being asked* or *related to*
        conversation_summary_so_far = self$current_conversation_summary,
        current_phase = current_phase_name,
        conversation_log = moderator_log,
        call_params = .fg_call_params(self, "moderator")
      ))
    },

    step_after_item = function(verbose) {
      if (private$checkpoint(paste0("after_item:", private$state$item), verbose)) {
        return(TRUE)
      }
      current_phase_name <- private$state$details$phase
      # A script may contain several closing turns. End after the final closing
      # entry; otherwise continue through the remaining scripted instructions.
      if (current_phase_name == "closing") {
        if (private$current_phase_index >= length(self$question_script)) {
          if (verbose) cat("Moderator completed the closing. Ending simulation.\n")
          private$state$ended_by <- "closing"
          private$state$stage <- "round_end_last"
        } else {
          private$state$stage <- "round_end"
        }
      } else if (current_phase_name %in% .fg_response_phases) {
        # Allow multiple participants to respond organically before moderator intervenes again
        private$state$responses <- 0L
        private$state$stage <- "turn"
      } else {
        private$state$stage <- "round_end"
      }
      FALSE
    },

    # One participant turn of the current question: to the participant the
    # moderator's last line addressed, if any, otherwise to the flow's pick.
    step_turn = function(verbose) {
      turns <- private$participant_turns
      if (private$checkpoint(paste0("before_turn:", turns + 1L), verbose)) {
        return(TRUE)
      }
      # A participant's injected message took this turn.
      if (private$participant_turns != turns) return(FALSE)
      addressed <- private$state$addressed
      if (!is.null(addressed)) {
        private$state$addressed <- NULL
        agent <- self$agents[[addressed$participant]]
        if (verbose) cat(paste("Addressed by the moderator:", agent$id, "\n"))
        pick <- private$record_forced_routing(agent$id, "addressed",
                                              addressed$message_id)
        private$participant_turn(agent, pick, cue = .fg_addressed_cue,
                                 metadata = list(addressed = TRUE), verbose = verbose)
      } else {
        next_participant <- self$turn_taking_flow$select_next_speaker(self) # Flow should select a PARTICIPANT
        # A choice of no participant, or of the moderator, ends the discussion
        # of the item. It is not a turn, so it has no routing record.
        if (is.null(next_participant)) {
          private$discard_routing()
          if (verbose) cat("No participant selected by flow (e.g., low desire). Ending participant response round.\n")
          private$end_discussion()
          return(FALSE)
        }
        if (next_participant$id == self$moderator_id && length(self$agents) > 1) {
          private$discard_routing()
          if (verbose) cat("Turn-taking flow selected moderator during participant response phase. Allowing moderator to manage flow.\n")
          private$end_discussion()
          return(FALSE)
        }
        pick <- private$record_routing(next_participant)
        if (verbose) cat(paste("Selected participant by flow:", next_participant$id, "\n"))
        private$participant_turn(
          next_participant, pick,
          metadata = self$turn_taking_flow$selection_metadata %||% list(),
          verbose = verbose)
      }

      private$state$responses <- private$state$responses + 1L
      # Break if we've had enough participant exchanges
      if (private$state$responses >= self$max_participant_responses) {
        if (verbose) cat("Maximum participant exchanges reached for this round.\n")
        private$end_discussion()
      }
      FALSE
    },

    # The body of a participant turn, shared by the flow's picks, addressed
    # turns, and call-ons: the agent replies to the log as it sees it, with
    # `cue` appended to the turn instruction, and the reply is logged with
    # `metadata` and linked to routing record `pick`.
    participant_turn = function(agent, pick, cue = NULL, metadata = list(),
                                verbose = FALSE) {
      current_phase_name <- private$state$details$phase
      view <- private$agent_view()
      history_for_participant <- make_prompt_history(
        view,
        include_summary = self$current_conversation_summary,
        max_tokens_history = .fg_prompt_window_tokens
      )
      template <- .fg_pick_template(self$prompt_templates, "utterance")
      if (!is.null(cue)) template <- paste(template, cue, sep = "\n")

      part_res <- .fg_with_group_usage(self, agent$generate_utterance(
        topic = self$topic,
        conversation_history_string = history_for_participant,
        utterance_prompt_template = template,
        max_tokens_utterance = max(self$max_tokens_utterance, 220L),
        current_moderator_question = self$current_question_text, # The question they are responding to
        conversation_summary_so_far = self$current_conversation_summary,
        current_phase = current_phase_name,
        conversation_log = view,
        call_params = .fg_call_params(self, "participant")
      ))
      part_text <- if (is.list(part_res)) part_res$text else as.character(part_res)
      part_meta <- if (is.list(part_res) && !is.null(part_res$meta)) part_res$meta else list()
      part_meta$metadata <- utils::modifyList(part_meta$metadata %||% list(),
                                              metadata %||% list())
      message_id <- private$log_message(agent$id, part_text,
                                        round = private$current_round,
                                        is_moderator = FALSE,
                                        phase = current_phase_name,
                                        meta = part_meta)
      self$records$routing[[pick]]$message_id <- message_id
      private$participant_turns <- private$participant_turns + 1L
      if (verbose) cat(paste0(agent$id, ": ", part_text, "\n"))

      self$turn_taking_flow$update_state_post_selection(agent$id, self)
      invisible(message_id)
    },

    # The discussion of the current item is over: call on the silent
    # participants when the item is a call-on point, otherwise end the round.
    end_discussion = function() {
      points <- self$call_on_silent
      at <- match(private$state$item, points$items %||% integer())
      if (!is.na(at)) {
        silent <- private$silent_participants()
        if (length(silent)) {
          private$state$call_on <- list(item = private$state$item,
                                        point = points$points[[at]],
                                        queue = silent, position = 1L,
                                        line_logged = FALSE,
                                        line_message_id = NA_integer_)
          private$state$stage <- "call_on"
          return(invisible(NULL))
        }
        private$state$call_on_mark <- private$last_message_id()
      }
      private$state$stage <- "round_end"
      invisible(NULL)
    },

    # The participants, in roster order, with no message after the previous
    # call-on point.
    silent_participants = function() {
      mark <- private$state$call_on_mark %||% 0L
      participant_ids <- setdiff(names(self$agents), self$moderator_id)
      spoke <- unlist(lapply(self$conversation_log, function(m) {
        if (isTRUE(as.integer(m$message_id %||% NA_integer_) > mark)) m$speaker_id
      }))
      setdiff(participant_ids, spoke)
    },

    # One call-on: the moderator's line to the next silent participant, then
    # that participant's turn, which the flow does not choose and no desire
    # score precedes. The line is logged once even when the session pauses
    # at the turn's checkpoint, which comes after it.
    step_call_on = function(verbose) {
      co <- private$state$call_on
      if (co$position > length(co$queue)) {
        private$state$call_on <- NULL
        private$state$call_on_mark <- private$last_message_id()
        private$state$stage <- "round_end"
        return(FALSE)
      }
      id <- co$queue[[co$position]]
      handle <- private$handle_of(id)
      if (!isTRUE(co$line_logged)) {
        text <- gsub("{participant}", handle, self$call_on_text, fixed = TRUE)
        co$line_message_id <- private$log_message(
          self$moderator_id, text, round = private$current_round,
          is_moderator = TRUE, phase = private$state$details$phase,
          meta = list(metadata = list(
            verbatim = TRUE,
            call_on = list(point = co$point, participant = id, handle = handle))))
        co$line_logged <- TRUE
        private$state$call_on <- co
        if (verbose) cat(paste0(self$moderator_id, " (Moderator): ", text, "\n"))
      }
      turns <- private$participant_turns
      if (private$checkpoint(paste0("before_turn:", turns + 1L), verbose)) {
        return(TRUE)
      }
      # A participant's injected message took this turn; the queue has moved on.
      if (private$participant_turns != turns) return(FALSE)
      pick <- private$record_forced_routing(id, "called_on", co$line_message_id)
      private$participant_turn(self$agents[[id]], pick, cue = .fg_called_on_cue,
                               metadata = list(called_on = TRUE), verbose = verbose)
      co$position <- co$position + 1L
      co$line_logged <- FALSE
      co$line_message_id <- NA_integer_
      private$state$call_on <- co
      FALSE
    },

    step_round_end = function(verbose, last) {
      private$maybe_interim_summary(verbose)
      utils::flush.console()
      private$state$stage <- if (last) "loop_end" else "round_start"
      FALSE
    },

    step_loop_end = function(verbose) {
      # Without a closing line after the discussion, the "before_closing"
      # point comes just before "end"; a session cut short by `num_rounds`
      # never reaches it.
      if (!identical(private$state$ended_by, "num_rounds") &&
          private$checkpoint("before_closing", verbose)) {
        return(TRUE)
      }
      private$checkpoint("end", verbose)
      # The loop exits one of two ways: the round limit, which prints its own
      # line, or the normal close (script exhausted or moderator ended).
      if (verbose && !identical(private$state$ended_by, "num_rounds")) {
        cat("\n--- Simulation Concluded (script ended or moderator closed) ---\n")
      }
      private$report_undelivered()
      private$state$stage <- "done"
      FALSE
    },

    # Threshold-based interim summarization. The summary stands in for the
    # messages the prompt window no longer shows, so it covers exactly those;
    # while the window still holds the whole discussion there is nothing to
    # summarize.
    maybe_interim_summary = function(verbose) {
      round_counter <- private$current_round
      summarize_trigger_tokens <- 48000L
      summarize_trigger_count <- 200L
      summarize_cooldown_rounds <- 10L
      needs_summary <- FALSE
      if ((round_counter - private$last_summary_round) >= summarize_cooldown_rounds) {
        recent_hist <- make_prompt_history(
          self$conversation_log,
          include_summary = NULL,
          max_tokens_history = .fg_prompt_window_tokens
        )
        recent_tokens <- estimate_tokens(recent_hist)
        needs_summary <- recent_tokens >= summarize_trigger_tokens ||
          length(self$conversation_log) > summarize_trigger_count
      }
      dropped_transcript <- if (needs_summary) {
        private$get_dropped_transcript_for_summary(.fg_prompt_window_tokens)
      } else ""
      if (nzchar(dropped_transcript)) {
        if (verbose) cat("\n--- Generating interim summary for context management ---\n")
        attr(self, "execution_phase") <- "interim_summary"
        interim_summary_text <- self$summarize(
          config = .fg_with_params(self$admin_config,
                                   .fg_call_params(self, "summary")),
          summary_level = 3,
          max_tokens = self$max_tokens_interim_summary,
          internal_call = TRUE,
          transcript_override = dropped_transcript,
          .runner = self$agents[[self$moderator_id]]$.runner
        )
        self$current_conversation_summary <- interim_summary_text
        private$last_summary_round <- round_counter
        if (verbose && !is.null(interim_summary_text)) cat("Interim summary generated.\n")
      }
      invisible(NULL)
    },

    # Final summary of the entire discussion. The transcript is complete at
    # this point; a summary failure is reported, never allowed to destroy
    # the finished run. A session run with `final_summary = FALSE` makes no
    # summary call.
    session_finish = function(verbose) {
      if (isFALSE(self$records$info$final_summary)) {
        self$final_summary <- NULL
        return(invisible(self$conversation_log))
      }
      if (verbose) cat("\n--- Generating final summary of the entire discussion ---\n")
      final_summary <- tryCatch(
        self$summarize(config = .fg_with_params(self$admin_config,
                                                .fg_call_params(self, "summary")),
                       summary_level = 1, max_tokens = self$max_tokens_summary,
                       internal_call = FALSE,
                       .runner = self$agents[[self$moderator_id]]$.runner),
        error = function(e) {
          warning("The final summary call failed (",
                  conditionMessage(e),
                  "); the completed transcript is returned without it.",
                  call. = FALSE)
          NA_character_
        })
      self$final_summary <- final_summary
      if (verbose && !is.na(final_summary)) {
        cat("Final Summary:\n", final_summary, "\n")
      }
      invisible(self$conversation_log)
    },

    # ---- Probes and injections ---------------------------------------------

    # Run the probes and injections planned for a checkpoint, once. Returns
    # TRUE when the session is to pause there.
    checkpoint = function(event, verbose) {
      if (event %in% private$fired) return(FALSE)
      private$fired <- c(private$fired, event)
      for (probe in private$plan$probes) {
        if (event %in% probe$events) private$run_probe(probe, event, verbose)
      }
      for (injection in private$plan$injections) {
        if (identical(injection$event, event)) private$apply_injection(injection, verbose)
      }
      identical(private$plan$pause_at, event)
    },

    # Check and store the plan of injections and probes for this session.
    # Points must lie within the guide: an item number no larger than the
    # number of items, a turn no later than the last turn the guide allows,
    # counting a turn for every participant at every call-on point.
    prepare_plan = function(injections = list(), probes = list(), pause_at = NULL) {
      n_items <- length(self$question_script)
      if (!n_items) n_items <- 1L
      phases <- if (length(self$question_script)) {
        vapply(self$question_script, function(item) as.character(item$phase %||% ""),
               character(1))
      } else "generic_discussion"
      participant_ids <- setdiff(names(self$agents), self$moderator_id)
      reserve <- length(self$call_on_silent$items %||% integer()) *
        as.double(length(participant_ids))
      max_turns <- sum(phases %in% .fg_response_phases) *
        as.double(self$max_participant_responses) + reserve
      check_point <- function(point, what) {
        if (identical(point$type, "after_item") && point$value > n_items) {
          stop(what, " ", format(point), " is beyond the ", n_items,
               " item(s) of the guide.", call. = FALSE)
        }
        if (identical(point$type, "before_turn") && point$value > max_turns) {
          stop(what, " ", format(point), " is beyond the ", max_turns,
               " participant turn(s) the guide allows.", call. = FALSE)
        }
      }
      injections <- lapply(seq_along(injections), function(i) {
        given <- injections[[i]]$from
        injection <- private$plan_entry(injections[[i]])
        check_point(injection$at, "Injection")
        if (identical(injection$from, "System")) {
          stop("An injection cannot come from 'System', which labels the roster row.",
               call. = FALSE)
        }
        if (isTRUE(injection$as_turn)) {
          if (!identical(injection$at$type, "before_turn")) {
            stop("An injection from '", given, "', a participant's ID or handle, is ",
                 "that participant's turn, so it goes at fg_before_turn(); found ",
                 format(injection$at), ".", call. = FALSE)
          }
          if (!identical(injection$visibility, "shared")) {
            stop("An injection from '", given, "', a participant's ID or handle, is a ",
                 "turn of the discussion and must have visibility \"shared\".",
                 call. = FALSE)
          }
        } else if (!identical(injection$from, "moderator") &&
                   grepl("^p[0-9]+$", tolower(injection$from))) {
          stop("An injection cannot come from '", injection$from, "': it is no ",
               "participant of the session, and an outside voice named like a ",
               "participant's handle would read as one.", call. = FALSE)
        }
        injection$label <- injection$label %||% paste0("injection_", i)
        injection
      })
      turn_events <- vapply(injections, function(inj) {
        if (isTRUE(inj$as_turn)) inj$event else NA_character_
      }, character(1))
      turn_events <- turn_events[!is.na(turn_events)]
      if (anyDuplicated(turn_events)) {
        stop("At most one participant's injected turn can stand at one point; ",
             .fg_event_label(turn_events[anyDuplicated(turn_events)]),
             " has more.", call. = FALSE)
      }
      probes <- lapply(seq_along(probes), function(i) {
        probe <- probes[[i]]
        for (point in probe$at) check_point(point, "Probe")
        unknown <- setdiff(probe$participants %||% character(), participant_ids)
        if (length(unknown)) {
          stop("Probe participants not in the session: ",
               paste(unknown, collapse = ", "), ".", call. = FALSE)
        }
        probe$label <- probe$label %||% paste0("probe_", i)
        probe$events <- vapply(probe$at, .fg_point_event, character(1))
        probe
      })
      private$plan <- list(injections = injections, probes = probes,
                           pause_at = pause_at)
      invisible(private$plan)
    },

    # Log an injected message and record it.
    apply_injection = function(injection, verbose = FALSE) {
      if (isTRUE(injection$as_turn)) return(private$apply_turn_injection(injection, verbose))
      speaker <- if (identical(injection$from, "moderator")) self$moderator_id else injection$from
      log <- self$conversation_log
      cut_after <- if (length(log)) as.integer(log[[length(log)]]$message_id) else NA_integer_
      phase <- private$state$details$phase %||% "setup"
      info <- list(label = injection$label, arm = private$arm %||% NA_character_,
                   point = format(injection$at), from = injection$from,
                   visibility = injection$visibility,
                   cut_after_message_id = cut_after, as_turn = FALSE,
                   replaced = NA_character_)
      message_id <- private$log_message(
        speaker, injection$text, round = private$current_round,
        is_moderator = identical(speaker, self$moderator_id), phase = phase,
        meta = list(metadata = list(injected = TRUE, injection = info)))
      self$records$injections[[length(self$records$injections) + 1L]] <- c(
        info, list(delivered = TRUE, message_id = message_id, speaker_id = speaker,
                   text = injection$text, round = private$current_round,
                   phase = phase, turn = private$participant_turns + 1L))
      if (verbose) cat(paste0(speaker, " (injected): ", injection$text, "\n"))
      invisible(message_id)
    },

    # A participant's injected message, which is that participant's turn: it
    # is logged under the participant, given a routing row with the rule
    # "injected", counted as a participant turn, and passed to the flow as
    # the participant's turn. It then takes the place of the turn the session
    # was about to give: the reply to a call-on line, when it comes from the
    # participant called on, moves the call-on queue on (from anyone else it
    # takes no one's place, and the participant called on answers next); any
    # other turn counts toward the item's responses, clears an addressed turn,
    # and can end the discussion of the item. `replaced` records which turn it
    # took the place of, or "none".
    apply_turn_injection = function(injection, verbose = FALSE) {
      id <- injection$from
      calling_on <- identical(private$state$stage, "call_on")
      # At a call-on turn the injected line answers the call-on only when it
      # comes from the participant called on; from anyone else it is an
      # interjection, and the participant called on still answers next.
      called <- if (calling_on) private$state$call_on$queue[[private$state$call_on$position]]
      takes_call <- calling_on && identical(id, called)
      replaced <- if (takes_call) "called_on" else if (calling_on) "none" else
        if (!is.null(private$state$addressed)) "addressed" else "flow"
      log <- self$conversation_log
      cut_after <- if (length(log)) as.integer(log[[length(log)]]$message_id) else NA_integer_
      phase <- private$state$details$phase %||% "setup"
      turn <- private$participant_turns + 1L
      info <- list(label = injection$label, arm = private$arm %||% NA_character_,
                   point = format(injection$at), from = id,
                   visibility = injection$visibility,
                   cut_after_message_id = cut_after, as_turn = TRUE,
                   replaced = replaced)
      pick <- private$record_forced_routing(id, "injected", NA_integer_)
      message_id <- private$log_message(
        id, injection$text, round = private$current_round, is_moderator = FALSE,
        phase = phase,
        meta = list(metadata = list(injected = TRUE, as_turn = TRUE, injection = info)))
      self$records$routing[[pick]]$message_id <- message_id
      self$records$injections[[length(self$records$injections) + 1L]] <- c(
        info, list(delivered = TRUE, message_id = message_id, speaker_id = id,
                   text = injection$text, round = private$current_round,
                   phase = phase, turn = turn))
      private$participant_turns <- turn
      if (verbose) cat(paste0(id, " (injected turn): ", injection$text, "\n"))
      self$turn_taking_flow$update_state_post_selection(id, self)
      if (takes_call) {
        co <- private$state$call_on
        co$position <- co$position + 1L
        co$line_logged <- FALSE
        co$line_message_id <- NA_integer_
        private$state$call_on <- co
      } else if (!calling_on) {
        private$state$addressed <- NULL
        private$state$responses <- private$state$responses + 1L
        if (private$state$responses >= self$max_participant_responses) {
          private$end_discussion()
        }
      }
      invisible(message_id)
    },

    # Ask one probe of each of its participants. The answers are recorded and
    # never logged. With a scale, each answer is read as a number, after one
    # re-ask when none can be read; the participants whose value stays NA are
    # named in one warning.
    run_probe = function(probe, event, verbose) {
      targets <- probe$participants %||% setdiff(names(self$agents), self$moderator_id)
      template <- self$prompt_templates$participant_probe_instruction %||%
        get_default_prompt_templates()$participant_probe_instruction
      reask <- self$prompt_templates$participant_probe_reask %||%
        get_default_prompt_templates()$participant_probe_reask
      log <- self$conversation_log
      after <- if (length(log)) as.integer(log[[length(log)]]$message_id) else NA_integer_
      phase <- private$state$details$phase %||% "setup"
      previous_phase <- attr(self, "execution_phase")
      attr(self, "execution_phase") <- "probe"
      view <- private$agent_view(log)
      unread <- character()
      for (id in targets) {
        out <- .fg_with_group_usage(self, self$agents[[id]]$answer_probe(
          question = probe$question, conversation_log = view, topic = self$topic,
          template = template, max_tokens = self$max_tokens_probe,
          conversation_summary = self$current_conversation_summary,
          call_params = .fg_call_params(self, "probe"),
          scale = probe$scale, reask_template = reask))
        if (is.na(out$answer)) {
          warning(sprintf("Probe '%s' at %s returned no answer from %s; it is recorded as NA.",
                          probe$label, .fg_event_label(event), id), call. = FALSE)
        } else if (!is.null(probe$scale) && is.na(out$value)) {
          unread <- c(unread, id)
        }
        self$records$probes[[length(self$records$probes) + 1L]] <- list(
          probe = probe$label, point = .fg_event_label(event), agent_id = id,
          question = probe$question, answer = out$answer, value = out$value,
          raw_text = out$raw_text, finish_reason = out$finish_reason,
          attempts = out$attempts, call_id = out$call_id,
          round = private$current_round, phase = phase,
          turn = private$participant_turns + 1L, after_message_id = after,
          arm = private$arm %||% NA_character_)
        if (verbose) cat(paste0(id, " (probe ", probe$label, "): ", out$answer, "\n"))
      }
      if (length(unread)) {
        warning(sprintf(paste(
          "Probe '%s' at %s: no whole number from %s to %s could be read from",
          "the answer of %s, even after a re-ask; the value is recorded as NA."),
          probe$label, .fg_event_label(event), .fg_number_text(probe$scale[[1]]),
          .fg_number_text(probe$scale[[2]]), paste(unread, collapse = ", ")), call. = FALSE)
      }
      attr(self, "execution_phase") <- previous_phase
      invisible(NULL)
    },

    # Continue a copy of this session, stopped at `branch_event`, once per
    # arm: the arm's injections at the branch point are applied, its later
    # ones planned, and the session run to its end. Every arm starts from the
    # random-number state at the branch point.
    branch_out = function(arms, probes, branch_event, verbose) {
      # Every arm starts from the random-number state at the branch point.
      if (!exists(".Random.seed", envir = globalenv(), inherits = FALSE)) stats::runif(1)
      rng_state <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      snapshot <- .fg_clone_session(self)
      out <- list()
      for (name in names(arms)) {
        assign(".Random.seed", rng_state, envir = globalenv())
        branch <- .fg_clone_session(snapshot)
        bp <- branch$.__enclos_env__$private
        bp$arm <- name
        injections <- lapply(arms[[name]], function(inj) {
          inj$label <- inj$label %||% name
          inj
        })
        at_branch <- vapply(injections, function(inj) {
          identical(.fg_point_event(inj$at), branch_event)
        }, logical(1))
        bp$prepare_plan(injections[!at_branch], .fg_probe_list(probes))
        bp$fired <- private$fired
        if (verbose) cat("
=== Arm: ", name, " ===
", sep = "")
        done <- tryCatch({
          bp$with_records({
            for (inj in injections[at_branch]) bp$apply_injection(bp$plan_entry(inj), verbose)
            bp$session_continue(verbose)
            bp$session_finish(verbose)
          })
          TRUE
        }, error = function(e) e)
        if (!isTRUE(done)) {
          rlang::abort(sprintf("Arm '%s' failed.", name),
                       class = "focusgroup_branch_failure", arm = name,
                       branch = branch, completed = out,
                       phase = attr(branch, "execution_phase") %||% "setup",
                       parent = done)
        }
        out[[name]] <- branch
      }
      list(prefix = self, arms = out)
    },

    # ---- Continuing a recorded session ----------------------------------------

    # Put the session in the state it was in just before participant turn
    # `t` (the t-th message by a participant on the roster), as a session
    # run with this record would be when paused at fg_before_turn(t). The
    # log and records the object holds are the record: the log is cut to the
    # messages before that turn, the records to the selections, probes,
    # injections, and calls made before it, and the engine state is set from
    # them: the guide item, its stage (a turn or a call-on), the responses on
    # the item, the call-on queue and mark, an addressed participant, the
    # points passed, the question in force, and the interim summary in force.
    # The turn-taking flow is made anew from its recorded type and parameters
    # and is told of every moderator line and participant turn of the prefix,
    # as the session told it; the round-robin position is that of the
    # participant the last rotation pick chose, or, in a record without
    # selections, that of the last speaker. A custom flow cannot be rebuilt. Token counters are those of
    # the calls kept.
    position_at = function(t, final_summary = TRUE) {
      flow_name <- .fg_flow_name(self$turn_taking_flow)
      if (!flow_name %in% c("round_robin", "probabilistic", "desire_based")) {
        stop("A session with the custom turn-taking flow '", flow_name, "' cannot be ",
             "continued: its state cannot be rebuilt from the record.", call. = FALSE)
      }
      flow_params <- .fg_flow_params(self$turn_taking_flow)
      log <- self$conversation_log
      record <- self$records %||% .fg_empty_records()
      participant_ids <- setdiff(names(self$agents), self$moderator_id)
      speaker <- vapply(log, function(m) as.character(m$speaker_id %||% NA_character_),
                        character(1))
      is_mod <- vapply(log, function(m) identical(m$speaker_id, self$moderator_id),
                       logical(1))
      is_turn <- !is_mod & speaker %in% participant_ids
      turn_rows <- which(is_turn)
      if (t > length(turn_rows)) {
        stop("The record has ", length(turn_rows), " participant turn(s), so it cannot ",
             "be continued from turn ", t, ".", call. = FALSE)
      }
      cut <- turn_rows[[t]]
      turn_msg <- log[[cut]]
      prefix <- log[seq_len(cut - 1L)]
      id_t <- as.integer(turn_msg$message_id)
      ids <- vapply(prefix, function(m) as.integer(m$message_id), integer(1))
      item_of <- function(m) as.integer(m$item %||% NA_integer_)
      k <- item_of(turn_msg)
      script <- self$question_script
      if (is.na(k) || k < 1L || k > length(script)) {
        stop("Participant turn ", t, " belongs to no item of the guide.", call. = FALSE)
      }
      details <- script[[k]]
      round <- as.integer(turn_msg$round %||% k)
      kind_of <- function(m) {
        inj <- m$metadata$injection
        if (isTRUE(m$metadata$as_turn)) inj$replaced %||% "flow" else
          if (isTRUE(m$metadata$called_on)) "called_on" else "turn"
      }
      # An injected line during a call-on stage answers the call-on
      # ("called_on") or interjects ("none"); either way the stage was under way.
      calling_on <- kind_of(turn_msg) %in% c("called_on", "none")

      # The records made before the turn: the picks of the earlier turns, the
      # probes asked before it, the injections delivered before it, and the
      # calls up to the last of them.
      turn_of <- function(x) as.integer(x$turn %||% NA_integer_)
      routing <- Filter(function(r) isTRUE(turn_of(r) < t), record$routing)
      probes <- Filter(function(p) isTRUE(turn_of(p) <= t), record$probes)
      injections <- Filter(function(i) {
        isTRUE(i$delivered) && isTRUE(as.integer(i$message_id) %in% ids)
      }, record$injections)
      calls <- record$calls
      call_ids <- vapply(calls, function(x) as.integer(x$call_id %||% NA_integer_), integer(1))
      call_messages <- vapply(calls, function(x) as.integer(x$message_id %||% NA_integer_),
                              integer(1))
      call_rounds <- vapply(calls, function(x) as.integer(x$round %||% NA_integer_), integer(1))
      call_types <- vapply(calls, function(x) as.character(x$call_type %||% NA_character_),
                           character(1))
      summaries <- which(call_types %in% "interim_summary" & call_rounds < round &
                           vapply(calls, function(x) is.na(x$error %||% NA), logical(1)))
      # The calls of one probe answer are consecutive calls by one agent with
      # attempts 1, 2, ...; the answer names one of them, and the last is
      # the one that counts here.
      attempt_of <- vapply(calls, function(x) as.integer(x$attempt %||% 1L) %|NA|% 1L,
                           integer(1))
      agent_of <- vapply(calls, function(x) as.character(x$agent_id %||% NA_character_),
                         character(1))
      probe_end <- function(p) {
        at <- match(as.integer(p$call_id %||% NA_integer_), call_ids)
        if (is.na(at)) return(0L)
        while (at < length(calls) && identical(call_types[[at + 1L]], "probe") &&
               identical(agent_of[[at + 1L]], agent_of[[at]]) &&
               attempt_of[[at + 1L]] == attempt_of[[at]] + 1L) {
          at <- at + 1L
        }
        call_ids[[at]]
      }
      last_call <- max(c(0L,
        call_ids[!is.na(call_messages) & call_messages < id_t],
        vapply(probes, probe_end, integer(1)),
        call_ids[summaries]), na.rm = TRUE)
      calls <- calls[!is.na(call_ids) & call_ids <= last_call]

      private$start_records(final_summary)
      self$conversation_log <- prefix
      self$records$routing <- routing
      self$records$probes <- probes
      self$records$injections <- injections
      self$records$calls <- calls
      count <- function(name) {
        vapply(calls, function(x) as.numeric(x[[name]] %||% NA_real_)[1], numeric(1))
      }
      sent <- count("sent_tokens")
      rec <- count("rec_tokens")
      total <- count("total_tokens")
      total <- ifelse(is.na(total), ifelse(is.na(sent) | is.na(rec), NA_real_, sent + rec),
                      total)
      self$total_tokens_sent <- sum(sent, na.rm = TRUE)
      self$total_tokens_received <- sum(rec, na.rm = TRUE)
      self$total_tokens <- sum(total, na.rm = TRUE)
      self$calls_usage_unknown <- as.double(sum(is.na(total) | is.na(sent) | is.na(rec)))
      self$final_summary <- NULL
      attr(self, "session_status") <- NULL

      # The interim summary in force: the last one made before this round.
      if (length(summaries)) {
        last <- record$calls[[summaries[[length(summaries)]]]]
        private$last_summary_round <- as.integer(last$round)
        self$current_conversation_summary <- as.character(last$cleaned_text)
      } else {
        private$last_summary_round <- -Inf
        self$current_conversation_summary <- NULL
      }

      # The points passed: the start, each item's line, and each turn up to t.
      fired <- "start"
      announced <- 0L
      turns <- 0L
      for (m in c(prefix, list(turn_msg))) {
        if (identical(m$speaker_id, "System")) next
        j <- item_of(m)
        if (!is.na(j) && j > announced) {
          fired <- c(fired, paste0("after_item:", seq.int(announced + 1L, j)))
          announced <- j
        }
        if (m$speaker_id %in% participant_ids) {
          turns <- turns + 1L
          fired <- c(fired, paste0("before_turn:", turns))
        }
      }

      # The flow, told of the prefix as the session told it.
      flow <- create_conversation_flow(flow_name, self$agents, self$moderator_id,
                                       flow_params = flow_params)
      handles <- vapply(participant_ids, private$handle_of, character(1))
      addressed <- NULL
      responses <- 0L
      for (m in prefix) {
        if (identical(m$speaker_id, self$moderator_id)) {
          if (is.function(flow$note_moderator_turn)) flow$note_moderator_turn()
          target <- if (is.null(m$metadata$call_on)) {
            .fg_addressee(as.character(m$text), handles)
          } else NA_character_
          addressed <- if (!is.na(target)) {
            list(participant = target, message_id = as.integer(m$message_id))
          }
        } else if (m$speaker_id %in% participant_ids) {
          flow$update_state_post_selection(m$speaker_id, self)
          kind <- kind_of(m)
          if (!kind %in% c("called_on", "none")) {
            addressed <- NULL
            if (identical(item_of(m), k)) responses <- responses + 1L
          }
        }
      }
      own <- Filter(function(r) identical(turn_of(r), as.integer(t)), record$routing)
      rule <- if (length(own)) as.character(own[[1]]$rule) else NA_character_
      if (identical(rule, "addressed")) {
        addressed <- list(participant = as.character(own[[1]]$selected_id),
                          message_id = as.integer(own[[1]]$trigger_message_id))
      } else if (!is.na(rule) && !rule %in% c("called_on", "injected")) {
        addressed <- NULL
      } else if (identical(rule, "injected") && identical(kind_of(turn_msg), "flow")) {
        addressed <- NULL
      }
      if (inherits(flow, "RoundRobinFlow")) {
        rotation <- Filter(function(r) identical(r$rule, "rotation"), routing)
        # The rotation stops at the participant it chose.
        position <- if (length(rotation)) {
          match(as.character(rotation[[length(rotation)]]$selected_id), participant_ids)
        } else if (!length(record$routing) && t > 1L) {
          last <- speaker[turn_rows[[t - 1L]]]
          match(last, participant_ids)
        } else 0L
        flow$.__enclos_env__$private$current_participant_index <- as.numeric(position)
      }
      # A desire-based flow keeps the notes of its last selection, which the
      # turn it chose carries in its metadata.
      if (inherits(flow, "DesireBasedFlow")) {
        chosen <- Filter(function(r) !r$rule %in% c("called_on", "addressed", "injected"),
                         routing)
        if (length(chosen)) {
          at <- match(as.integer(chosen[[length(chosen)]]$message_id), ids)
          md <- if (!is.na(at)) prefix[[at]]$metadata else list()
          keys <- c("flow", "fallback", "threshold", "scores", "selected_id",
                    "desire_scoring_failure")
          notes <- md[intersect(names(md), keys)]
          flow$selection_metadata <- if (length(notes)) notes
        }
      }
      self$turn_taking_flow <- flow

      # The call-on stage in progress, and the mark of the previous point.
      points <- self$call_on_silent
      items <- as.integer(points$items %||% integer())
      earlier <- items[items < k]
      mark <- if (length(earlier)) {
        j <- max(earlier)
        in_j <- vapply(prefix, function(m) identical(item_of(m), j), logical(1))
        if (any(in_j)) max(ids[in_j]) else 0L
      } else 0L
      call_on <- NULL
      if (calling_on && !k %in% items) {
        stop("Participant turn ", t, " answers a call-on line, but guide item ", k,
             " is not a call-on point of the record.", call. = FALSE)
      }
      if (calling_on) {
        lines <- which(vapply(prefix, function(m) {
          identical(m$speaker_id, self$moderator_id) && !is.null(m$metadata$call_on) &&
            identical(item_of(m), k)
        }, logical(1)))
        before <- prefix[seq_len(lines[[1]] - 1L)]
        spoke <- unlist(lapply(before, function(m) {
          if (isTRUE(as.integer(m$message_id) > mark)) m$speaker_id
        }))
        call_on <- list(item = k, point = points$points[[match(k, items)]],
                        queue = setdiff(participant_ids, spoke),
                        position = length(lines), line_logged = TRUE,
                        line_message_id = as.integer(prefix[[lines[[length(lines)]]]]$message_id))
      }

      private$current_phase_index <- as.numeric(k)
      private$current_round <- round
      private$participant_turns <- as.integer(t - 1L)
      private$pending_calls <- integer()
      private$arm <- NULL
      private$fired <- fired
      private$plan <- list(injections = list(), probes = list(),
                           pause_at = paste0("before_turn:", t))
      private$state <- list(stage = if (calling_on) "call_on" else "turn",
                            num_rounds = NULL, details = details, item = k,
                            responses = responses, ended_by = NA_character_,
                            call_on = call_on, call_on_mark = as.integer(mark),
                            addressed = addressed)
      attr(self, "execution_phase") <- details$phase
      self$current_question_text <- if (!is.null(details$text) && nzchar(details$text)) {
        details$text
      } else if (details$phase %in% .fg_question_phases) self$topic
      private$claim_handles()
      invisible(self)
    },

    # Warn about, and record, the planned injections and probes a finished
    # session never reached.
    report_undelivered = function() {
      for (injection in private$plan$injections) {
        if (injection$event %in% private$fired) next
        warning(sprintf("Injection '%s' at %s was not delivered: the session ended first.",
                        injection$label, format(injection$at)), call. = FALSE)
        self$records$injections[[length(self$records$injections) + 1L]] <- list(
          label = injection$label, arm = private$arm %||% NA_character_,
          point = format(injection$at), from = injection$from,
          visibility = injection$visibility, cut_after_message_id = NA_integer_,
          delivered = FALSE, message_id = NA_integer_, speaker_id = NA_character_,
          text = injection$text, round = NA_integer_, phase = NA_character_,
          turn = NA_integer_, as_turn = isTRUE(injection$as_turn),
          replaced = NA_character_)
      }
      for (probe in private$plan$probes) {
        missed <- setdiff(probe$events, private$fired)
        if (length(missed)) {
          warning(sprintf("Probe '%s' was not asked at %s: the session ended first.",
                          probe$label, paste(vapply(missed, .fg_event_label, character(1)),
                                             collapse = ", ")), call. = FALSE)
        }
      }
    },

    # The log as the moderator sees it: without messages injected for the
    # participants alone.
    moderator_view = function(log = self$conversation_log) {
      Filter(function(m) !identical(m$metadata$injection$visibility, "participants"),
             log)
    },

    # The log as the agents see it: every agent under its handle, and the
    # roster row listing the handles. Every prompt is built from this view;
    # the log itself keeps the agent IDs.
    agent_view = function(log = self$conversation_log) {
      handles <- self$handles
      if (!length(handles) || !length(log)) return(log)
      participant_ids <- setdiff(names(self$agents), self$moderator_id)
      roster <- .fg_roster_text(vapply(participant_ids, private$handle_of, character(1)),
                                private$handle_of(self$moderator_id))
      lapply(log, function(m) {
        id <- m$speaker_id
        if (is.character(id) && length(id) == 1L && !is.na(id) && id %in% names(handles)) {
          m$speaker_id <- handles[[id]]
        }
        if (isTRUE(m$metadata$roster)) m$text <- roster
        m
      })
    },

    # Give every agent its handle in this session. An agent object can belong
    # to more than one session, each with its own handles, so the session
    # sets them again whenever it runs.
    claim_handles = function() {
      for (id in names(self$agents)) {
        agent <- self$agents[[id]]
        if ("handle" %in% names(agent)) agent$handle <- unname(self$handles[[id]])
      }
      invisible(NULL)
    },

    # The handle of a speaker: an agent's handle, or the speaker's own label
    # for the roster row and for an injected voice.
    handle_of = function(id) {
      handles <- self$handles
      if (length(id) == 1L && !is.na(id) && id %in% names(handles)) {
        unname(handles[[id]])
      } else {
        as.character(id)
      }
    },

    last_message_id = function() {
      ids <- vapply(self$conversation_log, function(m) {
        as.integer(m$message_id %||% NA_integer_)
      }, integer(1))
      if (length(ids) && any(!is.na(ids))) max(ids, na.rm = TRUE) else 0L
    },
    # Guard for the plot_* methods: ggplot2 is a Suggests, not an Import.
    require_ggplot2 = function() {
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Package 'ggplot2' is required for plotting. ",
             "Install it with install.packages(\"ggplot2\").", call. = FALSE)
      }
      invisible(TRUE)
    },

    # Helper to get the next phase/question from the script
    get_next_phase_or_question = function() {
      if (is.null(self$question_script) || length(self$question_script) == 0) {
        # If no script, run a single "generic_discussion" phase then end.
        if (private$current_phase_index == 0) { # First call
            private$current_phase_index <- 1 # Mark as started
            return(list(phase = "generic_discussion", text = self$topic))
        }
        return(NULL) # End after one generic phase if no script
      }

      private$current_phase_index <- private$current_phase_index + 1
      if (private$current_phase_index > length(self$question_script)) {
        return(NULL) # Script exhausted
      }
      return(self$question_script[[private$current_phase_index]])
    },

    # Log a message. Its id follows the largest id in the log, so that a log
    # whose ids are not consecutive, as an imported one may be, never gets a
    # duplicate. After a moderator line the turn-taking flow is told, so that
    # the participant who spoke before it is no longer the previous speaker.
    log_message = function(speaker_id, text, round, is_moderator = NULL,
                           phase = "unknown", meta = list()) {
      if (is.null(is_moderator)) {
        is_moderator <- (speaker_id == self$moderator_id)
      }
      msg <- list(
        message_id = as.integer(private$last_message_id() + 1L),
        round = as.integer(round),
        item = as.integer(private$state$item %||% 0L),
        speaker_id = speaker_id,
        handle = private$handle_of(speaker_id),
        is_moderator = is_moderator,
        text = text,
        timestamp = Sys.time(),
        phase = phase,
        response_id = meta$response_id %||% NA_character_,
        finish_reason = meta$finish_reason %||% NA_character_,
        sent_tokens = meta$sent_tokens %||% NA_integer_,
        rec_tokens = meta$rec_tokens %||% NA_integer_,
        total_tokens = meta$total_tokens %||% NA_integer_,
        duration_s = meta$duration_s %||% NA_real_,
        provider = meta$provider %||% NA_character_,
        model = meta$model %||% NA_character_,
        metadata = meta$metadata %||% list()
      )
      if (!is.null(meta$usage_unknown)) msg$metadata$usage_unknown <- meta$usage_unknown
      self$conversation_log <- c(self$conversation_log, list(msg))
      # The model calls that produced this message are linked to it.
      for (id in private$pending_calls) {
        self$records$calls[[id]]$message_id <- msg$message_id
      }
      private$pending_calls <- integer()
      if (identical(speaker_id, self$moderator_id)) {
        flow <- self$turn_taking_flow
        if (is.function(flow$note_moderator_turn)) flow$note_moderator_turn()
        # The latest moderator line decides whether the next turn is given
        # to a participant it named; a call-on line gives its own turn.
        if (!is.null(private$state)) {
          target <- if (is.null(msg$metadata$call_on)) {
            participant_ids <- setdiff(names(self$agents), self$moderator_id)
            .fg_addressee(as.character(text),
                          vapply(participant_ids, private$handle_of, character(1)))
          } else NA_character_
          private$state$addressed <- if (!is.na(target)) {
            list(participant = target, message_id = msg$message_id)
          }
        }
      }
      invisible(msg$message_id)
    },

    # Helper to get filtered log for analysis methods. The synthetic setup
    # "System" roster message is part of the prompt context, not of the
    # conversation, so it is dropped by default: no analysis or plot should
    # report "System" as a speaker. Explicitly requesting speaker_ids
    # containing "System" (or include_system = TRUE) keeps it.
    get_filtered_log = function(message_ids = NULL, speaker_ids = NULL,
                                include_system = FALSE) {
      if (length(self$conversation_log) == 0) return(list())

      filtered_log <- self$conversation_log
      if (!include_system && !("System" %in% (speaker_ids %||% character(0)))) {
        filtered_log <- Filter(function(x) !identical(x$speaker_id, "System"), filtered_log)
      }
      if (!is.null(message_ids)) {
        if (!is.numeric(message_ids)) {
          stop("`message_ids` must be a numeric vector.", call. = FALSE)
        }
        filtered_log <- Filter(function(x) x$message_id %in% message_ids,
                               filtered_log)
      }
      if (!is.null(speaker_ids)) {
        if(!is.character(speaker_ids)) stop("'speaker_ids' must be a character vector.")
        filtered_log <- Filter(function(x) x$speaker_id %in% speaker_ids, filtered_log)
      }
      return(filtered_log)
    },

    current_phase_index = 0L,
    last_summary_round = NULL,
    current_round = 0L,
    participant_turns = 0L,
    pending_calls = integer(),
    state = NULL,
    plan = list(injections = list(), probes = list(), pause_at = NULL),
    fired = character(),
    arm = NULL,

    # A session runs once.
    check_unused = function() {
      if (length(self$conversation_log)) {
        stop(
          "This FocusGroup object already contains a session and cannot run again. ",
          "Use the continuation experiment in run_focus_studio() to generate a continuation.",
          call. = FALSE
        )
      }
    },

    # An injection prepared for this session's records, as prepare_plan()
    # prepares the planned ones. An injection from the moderator's ID or
    # handle is the moderator's; one from a participant's ID or handle (the
    # handle in any case) is that participant's turn, recorded under its ID.
    plan_entry = function(injection) {
      if (identical(injection$from, self$moderator_id) ||
          identical(injection$from, private$handle_of(self$moderator_id))) {
        injection$from <- "moderator"
      }
      participant_ids <- setdiff(names(self$agents), self$moderator_id)
      handles <- vapply(participant_ids, private$handle_of, character(1))
      by_handle <- participant_ids[match(tolower(injection$from), tolower(handles))]
      # A name that is one participant's ID and another's handle, as under
      # permuted handles, could mean either.
      if (injection$from %in% participant_ids && !is.na(by_handle) &&
          !identical(by_handle, injection$from)) {
        stop(sprintf(paste0(
          "An injection from '%s' is ambiguous: '%s' is the ID of participant %s, ",
          "who is shown as %s, and the handle of participant %s. Give the ",
          "participants handles that are not other participants' IDs to inject ",
          "their turns."), injection$from, injection$from, injection$from,
          handles[[injection$from]], by_handle), call. = FALSE)
      }
      who <- if (injection$from %in% participant_ids) injection$from else by_handle
      if (length(who) == 1L && !is.na(who)) {
        injection$from <- who
        injection$as_turn <- TRUE
      }
      injection$event <- .fg_point_event(injection$at)
      injection
    },

    # The transcript of the messages that fall before the prompt window, for
    # the interim summary; "" when the window still holds every message. The
    # System roster row is not discussion and is left out, and so is a
    # message injected for the participants alone, since the moderator reads
    # the summary. The agents read the summary, so the speakers are shown
    # under their handles.
    get_dropped_transcript_for_summary = function(max_tokens_history) {
        log <- private$agent_view()
        if (length(log) == 0) return("")
        start <- .fg_window_start(log, max_tokens_history)
        dropped <- Filter(function(msg) {
          !identical(msg$speaker_id, "System") &&
            !identical(msg$metadata$injection$visibility, "participants")
        }, log[seq_len(start - 1L)])
        if (!length(dropped)) return("")
        paste(vapply(dropped, function(msg) {
            paste0(msg$speaker_id, ": ", msg$text)
        }, character(1)), collapse = "\n\n")
    }
  )
)

#' Print a FocusGroup
#'
#' @param x A `FocusGroup` object.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @keywords internal
#' @export
print.FocusGroup <- function(x, ...) {
  rounds <- if (length(x$conversation_log)) {
    vapply(x$conversation_log, function(message) {
      as.integer(message$round %||% NA_integer_)
    }, integer(1))
  } else {
    integer(0)
  }
  rounds <- unique(rounds[!is.na(rounds) & rounds > 0L])

  cat("<FocusGroup>\n")
  cat("  Topic: ", x$topic, "\n", sep = "")
  cat("  Participants: ",
      length(setdiff(names(x$agents), x$moderator_id)), "\n", sep = "")
  cat("  Messages: ", length(x$conversation_log),
      " in ", length(rounds), " round(s)\n", sep = "")
  status <- attr(x, "session_status")
  if (!is.null(status)) {
    cat("  Status: ", if (isTRUE(status$complete)) "complete" else
        paste0("incomplete (stopped during ", status$failure$phase %||% "an unknown phase", ")"),
        "\n", sep = "")
  }
  invisible(x)
}
