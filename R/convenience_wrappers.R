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

# Convert numeric phase counts or user-supplied phase scripts to the structure
# consumed by FocusGroup. Numeric values select distinct defaults; character
# vectors supply the exact ordered moderator questions or instructions.
.fg_build_question_script <- function(guide, topic) {
  if (is.null(guide)) return(list())
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
      if (anyNA(value) || any(!nzchar(trimws(value)))) {
        stop("Character scripts for phase '", phase,
             "' must be non-missing and non-empty.", call. = FALSE)
      }
      unname(value)
    } else {
      if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
          !is.finite(value) || value < 0 || value != floor(value)) {
        stop("The numeric count for phase '", phase,
             "' must be one non-negative whole number.", call. = FALSE)
      }
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
#' focus group session. The model configuration is explicit. Before a live run,
#' the function estimates the number of generated model outputs and applies the
#' `max_calls` limit unless `confirm` is `TRUE`.
#'
#' @param topic Character. Focus group topic.
#' @param config An explicit `LLMR::llm_config` used by all agents and group-level
#'   model tasks.
#' @param n_participants Integer. Number of participants, excluding the moderator.
#' @param guide A named numeric vector or named list. Numeric values select that
#'   many moderator instructions from the phase banks. Character vectors supply
#'   the ordered instructions directly.
#' @param demographics Optional participant demographics.
#' @param survey_responses Optional participant survey responses.
#' @param flow Character. One of `"round_robin"`, `"probabilistic"`, or
#'   `"desire_based"`.
#' @param seed Optional integer governing in-package sampling.
#' @param message_mode Character. `"roleflip"` or `"flat"`.
#' @param verbose Logical. Print session progress.
#' @param max_participant_responses Optional integer maximum number of participant
#'   responses per moderator question.
#' @param max_calls Integer. Maximum estimated live model outputs allowed without
#'   confirmation.
#' @param confirm Logical. Permit a live run whose estimate exceeds `max_calls`.
#' @param .runner Optional experiments-frame runner. It receives a data frame
#'   with `config` and `messages` list-columns and returns those rows with at
#'   least `response_text`.
#' @return A `focus_group_result` with the group, transcript, summary,
#'   participant table, token usage, and sanitized metadata.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
#' result <- run_focus_group(
#'   topic = "Library funding priorities",
#'   config = cfg,
#'   n_participants = 4,
#'   guide = list(
#'     Opening = "Welcome the participants and state the ground rules.",
#'     Exploration = "Which funding priority deserves attention first?",
#'     Closing = "Thank the participants and close the session."
#'   ),
#'   flow = "round_robin"
#' )
#' }
#' @export
run_focus_group <- function(topic, config, n_participants = 6,
                            guide = c(Opening = 2, Icebreaker = 3,
                                      Engagement = 8, Exploration = 10,
                                      Closing = 2),
                            demographics = NULL, survey_responses = NULL,
                            flow = "desire_based", seed = NULL,
                            message_mode = c("roleflip", "flat"), verbose = TRUE,
                            max_participant_responses = NULL, max_calls = 100L,
                            confirm = FALSE, .runner = NULL) {
  .fg_validate_config(config)
  message_mode <- match.arg(message_mode)

  if (!is.character(topic) || length(topic) != 1L || is.na(topic) ||
      !nzchar(trimws(topic))) {
    stop("`topic` must be a non-empty character string.", call. = FALSE)
  }
  if (!is.numeric(n_participants) || length(n_participants) != 1L ||
      is.na(n_participants) || n_participants < 1 ||
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
      max_calls < 1 || max_calls != floor(max_calls)) {
    stop("`max_calls` must be one positive integer.", call. = FALSE)
  }
  if (!is.logical(confirm) || length(confirm) != 1L || is.na(confirm)) {
    stop("`confirm` must be `TRUE` or `FALSE`.", call. = FALSE)
  }
  if (!is.null(.runner) && !is.function(.runner)) {
    stop("`.runner` must be `NULL` or a function.", call. = FALSE)
  }

  response_limit <- max_participant_responses %||%
    getOption("focusgroup.max_participant_responses", 3L)
  if (!is.numeric(response_limit) || length(response_limit) != 1L ||
      is.na(response_limit) || response_limit < 1 ||
      response_limit != floor(response_limit)) {
    stop("`max_participant_responses` must be `NULL` or one positive integer.",
         call. = FALSE)
  }
  response_limit <- as.integer(response_limit)

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
  phases <- vapply(estimate_script, `[[`, character(1), "phase")
  question_rounds <- sum(phases %in% response_phases)
  moderator_outputs <- length(estimate_script)
  participant_outputs <- question_rounds * response_limit
  desire_outputs <- if (identical(flow, "desire_based")) {
    participant_outputs * n_participants
  } else {
    0L
  }
  estimated_calls <- as.integer(
    moderator_outputs + participant_outputs + desire_outputs + 1L
  )
  if (is.null(.runner) && estimated_calls > max_calls && !confirm) {
    stop(
      "Estimated model outputs (", estimated_calls, ") exceed `max_calls` (",
      as.integer(max_calls), "). Increase `max_calls` or set `confirm = TRUE` ",
      "after reviewing the guide and flow.", call. = FALSE
    )
  }

  if (!is.null(seed)) withr::local_seed(as.integer(seed))
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
    turn_taking_flow = create_conversation_flow(flow, agents, moderator_id),
    question_script = question_script,
    admin_config = config,
    max_participant_responses = response_limit
  )

  withr::with_options(
    list(focusgroup.message_mode = message_mode),
    fg$run_simulation(verbose = verbose)
  )

  result <- .fg_focus_group_result(
    focus_group = fg,
    flow = flow,
    message_mode = message_mode,
    n_participants = n_participants,
    estimated_calls = estimated_calls
  )

  if (verbose) cat("Simulation complete.\n")
  result
}

.fg_validate_config <- function(config) {
  if (missing(config) || is.null(config) || !inherits(config, "llm_config")) {
    stop("`config` must be an explicit `llm_config` object.", call. = FALSE)
  }
  invisible(config)
}

.fg_transcript_data <- function(log) {
  out <- data.frame(
    message_id = integer(), round = integer(), speaker_id = character(),
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
  out <- data.frame(
    message_id = vapply(log, function(x) as.integer(scalar(x$message_id, NA_integer_)), integer(1)),
    round = vapply(log, function(x) as.integer(scalar(x$round, NA_integer_)), integer(1)),
    speaker_id = vapply(log, function(x) as.character(scalar(x$speaker_id, "")), character(1)),
    is_moderator = vapply(log, function(x) isTRUE(x$is_moderator), logical(1)),
    text = vapply(log, function(x) as.character(scalar(x$text, "")), character(1)),
    phase = vapply(log, function(x) as.character(scalar(x$phase, "unknown")), character(1)),
    response_id = vapply(log, function(x) as.character(scalar(x$response_id, NA_character_)), character(1)),
    finish_reason = vapply(log, function(x) as.character(scalar(x$finish_reason, NA_character_)), character(1)),
    sent_tokens = vapply(log, function(x) as.integer(scalar(x$sent_tokens, NA_integer_)), integer(1)),
    rec_tokens = vapply(log, function(x) as.integer(scalar(x$rec_tokens, NA_integer_)), integer(1)),
    total_tokens = vapply(log, function(x) as.integer(scalar(x$total_tokens, NA_integer_)), integer(1)),
    duration_s = vapply(log, function(x) as.numeric(scalar(x$duration_s, NA_real_)), numeric(1)),
    provider = vapply(log, function(x) as.character(scalar(x$provider, NA_character_)), character(1)),
    model = vapply(log, function(x) as.character(scalar(x$model, NA_character_)), character(1)),
    timestamp = as.POSIXct(vapply(
      log, function(x) as.numeric(scalar(x$timestamp, as.POSIXct(NA))), numeric(1)
    ), origin = "1970-01-01"),
    stringsAsFactors = FALSE
  )
  out$metadata <- I(lapply(log, function(x) x$metadata %||% list()))
  out
}

.fg_participant_data <- function(agents) {
  out <- data.frame(
    id = vapply(agents, function(agent) agent$id, character(1)),
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
  out[c("id", "role", "is_moderator", "persona", "demographics",
        "survey_responses", "provider", "model")]
}

.fg_focus_group_result <- function(focus_group, flow, message_mode,
                                   n_participants, estimated_calls) {
  transcript <- .fg_transcript_data(focus_group$conversation_log)
  participant_data <- .fg_participant_data(focus_group$agents)
  sent <- .fg_tok0(focus_group$total_tokens_sent)
  rec <- .fg_tok0(focus_group$total_tokens_received)
  moderator_config <- focus_group$agents[[focus_group$moderator_id]]$config
  structure(
    list(
      focus_group = focus_group,
      transcript = transcript,
      summary = focus_group$final_summary %||% character(),
      participants = participant_data,
      usage = list(sent = sent, rec = rec, total = sent + rec),
      metadata = list(
        topic = focus_group$topic,
        purpose = focus_group$purpose,
        flow = flow,
        message_mode = message_mode,
        n_participants = as.integer(n_participants),
        estimated_calls = as.integer(estimated_calls),
        provider = moderator_config$provider %||% NA_character_,
        model = moderator_config$model %||% NA_character_
      )
    ),
    class = c("focus_group_result", "list")
  )
}

#' @export
print.focus_group_result <- function(x, ...) {
  cat("<focus_group_result>\n")
  cat("Topic: ", x$metadata$topic, "\n", sep = "")
  cat("Participants: ", x$metadata$n_participants,
      " | Messages: ", nrow(x$transcript),
      " | Tokens: ", x$usage$total, "\n", sep = "")
  invisible(x)
}

#' Create Focus Group Agents
#'
#' Creates participant agents from supplied records, direct personas, or
#' generated profiles, and adds a moderator.
#'
#' @param n_participants Integer number of participants to create
#' @param config An explicit `LLMR::llm_config` for all agents.
#' @param demographics Optional data frame with demographics. If NULL, generates diverse demographics.
#' @param survey_responses Optional data frame with survey responses. If NULL, generates responses.
#' @param direct_persona_descriptions Optional character vector of pre-rendered
#'   participant personas. When supplied, these descriptions are used directly
#'   and are recycled in order if necessary. Demographics and survey responses
#'   remain attached as raw reporting fields.
#' @param .runner Optional experiments-frame runner stored on every agent.
#'
#' @return A named list of FGAgent objects (participants + 1 moderator), keyed
#'   by agent ID.
#'
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
#' agents <- create_agents(6, config = cfg)
#'
#' # Create with custom demographics
#' demo_data <- data.frame(
#'   age = c(22, 35, 28, 41, 19, 33),
#'   gender = c("Female", "Male", "Male", "Female", "Male", "Female"),
#'   education = c("Bachelor's", "Master's", "High School", "PhD", "Some College", "Bachelor's")
#' )
#' agents <- create_agents(6, config = cfg, demographics = demo_data)
#' }
#'
#' @export
create_agents <- function(n_participants, config, demographics = NULL,
                          survey_responses = NULL,
                          direct_persona_descriptions = NULL, .runner = NULL) {

  .fg_validate_config(config)

  if (!is.numeric(n_participants) || length(n_participants) != 1L ||
      is.na(n_participants) || n_participants < 1 ||
      n_participants != floor(n_participants)) {
    stop("`n_participants` must be a single positive integer.", call. = FALSE)
  }
  n_participants <- as.integer(n_participants)

  # Generate diverse demographics if not provided
  if (is.null(demographics)) {
    demographics <- generate_diverse_demographics(n_participants)
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

  # Direct personas do not need synthetic survey answers.
  if (is.null(survey_responses) && is.null(direct_persona_descriptions)) {
    survey_responses <- generate_survey_responses(n_participants)
  }

  # A data.frame with fewer rows than participants is recycled by row, openly.
  # (Without this, the is.list() branch below would treat the frame as a list
  # of COLUMNS and hand agent i a whole column as its persona data.)
  if (is.data.frame(demographics) && nrow(demographics) > 0 &&
      nrow(demographics) < n_participants) {
    warning(sprintf("`demographics` has %d rows for %d participants; rows are recycled in order.",
                    nrow(demographics), n_participants), call. = FALSE)
  }
  if (is.data.frame(survey_responses) && nrow(survey_responses) > 0 &&
      nrow(survey_responses) < n_participants) {
    warning(sprintf("`survey_responses` has %d rows for %d participants; rows are recycled in order.",
                    nrow(survey_responses), n_participants), call. = FALSE)
  }

  agents <- list()

  # Create participants

  for (i in seq_len(n_participants)) {
    # data.frame row -> named list of non-missing question:value pairs.
    # drop = FALSE matters: a single-column frame would otherwise collapse to a
    # vector and lose the column names.
    row_to_list <- function(df, i) {
      row <- df[i, , drop = FALSE]
      out <- list()
      for (col in names(row)) {
        v <- row[[col]]
        if (length(v) && !is.na(v) && nzchar(v) && !identical(as.character(v), "NA")) {
          out[[col]] <- as.character(v)
        }
      }
      out
    }

    agent_demographics <- if (!is.null(demographics) && is.data.frame(demographics) && nrow(demographics) > 0) {
      row_to_list(demographics, ((i - 1L) %% nrow(demographics)) + 1L)
    } else if (!is.null(demographics) && !is.data.frame(demographics) && is.list(demographics) && length(demographics) >= i) {
      demographics[[i]]
    } else {
      list(
        age = sample(18:65, 1),
        gender = sample(c("Male","Female","Nonbinary"), 1, prob = c(0.49,0.49,0.02))
      )
    }

    agent_survey <- if (!is.null(survey_responses) && is.data.frame(survey_responses) && nrow(survey_responses) > 0) {
      row_to_list(survey_responses, ((i - 1L) %% nrow(survey_responses)) + 1L)
    } else if (!is.null(survey_responses) && !is.data.frame(survey_responses) && is.list(survey_responses) && length(survey_responses) >= i) {
      survey_responses[[i]]
    } else {
      NULL
    }

    persona <- if (!is.null(direct_persona_descriptions)) {
      direct_persona_descriptions[
        ((i - 1L) %% length(direct_persona_descriptions)) + 1L
      ]
    } else {
      generate_persona(agent_demographics, agent_survey)
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

  return(agents)
}

#' Analyze Focus Group Results
#'
#' Runs descriptive analyses offline. Thematic analysis and a model-generated
#' summary are opt-in and run only when `config` is supplied.
#'
#' @param focus_group_result A `FocusGroup` object or `focus_group_result`.
#' @param num_topics Integer number of topics for topic modeling.
#' @param include_plots Logical. Attempt the four descriptive plots.
#' @param config Optional explicit `LLMR::llm_config`. When supplied, thematic
#'   analysis and a model summary are generated.
#' @param .runner Optional experiments-frame runner for the two model analyses.
#' @return A `focus_group_analysis` with fixed fields and an `issues` table.
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
                                config = NULL,
                                .runner = NULL) {
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
  topics <- capture_component(
    "topics", function() fg$analyze_topics(num_topics = as.integer(num_topics)),
    list()
  )
  empty_tfidf <- dplyr::tibble(
    speaker_id = character(), term = character(), tf_idf = double()
  )
  tfidf <- capture_component(
    "tfidf", function() fg$analyze_tfidf(),
    empty_tfidf
  )
  if (!is.data.frame(tfidf) || !nrow(tfidf)) tfidf <- empty_tfidf

  empty_readability <- dplyr::tibble(speaker_id = character())
  readability <- capture_component(
    "readability", function() fg$analyze_readability(),
    empty_readability
  )
  if (!is.data.frame(readability) || !nrow(readability)) {
    readability <- empty_readability
  }

  themes <- character()
  model_summary <- character()
  if (!is.null(config)) {
    themes <- fg$analyze_themes(config = config, .runner = .runner)
    model_summary <- fg$summarize(config = config, .runner = .runner)
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
  cat("Speakers: ", nrow(x$basic_stats$speaker_stats),
      " | Topics: ", length(x$topics),
      " | Issues: ", nrow(x$issues), "\n", sep = "")
  cat("Model analysis: ",
      if (length(x$themes) || length(x$model_summary)) "included" else "not requested",
      "\n", sep = "")
  invisible(x)
}

#' Generate Diverse Demographics
#'
#' Internal function to generate diverse demographic profiles for participants.
#'
#' @param n Integer number of profiles to generate
#' @return Data frame with demographic information
#'
#' @keywords internal
generate_diverse_demographics <- function(n) {
  seed_val <- getOption("focusgroup.seed", NA_integer_)
  build <- function() {
    ages <- sample(18:65, n, replace = TRUE)
    genders <- sample(c("Male", "Female", "Nonbinary"), n, replace = TRUE, prob = c(0.49, 0.49, 0.02))

    education_levels <- c("High School", "Some College", "Bachelor's", "Master's", "PhD")
    education <- sample(education_levels, n, replace = TRUE, prob = c(0.2, 0.2, 0.35, 0.2, 0.05))

    income_levels <- c("Under $30k", "$30k-$50k", "$50k-$75k", "$75k-$100k", "Over $100k")
    income <- sample(income_levels, n, replace = TRUE, prob = c(0.15, 0.25, 0.25, 0.20, 0.15))

    locations <- c("Urban", "Suburban", "Rural")
    location <- sample(locations, n, replace = TRUE, prob = c(0.4, 0.4, 0.2))

    data.frame(
      age = ages,
      gender = genders,
      education = education,
      income = income,
      location = location,
      stringsAsFactors = FALSE
    )
  }
  # with_seed keeps the seeding contained: the draw is deterministic but the
  # caller's RNG stream is restored afterwards.
  if (!is.na(seed_val)) withr::with_seed(as.integer(seed_val), build()) else build()
}

#' Generate Survey Responses
#'
#' Internal function to generate diverse survey responses for participants.
#'
#' @param n Integer number of response sets to generate
#' @return Data frame with survey responses
#'
#' @keywords internal
generate_survey_responses <- function(n) {
  # Generate responses on Likert scales for common topics
  tech_usage <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.15, 0.3, 0.3, 0.15))
  social_media <- sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.2, 0.4, 0.25))
  privacy_concern <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.25, 0.3, 0.15))
  env_concern <- sample(1:5, n, replace = TRUE, prob = c(0.05, 0.1, 0.15, 0.35, 0.35))

  data.frame(
    tech_usage_comfort = tech_usage,
    social_media_frequency = social_media,
    privacy_concern_level = privacy_concern,
    environmental_concern = env_concern,
    stringsAsFactors = FALSE
  )
}

#' Generate a participant persona from demographics and survey responses
#'
#' Renders the persona text for a synthetic participant from the demographics and
#' survey responses the researcher supplied. It states those facts and nothing
#' more: what a given age, education, place of residence, or survey answer implies
#' about a person is left to the model, not decided here. Mapping a demographic
#' label to a fixed disposition is the essentialism a research instrument should
#' avoid, so the package does not do it.
#'
#' Both the demographics and the survey responses are rendered in full (every
#' supplied field), not a fixed subset. Pass survey responses keyed by the
#' question wording (for survey-file input, the variable label is used as the key)
#' so the model sees the item, not a code name.
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

  # Normalize a one-row data frame to a named list; drop NA/empty fields so they
  # are simply absent rather than rendered as "NA".
  to_named_list <- function(x) {
    if (is.null(x)) return(list())
    if (is.data.frame(x)) x <- as.list(x[1, , drop = FALSE])
    if (!is.list(x)) x <- as.list(x)
    x <- lapply(x, function(v) if (length(v)) v[[1]] else NA)
    keep <- vapply(x, function(v) {
      !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(as.character(v)))
    }, logical(1))
    x[keep]
  }

  # Strip leftover code prefixes/suffixes that some labeled surveys carry on a
  # value label (e.g. "1. Male", "Male (R volunteered)"). A no-op on plain text.
  tidy_value <- function(x) {
    x <- as.character(x)
    x <- gsub("^[0-9-]+\\.\\s*", "", x)
    x <- gsub("\\s*\\([^)]*\\)$", "", x)
    trimws(x)
  }

  demos <- to_named_list(demographics)
  demos <- lapply(demos, tidy_value)
  resp  <- to_named_list(survey_responses)
  resp  <- lapply(resp, tidy_value)

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

# Default missing-value vocabulary for labeled surveys. Configurable per call.
.fg_default_na_strings <- c("inapplicable", "missing", "refused",
                            "don't know", "don.t know", "not asked",
                            "legitimate skip")

# Decode a haven column to its value label (character); leave plain columns as
# character. NA out values matching `na_strings`.
.fg_decode_col <- function(col, na_strings) {
  v <- if (inherits(col, c("haven_labelled", "labelled"))) {
    as.character(haven::as_factor(col, levels = "labels"))
  } else as.character(col)
  if (length(na_strings)) {
    pat <- paste(na_strings, collapse = "|")
    v[grepl(pat, v, ignore.case = TRUE)] <- NA
  }
  v
}

# Choose the column key shown to the model: the human variable label (question
# wording) when present, else the column name. `named` overrides take priority.
.fg_col_key <- function(raw_lab, code, override = NULL) {
  if (!is.null(override) && nzchar(override)) return(override)
  lab <- tryCatch(attr(raw_lab[[code]], "label"), error = function(...) NULL)
  if (!is.null(lab) && nzchar(lab)) lab else code
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
  idx <- seq_len(N)
  if (!is.null(rows)) {
    idx <- if (is.function(rows)) which(rows(demo_df)) else {
      if (is.logical(rows)) which(rows) else as.integer(rows)
    }
    idx <- idx[idx >= 1 & idx <= N]
    if (!length(idx)) stop("`rows` selected no eligible respondents.")
  }

  seed_val <- getOption("focusgroup.seed", NA_integer_)

  prob <- NULL
  if (!is.null(weights)) {
    w <- suppressWarnings(as.numeric(weights))[idx]
    w[is.na(w) | w < 0] <- 0
    if (sum(w) > 0) prob <- w / sum(w)
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

  create_agents(
    n_participants = n_participants,
    config = config,
    demographics = demo_sample,
    survey_responses = survey_sample,
    direct_persona_descriptions = persona_sample,
    .runner = .runner
  )
}

#' Create agents from a labeled survey file
#'
#' Reads a labeled survey file (Stata `.dta`, SPSS `.sav`, or SAS
#' `.sas7bdat`), decodes the chosen variables from their value labels, and turns
#' each selected respondent into an `FGAgent` whose persona states that
#' respondent's demographics and survey answers. Numeric codes are decoded from
#' the file's own value labels, so the same call works on ANES, GSS, WVS, or any
#' other labeled file; nothing about a particular dataset is hard-coded.
#'
#' The survey answers are keyed by each variable's question wording (its label in
#' the file) so the model sees the item, not a code name. The persona draws no
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
#'   be named (names become the question wording shown). If `NULL`, labeled
#'   variables that are not demographics are used.
#' @param rows Optional row selector restricting the eligible respondents before
#'   sampling: an integer or logical vector, or a predicate
#'   `function(df) -> logical` over the decoded demographics frame.
#' @param weights Optional sampling weights: a column name in the file, or a
#'   numeric vector aligned to the file's rows. Used only to weight which
#'   respondents are drawn.
#' @param na_strings Character vector of value-label substrings treated as
#'   missing (case-insensitive). Defaults to a small common set; pass your own to
#'   match another file's missing-data vocabulary.
#' @param .runner Optional experiments-frame runner stored on every agent.
#'
#' @return A named list of `FGAgent` objects (participants + moderator), keyed
#'   by agent ID.
#' @seealso [create_agents_from_data()] for an in-memory data frame, and
#'   `LLMR::anes_2024_personas` for a ready-made example.
#' @examples
#' \dontrun{
#' agents <- create_agents_from_survey(
#'   n_participants = 6,
#'   survey_path = "anes_timeseries_2024_stata.dta",
#'   config = LLMR::llm_config("openai", "gpt-4o-mini"),
#'   demographic_vars = c(age = "V241458x", education = "V241465x"),
#'   survey_vars = c("Party identification" = "V241227x",
#'                   "Ideology" = "V241177"),
#'   rows = function(df) df$age != "18-24"   # exclude the youngest band
#' )
#' }
#' @export
create_agents_from_survey <- function(n_participants,
                                      survey_path,
                                      config,
                                      demographic_vars = NULL,
                                      survey_vars = NULL,
                                      rows = NULL,
                                      weights = NULL,
                                      na_strings = .fg_default_na_strings,
                                      .runner = NULL) {
  .fg_validate_config(config)
  if (!file.exists(survey_path)) stop("Survey file not found at: ", survey_path)

  ext <- tolower(tools::file_ext(survey_path))
  raw_lab <- switch(ext,
    "dta" = haven::read_dta(survey_path),
    "sav" = haven::read_sav(survey_path),
    "sas7bdat" = haven::read_sas(survey_path),
    stop("Unsupported survey file type: .", ext)
  )

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
  # Survey vars: explicit, else any labeled var that is not a demographic.
  if (is.null(survey_vars)) {
    has_label <- vapply(names(raw_lab), function(v) nzchar(label_of(v)), logical(1))
    survey_vars <- setdiff(names(raw_lab)[has_label], unname(demographic_vars))
  }

  demo_codes <- demographic_vars[unname(demographic_vars) %in% names(raw_lab)]
  svy_codes  <- survey_vars[unname(survey_vars) %in% names(raw_lab)]
  if (!length(demo_codes)) stop("None of `demographic_vars` are in the file.")

  demo_keys <- vapply(seq_along(demo_codes), function(i)
    .fg_col_key(raw_lab, unname(demo_codes)[i], names(demo_codes)[i]), "")
  svy_keys <- if (length(svy_codes)) vapply(seq_along(svy_codes), function(i)
    .fg_col_key(raw_lab, unname(svy_codes)[i], names(svy_codes)[i]), "") else character(0)

  demo_df <- as.data.frame(lapply(unname(demo_codes), function(c)
    .fg_decode_col(raw_lab[[c]], na_strings)), stringsAsFactors = FALSE)
  names(demo_df) <- demo_keys
  survey_df <- if (length(svy_codes)) {
    s <- as.data.frame(lapply(unname(svy_codes), function(c)
      .fg_decode_col(raw_lab[[c]], na_strings)), stringsAsFactors = FALSE)
    names(s) <- svy_keys; s
  } else NULL

  # Keep respondents with at least one demographic; the row filter is applied to
  # this SAME frame so demographics and survey answers never misalign.
  keep <- rowSums(!is.na(demo_df)) > 0
  weights_vec <- if (is.character(weights) && length(weights) == 1L &&
                     weights %in% names(raw_lab)) {
    suppressWarnings(as.numeric(haven::zap_labels(raw_lab[[weights]])))
  } else weights
  demo_df <- demo_df[keep, , drop = FALSE]
  if (!is.null(survey_df)) survey_df <- survey_df[keep, , drop = FALSE]
  if (!is.null(weights_vec) && length(weights_vec) == length(keep)) weights_vec <- weights_vec[keep]

  .fg_agents_from_frames(
    demo_df, survey_df, n_participants, config,
    rows = rows, weights = weights_vec, .runner = .runner
  )
}

#' Create agents from an in-memory data frame of respondents
#'
#' Like [create_agents_from_survey()] but starting from a data frame already in
#' memory (for example `LLMR::anes_2024_personas`). Demographic columns are
#' rendered as background; the remaining columns are rendered as survey responses,
#' keyed by their question wording when the frame carries a `dictionary` attribute
#' (see [LLMR::llm_persona_split()]), else by their column names. Values are taken
#' as-is (decode and clean them first if they are still coded).
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
#' @param rows,weights See [create_agents_from_survey()].
#' @param .runner Optional experiments-frame runner stored on every agent.
#' @return A named list of `FGAgent` objects (participants + moderator), keyed
#'   by agent ID.
#' @examples
#' \dontrun{
#' data(anes_2024_personas, package = "LLMR")
#' cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
#' agents <- create_agents_from_data(
#'   anes_2024_personas, n_participants = 6, config = cfg
#' )
#' }
#' @export
create_agents_from_data <- function(data, n_participants, config,
                                    demographic_cols = NULL,
                                    rows = NULL, weights = NULL,
                                    .runner = NULL) {
  stopifnot(is.data.frame(data))
  .fg_validate_config(config)

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
  demographic_cols <- setdiff(intersect(demographic_cols, names(data)),
                              reserved_panel_cols)
  # a score/sort column is not a persona fact
  drop_cols <- intersect(c("ideology_score"), names(data))
  survey_cols <- setdiff(names(data), c(demographic_cols, drop_cols,
                                        reserved_panel_cols))

  to_char <- function(df) {
    if (!ncol(df)) return(data.frame(row.names = seq_len(nrow(df))))
    as.data.frame(lapply(df, function(x) {
      v <- as.character(x); v[!nzchar(v %||% "")] <- NA; v
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

  demo_df <- to_char(data[, demographic_cols, drop = FALSE])
  names(demo_df) <- relabel(demographic_cols)
  survey_df <- if (length(survey_cols)) {
    s <- to_char(data[, survey_cols, drop = FALSE])
    names(s) <- relabel(survey_cols); s
  } else NULL

  wv <- if (is.character(weights) && length(weights) == 1L && weights %in% names(data)) {
    suppressWarnings(as.numeric(data[[weights]]))
  } else weights

  .fg_agents_from_frames(
    demo_df, survey_df, n_participants, config,
    rows = rows, weights = wv,
    direct_persona_descriptions = direct_personas,
    .runner = .runner
  )
}

 
