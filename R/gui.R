# gui.R -------------------------------------------------------------------------
# An optional Shiny GUI for FocusGroup, launched with run_focus_studio(). It has
# three tabs. Run a focus group (live) starts a fresh moderated session and
# offers it as a downloadable .rds and transcript CSV. Analyze loads a
# transcript and runs the package's descriptive analyses offline, with optional
# model analysis. Continuation experiment cuts a conversation at a message and
# compares repeated control responses with several perturbed histories.
#
# shiny, bslib, DT, and the shared LLMR.shiny substrate are Suggests, not
# Imports: a non-GUI user installs none of them, and the analysis package stays
# lean. Every call into those packages is fully qualified and the launcher guards
# on all four. The continuation experiment is built on the existing public API
# (FGAgent$generate_utterance takes the history as a plain string and does not
# read agent-internal state when forming a prompt, so the two branches are
# independent); the target agent is cloned per branch for clean bookkeeping.

#' Launch the FocusGroup Shiny GUI
#'
#' A point-and-click front end with three tabs. Run a focus group starts a fresh
#' moderated session live from a topic and a handful of participants, shows the
#' transcript, and offers a complete \code{.rds} session and transcript CSV.
#' Analyze accepts a finished run, saved session, CSV, or bundled example and
#' runs the package's descriptive analyses offline; thematic analysis and a
#' model summary are optional live actions. The continuation experiment takes a
#' saved focus group up to a message and compares repeated next-message
#' responses under one control history and several labeled treatment histories.
#' Running and the continuation experiment generate text and need an API key;
#' analysis works offline.
#'
#' The GUI is optional. It needs the suggested packages shiny, bslib, DT, and
#' LLMR.shiny; install them first. Keys are read from environment variables only,
#' never pasted into the app.
#'
#' @param ... Passed to \code{shiny::runApp()}.
#' @details
#' The Studio needs LLMR.shiny 0.1.3 or later and stops with an error when an
#' older version is installed.
#'
#' The usage tile in the sidebar counts the model calls each action made and
#' the tokens they reported, including the calls an action made before it
#' failed. When some calls reported no usage, the token counts are shown as
#' lower bounds. If usage cannot be recorded, a notification says so, and the
#' action's results remain available.
#'
#' A run that fails partway keeps the turns it completed: the partial
#' transcript can be analyzed and downloaded, and a notice names the phase in
#' which the run stopped. A downloaded session keeps its completion status and
#' the rows its participants were drawn from, but no model configuration or
#' runner function, since a configuration can hold a literal API key.
#'
#' A continuation from a session saved by FocusGroup 0.5.2 or earlier rebuilds
#' the saved `FGAgent` agents with the current class and their saved persona,
#' style, and records, so that their prompts are unchanged and their usage is
#' recorded as described in [run_focus_group()]; agents of other classes are
#' used as saved.
#'
#' @return Invisibly, the value of \code{shiny::runApp()}; called for the side
#'   effect of starting the app.
#' @examples
#' if (interactive() &&
#'     all(vapply(c("shiny", "bslib", "DT", "LLMR.shiny"),
#'                requireNamespace, logical(1), quietly = TRUE))) {
#'   run_focus_studio()
#' }
#' @export
run_focus_studio <- function(...) {
  .fg_gui_require()
  app <- shiny::shinyApp(ui = .fg_gui_ui(), server = .fg_gui_server)
  shiny::runApp(app, ...)
}

.fg_gui_require <- function() {
  need <- c("shiny", "bslib", "DT", "LLMR.shiny")
  missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("The FocusGroup GUI needs these packages: ",
         paste(missing, collapse = ", "),
         ". Install them, then retry.", call. = FALSE)
  }
  installed <- utils::packageVersion("LLMR.shiny")
  if (installed < "0.1.3") {
    stop("FocusGroup Studio requires LLMR.shiny >= 0.1.3; installed version is ",
         as.character(installed), ". Run install.packages(\"LLMR.shiny\").", call. = FALSE)
  }
  invisible(TRUE)
}

# Add one Studio action's recorded calls and tokens to the session usage tile.
# Accounting never removes the action's results: the usage is recorded even
# when reading it raised problems, and those problems, or a failure of the tile
# update itself, are shown in one notification. `mark` receives TRUE when the
# record is incomplete for either reason.
.fg_account_action <- function(recorder, shared, mark,
                               notify = function(message) shiny::showNotification(message, type = "warning")) {
  usage <- recorder$value()
  tokens <- list(calls = usage$calls, sent = usage$sent, received = usage$rec,
                 total = usage$total, unknown = usage$unknown,
                 sent_unknown = usage$sent_unknown,
                 received_unknown = usage$rec_unknown)
  problems <- unique(recorder$issues())
  # An NA count records that completeness could not be determined, which the
  # tile already shows. LLMR.shiny::usage_add() warns about each such count;
  # those warnings are expected and are not repeated to the user.
  expected <- sprintf("`tokens$%s` is NA",
                      names(tokens)[vapply(tokens, function(x) length(x) == 1L && is.na(x), logical(1))])
  recorded <- tryCatch({
    withCallingHandlers(shared$add_usage(tokens), warning = function(w) {
      text <- conditionMessage(w)
      if (!any(startsWith(text, expected))) problems <<- c(problems, text)
      invokeRestart("muffleWarning")
    })
    TRUE
  }, error = function(e) {
    problems <<- c(problems, conditionMessage(e))
    FALSE
  })
  mark(length(problems) > 0L)
  if (length(problems)) {
    notice <- paste(
      if (recorded) "Usage was recorded, but it may be incomplete." else
        "Usage could not be recorded.",
      "Generated results remain available.",
      paste(unique(problems), collapse = "; "))
    tryCatch(withCallingHandlers(notify(notice), warning = function(w) invokeRestart("muffleWarning")),
             error = function(e) message(notice))
  }
  invisible(recorded && !length(problems))
}

.fg_partial_banner <- function(error) {
  bslib::card(class = "border-warning",
    bslib::card_header("Partial session"),
    bslib::card_body(paste0("A partial result is available. The session stopped during phase '",
      error$phase, "'. ", conditionMessage(error$parent %||% error))))
}

# ---- shell ------------------------------------------------------------------

.fg_gui_ui <- function() {
  bslib::page_navbar(
    title = "FocusGroup",
    id = "main_nav",
    selected = "run",
    fillable = FALSE,
    theme = LLMR.shiny::llmr_theme("focus"),
    sidebar = LLMR.shiny::shell_sidebar(),
    bslib::nav_panel("Run a focus group", value = "run", .fg_run_ui("run")),
    bslib::nav_panel("Analyze transcript", value = "analyze", .fg_analyze_ui("analyze")),
    bslib::nav_panel("Continuation comparison", value = "experiment", .fg_experiment_ui("experiment"))
  )
}

.fg_gui_server <- function(input, output, session) {
  shared <- LLMR.shiny::shell_context(input, output, session)
  shared$active_tab <- shiny::reactive(input$main_nav)
  shared$finished_session <- shiny::reactiveVal(NULL)
  shared$analysis_handoff <- shiny::reactiveVal(NULL)

  shiny::observeEvent(shared$analysis_handoff(), {
    bslib::nav_select("main_nav", "analyze", session = session)
  }, ignoreInit = TRUE)

  .fg_run_server("run", shared)
  .fg_analyze_server("analyze", shared)
  .fg_experiment_server("experiment", shared)
}

# Live runs above this planned API-call count require explicit confirmation.
.fg_large_run_threshold <- 50L

# A live focus-group action is allowed only in live mode WITH a key. The shared
# substrate's can_run() is TRUE in demo mode by design, but the GUI does not
# accept a programmatic runner, so demo mode must never start a run. Use this
# everywhere a live focus-group call is gated.
.fg_live_ok <- function(shared) {
  identical(shared$mode(), "live") && isTRUE(shared$can_run())
}

# A clean display view of a conversation log for the GUI. Drop the System row
# and number the remaining messages by position. Returns a data frame mapping
# the user-facing display_message_id to the original log_index plus speaker
# and text, used by both the Run and Continuation tabs.
.fg_display_messages <- function(log) {
  if (!length(log))
    return(data.frame(display_message_id = integer(0), log_index = integer(0),
                      speaker_id = character(0), text = character(0),
                      stringsAsFactors = FALSE))
  is_real <- vapply(log, function(e) !identical(e$speaker_id, "System"), logical(1))
  idx <- which(is_real)
  data.frame(
    display_message_id = seq_along(idx),
    log_index = idx,
    speaker_id = vapply(idx, function(i) as.character(log[[i]]$speaker_id %||% ""), character(1)),
    text = vapply(idx, function(i) as.character(log[[i]]$text %||% ""), character(1)),
    stringsAsFactors = FALSE)
}

# The `message_id` of display row `j` in a cut history whose System row has
# been dropped: the identifier .fg_perturb_log() matches on. When the log
# lacks complete ids, .fg_perturb_log() indexes by position, and display row
# j is position j of the cut history.
.fg_display_message_id <- function(cut_log, j) {
  ids <- vapply(cut_log, function(m) as.integer(m$message_id %||% NA_integer_),
                integer(1))
  if (anyNA(ids)) as.integer(j) else ids[[j]]
}

.fg_transcript_csv <- function(transcript) {
  transcript <- as.data.frame(transcript, stringsAsFactors = FALSE)
  keep <- vapply(transcript, function(column) {
    is.atomic(column) && !is.list(column)
  }, logical(1))
  transcript[, keep, drop = FALSE]
}

.fg_table_columns <- function(data, text_col = NULL, id_cols = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (is.null(text_col)) {
    text_col <- intersect(
      c("text", "response_text", "utterance", "message", "content", "reason"),
      names(data)
    )
    text_col <- if (length(text_col)) text_col[[1]] else character()
  } else {
    text_col <- intersect(text_col, names(data))
  }
  if (is.null(id_cols)) {
    id_cols <- intersect(
      c(
        "message_id", "response_id", "display_message_id", "log_index",
        "perturb_message_id", "arm_id"
      ),
      names(data)
    )
  } else {
    id_cols <- intersect(id_cols, names(data))
  }

  preferred <- intersect(
    c("round", "speaker_id", "speaker", "role", "phase", text_col),
    names(data)
  )
  remaining <- setdiff(names(data), c(preferred, id_cols))
  order <- c(preferred, remaining, id_cols)
  list(data = data[, order, drop = FALSE], text_col = text_col,
       id_cols = id_cols)
}

.fg_datatable <- function(data, text_col = NULL, id_cols = NULL,
                          digits = 3L, page_length = 8L, dom = NULL,
                          round_cols = character(), round_digits = 2L) {
  arranged <- .fg_table_columns(data, text_col, id_cols)
  shown <- LLMR.shiny::as_display_table(arranged$data, digits = digits)
  round_cols <- intersect(round_cols, names(shown))
  if (length(round_cols)) {
    shown[round_cols] <- arranged$data[round_cols]
  }
  text_target <- match(arranged$text_col, names(shown)) - 1L
  id_targets <- match(arranged$id_cols, names(shown)) - 1L
  short_targets <- match(
    intersect(
      c("round", "speaker_id", "speaker", "role", "phase", "rank", "status"),
      names(shown)
    ),
    names(shown)
  ) - 1L

  column_defs <- list()
  if (length(text_target)) {
    column_defs <- c(column_defs, list(list(
      targets = unname(text_target),
      width = "65%",
      className = "fg-dt-wrap",
      render = DT::JS(paste(
        "function(data, type, row, meta) {",
        "if (type !== 'display') return data;",
        "var safe = $.fn.dataTable.render.text().display(data);",
        paste0(
          "return '<div style=\"white-space:normal;",
          "overflow-wrap:break-word;word-break:normal\">' + safe + '</div>';"
        ),
        "}"
      ))
    )))
  }
  if (length(short_targets)) {
    column_defs <- c(column_defs, list(list(
      targets = unname(short_targets),
      width = "12%"
    )))
  }
  if (length(id_targets)) {
    column_defs <- c(column_defs, list(list(
      targets = unname(id_targets),
      visible = FALSE
    )))
  }

  if (is.null(dom)) dom <- if (length(id_targets)) "Blfrtip" else "lfrtip"
  options <- list(
    pageLength = as.integer(page_length),
    scrollX = TRUE,
    autoWidth = TRUE,
    dom = dom,
    columnDefs = column_defs
  )
  extensions <- list()
  if (length(id_targets)) {
    extensions <- "Buttons"
    options$buttons <- list(list(extend = "colvis", text = "Columns"))
  }

  widget <- DT::datatable(
    shown,
    rownames = FALSE,
    escape = setdiff(names(shown), arranged$text_col),
    extensions = extensions,
    options = options
  )
  if (length(arranged$text_col)) {
    widget <- DT::formatStyle(
      widget,
      arranged$text_col,
      whiteSpace = "normal",
      wordBreak = "normal",
      overflowWrap = "break-word"
    )
  }
  if (length(round_cols)) {
    widget <- DT::formatRound(widget, round_cols, digits = round_digits)
  }
  widget
}

.fg_timing_stats <- function(transcript) {
  df <- if (is.data.frame(transcript)) {
    transcript
  } else if (is.list(transcript)) {
    .fg_transcript_data(transcript)
  } else {
    data.frame()
  }

  wall_seconds <- NA_real_
  if ("timestamp" %in% names(df)) {
    timestamp <- if (inherits(df$timestamp, "POSIXt")) {
      as.POSIXct(df$timestamp)
    } else {
      suppressWarnings(as.POSIXct(as.character(df$timestamp)))
    }
    timestamp <- timestamp[!is.na(timestamp)]
    if (length(timestamp) >= 2L) {
      wall_seconds <- as.numeric(
        difftime(max(timestamp), min(timestamp), units = "secs")
      )
    }
  }

  speaker_stats <- data.frame(
    speaker_id = character(),
    messages_timed = integer(),
    total_seconds = double(),
    mean_seconds = double(),
    stringsAsFactors = FALSE
  )
  if (all(c("speaker_id", "duration_s") %in% names(df))) {
    duration <- suppressWarnings(as.numeric(df$duration_s))
    speaker <- as.character(df$speaker_id)
    keep <- is.finite(duration) & !is.na(speaker) & nzchar(speaker) &
      speaker != "System"
    if (any(keep)) {
      ids <- unique(speaker[keep])
      speaker_stats <- do.call(rbind, lapply(ids, function(id) {
        values <- duration[keep & speaker == id]
        data.frame(
          speaker_id = id,
          messages_timed = length(values),
          total_seconds = sum(values),
          mean_seconds = mean(values),
          stringsAsFactors = FALSE
        )
      }))
      rownames(speaker_stats) <- NULL
    }
  }

  list(
    wall_seconds = wall_seconds,
    speaker_stats = speaker_stats,
    has_wall_time = is.finite(wall_seconds),
    has_durations = nrow(speaker_stats) > 0L
  )
}

.fg_timing_plot <- function(speaker_stats) {
  plot_data <- rbind(
    data.frame(
      speaker_id = speaker_stats$speaker_id,
      metric = "Total seconds",
      seconds = speaker_stats$total_seconds
    ),
    data.frame(
      speaker_id = speaker_stats$speaker_id,
      metric = "Mean seconds per message",
      seconds = speaker_stats$mean_seconds
    )
  )
  plot_data$metric <- factor(
    plot_data$metric,
    levels = c("Total seconds", "Mean seconds per message")
  )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$speaker_id,
      y = .data$seconds,
      fill = .data$metric
    )
  ) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c("Total seconds" = "#2C6E8F",
                 "Mean seconds per message" = "#D36D27")
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Seconds",
      fill = NULL,
      title = "Time taken by speaker"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}

# ---- ANES persona helpers (for the Run tab's participant picker) ------------

# The example personas frame shipped with the package.
.fg_personas <- function() FocusGroup::anes_2024_personas

# Restrict the fields rendered into a persona while retaining the data
# dictionary and demographic classification used by create_agents_from_data().
.fg_persona_columns <- function(data, columns = NULL) {
  if (is.null(columns) || identical(columns, names(data))) return(data)
  columns <- intersect(as.character(columns), names(data))
  out <- data[, columns, drop = FALSE]
  demographic_fields <- intersect(
    attr(data, "demographic_fields") %||% character(), columns
  )
  attr(out, "demographic_fields") <- demographic_fields
  dictionary <- attr(data, "dictionary")
  if (is.data.frame(dictionary) && "handle" %in% names(dictionary)) {
    dictionary <- dictionary[dictionary$handle %in% columns, , drop = FALSE]
  }
  attr(out, "dictionary") <- dictionary
  attr(out, "source") <- attr(data, "source")
  attr(out, "n_demographics") <- length(demographic_fields)
  attr(out, "n_attitudes") <- length(setdiff(
    columns, c(demographic_fields, "ideology_score")
  ))
  out
}

# Render one row by the same demographic/survey split used to construct agents.
.fg_persona_text <- function(data, row, columns = NULL) {
  scoped <- .fg_persona_columns(data, columns)
  row <- as.integer(row)
  if (length(row) != 1L || is.na(row) || row < 1L || row > nrow(scoped)) {
    return("Select a persona row to inspect its full text.")
  }

  demographic_cols <- intersect(
    attr(scoped, "demographic_fields") %||% character(), names(scoped)
  )
  survey_cols <- setdiff(
    names(scoped), c(demographic_cols, "ideology_score")
  )
  dictionary <- attr(scoped, "dictionary")
  relabel <- function(columns) {
    if (!is.data.frame(dictionary) ||
        !all(c("handle", "question") %in% names(dictionary))) {
      return(columns)
    }
    questions <- dictionary$question[match(columns, dictionary$handle)]
    ifelse(is.na(questions), columns, questions)
  }
  values <- function(columns) {
    if (!length(columns)) return(NULL)
    x <- as.list(scoped[row, columns, drop = FALSE])
    names(x) <- relabel(columns)
    x
  }

  generate_persona(values(demographic_cols), values(survey_cols))
}

.fg_gui_phases <- function() {
  c("Opening", "Icebreaker", "Engagement", "Exploration", "Closing")
}

# The compact moderator guide shown in the editor before a user changes it.
.fg_gui_guide <- function(topic) {
  list(
    Opening = "Welcome the participants and state the ground rules.",
    Icebreaker = paste0(
      "What first comes to mind when you think about ", topic, "?"
    ),
    Engagement = c(
      paste0("From your perspective, what matters most about ", topic,
             " in the next year?"),
      paste0("What recent experience shaped your view on ", topic, "?")
    ),
    Exploration = c(
      paste0("What are the trade-offs or tensions around ", topic, "?"),
      paste0("Where do you see common ground or division on ", topic, "?"),
      paste0("What would change your mind on any part of ", topic, "?")
    ),
    Closing = "Thank the participants and close the discussion."
  )
}

.fg_guide_text <- function(questions) {
  paste(as.character(questions), collapse = "\n")
}

.fg_split_guide_text <- function(value) {
  value <- gsub("\r\n?", "\n", as.character(value %||% ""))
  lines <- trimws(strsplit(value, "\n", fixed = TRUE)[[1]])
  lines[nzchar(lines)]
}

.fg_guide_from_text <- function(values, included) {
  phases <- .fg_gui_phases()
  out <- stats::setNames(lapply(phases, function(phase) {
    .fg_split_guide_text(values[[phase]] %||% "")
  }), phases)
  included <- vapply(phases, function(phase) {
    isTRUE(included[[phase]])
  }, logical(1))
  out[included & lengths(out) > 0L]
}

.fg_guide_editor_ui <- function(ns, topic = "public libraries") {
  defaults <- .fg_gui_guide(topic)
  shiny::tagList(
    shiny::tags$p(
      class = "text-muted small",
      paste(
        "Use one moderator question or instruction per line.",
        "Clear a field or remove its phase to skip it."
      )
    ),
    lapply(.fg_gui_phases(), function(phase) {
      key <- tolower(phase)
      bslib::card(
        class = "mb-2",
        bslib::card_header(phase),
        bslib::card_body(
          shiny::checkboxInput(
            ns(paste0("guide_", key, "_included")),
            paste("Include", tolower(phase), "phase"),
            value = TRUE
          ),
          shiny::textAreaInput(
            ns(paste0("guide_", key)),
            "Questions or instructions, one per line",
            value = .fg_guide_text(defaults[[phase]]),
            rows = max(3L, length(defaults[[phase]]) + 1L),
            width = "100%",
            placeholder = paste(
              "Leave empty to skip the", tolower(phase), "phase"
            )
          )
        )
      )
    }),
    shiny::uiOutput(ns("guide_preview"))
  )
}

.fg_gui_run_plan <- function(guide, topic, n_participants, flow,
                             max_participant_responses) {
  script <- if (length(guide)) {
    .fg_build_question_script(guide, topic)
  } else {
    list()
  }
  response_phases <- c(
    "icebreaker_question", "engagement_question", "exploration_question"
  )
  phases <- if (length(script)) {
    vapply(script, `[[`, character(1), "phase")
  } else {
    character()
  }
  question_rounds <- sum(phases %in% response_phases)
  moderator_outputs <- length(script)
  participant_outputs <- as.double(question_rounds) *
    as.double(max_participant_responses)
  desire_outputs <- if (identical(flow, "desire_based")) {
    participant_outputs * as.double(n_participants)
  } else {
    0L
  }
  # The Studio runs with the default call-ons: at each point, at most one
  # call-on line, delivered as written, and one reply per participant.
  call_on_points <- length(.fg_call_on_points(.fg_call_on_names, phases)$items)
  call_on_turns <- as.double(call_on_points) * as.double(n_participants)
  # While the number of participants is not set, the counts that depend on
  # it are unknown, not zero.
  known <- !anyNA(c(desire_outputs, call_on_turns))
  list(
    calls = if (!moderator_outputs) {
      0L
    } else if (!known) {
      NA_integer_
    } else {
      .fg_validate_estimate(
        moderator_outputs + participant_outputs + desire_outputs +
          call_on_turns + 1
      )
    },
    utterances = as.integer(moderator_outputs + participant_outputs +
                              2 * call_on_turns),
    moderator_turns = as.integer(moderator_outputs),
    question_rounds = as.integer(question_rounds),
    participant_outputs = as.integer(participant_outputs),
    desire_calls = as.integer(desire_outputs),
    call_on_turns = as.integer(call_on_turns),
    phases = names(guide)
  )
}

# Build agents from chosen persona rows and run a focus group. `rows` are row
# indices into `data` (empty selects a seeded sample of `n_participants`).
.fg_run_from_personas <- function(topic, n_participants, rows, flow, message_mode,
                                   seed, max_participant_responses,
                                   config, data = NULL, persona_cols = NULL,
                                   guide = NULL, .runner = NULL) {
  data <- data %||% .fg_personas()
  rendered <- .fg_persona_columns(data, persona_cols)
  n <- if (length(rows)) length(unique(rows)) else n_participants
  guide <- guide %||% .fg_gui_guide(topic)
  plan <- .fg_gui_run_plan(guide, topic, n, flow, max_participant_responses)

  # focusgroup.seed must be in place BEFORE the agents are built: the diverse
  # draw inside create_agents_from_data() reads it. Setting it only around
  # run_simulation() would leave the seed input inert on this path. The rows
  # are selected inside create_agents_from_data() so that the roster records
  # their positions in the persona table.
  withr::local_options(list(focusgroup.message_mode = message_mode, focusgroup.seed = seed))
  agents <- create_agents_from_data(rendered, n_participants = n,
    rows = if (length(rows)) rows else NULL, config = config, .runner = .runner)
  agents <- .fg_roster_provenance(agents, data, attr(agents, "source_rows"))
  fg <- FocusGroup$new(topic = topic, purpose = paste("Explore perspectives on", topic),
    agents = agents, moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow(flow, agents, "MOD"),
    question_script = .fg_build_question_script(guide, topic),
    max_participant_responses = max_participant_responses)
  .fg_execute_session(fg, flow, message_mode, n, plan$calls)
}

# ---- run module (live: run a fresh focus group) -----------------------------

.fg_run_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

# `run_fun` is an injection seam: defaults to run_focus_group(), overridden in
# tests with a fake so the module can be exercised via shiny::testServer() with
# no live call.
.fg_run_server <- function(id, shared, run_fun = NULL, persona_run_fun = NULL) {
  run_fun <- run_fun %||% FocusGroup::run_focus_group
  persona_run_fun <- persona_run_fun %||% .fg_run_from_personas
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL)
    err <- shiny::reactiveVal(NULL)
    accounting_incomplete <- shiny::reactiveVal(FALSE)
    pending_run <- shiny::reactiveVal(NULL)
    warn_card <- function(m) bslib::card(class = "border-warning", bslib::card_body(m))
    warn_user <- function(message, ui = warn_card(message)) {
      err(ui)
      shiny::showNotification(message, type = "warning")
      invisible(NULL)
    }

    # The selectable persona table is the shared LLMR.shiny module: rows run
    # liberal -> conservative, multi-select, returning the chosen row indices.
    persona_rows <- LLMR.shiny::persona_selector_server(
      "personas", .fg_personas()
    )

    guide_edited <- shiny::reactiveValues()
    guide_last_default <- shiny::reactiveValues()
    initial_guide <- .fg_gui_guide("public libraries")
    for (phase in .fg_gui_phases()) {
      local({
        current_phase <- phase
        key <- tolower(current_phase)
        input_id <- paste0("guide_", key)
        guide_edited[[key]] <- FALSE
        guide_last_default[[key]] <- .fg_guide_text(
          initial_guide[[current_phase]]
        )
        shiny::observeEvent(input[[input_id]], {
          value <- gsub("\r\n?", "\n", input[[input_id]] %||% "")
          expected <- gsub(
            "\r\n?", "\n", guide_last_default[[key]] %||% ""
          )
          guide_edited[[key]] <- !identical(value, expected)
        }, ignoreInit = TRUE)
      })
    }

    shiny::observeEvent(input$topic, {
      topic <- trimws(input$topic %||% "")
      if (!nzchar(topic)) topic <- "public libraries"
      defaults <- .fg_gui_guide(topic)
      for (phase in .fg_gui_phases()) {
        key <- tolower(phase)
        if (!isTRUE(guide_edited[[key]])) {
          value <- .fg_guide_text(defaults[[phase]])
          guide_last_default[[key]] <- value
          shiny::updateTextAreaInput(
            session,
            paste0("guide_", key),
            value = value
          )
        }
      }
    }, ignoreInit = TRUE)

    guide_for_run <- shiny::reactive({
      topic <- trimws(input$topic %||% "")
      if (!nzchar(topic)) topic <- "public libraries"
      defaults <- .fg_gui_guide(topic)
      values <- stats::setNames(lapply(.fg_gui_phases(), function(phase) {
        input[[paste0("guide_", tolower(phase))]] %||%
          .fg_guide_text(defaults[[phase]])
      }), .fg_gui_phases())
      included <- stats::setNames(lapply(.fg_gui_phases(), function(phase) {
        input[[paste0("guide_", tolower(phase), "_included")]] %||% TRUE
      }), .fg_gui_phases())
      .fg_guide_from_text(values, included)
    })

    estimate_run <- function() {
      p <- as.integer(input$n_participants %||% 3L)
      selected_rows <- persona_rows()
      if (identical(input$source %||% "synthetic", "anes") &&
          length(selected_rows)) {
        p <- length(selected_rows)
      }
      mr <- as.integer(input$max_resp %||% 1L)
      topic <- trimws(input$topic %||% "")
      if (!nzchar(topic)) topic <- "public libraries"
      .fg_gui_run_plan(
        guide_for_run(),
        topic,
        n_participants = p,
        flow = input$flow %||% "round_robin",
        max_participant_responses = mr
      )
    }

    run_action_state <- shiny::reactive({
      if (identical(shared$mode(), "demo")) {
        return(list(
          disabled = TRUE,
          reason = "Switch to Live mode to run this focus group."
        ))
      }
      if (!isTRUE(shared$can_run())) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Set the selected provider's API key in the environment to run",
            "this focus group."
          )
        ))
      }
      if (!nzchar(trimws(shared$model() %||% ""))) {
        return(list(
          disabled = TRUE,
          reason = "Enter a model in the sidebar to run this focus group."
        ))
      }
      selected_rows <- persona_rows()
      source <- input$source %||% "synthetic"
      participants <- suppressWarnings(
        as.numeric(input$n_participants %||% 3L)
      )
      if (!(identical(source, "anes") && length(selected_rows)) &&
          (length(participants) != 1L || is.na(participants) ||
           !is.finite(participants) || participants < 2L ||
           participants > 6L || participants != floor(participants))) {
        return(list(
          disabled = TRUE,
          reason = "Set participants to an integer from 2 through 6."
        ))
      }
      max_responses <- suppressWarnings(
        as.numeric(input$max_resp %||% 1L)
      )
      if (length(max_responses) != 1L || is.na(max_responses) ||
          !is.finite(max_responses) || max_responses < 1L ||
          max_responses > 3L || max_responses != floor(max_responses)) {
        return(list(
          disabled = TRUE,
          reason = "Set maximum responses to an integer from 1 through 3."
        ))
      }
      seed <- suppressWarnings(as.numeric(input$seed %||% 110L))
      if (length(seed) != 1L || !is.finite(seed) ||
          seed < 0 || seed != floor(seed)) {
        return(list(
          disabled = TRUE,
          reason = "Set the seed to one nonnegative integer."
        ))
      }
      if (!nzchar(trimws(input$topic %||% "public libraries"))) {
        return(list(
          disabled = TRUE,
          reason = "Enter a topic to run this focus group."
        ))
      }
      if (!length(guide_for_run())) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Include a guide phase with a moderator question or instruction",
            "to run this focus group."
          )
        ))
      }
      persona_cols <- input$persona_cols
      if (is.null(persona_cols)) persona_cols <- names(.fg_personas())
      if (identical(input$source %||% "synthetic", "anes") &&
          !length(persona_cols)) {
        return(list(
          disabled = TRUE,
          reason = "Select at least one persona column for ANES participants."
        ))
      }
      list(disabled = FALSE, reason = NULL)
    })

    output$module_ui <- shiny::renderUI({
      demo_mode <- identical(shared$mode(), "demo")
      bslib::card(
        bslib::card_header("Run a focus group"),
        bslib::card_body(
          shiny::uiOutput(ns("err")),
          shiny::tags$p(class = "text-muted",
            if (demo_mode) {
              paste(
                "Configure a session and inspect the bundled example below.",
                "The example is deterministic and makes no model or API calls."
              )
            } else {
              paste(
                "Run a moderated session, then download the saved focus group",
                "for use in the Continuation Experiment tab.",
                "The provider and model come from the sidebar."
              )
            }),
          bslib::accordion(
            id = ns("run_sections"),
            open = "study",
            bslib::accordion_panel(
              "1. Study",
              value = "study",
              shiny::textInput(
                ns("topic"),
                "Topic",
                value = "public libraries",
                width = "100%"
              )
            ),
            bslib::accordion_panel(
              "2. Discussion guide",
              value = "guide",
              .fg_guide_editor_ui(ns)
            ),
            bslib::accordion_panel(
              "3. Participants",
              value = "participants",
              shiny::numericInput(
                ns("n_participants"),
                "Participants",
                value = 3,
                min = 2,
                max = 6,
                step = 1
              ),
              shiny::radioButtons(
                ns("source"),
                shiny::tagList(
                  "Participants drawn from ",
                  LLMR.shiny::help_tip(
                    "Use demographic fields alone, or select respondent fields to include in each persona."
                  )
                ),
                choices = c(
                  "ANES demographics" = "synthetic",
                  "ANES respondent fields" = "anes"
                ),
                selected = "synthetic",
                inline = TRUE
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == 'anes'", ns("source")),
                shiny::tags$p(
                  class = "text-muted small",
                  paste(
                    "Pick rows to use as participants. The list runs from",
                    "most liberal at the top to most conservative at the",
                    "bottom. Select none to sample from the available records",
                    "automatically."
                  )
                ),
                LLMR.shiny::persona_selector_ui(ns("personas")),
                shiny::tags$style(
                  ".fg-persona-column-picker .selectize-input {",
                  "max-height: 10rem; overflow-y: auto;",
                  "}"
                ),
                shiny::tags$div(
                  class = "fg-persona-column-picker",
                  shiny::selectizeInput(
                    ns("persona_cols"),
                    "Columns rendered into each persona",
                    choices = names(.fg_personas()),
                    selected = names(.fg_personas()),
                    multiple = TRUE,
                    options = list(
                      plugins = list("remove_button"),
                      maxOptions = ncol(.fg_personas())
                    )
                  )
                ),
                shiny::uiOutput(ns("persona_detail_ui"))
              )
            ),
            bslib::accordion_panel(
              "4. Conversation",
              value = "conversation",
              shiny::fluidRow(
                shiny::column(4, shiny::selectInput(
                  ns("flow"),
                  shiny::tagList(
                    "Turn-taking flow ",
                    LLMR.shiny::help_tip(
                      "Determines how the next participant is selected."
                    )
                  ),
                  choices = c(
                    "Round robin" = "round_robin",
                    "Desire based" = "desire_based",
                    "Probabilistic" = "probabilistic"
                  ),
                  selected = "round_robin"
                )),
                shiny::column(4, shiny::selectInput(
                  ns("message_mode"),
                  shiny::tagList(
                    "Message construction ",
                    LLMR.shiny::help_tip(
                      "Role flip preserves each speaker's voice; flat sends one labeled transcript prompt."
                    )
                  ),
                  choices = c("Role flip" = "roleflip", "Flat" = "flat"),
                  selected = "roleflip"
                )),
                shiny::column(4, shiny::numericInput(
                  ns("max_resp"),
                  shiny::tagList(
                    "Max responses per question ",
                    LLMR.shiny::help_tip(
                      "Sets how many participant replies follow each moderator question."
                    )
                  ),
                  value = 1,
                  min = 1,
                  max = 3,
                  step = 1
                ))
              ),
              shiny::numericInput(
                ns("seed"),
                shiny::tagList(
                  "Seed ",
                  LLMR.shiny::help_tip(
                    "Controls participant sampling and speaker-order randomness, not model output."
                  )
                ),
                value = 110,
                step = 1
              )
            )
          ),
          shiny::uiOutput(ns("cost_note")),
          if (demo_mode) {
            shiny::tags$p(
              class = "text-muted small",
              "Switch to Live mode to run this configuration."
            )
          } else if (!shared$can_run()) {
            LLMR.shiny::live_run_blocker_ui(shared$key())
          },
          shiny::uiOutput(ns("run_action")),
          shiny::tags$hr(),
          shiny::uiOutput(ns("results"))
        )
      )
    })

    output$run_action <- shiny::renderUI({
      action_state <- run_action_state()
      shiny::tagList(
        shiny::actionButton(
          ns("run"),
          "Run focus group",
          class = "btn-primary",
          disabled = action_state$disabled
        ),
        if (action_state$disabled) {
          shiny::tags$p(
            class = "text-muted small mt-2",
            action_state$reason
          )
        }
      )
    })

    output$err <- shiny::renderUI(err())

    output$persona_detail_ui <- shiny::renderUI({
      rows <- persona_rows()
      if (!length(rows)) {
        return(shiny::tags$p(
          class = "text-muted small",
          "Select one or more persona rows to inspect the full rendered text."
        ))
      }
      selected <- as.character(utils::tail(rows, 1L))
      shiny::tagList(
        shiny::selectInput(
          ns("persona_detail_row"),
          "Inspect selected persona",
          choices = stats::setNames(as.character(rows), paste("Row", rows)),
          selected = selected
        ),
        LLMR.shiny::text_block_output(ns("persona_detail_text"), height = "14rem")
      )
    })

    output$persona_detail_text <- shiny::renderText({
      rows <- persona_rows()
      shiny::req(length(rows))
      row <- as.integer(input$persona_detail_row %||% utils::tail(rows, 1L))
      columns <- input$persona_cols
      if (is.null(columns)) columns <- names(.fg_personas())
      .fg_persona_text(.fg_personas(), row, columns)
    })

    plan_label <- function(plan) {
      sprintf(
        paste(
          "Focus group, %d moderator turns and up to %d transcript",
          "utterances; retries excluded"
        ),
        plan$moderator_turns,
        plan$utterances
      )
    }

    output$guide_preview <- shiny::renderUI({
      plan <- estimate_run()
      if (!plan$moderator_turns) {
        return(shiny::tags$p(
          class = "text-warning small",
          "The current guide has no moderator turns. Include a non-empty phase before running."
        ))
      }
      shiny::tags$p(
        class = "text-muted small",
        sprintf(
          paste(
            "Current guide: %d included phase(s), %d moderator turn(s),",
            "and %d participant-facing question(s)."
          ),
          length(plan$phases),
          plan$moderator_turns,
          plan$question_rounds
        )
      )
    })

    output$cost_note <- shiny::renderUI({
      flow <- input$flow %||% "round_robin"
      extra <- if (identical(flow, "desire_based"))
        paste(
          "Desire based adds one scoring call per eligible participant",
          "before each response."
        ) else ""
      plan <- estimate_run()
      scale <- if (is.na(plan$calls)) {
        "Set the number of participants to see the planned API calls."
      } else if (identical(shared$mode(), "demo")) {
        sprintf(
          "In Live mode, this configuration would make %d API calls and produce up to %d transcript utterances.",
          plan$calls, plan$utterances
        )
      } else {
        sprintf(
          "This configuration plans %d API calls and up to %d transcript utterances.",
          plan$calls, plan$utterances
        )
      }
      shiny::tags$div(
        class = "border rounded p-2 my-2",
        shiny::tags$strong("Run preview"),
        shiny::tags$p(
          class = "text-muted small mb-0",
          paste(
            scale,
            sprintf(
              "The guide contains %d moderator turns in %d included phases.",
              plan$moderator_turns,
              length(plan$phases)
            ),
            if (!is.na(plan$call_on_turns) && plan$call_on_turns > 0L) sprintf(
              paste("The plan counts up to %d replies from participants the",
                    "moderator calls on because they have not spoken."),
              plan$call_on_turns
            ),
            "Retries and incomplete-utterance continuations are excluded and can add calls.",
            sprintf(
              "Live runs above %d planned calls require confirmation.",
              .fg_large_run_threshold
            ),
            extra
          )
        )
      )
    })

    shiny::observe({
      if (!identical(shared$active_tab(), "run")) return()
      plan <- estimate_run()
      if (identical(shared$mode(), "live") && !is.na(plan$calls)) {
        shared$set_plan(plan$calls, plan_label(plan))
      } else {
        shared$set_plan(0L)
      }
    })

    prepare_run <- function() {
      err(NULL)
      result(NULL)
      pending_run(NULL)
      if (identical(shared$mode(), "demo")) {
        warn_user("Switch to Live mode to run a focus group.")
        return(NULL)
      }
      if (!isTRUE(shared$can_run())) {
        warn_user(
          "Live mode requires an API key for the selected provider.",
          LLMR.shiny::live_run_blocker_ui(shared$key())
        )
        return(NULL)
      }

      topic <- trimws(input$topic %||% "")
      if (!nzchar(topic)) {
        warn_user("Enter a topic before running the focus group.")
        return(NULL)
      }
      guide <- guide_for_run()
      if (!length(guide)) {
        warn_user(
          "Include at least one guide phase with a moderator question or instruction."
        )
        return(NULL)
      }
      model <- trimws(shared$model() %||% "")
      if (!nzchar(model)) {
        warn_user("Enter a model in the sidebar before running the focus group.")
        return(NULL)
      }
      provider <- shared$provider()
      cfg_result <- LLMR.shiny::safe_llmr_call(
        LLMR.shiny::build_llm_config(
          provider,
          model,
          temperature = shared$temperature(),
          max_tokens = shared$max_tokens(),
          reasoning_effort = shared$reasoning_effort()
        ),
        provider
      )
      if (!cfg_result$ok) {
        err(cfg_result$ui)
        return(NULL)
      }

      plan <- estimate_run()
      shared$set_plan(plan$calls, plan_label(plan))
      persona_cols <- input$persona_cols
      if (is.null(persona_cols)) persona_cols <- names(.fg_personas())
      if (identical(input$source %||% "synthetic", "anes") &&
          !length(persona_cols)) {
        warn_user("Select at least one persona column for ANES participants.")
        return(NULL)
      }
      list(
        topic = topic,
        provider = provider,
        config = cfg_result$value,
        plan = plan,
        guide = guide,
        source = input$source %||% "synthetic",
        n_participants = as.integer(input$n_participants %||% 3L),
        rows = persona_rows(),
        persona_cols = persona_cols,
        flow = input$flow %||% "round_robin",
        message_mode = input$message_mode %||% "roleflip",
        seed = as.integer(input$seed %||% 110L),
        max_responses = as.integer(input$max_resp %||% 1L)
      )
    }

    execute_run <- function(request) {
      planned <- request$plan$calls
      recorder <- .fg_usage_recorder(LLMR.shiny::build_runner("live"), safe = TRUE)
      on.exit(.fg_account_action(recorder, shared, accounting_incomplete), add = TRUE)
      completed <- 0L
      run_output <- shiny::withProgress(
        message = "Running Focus Group", value = 0,
        {
          live_runner <- recorder$runner
          progress_runner <- function(experiments, ...) {
            rows <- lapply(seq_len(NROW(experiments)), function(i) {
              out <- live_runner(experiments[i, , drop = FALSE], ...)
              before <- completed
              completed <<- completed + 1L
              amount <- (
                min(completed, planned) - min(before, planned)
              ) / planned
              detail <- if (completed <= planned) {
                sprintf(
                  "Completed %d of an estimated %d API calls",
                  completed, planned
                )
              } else {
                sprintf(
                  "Completed %d API calls; %d were estimated",
                  completed, planned
                )
              }
              shiny::incProgress(amount, detail = detail)
              out
            })
            do.call(rbind, rows)
          }

          res <- if (identical(request$source, "anes")) {
            LLMR.shiny::safe_llmr_call(
              persona_run_fun(
                topic = request$topic,
                n_participants = request$n_participants,
                rows = request$rows,
                flow = request$flow,
                message_mode = request$message_mode,
                seed = request$seed,
                max_participant_responses = request$max_responses,
                config = request$config,
                persona_cols = request$persona_cols,
                guide = request$guide,
                .runner = progress_runner
              ),
              request$provider
            )
          } else {
            LLMR.shiny::safe_llmr_call(
              run_fun(
                topic = request$topic,
                config = request$config,
                n_participants = request$n_participants,
                guide = request$guide,
                flow = request$flow,
                seed = request$seed,
                message_mode = request$message_mode,
                verbose = FALSE,
                max_participant_responses = request$max_responses,
                confirm = TRUE,
                .runner = progress_runner
              ),
              request$provider
            )
          }
          if (isTRUE(res$ok) && completed < planned) {
            shiny::incProgress(
              (planned - completed) / planned,
              detail = if (completed > 0L) {
                sprintf(
                  "Run complete after %d API calls; %d were estimated",
                  completed, planned
                )
              } else {
                "Run complete"
              }
            )
          }
          list(
            result = res,
            completed = completed
          )
        }
      )

      res <- run_output$result
      if (!res$ok) {
        if (inherits(res$error, "focusgroup_incomplete_session")) {
          res$value <- res$error$partial
          err(.fg_partial_banner(res$error))
        } else {
          err(res$ui)
          return()
        }
      }
      result(res$value)
      if (is.function(shared$finished_session)) {
        shared$finished_session(res$value)
      }
    }

    shiny::observeEvent(input$run, {
      request <- prepare_run()
      if (is.null(request)) return()
      if (request$plan$calls > .fg_large_run_threshold) {
        pending_run(request)
        shiny::showModal(shiny::modalDialog(
          title = "Confirm Large Live Run",
          shiny::tags$p(sprintf(
            "This run plans %d API calls and up to %d transcript utterances.",
            request$plan$calls, request$plan$utterances
          )),
          shiny::tags$p(
            "Retries and incomplete-utterance continuations are excluded and can add calls."
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_run"),
              sprintf("Run %d Calls", request$plan$calls),
              class = "btn-primary"
            )
          ),
          easyClose = FALSE
        ))
        return()
      }
      execute_run(request)
    })

    shiny::observeEvent(input$confirm_run, {
      request <- pending_run()
      shiny::req(request)
      pending_run(NULL)
      shiny::removeModal()
      execute_run(request)
    })

    # The RDS keeps the agents and session state needed by continuation work.
    # It is sanitized first: no model configurations (a config may carry a
    # literal key) and no runner closures leave the session.
    output$download_rds <- shiny::downloadHandler(
      filename = function() "focus_group.rds",
      content = function(file)
        saveRDS(.fg_sanitize_session(result()$focus_group), file))

    output$download_csv <- shiny::downloadHandler(
      filename = function() "focus_group_transcript.csv",
      content = function(file) {
        utils::write.csv(
          .fg_transcript_csv(result()$transcript),
          file,
          row.names = FALSE,
          na = ""
        )
      }
    )

    shiny::observeEvent(input$analyze_this, {
      finished <- if (is.function(shared$finished_session)) {
        shared$finished_session()
      } else {
        result()
      }
      shiny::req(finished)
      if (is.function(shared$analysis_handoff)) {
        shared$analysis_handoff(list(
          result = finished,
          label = paste0(
            "Finished run: ",
            finished$focus_group$topic %||% "Untitled session"
          )
        ))
      }
    })

    output$results <- shiny::renderUI({
      r <- result()
      # In demo mode (or before any run) show the static example transcript so a
      # user can see the shape of the output without spending anything.
      if (is.null(r)) {
        if (identical(shared$mode(), "demo")) {
          return(shiny::tagList(
            shiny::tags$h5("Example transcript (demo)"),
            DT::DTOutput(ns("demo_transcript"))))
        }
        return(shiny::tags$p(
          class = "text-muted",
          "Run the configured focus group to produce a transcript."
        ))
      }
      timing <- .fg_timing_stats(r$transcript)
      shiny::tagList(
        shiny::tags$h5("Transcript"), DT::DTOutput(ns("transcript")),
        shiny::tags$h5("Usage"),
        LLMR.shiny::text_block_output(ns("usage"), height = "9rem"),
        shiny::tags$h5("Timing"),
        if (timing$has_wall_time) {
          shiny::tags$p(sprintf(
            "Total session wall time: %.1f seconds.",
            timing$wall_seconds
          ))
        } else {
          shiny::tags$p(
            class = "text-muted",
            "Total session wall time is unavailable because the transcript lacks two valid timestamps."
          )
        },
        if (timing$has_durations) {
          shiny::tagList(
            DT::DTOutput(ns("timing_table")),
            if (requireNamespace("ggplot2", quietly = TRUE)) {
              shiny::plotOutput(ns("timing_plot"), height = "320px")
            } else {
              LLMR.shiny::install_guidance_ui(
                "ggplot2", "Timing plot"
              )
            }
          )
        } else {
          shiny::tags$p(
            class = "text-muted",
            "Per-speaker timing is unavailable because no messages contain duration_s values."
          )
        },
        shiny::tags$h5("Save or continue"),
        shiny::tags$p(
          class = "text-muted small",
          paste(
            "The .rds preserves the whole session and can be reloaded in the",
            "Continuation experiment tab. The CSV contains transcript rows",
            "only for spreadsheets and other tools; it cannot reload or",
            "continue a session."
          )
        ),
        bslib::layout_column_wrap(
          width = 1 / 3,
          shiny::downloadButton(
            ns("download_rds"), "Download full session (.rds)"
          ),
          shiny::downloadButton(
            ns("download_csv"), "Download transcript (.csv)"
          ),
          shiny::actionButton(
            ns("analyze_this"), "Analyze this session",
            class = "btn-secondary"
          )
        ))
    })

    output$demo_transcript <- DT::renderDT({
      .fg_datatable(
        .fg_demo_transcript(),
        text_col = "text",
        id_cols = character(),
        page_length = 8L
      )
    })

    output$transcript <- DT::renderDT({
      shiny::req(result())
      tr <- result()$transcript
      cols <- intersect(c("message_id", "round", "speaker_id", "text"), names(tr))
      .fg_datatable(
        tr[, cols, drop = FALSE],
        text_col = "text",
        id_cols = "message_id",
        page_length = 10L
      )
    })

    output$usage <- shiny::renderPrint({
      shiny::req(result())
      usage <- result()$usage
      cat("Transcript utterances:", nrow(result()$transcript), "\n")
      cat("Sent tokens:", .fg_usage_text(usage$sent, usage$unknown), "\n")
      cat("Received tokens:", .fg_usage_text(usage$rec, usage$unknown), "\n")
      cat("Total tokens:", .fg_usage_text(usage$total, usage$unknown), "\n")
      cat(.fg_usage_note(usage$unknown))
    })

    output$timing_table <- DT::renderDT({
      shiny::req(result())
      timing <- .fg_timing_stats(result()$transcript)
      shiny::req(timing$has_durations)
      .fg_datatable(
        timing$speaker_stats,
        id_cols = character(),
        page_length = 8L,
        dom = "tip",
        round_cols = c("total_seconds", "mean_seconds"),
        round_digits = 2L
      )
    })

    output$timing_plot <- shiny::renderPlot({
      shiny::req(result(), requireNamespace("ggplot2", quietly = TRUE))
      timing <- .fg_timing_stats(result()$transcript)
      shiny::req(timing$has_durations)
      print(.fg_timing_plot(timing$speaker_stats))
    })
  })
}

# ---- continuation logic (built on the existing API) -------------------------

# Render a conversation log slice to the history string the run loop itself uses.
.fg_history_string <- function(log) {
  format_conversation_history(log)
}

.fg_perturb_log <- function(log, message_index, new_text) {
  ids <- vapply(log, function(m) as.integer(m$message_id %||% NA_integer_),
                integer(1))
  if (!anyNA(ids)) {
    hit <- match(as.integer(message_index), ids)
    if (is.na(hit)) {
      stop("`perturb_message_id` is not a message_id in the cut history.",
           call. = FALSE)
    }
    message_index <- hit
  }
  if (message_index >= 1L && message_index <= length(log)) {
    log[[message_index]]$text <- new_text
  }
  log
}

# Generate one continuation utterance; clone the agent so counters/history do not
# leak across branches. `conversation_log` (the cut prefix for this branch) is
# forwarded so the continuation uses the SAME message construction (role-flip by
# default) as the main run loop, instead of always falling back to flat.
.fg_generate_once <- function(agent, topic, history_string, template,
                              question = "N/A", summary = "N/A",
                              phase = "exploration_question", max_tokens = 220L,
                              conversation_log = NULL, config = NULL,
                              .runner = NULL) {
  a <- if (inherits(agent, "R6")) agent$clone(deep = TRUE) else agent
  # An FGAgent saved by an earlier version carries that version's methods,
  # which record usage without its completeness. Rebuild it with the current
  # class and copy its persona fields as saved, so the prompt is unchanged; a
  # subclass is used as saved.
  if (identical(class(agent), c("FGAgent", "R6")) &&
      !"tokens_total_agent" %in% names(agent)) {
    a <- FGAgent$new(agent$id,
      list(direct_persona_description = agent$persona_description),
      config = config %||% agent$config, is_moderator = isTRUE(agent$is_moderator),
      .runner = .runner %||% agent$.runner)
    for (field in c("persona_description", "communication_style_instruction",
                    "role", "demographics", "survey_responses", "history")) {
      if (field %in% names(agent)) a[[field]] <- agent[[field]]
    }
  }
  if (!is.null(config)) a$config <- config
  if (!is.null(.runner)) a$.runner <- .runner
  a$generate_utterance(
    topic = topic, conversation_history_string = history_string,
    utterance_prompt_template = template, max_tokens_utterance = max_tokens,
    current_moderator_question = question, conversation_summary_so_far = summary,
    current_phase = phase, conversation_log = conversation_log)
}

.fg_utterance_text <- function(x) {
  if (is.character(x)) return(paste(x, collapse = "\n"))
  if (is.list(x)) for (f in c("text", "utterance", "response_text", "content"))
    if (!is.null(x[[f]]) && is.character(x[[f]])) return(x[[f]])
  paste(utils::capture.output(print(x)), collapse = "\n")
}

.fg_run_experiment_many <- function(
    fg, log, speaker_id, perturb_message_id, arms, runs_per_arm = 1L,
    question = "N/A", summary = NULL, phase = NULL, config = NULL,
    .runner = NULL, .progress = NULL) {
  agent <- fg$agents[[speaker_id]]
  if (is.null(agent)) stop("No agent with id '", speaker_id, "'.", call. = FALSE)
  if (!is.data.frame(arms) || !all(c("label", "text") %in% names(arms)) ||
      !nrow(arms)) {
    stop("`arms` must contain at least one treatment label and text.",
         call. = FALSE)
  }
  arms$label <- trimws(as.character(arms$label))
  arms$text <- trimws(as.character(arms$text))
  if (any(!nzchar(arms$label)) || any(!nzchar(arms$text))) {
    stop("Every treatment arm must have a non-empty label and text.",
         call. = FALSE)
  }
  if (anyDuplicated(arms$label)) {
    stop("Treatment arm labels must be unique.", call. = FALSE)
  }
  if (!is.numeric(runs_per_arm) || length(runs_per_arm) != 1L ||
      is.na(runs_per_arm) || !is.finite(runs_per_arm) ||
      runs_per_arm < 1L || runs_per_arm != floor(runs_per_arm)) {
    stop("`runs_per_arm` must be one positive integer.", call. = FALSE)
  }
  runs_per_arm <- as.integer(runs_per_arm)

  # Replay the saved message construction without mutating the loaded session.
  saved_mode <- fg$message_mode %||%
    getOption("focusgroup.message_mode", "roleflip")
  old_mode <- getOption("focusgroup.message_mode")
  options(focusgroup.message_mode = saved_mode)
  on.exit(options(focusgroup.message_mode = old_mode), add = TRUE)
  template <- .fg_pick_template(fg$prompt_templates, "utterance", mode = saved_mode)
  max_tokens <- max(fg$max_tokens_utterance %||% 160L, 220L)
  # The session's interim summary may cover turns AFTER the cut (it was
  # written at the end of the full run), so injecting it would leak
  # post-cut information into both arms. The cut history alone is the
  # evidence; no summary is supplied unless the caller passes one.
  if (is.null(summary)) summary <- "N/A"
  if (is.null(phase)) {
    phase <- if (length(log)) log[[length(log)]]$phase %||% "exploration_question"
             else "exploration_question"
  }

  histories <- c(
    list(list(
      arm_id = "control",
      arm_label = "Control",
      is_control = TRUE,
      log = log
    )),
    lapply(seq_len(nrow(arms)), function(i) {
      list(
        arm_id = paste0("treatment_", i),
        arm_label = arms$label[[i]],
        is_control = FALSE,
        log = .fg_perturb_log(
          log, as.integer(perturb_message_id), arms$text[[i]]
        )
      )
    })
  )

  rows <- vector("list", length(histories) * runs_per_arm)
  out_index <- 0L
  claim <- if (inherits(fg, "FocusGroup")) fg$.__enclos_env__$private$claim_handles
  if (is.function(claim)) claim()
  for (history in histories) {
    # The agent reads the cut log as it read the session: speakers under
    # their handles.
    history$log <- .fg_agent_log(fg, history$log)
    history_string <- .fg_history_string(history$log)
    for (run_id in seq_len(runs_per_arm)) {
      response <- .fg_generate_once(
        agent = agent,
        topic = fg$topic,
        history_string = history_string,
        template = template,
        question = question,
        summary = summary,
        phase = phase,
        max_tokens = max_tokens,
        conversation_log = history$log,
        config = config,
        .runner = .runner
      )
      meta <- if (is.list(response)) response$meta %||% list() else list()
      out_index <- out_index + 1L
      rows[[out_index]] <- data.frame(
        speaker_id = speaker_id,
        perturb_message_id = as.integer(perturb_message_id),
        arm_id = history$arm_id,
        arm_label = history$arm_label,
        is_control = history$is_control,
        run_id = as.integer(run_id),
        response_text = .fg_utterance_text(response),
        sent_tokens = meta$sent_tokens %||% NA_integer_,
        rec_tokens = meta$rec_tokens %||% NA_integer_,
        total_tokens = meta$total_tokens %||% NA_integer_,
        duration_s = as.numeric(meta$duration_s %||% NA_real_),
        stringsAsFactors = FALSE
      )
      if (is.function(.progress)) {
        .progress(sprintf(
          "%s response, run %d, complete",
          history$arm_label,
          run_id
        ))
      }
    }
  }
  do.call(rbind, rows)
}

# Backward-compatible paired helper used by the direct continuation tests.
.fg_run_experiment <- function(fg, log, speaker_id, perturb_message_id,
                               perturb_text,
                               question = "N/A", summary = NULL, phase = NULL,
                               config = NULL, .runner = NULL,
                               .progress = NULL) {
  progress <- if (is.function(.progress)) {
    function(detail) {
      if (grepl("^Control", detail)) {
        .progress("Control response complete")
      } else {
        .progress("Perturbed response complete")
      }
    }
  } else {
    NULL
  }
  responses <- .fg_run_experiment_many(
    fg = fg,
    log = log,
    speaker_id = speaker_id,
    perturb_message_id = perturb_message_id,
    arms = data.frame(
      label = "Perturbed",
      text = perturb_text,
      stringsAsFactors = FALSE
    ),
    runs_per_arm = 1L,
    question = question,
    summary = summary,
    phase = phase,
    config = config,
    .runner = .runner,
    .progress = progress
  )
  list(
    speaker_id = speaker_id,
    perturb_message_id = as.integer(perturb_message_id),
    control = responses$response_text[responses$is_control][[1]],
    perturbed = responses$response_text[!responses$is_control][[1]]
  )
}

.fg_experiment_summary <- function(responses) {
  words <- .fg_word_count(responses$response_text)
  ids <- unique(responses$arm_id)
  do.call(rbind, lapply(ids, function(id) {
    keep <- responses$arm_id == id
    values <- words[keep]
    data.frame(
      arm = responses$arm_label[which(keep)[[1]]],
      runs = length(values),
      mean_words = mean(values),
      sd_words = if (length(values) > 1L) stats::sd(values) else NA_real_,
      min_words = min(values),
      max_words = max(values),
      stringsAsFactors = FALSE
    )
  }))
}

.fg_experiment_plot <- function(responses) {
  plot_data <- responses
  plot_data$words <- .fg_word_count(plot_data$response_text)
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$arm_label, y = .data$words)
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA, fill = "#D9E6EC") +
    ggplot2::geom_jitter(width = 0.08, height = 0, color = "#2C3E50") +
    ggplot2::labs(
      x = NULL,
      y = "Response length in words",
      title = "Response length by arm"
    ) +
    ggplot2::theme_minimal(base_size = 13)
}

.fg_format_arm_runs <- function(responses, arm_id) {
  arm <- responses[responses$arm_id == arm_id, , drop = FALSE]
  paste(vapply(seq_len(nrow(arm)), function(i) {
    paste0("Run ", arm$run_id[[i]], "\n", arm$response_text[[i]])
  }, character(1)), collapse = "\n\n")
}

# ---- transcript display (analyze mode) ---------------------------------------
# (.fg_as_log, the transcript coercion shared with focus_group_from_transcript(),
# lives in R/utils.R.)

.fg_log_to_df <- function(log) {
  data.frame(
    message_id = vapply(log, function(e) {
      as.integer(e$message_id %||% NA_integer_)
    }, integer(1)),
    round = vapply(log, function(e) {
      as.integer(e$round %||% NA_integer_)
    }, integer(1)),
    speaker_id = vapply(log, function(e) as.character(e$speaker_id %||% ""), character(1)),
    text = vapply(log, function(e) as.character(e$text %||% ""), character(1)),
    stringsAsFactors = FALSE)
}

.fg_demo_transcript <- function() {
  data.frame(
    speaker = c("Moderator","P1","P2","P3","P1","Moderator","P2","P3"),
    text = c("Welcome. Today we discuss public libraries. What comes to mind?",
             "The library is one of the few quiet public places in my neighborhood.",
             "For me it is internet access. I used it when my service was interrupted.",
             "The children's programs matter, but their schedule is hard for working parents.",
             "Right, longer evening hours would make the building more useful.",
             "Access, programs, and hours have come up. What else should we consider?",
             "The collection needs more books in the languages people speak nearby.",
             "Staff help matters too. Finding public records can be difficult."),
    stringsAsFactors = FALSE)
}

# Read an uploaded transcript CSV with every value kept as written. Type
# inference would turn the speaker ids 001 and 01 into the same number 1,
# round long numeric ids, and make the text "NA" missing. The columns other
# than the speaker, the text, and response ids are given their usual types
# once the user has mapped the columns; see .fg_convert_upload_columns().
.fg_read_transcript_csv <- function(file) {
  out <- LLMR.shiny::read_csv_upload(file, col_classes = "character",
                                     na_strings = character(0))
  attr(out, "focusgroup_csv_upload") <- TRUE
  out
}

# Give the columns of an uploaded transcript other than `keep` (the mapped
# speaker and text columns) and `response_id` the types utils::read.csv()
# would have inferred, with "NA" read as missing. Other data frames are
# returned unchanged.
.fg_convert_upload_columns <- function(df, keep) {
  if (!isTRUE(attr(df, "focusgroup_csv_upload"))) return(df)
  attr(df, "focusgroup_csv_upload") <- NULL
  for (j in which(!names(df) %in% c(keep, "response_id"))) {
    df[[j]] <- utils::type.convert(df[[j]], as.is = TRUE, na.strings = "NA")
  }
  df
}

.fg_prepare_analysis_source <- function(obj, speaker_col = NULL,
                                        text_col = NULL) {
  if (inherits(obj, "focus_group_result")) {
    return(list(
      focus_group = obj$focus_group,
      transcript = obj$transcript
    ))
  }
  if (inherits(obj, "FocusGroup")) {
    return(list(
      focus_group = obj,
      transcript = .fg_transcript_data(obj$conversation_log)
    ))
  }
  if (!is.data.frame(obj)) {
    stop(
      "The source must be a transcript data frame, FocusGroup, or focus_group_result.",
      call. = FALSE
    )
  }

  # focus_group_from_transcript() keeps the fields an exported transcript
  # carries, so the table shown is the imported log itself, one row per kept
  # turn, rather than the uploaded rows.
  defaults <- .fg_analysis_column_defaults(obj)
  fg <- focus_group_from_transcript(obj,
    speaker_col = speaker_col %||% defaults$speaker,
    text_col = text_col %||% defaults$text)
  list(focus_group = fg, transcript = .fg_transcript_data(fg$conversation_log))
}

.fg_missing_analysis_packages <- function(include_plots = TRUE) {
  packages <- c("topicmodels", "tidytext", "quanteda", "quanteda.textstats")
  if (isTRUE(include_plots)) packages <- c(packages, "ggplot2")
  packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
}

.fg_analysis_issue_table <- function(analysis) {
  issues <- as.data.frame(analysis$issues, stringsAsFactors = FALSE)
  if (!nrow(issues)) return(issues)
  skipped <- vapply(issues$component, function(component) {
    if (identical(component, "topics")) return(!length(analysis$topics))
    if (identical(component, "tfidf")) return(!nrow(analysis$tfidf))
    if (identical(component, "readability")) {
      return(!nrow(analysis$readability))
    }
    if (identical(component, "plots")) return(!length(analysis$plots))
    if (startsWith(component, "plots.")) {
      return(!sub("^plots[.]", "", component) %in% names(analysis$plots))
    }
    FALSE
  }, logical(1))
  data.frame(
    status = ifelse(skipped, "Skipped", "Limited"),
    component = issues$component,
    reason = issues$reason,
    stringsAsFactors = FALSE
  )
}

.fg_top_tfidf <- function(tfidf, top_n = 8L) {
  empty <- data.frame(
    speaker_id = character(),
    rank = integer(),
    term = character(),
    tf_idf = double(),
    stringsAsFactors = FALSE
  )
  tfidf <- as.data.frame(tfidf, stringsAsFactors = FALSE)
  if (!nrow(tfidf)) return(empty)

  keep <- !is.na(tfidf$speaker_id) & nzchar(trimws(tfidf$speaker_id)) &
    !is.na(tfidf$term) & nzchar(trimws(tfidf$term)) &
    is.finite(tfidf$tf_idf)
  tfidf <- tfidf[keep, c("speaker_id", "term", "tf_idf"), drop = FALSE]
  if (!nrow(tfidf)) return(empty)

  speakers <- unique(tfidf$speaker_id)
  rows <- lapply(speakers, function(speaker) {
    current <- tfidf[tfidf$speaker_id == speaker, , drop = FALSE]
    current <- current[
      order(-current$tf_idf, current$term, na.last = NA),
      ,
      drop = FALSE
    ]
    current <- utils::head(current, as.integer(top_n))
    current$rank <- seq_len(nrow(current))
    current[, c("speaker_id", "rank", "term", "tf_idf"), drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.fg_tfidf_plot <- function(top_terms) {
  plot_data <- top_terms[
    order(top_terms$speaker_id, top_terms$rank),
    ,
    drop = FALSE
  ]
  plot_data$term_key <- paste(
    plot_data$speaker_id,
    plot_data$rank,
    plot_data$term,
    sep = "\r"
  )
  plot_data$term_key <- factor(
    plot_data$term_key,
    levels = rev(unique(plot_data$term_key))
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data$term_key, y = .data$tf_idf)
  ) +
    ggplot2::geom_col(fill = "#2C6E8F", width = 0.75) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$speaker_id),
      scales = "free_y"
    ) +
    ggplot2::scale_x_discrete(
      labels = function(value) sub("^[^\r]+\r[^\r]+\r", "", value)
    ) +
    ggplot2::scale_y_continuous(
      labels = function(value) {
        format(signif(value, 3L), trim = TRUE, scientific = FALSE)
      }
    ) +
    ggplot2::labs(
      x = NULL,
      y = "TF-IDF",
      title = "Top terms by speaker"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 11)
    )
}

# ---- analyze module ---------------------------------------------------------

.fg_analysis_column_defaults <- function(df) {
  columns <- LLMR.shiny::column_names_for_mapping(df)
  if (!length(columns)) {
    return(list(
      columns = character(),
      speaker = NULL,
      text = NULL
    ))
  }

  normalize <- function(x) {
    gsub("[^[:alnum:]]", "", tolower(trimws(x)))
  }
  speaker_variants <- c(
    "speaker_id", "speaker", "agent", "role",
    "id", "doc_id", "unit_id"
  )
  matched <- match(
    normalize(speaker_variants),
    normalize(columns),
    nomatch = 0L
  )
  matched <- matched[matched > 0L]
  speaker <- if (length(matched)) {
    columns[[matched[[1L]]]]
  } else {
    NULL
  }
  text <- LLMR.shiny::guess_column(
    columns,
    kind = "text",
    exclude = speaker %||% character()
  )
  speaker <- if (is.null(speaker)) {
    LLMR.shiny::guess_column(
      columns,
      kind = "id",
      exclude = text %||% character()
    )
  } else {
    speaker
  }

  list(columns = columns, speaker = speaker, text = text)
}

.fg_analyze_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

.fg_analyze_server <- function(
    id, shared, analysis_fun = FocusGroup::analyze_focus_group) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    raw <- shiny::reactiveVal(NULL)
    focus_group <- shiny::reactiveVal(NULL)
    transcript <- shiny::reactiveVal(NULL)
    analysis <- shiny::reactiveVal(NULL)
    analysis_settings <- shiny::reactiveVal(list(
      num_topics = 5L,
      include_plots = TRUE,
      text_col = NULL
    ))
    source_label <- shiny::reactiveVal("No transcript loaded")
    err <- shiny::reactiveVal(NULL)
    accounting_incomplete <- shiny::reactiveVal(FALSE)
    warn_card <- function(message) {
      bslib::card(class = "border-warning", bslib::card_body(message))
    }
    warn_user <- function(message, ui = warn_card(message)) {
      err(ui)
      shiny::showNotification(message, type = "warning")
      invisible(NULL)
    }
    model_ready <- shiny::reactive({
      .fg_live_ok(shared) && nzchar(trimws(shared$model() %||% ""))
    })
    descriptive_action_state <- shiny::reactive({
      value <- raw()
      if (is.null(value)) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Load a transcript or the bundled demo before running",
            "descriptive analysis."
          )
        ))
      }
      if (!is.data.frame(value)) {
        if (inherits(value, "FocusGroup")) {
          if (!length(value$conversation_log)) {
            return(list(
              disabled = TRUE,
              reason = "The saved focus group has no conversation messages."
            ))
          }
          return(list(disabled = FALSE, reason = NULL))
        }
        if (inherits(value, "focus_group_result")) {
          if (!inherits(value$focus_group, "FocusGroup") ||
              !length(value$focus_group$conversation_log) ||
              !is.data.frame(value$transcript)) {
            return(list(
              disabled = TRUE,
              reason = "The saved focus-group result lacks a session object or transcript table."
            ))
          }
          return(list(disabled = FALSE, reason = NULL))
        }
        return(list(
          disabled = TRUE,
          reason = "Load a transcript data frame or a saved focus group."
        ))
      }
      defaults <- .fg_analysis_column_defaults(value)
      speaker <- input$speaker_col %||% defaults$speaker
      text <- input$text_col %||% defaults$text
      if (is.null(speaker) || !speaker %in% names(value)) {
        return(list(
          disabled = TRUE,
          reason = "Select a speaker column before running descriptive analysis."
        ))
      }
      if (is.null(text) || !text %in% names(value)) {
        return(list(
          disabled = TRUE,
          reason = "Select a text column before running descriptive analysis."
        ))
      }
      if (identical(speaker, text)) {
        return(list(
          disabled = TRUE,
          reason = "Select different speaker and text columns."
        ))
      }
      list(disabled = FALSE, reason = NULL)
    })
    model_action_state <- shiny::reactive({
      if (is.null(focus_group())) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Run descriptive analysis before generating themes and a model",
            "summary."
          )
        ))
      }
      if (identical(shared$mode(), "demo")) {
        return(list(
          disabled = TRUE,
          reason = "Switch to Live mode to generate themes and a model summary."
        ))
      }
      if (!isTRUE(shared$can_run())) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Set the selected provider's API key in the environment to",
            "generate themes and a model summary."
          )
        ))
      }
      if (!nzchar(trimws(shared$model() %||% ""))) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Enter a model in the sidebar to generate themes and a model",
            "summary."
          )
        ))
      }
      list(
        disabled = FALSE,
        reason = paste(
          "Model analysis uses the live provider and generation settings in",
          "the sidebar."
        )
      )
    })

    output$module_ui <- shiny::renderUI({
      bslib::card(
        bslib::card_header("Analyze a focus-group transcript"),
        bslib::card_body(
          shiny::uiOutput(ns("err")),
          shiny::fluidRow(
            shiny::column(6, shiny::fileInput(
              ns("file"),
              "Transcript file",
              accept = c(".csv", ".rds"),
              placeholder = "Choose a CSV or .rds transcript"
            )),
            shiny::column(
              6,
              shiny::actionButton(ns("load_demo"), "Load demo transcript")
            )),
          shiny::tags$p(
            class = "text-muted small",
            "Upload a CSV transcript or saved focus group, or load the bundled demo."
          ),
          shiny::uiOutput(ns("source")),
          shiny::uiOutput(ns("map_ui")),
          shiny::tags$div(
            class = "mb-2",
            shiny::tags$strong("Analysis options "),
            LLMR.shiny::help_tip(
              "Descriptive analyses run offline; model themes and summaries make two live calls."
            )
          ),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::numericInput(
                ns("num_topics"),
                shiny::tagList(
                  "Number of topics ",
                  LLMR.shiny::help_tip(
                    "Sets the requested topic count for the optional LDA analysis."
                  )
                ),
                value = 5,
                min = 1,
                step = 1
              )
            ),
            shiny::column(
              4,
              shiny::checkboxInput(
                ns("include_plots"),
                shiny::tagList(
                  "Descriptive plots ",
                  LLMR.shiny::help_tip(
                    "Requests the package's four ggplot2 transcript plots."
                  )
                ),
                value = TRUE
              )
            )
          ),
          shiny::uiOutput(ns("analysis_actions")),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
    })

    output$analysis_actions <- shiny::renderUI({
      descriptive_state <- descriptive_action_state()
      model_state <- model_action_state()
      shiny::tagList(
        shiny::actionButton(
          ns("analyze"),
          "Run descriptive analysis",
          class = "btn-primary",
          disabled = descriptive_state$disabled
        ),
        if (descriptive_state$disabled) {
          shiny::tags$p(
            class = "text-muted small mt-2",
            descriptive_state$reason
          )
        },
        shiny::actionButton(
          ns("model_analyze"),
          shiny::tagList(
            "Generate themes and model summary ",
            LLMR.shiny::help_tip(
              "Runs the package's thematic analysis and model summary as two live calls."
            )
          ),
          class = "btn-secondary",
          disabled = model_state$disabled
        ),
        shiny::tags$p(
          class = "text-muted small mt-2",
          model_state$reason
        )
      )
    })

    output$err <- shiny::renderUI(err())
    output$source <- shiny::renderUI({
      shiny::tags$div(
        class = "border rounded p-2 mb-3 bg-body-tertiary",
        shiny::tags$strong("Current source: "),
        source_label()
      )
    })

    reset_analysis <- function() {
      focus_group(NULL)
      transcript(NULL)
      analysis(NULL)
      analysis_settings(list(
        num_topics = 5L,
        include_plots = TRUE,
        text_col = NULL
      ))
    }

    shiny::observeEvent(input$file, {
      err(NULL)
      reset_analysis()
      obj <- tryCatch(
        if (grepl("\\.rds$", input$file$name, ignore.case = TRUE)) readRDS(input$file$datapath)
        else .fg_read_transcript_csv(input$file),
        error = function(e) {
          warn_user(conditionMessage(e))
          NULL
        })
      raw(obj)
      if (!is.null(obj)) {
        source_label(paste0("Uploaded file: ", input$file$name))
      }
    })
    shiny::observeEvent(input$load_demo, {
      err(NULL)
      reset_analysis()
      raw(.fg_demo_transcript())
      source_label("Bundled demo transcript")
    })

    if (is.function(shared$analysis_handoff)) {
      shiny::observeEvent(shared$analysis_handoff(), {
        handoff <- shared$analysis_handoff()
        shiny::req(handoff$result)
        err(NULL)
        reset_analysis()
        raw(handoff$result)
        source_label(handoff$label %||% "Finished run")
      }, ignoreInit = TRUE)
    }

    output$map_ui <- shiny::renderUI({
      df <- raw()
      if (!is.data.frame(df)) return(NULL)
      cols <- LLMR.shiny::column_names_for_mapping(df)
      if (!length(cols)) return(warn_card("The transcript has no columns."))
      defaults <- .fg_analysis_column_defaults(df)
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("speaker_col"),
            "Speaker column",
            choices = cols,
            selected = defaults$speaker
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("text_col"),
            "Text column",
            choices = cols,
            selected = defaults$text
          )
        )
      )
    })

    shiny::observeEvent(input$analyze, {
      err(NULL)
      obj <- raw()
      if (is.null(obj)) {
        warn_user("Load a transcript or the bundled demo before analyzing.")
        return()
      }
      speaker_col <- input$speaker_col
      text_col <- input$text_col
      if (is.data.frame(obj) && ncol(obj)) {
        defaults <- .fg_analysis_column_defaults(obj)
        speaker_col <- speaker_col %||% defaults$speaker
        text_col <- text_col %||% defaults$text
        obj <- .fg_convert_upload_columns(obj, c(speaker_col, text_col))
      }
      prepared <- tryCatch(
        withCallingHandlers(.fg_prepare_analysis_source(
          obj,
          speaker_col = speaker_col,
          text_col = text_col
        ), warning = function(w) {
          warn_user(conditionMessage(w))
          invokeRestart("muffleWarning")
        }),
        error = function(e) {
          warn_user(conditionMessage(e))
          NULL
        }
      )
      if (is.null(prepared)) return()

      result <- LLMR.shiny::safe_llmr_call(
        analysis_fun(
          prepared$focus_group,
          num_topics = input$num_topics %||% 5L,
          include_plots = isTRUE(input$include_plots),
          config = NULL
        )
      )
      if (!result$ok) {
        err(result$ui)
        return()
      }
      focus_group(prepared$focus_group)
      transcript(prepared$transcript)
      analysis(result$value)
      analysis_settings(list(
        num_topics = input$num_topics %||% 5L,
        include_plots = isTRUE(input$include_plots),
        text_col = "text"
      ))
    })

    shiny::observeEvent(input$model_analyze, {
      err(NULL)
      g <- focus_group()
      if (is.null(g)) {
        warn_user("Run the descriptive analysis before requesting model analysis.")
        return()
      }
      if (!model_ready()) {
        warn_user(
          "Model analysis requires Live mode, a model, and an API key for the selected provider.",
          if (identical(shared$mode(), "demo")) {
            warn_card("Switch to Live mode to run model analysis.")
          } else if (!isTRUE(shared$can_run())) {
            LLMR.shiny::live_run_blocker_ui(shared$key())
          } else {
            warn_card("Enter a model in the sidebar.")
          }
        )
        return()
      }

      provider <- shared$provider()
      config_result <- LLMR.shiny::safe_llmr_call(
        LLMR.shiny::build_llm_config(
          provider,
          shared$model(),
          temperature = shared$temperature(),
          max_tokens = shared$max_tokens(),
          reasoning_effort = shared$reasoning_effort()
        ),
        provider
      )
      if (!config_result$ok) {
        err(config_result$ui)
        return()
      }
      shared$set_plan(
        2L,
        "Thematic analysis and model summary; retries excluded"
      )
      recorder <- .fg_usage_recorder(LLMR.shiny::build_runner("live"), safe = TRUE)
      on.exit(.fg_account_action(recorder, shared, accounting_incomplete), add = TRUE)
      result <- shiny::withProgress(
        message = "Running Model Analysis",
        value = 0,
        {
          shiny::incProgress(0.1, detail = "Preparing two model calls")
          out <- LLMR.shiny::safe_llmr_call(
            analysis_fun(
              g,
              num_topics = input$num_topics %||% 5L,
              include_plots = isTRUE(input$include_plots),
              config = config_result$value,
              .runner = recorder$runner
            ),
            provider
          )
          shiny::incProgress(0.9, detail = "Model analysis complete")
          out
        }
      )
      if (!result$ok) {
        err(result$ui)
        return()
      }
      analysis(result$value)
      current_settings <- analysis_settings()
      analysis_settings(list(
        num_topics = input$num_topics %||% 5L,
        include_plots = isTRUE(input$include_plots),
        text_col = current_settings$text_col
      ))
    })

    shiny::observe({
      if (!identical(shared$active_tab(), "analyze")) return()
      if (model_ready() && !is.null(focus_group())) {
        shared$set_plan(
          2L,
          "Thematic analysis and model summary; retries excluded"
        )
      } else {
        shared$set_plan(0L)
      }
    })

    tfidf_top_terms <- shiny::reactive({
      shiny::req(analysis(), nrow(analysis()$tfidf))
      top_n <- as.integer(input$tfidf_top_n %||% 8L)
      if (length(top_n) != 1L || is.na(top_n) || top_n < 1L) top_n <- 8L
      .fg_top_tfidf(analysis()$tfidf, top_n)
    })

    output$results <- shiny::renderUI({
      shiny::validate(shiny::need(
        !is.null(analysis()),
        "Load a transcript or the bundled demo, then run descriptive analysis."
      ))
      result <- analysis()
      topics <- result$topics
      timing <- .fg_timing_stats(transcript())
      settings <- analysis_settings()
      issue_table <- .fg_analysis_issue_table(result)
      missing_packages <- .fg_missing_analysis_packages(
        settings$include_plots
      )
      shiny::tagList(
        shiny::tags$h5("Transcript"),
        DT::DTOutput(ns("analysis_transcript")),
        shiny::tags$h5("Basic statistics"),
        DT::DTOutput(ns("basic_stats")),
        shiny::tags$h6("Full transcript"),
        LLMR.shiny::text_block_output(
          ns("full_transcript"),
          height = "20rem"
        ),
        shiny::tags$h5("Topics"),
        if (length(topics)) {
          shiny::tagList(
            shiny::tags$h6("Topic labels"),
            DT::DTOutput(ns("topic_labels")),
            shiny::tags$h6("Top terms"),
            DT::DTOutput(ns("topic_terms")),
            shiny::tags$h6("Document-topic proportions"),
            DT::DTOutput(ns("document_topics"))
          )
        } else {
          shiny::tags$p(
            class = "text-muted",
            "Topic modeling was skipped or returned no result; see Analysis issues."
          )
        },
        shiny::tags$h5("TF-IDF"),
        if (nrow(result$tfidf)) {
          available_terms <- max(
            as.integer(table(result$tfidf$speaker_id)),
            1L
          )
          shiny::tagList(
            shiny::numericInput(
              ns("tfidf_top_n"),
              "Terms shown per speaker",
              value = min(8L, available_terms),
              min = 1,
              max = available_terms,
              step = 1,
              width = "14rem"
            ),
            shiny::tags$h6("Top terms per speaker"),
            DT::DTOutput(ns("tfidf")),
            if (requireNamespace("ggplot2", quietly = TRUE)) {
              shiny::plotOutput(ns("tfidf_plot"), height = "440px")
            } else {
              LLMR.shiny::install_guidance_ui(
                "ggplot2",
                "TF-IDF plot"
              )
            }
          )
        } else {
          shiny::tags$p(
            class = "text-muted",
            "TF-IDF was skipped or returned no terms; see Analysis issues."
          )
        },
        shiny::tags$h5("Readability"),
        if (nrow(result$readability)) {
          DT::DTOutput(ns("readability"))
        } else {
          shiny::tags$p(
            class = "text-muted",
            "Readability was skipped or returned no scores; see Analysis issues."
          )
        },
        shiny::tags$h5("Model-dependent analysis"),
        shiny::tags$h6("Themes"),
        LLMR.shiny::text_block_output(ns("themes"), height = "16rem"),
        shiny::tags$h6("Model summary"),
        LLMR.shiny::text_block_output(ns("model_summary"), height = "16rem"),
        shiny::tags$h5("Package plots"),
        if (length(result$plots)) {
          bslib::layout_column_wrap(
            width = 1 / 2,
            lapply(names(result$plots), function(name) {
              bslib::card(
                bslib::card_header(gsub("_", " ", name)),
                bslib::card_body(
                  shiny::plotOutput(
                    ns(paste0("analysis_plot_", name)),
                    height = "300px"
                  )
                )
              )
            })
          )
        } else {
          shiny::tags$p(
            class = "text-muted",
            if (settings$include_plots) {
              "Package plots were skipped or returned no result; see Analysis issues."
            } else {
              "Package plots were not requested."
            }
          )
        },
        shiny::tags$h5("Timing"),
        if (timing$has_wall_time) {
          shiny::tags$p(sprintf(
            "Total session wall time: %.1f seconds.",
            timing$wall_seconds
          ))
        } else {
          shiny::tags$p(
            class = "text-muted",
            "Total session wall time is unavailable because the source lacks two valid timestamps."
          )
        },
        if (timing$has_durations) {
          shiny::tagList(
            DT::DTOutput(ns("analysis_timing_table")),
            if (requireNamespace("ggplot2", quietly = TRUE)) {
              shiny::plotOutput(ns("analysis_timing_plot"), height = "320px")
            } else {
              LLMR.shiny::install_guidance_ui("ggplot2", "Timing plot")
            }
          )
        } else {
          shiny::tags$p(
            class = "text-muted",
            "Per-speaker timing is unavailable because this source has no duration_s values."
          )
        },
        if (length(missing_packages)) {
          shiny::tagList(
            shiny::tags$h5("Optional packages"),
            lapply(missing_packages, function(package) {
              LLMR.shiny::install_guidance_ui(package)
            })
          )
        },
        shiny::tags$h5("Analysis issues"),
        if (nrow(issue_table)) {
          shiny::tagList(
            shiny::tags$p(
              "Each skipped or limited optional component and its reason appear below."
            ),
            DT::DTOutput(ns("issues"))
          )
        } else {
          shiny::tags$p(
            class = "text-muted",
            "No optional analysis issues were reported."
          )
        }
      )
    })

    output$analysis_transcript <- DT::renderDT({
      shiny::req(transcript())
      shown <- .fg_transcript_csv(transcript())
      .fg_datatable(
        shown,
        text_col = analysis_settings()$text_col,
        page_length = 8L,
        round_cols = intersect(
          c("duration_s", "elapsed_seconds"),
          names(shown)
        ),
        round_digits = 2L
      )
    })

    output$basic_stats <- DT::renderDT({
      shiny::req(analysis())
      .fg_datatable(
        as.data.frame(analysis()$basic_stats$speaker_stats),
        id_cols = character(),
        page_length = 8L
      )
    })

    output$full_transcript <- shiny::renderText({
      shiny::req(analysis())
      analysis()$basic_stats$full_transcript %||% ""
    })

    output$topic_labels <- DT::renderDT({
      shiny::req(analysis(), length(analysis()$topics))
      .fg_datatable(
        as.data.frame(analysis()$topics$topic_labels),
        id_cols = character(),
        page_length = 8L
      )
    })

    output$topic_terms <- DT::renderDT({
      shiny::req(analysis(), length(analysis()$topics))
      .fg_datatable(
        as.data.frame(analysis()$topics$top_terms),
        id_cols = character(),
        page_length = 10L
      )
    })

    output$document_topics <- DT::renderDT({
      shiny::req(analysis(), length(analysis()$topics))
      shown <- as.data.frame(analysis()$topics$document_topics)
      .fg_datatable(
        shown,
        id_cols = intersect(
          c("document", "document_id", "doc_id", "message_id"),
          names(shown)
        ),
        digits = 3L,
        page_length = 10L
      )
    })

    output$tfidf <- DT::renderDT({
      shown <- tfidf_top_terms()
      shiny::req(nrow(shown))
      names(shown) <- c("Speaker", "Rank", "Term", "TF-IDF")
      .fg_datatable(
        shown,
        id_cols = character(),
        digits = 3L,
        page_length = min(12L, nrow(shown))
      )
    })

    output$tfidf_plot <- shiny::renderPlot({
      shiny::req(requireNamespace("ggplot2", quietly = TRUE))
      shown <- tfidf_top_terms()
      shiny::req(nrow(shown))
      print(.fg_tfidf_plot(shown))
    })

    output$readability <- DT::renderDT({
      shiny::req(analysis(), nrow(analysis()$readability))
      .fg_datatable(
        as.data.frame(analysis()$readability),
        id_cols = character(),
        digits = 3L,
        page_length = 8L
      )
    })

    output$themes <- shiny::renderText({
      shiny::req(analysis())
      if (length(analysis()$themes)) {
        paste(analysis()$themes, collapse = "\n")
      } else {
        paste(
          "Not run. Model themes are generated only when the user selects",
          "the live model-analysis action."
        )
      }
    })

    output$model_summary <- shiny::renderText({
      shiny::req(analysis())
      if (length(analysis()$model_summary)) {
        paste(analysis()$model_summary, collapse = "\n")
      } else {
        paste(
          "Not run. A model summary is generated only when the user selects",
          "the live model-analysis action."
        )
      }
    })

    output$analysis_timing_table <- DT::renderDT({
      shiny::req(transcript())
      timing <- .fg_timing_stats(transcript())
      shiny::req(timing$has_durations)
      .fg_datatable(
        timing$speaker_stats,
        id_cols = character(),
        page_length = 8L,
        dom = "tip",
        round_cols = c("total_seconds", "mean_seconds"),
        round_digits = 2L
      )
    })

    output$analysis_timing_plot <- shiny::renderPlot({
      shiny::req(transcript(), requireNamespace("ggplot2", quietly = TRUE))
      timing <- .fg_timing_stats(transcript())
      shiny::req(timing$has_durations)
      print(.fg_timing_plot(timing$speaker_stats))
    })

    output$issues <- DT::renderDT({
      shiny::req(analysis())
      issue_table <- .fg_analysis_issue_table(analysis())
      shiny::req(nrow(issue_table))
      .fg_datatable(
        issue_table,
        text_col = "reason",
        id_cols = character(),
        page_length = 8L
      )
    })

    plot_names <- c(
      "participation_timeline",
      "word_count_distribution",
      "participation_by_agent",
      "message_length_timeline"
    )
    for (plot_name in plot_names) {
      local({
        name <- plot_name
        output[[paste0("analysis_plot_", name)]] <- shiny::renderPlot({
          shiny::req(analysis())
          plot <- analysis()$plots[[name]]
          shiny::req(!is.null(plot))
          print(plot)
        })
      })
    }
  })
}

# ---- experiment module ------------------------------------------------------

.fg_experiment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

.fg_experiment_call_count <- function(n_treatments, runs_per_arm) {
  as.integer((1L + as.integer(n_treatments)) * as.integer(runs_per_arm))
}

.fg_experiment_server <- function(
    id, shared, experiment_fun = .fg_run_experiment_many) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    fg <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal(NULL)
    pending_run <- shiny::reactiveVal(NULL)
    err <- shiny::reactiveVal(NULL)
    accounting_incomplete <- shiny::reactiveVal(FALSE)
    warn_card <- function(message) {
      bslib::card(class = "border-warning", bslib::card_body(message))
    }
    warn_user <- function(message, ui = warn_card(message)) {
      err(ui)
      shiny::showNotification(message, type = "warning")
      invisible(NULL)
    }

    continuation_action_state <- shiny::reactive({
      g <- fg()
      if (is.null(g)) {
        return(list(
          disabled = TRUE,
          reason = "Load a saved focus group before generating continuations."
        ))
      }
      messages <- .fg_display_messages(g$conversation_log)
      if (!nrow(messages)) {
        return(list(
          disabled = TRUE,
          reason = "The loaded focus group has no conversation messages."
        ))
      }
      participants <- setdiff(
        names(g$agents),
        g$moderator_id %||% character()
      )
      if (!length(participants)) participants <- names(g$agents)
      if (!length(participants)) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "The loaded focus group has no agent who can generate a",
            "continuation."
          )
        ))
      }
      if (identical(shared$mode(), "demo")) {
        return(list(
          disabled = TRUE,
          reason = "Switch to Live mode to generate continuation responses."
        ))
      }
      if (!isTRUE(shared$can_run())) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Set the selected provider's API key in the environment to",
            "generate continuation responses."
          )
        ))
      }
      if (!nzchar(trimws(shared$model() %||% ""))) {
        return(list(
          disabled = TRUE,
          reason = paste(
            "Enter a model in the sidebar to generate continuation",
            "responses."
          )
        ))
      }

      n <- nrow(messages)
      default_cut <- min(n, max(2L, n - 1L))
      cut <- as.integer(input$cut %||% default_cut)
      perturb <- as.integer(input$perturb_message_id %||% 1L)
      if (anyNA(c(perturb, cut)) || cut < 1L || cut > n ||
          perturb < 1L || perturb > cut) {
        return(list(
          disabled = TRUE,
          reason = "Choose a perturbed message at or before the conversation cut."
        ))
      }

      enabled <- vapply(seq_len(3L), function(i) {
        value <- input[[paste0("arm_", i, "_enabled")]]
        if (is.null(value)) identical(i, 1L) else isTRUE(value)
      }, logical(1))
      if (!any(enabled)) {
        return(list(
          disabled = TRUE,
          reason = "Include at least one treatment arm."
        ))
      }
      arm_index <- which(enabled)
      labels <- vapply(arm_index, function(i) {
        trimws(
          input[[paste0("arm_", i, "_label")]] %||%
            paste("Treatment", i)
        )
      }, character(1))
      if (any(!nzchar(labels))) {
        return(list(
          disabled = TRUE,
          reason = "Enter a label for every included treatment arm."
        ))
      }
      if (anyDuplicated(labels)) {
        return(list(
          disabled = TRUE,
          reason = "Use a unique label for every included treatment arm."
        ))
      }
      texts <- vapply(arm_index, function(i) {
        trimws(input[[paste0("arm_", i, "_text")]] %||% "")
      }, character(1))
      if (any(!nzchar(texts))) {
        return(list(
          disabled = TRUE,
          reason = "Enter replacement text for every included treatment arm."
        ))
      }

      runs <- input$runs_per_arm %||% 1L
      if (!is.numeric(runs) || length(runs) != 1L || is.na(runs) ||
          !is.finite(runs) || runs < 1L || runs != floor(runs)) {
        return(list(
          disabled = TRUE,
          reason = "Set runs per arm to one positive integer."
        ))
      }
      speaker <- input$speaker %||% participants[[1L]]
      if (!nzchar(speaker) || !speaker %in% participants) {
        return(list(
          disabled = TRUE,
          reason = "Select an available participant to speak next."
        ))
      }
      list(disabled = FALSE, reason = NULL)
    })

    output$module_ui <- shiny::renderUI({
      demo_mode <- identical(shared$mode(), "demo")
      bslib::card(
        bslib::card_header("Continuation comparison"),
        bslib::card_body(
          shiny::uiOutput(ns("err")),
          shiny::tags$p(class = "text-muted",
            if (demo_mode) {
              paste(
                "Load a saved focus group to inspect the comparison controls.",
                "Generating continuation responses requires Live mode."
              )
            } else {
              paste(
                "Load a saved focus group containing the agents and conversation.",
                "Each included treatment is compared with the same control",
                "history for the requested number of runs."
              )
            }),
          shiny::fileInput(
            ns("file"),
            "Saved focus group",
            accept = ".rds",
            placeholder = "Choose a saved FocusGroup .rds file"
          ),
          shiny::uiOutput(ns("controls")),
          shiny::uiOutput(ns("continuation_action")),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
    })

    output$continuation_action <- shiny::renderUI({
      action_state <- continuation_action_state()
      shiny::tagList(
        shiny::actionButton(
          ns("run"),
          "Generate continuation responses",
          class = "btn-primary",
          disabled = action_state$disabled
        ),
        if (action_state$disabled) {
          shiny::tags$p(
            class = "text-muted small mt-2",
            action_state$reason
          )
        }
      )
    })

    output$err <- shiny::renderUI(err())

    shiny::observeEvent(input$file, {
      err(NULL); result(NULL)
      obj <- tryCatch(
        readRDS(input$file$datapath),
        error = function(e) {
          warn_user(conditionMessage(e))
          NULL
        }
      )
      if (!is.null(obj) && !inherits(obj, "FocusGroup")) {
        warn_user("The selected file must contain a saved focus group.")
        obj <- NULL
      }
      fg(obj)
    })

    output$controls <- shiny::renderUI({
      g <- fg()
      if (is.null(g)) {
        return(shiny::tags$p(
          class = "text-muted",
          "Load a saved focus group to configure the comparison."
        ))
      }
      # Index off speaker messages only; the System roster row is dropped.
      dt <- .fg_display_messages(g$conversation_log)
      n <- nrow(dt)
      if (!n) {
        return(warn_card(
          "The loaded focus group has no conversation messages."
        ))
      }
      k0 <- min(n, max(2, n - 1))
      # The continuation speaks message k+1, so only participants (not the
      # moderator) are valid next speakers; the participant utterance template
      # is what generate_utterance uses.
      participants <- setdiff(names(g$agents), g$moderator_id %||% character(0))
      if (!length(participants)) participants <- names(g$agents)
      if (!length(participants)) {
        return(warn_card(
          "The loaded focus group has no agent who can generate a continuation."
        ))
      }
      shiny::tagList(
        shiny::sliderInput(
          ns("cut"),
          shiny::tagList(
            "Cut the conversation after message k ",
            LLMR.shiny::help_tip(
              "The next response is generated from messages 1 through k."
            )
          ),
          min = 1,
          max = n,
          value = k0,
          step = 1
        ),
        # The perturbed message must lie within the cut prefix (j <= k), otherwise
        # the treatment would fall outside the history fed to the next message and
        # silently do nothing. The max tracks the cut in the run handler.
        shiny::numericInput(
          ns("perturb_message_id"),
          shiny::tagList(
            "Perturb message j (must be <= k) ",
            LLMR.shiny::help_tip(
              "Message j is replaced separately in each treatment history and must not exceed k."
            )
          ),
          value = 1,
          min = 1,
          max = k0,
          step = 1
        ),
        shiny::tags$h5("Treatment arms"),
        bslib::layout_column_wrap(
          width = 1 / 3,
          lapply(seq_len(3L), function(i) {
            bslib::card(
              bslib::card_header(paste("Treatment arm", i)),
              bslib::card_body(
                shiny::checkboxInput(
                  ns(paste0("arm_", i, "_enabled")),
                  "Include this arm",
                  value = identical(i, 1L)
                ),
                shiny::textInput(
                  ns(paste0("arm_", i, "_label")),
                  "Arm label",
                  value = paste("Treatment", i),
                  placeholder = paste("Treatment", i)
                ),
                shiny::textAreaInput(
                  ns(paste0("arm_", i, "_text")),
                  "Replacement text for message j",
                  value = "",
                  rows = 4,
                  placeholder = "Type the replacement message for this arm"
                )
              )
            )
          })
        ),
        shiny::numericInput(
          ns("runs_per_arm"),
          shiny::tagList(
            "Runs per arm ",
            LLMR.shiny::help_tip(
              "Each run generates one new response for the control and every included treatment."
            )
          ),
          value = 1,
          min = 1,
          step = 1
        ),
        shiny::selectInput(ns("speaker"), "Who speaks message k+1?", choices = participants),
        shiny::textInput(ns("question"), "Moderator question in context (optional)", value = "N/A"),
        shiny::uiOutput(ns("cost_note")),
        if (identical(shared$mode(), "live") && !shared$can_run())
          LLMR.shiny::live_run_blocker_ui(shared$key()),
        if (identical(shared$mode(), "demo"))
          shiny::tags$p(
            class = "text-muted small",
            "Switch to Live mode to generate continuation responses."
          )
      )
    })

    current_plan <- shiny::reactive({
      enabled <- vapply(seq_len(3L), function(i) {
        value <- input[[paste0("arm_", i, "_enabled")]]
        if (is.null(value)) identical(i, 1L) else isTRUE(value)
      }, logical(1))
      runs <- as.integer(input$runs_per_arm %||% 1L)
      if (is.na(runs) || runs < 1L) runs <- 1L
      list(
        treatments = sum(enabled),
        runs = runs,
        calls = .fg_experiment_call_count(sum(enabled), runs)
      )
    })

    output$cost_note <- shiny::renderUI({
      plan <- current_plan()
      shiny::tags$p(
        class = "text-muted small",
        sprintf(
          paste(
            "This comparison has 1 control and %d treatment arm(s),",
            "%d run(s) per arm, and %d planned API calls.",
            "Retries and incomplete-output continuations are excluded and",
            "can add calls. Live runs above %d planned calls require",
            "confirmation."
          ),
          plan$treatments,
          plan$runs,
          plan$calls,
          .fg_large_run_threshold
        )
      )
    })

    shiny::observe({
      if (!identical(shared$active_tab(), "experiment")) return()
      if (!continuation_action_state()$disabled) {
        plan <- current_plan()
        shared$set_plan(
          plan$calls,
          sprintf(
            "Continuation comparison, %d arm(s) by %d run(s); retries excluded",
            1L + plan$treatments,
            plan$runs
          )
        )
      } else {
        shared$set_plan(0L)
      }
    })

    # Keep the perturbed-message ceiling in lockstep with the cut, so the UI cannot
    # offer a j beyond k.
    shiny::observeEvent(input$cut, {
      shiny::updateNumericInput(session, "perturb_message_id",
        max = as.integer(input$cut),
        value = min(as.integer(input$perturb_message_id %||% 1L),
                    as.integer(input$cut)))
    })

    shiny::observeEvent(input$perturb_message_id, {
      g <- fg(); if (is.null(g)) return()
      dt <- .fg_display_messages(g$conversation_log)
      j <- as.integer(input$perturb_message_id)
      if (j >= 1 && j <= nrow(dt) &&
          !nzchar(input$arm_1_text %||% "")) {
        shiny::updateTextAreaInput(
          session,
          "arm_1_text",
          value = dt$text[j] %||% ""
        )
      }
    })

    prepare_run <- function() {
      err(NULL)
      pending_run(NULL)
      g <- fg()
      if (is.null(g)) {
        warn_user("Load a saved focus group before generating continuations.")
        return(NULL)
      }
      if (identical(shared$mode(), "demo")) {
        warn_user("Switch to Live mode to generate continuation responses.")
        return(NULL)
      }
      if (!isTRUE(shared$can_run())) {
        warn_user(
          "Live mode requires an API key for the selected provider.",
          LLMR.shiny::live_run_blocker_ui(shared$key())
        )
        return(NULL)
      }
      model <- trimws(shared$model() %||% "")
      if (!nzchar(model)) {
        warn_user("Enter a model in the sidebar before generating continuations.")
        return(NULL)
      }
      dt <- .fg_display_messages(g$conversation_log)
      k <- as.integer(input$cut)
      j <- as.integer(input$perturb_message_id)
      if (anyNA(c(j, k)) || k < 1L || k > nrow(dt) || j < 1L || j > k) {
        warn_user(sprintf(
          paste(
            "Perturbed message j (%s) must be between 1 and the cut k (%s);",
            "otherwise the treatment falls outside the history and does",
            "nothing."
          ),
          j, k))
        return(NULL)
      }
      enabled <- vapply(seq_len(3L), function(i) {
        isTRUE(input[[paste0("arm_", i, "_enabled")]])
      }, logical(1))
      if (!any(enabled)) {
        warn_user("Include at least one treatment arm.")
        return(NULL)
      }
      arm_index <- which(enabled)
      arms <- data.frame(
        label = vapply(arm_index, function(i) {
          trimws(input[[paste0("arm_", i, "_label")]] %||% "")
        }, character(1)),
        text = vapply(arm_index, function(i) {
          trimws(input[[paste0("arm_", i, "_text")]] %||% "")
        }, character(1)),
        stringsAsFactors = FALSE
      )
      if (any(!nzchar(arms$label))) {
        warn_user("Every included treatment arm needs a label.")
        return(NULL)
      }
      if (anyDuplicated(arms$label)) {
        warn_user("Treatment arm labels must be unique.")
        return(NULL)
      }
      if (any(!nzchar(arms$text))) {
        warn_user("Every included treatment arm needs replacement text.")
        return(NULL)
      }
      runs <- input$runs_per_arm
      if (!is.numeric(runs) || length(runs) != 1L || is.na(runs) ||
          !is.finite(runs) || runs < 1L || runs != floor(runs)) {
        warn_user("Runs per arm must be one positive integer.")
        return(NULL)
      }
      runs <- as.integer(runs)
      speaker <- input$speaker %||% ""
      if (!nzchar(speaker)) {
        warn_user("Select the participant who speaks next.")
        return(NULL)
      }
      log_idx <- dt$log_index[seq_len(k)]
      log_k <- g$conversation_log[log_idx]
      # The controls number display rows, which skip the System row; the
      # experiment edits by message_id, so translate the previewed row.
      perturb_id <- .fg_display_message_id(log_k, j)
      provider <- shared$provider()
      config_result <- LLMR.shiny::safe_llmr_call(
        LLMR.shiny::build_llm_config(
          provider,
          model,
          temperature = shared$temperature(),
          max_tokens = shared$max_tokens(),
          reasoning_effort = shared$reasoning_effort()
        ),
        provider
      )
      if (!config_result$ok) {
        err(config_result$ui)
        return(NULL)
      }
      calls <- .fg_experiment_call_count(nrow(arms), runs)
      shared$set_plan(
        calls,
        sprintf(
          "Continuation comparison, %d arm(s) by %d run(s); retries excluded",
          1L + nrow(arms),
          runs
        )
      )
      list(
        focus_group = g,
        log = log_k,
        speaker = speaker,
        perturb_message_id = perturb_id,
        arms = arms,
        runs = runs,
        question = input$question %||% "N/A",
        provider = provider,
        config = config_result$value,
        calls = calls
      )
    }

    execute_run <- function(request) {
      recorder <- .fg_usage_recorder(LLMR.shiny::build_runner("live"), safe = TRUE)
      on.exit(.fg_account_action(recorder, shared, accounting_incomplete), add = TRUE)
      completed <- 0L
      res <- shiny::withProgress(
        message = "Running Continuation Experiment", value = 0,
        {
          progress <- function(detail) {
            completed <<- completed + 1L
            shiny::incProgress(
              1 / request$calls,
              detail = sprintf(
                "%s (%d of %d planned API calls)",
                detail,
                completed,
                request$calls
              )
            )
          }
          LLMR.shiny::safe_llmr_call(
            experiment_fun(
              fg = request$focus_group,
              log = request$log,
              speaker_id = request$speaker,
              perturb_message_id = request$perturb_message_id,
              arms = request$arms,
              runs_per_arm = request$runs,
              question = request$question,
              config = request$config,
              .runner = recorder$runner,
              .progress = progress
            ),
            request$provider
          )
        }
      )
      if (!res$ok) { err(res$ui); return() }
      result(res$value)
    }

    shiny::observeEvent(input$run, {
      request <- prepare_run()
      if (is.null(request)) return()
      if (request$calls > .fg_large_run_threshold) {
        pending_run(request)
        shiny::showModal(shiny::modalDialog(
          title = "Confirm Large Live Run",
          shiny::tags$p(sprintf(
            paste(
              "This comparison has 1 control and %d treatment arm(s),",
              "%d run(s) per arm, and %d planned API calls."
            ),
            nrow(request$arms),
            request$runs,
            request$calls
          )),
          shiny::tags$p(
            "Retries and incomplete-output continuations are excluded and can add calls."
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(
              ns("confirm_run"),
              sprintf("Run %d Calls", request$calls),
              class = "btn-primary"
            )
          ),
          easyClose = FALSE
        ))
        return()
      }
      execute_run(request)
    })

    shiny::observeEvent(input$confirm_run, {
      request <- pending_run()
      shiny::req(request)
      pending_run(NULL)
      shiny::removeModal()
      execute_run(request)
    })

    output$results <- shiny::renderUI({
      r <- result()
      shiny::validate(shiny::need(
        !is.null(r),
        "Load a saved focus group and generate the continuation comparison."
      ))
      arm_ids <- unique(r$arm_id)
      runs <- max(r$run_id)
      shiny::tagList(
        shiny::tags$h5("Responses by arm"),
        bslib::layout_column_wrap(
          width = 1 / 2,
          lapply(arm_ids, function(arm_id) {
            label <- r$arm_label[match(arm_id, r$arm_id)]
            bslib::card(
              bslib::card_header(
                paste0(label, ": ", r$speaker_id[[1]], " speaks next")
              ),
              bslib::card_body(
                LLMR.shiny::text_block_output(
                  ns(paste0("arm_result_", arm_id)),
                  height = "18rem"
                )
              )
            )
          })
        ),
        shiny::tags$h5("Per-arm summary"),
        DT::DTOutput(ns("experiment_summary")),
        if (runs > 1L) {
          shiny::tagList(
            shiny::tags$h5("Response length comparison"),
            if (requireNamespace("ggplot2", quietly = TRUE)) {
              shiny::plotOutput(
                ns("experiment_length_plot"),
                height = "360px"
              )
            } else {
              LLMR.shiny::install_guidance_ui(
                "ggplot2",
                "Response length comparison"
              )
            }
          )
        }
      )
    })

    for (arm_id in c("control", paste0("treatment_", seq_len(3L)))) {
      local({
        id <- arm_id
        output[[paste0("arm_result_", id)]] <- shiny::renderText({
          shiny::req(result(), id %in% result()$arm_id)
          .fg_format_arm_runs(result(), id)
        })
      })
    }

    output$experiment_summary <- DT::renderDT({
      shiny::req(result())
      summary <- .fg_experiment_summary(result())
      .fg_datatable(
        summary,
        id_cols = character(),
        page_length = nrow(summary),
        dom = "t",
        round_cols = c("mean_words", "sd_words"),
        round_digits = 1L
      )
    })

    output$experiment_length_plot <- shiny::renderPlot({
      shiny::req(
        result(),
        max(result()$run_id) > 1L,
        requireNamespace("ggplot2", quietly = TRUE)
      )
      print(.fg_experiment_plot(result()))
    })
  })
}
