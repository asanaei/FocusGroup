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
  invisible(TRUE)
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
    bslib::nav_panel("Continuation experiment", value = "experiment", .fg_experiment_ui("experiment"))
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

.fg_transcript_csv <- function(transcript) {
  transcript <- as.data.frame(transcript, stringsAsFactors = FALSE)
  keep <- vapply(transcript, function(column) {
    is.atomic(column) && !is.list(column)
  }, logical(1))
  transcript[, keep, drop = FALSE]
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

# The example personas frame, from LLMR (the shared home).
.fg_personas <- function() LLMR::anes_2024_personas

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

# The compact moderator guide used by the GUI.
.fg_gui_guide <- function(topic) {
  list(
    Opening = "Welcome the participants and state the ground rules.",
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

# Build agents from chosen persona rows and run a focus group. `rows` are row
# indices into `data` (empty selects a seeded sample of `n_participants`).
.fg_run_from_personas <- function(topic, n_participants, rows, flow, message_mode,
                                   seed, max_participant_responses,
                                   config, data = NULL, persona_cols = NULL,
                                   .runner = NULL) {
  data <- data %||% .fg_personas()
  data <- .fg_persona_columns(data, persona_cols)
  chosen <- if (length(rows)) data[rows, , drop = FALSE] else data
  n <- if (length(rows)) length(rows) else n_participants

  # focusgroup.seed must be in place BEFORE the agents are built: the diverse
  # draw inside create_agents_from_data() reads it. Setting it only around
  # run_simulation() would leave the seed input inert on this path.
  fg <- withr::with_options(
    list(focusgroup.message_mode = message_mode, focusgroup.seed = seed),
    {
      agents <- create_agents_from_data(chosen, n_participants = n,
                                        config = config, .runner = .runner)
      flow_obj <- create_conversation_flow(flow, agents, "MOD")
      script <- .fg_build_question_script(.fg_gui_guide(topic), topic)
      fg <- FocusGroup$new(topic = topic,
                           purpose = paste("Explore perspectives on", topic),
                           agents = agents, moderator_id = "MOD",
                           turn_taking_flow = flow_obj, question_script = script,
                           max_participant_responses = max_participant_responses)
      fg$run_simulation(verbose = FALSE)
      fg
    })
  replies <- 5L * as.integer(max_participant_responses)
  desire_outputs <- if (identical(flow, "desire_based")) replies * n else 0L
  .fg_focus_group_result(
    focus_group = fg,
    flow = flow,
    message_mode = message_mode,
    n_participants = n,
    estimated_calls = 7L + replies + desire_outputs + 1L
  )
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
    pending_run <- shiny::reactiveVal(NULL)
    warn_card <- function(m) bslib::card(class = "border-warning", bslib::card_body(m))
    warn_user <- function(message, ui = warn_card(message)) {
      err(ui)
      shiny::showNotification(message, type = "warning")
      invisible(NULL)
    }

    estimate_run <- function() {
      p <- as.integer(input$n_participants %||% 3L)
      mr <- as.integer(input$max_resp %||% 1L)
      replies <- 5L * mr
      desire <- if (identical(input$flow %||% "round_robin", "desire_based"))
        replies * p else 0L
      list(
        calls = as.integer(7L + replies + desire + 1L),
        utterances = as.integer(7L + replies),
        desire_calls = as.integer(desire)
      )
    }

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
          shiny::fluidRow(
            shiny::column(8, shiny::textInput(ns("topic"), "Topic",
              value = "public libraries", width = "100%")),
            shiny::column(4, shiny::numericInput(ns("n_participants"), "Participants",
              value = 3, min = 2, max = 6, step = 1))),
          shiny::radioButtons(
            ns("source"),
            shiny::tagList(
              "Participants drawn from ",
              LLMR.shiny::help_tip(
                "Synthetic creates generated profiles; ANES uses selected or sampled survey respondents."
              )
            ),
            choices = c("Synthetic personas" = "synthetic",
                        "ANES 2024 personas" = "anes"),
            selected = "synthetic", inline = TRUE),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'anes'", ns("source")),
            shiny::tags$p(class = "text-muted small",
              "Pick rows to use as participants (click to toggle; the list runs from most liberal at the top to most conservative at the bottom). Select none to draw a diverse sample automatically."),
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
            shiny::uiOutput(ns("persona_detail_ui"))),
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(
              ns("flow"),
              shiny::tagList(
                "Turn-taking flow ",
                LLMR.shiny::help_tip(
                  "Determines how the next participant is selected."
                )
              ),
              choices = c("Round robin" = "round_robin",
                          "Desire based" = "desire_based",
                          "Probabilistic" = "probabilistic"),
              selected = "round_robin")),
            shiny::column(4, shiny::selectInput(
              ns("message_mode"),
              shiny::tagList(
                "Message construction ",
                LLMR.shiny::help_tip(
                  "Role flip preserves each speaker's voice; flat sends one labeled transcript prompt."
                )
              ),
              choices = c("Role flip" = "roleflip", "Flat" = "flat"),
              selected = "roleflip")),
            shiny::column(4, shiny::numericInput(ns("max_resp"),
              shiny::tagList(
                "Max responses per question ",
                LLMR.shiny::help_tip(
                  "Sets how many participant replies follow each moderator question."
                )
              ),
              value = 1, min = 1, max = 3, step = 1))),
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput(ns("seed"),
              shiny::tagList(
                "Seed ",
                LLMR.shiny::help_tip(
                  "Controls participant sampling and speaker-order randomness, not model output."
                )
              ),
              value = 110, step = 1))),
          shiny::uiOutput(ns("cost_note")),
          if (demo_mode)
            shiny::tags$p(class = "text-muted small",
              "Switch to Live mode to run this configuration.")
          else if (!shared$can_run())
            LLMR.shiny::live_run_blocker_ui(shared$key()),
          shiny::actionButton(
            ns("run"), "Run focus group", class = "btn-primary",
            disabled = demo_mode
          ),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
    })

    output$err <- shiny::renderUI(err())

    # The selectable persona table is the shared LLMR.shiny module: rows run
    # liberal -> conservative, multi-select, returning the chosen row indices.
    persona_rows <- LLMR.shiny::persona_selector_server("personas", .fg_personas())

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
        "Focus group, up to %d transcript utterances; retries excluded",
        plan$utterances
      )
    }

    output$cost_note <- shiny::renderUI({
      flow <- input$flow %||% "round_robin"
      extra <- if (identical(flow, "desire_based"))
        paste(
          "Desire based adds one scoring call per eligible participant",
          "before each response."
        ) else ""
      plan <- estimate_run()
      scale <- if (identical(shared$mode(), "demo")) {
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
      shiny::tags$p(class = "text-muted small",
        paste(
          scale,
          "Retries and incomplete-utterance continuations are excluded and can add calls.",
          sprintf(
            "Live runs above %d planned calls require confirmation.",
            .fg_large_run_threshold
          ),
          extra
        ))
    })

    shiny::observe({
      if (!identical(shared$active_tab(), "run")) return()
      plan <- estimate_run()
      if (identical(shared$mode(), "live")) {
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
      completed <- 0L
      run_output <- shiny::withProgress(
        message = "Running Focus Group", value = 0,
        {
          live_runner <- LLMR.shiny::build_runner("live")
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
                  "Completed %d of %d planned API calls",
                  completed, planned
                )
              } else {
                sprintf(
                  "Completed %d API calls; %d were planned",
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
                guide = .fg_gui_guide(request$topic),
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
                  "Run complete after %d API calls; %d were planned",
                  completed, planned
                )
              } else {
                "Run complete"
              }
            )
          }
          list(
            result = res,
            completed = if (completed > 0L) completed else planned
          )
        }
      )

      res <- run_output$result
      if (!res$ok) {
        err(res$ui)
        return()
      }
      result(res$value)
      if (is.function(shared$finished_session)) {
        shared$finished_session(res$value)
      }
      usage <- res$value$usage %||% list()
      shared$add_usage(list(calls = run_output$completed,
                            sent = usage$sent %||% 0L,
                            received = usage$rec %||% 0L))
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
    output$download_rds <- shiny::downloadHandler(
      filename = function() "focus_group.rds",
      content = function(file) saveRDS(result()$focus_group, file))

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
      DT::datatable(.fg_demo_transcript(), options = list(pageLength = 8, scrollX = TRUE))
    })

    output$transcript <- DT::renderDT({
      shiny::req(result())
      tr <- result()$transcript
      cols <- intersect(c("message_id", "round", "speaker_id", "text"), names(tr))
      DT::datatable(tr[, cols, drop = FALSE], options = list(pageLength = 10, scrollX = TRUE))
    })

    output$usage <- shiny::renderPrint({
      shiny::req(result())
      usage <- result()$usage
      cat("Transcript utterances:", nrow(result()$transcript), "\n")
      cat("Sent tokens:", usage$sent %||% NA, "\n")
      cat("Received tokens:", usage$rec %||% NA, "\n")
    })

    output$timing_table <- DT::renderDT({
      shiny::req(result())
      timing <- .fg_timing_stats(result()$transcript)
      shiny::req(timing$has_durations)
      shown <- timing$speaker_stats
      shown$total_seconds <- round(shown$total_seconds, 2)
      shown$mean_seconds <- round(shown$mean_seconds, 2)
      DT::datatable(
        shown,
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tip")
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
  if (is.null(summary)) summary <- fg$current_conversation_summary %||% "N/A"
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
  for (history in histories) {
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
        sent_tokens = as.integer(meta$sent_tokens %||% 0L),
        rec_tokens = as.integer(meta$rec_tokens %||% 0L),
        total_tokens = as.integer(meta$total_tokens %||% 0L),
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
  words <- lengths(strsplit(trimws(responses$response_text), "\\s+"))
  words[!nzchar(trimws(responses$response_text))] <- 0L
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
  plot_data$words <- lengths(strsplit(
    trimws(plot_data$response_text), "\\s+"
  ))
  plot_data$words[!nzchar(trimws(plot_data$response_text))] <- 0L
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

  fg <- focus_group_from_transcript(
    obj,
    speaker_col = speaker_col,
    text_col = text_col
  )
  # Retain exported transcript metadata when row alignment is unchanged.
  if (length(fg$conversation_log) == nrow(obj)) {
    fields <- intersect(
      c(
        "message_id", "round", "is_moderator", "phase", "response_id",
        "finish_reason", "sent_tokens", "rec_tokens", "total_tokens",
        "duration_s", "provider", "model", "timestamp"
      ),
      names(obj)
    )
    for (i in seq_len(nrow(obj))) {
      for (field in fields) {
        value <- obj[[field]][[i]]
        if (identical(field, "timestamp")) {
          value <- suppressWarnings(as.POSIXct(as.character(value)))
        }
        fg$conversation_log[[i]][[field]] <- value
      }
    }
  }
  list(focus_group = fg, transcript = obj)
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

# ---- analyze module ---------------------------------------------------------

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
      include_plots = TRUE
    ))
    source_label <- shiny::reactiveVal("No transcript loaded")
    err <- shiny::reactiveVal(NULL)
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
    column_defaults <- function(df) {
      columns <- LLMR.shiny::column_names_for_mapping(df)
      speaker <- intersect(
        c("speaker_id", "speaker", "agent", "role"), columns
      )
      text <- intersect(
        c("text", "utterance", "message", "content"), columns
      )
      list(
        columns = columns,
        speaker = if (length(speaker)) speaker[[1]] else columns[[1]],
        text = if (length(text)) {
          text[[1]]
        } else {
          columns[[min(2L, length(columns))]]
        }
      )
    }

    output$module_ui <- shiny::renderUI({
      can_model <- model_ready()
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
          shiny::actionButton(
            ns("analyze"),
            "Run descriptive analysis",
            class = "btn-primary"
          ),
          shiny::actionButton(
            ns("model_analyze"),
            shiny::tagList(
              "Generate themes and model summary ",
              LLMR.shiny::help_tip(
                "Runs the package's thematic analysis and model summary as two live calls."
              )
            ),
            class = "btn-secondary",
            disabled = !can_model
          ),
          shiny::tags$p(
            class = "text-muted small mt-2",
            if (can_model) {
              paste(
                "Model analysis is available with the live provider and",
                "generation settings in the sidebar."
              )
            } else {
              paste(
                "Model themes and the model summary are disabled in Demo mode",
                "or when the selected live provider has no key or model."
              )
            }
          ),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
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
      analysis_settings(list(num_topics = 5L, include_plots = TRUE))
    }

    shiny::observeEvent(input$file, {
      err(NULL)
      reset_analysis()
      obj <- tryCatch(
        if (grepl("\\.rds$", input$file$name, ignore.case = TRUE)) readRDS(input$file$datapath)
        else LLMR.shiny::read_csv_upload(input$file),
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
      defaults <- column_defaults(df)
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
        defaults <- column_defaults(obj)
        speaker_col <- speaker_col %||% defaults$speaker
        text_col <- text_col %||% defaults$text
      }
      prepared <- tryCatch(
        .fg_prepare_analysis_source(
          obj,
          speaker_col = speaker_col,
          text_col = text_col
        ),
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
        include_plots = isTRUE(input$include_plots)
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
              .runner = LLMR.shiny::build_runner("live")
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
      analysis_settings(list(
        num_topics = input$num_topics %||% 5L,
        include_plots = isTRUE(input$include_plots)
      ))
      shared$add_usage(list(calls = 2L, sent = 0L, received = 0L))
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
          DT::DTOutput(ns("tfidf"))
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
      DT::datatable(
        .fg_transcript_csv(transcript()),
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 8)
      )
    })

    output$basic_stats <- DT::renderDT({
      shiny::req(analysis())
      DT::datatable(
        as.data.frame(analysis()$basic_stats$speaker_stats),
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
      )
    })

    output$full_transcript <- shiny::renderText({
      shiny::req(analysis())
      analysis()$basic_stats$full_transcript %||% ""
    })

    output$topic_labels <- DT::renderDT({
      shiny::req(analysis(), length(analysis()$topics))
      DT::datatable(
        as.data.frame(analysis()$topics$topic_labels),
        rownames = FALSE,
        options = list(pageLength = 8)
      )
    })

    output$topic_terms <- DT::renderDT({
      shiny::req(analysis(), length(analysis()$topics))
      DT::datatable(
        as.data.frame(analysis()$topics$top_terms),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$document_topics <- DT::renderDT({
      shiny::req(analysis(), length(analysis()$topics))
      DT::datatable(
        as.data.frame(analysis()$topics$document_topics),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$tfidf <- DT::renderDT({
      shiny::req(analysis(), nrow(analysis()$tfidf))
      DT::datatable(
        as.data.frame(analysis()$tfidf),
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$readability <- DT::renderDT({
      shiny::req(analysis(), nrow(analysis()$readability))
      DT::datatable(
        as.data.frame(analysis()$readability),
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
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
      shown <- timing$speaker_stats
      shown$total_seconds <- round(shown$total_seconds, 2)
      shown$mean_seconds <- round(shown$mean_seconds, 2)
      DT::datatable(
        shown,
        rownames = FALSE,
        options = list(pageLength = 8, dom = "tip")
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
      DT::datatable(
        issue_table,
        rownames = FALSE,
        options = list(pageLength = 8, scrollX = TRUE)
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
    warn_card <- function(message) {
      bslib::card(class = "border-warning", bslib::card_body(message))
    }
    warn_user <- function(message, ui = warn_card(message)) {
      err(ui)
      shiny::showNotification(message, type = "warning")
      invisible(NULL)
    }

    output$module_ui <- shiny::renderUI({
      demo_mode <- identical(shared$mode(), "demo")
      bslib::card(
        bslib::card_header("Continuation experiment"),
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
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
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
      shiny::validate(shiny::need(
        !is.null(g),
        "Load a saved focus group to configure the comparison."
      ))
      # Index off speaker messages only; the System roster row is dropped.
      dt <- .fg_display_messages(g$conversation_log)
      n <- nrow(dt)
      if (!n) return(warn_card("The loaded focus group has no conversation messages."))
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
          ),
        shiny::actionButton(
          ns("run"), "Generate continuation responses",
          class = "btn-primary",
          disabled = identical(shared$mode(), "demo")
        ))
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
      if (identical(shared$mode(), "live") && !is.null(fg())) {
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
        perturb_message_id = j,
        arms = arms,
        runs = runs,
        question = input$question %||% "N/A",
        provider = provider,
        config = config_result$value,
        calls = calls
      )
    }

    execute_run <- function(request) {
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
              .runner = LLMR.shiny::build_runner("live"),
              .progress = progress
            ),
            request$provider
          )
        }
      )
      if (!res$ok) { err(res$ui); return() }
      result(res$value)
      shared$add_usage(list(
        calls = request$calls,
        sent = sum(res$value$sent_tokens %||% 0L, na.rm = TRUE),
        received = sum(res$value$rec_tokens %||% 0L, na.rm = TRUE)
      ))
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
      summary$mean_words <- round(summary$mean_words, 1)
      summary$sd_words <- round(summary$sd_words, 1)
      DT::datatable(
        summary,
        rownames = FALSE,
        options = list(dom = "t", pageLength = nrow(summary))
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
