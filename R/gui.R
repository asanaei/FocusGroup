# gui.R -------------------------------------------------------------------------
# An optional Shiny GUI for FocusGroup, launched with run_focus_studio(). It has
# three tabs. Run a focus group (live) starts a fresh moderated session and
# offers it as a downloadable .rds. Analyze (offline, no key) loads a transcript
# and reads its participation and word statistics. Continuation experiment (live)
# cuts a conversation at a message, perturbs an earlier message's text, and generates
# the next message under the original and perturbed histories so the downstream message
# can be compared as a dependent variable.
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
#' transcript, and offers it as a downloadable \code{.rds}. Analyze loads a
#' transcript and reads its participation and word statistics offline. The
#' continuation experiment takes a saved focus group up to a message, perturbs
#' an earlier message, and generates the next message under the original and
#' perturbed histories, so the downstream message can be compared.
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
    fillable = TRUE,
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
  shiny::observe({
    if (identical(input$main_nav, "analyze")) shared$set_plan(0L)
  })
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

# ---- ANES persona helpers (for the Run tab's participant picker) ------------

# The example personas frame, from LLMR (the shared home).
.fg_personas <- function() LLMR::anes_2024_personas

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
                                   config, data = NULL, .runner = NULL) {
  data <- data %||% .fg_personas()
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
          shiny::radioButtons(ns("source"), "Participants drawn from",
            choices = c("Synthetic personas" = "synthetic",
                        "ANES 2024 personas" = "anes"),
            selected = "synthetic", inline = TRUE),
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'anes'", ns("source")),
            shiny::tags$p(class = "text-muted small",
              "Pick rows to use as participants (click to toggle; the list runs from most liberal at the top to most conservative at the bottom). Select none to draw a diverse sample automatically."),
            LLMR.shiny::persona_selector_ui(ns("personas"))),
          shiny::fluidRow(
            shiny::column(4, shiny::selectInput(ns("flow"), "Turn-taking flow",
              choices = c("Round robin" = "round_robin",
                          "Desire based" = "desire_based",
                          "Probabilistic" = "probabilistic"),
              selected = "round_robin")),
            shiny::column(4, shiny::selectInput(ns("message_mode"), "Message construction",
              choices = c("Role flip" = "roleflip", "Flat" = "flat"),
              selected = "roleflip")),
            shiny::column(4, shiny::numericInput(ns("max_resp"),
              "Max responses per question", value = 1, min = 1, max = 3, step = 1))),
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput(ns("seed"),
              "Seed (speaker order only; not LLM output)", value = 110, step = 1))),
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
        LLMR.shiny::build_llm_config(provider, model, temperature = 0.7),
        provider
      )
      if (!cfg_result$ok) {
        err(cfg_result$ui)
        return(NULL)
      }

      plan <- estimate_run()
      shared$set_plan(plan$calls, plan_label(plan))
      list(
        topic = topic,
        provider = provider,
        config = cfg_result$value,
        plan = plan,
        source = input$source %||% "synthetic",
        n_participants = as.integer(input$n_participants %||% 3L),
        rows = persona_rows(),
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

    # The object to download: the FocusGroup R6, which the Continuation tab loads.
    output$download <- shiny::downloadHandler(
      filename = function() "focus_group.rds",
      content = function(file) saveRDS(result()$focus_group, file))

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
      shiny::tagList(
        shiny::tags$h5("Transcript"), DT::DTOutput(ns("transcript")),
        shiny::tags$h5("Usage"), shiny::verbatimTextOutput(ns("usage")),
        shiny::downloadButton(ns("download"), "Download Saved Focus Group"))
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
                              conversation_log = NULL) {
  a <- if (inherits(agent, "R6")) agent$clone(deep = TRUE) else agent
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

# Run the paired experiment for a fixed next speaker. By default the
# continuation inherits the conversation summary and the phase from the loaded
# focus group / cut point, so the prompt matches what the main run loop would
# have built; pass `summary`/`phase` explicitly to override.
.fg_run_experiment <- function(fg, log, speaker_id, perturb_message_id,
                               perturb_text,
                               question = "N/A", summary = NULL, phase = NULL,
                               .progress = NULL) {
  agent <- fg$agents[[speaker_id]]
  if (is.null(agent)) stop("No agent with id '", speaker_id, "'.", call. = FALSE)
  # Replay the SAME message construction the saved run used (recorded on the
  # object as fg$message_mode), so a saved flat run does not silently replay as
  # default roleflip. Pin it for the duration of this experiment.
  saved_mode <- fg$message_mode %||%
    getOption("focusgroup.message_mode", "roleflip")
  old_mode <- getOption("focusgroup.message_mode")
  options(focusgroup.message_mode = saved_mode)
  on.exit(options(focusgroup.message_mode = old_mode), add = TRUE)
  # Mode-aware template: flat transcript or role-flipped instruction.
  template <- .fg_pick_template(fg$prompt_templates, "utterance", mode = saved_mode)
  max_tokens <- max(fg$max_tokens_utterance %||% 160L, 220L)
  # Inherit the running summary the main loop maintains, and the phase of the
  # message where the conversation was cut, rather than fixed placeholders.
  if (is.null(summary)) summary <- fg$current_conversation_summary %||% "N/A"
  if (is.null(phase)) {
    phase <- if (length(log)) log[[length(log)]]$phase %||% "exploration_question"
             else "exploration_question"
  }

  perturbed_log <- .fg_perturb_log(
    log, as.integer(perturb_message_id), perturb_text)
  control_hist <- .fg_history_string(log)
  perturbed_hist <- .fg_history_string(perturbed_log)

  control <- .fg_utterance_text(.fg_generate_once(
    agent, fg$topic, control_hist, template,
    question, summary, phase, max_tokens,
    conversation_log = log
  ))
  if (is.function(.progress)) .progress("Control response complete")

  perturbed <- .fg_utterance_text(.fg_generate_once(
    agent, fg$topic, perturbed_hist, template,
    question, summary, phase, max_tokens,
    conversation_log = perturbed_log
  ))
  if (is.function(.progress)) .progress("Perturbed response complete")

  list(
    speaker_id = speaker_id,
    perturb_message_id = as.integer(perturb_message_id),
    control = control,
    perturbed = perturbed
  )
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

# ---- analyze module ---------------------------------------------------------

.fg_analyze_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

.fg_analyze_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    raw <- shiny::reactiveVal(NULL); log <- shiny::reactiveVal(NULL); err <- shiny::reactiveVal(NULL)

    output$module_ui <- shiny::renderUI({
      bslib::card(
        bslib::card_header("Analyze a focus-group transcript"),
        bslib::card_body(
          shiny::uiOutput(ns("err")),
          shiny::fluidRow(
            shiny::column(6, shiny::fileInput(
              ns("file"), "Transcript File", accept = c(".csv", ".rds")
            )),
            shiny::column(
              6,
              shiny::actionButton(ns("load_demo"), "Load demo transcript")
            )),
          shiny::tags$p(
            class = "text-muted small",
            "Upload a CSV transcript or saved focus group, or load the bundled demo."
          ),
          shiny::uiOutput(ns("map_ui")),
          shiny::actionButton(ns("analyze"), "Analyze", class = "btn-primary"),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
    })

    output$err <- shiny::renderUI(err())
    warn_card <- function(m) bslib::card(class = "border-warning", bslib::card_body(m))
    warn_user <- function(message) {
      err(warn_card(message))
      shiny::showNotification(message, type = "warning")
      invisible(NULL)
    }

    shiny::observeEvent(input$file, {
      err(NULL); log(NULL)
      obj <- tryCatch(
        if (grepl("\\.rds$", input$file$name, ignore.case = TRUE)) readRDS(input$file$datapath)
        else LLMR.shiny::read_csv_upload(input$file),
        error = function(e) {
          warn_user(conditionMessage(e))
          NULL
        })
      raw(obj)
    })
    shiny::observeEvent(input$load_demo, { err(NULL); raw(.fg_demo_transcript()); log(NULL) })

    output$map_ui <- shiny::renderUI({
      df <- raw(); if (!is.data.frame(df)) return(NULL)
      cols <- LLMR.shiny::column_names_for_mapping(df)
      shiny::fluidRow(
        shiny::column(6, shiny::selectInput(ns("speaker_col"), "Speaker column", choices = cols)),
        shiny::column(6, shiny::selectInput(ns("text_col"), "Text column", choices = cols,
                                            selected = cols[min(2, length(cols))])))
    })

    shiny::observeEvent(input$analyze, {
      err(NULL); obj <- raw()
      if (is.null(obj)) {
        warn_user("Load a transcript or the bundled demo before analyzing.")
        return()
      }
      l <- tryCatch(.fg_as_log(obj, input$speaker_col, input$text_col),
                    error = function(e) {
                      warn_user(conditionMessage(e))
                      NULL
                    })
      log(l)
    })

    output$results <- shiny::renderUI({
      shiny::validate(shiny::need(
        !is.null(log()),
        "Load a transcript or the bundled demo, then select Analyze."
      ))
      shiny::tagList(
        shiny::tags$h5("Transcript"), DT::DTOutput(ns("transcript")),
        shiny::tags$h5("Participation"), DT::DTOutput(ns("participation")),
        shiny::tags$h5("Summary"), shiny::verbatimTextOutput(ns("summary")))
    })

    output$transcript <- DT::renderDT({
      shiny::req(log()); DT::datatable(.fg_log_to_df(log()), options = list(scrollX = TRUE, pageLength = 8))
    })
    output$participation <- DT::renderDT({
      shiny::req(log()); df <- .fg_log_to_df(log())
      tab <- as.data.frame(table(df$speaker_id), stringsAsFactors = FALSE)
      names(tab) <- c("speaker_id","messages")
      tab$words <- vapply(tab$speaker_id, function(s)
        sum(lengths(strsplit(df$text[df$speaker_id == s], "\\s+"))), integer(1))
      DT::datatable(tab[order(-tab$messages), ], options = list(pageLength = 8))
    })
    output$summary <- shiny::renderPrint({
      shiny::req(log()); df <- .fg_log_to_df(log())
      tw <- sum(lengths(strsplit(df$text, "\\s+")))
      cat("messages:     ", nrow(df), "\n")
      cat("speakers:     ", length(unique(df$speaker_id)), "\n")
      cat("total words:  ", tw, "\n")
      cat("avg words/message:", round(tw / max(1, nrow(df)), 1), "\n")
    })
  })
}

# ---- experiment module ------------------------------------------------------

.fg_experiment_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

.fg_experiment_server <- function(id, shared) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    fg <- shiny::reactiveVal(NULL); result <- shiny::reactiveVal(NULL); err <- shiny::reactiveVal(NULL)
    warn_card <- function(m) bslib::card(class = "border-warning", bslib::card_body(m))
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
                "The comparison generates one control response and one perturbed response."
              )
            }),
          shiny::fileInput(ns("file"), "Saved Focus Group", accept = ".rds"),
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
      shiny::tagList(
        shiny::sliderInput(ns("cut"), "Cut the conversation after message k", min = 1, max = n,
                           value = k0, step = 1),
        # The perturbed message must lie within the cut prefix (j <= k), otherwise
        # the treatment would fall outside the history fed to the next message and
        # silently do nothing. The max tracks the cut in the run handler.
        shiny::numericInput(ns("perturb_message_id"), "Perturb message j (must be <= k)",
                            value = 1, min = 1, max = k0, step = 1),
        shiny::textAreaInput(ns("perturb_text"), "New text for message j", value = "", rows = 3),
        shiny::selectInput(ns("speaker"), "Who speaks message k+1?", choices = participants),
        shiny::textInput(ns("question"), "Moderator question in context (optional)", value = "N/A"),
        if (identical(shared$mode(), "live") && !shared$can_run())
          LLMR.shiny::live_run_blocker_ui(shared$key()),
        if (identical(shared$mode(), "demo"))
          shiny::tags$p(
            class = "text-muted small",
            "Switch to Live mode to generate the two continuation responses."
          ),
        shiny::actionButton(
          ns("run"), "Generate next message under both histories",
          class = "btn-primary",
          disabled = identical(shared$mode(), "demo")
        ))
    })

    shiny::observe({
      if (!identical(shared$active_tab(), "experiment")) return()
      if (identical(shared$mode(), "live") && !is.null(fg())) {
        shared$set_plan(
          2L,
          "Continuation comparison, 2 model responses; retries excluded"
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
      if (j >= 1 && j <= nrow(dt) && !nzchar(input$perturb_text %||% ""))
        shiny::updateTextAreaInput(session, "perturb_text", value = dt$text[j] %||% "")
    })

    shiny::observeEvent(input$run, {
      err(NULL); g <- fg()
      if (is.null(g)) {
        warn_user("Load a saved focus group before generating continuations.")
        return()
      }
      # Gate on live mode AND a key: shared$can_run() is TRUE in demo mode, but
      # the continuation always makes a live call, so demo must not run it.
      if (identical(shared$mode(), "demo")) {
        warn_user("Switch to Live mode to generate continuation responses.")
        return()
      }
      if (!isTRUE(shared$can_run())) {
        warn_user(
          "Live mode requires an API key for the selected provider.",
          LLMR.shiny::live_run_blocker_ui(shared$key())
        )
        return()
      }
      dt <- .fg_display_messages(g$conversation_log)
      k <- as.integer(input$cut); j <- as.integer(input$perturb_message_id)
      # Refuse a perturbation outside the cut prefix: otherwise control and
      # treatment histories would be identical and the experiment would report a
      # null effect that is really a null treatment.
      if (anyNA(c(j, k)) || k < 1L || k > nrow(dt) || j < 1L || j > k) {
        warn_user(sprintf(
          "Perturbed message j (%s) must be between 1 and the cut k (%s); otherwise the treatment falls outside the history and does nothing.",
          j, k))
        return()
      }
      perturb_text <- trimws(input$perturb_text %||% "")
      if (!nzchar(perturb_text)) {
        warn_user("Enter replacement text for message j.")
        return()
      }
      speaker <- input$speaker %||% ""
      if (!nzchar(speaker)) {
        warn_user("Select the participant who speaks next.")
        return()
      }
      # Map display messages to log indices and slice the cut prefix.
      log_idx <- dt$log_index[seq_len(k)]
      log_k <- g$conversation_log[log_idx]
      shared$set_plan(
        2L,
        "Continuation comparison, 2 model responses; retries excluded"
      )
      completed <- 0L
      res <- shiny::withProgress(
        message = "Running Continuation Experiment", value = 0,
        {
          progress <- function(detail) {
            completed <<- completed + 1L
            shiny::incProgress(
              1 / 2,
              detail = sprintf(
                "%s (%d of 2 planned API calls)",
                detail, completed
              )
            )
          }
          LLMR.shiny::safe_llmr_call(
            .fg_run_experiment(
              g, log_k, speaker, j, perturb_text,
              input$question %||% "N/A",
              .progress = progress
            ),
            shared$provider()
          )
        }
      )
      if (!res$ok) { err(res$ui); return() }
      result(res$value); shared$add_usage(list(calls = 2L))
    })

    output$results <- shiny::renderUI({
      r <- result()
      shiny::validate(shiny::need(
        !is.null(r),
        "Load a saved focus group and generate the continuation comparison."
      ))
      bslib::layout_column_wrap(width = 1/2,
        bslib::card(bslib::card_header(paste0("Control: ", r$speaker_id, " Speaks Next")),
                    bslib::card_body(shiny::tags$p(r$control))),
        bslib::card(bslib::card_header(paste0("Perturbed: Message ", r$perturb_message_id, " Changed")),
                    bslib::card_body(shiny::tags$p(r$perturbed))))
    })
  })
}
