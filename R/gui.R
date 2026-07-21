# gui.R -------------------------------------------------------------------------
# An optional Shiny GUI for FocusGroup, launched with run_focus_studio(). It has
# three tabs. Run a focus group (live) starts a fresh moderated session and
# offers it as a downloadable .rds. Analyze (offline, no key) loads a transcript
# and reads its participation and word statistics. Continuation experiment (live)
# cuts a conversation at a turn, perturbs an earlier turn's text, and generates
# the next turn under the original and perturbed histories so the downstream turn
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
#' continuation experiment takes a saved focus group up to a turn, perturbs an
#' earlier turn, and generates the next turn under the original and perturbed
#' histories, so the downstream turn can be compared as a dependent variable.
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
    theme = bslib::bs_theme(version = 5, bootswatch = "sandstone"),
    sidebar = LLMR.shiny::shell_sidebar(),
    bslib::nav_panel("Run a focus group", value = "run", .fg_run_ui("run")),
    bslib::nav_panel("Analyze transcript", value = "analyze", .fg_analyze_ui("analyze")),
    bslib::nav_panel("Continuation experiment", value = "experiment", .fg_experiment_ui("experiment"))
  )
}

.fg_gui_server <- function(input, output, session) {
  shared <- LLMR.shiny::shell_context(input, output, session)
  .fg_run_server("run", shared)
  .fg_analyze_server("analyze", shared)
  .fg_experiment_server("experiment", shared)
}

# A live focus-group action is allowed only in live mode WITH a key. The shared
# substrate's can_run() is TRUE in demo mode by design, but the GUI does not
# accept a programmatic runner, so demo mode must never start a run. Use this
# everywhere a live focus-group call is gated.
.fg_live_ok <- function(shared) {
  identical(shared$mode(), "live") && isTRUE(shared$can_run())
}

# A clean display view of a conversation log for the GUI. run_simulation() logs a
# turn-0 "System" roster entry first, and stored `turn` is not a guaranteed clean
# 1..N index (log_message() falls back to list length). So drop System rows and
# number the rest by position. Returns a data frame mapping the user-facing
# display_turn to the original log_index (into the UNFILTERED log) plus speaker
# and text, used by both the Run and Continuation tabs.
.fg_display_turns <- function(log) {
  if (!length(log))
    return(data.frame(display_turn = integer(0), log_index = integer(0),
                      speaker_id = character(0), text = character(0),
                      stringsAsFactors = FALSE))
  is_real <- vapply(log, function(e) !identical(e$speaker_id, "System"), logical(1))
  idx <- which(is_real)
  data.frame(
    display_turn = seq_along(idx),
    log_index = idx,
    speaker_id = vapply(idx, function(i) as.character(log[[i]]$speaker_id %||% ""), character(1)),
    text = vapply(idx, function(i) as.character(log[[i]]$text %||% ""), character(1)),
    stringsAsFactors = FALSE)
}

# ---- ANES persona helpers (for the Run tab's participant picker) ------------

# The example personas frame, from LLMR (the shared home).
.fg_personas <- function() LLMR::anes_2024_personas

# Build agents from chosen persona rows and run a focus group, returning the same
# shape fg_quick() does so the Run tab renders it identically. `rows` are row
# indices into `data` (empty -> a diverse default draw of `participants`).
.fg_run_from_personas <- function(topic, participants, rows, flow, msg_mode,
                                   seed, max_participant_responses,
                                   llm_config, data = NULL) {
  data <- data %||% .fg_personas()
  chosen <- if (length(rows)) data[rows, , drop = FALSE] else data
  n <- if (length(rows)) length(rows) else participants

  # focusgroup.seed must be in place BEFORE the agents are built: the diverse
  # draw inside create_agents_from_data() reads it. Setting it only around
  # run_simulation() would leave the seed input inert on this path.
  fg <- withr::with_options(
    list(focusgroup.msg_mode = msg_mode, focusgroup.seed = seed),
    {
      agents <- create_agents_from_data(chosen, n_participants = n,
                                        llm_config = llm_config)
      flow_obj <- create_conversation_flow(flow, agents, "MOD")
      script <- list(
        list(phase = "opening"),
        list(phase = "engagement_question",
             text = paste0("From your perspective, what matters most about ", topic, "?")),
        list(phase = "exploration_question",
             text = paste0("Where do you see common ground or disagreement on ", topic, "?")),
        list(phase = "closing"))
      fg <- FocusGroup$new(topic = topic,
                           purpose = paste("Explore perspectives on", topic),
                           agents = agents, moderator_id = "MOD",
                           turn_taking_flow = flow_obj, question_script = script,
                           max_participant_responses = max_participant_responses)
      fg$run_simulation(verbose = FALSE)
      fg
    })
  tr <- if (length(fg$conversation_log))
    do.call(rbind, lapply(fg$conversation_log, function(x) data.frame(
      turn = x$turn, speaker_id = x$speaker_id, text = x$text,
      stringsAsFactors = FALSE))) else data.frame()
  list(transcript = tr,
       summary = fg$final_summary %||% fg$summarize(summary_level = 1),
       totals = list(total_tokens_in = fg$total_tokens_sent,
                     total_tokens_out = fg$total_tokens_received,
                     total_turns = length(fg$conversation_log)),
       focus_group = fg)
}

# ---- run module (live: run a fresh focus group) -----------------------------

.fg_run_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("module_ui"))
}

# `run_fun` is an injection seam: defaults to FocusGroup::fg_quick, overridden in
# tests with a fake so the module can be exercised via shiny::testServer() with
# no live call.
.fg_run_server <- function(id, shared, run_fun = NULL, persona_run_fun = NULL) {
  run_fun <- run_fun %||% FocusGroup::fg_quick
  persona_run_fun <- persona_run_fun %||% .fg_run_from_personas
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    result <- shiny::reactiveVal(NULL); err <- shiny::reactiveVal(NULL)
    warn_card <- function(m) bslib::card(class = "border-warning", bslib::card_body(m))

    # Baseline live-call estimate (also shown in the cost note): 7 moderator +
    # participants x max_resp + 1 summary.
    est_calls <- function() {
      p <- as.integer(input$participants %||% 3L)
      mr <- as.integer(input$max_resp %||% 1L)
      7L + p * mr + 1L
    }

    output$module_ui <- shiny::renderUI({
      bslib::card(
        bslib::card_header("Run a focus group"),
        bslib::card_body(
          shiny::uiOutput(ns("err")),
          shiny::tags$p(class = "text-muted",
            "Run a fresh moderated session live, then download it as an .rds to reuse in the Continuation experiment tab. Running generates text and needs an API key; the provider and model come from the sidebar."),
          shiny::fluidRow(
            shiny::column(8, shiny::textInput(ns("topic"), "Topic",
              value = "public libraries", width = "100%")),
            shiny::column(4, shiny::numericInput(ns("participants"), "Participants",
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
              choices = c("round_robin", "desire_based", "probabilistic"),
              selected = "round_robin")),
            shiny::column(4, shiny::selectInput(ns("msg_mode"), "Message construction",
              choices = c("roleflip", "flat"), selected = "roleflip")),
            shiny::column(4, shiny::numericInput(ns("max_resp"),
              "Max responses per participant / question", value = 1, min = 1, max = 3, step = 1))),
          shiny::fluidRow(
            shiny::column(4, shiny::numericInput(ns("seed"),
              "Seed (speaker order only; not LLM output)", value = 110, step = 1))),
          shiny::uiOutput(ns("cost_note")),
          if (identical(shared$mode(), "demo"))
            warn_card("Demo mode shows an example transcript below; running a focus group needs a key. Switch to live mode and set a key to run your own.")
          else if (!shared$can_run())
            LLMR.shiny::live_run_blocker_ui(shared$key()),
          shiny::actionButton(ns("run"), "Run focus group", class = "btn-primary"),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
    })

    output$err <- shiny::renderUI(err())

    # The selectable persona table is the shared LLMR.shiny module: rows run
    # liberal -> conservative, multi-select, returning the chosen row indices.
    persona_rows <- LLMR.shiny::persona_selector_server("personas", .fg_personas())

    # A rough live-call estimate so the user is not surprised by the bill.
    output$cost_note <- shiny::renderUI({
      p <- as.integer(input$participants %||% 3L)
      mr <- as.integer(input$max_resp %||% 1L)
      flow <- input$flow %||% "round_robin"
      extra <- if (identical(flow, "desire_based"))
        " The desire_based flow adds a desire-scoring call per participant each round, so it costs markedly more." else ""
      shiny::tags$p(class = "text-muted small",
        sprintf("Estimated %d live calls (7 moderator + %d participant + 1 summary). Incomplete utterances and retries can add a few more.%s",
                est_calls(), p * mr, extra))
    })

    shiny::observeEvent(input$run, {
      err(NULL); result(NULL)
      if (!.fg_live_ok(shared)) {
        err(LLMR.shiny::live_run_blocker_ui(shared$key())); return()
      }
      topic <- trimws(input$topic %||% "")
      if (!nzchar(topic)) { err(warn_card("Enter a topic first.")); return() }
      cfg <- LLMR.shiny::build_llm_config(shared$provider(), shared$model(), temperature = 0.7)
      calls_est <- est_calls()
      res <- if (identical(input$source %||% "synthetic", "anes")) {
        LLMR.shiny::safe_llmr_call(
          persona_run_fun(
            topic = topic,
            participants = as.integer(input$participants %||% 3L),
            rows = persona_rows(),
            flow = input$flow %||% "round_robin",
            msg_mode = input$msg_mode %||% "roleflip",
            seed = as.integer(input$seed %||% 110L),
            max_participant_responses = as.integer(input$max_resp %||% 1L),
            llm_config = cfg),
          shared$provider())
      } else {
        LLMR.shiny::safe_llmr_call(
          run_fun(
            topic = topic,
            participants = as.integer(input$participants %||% 3L),
            flow = input$flow %||% "round_robin",
            llm_config = cfg,
            seed = as.integer(input$seed %||% 110L),
            msg_mode = input$msg_mode %||% "roleflip",
            verbose = FALSE,
            max_participant_responses = as.integer(input$max_resp %||% 1L)),
          shared$provider())
      }
      if (!res$ok) { err(res$ui); return() }
      result(res$value)
      # Real token totals; calls is an estimate (fg_quick reports no exact count),
      # passed so the cost tile shows a plausible figure rather than 0 calls.
      tot <- res$value$totals %||% list()
      shared$add_usage(list(calls = calls_est,
                            sent = tot$total_tokens_in %||% 0L,
                            received = tot$total_tokens_out %||% 0L))
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
        return(NULL)
      }
      shiny::tagList(
        shiny::tags$h5("Transcript"), DT::DTOutput(ns("transcript")),
        shiny::tags$h5("Totals"), shiny::verbatimTextOutput(ns("totals")),
        shiny::downloadButton(ns("download"), "Download this focus group (.rds)"))
    })

    output$demo_transcript <- DT::renderDT({
      DT::datatable(.fg_demo_transcript(), options = list(pageLength = 8, scrollX = TRUE))
    })

    output$transcript <- DT::renderDT({
      shiny::req(result())
      tr <- result()$transcript
      cols <- intersect(c("turn", "speaker_id", "text"), names(tr))
      DT::datatable(tr[, cols, drop = FALSE], options = list(pageLength = 10, scrollX = TRUE))
    })

    output$totals <- shiny::renderPrint({
      shiny::req(result())
      tot <- result()$totals
      cat("turns:        ", tot$total_turns %||% NA, "\n")
      cat("tokens in:    ", tot$total_tokens_in %||% NA, "\n")
      cat("tokens out:   ", tot$total_tokens_out %||% NA, "\n")
    })
  })
}

# ---- continuation logic (built on the existing API) -------------------------

# Render a conversation log slice to the history string the run loop itself uses.
.fg_history_string <- function(log) {
  format_conversation_history(log)
}

.fg_perturb_log <- function(log, turn_index, new_text) {
  if (turn_index >= 1L && turn_index <= length(log)) log[[turn_index]]$text <- new_text
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
.fg_run_experiment <- function(fg, log, speaker_id, perturb_turn, perturb_text,
                               question = "N/A", summary = NULL, phase = NULL) {
  agent <- fg$agents[[speaker_id]]
  if (is.null(agent)) stop("No agent with id '", speaker_id, "'.", call. = FALSE)
  # Replay the SAME message construction the saved run used (recorded on the
  # object as fg$msg_mode), so a saved flat run does not silently replay as
  # default roleflip. Pin it for the duration of this experiment.
  saved_mode <- fg$msg_mode %||% getOption("focusgroup.msg_mode", "roleflip")
  old_mm <- getOption("focusgroup.msg_mode")
  options(focusgroup.msg_mode = saved_mode)
  on.exit(options(focusgroup.msg_mode = old_mm), add = TRUE)
  # Mode-aware template (legacy template in flat mode, turn instruction otherwise).
  template <- .fg_pick_template(fg$prompt_templates, "utterance", mode = saved_mode)
  max_tokens <- max(fg$max_tokens_utterance %||% 160L, 220L)
  # Inherit the running summary the main loop maintains, and the phase of the
  # turn the conversation was cut at, rather than fixed placeholders.
  if (is.null(summary)) summary <- fg$current_conversation_summary %||% "N/A"
  if (is.null(phase)) {
    phase <- if (length(log)) log[[length(log)]]$phase %||% "exploration_question"
             else "exploration_question"
  }

  perturbed_log <- .fg_perturb_log(log, as.integer(perturb_turn), perturb_text)
  control_hist <- .fg_history_string(log)
  perturbed_hist <- .fg_history_string(perturbed_log)

  list(
    speaker_id = speaker_id,
    perturb_turn = as.integer(perturb_turn),
    control = .fg_utterance_text(.fg_generate_once(agent, fg$topic, control_hist, template,
                                                   question, summary, phase, max_tokens,
                                                   conversation_log = log)),
    perturbed = .fg_utterance_text(.fg_generate_once(agent, fg$topic, perturbed_hist, template,
                                                     question, summary, phase, max_tokens,
                                                     conversation_log = perturbed_log))
  )
}

# ---- transcript display (analyze mode) ---------------------------------------
# (.fg_as_log, the transcript coercion shared with focus_group_from_transcript(),
# lives in R/utils.R.)

.fg_log_to_df <- function(log) {
  data.frame(
    turn = vapply(log, function(e) as.integer(e$turn %||% NA_integer_), integer(1)),
    speaker_id = vapply(log, function(e) as.character(e$speaker_id %||% ""), character(1)),
    text = vapply(log, function(e) as.character(e$text %||% ""), character(1)),
    stringsAsFactors = FALSE)
}

.fg_demo_transcript <- function() {
  data.frame(
    speaker = c("Moderator","P1","P2","P3","P1","Moderator","P2","P3"),
    text = c("Welcome. Today we discuss public transit. What comes to mind?",
             "Buses are unreliable in my neighborhood; I gave up and drive.",
             "For me it is the cost. Fares went up twice this year.",
             "I like the train, but the last mile is the problem.",
             "Right, even when the bus comes the connection is hard.",
             "So reliability and cost both came up. Anything else?",
             "Safety at night, honestly. I avoid the late routes.",
             "Agreed, lighting at the stops would help a lot."),
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
            shiny::column(6, shiny::fileInput(ns("file"), "Transcript (.csv or .rds)", accept = c(".csv",".rds"))),
            shiny::column(6, shiny::actionButton(ns("load_demo"), "Load demo transcript"))),
          shiny::uiOutput(ns("map_ui")),
          shiny::actionButton(ns("analyze"), "Analyze", class = "btn-primary"),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
    })

    output$err <- shiny::renderUI(err())
    warn_card <- function(m) bslib::card(class = "border-warning", bslib::card_body(m))

    shiny::observeEvent(input$file, {
      err(NULL); log(NULL)
      obj <- tryCatch(
        if (grepl("\\.rds$", input$file$name, ignore.case = TRUE)) readRDS(input$file$datapath)
        else LLMR.shiny::read_csv_upload(input$file),
        error = function(e) { err(warn_card(conditionMessage(e))); NULL })
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
      if (is.null(obj)) { err(warn_card("Load a transcript first.")); return() }
      l <- tryCatch(.fg_as_log(obj, input$speaker_col, input$text_col),
                    error = function(e) { err(warn_card(conditionMessage(e))); NULL })
      log(l)
    })

    output$results <- shiny::renderUI({
      if (is.null(log())) return(NULL)
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
      names(tab) <- c("speaker_id","turns")
      tab$words <- vapply(tab$speaker_id, function(s)
        sum(lengths(strsplit(df$text[df$speaker_id == s], "\\s+"))), integer(1))
      DT::datatable(tab[order(-tab$turns), ], options = list(pageLength = 8))
    })
    output$summary <- shiny::renderPrint({
      shiny::req(log()); df <- .fg_log_to_df(log())
      tw <- sum(lengths(strsplit(df$text, "\\s+")))
      cat("turns:        ", nrow(df), "\n")
      cat("speakers:     ", length(unique(df$speaker_id)), "\n")
      cat("total words:  ", tw, "\n")
      cat("avg words/turn:", round(tw / max(1, nrow(df)), 1), "\n")
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

    output$module_ui <- shiny::renderUI({
      bslib::card(
        bslib::card_header("Continuation experiment"),
        bslib::card_body(
          shiny::uiOutput(ns("err")),
          shiny::tags$p(class = "text-muted",
            "Load a saved FocusGroup (.rds). It carries the agents and the conversation. Generating the next turn is a live operation and needs an API key."),
          shiny::fileInput(ns("file"), "Saved FocusGroup (.rds)", accept = ".rds"),
          shiny::uiOutput(ns("controls")),
          shiny::tags$hr(), shiny::uiOutput(ns("results"))))
    })

    output$err <- shiny::renderUI(err())

    shiny::observeEvent(input$file, {
      err(NULL); result(NULL)
      obj <- tryCatch(readRDS(input$file$datapath), error = function(e) { err(warn_card(conditionMessage(e))); NULL })
      if (!is.null(obj) && !inherits(obj, "FocusGroup")) {
        err(warn_card("The .rds must contain a FocusGroup object.")); obj <- NULL }
      fg(obj)
    })

    output$controls <- shiny::renderUI({
      g <- fg(); if (is.null(g)) return(NULL)
      # Index off real turns only (the turn-0 "System" roster row is dropped),
      # so display turn k means the k-th actual utterance.
      dt <- .fg_display_turns(g$conversation_log)
      n <- nrow(dt)
      if (!n) return(warn_card("The loaded focus group has no conversation turns."))
      k0 <- min(n, max(2, n - 1))
      # The continuation speaks turn k+1, so only participants (not the
      # moderator) are valid next speakers; the participant utterance template
      # is what generate_utterance uses.
      participants <- setdiff(names(g$agents), g$moderator_id %||% character(0))
      if (!length(participants)) participants <- names(g$agents)
      shiny::tagList(
        shiny::sliderInput(ns("cut"), "Cut the conversation after turn k", min = 1, max = n,
                           value = k0, step = 1),
        # The perturbed turn must lie within the cut prefix (j <= k), otherwise
        # the treatment would fall outside the history fed to the next turn and
        # silently do nothing. The max tracks the cut in the run handler.
        shiny::numericInput(ns("perturb_turn"), "Perturb the text of turn j (must be <= k)",
                            value = 1, min = 1, max = k0, step = 1),
        shiny::textAreaInput(ns("perturb_text"), "New text for turn j (the treatment)", value = "", rows = 3),
        shiny::selectInput(ns("speaker"), "Who speaks the next turn (k+1)?", choices = participants),
        shiny::textInput(ns("question"), "Moderator question in context (optional)", value = "N/A"),
        if (identical(shared$mode(), "live") && !shared$can_run())
          LLMR.shiny::live_run_blocker_ui(shared$key()),
        shiny::actionButton(ns("run"), "Generate next turn under both histories", class = "btn-primary"))
    })

    # Keep the perturb-turn ceiling in lockstep with the cut, so the UI cannot
    # offer a j beyond k.
    shiny::observeEvent(input$cut, {
      shiny::updateNumericInput(session, "perturb_turn",
        max = as.integer(input$cut),
        value = min(as.integer(input$perturb_turn %||% 1L), as.integer(input$cut)))
    })

    shiny::observeEvent(input$perturb_turn, {
      g <- fg(); if (is.null(g)) return()
      dt <- .fg_display_turns(g$conversation_log)
      j <- as.integer(input$perturb_turn)
      if (j >= 1 && j <= nrow(dt) && !nzchar(input$perturb_text %||% ""))
        shiny::updateTextAreaInput(session, "perturb_text", value = dt$text[j] %||% "")
    })

    shiny::observeEvent(input$run, {
      err(NULL); g <- fg()
      if (is.null(g)) { err(warn_card("Load a focus group first.")); return() }
      # Gate on live mode AND a key: shared$can_run() is TRUE in demo mode, but
      # the continuation always makes a live call, so demo must not run it.
      if (!.fg_live_ok(shared)) { err(LLMR.shiny::live_run_blocker_ui(shared$key())); return() }
      dt <- .fg_display_turns(g$conversation_log)
      k <- as.integer(input$cut); j <- as.integer(input$perturb_turn)
      # Refuse a perturbation outside the cut prefix: otherwise control and
      # treatment histories would be identical and the experiment would report a
      # null effect that is really a null treatment.
      if (is.na(j) || j < 1L || j > k || k > nrow(dt)) {
        err(warn_card(sprintf(
          "Perturbed turn j (%s) must be between 1 and the cut k (%s); otherwise the treatment falls outside the history and does nothing.",
          j, k)))
        return()
      }
      # Map display turns to log indices (real turns only) and slice the cut
      # prefix; perturb the j-th real turn within that prefix.
      log_idx <- dt$log_index[seq_len(k)]
      log_k <- g$conversation_log[log_idx]
      res <- LLMR.shiny::safe_llmr_call(
        .fg_run_experiment(g, log_k, input$speaker, j,
                           input$perturb_text %||% "", input$question %||% "N/A"),
        shared$provider())
      if (!res$ok) { err(res$ui); return() }
      result(res$value); shared$add_usage(list(calls = 2L))
    })

    output$results <- shiny::renderUI({
      r <- result(); if (is.null(r)) return(NULL)
      bslib::layout_column_wrap(width = 1/2,
        bslib::card(bslib::card_header(paste0("Control - ", r$speaker_id, " says next")),
                    bslib::card_body(shiny::tags$p(r$control))),
        bslib::card(bslib::card_header(paste0("Perturbed (turn ", r$perturb_turn, " changed)")),
                    bslib::card_body(shiny::tags$p(r$perturbed))))
    })
  })
}
