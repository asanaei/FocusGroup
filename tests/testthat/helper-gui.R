# Offline stand-ins for the Studio tests: an echo agent and group for the
# continuation experiment, a small conversation log, and the shared context
# that LLMR.shiny::shell_context() returns.

fake_fg_agent <- function() {
  env <- new.env()
  env$clone <- function(deep = TRUE) env
  env$generate_utterance <- function(topic, conversation_history_string, ...) {
    list(text = paste0("ECHO> ", conversation_history_string))
  }
  class(env) <- c("R6", "environment")
  env
}

fake_fg <- function() {
  list(topic = "transit", agents = list(P1 = fake_fg_agent()),
       prompt_templates = list(participant_utterance_subtle_persona = "{{conversation_history}}"),
       max_tokens_utterance = 160L, message_mode = "roleflip")
}

sample_fg_log <- function() {
  list(list(message_id = 1L, round = 1L, speaker_id = "Moderator", text = "What about transit?"),
       list(message_id = 2L, round = 1L, speaker_id = "P1", text = "Buses are unreliable."),
       list(message_id = 3L, round = 1L, speaker_id = "P2", text = "Cost is my issue."))
}

# A minimal fake `shared` context (the shape shell_context returns: reactives
# plus add_usage/can_run). Lets us drive the run module offline.
fake_shared <- function(mode = "live", can_run = TRUE, active_tab = "run") {
  usage <- new.env(); usage$last <- NULL
  tab <- active_tab
  finished_session <- shiny::reactiveVal(NULL)
  analysis_handoff <- shiny::reactiveVal(NULL)
  list(
    provider = function() "groq",
    model = function() "openai/gpt-oss-20b",
    mode = function() mode,
    temperature = function() 0.4,
    max_tokens = function() 480L,
    reasoning_effort = function() "low",
    active_tab = function() tab,
    key = function() list(found = can_run, var = "GROQ_API_KEY"),
    can_run = function() can_run,
    finished_session = finished_session,
    analysis_handoff = analysis_handoff,
    set_plan = function(calls, label = "Next run") {
      usage$plan <- list(calls = calls, label = label)
    },
    add_usage = function(tokens) usage$last <- tokens,
    .usage = usage)
}
