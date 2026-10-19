# Scripted runners for the usage and session-recovery tests. `.usage_runner()`
# returns one usage record per call, or raises a supplied condition;
# `.session_failure(n)` reports usage on every call and fails on call `n`.

.usage_runner <- function(records) {
  i <- 0L
  function(experiments, ...) {
    i <<- i + 1L
    record <- records[[min(i, length(records))]]
    if (inherits(record, "condition")) stop(record)
    experiments$response_text <- "A complete reply."
    for (name in names(record)) experiments[[name]] <- record[[name]]
    experiments
  }
}
.usage_agent <- function(runner) FGAgent$new("P1",
  list(direct_persona_description = "A participant."),
  LLMR::llm_config("openai", "example-model"), .runner = runner)
.usage_utterance <- function(agent) agent$generate_utterance("Hours", "", "State your view.")

.session_failure <- function(n) {
  i <- 0L
  cause <- structure(list(message = "fixture provider failure", call = NULL),
                     class = c("fixture_provider_error", "error", "condition"))
  function(experiments, ...) {
    i <<- i + 1L
    if (i == n) stop(cause)
    experiments$response_text <- "Evening hours matter after work."
    experiments$sent <- 2L; experiments$rec <- 3L; experiments$total <- 9L
    experiments
  }
}
