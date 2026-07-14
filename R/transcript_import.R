# File: R/transcript_import.R
# Purpose: Build a FocusGroup object from an imported (e.g. human) transcript,
# so the package's analysis methods run on conversations it did not simulate.

#' Build a FocusGroup object from an imported transcript
#'
#' Turns a transcript that the package did not generate -- a human focus group,
#' a transcript exported from other software, or a saved CSV -- into a
#' [FocusGroup] object whose `conversation_log` holds the imported turns. The
#' returned object supports the analysis methods (`analyze()`,
#' `analyze_participation_balance()`, `analyze_topics()`, `analyze_tfidf()`,
#' `analyze_readability()`, `analyze_themes()`), the plotting methods, and
#' [analyze_focus_group()]. No model call is made: importing and the
#' descriptive analyses run offline, and only the model-dependent methods
#' (`analyze_themes()`, `summarize()`) contact a provider when you call them.
#'
#' One lightweight agent is created per speaker so that speaker-aware methods
#' have a roster to report on; these agents carry a minimal placeholder persona
#' and are not meant to continue the conversation. The moderator is the first
#' speaker whose id contains "mod" (case-insensitive), e.g. "Moderator" or
#' "MOD"; when no speaker matches, a non-speaking "MOD" agent is added so the
#' object is complete, and it appears in `analyze()` with zero utterances, the
#' package's usual treatment of a silent agent. Rows whose speaker is missing
#' or blank are dropped with a warning; rows with speaker "System" are kept in
#' the log but excluded from analyses, matching how the simulator treats its
#' own roster message.
#'
#' @param data A data frame with one row per turn, in order.
#' @param speaker_col Character. Name of the column holding the speaker id.
#'   Default "speaker".
#' @param text_col Character. Name of the column holding the utterance text.
#'   Default "text".
#' @param topic Character or `NULL`. The discussion topic, used by the
#'   model-dependent analyses and stored on the object. `NULL` falls back to
#'   "Imported transcript".
#' @return A [FocusGroup] object with a populated `conversation_log`.
#' @seealso [analyze_focus_group()] to run the descriptive analyses in one
#'   call; [run_focus_group()] to simulate a discussion instead.
#' @examples
#' transcript <- data.frame(
#'   speaker = c("Moderator", "Ana", "Ben", "Ana", "Moderator", "Ben"),
#'   text = c(
#'     "Welcome, everyone. What do you make of the new library hours?",
#'     "They help working parents like me; evenings matter most.",
#'     "I see it differently. Mornings are now far too crowded.",
#'     "Crowded mornings still beat being locked out after work.",
#'     "Ben, say more about what changed in the mornings.",
#'     "Staff are stretched thin before noon, so lines are longer."
#'   )
#' )
#' fg <- focus_group_from_transcript(transcript, topic = "library hours")
#' fg$analyze_participation_balance()$participation_stats
#' @export
focus_group_from_transcript <- function(data,
                                        speaker_col = "speaker",
                                        text_col = "text",
                                        topic = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame with one row per turn.", call. = FALSE)
  }
  if (!is.character(speaker_col) || length(speaker_col) != 1L ||
      is.na(speaker_col) || !nzchar(speaker_col)) {
    stop("`speaker_col` must name one column in `data`.", call. = FALSE)
  }
  if (!is.character(text_col) || length(text_col) != 1L ||
      is.na(text_col) || !nzchar(text_col)) {
    stop("`text_col` must name one column in `data`.", call. = FALSE)
  }
  if (!all(c(speaker_col, text_col) %in% names(data))) {
    stop("Columns '", speaker_col, "' and '", text_col,
         "' must both be present in `data`.", call. = FALSE)
  }
  if (!is.null(topic) && (!is.character(topic) || length(topic) != 1L ||
                          is.na(topic) || !nzchar(trimws(topic)))) {
    stop("`topic` must be `NULL` or a non-empty character string.", call. = FALSE)
  }

  # Rows without an attributable speaker cannot enter speaker-level analyses.
  spk <- as.character(data[[speaker_col]])
  keep <- !is.na(spk) & nzchar(trimws(spk))
  if (!all(keep)) {
    warning(sprintf("Dropped %d row(s) with a missing or blank speaker.",
                    sum(!keep)), call. = FALSE)
    data <- data[keep, , drop = FALSE]
  }
  if (!nrow(data)) stop("The transcript has no attributable turns.", call. = FALSE)

  log <- .fg_as_log(data, speaker_col, text_col)

  # One placeholder agent per speaker. "System" stays out of the roster: it is
  # the simulator's bookkeeping label, filtered from analyses by convention.
  speakers <- unique(vapply(log, function(m) m$speaker_id, character(1)))
  speakers <- setdiff(speakers, "System")
  if (!length(speakers)) {
    stop("The transcript contains no speakers other than 'System'.", call. = FALSE)
  }
  is_mod <- vapply(speakers, function(s) {
    any(vapply(log, function(m)
      identical(m$speaker_id, s) && isTRUE(m$is_moderator), logical(1)))
  }, logical(1))

  cfg <- default_llmr_config()
  agents <- lapply(seq_along(speakers), function(i) {
    FGAgent$new(
      id = speakers[i],
      agent_details = list(direct_persona_description = paste0(
        "Speaker '", speakers[i], "' in an imported focus group transcript. ",
        "This placeholder stands in for the original speaker for analysis; ",
        "it carries no persona of its own.")),
      llm_config = cfg,
      is_moderator = is_mod[i]
    )
  })
  names(agents) <- speakers

  moderator_id <- if (any(is_mod)) speakers[which(is_mod)[1]] else "MOD"
  if (!moderator_id %in% names(agents)) {
    agents[[moderator_id]] <- FGAgent$new(
      id = moderator_id,
      agent_details = list(direct_persona_description = paste(
        "Placeholder moderator for an imported transcript in which no speaker",
        "was identifiable as the moderator.")),
      llm_config = cfg,
      is_moderator = TRUE
    )
  }

  the_topic <- topic %||% "Imported transcript"
  fg <- FocusGroup$new(
    topic = the_topic,
    purpose = paste("Analyze an imported focus group transcript on:", the_topic),
    agents = agents,
    moderator_id = moderator_id,
    turn_taking_flow = RoundRobinFlow$new(agents, moderator_id),
    # A one-entry placeholder script: the conversation already happened, so
    # nothing here is meant to run (and the empty-script message is not useful).
    question_script = list(list(phase = "generic_discussion", text = the_topic))
  )
  fg$conversation_log <- log
  fg
}
