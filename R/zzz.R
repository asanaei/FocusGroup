#' FocusGroup: Simulated Moderated Discussions
#' @docType package
#' @name FocusGroup-package
#' @keywords internal
#'
#' @description
#' FocusGroup simulates moderated discussions with language model participants.
#' A [FocusGroup] object advances a moderator script, selects participant
#' speakers, and stores each message in a structured conversation log. It can
#' also hold an imported transcript for analysis.
#'
#' @details
#' [run_focus_group()] accepts phase counts or ordered moderator instructions
#' and returns a structured `focus_group_result`. For direct construction,
#' create [FGAgent] objects and a [ConversationFlow], then pass them to
#' [FocusGroup]. Built-in flows are constructed with
#' [create_conversation_flow()].
#'
#' [create_agents()], [create_agents_from_data()], and
#' [create_agents_from_survey()] construct agents from direct personas or
#' respondent records. [focus_group_from_transcript()] imports an existing
#' transcript without generating new turns.
#'
#' Model calls require an explicit `config` built with [LLMR::llm_config()].
#' The high-level runner and agent constructors accept `.runner`, an optional
#' function used instead of live model calls. It uses a request and response
#' data-frame format.
#' Descriptive analysis of an imported transcript does not require a provider
#' call. Model summaries and thematic analysis are opt-in through an explicit
#' `config`.
#'
#' @author Ali Sanaei \email{sanaei@@uchicago.edu}
#' @keywords package focus-group llm simulation qualitative-research
"_PACKAGE"


# Set package options on load without overriding user choices
#' @noRd
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.focusgroup <- list(
    focusgroup.seed = getOption("focusgroup.seed", NA_integer_),
    focusgroup.max_participant_responses = getOption("focusgroup.max_participant_responses", 3)
  )
  toset <- !(names(op.focusgroup) %in% names(op))
  if (any(toset)) options(op.focusgroup[toset])
  invisible()
}
