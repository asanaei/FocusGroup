% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FGAgent.R
\name{FGAgent}
\alias{FGAgent}
\title{FGAgent Class}
\description{
Represents an agent (participant or moderator) in a focus group simulation.
Each agent has a unique ID, a persona, an LLM configuration, and methods to
generate utterances and express a desire to speak.
}
\section{Customizing Agents}{

`FGAgent` is designed to be flexible:
\itemize{
  \item **Per-Agent LLM Configuration**: Each agent is initialized with its own `model_config`.
    This allows different agents to use different LLMs, temperatures, or even providers.
  \item **Persona Definition**: The `agent_details` (containing `demographics`,
    `survey_responses`, `direct_persona_description`, and/or `communication_style`)
    are used to construct the `persona_description` and `communication_style_instruction`.
  \item **Subclassing**: For advanced customization, inherit from `FGAgent` to override
    methods or add new fields.
}
}

\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{Character. Unique identifier for the agent.}

\item{\code{persona_description}}{Character. Textual description of the agent's persona,
derived from demographics, survey responses, or direct input.}

\item{\code{communication_style_instruction}}{Character. A specific instruction about the agent's
communication style, to be included in prompts.}

\item{\code{model_config}}{An `llm_config` object (from the `LLMR` package) specifying the
LLM provider, model, API key, and other parameters for this agent.}

\item{\code{is_moderator}}{Logical. `TRUE` if the agent is the moderator, `FALSE` otherwise.}

\item{\code{history}}{List. A log of utterances made by this agent during the simulation.}

\item{\code{tokens_sent_agent}}{Numeric. Total tokens sent by this agent.}

\item{\code{tokens_received_agent}}{Numeric. Total tokens received by this agent.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-FGAgent-new}{\code{FGAgent$new()}}
\item \href{#method-FGAgent-generate_utterance}{\code{FGAgent$generate_utterance()}}
\item \href{#method-FGAgent-get_need_to_talk}{\code{FGAgent$get_need_to_talk()}}
\item \href{#method-FGAgent-clone}{\code{FGAgent$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FGAgent-new"></a>}}
\if{latex}{\out{\hypertarget{method-FGAgent-new}{}}}
\subsection{Method \code{new()}}{
Initialize a new FGAgent.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FGAgent$new(id, agent_details, model_config, is_moderator = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{id}}{Character. A unique identifier for the agent.}

\item{\code{agent_details}}{List. Contains information to build the agent's persona.
Can include:
\itemize{
  \item `demographics`: A named list of demographic attributes (e.g., `list(age = 30, occupation = "teacher")`).
  \item `survey_responses`: A named list of survey questions and answers.
  \item `direct_persona_description`: A character string to be used directly as the persona. Overrides demographics/survey if provided.
  \item `communication_style`: A character string describing the agent's communication style (e.g., "analytical and direct", "empathetic and story-driven").
}
If `is_moderator` is `TRUE` and no specific details are provided, a default moderator persona is used.}

\item{\code{model_config}}{An `llm_config` object from `LLMR::llm_config()`.}

\item{\code{is_moderator}}{Logical. `TRUE` if this agent is the moderator, `FALSE` otherwise.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FGAgent-generate_utterance"></a>}}
\if{latex}{\out{\hypertarget{method-FGAgent-generate_utterance}{}}}
\subsection{Method \code{generate_utterance()}}{
Generate an utterance for the agent.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FGAgent$generate_utterance(
  topic,
  conversation_history_string,
  utterance_prompt_template,
  max_tokens_utterance = 150,
  current_moderator_question = "N/A",
  conversation_summary_so_far = "N/A",
  current_phase = "discussion"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{topic}}{Character. The current discussion topic.}

\item{\code{conversation_history_string}}{Character. Formatted string of recent conversation history.}

\item{\code{utterance_prompt_template}}{Character. The prompt template to use.}

\item{\code{max_tokens_utterance}}{Integer. Maximum tokens for the generated utterance.}

\item{\code{current_moderator_question}}{Character. The current question posed by the moderator.}

\item{\code{conversation_summary_so_far}}{Character. A summary of earlier parts of the conversation.}

\item{\code{current_phase}}{Character. The current phase of the focus group (e.g., "icebreaker", "exploration").}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Character string containing the agent's generated utterance.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FGAgent-get_need_to_talk"></a>}}
\if{latex}{\out{\hypertarget{method-FGAgent-get_need_to_talk}{}}}
\subsection{Method \code{get_need_to_talk()}}{
Get the agent's "desire to talk" score.
This method queries the LLM to rate how strongly the agent feels the need to
contribute to the discussion at the current moment.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FGAgent$get_need_to_talk(
  topic,
  conversation_history_string,
  desire_prompt_template,
  max_tokens_desire = 20,
  current_moderator_question = "N/A",
  last_speaker_id = "N/A",
  last_utterance_text = "N/A"
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{topic}}{Character. The current discussion topic.}

\item{\code{conversation_history_string}}{Character. Formatted string of recent conversation history.}

\item{\code{desire_prompt_template}}{Character. The prompt template to use for this query.}

\item{\code{max_tokens_desire}}{Integer. Maximum tokens for the LLM's response to the desire query.}

\item{\code{current_moderator_question}}{Character. The current question posed by the moderator.}

\item{\code{last_speaker_id}}{Character. The ID of the agent who spoke last.}

\item{\code{last_utterance_text}}{Character. The text of the last utterance.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Numeric. A score from 0 (no desire) to 10 (very strong desire).
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-FGAgent-clone"></a>}}
\if{latex}{\out{\hypertarget{method-FGAgent-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{FGAgent$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
