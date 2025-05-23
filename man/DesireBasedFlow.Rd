% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConversationFlow.R
\name{DesireBasedFlow}
\alias{DesireBasedFlow}
\title{DesireBasedFlow Class}
\description{
Selects the next participant speaker based on their LLM-rated "desire to talk".
The moderator's turns are primarily handled by the `FocusGroup`'s phase/script logic.
This flow is mainly for choosing which participant responds to the moderator.
}
\section{Super class}{
\code{FocusGroup::ConversationFlow} -> \code{DesireBasedFlow}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{last_desire_scores}}{Named numeric vector. Stores the most recent desire scores.}

\item{\code{min_desire_threshold}}{Numeric. Minimum desire score for a participant to be considered.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DesireBasedFlow-new}{\code{DesireBasedFlow$new()}}
\item \href{#method-DesireBasedFlow-select_next_speaker}{\code{DesireBasedFlow$select_next_speaker()}}
\item \href{#method-DesireBasedFlow-get_last_desire_scores}{\code{DesireBasedFlow$get_last_desire_scores()}}
\item \href{#method-DesireBasedFlow-clone}{\code{DesireBasedFlow$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="FocusGroup" data-topic="ConversationFlow" data-id="update_state_post_selection"><a href='../../FocusGroup/html/ConversationFlow.html#method-ConversationFlow-update_state_post_selection'><code>FocusGroup::ConversationFlow$update_state_post_selection()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DesireBasedFlow-new"></a>}}
\if{latex}{\out{\hypertarget{method-DesireBasedFlow-new}{}}}
\subsection{Method \code{new()}}{
Initialize DesireBasedFlow.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DesireBasedFlow$new(agents, moderator_id, min_desire_threshold = 3)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{agents}}{A named list of `FGAgent` objects.}

\item{\code{moderator_id}}{Character. The ID of the moderator agent.}

\item{\code{min_desire_threshold}}{Numeric. Minimum desire score to be eligible.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DesireBasedFlow-select_next_speaker"></a>}}
\if{latex}{\out{\hypertarget{method-DesireBasedFlow-select_next_speaker}{}}}
\subsection{Method \code{select_next_speaker()}}{
Selects the next participant based on desire to talk.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DesireBasedFlow$select_next_speaker(focus_group)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{focus_group}}{The `FocusGroup` object, providing context like current question, history.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The selected `FGAgent` (a participant), or `NULL` if no participant meets threshold.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DesireBasedFlow-get_last_desire_scores"></a>}}
\if{latex}{\out{\hypertarget{method-DesireBasedFlow-get_last_desire_scores}{}}}
\subsection{Method \code{get_last_desire_scores()}}{
Get the last calculated desire scores for participants.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DesireBasedFlow$get_last_desire_scores()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A named numeric vector of desire scores.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DesireBasedFlow-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DesireBasedFlow-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DesireBasedFlow$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
