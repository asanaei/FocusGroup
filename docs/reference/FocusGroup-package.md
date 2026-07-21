# FocusGroup: Simulated Moderated Discussions

FocusGroup simulates moderated discussions with language model
participants. A \[FocusGroup\] object advances a moderator script,
selects participant speakers, and stores each message in a structured
conversation log. It can also hold an imported transcript for analysis.

## Details

\[run_focus_group()\] accepts phase counts or ordered moderator
instructions and returns a structured \`focus_group_result\`. For direct
construction, create \[FGAgent\] objects and a \[ConversationFlow\],
then pass them to \[FocusGroup\]. Built-in flows are constructed with
\[create_conversation_flow()\].

\[create_agents()\], \[create_agents_from_data()\], and
\[create_agents_from_survey()\] construct agents from direct personas or
respondent records. \[focus_group_from_transcript()\] imports an
existing transcript without generating new turns.

Model calls require an explicit \`config\` built with
\[LLMR::llm_config()\]. The high-level runner and agent constructors
accept a \`.runner\` function using the experiments-frame contract for
caller-controlled execution. Descriptive analysis of an imported
transcript does not require a provider call. Model summaries and
thematic analysis are opt-in through an explicit \`config\`.

## See also

Useful links:

- <https://github.com/asanaei/FocusGroup>

- <https://asanaei.github.io/FocusGroup/>

- Report bugs at <https://github.com/asanaei/FocusGroup/issues>

## Author

Ali Sanaei <sanaei@uchicago.edu>
