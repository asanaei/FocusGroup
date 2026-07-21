# FGAgent Class

FGAgent Class

FGAgent Class

## Details

Represents an agent (participant or moderator) in a focus group
simulation. Each agent has a unique ID, a persona, an LLM configuration,
and methods to generate utterances and express a desire to speak.

## Customizing Agents

\`FGAgent\` is designed to be flexible:

- \*\*Per-Agent LLM Configuration\*\*: Each agent is initialized with
  its own \`llm_config\` (stored in the \`config\` field). This allows
  different agents to use different LLMs, temperatures, or even
  providers.

- \*\*Persona Definition\*\*: The \`agent_details\` (containing
  \`demographics\`, \`survey_responses\`,
  \`direct_persona_description\`, and/or \`communication_style\`) are
  used to construct the \`persona_description\` and
  \`communication_style_instruction\`.

- \*\*Subclassing\*\*: For advanced customization, inherit from
  \`FGAgent\` to override methods or add new fields.

## Public fields

- `id`:

  Character. Unique identifier for the agent.

- `persona_description`:

  Character. Textual description of the agent's persona, derived from
  demographics, survey responses, or direct input.

- `communication_style_instruction`:

  Character. A specific instruction about the agent's communication
  style, to be included in prompts.

- `config`:

  An \`llm_config\` object (from the \`LLMR\` package) specifying the
  LLM provider, model, API key, and other parameters for this agent.

- `.runner`:

  \`NULL\` or an experiments-frame runner. It receives a data frame with
  \`config\` and \`messages\` list-columns and returns those rows with
  at least \`response_text\`.

- `is_moderator`:

  Logical. \`TRUE\` if the agent is the moderator, \`FALSE\` otherwise.

- `history`:

  List. A log of utterances made by this agent during the simulation.

- `tokens_sent_agent`:

  Numeric. Total tokens sent by this agent.

- `tokens_received_agent`:

  Numeric. Total tokens received by this agent.

- `role`:

  Character. "moderator" or "participant" for convenience in reports.

- `demographics`:

  Named list. Raw demographics used to build persona.

- `survey_responses`:

  Named list. Raw survey responses used to build persona.

## Methods

### Public methods

- [`FGAgent$new()`](#method-FGAgent-new)

- [`FGAgent$generate_utterance()`](#method-FGAgent-generate_utterance)

- [`FGAgent$get_need_to_talk()`](#method-FGAgent-get_need_to_talk)

- [`FGAgent$clone()`](#method-FGAgent-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new FGAgent.

#### Usage

    FGAgent$new(id, agent_details, config, is_moderator = FALSE, .runner = NULL)

#### Arguments

- `id`:

  Character. A unique identifier for the agent.

- `agent_details`:

  List. Contains information to build the agent's persona. Can include:

  - \`demographics\`: A named list of demographic attributes (e.g.,
    \`list(age = 30, occupation = "teacher")\`).

  - \`survey_responses\`: A named list of survey questions and answers.

  - \`direct_persona_description\`: A character string to be used
    directly as the persona. Overrides demographics/survey if provided.

  - \`communication_style\`: A character string describing the agent's
    communication style (e.g., "analytical and direct", "empathetic and
    story-driven").

  If \`is_moderator\` is \`TRUE\` and no specific details are provided,
  a default moderator persona is used.

- `config`:

  An \`llm_config\` object from \`LLMR::llm_config()\`. It may be
  \`NULL\` only for an analysis-only agent that will not generate
  output.

- `is_moderator`:

  Logical. \`TRUE\` if this agent is the moderator, \`FALSE\` otherwise.

- `.runner`:

  \`NULL\` or an experiments-frame runner. \`NULL\` uses live LLMR
  calls.

------------------------------------------------------------------------

### Method `generate_utterance()`

Generate an utterance for the agent.

#### Usage

    FGAgent$generate_utterance(
      topic,
      conversation_history_string,
      utterance_prompt_template,
      max_tokens_utterance = 150,
      current_moderator_question = "N/A",
      conversation_summary_so_far = "N/A",
      current_phase = "discussion",
      conversation_log = NULL,
      standing_rules = NULL,
      self_state = TRUE
    )

#### Arguments

- `topic`:

  Character. The current discussion topic.

- `conversation_history_string`:

  Character. Formatted string of recent conversation history.

- `utterance_prompt_template`:

  Character. The prompt template to use.

- `max_tokens_utterance`:

  Integer. Maximum tokens for the generated utterance.

- `current_moderator_question`:

  Character. The current question posed by the moderator.

- `conversation_summary_so_far`:

  Character. A summary of earlier parts of the conversation.

- `current_phase`:

  Character. The current phase of the focus group (e.g., "icebreaker",
  "exploration").

- `conversation_log`:

  List or NULL. The structured conversation log. When supplied (and the
  template is not a legacy flat template), the message is built
  role-flipped: this agent's own prior turns become \`assistant\`
  messages and others' become labeled \`user\` messages. \`NULL\` keeps
  the legacy flat single-user-message construction.

- `standing_rules`:

  Character or NULL. The system-message standing rules
  (persona-anchoring, safety, etc.) for the role-flipped path; \`NULL\`
  uses a built-in default appropriate to the agent's role.

- `self_state`:

  Logical. If \`TRUE\` (default), a compact "points you have already
  made" digest of the agent's own prior turns is added to the system
  message (participants only) to discourage self-repetition.

#### Returns

A list with \`text\` (the generated utterance) and \`meta\` (a list of
call metadata: token counts, finish reason, provider, model, timing).

------------------------------------------------------------------------

### Method `get_need_to_talk()`

Get the agent's "desire to talk" score. This method queries the LLM to
rate how strongly the agent feels the need to contribute to the
discussion at the current moment.

#### Usage

    FGAgent$get_need_to_talk(
      topic,
      conversation_history_string,
      desire_prompt_template,
      max_tokens_desire = 20,
      current_moderator_question = "N/A",
      last_speaker_id = "N/A",
      last_utterance_text = "N/A",
      conversation_log = NULL
    )

#### Arguments

- `topic`:

  Character. The current discussion topic.

- `conversation_history_string`:

  Character. Formatted string of recent conversation history.

- `desire_prompt_template`:

  Character. The prompt template to use for this query.

- `max_tokens_desire`:

  Integer. Maximum tokens for the LLM's response to the desire query.

- `current_moderator_question`:

  Character. The current question posed by the moderator.

- `last_speaker_id`:

  Character. The ID of the agent who spoke last.

- `last_utterance_text`:

  Character. The text of the last utterance.

- `conversation_log`:

  List or NULL. The structured conversation log. When supplied (and the
  template is not a legacy flat template), desire scoring is
  role-flipped so the agent reads its own prior turns as its own voice;
  \`NULL\` keeps the legacy flat construction.

#### Returns

Numeric. A score from 0 (no desire) to 10 (very strong desire).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FGAgent$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
