# FocusGroup R6 Class

Main R6 class for managing and running a focus group simulation. It
orchestrates the simulation through distinct phases (Opening,
Icebreaker, Engagement, Exploration, Closing) guided by a question
script, manages agent interactions via a turn-taking flow, and provides
methods for analysis and visualization of the conversation.

## Public fields

- `topic`:

  Character. The main topic of the focus group discussion.

- `purpose`:

  Character. The primary purpose or research objective of conducting the
  focus group.

- `agents`:

  Named list. A list of \`FGAgent\` objects participating in the
  simulation, indexed by their IDs.

- `moderator_id`:

  Character. The ID of the agent designated as the moderator.

- `conversation_log`:

  List. A chronological log of all messages. Each message is a list that
  includes at least \`message_id\`, \`round\`, \`speaker_id\`,
  \`is_moderator\`, \`text\`, \`timestamp\`, \`phase\`, and
  \`metadata\`, along with call metadata: \`response_id\`,
  \`finish_reason\`, \`sent_tokens\`, \`rec_tokens\`, \`total_tokens\`,
  \`duration_s\`, \`provider\`, and \`model\`. \`message_id\` is unique
  message order; moderator and participant messages from one moderator
  cycle share \`round\`.

- `turn_taking_flow`:

  A \`ConversationFlow\` object dictating participant turn-taking.

- `prompt_templates`:

  List. Holds prompt templates for agent/moderator actions.

- `question_script`:

  List. A structured list defining phases and specific questions/actions
  for the moderator. Each element is a list with \`phase\` (e.g.,
  "opening", "icebreaker_question") and optionally \`text\` (for
  specific questions).

- `current_question_text`:

  Character. Text of the current question being discussed.

- `current_conversation_summary`:

  Character. An LLM-generated summary of earlier parts of the
  conversation, used for managing context length in prompts.

- `final_summary`:

  Character. Final LLM-generated summary from the most recent simulation
  run.

- `message_mode`:

  Character. The message construction used by the most recent
  \`run_simulation()\` ("roleflip" or "flat"), recorded so a saved
  object can be replayed (e.g. in the GUI continuation experiment) with
  the same construction.

- `admin_config`:

  An \`llm_config\` object for group-level model tasks.

- `max_tokens_utterance`:

  Integer. Default max tokens for participant utterances.

- `max_tokens_moderator`:

  Integer. Default max tokens for moderator utterances.

- `max_tokens_desire`:

  Integer. Default max tokens for desire-to-talk queries.

- `max_participant_responses`:

  Integer. Maximum number of participant exchanges per round before the
  moderator can intervene. Can also be set globally via
  \`options(focusgroup.max_participant_responses = N)\`.

- `total_tokens_sent`:

  Numeric. Total tokens sent across all LLM calls in the group.

- `total_tokens_received`:

  Numeric. Total tokens received across all LLM calls.

## Methods

### Public methods

- [`FocusGroup$new()`](#method-FocusGroup-new)

- [`FocusGroup$run_simulation()`](#method-FocusGroup-run_simulation)

- [`FocusGroup$summarize()`](#method-FocusGroup-summarize)

- [`FocusGroup$analyze()`](#method-FocusGroup-analyze)

- [`FocusGroup$analyze_topics()`](#method-FocusGroup-analyze_topics)

- [`FocusGroup$analyze_tfidf()`](#method-FocusGroup-analyze_tfidf)

- [`FocusGroup$analyze_readability()`](#method-FocusGroup-analyze_readability)

- [`FocusGroup$analyze_themes()`](#method-FocusGroup-analyze_themes)

- [`FocusGroup$analyze_statistics()`](#method-FocusGroup-analyze_statistics)

- [`FocusGroup$analyze_participation_balance()`](#method-FocusGroup-analyze_participation_balance)

- [`FocusGroup$analyze_response_patterns()`](#method-FocusGroup-analyze_response_patterns)

- [`FocusGroup$analyze_question_patterns()`](#method-FocusGroup-analyze_question_patterns)

- [`FocusGroup$analyze_key_phrases()`](#method-FocusGroup-analyze_key_phrases)

- [`FocusGroup$plot_participation_timeline()`](#method-FocusGroup-plot_participation_timeline)

- [`FocusGroup$plot_word_count_distribution()`](#method-FocusGroup-plot_word_count_distribution)

- [`FocusGroup$plot_participation_by_agent()`](#method-FocusGroup-plot_participation_by_agent)

- [`FocusGroup$plot_message_length_timeline()`](#method-FocusGroup-plot_message_length_timeline)

- [`FocusGroup$clone()`](#method-FocusGroup-clone)

------------------------------------------------------------------------

### Method `new()`

Initialize a new FocusGroup simulation.

#### Usage

    FocusGroup$new(
      topic,
      purpose,
      agents,
      moderator_id,
      turn_taking_flow,
      question_script = list(),
      prompt_templates = list(),
      admin_config = NULL,
      max_tokens_config = list(),
      max_participant_responses = NULL
    )

#### Arguments

- `topic`:

  Character. The main discussion topic.

- `purpose`:

  Character. The primary purpose of the focus group.

- `agents`:

  Named list of initialized \`FGAgent\` objects.

- `moderator_id`:

  Character. The ID of the agent acting as moderator.

- `turn_taking_flow`:

  An initialized \`ConversationFlow\` object.

- `question_script`:

  List. Moderator's script defining phases and questions. If empty, a
  minimal default script (opening, generic discussion, closing) is used.

- `prompt_templates`:

  List. Custom prompt templates. Defaults are used if not provided.

- `admin_config`:

  An \`llm_config\` object for administrative model tasks. If \`NULL\`,
  the moderator's explicitly supplied \`config\` is used.

- `max_tokens_config`:

  List. Optional. Named list with \`utterance\`, \`moderator\`,
  \`desire\` to override default max token limits for these LLM call
  types.

- `max_participant_responses`:

  Integer. Optional. Maximum participant exchanges per round before
  moderator intervention. Defaults to
  \`getOption("focusgroup.max_participant_responses", 3)\`.

------------------------------------------------------------------------

### Method `run_simulation()`

Run the full focus group simulation. Iterates through the
\`question_script\` phases or a specified number of rounds.

#### Usage

    FocusGroup$run_simulation(num_rounds = NULL, verbose = FALSE)

#### Arguments

- `num_rounds`:

  Integer. Optional. Maximum number of moderator cycles to run. If
  \`NULL\` (default), the simulation runs until the \`question_script\`
  is exhausted or the moderator decides to end. If both \`num_rounds\`
  and \`question_script\` are provided, the simulation stops at
  whichever condition is met first.

- `verbose`:

  Logical. If \`TRUE\`, prints progress and utterances to the console.

#### Returns

Invisibly returns the \`conversation_log\`.

------------------------------------------------------------------------

### Method `summarize()`

Generate a summary of the conversation using an LLM.

#### Usage

    FocusGroup$summarize(
      config,
      summary_level = 1,
      max_tokens = NULL,
      internal_call = FALSE,
      transcript_override = NULL,
      .runner = NULL
    )

#### Arguments

- `config`:

  An explicit \`llm_config\` object for the summarization model.

- `summary_level`:

  Integer (1-3). 1: Prose overview, 2: Detailed bulleted, 3: Short
  bulleted takeaways.

- `max_tokens`:

  Integer. Optional. Max tokens for the summary.

- `internal_call`:

  Logical. If TRUE, this is an internal call (e.g. for context window
  management) and token counts are not added to the agent who owns
  \`admin_config\`.

- `transcript_override`:

  Character. Optional. If provided, this transcript is summarized
  instead of \`self\$conversation_log\`.

- `.runner`:

  Optional experiments-frame runner. It receives a data frame with
  \`config\` and \`messages\` list-columns and returns the rows with at
  least \`response_text\`.

#### Returns

Character string containing the generated summary.

------------------------------------------------------------------------

### Method `analyze()`

Basic analysis of the conversation log.

#### Usage

    FocusGroup$analyze(message_ids = NULL, speaker_ids = NULL)

#### Arguments

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze. If
  \`NULL\`, all messages are analyzed.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze. If \`NULL\`,
  analyzes all speakers.

#### Returns

A list with \`speaker_stats\` (a tibble: speaker_id, utterance_count,
total_words, avg_words_per_utterance) and \`full_transcript\` (character
string).

------------------------------------------------------------------------

### Method `analyze_topics()`

Perform LDA topic modeling on the conversation.

#### Usage

    FocusGroup$analyze_topics(
      num_topics = 5,
      min_doc_length = 20,
      top_n_terms = 10,
      message_ids = NULL,
      speaker_ids = NULL,
      seed = 110,
      ...
    )

#### Arguments

- `num_topics`:

  Integer. Number of topics to identify.

- `min_doc_length`:

  Integer. Min words for a speaker's aggregated text to be a document.

- `top_n_terms`:

  Integer. Number of top terms per topic to return.

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

- `seed`:

  Integer or NULL. LDA seed for reproducibility (default 110, the
  package convention); \`NULL\` leaves the LDA control seed unset.

- `...`:

  Additional arguments to \`topicmodels::LDA()\`.

#### Returns

A list with LDA model, topic terms, and document-topic proportions.
\`NULL\` on failure.

------------------------------------------------------------------------

### Method `analyze_tfidf()`

Calculate TF-IDF scores for terms per participant.

#### Usage

    FocusGroup$analyze_tfidf(
      top_n_terms = 10,
      message_ids = NULL,
      speaker_ids = NULL,
      ...
    )

#### Arguments

- `top_n_terms`:

  Integer. Number of top TF-IDF terms per participant.

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

- `...`:

  Additional arguments to \`tidytext::unnest_tokens\`.

#### Returns

A tibble with TF-IDF scores.

------------------------------------------------------------------------

### Method `analyze_readability()`

Calculate readability scores for each participant's aggregated text.

#### Usage

    FocusGroup$analyze_readability(
      measures = "Flesch",
      message_ids = NULL,
      speaker_ids = NULL
    )

#### Arguments

- `measures`:

  Character vector. Readability measure(s) from
  \`quanteda.textstats::textstat_readability\`.

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

#### Returns

A tibble with readability scores.

------------------------------------------------------------------------

### Method `analyze_themes()`

Perform LLM-assisted thematic analysis on the transcript.

#### Usage

    FocusGroup$analyze_themes(
      config,
      message_ids = NULL,
      speaker_ids = NULL,
      .runner = NULL
    )

#### Arguments

- `config`:

  An explicit \`llm_config\` object for thematic analysis.

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

- `.runner`:

  Optional experiments-frame runner. It receives a data frame with
  \`config\` and \`messages\` list-columns and returns the rows with at
  least \`response_text\`.

#### Returns

The thematic summary as a character string. An empty transcript returns
\`character(0)\`. Provider failures are propagated.

------------------------------------------------------------------------

### Method `analyze_statistics()`

Perform statistical analysis on conversation patterns.

#### Usage

    FocusGroup$analyze_statistics(message_ids = NULL, speaker_ids = NULL)

#### Arguments

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

#### Returns

A list with ANOVA results, phase participation stats, and correlations.

------------------------------------------------------------------------

### Method `analyze_participation_balance()`

Analyze participation balance and dominance patterns.

#### Usage

    FocusGroup$analyze_participation_balance(
      message_ids = NULL,
      speaker_ids = NULL
    )

#### Arguments

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

#### Returns

A list with participation statistics and balance metrics.

------------------------------------------------------------------------

### Method `analyze_response_patterns()`

Analyze response patterns and interaction behaviors.

#### Usage

    FocusGroup$analyze_response_patterns(message_ids = NULL, speaker_ids = NULL)

#### Arguments

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

#### Returns

A list with response and interaction pattern metrics.

------------------------------------------------------------------------

### Method `analyze_question_patterns()`

Analyze question asking patterns during the conversation.

#### Usage

    FocusGroup$analyze_question_patterns(message_ids = NULL, speaker_ids = NULL)

#### Arguments

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

#### Returns

A list with question pattern analysis.

------------------------------------------------------------------------

### Method `analyze_key_phrases()`

Extract and analyze key phrases using n-grams.

#### Usage

    FocusGroup$analyze_key_phrases(
      min_freq = 2,
      message_ids = NULL,
      speaker_ids = NULL
    )

#### Arguments

- `min_freq`:

  Integer. Minimum frequency for phrases to be considered key.

- `message_ids`:

  Integer vector. Optional \`message_id\` values to analyze.

- `speaker_ids`:

  Character vector. Optional. Specific speakers to analyze.

#### Returns

A list with bigram and trigram analysis.

------------------------------------------------------------------------

### Method `plot_participation_timeline()`

Create a participation timeline of cumulative messages by participant
across phases.

#### Usage

    FocusGroup$plot_participation_timeline()

#### Returns

ggplot object

------------------------------------------------------------------------

### Method `plot_word_count_distribution()`

Create word count distribution plot showing message length patterns.

#### Usage

    FocusGroup$plot_word_count_distribution()

#### Returns

ggplot object

------------------------------------------------------------------------

### Method `plot_participation_by_agent()`

Create a participation by agent plot showing total messages per
participant.

#### Usage

    FocusGroup$plot_participation_by_agent()

#### Returns

ggplot object

------------------------------------------------------------------------

### Method `plot_message_length_timeline()`

Create a timeline showing message length over message order.

#### Usage

    FocusGroup$plot_message_length_timeline()

#### Returns

ggplot object

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FocusGroup$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
