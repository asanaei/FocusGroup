# ConversationFlow Base Class

\`ConversationFlow\` is an R6 base class defining the interface for
turn-taking mechanisms in a focus group simulation. Subclasses implement
specific strategies for selecting the next speaker.

## Methods for Subclassing

- \`initialize(agents, moderator_id)\`:

  Sets up the flow. Call \`super\$initialize(agents, moderator_id)\`.

- \`select_next_speaker(focus_group)\`:

  \*Required\*. Logic to choose the next \`FGAgent\` to speak. Should
  return the agent object or \`NULL\`.

- \`update_state_post_selection(speaker_id, focus_group)\`:

  Optional. Updates internal state after a speaker has spoken. Base
  implementation updates \`self\$last_speaker_id\`.

## Public fields

- `agents`:

  A named list of \`FGAgent\` objects participating in the conversation.

- `participant_ids`:

  A character vector of agent identifiers, excluding the moderator.

- `moderator_id`:

  Character. The ID of the moderator agent.

- `last_speaker_id`:

  The ID of the agent who last spoke. Can be \`NULL\`.

- `selection_metadata`:

  A list of provenance for the latest selection. Desire-scoring failures
  record the condition and neutral fallback here.

## Methods

### Public methods

- [`ConversationFlow$new()`](#method-ConversationFlow-new)

- [`ConversationFlow$select_next_speaker()`](#method-ConversationFlow-select_next_speaker)

- [`ConversationFlow$update_state_post_selection()`](#method-ConversationFlow-update_state_post_selection)

- [`ConversationFlow$clone()`](#method-ConversationFlow-clone)

------------------------------------------------------------------------

### Method `new()`

Initializes the ConversationFlow object.

#### Usage

    ConversationFlow$new(agents, moderator_id)

#### Arguments

- `agents`:

  A named list of \`FGAgent\` objects.

- `moderator_id`:

  Character. The ID of the moderator agent.

------------------------------------------------------------------------

### Method `select_next_speaker()`

Selects the next speaker. Must be implemented by subclasses.

#### Usage

    ConversationFlow$select_next_speaker(focus_group)

#### Arguments

- `focus_group`:

  The \`FocusGroup\` object managing the simulation, providing context.

#### Returns

The \`FGAgent\` object of the selected speaker, or \`NULL\`.

------------------------------------------------------------------------

### Method `update_state_post_selection()`

Updates internal state after a speaker is selected.

#### Usage

    ConversationFlow$update_state_post_selection(speaker_id, focus_group)

#### Arguments

- `speaker_id`:

  The ID of the agent who was selected and just spoke.

- `focus_group`:

  The \`FocusGroup\` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ConversationFlow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
