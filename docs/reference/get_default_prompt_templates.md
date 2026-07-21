# Get Default Prompt Templates

Retrieves a list containing the default prompt templates used by the
\`FocusGroup\` package. These templates guide the behavior of LLM agents
(participants and moderator) during different phases and actions within
the simulation.

## Usage

``` r
get_default_prompt_templates()
```

## Value

A named list where each element is a character string representing a
prompt template. Placeholders like \`{{topic}}\`,
\`{{persona_description}}\`, etc., are used within the templates and
will be filled dynamically during the simulation.

## Details

Users can retrieve this list, modify specific templates, and then pass
the modified list to \`FocusGroup\$new()\` or higher-level wrapper
functions to customize the simulation's prompts.

## Participant templates and message construction

Two participant templates ship here, and which one is used depends on
the message construction (see the \`message_mode\` argument of
\[run_focus_group()\], or \`options(focusgroup.message_mode=)\`).

- \`participant_turn_instruction\` is the canonical template for the
  default role-flipped construction. It carries only the current
  question and the turn cue, because the persona, standing rules, and
  the transcript are supplied structurally (a system message plus
  role-separated turns).

- \`participant_utterance_subtle_persona\` is the flat template. It
  inlines the persona and the whole transcript into one user message. A
  custom template that contains \`{{conversation_history}}\` or
  \`{{persona_description}}\` is treated as a flat template and routed
  through the flat path.

## Examples

``` r
default_prompts <- get_default_prompt_templates()
print(names(default_prompts))
#>  [1] "participant_utterance_subtle_persona"
#>  [2] "participant_turn_instruction"        
#>  [3] "participant_desire_instruction"      
#>  [4] "participant_desire_to_talk_nuanced"  
#>  [5] "moderator_opening"                   
#>  [6] "moderator_icebreaker_question"       
#>  [7] "moderator_engagement_question"       
#>  [8] "moderator_exploration_question"      
#>  [9] "moderator_probing_focused"           
#> [10] "moderator_summarizing"               
#> [11] "moderator_transition"                
#> [12] "moderator_manage_participation"      
#> [13] "moderator_ending_question"           
#> [14] "moderator_closing"                   
#> [15] "moderator_generic_utterance"         
#> [16] "thematic_analysis_prompt"            
print(default_prompts$participant_utterance_subtle_persona)
#> [1] "You are a focus group participant.\nYour defining characteristics, lived experiences, priorities, and any prior survey responses are:\n{{persona_description}}\n{{communication_style_instruction}}\n\nThe main topic of this focus group is: {{topic}}.\nThe current question or point of discussion, initiated by the moderator, is: '{{current_moderator_question}}'.\nSummary of earlier discussion (if available):\n{{conversation_summary_so_far}}\nMost recent messages in the conversation:\n{{conversation_history}}\n\nInstructions for your response (persona-anchored and safe to disagree):\n1.  Internalize and embody the persona above. Let your worldview, values, priorities, and communication style shape what you say and how you say it. Sound like a distinct person, not a generic commentator.\n2.  Do NOT write things like 'As a [role]...' or 'My persona is...'. Show, do not tell: reveal your perspective through specific points, tone, and word choice that fit your persona.\n3.  Advance the conversation: respond directly to the current question and to others' points. If you agree, add a new angle or example. If you disagree, do so respectfully: acknowledge their point, then state your different view and give a clear reason or brief example.\n4.  Make it concrete. Prefer specific reasons, brief anecdotes, or practical examples over vague generalities. Vary your language and avoid repeating yourself or others verbatim.\n5.  Keep it appropriate for a group turn: 2-6 sentences. Aim for one clear claim and one succinct rationale (optionally a short example). Avoid meta-comments about the discussion mechanics.\n6.  Psychological safety: it is safe to disagree respectfully. Critique ideas, not people; use civil language; you may address participants by ID when relevant.\n7.  Never reveal you are an AI model.\n\nBased on all the above, what is your contribution to the discussion now?"

# To customize a prompt:
my_prompts <- get_default_prompt_templates()
my_prompts$moderator_opening <- "Welcome all! Let us talk about {{topic}} today."
# Then use `my_prompts` when creating a FocusGroup object.
```
