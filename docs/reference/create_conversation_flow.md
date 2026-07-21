# Create a Conversation Flow Object

Factory function to create an instance of a \`ConversationFlow\`
subclass.

## Usage

``` r
create_conversation_flow(mode, agents, moderator_id, flow_params = list())
```

## Arguments

- mode:

  Character. The type of turn-taking flow to create. Supported:
  "round_robin", "probabilistic", "desire_based".

- agents:

  A named list of \`FGAgent\` objects.

- moderator_id:

  Character. The ID of the moderator agent.

- flow_params:

  List. Additional parameters specific to the chosen flow type. For
  "probabilistic": \`initial_propensities\` (named numeric vector),
  \`recovery_increment\` (numeric). For "desire_based":
  \`min_desire_threshold\` (numeric).

## Value

An initialized \`ConversationFlow\` object (e.g., \`RoundRobinFlow\`,
\`ProbabilisticFlow\`, \`DesireBasedFlow\`).

## Examples

``` r
# Assuming FGAgent class and agents list (agent1, agent2, mod) are defined
# mod_id <- "MOD"
# all_my_agents <- list(P1 = agent1, P2 = agent2, MOD = mod)
# flow <- create_conversation_flow("desire_based", all_my_agents, mod_id)
```
