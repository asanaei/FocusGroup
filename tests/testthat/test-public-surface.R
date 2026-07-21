test_that("the package exports exactly the first-release surface", {
  expected <- c(
    "ConversationFlow",
    "FGAgent",
    "FocusGroup",
    "analyze_focus_group",
    "create_agents",
    "create_agents_from_data",
    "create_agents_from_survey",
    "create_conversation_flow",
    "focus_group_from_transcript",
    "get_default_prompt_templates",
    "run_focus_group",
    "run_focus_studio"
  )
  expect_identical(sort(getNamespaceExports("FocusGroup")), sort(expected))
})

test_that("public call signatures use the family grammar", {
  run_args <- names(formals(run_focus_group))
  expect_identical(
    run_args,
    c("topic", "config", "n_participants", "guide", "demographics",
      "survey_responses", "flow", "seed", "message_mode", "verbose",
      "max_participant_responses", "max_calls", "confirm", ".runner")
  )

  analysis_args <- names(formals(analyze_focus_group))
  expect_identical(
    analysis_args,
    c("focus_group_result", "num_topics", "include_plots", "config", ".runner")
  )

  for (fun in list(create_agents, create_agents_from_data,
                   create_agents_from_survey)) {
    args <- names(formals(fun))
    expect_true("config" %in% args)
    expect_identical(tail(args, 1L), ".runner")
  }

  agent_init <- names(formals(FGAgent$public_methods$initialize))
  expect_true("config" %in% agent_init)
  expect_identical(tail(agent_init, 1L), ".runner")

  summary_args <- names(formals(FocusGroup$public_methods$summarize))
  theme_args <- names(formals(FocusGroup$public_methods$analyze_themes))
  expect_true("config" %in% summary_args)
  expect_true("config" %in% theme_args)
  expect_identical(tail(summary_args, 1L), ".runner")
  expect_identical(tail(theme_args, 1L), ".runner")
  expect_true("num_rounds" %in%
                names(formals(FocusGroup$public_methods$run_simulation)))
  for (method in c("analyze", "analyze_topics", "analyze_tfidf",
                   "analyze_readability", "analyze_themes",
                   "analyze_statistics", "analyze_participation_balance",
                   "analyze_response_patterns", "analyze_question_patterns",
                   "analyze_key_phrases")) {
    expect_true("message_ids" %in%
                  names(formals(FocusGroup$public_methods[[method]])))
  }
  expect_true("plot_message_length_timeline" %in%
                names(FocusGroup$public_methods))
})

test_that("model-output constructors require an explicit config", {
  expect_error(create_agents(1), "config")
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  expect_error(create_agents(1.9, config = config), "positive integer")
  expect_error(
    FGAgent$new(
      id = "P1",
      agent_details = list(direct_persona_description = "A participant.")
    ),
    "config"
  )
  expect_error(
    create_agents_from_data(data.frame(age = 30), n_participants = 1),
    "config"
  )
  expect_error(
    run_focus_group(
      topic = "library hours",
      n_participants = 1,
      guide = c(Opening = 1, Closing = 1),
      flow = "round_robin",
      verbose = FALSE
    ),
    "config"
  )

  expect_true(rlang::is_missing(formals(create_agents)$config))
  expect_true(rlang::is_missing(formals(create_agents_from_data)$config))
  expect_true(rlang::is_missing(formals(create_agents_from_survey)$config))
  expect_true(rlang::is_missing(formals(run_focus_group)$config))
})

test_that("renamed R6 state is public and internal counters are demoted", {
  config <- LLMR::llm_config("openai", "gpt-4o-mini")
  agents <- create_agents(1, config = config)
  agent <- agents[["P1"]]
  expect_identical(agent$config, config)

  flow <- create_conversation_flow("round_robin", agents, "MOD")
  fg <- FocusGroup$new(
    topic = "library hours",
    purpose = "Test the guide.",
    agents = agents,
    moderator_id = "MOD",
    turn_taking_flow = flow,
    question_script = list(list(phase = "closing", text = "Close.")),
    admin_config = config
  )
  expect_identical(fg$admin_config, config)
  expect_null(fg$message_mode)
  expect_false("current_phase_index" %in% names(fg))
  expect_false("advance_round" %in% names(fg))
  expect_false("record_token_usage" %in% names(fg))
  expect_identical(fg$agents[["MOD"]]$role, "moderator")

  desire_flow <- create_conversation_flow("desire_based", agents, "MOD")
  expect_false(any(c("last_desire_scores", "last_scoring_mode",
                     "get_last_desire_scores") %in% names(desire_flow)))
  round_robin <- create_conversation_flow("round_robin", agents, "MOD")
  expect_false("current_participant_index" %in% names(round_robin))
  expect_false("agent_ids" %in% names(round_robin))
  probabilistic <- create_conversation_flow("probabilistic", agents, "MOD")
  expect_false(any(c("propensities", "base_propensities",
                     "recovery_increment") %in% names(probabilistic)))

  fg$conversation_log <- list(
    list(message_id = 1L, round = 1L, speaker_id = "P1",
         is_moderator = FALSE, text = "A comment.", phase = "imported",
         metadata = list())
  )
  expect_error(fg$summarize(), "config")
  expect_error(fg$analyze_themes(), "config")
})

test_that("default prompt names are all supported contracts", {
  templates <- get_default_prompt_templates()
  expect_named(templates, c(
    "participant_utterance_subtle_persona",
    "participant_turn_instruction",
    "participant_desire_instruction",
    "participant_desire_to_talk_nuanced",
    "moderator_opening",
    "moderator_icebreaker_question",
    "moderator_engagement_question",
    "moderator_exploration_question",
    "moderator_probing_focused",
    "moderator_summarizing",
    "moderator_transition",
    "moderator_manage_participation",
    "moderator_ending_question",
    "moderator_closing",
    "moderator_generic_utterance",
    "thematic_analysis_prompt"
  ), ignore.order = FALSE)
})
