# The IDs a researcher gives agents stay in the records and out of every
# prompt: the agents see each other under handles. Offline, with scripted
# runners.

.hd_config <- function() LLMR::llm_config("openai", "example-model")

.hd_prompt <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content %||% ""),
               character(1)), collapse = "\n")
}

# Replies long enough that the log outgrows the prompt window, so that an
# interim summary is written; probes answer with a number.
.hd_stub <- function(long = FALSE) {
  env <- new.env()
  env$n <- 0L
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    prompt <- .hd_prompt(experiments$messages[[1]])
    filler <- if (long) paste(rep("Evening hours let working families use the branch.", 260),
                              collapse = " ") else ""
    experiments$response_text <- if (grepl("private question", prompt, fixed = TRUE)) {
      "5"
    } else if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      "6"
    } else if (grepl("Summary:$", prompt)) {
      "A short summary of what was said."
    } else {
      paste(sprintf("Reply %03d: the hours matter for a concrete reason.", env$n), filler)
    }
    experiments
  }
  env
}

.hd_agents <- function(runner, ids = c("teacher_conservative", "V240001"),
                       moderator = "lead_moderator") {
  agents <- lapply(seq_along(ids), function(i) {
    FGAgent$new(ids[[i]], list(direct_persona_description = paste("Persona", i, "of the group.")),
                .hd_config(), .runner = runner)
  })
  names(agents) <- ids
  agents[[moderator]] <- FGAgent$new(moderator, list(direct_persona_description = "A moderator."),
                                      .hd_config(), is_moderator = TRUE, .runner = runner)
  agents
}

.hd_group <- function(agents, script, ...) {
  moderator <- names(agents)[length(agents)]
  FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = moderator,
    turn_taking_flow = create_conversation_flow("desire_based", agents, moderator),
    question_script = script, max_participant_responses = 2, ...)
}

.hd_script <- function(n = 6) {
  c(list(list(phase = "opening", text = "Welcome the group.")),
    lapply(seq_len(n), function(i) list(phase = "engagement_question",
                                        text = sprintf("Question %d about the hours?", i))),
    list(list(phase = "closing", text = "Close the session.")))
}

test_that("no prompt shows the agents' IDs, and the records keep them", {
  stub <- .hd_stub(long = TRUE)
  agents <- .hd_agents(stub$runner)
  fg <- .hd_group(agents, .hd_script())
  fg$run_simulation(probes = fg_probe("From 1 to 7, how much do the hours matter?",
                                      at = list("start", fg_after_item(3), "before_closing", "end")))
  result <- FocusGroup:::.fg_focus_group_result(fg, "desire_based", "roleflip", 2L, NA_integer_)
  calls <- result$calls
  expect_true(all(c("moderator", "participant", "desire", "probe", "interim_summary",
                    "summary") %in% calls$call_type))
  ids <- c("teacher_conservative", "V240001", "lead_moderator")
  # The final summary is written for the researcher and read by no agent,
  # so it shows each ID beside the handle; no other prompt shows an ID.
  for (i in seq_len(nrow(calls))) {
    prompt <- .hd_prompt(calls$messages[[i]])
    if (identical(calls$call_type[[i]], "summary")) {
      expect_match(prompt, "P1 (ID teacher_conservative, Message", fixed = TRUE)
      next
    }
    for (id in ids) {
      expect_false(grepl(id, prompt, fixed = TRUE),
                   info = paste(calls$call_type[[i]], "call", i, "shows", id))
    }
  }
  expect_identical(sum(calls$call_type == "summary"), 1L)
  # The agents see each other as P1, P2, and Moderator.
  participant <- calls[calls$call_type == "participant", ]
  later <- .hd_prompt(participant$messages[[nrow(participant)]])
  expect_match(later, "Moderator: ", fixed = TRUE)
  expect_match(later, "In this discussion you are P[12]\\.")
  for (i in seq_len(nrow(participant))) {
    system <- participant$messages[[i]][[1]]$content
    expected <- result$participants$handle[result$participants$id == participant$agent_id[[i]]]
    expect_match(system, paste0("In this discussion you are ", expected, "."), fixed = TRUE)
  }
  # An agent reads its own earlier turns as its own.
  own <- participant$messages[[nrow(participant)]]
  speaker <- participant$agent_id[[nrow(participant)]]
  said <- result$transcript$text[result$transcript$speaker_id == speaker]
  assistant <- vapply(Filter(function(m) identical(m$role, "assistant"), own),
                      function(m) m$content, character(1))
  expect_true(length(assistant) > 0L)
  expect_true(all(assistant %in% said))

  # The records keep the IDs, beside the handles.
  tr <- result$transcript
  expect_setequal(unique(tr$speaker_id), c("System", ids))
  expect_identical(unique(tr$handle[tr$speaker_id == "teacher_conservative"]), "P1")
  expect_identical(unique(tr$handle[tr$speaker_id == "V240001"]), "P2")
  expect_identical(unique(tr$handle[tr$speaker_id == "lead_moderator"]), "Moderator")
  expect_identical(tr$item[[1]], 0L)
  expect_match(tr$text[[1]], "teacher_conservative", fixed = TRUE)
  expect_identical(result$participants$handle, c("P1", "P2", "Moderator"))
  expect_identical(result$metadata$handles,
                   data.frame(agent_id = ids, handle = c("P1", "P2", "Moderator"),
                              stringsAsFactors = FALSE))
  expect_true(all(result$routing$selected_id %in% ids))
  expect_true(all(vapply(fg$records$probes, `[[`, character(1), "agent_id") %in% ids))
  expect_true(nzchar(fg$current_conversation_summary))
})

test_that("handles = \"ids\" shows the IDs, with a message, and custom handles replace them", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  expect_message(fg <- .hd_group(agents, .hd_script(1), handles = "ids", call_on_silent = "none"),
                 "teacher_conservative, V240001, lead_moderator")
  fg$run_simulation()
  prompts <- vapply(fg$records$calls, function(call) .hd_prompt(call$messages), character(1))
  expect_true(any(grepl("teacher_conservative: ", prompts, fixed = TRUE)))
  expect_identical(unname(fg$handles), names(agents))

  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  fg <- .hd_group(agents, .hd_script(1), handles = c(teacher_conservative = "Ana"),
                  call_on_silent = "none")
  expect_identical(fg$handles, c(teacher_conservative = "Ana", V240001 = "P2",
                                 lead_moderator = "Moderator"))
  fg$run_simulation()
  agent_facing <- Filter(function(call) !identical(call$call_type, "summary"),
                         fg$records$calls)
  prompts <- vapply(agent_facing, function(call) .hd_prompt(call$messages), character(1))
  expect_false(any(grepl("teacher_conservative", prompts, fixed = TRUE)))
  expect_true(any(grepl("In this discussion you are Ana.", prompts, fixed = TRUE)))
  expect_identical(fg$agents$teacher_conservative$handle, "Ana")
})

test_that("handles and agent names are checked before any call", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  build <- function(handles, ...) .hd_group(agents, .hd_script(1), handles = handles, ...)
  expect_error(build(c(teacher_conservative = "Ana", V240001 = "Ana")), "own handle")
  expect_error(build(c(teacher_conservative = "ana", V240001 = "Ana")), "own handle")
  expect_error(build(c(teacher_conservative = "P2")), "own handle")
  expect_error(build(c(teacher_conservative = "System")), "System")
  expect_error(build(c(teacher_conservative = "Participant 3")), "standalone number")
  expect_error(build(c(teacher_conservative = "10")), "standalone number")
  expect_error(build(c(nobody = "Ana")), "not in the roster: nobody")
  expect_error(build(c("Ana")), "named by the agent ID")
  expect_error(build(c(teacher_conservative = " ")), "non-empty")
  expect_error(build(c(teacher_conservative = "Ana", teacher_conservative = "Bo")), "more than once")
  expect_error(build(list(teacher_conservative = "Ana")), "character vector")
  expect_identical(build(c(teacher_conservative = "P3x"))$handles[["teacher_conservative"]], "P3x")

  renamed <- agents
  names(renamed)[1] <- "teacher"
  expect_error(FocusGroup$new("library hours", "Test.", renamed, "lead_moderator",
                              create_conversation_flow("round_robin", agents, "lead_moderator")),
               "named by its agent's `id`")
  expect_error(FocusGroup$new("library hours", "Test.", unname(agents), "lead_moderator",
                              create_conversation_flow("round_robin", agents, "lead_moderator")),
               "named by its agent's `id`")

  # An injected voice may not take a participant's handle; one from the
  # moderator's handle is the moderator's.
  fg <- build(c(teacher_conservative = "Ana"), call_on_silent = "none")
  expect_error(fg$run_simulation(injections = fg_injection("Hello.", fg_after_item(2),
                                                           from = "Ana")),
               "handle")
  fg <- build(NULL, call_on_silent = "none")
  expect_error(fg$run_simulation(injections = fg_injection("Hello.", fg_after_item(2),
                                                           from = "p2")),
               "handle")
  expect_identical(stub$n, 0L)
  fg$run_simulation(injections = fg_injection("A word from me.", fg_after_item(2),
                                              from = "Moderator"))
  injected <- Filter(function(m) isTRUE(m$metadata$injected), fg$conversation_log)[[1]]
  expect_identical(injected$speaker_id, "lead_moderator")
  expect_true(injected$is_moderator)
})

test_that("no neutral handle names another agent's ID, the moderator's included", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner, ids = c("alice", "bob"), moderator = "P1")
  fg <- .hd_group(agents, .hd_script(1), call_on_silent = "none")
  expect_identical(fg$handles, c(alice = "P2", bob = "P3", P1 = "Moderator"))
  fg$run_simulation(final_summary = FALSE)
  view <- fg$.__enclos_env__$private$agent_view()
  expect_identical(view[[1]]$text, "Participants: P2, P3; Moderator: Moderator")
  prompts <- vapply(fg$records$calls, function(call) .hd_prompt(call$messages), character(1))
  expect_false(any(grepl("\\bP1\\b", prompts, perl = TRUE)))

  neutral <- FocusGroup:::.fg_neutral_handles
  # IDs are compared without regard to case, and reserved names are skipped.
  expect_identical(neutral(c("p1", "x", "MOD"), "MOD"),
                   c(p1 = "P2", x = "P3", MOD = "Moderator"))
  expect_identical(neutral(c("a", "b", "MOD"), "MOD", reserved = c("P1", "p3")),
                   c(a = "P2", b = "P4", MOD = "Moderator"))
  # Hard negatives: rosters whose IDs take no number another agent needs
  # keep the handles they had.
  expect_identical(neutral(c("P1", "P2", "P3", "MOD"), "MOD"),
                   c(P1 = "P1", P2 = "P2", P3 = "P3", MOD = "Moderator"))
  expect_identical(neutral(c("teacher_conservative", "V240001", "lead_moderator"),
                           "lead_moderator"),
                   c(teacher_conservative = "P1", V240001 = "P2",
                     lead_moderator = "Moderator"))
  expect_identical(neutral(c("P2", "x", "MOD"), "MOD"),
                   c(P2 = "P2", x = "P1", MOD = "Moderator"))
  # A handle given by name may not be a reserved voice's name.
  resolve <- FocusGroup:::.fg_resolve_handles
  expect_error(resolve(c(a = "Guest"), c("a", "b", "MOD"), "MOD", reserved = "guest"),
               "injected voice: Guest")
  expect_identical(resolve(c(a = "Ana"), c("a", "b", "MOD"), "MOD", reserved = "Guest"),
                   c(a = "Ana", b = "P2", MOD = "Moderator"))
})

test_that("in flat mode each participant is told its handle, and the moderator is not", {
  withr::local_options(list(focusgroup.message_mode = "flat"))
  stub <- .hd_stub()
  fg <- .hd_group(.hd_agents(stub$runner), .hd_script(1), call_on_silent = "none")
  fg$run_simulation(final_summary = FALSE)
  calls <- fg$records$calls
  types <- vapply(calls, `[[`, character(1), "call_type")
  expect_true(all(c("moderator", "participant", "desire") %in% types))
  for (call in calls) {
    first <- call$messages[[1]]$content
    if (identical(call$call_type, "moderator")) {
      expect_false(grepl("In this discussion you are", first, fixed = TRUE))
      next
    }
    # One user message that opens with the agent's own handle.
    expect_length(call$messages, 1L)
    expect_identical(call$messages[[1]]$role, "user")
    expect_true(startsWith(first, paste0("In this discussion you are ",
                                         fg$handles[[call$agent_id]], ".\n\n")),
                info = call$agent_id)
  }
  participant <- Filter(function(call) identical(call$call_type, "participant"), calls)
  text <- participant[[1]]$messages[[1]]$content
  expect_match(text, "address other participants by their labels", fixed = TRUE)
  expect_false(grepl("by ID", text, fixed = TRUE))

  # The broadcast desire path sends the same flat message.
  seen <- NULL
  testthat::local_mocked_bindings(
    call_llm_broadcast = function(config, messages, ...) {
      seen <<- c(seen, messages)
      data.frame(response_text = rep("6", length(messages)), success = TRUE)
    }, .package = "LLMR")
  live <- .hd_agents(NULL)
  flow <- create_conversation_flow("desire_based", live, "lead_moderator")
  group <- .hd_group(live, .hd_script(1))
  group$turn_taking_flow <- flow
  group$.__enclos_env__$private$claim_handles()
  flow$select_next_speaker(group)
  expect_length(seen, 2L)
  firsts <- vapply(seen, function(m) m[[1]]$content, character(1))
  expect_true(startsWith(firsts[[1]], "In this discussion you are P1.\n\n"))
  expect_true(startsWith(firsts[[2]], "In this discussion you are P2.\n\n"))

  # Hard negative: in role-flip mode the handle is in the system message,
  # once, and not added to the instruction.
  withr::local_options(list(focusgroup.message_mode = "roleflip"))
  stub <- .hd_stub()
  fg <- .hd_group(.hd_agents(stub$runner), .hd_script(1), call_on_silent = "none")
  fg$run_simulation(final_summary = FALSE)
  for (call in Filter(function(call) identical(call$call_type, "participant"),
                      fg$records$calls)) {
    contents <- vapply(call$messages, function(m) as.character(m$content), character(1))
    expect_identical(sum(grepl("In this discussion you are", contents, fixed = TRUE)), 1L)
    expect_identical(call$messages[[1]]$role, "system")
  }
})

test_that("an agent's own label is its handle, so another speaker's handle is kept", {
  runner <- function(experiments, ...) {
    messages <- experiments$messages[[1]]
    prompt <- .hd_prompt(messages)
    you <- sub(".*In this discussion you are ([^.]+)\\..*", "\\1", prompt)
    experiments$response_text <- if (grepl("Summary:$", prompt)) {
      "A short summary."
    } else if (grepl("MODERATOR", prompt, fixed = TRUE)) {
      "**MOD:** Which hours matter most to you?"
    } else if (identical(you, "P3")) {
      "P2: you make a fair point about the evening hours."
    } else {
      sprintf("%s: evening hours help me after my shift ends.", you)
    }
    experiments
  }
  agents <- create_agents(3, .hd_config(), direct_persona_descriptions = c("A.", "B.", "C."),
                          .runner = runner)
  fg <- FocusGroup$new(
    topic = "library hours", purpose = "Test.", agents = agents, moderator_id = "MOD",
    turn_taking_flow = create_conversation_flow("round_robin", agents, "MOD"),
    question_script = list(list(phase = "engagement_question", text = "Which hours?")),
    max_participant_responses = 3, call_on_silent = "none",
    handles = c(P1 = "P2", P2 = "P3", P3 = "P1"))
  fg$run_simulation(final_summary = FALSE)
  log <- fg$conversation_log
  said <- function(id) Filter(function(m) identical(m$speaker_id, id), log)[[1]]
  # Agent P2, shown as P3, addresses the agent shown as P2: the label stays.
  expect_identical(said("P2")$text, "P2: you make a fair point about the evening hours.")
  expect_null(said("P2")$metadata$self_label_removed)
  # Agents P1 and P3 wrote their own handles, P2 and P1, which are removed.
  expect_identical(said("P1")$text, "evening hours help me after my shift ends.")
  expect_true(said("P1")$metadata$self_label_removed)
  expect_identical(said("P3")$text, "evening hours help me after my shift ends.")
  expect_identical(said("MOD")$text, "Which hours matter most to you?")

  labels <- FocusGroup:::.fg_self_labels
  expect_identical(labels(fg$agents$P2), "P3")
  expect_identical(labels(fg$agents$MOD), c("Moderator", "MOD"))
  # An agent outside a session answers to its ID.
  loose <- FGAgent$new("teacher", list(direct_persona_description = "A."), .hd_config())
  expect_identical(labels(loose), "teacher")
})

test_that("identifier columns stay out of persona text and are reported", {
  cfg <- .hd_config()
  data <- data.frame(id = c("R-101", "R-102"), respondent_id = c(9001L, 9002L),
                     age = c("30", "41"), answer = c("Yes", "No"))
  agents <- create_agents_from_data(data, 2, cfg, demographic_cols = "age")
  personas <- vapply(agents[1:2], function(a) a$persona_description, character(1))
  expect_false(any(grepl("R-10|900[12]", personas)))
  expect_true(all(grepl("age: ", personas, fixed = TRUE)))
  expect_true(all(grepl("Question: answer", personas, fixed = TRUE)))
  report <- persona_rendering_report(agents)
  ids <- report[report$variable %in% c("id", "respondent_id"), ]
  expect_identical(nrow(ids), 4L)
  expect_true(all(ids$status == "dropped"))
  expect_true(all(ids$reason == "identifier column"))
  expect_setequal(ids$code, c("R-101", "R-102", "9001", "9002"))
  expect_identical(sort(as.character(attr(agents, "source_ids")$id)), c("R-101", "R-102"))

  # The same for a data frame given to create_agents().
  agents <- create_agents(2, cfg, demographics = data.frame(persona_id = c("x7", "y8"),
                                                            age = c(30, 41)))
  personas <- vapply(agents[1:2], function(a) a$persona_description, character(1))
  expect_false(any(grepl("x7|y8|persona_id", personas)))
  report <- persona_rendering_report(agents)
  expect_identical(unique(report$reason[report$variable == "persona_id"]), "identifier column")
  expect_identical(agents$P1$demographics$persona_id, "x7")
})

test_that("an automatically chosen survey variable that differs in every row is left out", {
  lab <- function(x, labels = NULL, label) {
    x <- if (is.null(labels)) x else haven::labelled(x, labels)
    attr(x, "label") <- label
    x
  }
  d <- data.frame(
    AGE = lab(c(1L, 2L, 3L, 2L), c("18-29" = 1, "30-49" = 2, "50+" = 3), "Age group"),
    CASEID = lab(c(240001, 240002, 240003, 240004), label = "Case ID"),
    PARTY = lab(c(1L, 2L, 1L, 2L), c("Democrat" = 1, "Republican" = 2), "Party"),
    RANK = lab(c(1L, 2L, 3L, 4L), c("One" = 1, "Two" = 2, "Three" = 3, "Four" = 4), "Rank"),
    INCOME = lab(c(50000, 62000, 50000, 71000), label = "Income in dollars"))
  f <- tempfile(fileext = ".dta"); on.exit(unlink(f), add = TRUE)
  haven::write_dta(d, f)
  cfg <- .hd_config()
  msgs <- testthat::capture_messages(suppressWarnings(
    agents <- create_agents_from_survey(4, f, cfg, demographic_vars = c(age = "AGE"))))
  expect_true(any(grepl("respondent identifiers.*CASEID", msgs)))
  personas <- vapply(agents[1:4], function(a) a$persona_description, character(1))
  expect_false(any(grepl("24000[1-4]|Case ID", personas)))
  # A labeled item and an unlabeled number with a repeated value are kept.
  expect_true(all(grepl("Rank", personas, fixed = TRUE)))
  expect_true(all(grepl("Income in dollars", personas, fixed = TRUE)))
  # Hard negatives: in a small file, distinct ages and distinct written
  # answers are content, not identifiers; a large file's case numbers are.
  distinct <- FocusGroup:::.fg_distinct_in_every_row
  expect_false(distinct(c(23, 31, 45, 52, 67, 70), "AGE"))
  expect_false(distinct(c("Too few buses", "Rent", "Schools are full"), "MIP"))
  expect_false(distinct(c(1.5, 2.25, 3.75), "SCORE"))
  expect_false(distinct(c(101, 102, 102), "CASEID"))
  expect_false(distinct(c(101, 102, NA), "CASEID"))
  expect_true(distinct(c(101, 102, 103), "CASEID"))
  expect_false(distinct(c("R-1", "R-2"), "V1"))
  expect_true(distinct(structure(c(7, 8, 9), label = "Respondent number"), "V9"))
  expect_true(distinct(240001:240040, "V240001"))
  expect_false(distinct(c(30:59, 59), "V9"))
  report <- persona_rendering_report(agents)
  expect_true(all(report$reason[report$variable == "CASEID"] == "identifier column"))
  # Named in `survey_vars`, the variable is rendered.
  suppressWarnings(named <- create_agents_from_survey(
    4, f, cfg, demographic_vars = c(age = "AGE"), survey_vars = c("CASEID", "PARTY")))
  expect_true(any(grepl("24000", vapply(named[1:4], function(a) a$persona_description, ""))))
})

test_that("an imported transcript keeps its speakers' names and numbers its items", {
  data <- data.frame(
    speaker = c("Ana", "Moderator", "Ana", "Speaker 1", "Moderator", "Speaker 1"),
    text = c("Before we start.", "Welcome. What matters?", "Evenings.", "Mornings.",
             "Ana, say more?", "Weekends too."))
  expect_no_message(fg <- focus_group_from_transcript(data))
  expect_identical(unname(fg$handles), names(fg$agents))
  log <- fg$conversation_log
  expect_identical(vapply(log, `[[`, integer(1), "item"), c(0L, 1L, 1L, 1L, 2L, 2L))
  expect_identical(vapply(log, `[[`, character(1), "handle"), data$speaker)
  view <- fg$.__enclos_env__$private$agent_view()
  expect_identical(vapply(view, `[[`, character(1), "speaker_id"), data$speaker)
  tr <- FocusGroup:::.fg_transcript_data(log)
  expect_identical(tr$item, c(0L, 1L, 1L, 1L, 2L, 2L))
  expect_identical(tr$handle, data$speaker)

  # A supplied item column is kept; message ids continue after the largest.
  data$item <- c(0L, 1L, 1L, 1L, 3L, 3L)
  data$message_id <- c(1L, 2L, 5L, 9L, 10L, 14L)
  fg <- focus_group_from_transcript(data)
  expect_identical(vapply(fg$conversation_log, `[[`, integer(1), "item"), data$item)
  next_id <- fg$.__enclos_env__$private$log_message("Ana", "One more.", round = 2L,
                                                    is_moderator = FALSE)
  expect_identical(next_id, 15L)
})

test_that("summarize() labels by handle, leaves out the roster, and calls live unless given a runner", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  fg <- .hd_group(agents, .hd_script(1), call_on_silent = "none")
  fg$run_simulation()
  seen <- NULL
  recorder <- function(experiments, ...) {
    seen <<- .hd_prompt(experiments$messages[[1]])
    experiments$response_text <- "A summary."
    experiments
  }
  fg$summarize(.hd_config(), .runner = recorder)
  expect_false(grepl("System", seen, fixed = TRUE))
  expect_match(seen, "P1 (ID teacher_conservative, Message", fixed = TRUE)
  expect_match(seen, "Moderator (ID lead_moderator, Message", fixed = TRUE)
  # Where the handle is the ID, the ID is not repeated.
  ids_fg <- suppressMessages(.hd_group(.hd_agents(stub$runner), .hd_script(1),
                                       handles = "ids", call_on_silent = "none"))
  ids_fg$run_simulation()
  ids_fg$summarize(.hd_config(), .runner = recorder)
  expect_match(seen, "teacher_conservative (Message", fixed = TRUE)
  expect_false(grepl("(ID ", seen, fixed = TRUE))

  # Without a runner the call goes to the provider, not to the session's
  # scripted runner.
  testthat::local_mocked_bindings(
    call_llm_robust = function(...) stop("A live call was attempted."), .package = "LLMR")
  before <- stub$n
  expect_error(fg$summarize(.hd_config()), "live call was attempted")
  expect_identical(stub$n, before)

  empty <- .hd_group(.hd_agents(stub$runner), .hd_script(1))
  expect_error(empty$summarize(.hd_config(), .runner = recorder), "no messages to summarize")
  expect_error(fg$summarize(.hd_config(), transcript_override = "  ", .runner = recorder),
               "empty")
})

test_that("an agent shared by two sessions answers to its handle in the one that runs", {
  stub <- .hd_stub()
  agents <- create_agents(3, .hd_config(), direct_persona_descriptions = c("A.", "B.", "C."),
                          .runner = stub$runner)
  script <- list(list(phase = "engagement_question", text = "Which hours?"))
  fg1 <- FocusGroup$new("library hours", "Test.", agents, "MOD",
                        create_conversation_flow("round_robin", agents, "MOD"),
                        question_script = script, max_participant_responses = 3,
                        call_on_silent = "none")
  # A second session over two of the same agents gives P3 another handle.
  fg2 <- FocusGroup$new("library hours", "Test.", agents[c("P3", "MOD")], "MOD",
                        create_conversation_flow("round_robin", agents[c("P3", "MOD")], "MOD"),
                        question_script = script, handles = c(P3 = "Ana"),
                        call_on_silent = "none")
  expect_identical(agents$P3$handle, "Ana")
  fg1$run_simulation()
  expect_identical(agents$P3$handle, "P3")
  calls <- fg1$records$calls
  p3 <- Filter(function(call) identical(call$agent_id, "P3") &&
                 identical(call$call_type, "participant"), calls)[[1]]
  expect_match(p3$messages[[1]]$content, "In this discussion you are P3.", fixed = TRUE)
})

test_that("participants whose IDs already read as handles keep them", {
  neutral <- FocusGroup:::.fg_neutral_handles
  expect_identical(neutral(c("MOD", "P2", "P4", "P6"), "MOD"),
                   c(MOD = "Moderator", P2 = "P2", P4 = "P4", P6 = "P6"))
  expect_identical(neutral(c("P2", "P1", "MOD"), "MOD"),
                   c(P2 = "P2", P1 = "P1", MOD = "Moderator"))
  expect_identical(neutral(c("P2", "teacher", "nurse", "MOD"), "MOD"),
                   c(P2 = "P2", teacher = "P1", nurse = "P3", MOD = "Moderator"))
  # A respondent number is not kept.
  expect_identical(neutral(c("P240001", "P240002", "MOD"), "MOD"),
                   c(P240001 = "P1", P240002 = "P2", MOD = "Moderator"))
  expect_identical(neutral(c("P0", "p3", "MOD"), "MOD"),
                   c(P0 = "P1", p3 = "P2", MOD = "Moderator"))
})

test_that("an agent saved by an earlier version keeps its ID as its handle, with a warning", {
  stub <- .hd_stub()
  agents <- .hd_agents(stub$runner)
  Old <- R6::R6Class("FGAgent", public = list(
    id = "old_timer", persona_description = "Persona.", config = NULL, .runner = NULL,
    is_moderator = FALSE, role = "participant", demographics = list(),
    survey_responses = list(), history = list()))
  agents$old_timer <- Old$new()
  agents <- agents[c("teacher_conservative", "old_timer", "V240001", "lead_moderator")]
  expect_warning(fg <- .hd_group(agents, .hd_script(1)), "old_timer.*under their IDs")
  expect_identical(fg$handles[["old_timer"]], "old_timer")
  expect_identical(fg$handles[["V240001"]], "P3")

  # Its ID may not clash with a handle another agent has.
  clash <- agents
  clash$old_timer$id <- "P3"
  names(clash)[2] <- "P3"
  expect_error(.hd_group(clash, .hd_script(1), handles = c(V240001 = "P3")),
               "own handle|would be shared")
})
