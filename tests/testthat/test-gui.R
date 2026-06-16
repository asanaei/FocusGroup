# The optional GUI's launcher and continuation logic. The Shiny machinery is a
# Suggests concern; the continuation experiment is live, so its logic is tested
# with a fake echo agent that proves the control and perturbed branches diverge
# without any model call.

fake_fg_agent <- function() {
  env <- new.env()
  env$clone <- function(deep = TRUE) env
  env$generate_utterance <- function(topic, conversation_history_string, ...) {
    list(text = paste0("ECHO> ", conversation_history_string))
  }
  class(env) <- c("R6", "environment")
  env
}

fake_fg <- function() {
  list(topic = "transit", agents = list(P1 = fake_fg_agent()),
       prompt_templates = list(participant_utterance_subtle_persona = "{{conversation_history}}"),
       max_tokens_utterance = 160L)
}

sample_fg_log <- function() {
  list(list(turn = 1, speaker_id = "Moderator", text = "What about transit?"),
       list(turn = 2, speaker_id = "P1", text = "Buses are unreliable."),
       list(turn = 3, speaker_id = "P2", text = "Cost is my issue."))
}

test_that("run_focus_studio errors helpfully when GUI packages are missing", {
  need <- c("shiny", "bslib", "DT", "LLMR.shiny")
  if (all(vapply(need, requireNamespace, logical(1), quietly = TRUE))) {
    expect_true(isTRUE(FocusGroup:::.fg_gui_require()))
  } else {
    expect_error(FocusGroup:::.fg_gui_require(), "GUI needs these packages")
  }
})

test_that("perturbing a turn changes only that turn", {
  log <- sample_fg_log()
  out <- FocusGroup:::.fg_perturb_log(log, 2, "I LOVE THE BUS")
  expect_equal(out[[2]]$text, "I LOVE THE BUS")
  expect_equal(out[[1]]$text, log[[1]]$text)
  expect_equal(out[[3]]$text, log[[3]]$text)
})

test_that("the paired experiment produces diverging control vs perturbed turns", {
  res <- FocusGroup:::.fg_run_experiment(
    fake_fg(), sample_fg_log(), "P1", perturb_turn = 2, perturb_text = "I LOVE THE BUS NOW")
  expect_false(identical(res$control, res$perturbed))
  expect_true(grepl("unreliable", res$control))
  expect_true(grepl("I LOVE THE BUS NOW", res$perturbed))
})

test_that("a CSV transcript coerces to a conversation log", {
  log <- FocusGroup:::.fg_as_log(FocusGroup:::.fg_demo_transcript())
  expect_true(length(log) >= 6)
  expect_true(all(vapply(log, function(e) !is.null(e$text), logical(1))))
})

test_that("the GUI assembles when its suggested packages are present", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("LLMR.shiny")
  expect_s3_class(FocusGroup:::.fg_gui_ui(), "bslib_page")
})

test_that(".fg_display_turns drops the System row and numbers real turns", {
  log <- c(list(list(turn = 0, speaker_id = "System", text = "Participants: P1, P2")),
           sample_fg_log())
  dt <- FocusGroup:::.fg_display_turns(log)
  expect_equal(nrow(dt), 3L)                       # System row dropped
  expect_equal(dt$display_turn, 1:3)               # clean 1..N numbering
  expect_equal(dt$log_index, 2:4)                  # maps back into the full log
  expect_false(any(dt$speaker_id == "System"))
  expect_equal(dt$text[1], "What about transit?")
  # an empty log yields an empty frame, not an error
  expect_equal(nrow(FocusGroup:::.fg_display_turns(list())), 0L)
})

# A minimal fake `shared` context (the shape shell_context returns: reactives
# plus add_usage/can_run). Lets us drive the run module offline.
fake_shared <- function(mode = "live", can_run = TRUE) {
  usage <- new.env(); usage$last <- NULL
  list(
    provider = function() "groq",
    model = function() "openai/gpt-oss-20b",
    mode = function() mode,
    key = function() list(found = can_run, var = "GROQ_API_KEY"),
    can_run = function() can_run,
    add_usage = function(tokens) usage$last <- tokens,
    .usage = usage)
}

test_that(".fg_live_ok requires live mode AND a key (demo never runs)", {
  expect_true(FocusGroup:::.fg_live_ok(fake_shared("live", TRUE)))
  expect_false(FocusGroup:::.fg_live_ok(fake_shared("live", FALSE)))
  expect_false(FocusGroup:::.fg_live_ok(fake_shared("demo", TRUE)))  # demo: no live call
})

test_that("the run module runs via an injected fake and stores the focus group", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("live", TRUE)
  fake_quick <- function(topic, participants, flow, model_config, seed, mode,
                         verbose, max_participant_responses) {
    list(
      focus_group = structure(list(topic = topic), class = "FocusGroup"),
      transcript = data.frame(turn = 1:2, speaker_id = c("MOD", "P1"),
                              text = c("Welcome.", "Hi."), stringsAsFactors = FALSE),
      totals = list(total_turns = 2L, total_tokens_in = 100L, total_tokens_out = 20L))
  }
  shiny::testServer(
    FocusGroup:::.fg_run_server, args = list(shared = shared, run_fun = fake_quick),
    {
      session$setInputs(topic = "public libraries", participants = 3, flow = "round_robin",
                        max_resp = 1, seed = 110)
      session$setInputs(run = 1)
      expect_true(inherits(result()$focus_group, "FocusGroup"))
      # real token totals propagate to usage; calls is the estimate
      expect_equal(shared$.usage$last$sent, 100L)
      expect_equal(shared$.usage$last$received, 20L)
      expect_true(shared$.usage$last$calls >= 1L)
    })
})

test_that("the run module makes NO call in demo mode", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("LLMR.shiny")
  shared <- fake_shared("demo", TRUE)
  called <- new.env(); called$n <- 0L
  fake_quick <- function(...) { called$n <- called$n + 1L; stop("must not be called") }
  shiny::testServer(
    FocusGroup:::.fg_run_server, args = list(shared = shared, run_fun = fake_quick),
    {
      session$setInputs(topic = "x", participants = 3, flow = "round_robin",
                        max_resp = 1, seed = 110, run = 1)
      expect_equal(called$n, 0L)        # demo mode blocked the live run
      expect_null(result())
    })
})
