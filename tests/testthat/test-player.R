# fg_player(): the HTML replay of a finished session, built from its records
# alone. Offline: the sessions come from stub runners and the bundled fixture.

.player_fixture <- function() {
  readRDS(system.file("extdata", "player-session.rds", package = "FocusGroup"))
}

.player_people <- function(n) {
  data.frame(
    age = 30 + seq_len(n),
    occupation = c("teacher", "nurse", "baker", "student", "driver", "clerk",
                   "welder", "pharmacist", "farmer", "pilot", "editor")[seq_len(n)],
    stringsAsFactors = FALSE)
}

# A runner that numbers its replies and cycles through desire scores with
# ties among them.
.player_stub <- function(reply = NULL) {
  env <- new.env()
  env$n <- 0L
  env$runner <- function(experiments, ...) {
    env$n <- env$n + 1L
    prompt <- paste(vapply(experiments$messages[[1]], function(m) {
      as.character(m$content %||% "")
    }, character(1)), collapse = "\n")
    experiments$response_text <- if (grepl("private question", prompt, fixed = TRUE)) {
      "5. A private answer."
    } else if (grepl("Desire to talk score", prompt, fixed = TRUE)) {
      c("6", "4", "6", "3", "7")[(env$n %% 5L) + 1L]
    } else if (grepl("Summary:$", prompt)) {
      "A short summary of the session."
    } else if (!is.null(reply)) {
      reply(env$n)
    } else {
      sprintf("Reply %03d: evening hours matter to working families.", env$n)
    }
    experiments
  }
  env
}

.player_config <- function() LLMR::llm_config("openai", "example-model")

.player_guide <- list(Engagement = c("Which hours matter?", "Who is left out?"),
                      Closing = "Close the session.")

.player_run <- function(flow = "desire_based", n = 3, stub = .player_stub(), ...) {
  run_focus_group("library hours", config = .player_config(), n_participants = n,
                  guide = .player_guide, demographics = .player_people(n),
                  flow = flow, max_participant_responses = 2, seed = 110,
                  verbose = FALSE, .runner = stub$runner, ...)
}

.player_read <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

# The data block of a page, as written and as parsed.
.player_json <- function(html) {
  m <- regmatches(html, regexec(
    '<script id="fg-data" type="application/json">(.*?)</script>', html))[[1]]
  expect_length(m, 2L)
  m[[2]]
}
.player_payload <- function(html) jsonlite::fromJSON(.player_json(html), simplifyVector = FALSE)

.player_build <- function(x, ...) {
  path <- fg_player(x, file = tempfile(fileext = ".html"), open = FALSE, ...)
  html <- .player_read(path)
  expect_false(grepl("http://", html, fixed = TRUE))
  expect_false(grepl("https://", html, fixed = TRUE))
  list(path = path, html = html, payload = .player_payload(html))
}

.player_kinds <- function(payload) vapply(payload$steps, `[[`, character(1), "kind")
.player_message_kinds <- c("system", "moderator", "injected", "turn")

.player_probe_groups <- function(result) {
  length(unique(paste(result$probes$probe, result$probes$point,
                      result$probes$after_message_id)))
}

# Pages, log entries, selection panels, and probe panels, in the counts the
# records give.
.player_check_steps <- function(result, payload) {
  kinds <- .player_kinds(payload)
  people <- sum(!result$participants$is_moderator)
  panels <- sum(!result$routing$rule %in% "rotation" & !is.na(result$routing$message_id))
  expect_identical(kinds[[1]], "title")
  expect_identical(kinds[seq_len(people) + 1L], rep("persona", people))
  expect_identical(kinds[[length(kinds)]], "end")
  expect_identical(sum(kinds %in% .player_message_kinds), nrow(result$transcript))
  expect_identical(sum(kinds == "routing"), panels)
  expect_identical(sum(kinds == "probe"), .player_probe_groups(result))
  expect_identical(length(kinds), 1L + people + nrow(result$transcript) + panels +
                     .player_probe_groups(result) + 1L)

  messages <- payload$steps[kinds %in% .player_message_kinds]
  expect_identical(vapply(messages, function(s) as.integer(s$message_id), integer(1)),
                   result$transcript$message_id)
  expect_identical(vapply(messages, `[[`, character(1), "text"), result$transcript$text)

  # Each panel comes just before the turn it chose; each probe panel just
  # after the message it followed.
  for (j in which(kinds == "routing")) {
    expect_identical(kinds[[j + 1L]], "turn")
    expect_identical(payload$steps[[j + 1L]]$message_id, payload$steps[[j]]$message_id)
    expect_identical(payload$steps[[j + 1L]]$speaker, payload$steps[[j]]$chosen)
  }
  for (j in which(kinds == "probe")) {
    before <- max(which(kinds[seq_len(j)] %in% .player_message_kinds))
    expect_identical(payload$steps[[before]]$message_id, payload$steps[[j]]$after_message_id)
  }
  invisible(kinds)
}

# The scores in the page equal the routing record, and the participant chosen
# holds the top score or won a recorded tie draw.
.player_check_scores <- function(result, payload) {
  panels <- Filter(function(s) identical(s$kind, "routing"), payload$steps)
  rows <- result$routing[!result$routing$rule %in% "rotation", , drop = FALSE]
  expect_identical(vapply(panels, function(s) as.integer(s$pick), integer(1)), rows$pick)
  desire_rules <- c("highest_score", "threshold_lowered", "highest_below_threshold",
                    "uniform_after_failure")
  for (k in seq_along(panels)) {
    s <- panels[[k]]
    chosen <- rows$selected_id[[k]]
    recorded <- rows$scores[[k]]
    expect_identical(s$chosen, chosen)
    expect_identical(s$rule, rows$rule[[k]])
    scored <- Filter(function(e) e$status %in% c("asked", "failed"), s$entries)
    got <- vapply(scored, function(e) if (is.null(e$value)) NA_real_ else as.numeric(e$value),
                  numeric(1))
    names(got) <- vapply(scored, `[[`, character(1), "id")
    expect_setequal(names(got), names(recorded) %||% character())
    if (length(recorded)) expect_equal(got[names(recorded)], recorded, tolerance = 1e-12)
    if (rows$rule[[k]] %in% desire_rules) {
      expect_identical(s$scale, "desire")
      valid <- recorded[!is.na(recorded)]
      if (length(valid)) {
        expect_identical(unname(recorded[[chosen]]), max(valid))
      }
      if (sum(valid == max(valid)) > 1L || !length(valid)) {
        expect_true(s$tie || identical(rows$rule[[k]], "uniform_after_failure"))
        expect_identical(rows$tie_break[[k]], "random")
      }
      if (isTRUE(s$tie)) {
        expect_true(chosen %in% unlist(s$tied))
        expect_setequal(unlist(s$tied), rows$tied[[k]])
      }
    } else if (identical(rows$rule[[k]], "propensity_draw")) {
      expect_identical(s$scale, "chance")
      expect_true(chosen %in% rows$candidates[[k]])
      expect_equal(sum(got), 1, tolerance = 1e-12)
    } else if (rows$rule[[k]] %in% c("called_on", "addressed")) {
      expect_identical(s$scale, "none")
      expect_length(s$entries, 0L)
    }
  }
  invisible(panels)
}

test_that("the fixture page shows every log entry once, in order, with its panels", {
  x <- .player_fixture()
  page <- .player_build(x)
  .player_check_steps(x, page$payload)
  .player_check_scores(x, page$payload)
  expect_identical(page$payload$title, x$metadata$topic)
  expect_true("Replayed from the saved record; no model is called." %in%
                unlist(page$payload$steps[[1]]$lines))

  # Opening pages: one per participant, with the persona as sent.
  personas <- Filter(function(s) identical(s$kind, "persona"), page$payload$steps)
  people <- x$participants[!x$participants$is_moderator, ]
  expect_identical(vapply(personas, `[[`, character(1), "agent"), people$id)
  expect_identical(vapply(page$payload$agents, `[[`, character(1), "persona"), people$persona)

  # The end page counts every participant's turns, zeros included.
  end <- page$payload$steps[[length(page$payload$steps)]]
  turns <- x$transcript$speaker_id[!x$transcript$is_moderator &
                                     x$transcript$speaker_id %in% people$id]
  expect_identical(vapply(end$counts, function(c) as.integer(c$turns), integer(1)),
                   vapply(people$id, function(id) sum(turns == id), integer(1),
                          USE.NAMES = FALSE))
  expect_identical(end$summary, x$summary)
  expect_match(end$summary_label, "written by the model after the session")
})

test_that("call-ons, addressed turns, injections, and probes are marked as the records say", {
  x <- .player_fixture()
  steps <- .player_build(x)$payload$steps
  kinds <- vapply(steps, `[[`, character(1), "kind")

  call_on <- Filter(function(s) identical(s$kind, "moderator") && !is.null(s$tag) &&
                      startsWith(s$tag, "calls on"), steps)
  expected <- Filter(Negate(is.null), lapply(x$transcript$metadata, `[[`, "call_on"))
  expect_identical(length(call_on), length(expected))
  expect_identical(vapply(call_on, `[[`, character(1), "tag"),
                   paste("calls on", vapply(expected, `[[`, character(1), "participant")))

  routed <- Filter(function(s) identical(s$kind, "routing"), steps)
  rules <- vapply(routed, `[[`, character(1), "rule")
  expect_true(all(c("called_on", "addressed") %in% rules))
  for (s in routed[rules == "called_on"]) {
    expect_identical(s$title, "Called on by the moderator")
    trigger <- x$routing$trigger_message_id[x$routing$pick == s$pick]
    expect_identical(s$trigger_message_id, trigger)
    expect_match(s$note, paste0("message #", trigger), fixed = TRUE)
  }
  expect_identical(routed[[which(rules == "addressed")]]$title, "Addressed by the moderator")
  turn_tags <- unlist(lapply(Filter(function(s) identical(s$kind, "turn"), steps), `[[`, "tag"))
  expect_identical(sum(turn_tags == "called on"), sum(rules == "called_on"))
  expect_identical(sum(turn_tags == "addressed"), sum(rules == "addressed"))

  injected <- steps[kinds == "injected"]
  expect_length(injected, nrow(x$injections))
  expect_identical(injected[[1]]$source, x$injections$from[[1]])
  expect_identical(injected[[1]]$point, x$injections$point[[1]])
  expect_identical(injected[[1]]$text, x$injections$text[[1]])

  probes <- steps[kinds == "probe"]
  expect_length(probes, .player_probe_groups(x))
  answers <- unlist(lapply(probes, function(p) lapply(p$answers, `[[`, "answer")))
  expect_identical(answers, x$probes$answer)
})

test_that("a reply holding a script end tag stays text and the page still builds", {
  hostile <- "Fine.</script><script>alert(1)</script> <!-- <script> see https://example.org"
  result <- .player_run("round_robin", stub = .player_stub(function(n) {
    if (n == 2L) hostile else sprintf("Reply %03d: evening hours matter.", n)
  }))
  expect_true(hostile %in% result$transcript$text)
  page <- .player_build(result)
  json <- .player_json(page$html)
  expect_false(grepl("</", json, fixed = TRUE))
  expect_false(grepl("<!--", json, fixed = TRUE))
  expect_false(grepl("://", json, fixed = TRUE))
  # Outside the data block the page has its two script elements and no more.
  outside <- sub(json, "", page$html, fixed = TRUE)
  expect_identical(lengths(regmatches(outside, gregexpr("<script", outside, fixed = TRUE))), 2L)
  expect_identical(lengths(regmatches(page$html, gregexpr("</script>", page$html, fixed = TRUE))), 2L)
  texts <- vapply(Filter(function(s) s$kind %in% .player_message_kinds, page$payload$steps),
                  `[[`, character(1), "text")
  expect_true(hostile %in% texts)
  # The template never builds markup from data.
  template <- .player_read(system.file("player", "template.html", package = "FocusGroup"))
  expect_false(grepl("innerHTML|insertAdjacentHTML|document\\.write|eval\\(", template))
})

test_that("round robin, probabilistic, and desire-based sessions build", {
  rr <- .player_run("round_robin")
  page <- .player_build(rr)
  kinds <- .player_check_steps(rr, page$payload)
  expect_identical(sum(kinds == "routing"), sum(rr$routing$rule != "rotation"))
  expect_true(any(rr$routing$rule == "rotation"))
  expect_match(page$payload$steps[[1]]$lines[[3]], "^Round robin")

  prob <- .player_run("probabilistic")
  page <- .player_build(prob)
  .player_check_steps(prob, page$payload)
  panels <- .player_check_scores(prob, page$payload)
  expect_true(length(panels) > 0L)
  expect_identical(unique(vapply(panels, `[[`, character(1), "scale")), "chance")

  desire <- .player_run("desire_based")
  page <- .player_build(desire)
  .player_check_steps(desire, page$payload)
  panels <- .player_check_scores(desire, page$payload)
  scored <- Filter(function(s) identical(s$scale, "desire"), panels)
  expect_identical(length(scored), sum(desire$routing$rule %in% c(
    "highest_score", "threshold_lowered", "highest_below_threshold", "uniform_after_failure")))
  expect_true(length(scored) > 0L)
  # The previous speaker is marked as not asked.
  skipped <- unlist(lapply(scored, function(s) {
    vapply(Filter(function(e) identical(e$status, "not_asked"), s$entries), `[[`,
           character(1), "reason")
  }))
  expect_true(length(skipped) > 0L)
  expect_true(all(skipped == "spoke last; not asked"))
})

test_that("a session with probes and an injection places them where they happened", {
  result <- .player_run(
    "desire_based",
    injections = fg_injection("A reasoned objection to longer hours.", at = fg_after_item(1),
                              from = "Guest expert", visibility = "participants"),
    probes = fg_probe("From 1 to 7, how much do you favor longer hours?",
                      at = list("start", fg_after_item(1), "end")))
  page <- .player_build(result)
  .player_check_steps(result, page$payload)
  .player_check_scores(result, page$payload)
  steps <- page$payload$steps
  kinds <- .player_kinds(page$payload)
  expect_identical(sum(kinds == "probe"), 3L)
  injected <- steps[[which(kinds == "injected")]]
  expect_identical(injected$source, "Guest expert")
  expect_identical(injected$visibility, "participants")
  # The probe at fg_after_item(1) comes before the injection at the same point.
  probe_at <- which(kinds == "probe" &
                      vapply(steps, function(s) identical(s$point, "fg_after_item(1)"), logical(1)))
  expect_lt(probe_at, which(kinds == "injected"))
  expect_true(grepl("seen by no agent", .player_read(page$path), fixed = TRUE))
})

test_that("run_focus_group_arms() output gives one file per arm", {
  stub <- .player_stub()
  out <- run_focus_group_arms(
    "library hours", config = .player_config(), n_participants = 3,
    arms = list(objection = fg_injection("Longer hours would cut two programs.",
                                         at = fg_after_item(2), from = "Guest expert"),
                control = NULL),
    guide = .player_guide, demographics = .player_people(3), flow = "desire_based",
    probes = fg_probe("From 1 to 7?", at = "end"), max_participant_responses = 2,
    seed = 110, verbose = FALSE, .runner = stub$runner)
  dir <- tempfile("player_arms_")
  paths <- fg_player(out, file = dir, open = FALSE)
  expect_identical(names(paths), c("objection", "control"))
  expect_identical(unname(paths), file.path(dir, c("objection.html", "control.html")))
  expect_true(all(file.exists(paths)))

  payloads <- lapply(paths, function(p) .player_payload(.player_read(p)))
  for (arm in names(out$arms)) {
    .player_check_steps(out$arms[[arm]], payloads[[arm]])
    .player_check_scores(out$arms[[arm]], payloads[[arm]])
    expect_match(payloads[[arm]]$subtitle, paste0("arm ", arm, "$"))
  }
  # The shared messages are the same in both files.
  shared <- function(p) {
    m <- Filter(function(s) s$kind %in% .player_message_kinds, p$steps)
    m[seq_len(out$metadata$prefix_messages)]
  }
  expect_identical(shared(payloads$objection), shared(payloads$control))

  one <- fg_player(out, file = tempfile(fileext = ".html"), arm = "control", open = FALSE)
  expect_length(one, 1L)
  expect_identical(.player_payload(.player_read(one)), payloads$control)
  expect_error(fg_player(out, arm = "treatment", open = FALSE), "No arm named 'treatment'")
  expect_error(fg_player(out, file = tempfile(fileext = ".html"), open = FALSE),
               "names a directory")
  expect_error(fg_player(out$arms$control, arm = "objection", open = FALSE), "one session")
})

test_that("a FocusGroup after run_simulation() and run_arms() output build", {
  stub <- .player_stub()
  make_group <- function() {
    agents <- create_agents(3, config = .player_config(), demographics = .player_people(3),
                            .runner = stub$runner)
    FocusGroup$new(
      topic = "library hours", purpose = "Test the player.", agents = agents,
      moderator_id = "MOD",
      turn_taking_flow = create_conversation_flow("desire_based", agents, "MOD"),
      question_script = list(list(phase = "engagement_question", text = "Q1?"),
                             list(phase = "engagement_question", text = "Q2?"),
                             list(phase = "closing", text = "Close.")),
      max_participant_responses = 2)
  }
  fg <- make_group()
  expect_error(fg_player(fg, open = FALSE), "run_simulation")
  withr::with_seed(110, fg$run_simulation())
  page <- .player_build(fg)
  result <- .fg_player_from_object(fg)
  .player_check_steps(result, page$payload)
  .player_check_scores(result, page$payload)
  expect_match(page$payload$subtitle, "^Desire-based selection")

  branched <- make_group()
  runs <- withr::with_seed(110, branched$run_arms(list(
    a = fg_injection("A comment from outside.", fg_after_item(2), from = "Guest expert"),
    b = NULL)))
  dir <- tempfile("player_run_arms_")
  paths <- fg_player(runs, file = dir, open = FALSE)
  expect_identical(names(paths), c("a", "b"))
  expect_true(all(file.exists(paths)))
  for (arm in c("a", "b")) {
    p <- .player_payload(.player_read(paths[[arm]]))
    r <- .fg_player_from_object(runs$arms[[arm]], arm = arm)
    .player_check_steps(r, p)
    .player_check_scores(r, p)
    expect_true(any(grepl(paste0("Arm \"", arm, "\""), unlist(p$steps[[1]]$lines), fixed = TRUE)))
  }
  # The list of branches alone is accepted too.
  expect_identical(names(fg_player(runs$arms, file = tempfile("player_branches_"),
                                   open = FALSE)), c("a", "b"))
})

test_that("handles differing from IDs are shown beside them, and 0.5.4 records fall back", {
  x <- .player_fixture()
  x$participants$handle[x$participants$id == "P1"] <- "Ana"
  x$transcript$handle[x$transcript$speaker_id == "P1"] <- "Ana"
  steps <- .player_build(x)$payload
  agent <- Filter(function(a) identical(a$id, "P1"), steps$agents)[[1]]
  expect_identical(agent$handle, "Ana")
  p1_turns <- Filter(function(s) identical(s$kind, "turn") && identical(s$speaker, "P1"),
                     steps$steps)
  expect_true(length(p1_turns) > 0L)
  expect_true(all(vapply(p1_turns, `[[`, character(1), "handle") == "Ana"))
  notes <- unlist(lapply(Filter(function(s) identical(s$kind, "routing") &&
                                  identical(s$chosen, "P1"), steps$steps), `[[`, "note"))
  expect_true(all(grepl("Ana", notes, fixed = TRUE)))

  old <- .player_fixture()
  old$transcript$handle <- NULL
  old$transcript$item <- NULL
  old$routing$trigger_message_id <- NULL
  old$participants$handle <- NULL
  old$metadata$handles <- NULL
  page <- .player_build(old)
  .player_check_steps(old, page$payload)
  expect_identical(vapply(page$payload$agents, `[[`, character(1), "handle"),
                   old$participants$id[!old$participants$is_moderator])
  messages <- Filter(function(s) s$kind %in% .player_message_kinds, page$payload$steps)
  expect_identical(vapply(messages, function(s) as.integer(s$item), integer(1)),
                   old$transcript$round)
  expect_identical(vapply(messages, `[[`, character(1), "handle"), old$transcript$speaker_id)
})

test_that("strip_speaker_prefix drops only the speaker's own label, and the title says so", {
  x <- .player_fixture()
  tr <- x$transcript
  mod <- which(tr$is_moderator & !vapply(tr$metadata, function(m) isTRUE(m$verbatim), logical(1)))[[1]]
  p2 <- which(tr$speaker_id == "P2")[[1]]
  p3 <- which(tr$speaker_id == "P3")[[1]]
  original <- tr$text
  tr$text[[mod]] <- paste("**MOD:**", original[[mod]])
  tr$text[[p2]] <- paste("P2:", original[[p2]])
  tr$text[[p3]] <- paste("P2: I think", original[[p3]])
  x$transcript <- tr

  kept <- .player_build(x)$payload
  texts <- function(p) vapply(Filter(function(s) s$kind %in% .player_message_kinds, p$steps),
                              `[[`, character(1), "text")
  expect_identical(texts(kept), tr$text)
  expect_false(any(grepl("speaker label", unlist(kept$steps[[1]]$lines))))

  stripped <- .player_build(x, strip_speaker_prefix = TRUE)$payload
  out <- texts(stripped)
  expect_identical(out[[mod]], original[[mod]])
  expect_identical(out[[p2]], original[[p2]])
  expect_identical(out[[p3]], tr$text[[p3]])
  expect_true(any(grepl("speaker label the model wrote.*2 lines had one",
                        unlist(stripped$steps[[1]]$lines))))
})

test_that("a missing record stops with an error naming it", {
  x <- .player_fixture()
  drop <- function(name) { y <- x; y[[name]] <- NULL; y }
  build <- function(y) fg_player(y, file = tempfile(fileext = ".html"), open = FALSE)
  expect_error(build(drop("routing")), "`routing`")
  expect_error(build(drop("probes")), "`probes`")
  expect_error(build(drop("participants")), "`participants`")
  expect_error(build(drop("summary")), "`summary`")
  y <- x
  y$transcript$metadata <- NULL
  expect_error(build(y), "`transcript$metadata`", fixed = TRUE)
  y <- x
  y$metadata$started <- NULL
  expect_error(build(y), "`metadata$started`", fixed = TRUE)
  y <- x
  y$routing$scores <- NULL
  expect_error(build(y), "`routing$scores`", fixed = TRUE)
  y <- x
  y$routing <- y$routing[-2, ]
  expect_error(build(y), "no speaker selection for message")
  y <- x
  y$probes$after_message_id[[1]] <- 999L
  expect_error(build(y), "after_message_id")

  agents <- create_agents(2, config = .player_config(), demographics = .player_people(2))
  fg <- FocusGroup$new(topic = "t", purpose = "p", agents = agents, moderator_id = "MOD",
                       turn_taking_flow = create_conversation_flow("round_robin", agents, "MOD"),
                       question_script = list(list(phase = "closing", text = "Close.")))
  fg$conversation_log <- list(list(message_id = 1L, speaker_id = "System", text = "x"))
  fg$records <- NULL
  expect_error(build(fg), "`records`")
  expect_error(build(list(a = 1)), "must be a focus_group_result")
  expect_error(fg_player(x, mode = "slides", open = FALSE), "should be one of")
  expect_error(fg_player(x, open = NA), "`open`")
})

test_that("annotations reach the participant's page and card, and unknown IDs are refused", {
  x <- .player_fixture()
  p <- .player_build(x, annotations = list(P2 = c(stake = "high", recruited = "clinic")))$payload
  agent <- Filter(function(a) identical(a$id, "P2"), p$agents)[[1]]
  expect_identical(agent$annotations,
                   list(list(name = "stake", value = "high"),
                        list(name = "recruited", value = "clinic")))
  expect_length(Filter(function(a) identical(a$id, "P1"), p$agents)[[1]]$annotations, 0L)
  expect_error(fg_player(x, annotations = list(P9 = c(stake = "low")), open = FALSE), "P9")
  expect_error(fg_player(x, annotations = list(P1 = "low"), open = FALSE), "named")
  expect_identical(.player_build(x, mode = "static")$payload$mode, "static")
})

test_that("each participant has one color from a color-blind-safe palette", {
  expect_identical(.fg_player_colors(3), c("#0072B2", "#D55E00", "#009E73"))
  expect_length(unique(.fg_player_colors(7)), 7L)
  expect_identical(.fg_player_colors(9)[[1]], "#332288")
  expect_message(many <- .fg_player_colors(11), "hcl.colors")
  expect_length(unique(many), 11L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", many)))
  expect_false(.fg_player_moderator_color %in% c(.fg_player_colors(7), .fg_player_colors(9)))

  # The same color in the sidebar, the turns, and the bars: the page takes
  # every color from the roster.
  p <- .player_build(.player_fixture())$payload
  expect_identical(vapply(p$agents, `[[`, character(1), "color"), .fg_player_colors(4))
})

test_that("the payload of the fixture is stable", {
  page <- .player_build(.player_fixture())
  lines <- strsplit(as.character(jsonlite::prettify(.player_json(page$html), indent = 1)),
                    "\n", fixed = TRUE)[[1]]
  expect_snapshot_value(as.list(lines), style = "json")
})
