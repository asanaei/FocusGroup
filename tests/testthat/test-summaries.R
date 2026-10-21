# Multi-level summaries: the deterministic checks, the re-ask and failure
# paths, and what reaches the summarizing model. All offline, with scripted
# runners standing in for the model.

# ---- The verifier, against positives and hard negatives -----------------------

# A session record built by hand, so that every message text is known: curly
# and straight apostrophes, a double space and a line break, an ellipsis in
# the speech itself, a silent participant, a probe answer, and personas.
.sum_fixture <- function() {
  transcript <- data.frame(
    message_id = 1:8,
    round = c(0L, 1L, 1L, 1L, 1L, 2L, 2L, 2L),
    speaker_id = c("System", "MOD", "P1", "P2", "P3", "MOD", "P2", "P1"),
    is_moderator = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
    text = c("Participants: P1, P2, P3, P4; Moderator: MOD",
             "Welcome. What should change about the library hours?",
             "I don\u2019t think the evening hours should be cut. Working parents rely on them.",
             "I disagree.  The mornings matter more\nfor retirees who cannot drive at night.",
             "We should not raise fees... but weekends need more staff.",
             "P2, what would you cut first?",
             "Honestly, I would cut the Sunday hours before anything else.",
             "Cutting Sundays would hurt students, so I'm against that."),
    phase = c("setup", rep("engagement_question", 4), rep("exploration_question", 3)),
    stringsAsFactors = FALSE)
  transcript$metadata <- I(rep(list(list()), 8))
  participants <- data.frame(
    id = c("P1", "P2", "P3", "P4", "MOD"),
    role = c(rep("participant", 4), "moderator"),
    is_moderator = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    persona = c("A night-shift nurse with two children.",
                "A retired bus driver who lives alone and reads mystery novels every week.",
                "A student who works weekends.", "A shop owner.", "A neutral moderator."),
    stringsAsFactors = FALSE)
  probes <- data.frame(
    probe = "position", point = "end", agent_id = "P1",
    question = "What do you really think?",
    answer = "Privately, I think the library should close on Mondays entirely.",
    raw_text = NA_character_, finish_reason = "stop", call_id = NA_integer_,
    round = 2L, phase = "probe", turn = 5L, after_message_id = 8L, arm = NA_character_,
    stringsAsFactors = FALSE)
  structure(list(transcript = transcript, participants = participants,
                 routing = FocusGroup:::.fg_routing_data(list()), probes = probes,
                 metadata = list(topic = "library hours")),
            class = c("focus_group_result", "list"))
}

# The plan unit of `by` (the first, or the one keyed `key`) for a source.
.sum_unit <- function(x, by = "session", key = NULL, levels = "brief",
                      include_probes = FALSE) {
  source <- FocusGroup:::.fg_summary_source(x)
  plan <- FocusGroup:::.fg_summary_plan(source, by, levels, include_probes,
                                        FocusGroup:::.fg_summary_templates(NULL),
                                        NULL, .sum_config())
  if (is.null(key)) plan[[1]] else Filter(function(u) identical(u$key, key), plan)[[1]]
}

# A reply with one claim, listed at "brief".
.sum_reply <- function(...) {
  claim <- list(id = 1, unit = "session", kind = "position", text = "A claim.",
                status = "stated", speakers = list(), evidence = list(), context = list(),
                quotes = list())
  fields <- list(...)
  claim[names(fields)] <- fields
  as.character(jsonlite::toJSON(list(claims = list(claim), levels = list(brief = list(1))),
                                auto_unbox = TRUE, null = "null"))
}

.q <- function(id, speaker, text) list(message_id = id, speaker = speaker, text = text)

test_that("the verifier admits every positive and rejects every hard negative", {
  x <- .sum_fixture()
  session <- .sum_unit(x, "session")
  item1 <- .sum_unit(x, "item", key = "1")
  p1 <- function(...) .sum_reply(speakers = list("P1"), evidence = list(3), ...)
  p2 <- function(...) .sum_reply(speakers = list("P2"), evidence = list(4), ...)
  positives <- list(
    exact_span = p1(quotes = list(.q(3, "P1", "Working parents rely on them"))),
    curly_apostrophe_in_message = p1(quotes = list(.q(3, "P1", "I don't think the evening hours"))),
    curly_apostrophe_in_quote = .sum_reply(speakers = list("P1"), evidence = list(8),
      quotes = list(.q(8, "P1", "so I\u2019m against that"))),
    curly_marks_around_quote = .sum_reply(speakers = list("P2"), evidence = list(7),
      quotes = list(.q(7, "P2", "\u201cHonestly, I would cut the Sunday hours\u201d"))),
    whitespace_and_line_break = p2(quotes = list(.q(4, "P2", "The mornings matter more for retirees"))),
    case = p2(quotes = list(.q(4, "P2", "the MORNINGS matter more"))),
    ordered_ellipsis = .sum_reply(speakers = list("P3"), evidence = list(5),
      quotes = list(.q(5, "P3", "We should not raise fees ... weekends need more staff"))),
    unicode_ellipsis = p2(quotes = list(.q(4, "P2", "I disagree\u2026 for retirees who cannot drive"))),
    ellipsis_spoken = .sum_reply(speakers = list("P3"), evidence = list(5),
      quotes = list(.q(5, "P3", "We should not raise fees... but weekends"))),
    change_in_order = .sum_reply(kind = "change", speakers = list("P2"), evidence = list(4, 7),
      text = "P2 moved from defending mornings to cutting Sundays."),
    universal_with_every_speaker = .sum_reply(speakers = list("P1", "P2", "P3"),
      evidence = list(3, 4, 5), kind = "agreement",
      text = "Everyone on the first question spoke about the schedule."),
    silence_not_stated = .sum_reply(status = "not_stated", speakers = list("P4"),
      context = list(2, 6), text = "P4 gave no view on either question."),
    reaction_quoting_context = .sum_reply(speakers = list("P1"), evidence = list(8),
      context = list(7), quotes = list(.q(7, "P2", "I would cut the Sunday hours")),
      text = "P1 rejected P2's proposal to cut Sundays."),
    idiom_is_not_universal = p1(text = "P1 does not want the evening hours cut at all."),
    moderator_invites_everyone = .sum_reply(speakers = list("Moderator"), evidence = list(2),
      text = "The moderator welcomed everyone and asked what should change."),
    own_words_marked_in_text = p1(text = "P1 said \"Working parents rely on them.\""),
    one_marked_word_in_text = p1(text = "P1 does not want the evening hours \"cut\"."),
    silent_about_a_topic = .sum_reply(status = "not_stated", speakers = list("P2"),
      context = list(2), text = "P2 said nothing about fees."),
    possessive_kept_whole = .sum_reply(speakers = list("P1"), evidence = list(8),
      quotes = list(.q(8, "P1", "so I'm against that"))))
  negatives <- list(
    one_word_changed = c("quote_text", p1(quotes = list(.q(3, "P1", "Working parents depend on them")))),
    dropped_not = c("quote_text", .sum_reply(speakers = list("P3"), evidence = list(5),
      quotes = list(.q(5, "P3", "We should raise fees")))),
    reordered_words = c("quote_text", p1(quotes = list(.q(3, "P1", "parents working rely on them")))),
    span_across_messages = c("quote_text", p1(quotes = list(.q(3, "P1", "rely on them. I disagree")))),
    right_text_wrong_id = c("quote_text", p2(quotes = list(.q(4, "P2", "Working parents rely on them")))),
    right_text_same_speaker_wrong_id = c("quote_text", .sum_reply(speakers = list("P1"),
      evidence = list(3, 8), quotes = list(.q(3, "P1", "Cutting Sundays would hurt students")))),
    right_text_wrong_handle = c("quote_speaker", p1(quotes = list(.q(3, "P2", "Working parents rely on them")))),
    id_999 = c("ids_exist", .sum_reply(speakers = list("P1"), evidence = list(999))),
    out_of_order_ellipsis = c("quote_text", .sum_reply(speakers = list("P3"), evidence = list(5),
      quotes = list(.q(5, "P3", "weekends need more staff ... We should not raise fees")))),
    moderator_line_as_participant = c("speaker_match", .sum_reply(speakers = list("P2"),
      evidence = list(6), quotes = list(.q(6, "P2", "what would you cut first")))),
    moderator_quote_as_participant = c("quote_speaker", .sum_reply(speakers = list("P2"),
      evidence = list(7), context = list(6), quotes = list(.q(6, "P2", "what would you cut first")))),
    probe_text_as_quote = c("quote_text", p1(quotes = list(.q(3, "P1", "the library should close on Mondays")))),
    probe_text_in_claim = c("probe_leak", p1(text = "P1 thinks the library should close on Mondays entirely.")),
    persona_text_as_quote = c("quote_text", p2(quotes = list(.q(4, "P2", "a retired bus driver who lives alone")))),
    two_word_quote = c("quote_length", p1(quotes = list(.q(3, "P1", "Working parents")))),
    mid_word_fragment = c("quote_text", p1(quotes = list(.q(3, "P1", "orking parents rely on")))),
    system_row = c("not_system", .sum_reply(speakers = list("P1"), evidence = list(1, 3))),
    silent_participant_content = c("speaker_evidence", .sum_reply(speakers = list("P4"),
      evidence = list(3), text = "P4 wants evening hours.")),
    stated_without_evidence = c("evidence", .sum_reply(speakers = list("P1"),
      text = "P1 wants evening hours.")),
    universal_without_every_speaker = c("universal", p1(text = "Everyone wanted longer evening hours.")),
    consensus_without_speakers = c("universal", .sum_reply(status = "not_stated",
      context = list(2), text = "No consensus was reached on the first question.")),
    all_of_them_one_speaker = c("universal", p1(text = "All of them opposed cuts to the evening hours.")),
    handle_off_roster = c("roster", p1(text = "P1 and P7 opposed the cuts.")),
    change_out_of_order = c("change", .sum_reply(kind = "change", speakers = list("P2"),
      evidence = list(7, 4))),
    change_two_speakers = c("change", .sum_reply(kind = "change", speakers = list("P1", "P2"),
      evidence = list(3, 7))),
    quote_from_uncited_message = c("quote_source", p1(quotes = list(.q(5, "P3", "weekends need more staff")))),
    unknown_speaker = c("speakers", .sum_reply(speakers = list("Ana"), evidence = list(3))),
    other_speakers_words_marked_in_text = c("text_quote",
      p1(text = "P1 said \"I would cut the Sunday hours before anything else.\"")),
    text_names_unlisted_participant = c("text_speakers", p1(text = "P3 wants the evening hours kept.")),
    text_gives_view_to_silent = c("silent", .sum_reply(speakers = list("P1", "P4"),
      evidence = list(3), text = "P1 and P4 want the evening hours kept.")),
    not_stated_silence_of_a_speaker = c("silence", .sum_reply(status = "not_stated",
      speakers = list("P2"), context = list(2), text = "P2 said nothing in the discussion.")),
    moderator_speaker_everyone_agreed = c("universal", .sum_reply(speakers = list("Moderator"),
      evidence = list(2), text = "Everyone agreed that the hours should change.")),
    nobody_disagreed = c("universal", p1(text = "Nobody disagreed with P1.")),
    no_one_objected = c("universal", p1(text = "No one objected to keeping evening hours.")),
    whole_group = c("universal", p1(text = "The whole group wanted evening hours.")),
    participants_agreed = c("universal", p1(text = "The participants agreed that evening hours matter.")),
    each_of_them = c("universal", p1(text = "Each of them valued the evening hours.")),
    broad_agreement = c("universal", p1(text = "There was broad agreement on evening hours.")),
    ellipsis_drops_not = c("quote_omission", .sum_reply(speakers = list("P3"), evidence = list(5),
      quotes = list(.q(5, "P3", "We should ... raise fees")))),
    ellipsis_drops_contraction = c("quote_omission", p1(quotes = list(
      .q(3, "P1", "I ... think the evening hours should be cut")))),
    ellipsis_single_words = c("quote_omission", p2(quotes = list(.q(4, "P2", "I ... cannot ... drive")))),
    starts_inside_contraction = c("quote_text", p1(quotes = list(.q(3, "P1", "t think the evening hours")))),
    ends_inside_contraction = c("quote_text", .sum_reply(speakers = list("P1"), evidence = list(8),
      quotes = list(.q(8, "P1", "hurt students, so I")))))

  admitted <- function(reply, unit = session) {
    verdict <- FocusGroup:::.fg_verify_unit(reply, "stop", unit)
    list(admitted = !length(verdict$claims[[1]]$failures),
         failed = unique(verdict$checks$check[!verdict$checks$passed]),
         passed = verdict$passed)
  }
  false_rejections <- names(Filter(function(r) !admitted(r)$passed, positives))
  false_admissions <- names(Filter(function(n) admitted(n[2])$admitted, negatives))
  expect_identical(false_rejections, character())
  expect_identical(false_admissions, character())
  # Each negative fails for the reason it was built to show.
  for (name in names(negatives)) {
    expect_true(negatives[[name]][1] %in% admitted(negatives[[name]][2])$failed, info = name)
  }
  # Scope: a message from item 2 is outside an item-1 unit.
  out_of_scope <- admitted(.sum_reply(unit = "1", speakers = list("P2"), evidence = list(7)), item1)
  expect_false(out_of_scope$admitted)
  expect_true("in_scope" %in% out_of_scope$failed)
  in_scope <- admitted(.sum_reply(unit = "1", speakers = list("P2"), evidence = list(4)), item1)
  expect_true(in_scope$admitted)
  # With include_probes, a probe answer may be cited and quoted.
  probed <- .sum_unit(x, "session", include_probes = TRUE)
  reply <- .sum_reply(speakers = list("P1"), probe_evidence = list("Q1"),
                      text = "P1 said privately that the library should close on Mondays entirely.",
                      quotes = list(list(message_id = NULL, probe = "Q1", speaker = "P1",
                                         text = "the library should close on Mondays")))
  expect_true(admitted(reply, probed)$passed)
  expect_false(admitted(reply, session)$admitted)
})

test_that("quotes are cut from the source with their offsets", {
  source <- "I disagree.  The mornings matter more\nfor retirees who cannot drive at night."
  m <- FocusGroup:::.fg_match_quote("the MORNINGS matter more for retirees", source)
  expect_true(m$found)
  expect_identical(m$verbatim, "The mornings matter more\nfor retirees")
  expect_identical(substr(source, m$start, m$end), m$verbatim)
  joined <- FocusGroup:::.fg_match_quote("I disagree ... cannot drive", source)
  expect_identical(joined$verbatim, "I disagree ... cannot drive")
  expect_identical(nrow(joined$spans), 2L)
  expect_identical(substr(source, joined$spans$start[2], joined$spans$end[2]), "cannot drive")
  curly <- "I don\u2019t think so, honestly."
  m <- FocusGroup:::.fg_match_quote("don't think so", curly)
  expect_identical(m$verbatim, "don\u2019t think so")
})

test_that("a reply that does not parse, or was cut off, fails as a whole", {
  unit <- .sum_unit(.sum_fixture(), "session")
  bad <- FocusGroup:::.fg_verify_unit("Here are my thoughts, without JSON.", "stop", unit)
  expect_false(bad$passed)
  expect_true(bad$fatal)
  expect_match(bad$failures[1], "not a valid JSON object")
  cut <- FocusGroup:::.fg_verify_unit(.sum_reply(speakers = list("P1"), evidence = list(3)),
                                      "length", unit)
  expect_false(cut$passed)
  expect_true(cut$truncated)
  fenced <- paste0("```json\n", .sum_reply(speakers = list("P1"), evidence = list(3)), "\n```")
  expect_true(FocusGroup:::.fg_verify_unit(fenced, "stop", unit)$passed)
  no_level <- '{"claims": [], "levels": {}}'
  expect_true(FocusGroup:::.fg_verify_unit(no_level, "stop", unit)$fatal)
})

test_that("coverage counts only claims that report what a speaker said", {
  x <- .sum_fixture()
  p2 <- .sum_unit(x, "participant", key = "P2")
  only_not_stated <- .sum_reply(unit = "P2", status = "not_stated", speakers = list("P2"),
                                text = "P2 did not take a position.")
  verdict <- FocusGroup:::.fg_verify_unit(only_not_stated, "stop", p2)
  expect_true(verdict$fatal)
  expect_true(any(grepl("no claim that reports what P2 said", verdict$failures)))
  item1 <- .sum_unit(x, "item", key = "1")
  reply <- as.character(jsonlite::toJSON(list(claims = list(
    list(id = 1, unit = "1", kind = "position", text = "P1 wants evening hours.",
         status = "stated", speakers = list("P1"), evidence = list(3), context = list(),
         quotes = list()),
    list(id = 2, unit = "1", kind = "other", text = "P2 and P3 said nothing.",
         status = "not_stated", speakers = list("P2", "P3"), evidence = list(),
         context = list(2), quotes = list())), levels = list(brief = list(1, 2))),
    auto_unbox = TRUE))
  verdict <- FocusGroup:::.fg_verify_unit(reply, "stop", item1)
  expect_true(verdict$fatal)
  expect_true(any(grepl("P2 says P2 did not speak", verdict$failures, fixed = TRUE) |
                    grepl("does not report P2", verdict$failures, fixed = TRUE)))
  expect_true(any(grepl("does not report P3", verdict$failures, fixed = TRUE)))
})

test_that("the reply is found among prose, braces, and other code blocks", {
  unit <- .sum_unit(.sum_fixture(), "session")
  reply <- .sum_reply(speakers = list("P1"), evidence = list(3))
  wrapped <- c(
    paste("Here is my answer, with {P1} noted:", reply, "Note: {P1} spoke first."),
    paste0("```text\nnot json\n```\n```json\n", reply, "\n```"),
    paste0("{\"draft\": true}\n", reply))
  for (w in wrapped) expect_true(FocusGroup:::.fg_verify_unit(w, "stop", unit)$passed, info = w)
  cut <- substr(reply, 1, nchar(reply) - 5L)
  expect_match(FocusGroup:::.fg_verify_unit(cut, "stop", unit)$failures[1],
               "no complete JSON object")
})

# ---- Happy paths ----------------------------------------------------------------

test_that("every unit summarizes a session with checked claims", {
  result <- .sum_session(n = 4)
  for (by in c("participant", "item", "disagreements", "trajectory", "turn", "session")) {
    stub <- .sum_runner()
    out <- summarize_focus_group(result, .sum_config(), by = by,
                                 compression = c("headline", "brief", "detailed"),
                                 .runner = stub$runner)
    expect_s3_class(out, "focus_group_summary")
    expect_true(all(out$checks$passed), info = by)
    expect_identical(nrow(out$calls), stub$n, info = by)
    expect_true(all(out$calls$call_type == paste0("summary_", by)), info = by)
    expect_identical(sort(unique(out$summaries$level)), sort(c("headline", "brief", "detailed")))
    expect_identical(out$metadata$by, by)
    expect_true(out$metadata$complete)
    if (by != "disagreements") {
      expect_gt(nrow(out$claims), 0L)
      expect_true(all(!is.na(out$summaries$text[out$summaries$n_claims > 0])))
      expect_identical(nrow(out$quotes), nrow(out$claims))
      # Each quote is cut from the message it cites.
      tr <- result$transcript
      for (i in seq_len(nrow(out$quotes))) {
        q <- out$quotes[i, ]
        expect_identical(substr(tr$text[tr$message_id == q$message_id], q$start, q$end),
                         q$verbatim)
      }
    }
  }
  # Units by participant: one call per participant, one row per length.
  stub <- .sum_runner()
  out <- summarize_focus_group(result, .sum_config(), .runner = stub$runner)
  expect_identical(stub$n, 4L)
  expect_identical(out$summaries$unit, c("P1", "P2", "P3", "P4"))
  expect_identical(out$summaries$speaker_id, c("P1", "P2", "P3", "P4"))
  expect_true(all(out$summaries$budget == "100-120 words"))
  # By turn, every message has a summary at every length.
  stub <- .sum_runner()
  out <- summarize_focus_group(result, .sum_config(), by = "turn",
                               compression = c("headline", "detailed"), .runner = stub$runner)
  spoken <- result$transcript$message_id[result$transcript$speaker_id != "System"]
  expect_setequal(as.integer(out$summaries$unit[out$summaries$level == "headline"]), spoken)
  expect_true(all(!is.na(out$claims$act)))
  expect_true(all(out$summaries$budget[out$summaries$level == "headline"] == "20 words per turn"))
})

test_that("the trajectory's stages are fixed in code", {
  result <- .sum_session(n = 4)
  stub <- .sum_runner()
  out <- summarize_focus_group(result, .sum_config(), by = "trajectory",
                               compression = c("headline", "brief"), .runner = stub$runner)
  expect_identical(out$summaries$unit[out$summaries$level == "headline"],
                   c("third_1", "third_2", "third_3"))
  expect_identical(out$summaries$unit[out$summaries$level == "brief"], c("item_2", "item_3"))
  expect_match(stub$prompts[1], "For the headline: third_1 (messages #2 to #4)", fixed = TRUE)
  # A headline list that names an item stage fails the coverage check.
  wrong <- function(prompt, n, messages) {
    reply <- jsonlite::fromJSON(.sum_honest_reply(prompt), simplifyVector = FALSE)
    reply$levels$headline <- reply$levels$brief
    as.character(jsonlite::toJSON(reply, auto_unbox = TRUE, null = "null"))
  }
  stub <- .sum_runner(wrong)
  err <- tryCatch(summarize_focus_group(result, .sum_config(), by = "trajectory",
                                        compression = c("headline", "brief"),
                                        .runner = stub$runner),
                  focusgroup_summary_check_failure = function(e) e)
  expect_s3_class(err, "focusgroup_summary_check_failure")
  expect_match(stub$messages[[2]][[3]]$content, "has no claim for stage third_1")
})

test_that("a FocusGroup and its result give the same summary input", {
  result <- .sum_session()
  a <- .sum_runner()
  b <- .sum_runner()
  from_result <- summarize_focus_group(result, .sum_config(), .runner = a$runner)
  from_group <- summarize_focus_group(result$focus_group, .sum_config(), .runner = b$runner)
  expect_identical(a$prompts, b$prompts)
  expect_identical(from_result$summaries, from_group$summaries)
})

# ---- Reasoning, re-asks, and failures -------------------------------------------

test_that("a JSON decoy inside reasoning is ignored and the raw reply is kept", {
  result <- .sum_session()
  decoy <- '{"claims": [{"id": 1, "unit": "P1", "kind": "position", "text": "Decoy.", "status": "stated", "speakers": ["P1"], "evidence": [999], "context": [], "quotes": []}], "levels": {"brief": [1]}}'
  stub <- .sum_runner(function(prompt, n, messages) {
    paste0("<think>", decoy, "</think>\n", .sum_honest_reply(prompt))
  })
  out <- summarize_focus_group(result, .sum_config(), .runner = stub$runner)
  expect_identical(stub$n, 3L)
  expect_false(any(out$claims$text == "Decoy."))
  expect_true(all(grepl("<think>", out$calls$raw_text, fixed = TRUE)))
  expect_false(any(grepl("<think>", out$calls$cleaned_text, fixed = TRUE)))
})

test_that("a bad quote is sent back once, naming the message and its text", {
  result <- .sum_session()
  stub <- .sum_runner(function(prompt, n, messages) {
    reply <- .sum_honest_reply(prompt)
    if (n == 1L) sub("Evening hours matter most", "Evening hours count most", reply, fixed = TRUE)
    else reply
  })
  out <- summarize_focus_group(result, .sum_config(), .runner = stub$runner)
  expect_identical(stub$n, 4L)
  reask <- stub$messages[[2]]
  expect_length(reask, 3L)
  expect_identical(reask[[2]]$role, "assistant")
  expect_match(reask[[3]]$content, "Quote 1 of claim 1 is not in message 4.", fixed = TRUE)
  expect_match(reask[[3]]$content,
               "Message 4 reads: \"Evening hours matter most to working parents like me.\"",
               fixed = TRUE)
  expect_identical(out$calls$attempt, c(1L, 2L, 1L, 1L))
  expect_identical(out$claims$attempt[out$claims$unit == "P1"], c(2L, 2L))
  expect_true(any(!out$checks$passed & out$checks$attempt == 1L))
})

test_that("two failures raise the classed error with the partial result", {
  result <- .sum_session()
  stub <- .sum_runner(function(prompt, n, messages) {
    reply <- .sum_honest_reply(prompt)
    if (grepl("one participant, P2", messages[[1]]$content, fixed = TRUE)) {
      sub("\"evidence\":[5]", "\"evidence\":[999]", reply, fixed = TRUE)
    } else reply
  })
  err <- tryCatch(summarize_focus_group(result, .sum_config(), .runner = stub$runner),
                  focusgroup_summary_check_failure = function(e) e)
  expect_s3_class(err, "focusgroup_summary_check_failure")
  expect_match(conditionMessage(err), "cites message 999, which does not exist")
  expect_identical(err$unit, "P2")
  expect_length(err$replies, 2L)
  expect_true(all(c(1L, 2L) %in% err$checks$attempt))
  expect_true(any(err$checks$check == "ids_exist" & !err$checks$passed))
  partial <- err$partial
  expect_s3_class(partial, "focus_group_summary")
  expect_false(partial$metadata$complete)
  expect_identical(unique(partial$claims$unit), "P1")
  expect_identical(nrow(partial$calls), 3L)
  expect_identical(partial$summaries$note[partial$summaries$unit == "P2"], "not summarized")
  expect_identical(stub$n, 3L)
})

test_that("a missing participant triggers a re-ask that names them", {
  result <- .sum_session()
  stub <- .sum_runner(function(prompt, n, messages) {
    reply <- jsonlite::fromJSON(.sum_honest_reply(prompt), simplifyVector = FALSE)
    if (n == 1L) {
      reply$claims <- reply$claims[1]
      reply$levels <- list(brief = list(reply$claims[[1]]$id))
    }
    as.character(jsonlite::toJSON(reply, auto_unbox = TRUE, null = "null"))
  })
  out <- summarize_focus_group(result, .sum_config(), by = "item", .runner = stub$runner)
  expect_match(stub$messages[[2]][[3]]$content,
               "The reply does not report P2, who spoke on this item (message #5)", fixed = TRUE)
  expect_true(out$metadata$complete)
})

test_that("content for a silent participant fails, and the silent get no call", {
  result <- .sum_session(n = 5)
  silent <- "P5"
  expect_false(silent %in% result$transcript$speaker_id)
  stub <- .sum_runner()
  out <- summarize_focus_group(result, .sum_config(), .runner = stub$runner)
  expect_identical(stub$n, 4L)
  row <- out$summaries[out$summaries$unit == silent, ]
  expect_identical(row$n_turns, 0L)
  expect_true(is.na(row$text))
  expect_match(row$note, "no call")
  expect_match(stub$prompts[1], "P5 (no messages)", fixed = TRUE)

  invent <- function(prompt, n, messages) {
    reply <- jsonlite::fromJSON(.sum_honest_reply(prompt), simplifyVector = FALSE)
    reply$claims[[length(reply$claims) + 1L]] <- list(
      id = 99, unit = "session", kind = "position", text = "P5 favored mornings.",
      status = "stated", speakers = list("P5"), evidence = list(5), context = list(),
      quotes = list())
    as.character(jsonlite::toJSON(reply, auto_unbox = TRUE, null = "null"))
  }
  stub <- .sum_runner(invent)
  err <- tryCatch(summarize_focus_group(result, .sum_config(), by = "session",
                                        .runner = stub$runner),
                  focusgroup_summary_check_failure = function(e) e)
  expect_s3_class(err, "focusgroup_summary_check_failure")
  expect_match(stub$messages[[2]][[3]]$content, "P5 has no message here", fixed = TRUE)
})

test_that("a reply cut off at the token limit is re-asked with twice the limit", {
  result <- .sum_session()
  stub <- .sum_runner(finish = function(n) if (n == 1L) "length" else "stop")
  out <- summarize_focus_group(result, .sum_config(), max_tokens = 500,
                               .runner = stub$runner)
  expect_identical(stub$tokens, c(500L, 1000L, 500L, 500L))
  expect_match(stub$messages[[2]][[3]]$content, "cut off at the token limit", fixed = TRUE)
  expect_true(out$metadata$complete)
})

test_that("on_fail = \"drop\" keeps the rest but never drops dissent or change", {
  result <- .sum_session()
  bad_second <- function(prompt, n, messages) {
    reply <- jsonlite::fromJSON(.sum_honest_reply(prompt), simplifyVector = FALSE)
    if (grepl("one participant, P1", messages[[1]]$content, fixed = TRUE)) {
      reply$claims[[2]]$quotes[[1]]$text <- "words P1 never said"
    }
    as.character(jsonlite::toJSON(reply, auto_unbox = TRUE, null = "null"))
  }
  stub <- .sum_runner(bad_second)
  out <- summarize_focus_group(result, .sum_config(), on_fail = "drop", .runner = stub$runner)
  expect_identical(nrow(out$rejected), 1L)
  expect_identical(out$rejected$unit, "P1")
  expect_match(out$rejected$reasons, "is not in message 8")
  expect_identical(out$summaries$n_claims[out$summaries$unit == "P1"], 1L)
  expect_true(out$metadata$complete)
  expect_output(print(out), "Dropped: 1")

  change <- function(prompt, n, messages) {
    reply <- jsonlite::fromJSON(.sum_honest_reply(prompt), simplifyVector = FALSE)
    if (grepl("one participant, P1", messages[[1]]$content, fixed = TRUE)) {
      reply$claims[[2]]$kind <- "change"
      reply$claims[[2]]$evidence <- list(8, 4)
    }
    as.character(jsonlite::toJSON(reply, auto_unbox = TRUE, null = "null"))
  }
  stub <- .sum_runner(change)
  err <- tryCatch(summarize_focus_group(result, .sum_config(), on_fail = "drop",
                                        .runner = stub$runner),
                  focusgroup_summary_check_failure = function(e) e)
  expect_s3_class(err, "focusgroup_summary_check_failure")
  expect_match(conditionMessage(err), "reports change")
})

test_that("on_fail = \"drop\" still stops on a cut-off reply or a coverage gap", {
  result <- .sum_session()
  stub <- .sum_runner(finish = function(n) "length")
  err <- tryCatch(summarize_focus_group(result, .sum_config(), on_fail = "drop",
                                        .runner = stub$runner),
                  focusgroup_summary_check_failure = function(e) e)
  expect_s3_class(err, "focusgroup_summary_check_failure")
  expect_identical(stub$n, 2L)
  # P2's only claim on item 2 misquotes every time; dropping it would leave
  # P2, who spoke, out of the item's summary.
  misquote <- function(prompt, n, messages) {
    sub("I disagree, the mornings", "I disagree, mornings", .sum_honest_reply(prompt), fixed = TRUE)
  }
  stub <- .sum_runner(misquote)
  err <- tryCatch(summarize_focus_group(result, .sum_config(), by = "item", on_fail = "drop",
                                        .runner = stub$runner),
                  focusgroup_summary_check_failure = function(e) e)
  expect_s3_class(err, "focusgroup_summary_check_failure")
  expect_match(conditionMessage(err), "does not report P2")
})

test_that("an ending is read only in the disagreements register", {
  result <- .sum_session()
  with_ending <- function(prompt, n, messages) {
    reply <- jsonlite::fromJSON(.sum_honest_reply(prompt), simplifyVector = FALSE)
    reply$claims[[1]]$ending <- list(how = "Concession", message_id = 5)
    as.character(jsonlite::toJSON(reply, auto_unbox = TRUE, null = "null"))
  }
  out <- summarize_focus_group(result, .sum_config(), compression = c("brief", "detailed"),
                               .runner = .sum_runner(with_ending)$runner)
  expect_false(any(grepl("concession", out$summaries$text)))
  expect_true(all(is.na(out$claims$ending)))
  register <- function(prompt, n, messages) {
    claim <- list(id = 1, unit = "D1", kind = "disagreement", status = "stated",
                  text = "P1 and P2 disagree on evening and morning hours.",
                  speakers = list("P1", "P2"), evidence = list(4, 5), context = list(3),
                  quotes = list(), ending = list(how = "Moved_On", message_id = 6))
    as.character(jsonlite::toJSON(list(claims = list(claim), levels = list(brief = list(1))),
                                  auto_unbox = TRUE, null = "null"))
  }
  out <- summarize_focus_group(result, .sum_config(), by = "disagreements",
                               .runner = .sum_runner(register)$runner)
  expect_identical(out$claims$ending, "moved_on")
  expect_match(out$summaries$text, "ended when the moderator moved on at #6", fixed = TRUE)
})

test_that("a claim that is not stated carries its status at every length", {
  result <- .sum_session()
  add_gap <- function(prompt, n, messages) {
    reply <- jsonlite::fromJSON(.sum_honest_reply(prompt), simplifyVector = FALSE)
    unit <- reply$claims[[1]]$unit
    reply$claims[[length(reply$claims) + 1L]] <- list(
      id = 9, unit = unit, kind = "not_addressed", status = "not_stated",
      text = paste(unit, "gave no reason for the view."), speakers = list(unit),
      evidence = list(), context = list(), quotes = list())
    reply$levels <- list(headline = list(9), brief = list(1, 9))
    as.character(jsonlite::toJSON(reply, auto_unbox = TRUE, null = "null"))
  }
  out <- summarize_focus_group(result, .sum_config(), compression = c("headline", "brief"),
                               .runner = .sum_runner(add_gap)$runner)
  expect_true(all(grepl("gave no reason for the view. [not stated]", out$summaries$text,
                        fixed = TRUE)))
})

test_that("retries on the live path leave the random-number state as it was", {
  result <- .sum_session()
  testthat::local_mocked_bindings(call_llm_robust = function(config, messages, ...) {
    stats::runif(1)
    data.frame(response_text = .sum_honest_reply(messages[[1]]$content), finish_reason = "stop")
  }, .package = "LLMR")
  set.seed(110)
  before <- .Random.seed
  out <- summarize_focus_group(result, .sum_config())
  expect_identical(nrow(out$calls), 3L)
  expect_identical(.Random.seed, before)
})

test_that("the summary writes its token limit into the field the configuration uses", {
  result <- .sum_session()
  seen <- list()
  stub <- .sum_runner(finish = function(n) if (n == 1L) "length" else "stop")
  runner <- function(experiments, ...) {
    seen[[length(seen) + 1L]] <<- experiments$config[[1]]$model_params
    stub$runner(experiments, ...)
  }
  cfg <- LLMR::llm_config("openai", "o4-mini", max_completion_tokens = 4000)
  out <- summarize_focus_group(result, cfg, .runner = runner)
  expect_identical(seen[[1]]$max_completion_tokens, 4000L)
  expect_null(seen[[1]]$max_tokens)
  expect_identical(seen[[2]]$max_completion_tokens, 8000L)
  expect_identical(unname(out$metadata$max_tokens[1]), 4000)
})

# ---- What reaches the model -----------------------------------------------------

test_that("imported speakers' names never reach a prompt", {
  transcript <- data.frame(
    speaker = c("Moderator", "Ana", "Ben", "Ana", "Moderator", "Ben"),
    text = c("What do you make of the new library hours?",
             "They help working parents like me; evenings matter most.",
             "I see it differently. Mornings are now far too crowded.",
             "Crowded mornings still beat being locked out after work.",
             "Say more about what changed in the mornings.",
             "Staff are stretched thin before noon, so lines are longer."))
  fg <- focus_group_from_transcript(transcript, topic = "library hours")
  for (by in c("participant", "item", "turn", "session", "trajectory", "disagreements")) {
    stub <- .sum_runner()
    out <- summarize_focus_group(fg, .sum_config(), by = by, .runner = stub$runner)
    expect_false(any(grepl("\\b(Ana|Ben)\\b", stub$prompts, perl = TRUE)), info = by)
    expect_false(any(grepl("\\b(Ana|Ben)\\b", unlist(lapply(stub$messages, .sum_prompt)),
                           perl = TRUE)), info = by)
  }
  handles <- out$metadata$handles
  expect_identical(handles$label[match(c("Ana", "Ben", "Moderator"), handles$speaker_id)],
                   c("P1", "P2", "Moderator"))
  stub <- .sum_runner()
  out <- summarize_focus_group(fg, .sum_config(), .runner = stub$runner)
  expect_identical(out$summaries$speaker_id, c("Ana", "Ben"))
  expect_identical(out$claims$speaker_ids[[1]], "Ana")
  expect_identical(unique(out$quotes$speaker_id), c("Ana", "Ben"))
  # Item numbers fall back to the moderator's messages: item 1 starts at the
  # first moderator line.
  turns <- fg_turns(fg)
  expect_identical(turns$item, c(1L, 1L, 1L, 1L, 2L, 2L))
  expect_identical(turns$link, rep("sequence", 6L))
})

test_that("handles from the records label the speakers; user IDs stay in the researcher's columns", {
  result <- .sum_contract(.sum_session(), handles = c(P1 = "Maria", P2 = "Jon", P3 = "Lee"))
  result$transcript$speaker_id <- sub("^P", "voter_", result$transcript$speaker_id)
  result$participants$id <- sub("^P", "voter_", result$participants$id)
  stub <- .sum_runner(function(prompt, n, messages) {
    unit <- sub(".*one participant, ([A-Za-z]+)\\..*", "\\1", prompt)
    msgs <- .sum_prompt_messages(prompt)
    own <- msgs[msgs$label == unit, ][1, ]
    claim <- .sum_claim(1, unit, unit, own)
    as.character(jsonlite::toJSON(list(claims = list(claim), levels = list(brief = list(1))),
                                  auto_unbox = TRUE))
  })
  out <- summarize_focus_group(result, .sum_config(), .runner = stub$runner)
  expect_false(any(grepl("voter_", stub$prompts, fixed = TRUE)))
  expect_match(stub$prompts[1], "[#4] Maria: ", fixed = TRUE)
  expect_match(stub$prompts[1], "] Moderator: ", fixed = TRUE)
  expect_identical(out$summaries$unit, c("Maria", "Jon", "Lee"))
  expect_identical(out$summaries$speaker_id, c("voter_1", "voter_2", "voter_3"))
})

test_that("names inside messages reach the model as labels; quotes are cut from the original", {
  transcript <- data.frame(
    speaker = c("Moderator", "Ana", "Ben", "Moderator", "Ana"),
    text = c("What should change about the hours?",
             "Longer evening hours for working parents.",
             "I agree with Ana about the evening hours.",
             "Ana, why do evenings matter to you?",
             "Because I work days and Ben works nights."))
  fg <- focus_group_from_transcript(transcript, topic = "library hours")
  reply <- function(prompt, n, messages) {
    claim <- list(id = 1, unit = "session", kind = "agreement", status = "stated",
                  text = "P2 agreed with P1 about evening hours.", speakers = list("P2"),
                  evidence = list(3), context = list(2),
                  quotes = list(list(message_id = 3, speaker = "P2",
                                     text = "I agree with P1 about")))
    as.character(jsonlite::toJSON(list(claims = list(claim), levels = list(brief = list(1))),
                                  auto_unbox = TRUE))
  }
  stub <- .sum_runner(reply)
  out <- summarize_focus_group(fg, .sum_config(), by = "session", .runner = stub$runner)
  expect_false(any(grepl("\\b(Ana|Ben)\\b", stub$prompts, perl = TRUE)))
  expect_match(stub$prompts[1], "[#4] Moderator: P1, why do evenings matter to you?", fixed = TRUE)
  expect_match(stub$prompts[1], "[#5] P1: Because I work days and P2 works nights.", fixed = TRUE)
  expect_identical(out$quotes$verbatim, "I agree with Ana about")
  expect_identical(substr(transcript$text[3], out$quotes$start, out$quotes$end),
                   "I agree with Ana about")
  named <- function(prompt, n, messages) {
    sub("P2 agreed with P1", "P2 agreed with Ana", reply(prompt, n, messages), fixed = TRUE)
  }
  err <- tryCatch(summarize_focus_group(fg, .sum_config(), by = "session",
                                        .runner = .sum_runner(named)$runner),
                  focusgroup_summary_check_failure = function(e) e)
  expect_s3_class(err, "focusgroup_summary_check_failure")
  expect_match(conditionMessage(err), "uses the name Ana; refer to that speaker as P1", fixed = TRUE)
})

test_that("a voice whose name is a free label keeps it, and participants take others", {
  result <- .sum_session(injections = fg_injection("A voice from outside.", at = fg_after_item(2),
                                                   from = "Guest"))
  result$transcript$speaker_id <- sub("^P", "voter_", result$transcript$speaker_id)
  result$transcript$speaker_id[result$transcript$speaker_id == "Guest"] <- "P2"
  result$participants$id <- sub("^P", "voter_", result$participants$id)
  reply <- function(prompt, n, messages) {
    msgs <- .sum_prompt_messages(prompt)
    claim <- .sum_claim(1, "session", "P1", msgs[msgs$label == "P1", ][1, ])
    as.character(jsonlite::toJSON(list(claims = list(claim), levels = list(brief = list(1))),
                                  auto_unbox = TRUE))
  }
  stub <- .sum_runner(reply)
  out <- summarize_focus_group(result, .sum_config(), by = "session", .runner = stub$runner)
  expect_match(stub$prompts[1], "P2 (injected): A voice from outside.", fixed = TRUE)
  expect_false(grepl("voter_", stub$prompts[1], fixed = TRUE))
  expect_setequal(unique(.sum_prompt_messages(stub$prompts[1])$label),
                  c("Moderator", "P1", "P2", "P3", "P4"))
  handles <- out$metadata$handles
  expect_identical(handles$label[match(c("voter_1", "voter_2", "voter_3"), handles$speaker_id)],
                   c("P1", "P3", "P4"))
})

test_that("results without the 0.5.5 record fields fall back as the contract states", {
  result <- .sum_session()
  expect_false(any(c("handle", "item") %in% names(result$transcript)))
  turns <- fg_turns(result)
  expect_identical(turns$item, result$transcript$round[-1])
  expect_identical(turns$handle, result$transcript$speaker_id[-1])
  expect_true(all(is.na(turns$trigger_message_id)))
  stub <- .sum_runner()
  summarize_focus_group(result, .sum_config(), .runner = stub$runner)
  # The moderator, whose handle is its ID "MOD", is shown as "Moderator".
  expect_false(any(grepl("MOD:", stub$prompts, fixed = TRUE)))
  expect_match(stub$prompts[1], "[#2] Moderator: ", fixed = TRUE)

  # With the contract's columns, items come from `item`, and routing links
  # come from `trigger_message_id`.
  result <- .sum_contract(result)
  result$transcript$item <- c(0L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 4L)
  called <- which(result$routing$message_id == 7L)
  result$routing$rule[called] <- "called_on"
  result$routing$trigger_message_id[called] <- 6L
  turns <- fg_turns(result)
  expect_identical(turns$item, c(1L, 2L, 2L, 2L, 2L, 2L, 3L, 4L))
  expect_identical(turns$link[turns$message_id == 7L], "called_on")
  expect_identical(turns$linked_message_id[turns$message_id == 7L], 6L)
  expect_identical(turns$link[turns$message_id == 8L], "sequence")
  expect_identical(turns$linked_message_id[turns$message_id == 8L], 7L)
  expect_identical(turns$handle[turns$speaker_id == "MOD"][1], "Moderator")
  stub <- .sum_runner()
  out <- summarize_focus_group(result, .sum_config(), by = "item", .runner = stub$runner)
  expect_identical(out$summaries$unit, c("1", "2", "3", "4"))
  expect_identical(out$summaries$n_turns, c(0L, 3L, 1L, 0L))
})

test_that("probe answers reach the model only when asked for", {
  result <- .sum_session(probes = fg_probe("What do you really think?", at = "end"))
  answer <- result$probes$answer[1]
  expect_false(is.na(answer))
  stub <- .sum_runner()
  summarize_focus_group(result, .sum_config(), by = "session", .runner = stub$runner)
  expect_false(any(grepl(answer, stub$prompts, fixed = TRUE)))
  expect_false(any(grepl("probe answers", stub$prompts, fixed = TRUE)))
  stub <- .sum_runner()
  out <- summarize_focus_group(result, .sum_config(), by = "session", include_probes = TRUE,
                               .runner = stub$runner)
  expect_match(stub$prompts[1], "Private probe answers.", fixed = TRUE)
  expect_match(stub$prompts[1], paste0("[Q1] P1, asked after message #9: \"What do you really ",
                                       "think?\" Answer: ", answer), fixed = TRUE)
  expect_true(out$metadata$include_probes)
})

test_that("injected messages are labeled, and the System row is never sent", {
  result <- .sum_session(n = 3, injections = list(
    fg_injection("A guest objection about costs.", at = fg_after_item(2),
                 from = "Guest expert"),
    fg_injection("A note for participants only.", at = fg_after_item(3),
                 visibility = "participants")))
  stub <- .sum_runner()
  summarize_focus_group(result, .sum_config(), by = "session", .runner = stub$runner)
  expect_match(stub$prompts[1], "Guest expert (injected): A guest objection about costs.",
               fixed = TRUE)
  expect_match(stub$prompts[1],
               "Moderator (injected, shown to participants only): A note for participants only.",
               fixed = TRUE)
  expect_false(grepl("Participants: P1, P2, P3; Moderator: MOD", stub$prompts[1], fixed = TRUE))
  turns <- fg_turns(result)
  expect_identical(turns$visibility[turns$injected], c("shared", "participants"))
})

test_that("arms are summarized as the shared prefix and each arm after the branch", {
  stub_session <- .sum_session
  count <- 0L
  runner <- function(experiments, ...) {
    count <<- count + 1L
    prompt <- .sum_prompt(experiments$messages[[1]])
    experiments$response_text <- if (grepl("MODERATOR", prompt, fixed = TRUE)) {
      "What should change about the library hours?"
    } else if (grepl("Summary:$", prompt)) "A summary." else
      sprintf("Reply %03d about evening hours and the staff.", count)
    experiments
  }
  arms <- run_focus_group_arms(
    "library hours", .sum_config(), n_participants = 2,
    arms = list(objection = fg_injection("Hours cost money.", at = fg_after_item(2),
                                         from = "Guest expert"), control = NULL),
    guide = c(Opening = 1, Engagement = 2, Closing = 1), flow = "round_robin",
    max_participant_responses = 2, verbose = FALSE, .runner = runner)
  m <- arms$metadata$prefix_messages
  stub <- .sum_runner()
  out <- summarize_focus_group(arms, .sum_config(), .runner = stub$runner)
  expect_identical(out$metadata$segments, c("prefix", "objection", "control"))
  expect_identical(unique(out$summaries$segment), c("prefix", "objection", "control"))
  arm_prompts <- stub$prompts[out$calls$segment != "prefix"]
  for (p in arm_prompts) {
    shown <- .sum_prompt_messages(p)$id
    expect_true(all(shown > m))
    expect_match(p, sprintf("continue a discussion after message #%d", m), fixed = TRUE)
  }
  expect_match(stub$prompts[out$calls$segment == "objection"][1],
               "Guest expert (injected): Hours cost money.", fixed = TRUE)
  turns <- fg_turns(arms)
  expect_identical(unique(turns$segment), c("prefix", "objection", "control"))
  expect_true(all(turns$message_id[turns$segment != "prefix"] > m))
})

# ---- Limits, templates, and the records ----------------------------------------

test_that("the budget gate fires before any call, and the session's runner is never used", {
  result <- .sum_session(n = 4)
  count <- 0L
  testthat::local_mocked_bindings(call_llm_robust = function(config, messages, ...) {
    count <<- count + 1L
    data.frame(response_text = .sum_honest_reply(messages[[1]]$content),
               finish_reason = "stop", sent = 10, rec = 5)
  }, .package = "LLMR")
  expect_error(summarize_focus_group(result, .sum_config(), max_calls = 7),
               "Estimated model calls \\(8: 4 unit\\(s\\)")
  expect_identical(count, 0L)
  out <- summarize_focus_group(result, .sum_config(), max_calls = 7, confirm = TRUE)
  expect_identical(count, 4L)
  expect_identical(out$metadata$estimated_calls, 8L)
  expect_identical(out$metadata$usage$total, 60)
  expect_error(summarize_focus_group(result), "config")
  expect_error(summarize_focus_group(result, NULL), "config")
  expect_error(summarize_focus_group(result, .sum_config(), max_calls = Inf), "max_calls")
  expect_error(summarize_focus_group(result, .sum_config(), by = "speaker"), "`by`")
  expect_error(summarize_focus_group(result, .sum_config(), compression = "short"),
               "compression")
})

test_that("a prompt longer than the window stops before any call", {
  result <- .sum_session()
  result$transcript$text[4] <- strrep("evening hours matter ", 15000)
  stub <- .sum_runner()
  err <- tryCatch(summarize_focus_group(result, .sum_config(), by = "session",
                                        .runner = stub$runner),
                  focusgroup_summary_input_too_long = function(e) e)
  expect_s3_class(err, "focusgroup_summary_input_too_long")
  expect_match(conditionMessage(err), "by = \"item\"", fixed = TRUE)
  expect_identical(stub$n, 0L)
})

test_that("the random-number state is left as it was", {
  result <- .sum_session()
  set.seed(110)
  before <- .Random.seed
  stub <- .sum_runner()
  summarize_focus_group(result, .sum_config(), by = "turn", .runner = stub$runner)
  fg_turns(result)
  expect_identical(.Random.seed, before)
})

test_that("templates are checked and rendered in one pass", {
  result <- .sum_session()
  stub <- .sum_runner()
  expect_error(summarize_focus_group(result, .sum_config(), .runner = stub$runner,
                                     templates = list(summary_participant = "Summarize {{unit}}. {{format}}")),
               "must contain {{transcript}}", fixed = TRUE)
  expect_error(summarize_focus_group(result, .sum_config(), .runner = stub$runner,
                                     templates = list(summary_reask = "Try again.")),
               "{{failures}}", fixed = TRUE)
  expect_error(summarize_focus_group(result, .sum_config(), .runner = stub$runner,
                                     templates = list(summary_other = "{{transcript}}")),
               "Unknown summary template")
  expect_error(summarize_focus_group(result, .sum_config(), .runner = stub$runner,
                                     templates = list(summary_participant =
                                       "{{transcript}} {{format}} {{audience}}")),
               "uses {{audience}}, which has no value", fixed = TRUE)
  expect_identical(stub$n, 0L)
  render <- FocusGroup:::.fg_render_template
  expect_identical(render("A {{x}} and {{y}}.", list(x = "{{y}}", y = "{{x}}")),
                   "A {{y}} and {{x}}.")
  expect_error(render("A {{ x }}.", list(x = "1")), "malformed placeholder")
  # A transcript that contains a placeholder reaches the model unchanged.
  result$transcript$text[4] <- "I typed {{topic}} and {{format}} by mistake, evenings matter."
  stub <- .sum_runner()
  custom <- paste("Topic {{topic}}. The contributions of one participant, {{unit}}.",
                  "{{transcript}}", "{{format}}")
  summarize_focus_group(result, .sum_config(), .runner = stub$runner,
                        templates = list(summary_participant = custom))
  expect_match(stub$prompts[1], "[#4] P1: I typed {{topic}} and {{format}} by mistake",
               fixed = TRUE)
  expect_match(stub$prompts[1], "^Topic library hours\\. The contributions of one participant, P1\\.")
  expect_identical(nrow(summarize_focus_group(result, .sum_config(),
                                              .runner = .sum_runner()$runner)$claims), 4L)
})

test_that("structured output is requested when LLMR offers it, and recorded", {
  result <- .sum_session()
  seen <- list()
  stub <- .sum_runner()
  runner <- function(experiments, ...) {
    seen[[length(seen) + 1L]] <<- experiments$config[[1]]
    stub$runner(experiments, ...)
  }
  out <- summarize_focus_group(result, .sum_config(), .runner = runner)
  expect_identical(out$metadata$structured_output$mode, "schema")
  expect_identical(seen[[1]]$model_params$response_format$type, "json_schema")
  schema <- out$metadata$structured_output$schema
  expect_identical(unlist(schema$properties$claims$items$properties$status$enum),
                   c("stated", "implied", "contradictory", "not_stated"))
  withr::local_options(focusgroup.summary_structured_output = FALSE)
  seen <- list()
  out <- summarize_focus_group(result, .sum_config(), .runner = runner)
  expect_identical(out$metadata$structured_output$mode, "prompt")
  expect_null(seen[[1]]$model_params$response_format)
  withr::local_options(focusgroup.summary_structured_output = NULL)
  preset <- LLMR::llm_config("openai", "example-model", response_format = list(type = "json_object"))
  out <- summarize_focus_group(result, preset, .runner = runner)
  expect_identical(out$metadata$structured_output$mode, "config")
  # Anthropic refuses a forced tool call while thinking, so the format stays
  # in the prompt, and the default limit leaves room after the budget.
  claude <- LLMR::llm_config("anthropic", "claude-example", thinking_budget = 2048)
  out <- summarize_focus_group(result, claude, .runner = runner)
  expect_identical(out$metadata$structured_output$mode, "prompt")
  expect_identical(unname(out$metadata$max_tokens[1]), 2000 + 2048)
})

test_that("the summary records its calls apart from the session", {
  result <- .sum_session()
  calls_before <- nrow(result$calls)
  records_before <- length(result$focus_group$records$calls)
  tokens_before <- result$focus_group$total_tokens
  stub <- .sum_runner()
  out <- summarize_focus_group(result, .sum_config(), reasoning_effort = "low",
                               .runner = stub$runner)
  expect_identical(length(result$focus_group$records$calls), records_before)
  expect_identical(result$focus_group$total_tokens, tokens_before)
  expect_identical(nrow(result$calls), calls_before)
  expect_identical(out$calls$call_id, 1:3)
  expect_identical(out$calls$unit, c("P1", "P2", "P3"))
  expect_identical(out$calls$segment, rep("session", 3L))
  expect_identical(out$calls$settings[[1]]$reasoning_effort, "low")
  expect_false(any(grepl("api_key", names(unlist(out$calls$settings)))))
  meta <- out$metadata
  expect_identical(meta$compression, "brief")
  expect_true(all(c("summary_participant", "summary_reask") %in% names(meta$templates)))
  expect_true(is.character(meta$source_hash) && nzchar(meta$source_hash))
  expect_s3_class(meta$created, "POSIXct")
  expect_identical(as.data.frame(out), out$summaries)
  expect_identical(as.data.frame(out, component = "quotes"), out$quotes)
  printed <- capture.output(print(out))
  expect_match(printed[1], "<focus_group_summary>", fixed = TRUE)
  expect_true(any(grepl("^P1: P1 spoke about the hours", printed)))
})
