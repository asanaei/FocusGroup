# Role-flipped message construction (Phase 3). Offline; the role-flip path needs
# LLMR's transcript_as_messages(), so guard on the LLMR version.

skip_if_old_llmr <- function() {
  testthat::skip_if_not_installed("LLMR")
  if (utils::packageVersion("LLMR") < "0.8.7")
    testthat::skip("needs LLMR >= 0.8.7 for transcript_as_messages()")
}

roles_of <- function(m) vapply(m, function(x) x$role, character(1))
content_of <- function(m) vapply(m, function(x) as.character(x$content %||% ""), character(1))

mk_log <- function() list(
  list(speaker_id = "System", text = "roster"),
  list(speaker_id = "Mod",   text = "What about gas stoves?", is_moderator = TRUE),
  list(speaker_id = "Maya",  text = "Costs hit families hardest."),
  list(speaker_id = "Devin", text = "Regulation has downsides.")
)

test_that(".fg_log_to_transcript drops the System row and yields speaker/text", {
  tx <- .fg_log_to_transcript(mk_log())
  expect_identical(names(tx), c("speaker", "text"))
  expect_false("System" %in% tx$speaker)
  expect_identical(nrow(tx), 3L)
})

test_that("role-flip: own turns are assistant (unprefixed), others are labeled user", {
  skip_if_old_llmr()
  tx <- .fg_log_to_transcript(mk_log())
  m  <- .fg_build_agent_messages(tx, "Maya", "You are Maya.",
                                 instruction = "Your turn.")
  r <- roles_of(m); ct <- content_of(m)
  # provider-safe: system first, then alternating, first non-system is user
  expect_identical(r[1], "system")
  nonsys <- which(r != "system")
  expect_identical(r[nonsys[1]], "user")
  expect_false(any(r[-1] == r[-length(r)]))
  # Maya's own turn -> assistant, no "Maya:" prefix
  own <- ct[r == "assistant"]
  expect_true(any(grepl("families", own)))
  expect_false(any(grepl("^Maya:", own)))
  # the moderator + Devin are labeled inside a user turn
  expect_true(any(grepl("Mod", ct[r == "user"])))
})

test_that("moderator role-flips with its own prior turns as assistant", {
  skip_if_old_llmr()
  log <- list(
    list(speaker_id = "Mod",  text = "Welcome.", is_moderator = TRUE),
    list(speaker_id = "Maya", text = "Hi."),
    list(speaker_id = "Mod",  text = "Tell me more.", is_moderator = TRUE)
  )
  tx <- .fg_log_to_transcript(log)
  m  <- .fg_build_agent_messages(tx, "Mod", "You are the moderator.",
                                 instruction = "Pose your question.")
  r <- roles_of(m); ct <- content_of(m)
  # the moderator's own turns are assistant; Maya is user
  expect_true(any(r == "assistant" & grepl("Welcome|Tell me more", ct)))
  expect_true(any(r == "user" & grepl("Maya:", ct)))
})

test_that("flat mode reproduces the single-user transcript", {
  tx <- .fg_log_to_transcript(mk_log())
  m  <- .fg_build_agent_messages(tx, "Maya", "You are Maya.",
                                 instruction = "Your turn.", mode = "flat")
  expect_identical(roles_of(m), c("system", "user"))
  body <- content_of(m)[2]
  expect_true(grepl("Maya: Costs", body))
  expect_true(grepl("Devin: Regulation", body))
})

test_that("empty transcript yields system -> instruction", {
  skip_if_old_llmr()
  tx <- .fg_log_to_transcript(list())
  m  <- .fg_build_agent_messages(tx, "Maya", "You are Maya.",
                                 instruction = "Open the discussion.")
  expect_identical(roles_of(m), c("system", "user"))
})

test_that("self-state digest summarizes the agent's own prior turns only", {
  tx <- .fg_log_to_transcript(mk_log())
  ss <- .fg_self_state(tx, "Maya")
  expect_true(grepl("already made", ss))
  expect_true(grepl("families", ss))
  expect_false(grepl("Regulation", ss))   # Devin's point, not Maya's
  expect_identical(.fg_self_state(tx, "Nobody"), "")  # never spoke -> ""
})
