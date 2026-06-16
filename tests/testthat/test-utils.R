# Pure core helpers that the rest of the package leans on: placeholder
# substitution, the default prompt templates, and the default LLM config.
# These run offline, with no live calls.

test_that("replace_placeholders substitutes known keys and blanks the rest", {
  out <- replace_placeholders("Hi {{name}}, topic is {{topic}}.",
                              list(name = "Ada", topic = "buses"))
  expect_equal(out, "Hi Ada, topic is buses.")

  # an unreferenced placeholder becomes empty, not left in place
  expect_equal(replace_placeholders("a {{x}} b", list()), "a  b")
  # a NULL value is treated as missing -> empty
  expect_equal(replace_placeholders("a {{x}} b", list(x = NULL)), "a  b")
  # a repeated placeholder is replaced everywhere
  expect_equal(replace_placeholders("{{w}}-{{w}}", list(w = "go")), "go-go")
  # non-character values are coerced
  expect_equal(replace_placeholders("n={{k}}", list(k = 3L)), "n=3")
})

test_that("replace_placeholders validates its inputs", {
  expect_error(replace_placeholders(NULL, list()), "single character")
  expect_error(replace_placeholders(c("a", "b"), list()), "single character")
  expect_error(replace_placeholders("x", "not a list"), "must be a list")
})

test_that("replace_placeholders_known preserves unknown tokens", {
  # only the supplied key is filled; the unsupplied {{keep}} stays verbatim
  out <- replace_placeholders_known("{{fill}} but {{keep}}", list(fill = "done"))
  expect_equal(out, "done but {{keep}}")
})

test_that("get_default_prompt_templates returns the expected named templates", {
  tmpl <- get_default_prompt_templates()
  expect_type(tmpl, "list")
  expect_true(all(c("participant_utterance_subtle_persona",
                    "participant_desire_to_talk_nuanced") %in% names(tmpl)))
  # the utterance template carries the placeholders the renderer relies on
  expect_match(tmpl$participant_utterance_subtle_persona, "{{persona_description}}",
               fixed = TRUE)
  expect_match(tmpl$participant_utterance_subtle_persona, "{{topic}}", fixed = TRUE)
})

test_that("extract_token_counts never returns NA when usage is unreported", {
  # A provider that reports no usage yields NA token columns; the counter must
  # coerce to 0 so callers' if((sent+rec)>0) guards do not hit a missing value.
  u <- extract_token_counts(list(sent_tokens = NA_integer_, rec_tokens = NA_integer_))
  expect_equal(u$sent, 0L)
  expect_equal(u$rec, 0L)
  expect_false(is.na(u$sent + u$rec))
  expect_no_error(if ((u$sent + u$rec) > 0) NULL)
  # a frame with a mix of NA and real counts sums the reals, NAs as 0
  u2 <- extract_token_counts(data.frame(sent_tokens = c(NA, 5L),
                                        rec_tokens = c(NA, 2L)))
  expect_false(is.na(u2$sent))
})

test_that("default_llmr_config honors focusgroup.* options", {
  skip_if_not_installed("LLMR")
  withr::local_options(focusgroup.provider = "groq",
                       focusgroup.model = "test-model",
                       focusgroup.temperature = 0.2)
  cfg <- default_llmr_config()
  expect_equal(cfg$provider, "groq")
  expect_equal(cfg$model, "test-model")
})
