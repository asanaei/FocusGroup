# Rendering of survey answers into persona text, and the rendering report.
#
# Every requested field and item of every persona is rendered by one of the
# rules below and recorded in a report, so that nothing a respondent said is
# left out or replaced without a record. `.fg_render_column()` applies the
# rules to a whole column; the constructors keep the rows they draw.

# One row of a data frame as a named list of its non-missing values, as
# character. drop = FALSE matters: a single-column frame would otherwise
# collapse to a vector and lose the column names.
.fg_row_fields <- function(df, i) {
  row <- df[i, , drop = FALSE]
  out <- list()
  for (col in names(row)) {
    v <- row[[col]]
    if (length(v) && !is.na(v) && nzchar(v) && !identical(as.character(v), "NA")) {
      out[[col]] <- .fg_value_text(v)
    }
  }
  out
}

# The fields a persona states: a one-row data frame or a list normalized to a
# named list, without missing or empty fields.
.fg_persona_fields <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) x <- as.list(x[1, , drop = FALSE])
  if (!is.list(x)) x <- as.list(x)
  x <- lapply(x, function(v) if (length(v)) v[[1]] else NA)
  keep <- vapply(x, function(v) {
    !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(as.character(v)))
  }, logical(1))
  x[keep]
}

# The record supplied for participant `i` to create_agents(): a row of a data
# frame, with every column, or the i-th element of a list of records.
.fg_supplied_record <- function(x, i) {
  if (is.data.frame(x) && nrow(x) > 0) {
    return(as.list(x[((i - 1L) %% nrow(x)) + 1L, , drop = FALSE]))
  }
  if (!is.null(x) && !is.data.frame(x) && is.list(x) && length(x) >= i) {
    return(as.list(x[[i]]))
  }
  list()
}

# Report rows for one participant whose persona was rendered from a supplied
# record: every named field of the record, its value, and the text the persona
# shows for it. `fields` is what the persona was rendered from. A persona
# states one value per field: a field holding several values shows the first,
# and a field whose first element is not a single value is not shown.
.fg_supplied_rows <- function(id, kind, record, fields) {
  variables <- names(record) %||% character()
  named <- !is.na(variables) & nzchar(variables)
  if (!any(named)) return(NULL)
  record <- record[named]
  variables <- variables[named]
  shown <- .fg_persona_fields(fields)
  first <- lapply(record, function(v) if (length(v)) v[[1]])
  single <- vapply(first, function(v) length(v) == 1L, logical(1))
  value <- vapply(seq_along(first), function(j) {
    if (single[j]) .fg_value_text(first[[j]]) else NA_character_
  }, character(1))
  text <- vapply(variables, function(v) {
    if (v %in% names(shown)) .fg_tidy_value(shown[[v]]) else NA_character_
  }, character(1), USE.NAMES = FALSE)
  several <- lengths(record) > 1L & !is.na(text)
  reason <- ifelse(!is.na(text), NA_character_,
                   ifelse(single | lengths(first) == 0L, "no value",
                          "the field does not hold a single value"))
  reason[several] <- sprintf("the field holds %d values; the persona shows the first",
                             lengths(record)[several])
  data.frame(
    agent_id = id, kind = kind, variable = variables, question = variables,
    question_source = "variable", code = value, text = text,
    rendered_as = ifelse(is.na(text), NA_character_, "value"),
    status = ifelse(is.na(text), "dropped", ifelse(several, "fallback", "rendered")),
    reason = reason, template = NA_character_,
    issue = ifelse(is.na(text), "missing", NA_character_), flag = value,
    stringsAsFactors = FALSE
  )
}

.fg_report_columns <- c("agent_id", "kind", "variable", "question",
                        "question_source", "code", "text", "rendered_as",
                        "status", "reason", "template", "issue", "flag")

.fg_bind_report <- function(pieces) {
  pieces <- Filter(Negate(is.null), pieces)
  out <- if (length(pieces)) do.call(rbind, pieces) else NULL
  if (is.null(out)) {
    out <- as.data.frame(stats::setNames(
      rep(list(character()), length(.fg_report_columns)), .fg_report_columns),
      stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}

# Strip a code prefix that labeled surveys carry on a value label ("1. Male")
# when whitespace follows the period, so a decimal value ("1.5") is kept. A
# trailing parenthetical is substantive ("Asian (non-Hispanic)") and kept.
.fg_tidy_value <- function(x) {
  x <- .fg_value_text(x)
  x <- gsub("^[0-9]+\\.\\s+", "", x)
  trimws(x)
}

# A value as text, with numbers written out in full (100000, not 1e+05).
.fg_value_text <- function(x) {
  if (is.numeric(x) && !is.factor(x) && !inherits(x, "haven_labelled")) {
    return(.fg_number_text(as.vector(x)))
  }
  as.character(x)
}

# Strip any leading code, negative codes included, from a label used inside a
# sentence (the endpoints of a scale).
.fg_label_body <- function(x) {
  trimws(gsub("^\\s*-?[0-9]+\\.\\s+", "", as.character(x)))
}

.fg_number_text <- function(x) {
  out <- as.character(x)
  shown <- !is.na(x) & is.finite(x)
  out[shown] <- vapply(x[shown], function(v) {
    format(v, scientific = FALSE, digits = 15, trim = TRUE)
  }, character(1))
  out
}

.fg_is_missing_label <- function(x, na_strings) {
  if (!length(na_strings)) return(rep(FALSE, length(x)))
  grepl(paste(na_strings, collapse = "|"), x, ignore.case = TRUE)
}

# Whether values that carry no value label, such as the text of a decoded
# frame, are missing-data labels: they match `na_strings` and read as a
# label, which means they start with a numeric code ("-9. Refused") or have
# at most six words. Longer text that mentions such words, as an open-ended
# answer may ("My landlord refused to fix the heat all winter"), is an
# answer.
.fg_is_missing_value <- function(x, na_strings) {
  x <- as.character(x)
  coded <- grepl("^\\s*-?[0-9]+\\.\\s", x)
  short <- lengths(strsplit(trimws(x), "\\s+")) <= 6L
  .fg_is_missing_label(x, na_strings) & (coded | short)
}

# Validate `na_strings`: NULL or a character vector of non-empty patterns. An
# empty pattern would match every value and leave every answer out.
.fg_check_na_strings <- function(na_strings) {
  if (is.null(na_strings)) return(character())
  if (!is.character(na_strings) || anyNA(na_strings) || any(!nzchar(na_strings))) {
    stop("`na_strings` must be a character vector of non-empty patterns.", call. = FALSE)
  }
  na_strings
}

# SPSS user-defined missing values, which haven keeps as values when a file
# is read with `user_na = TRUE`, are left out of `out`, the rendering of the
# labelled column `col`, as they are when the file is read without them.
.fg_user_missing <- function(out, col) {
  if (!inherits(col, "haven_labelled_spss")) return(out)
  values <- as.vector(unclass(col))
  user <- !is.na(values) & is.na(col)
  if (!any(user)) return(out)
  labels <- attr(col, "labels", exact = TRUE)
  label <- names(labels)[match(values[user], unname(labels))]
  shown <- ifelse(is.na(label), out$code[user], label)
  out$text[user] <- NA_character_
  out$rendered_as[user] <- NA_character_
  out$status[user] <- "dropped"
  out$template[user] <- NA_character_
  out$reason[user] <- paste("missing value:", shown, "(user-defined missing value)")
  out$issue[user] <- "missing"
  out$flag[user] <- out$code[user]
  out
}

.fg_no_value <- function(x) {
  is.na(x) | !nzchar(trimws(x)) | x == "NA"
}

# A top or bottom code states a bound on a number: "80. Eighty years old or
# older", "Six or more", "Under $5,000", "90+". Such labels mark a variable
# that holds numbers, not the endpoints of an attitude scale.
.fg_bound_label <- function(x) {
  x <- tolower(.fg_label_body(x))
  comparative <- grepl(paste0(
    "\\b(or|and) (more|older|over|above|higher|greater|less|fewer|younger|",
    "under|below)\\b|^(under|less than|fewer than|more than|over|at least|",
    "at most|up to|below|above)\\b|\\+$"), x)
  number <- grepl(paste0(
    "[0-9]|\\b(one|two|three|four|five|six|seven|eight|nine|ten|eleven|",
    "twelve|thirteen|fourteen|fifteen|sixteen|seventeen|eighteen|nineteen|",
    "twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety|hundred|thousand|",
    "million)\\b"), x)
  comparative & number
}

# The two labeled endpoints of the scale on which the unlabeled number `x`
# lies, or NULL. `codes` and `labels` are the substantive (non-missing) labels.
# The endpoints are the nearest labeled codes on either side of `x`; no other
# substantive label may lie below the lower endpoint, except negative codes
# below a scale that starts at zero or above (missing-data codes such as
# "-4. Error"); labels above the upper endpoint must lie more than the width of
# the scale beyond it ("99. Haven't thought much about this" after a 1 to 7
# scale), so that a labeled midpoint (0, 5, 10) is not taken for the end of a
# shorter scale. A scale with a negative lower endpoint must be centered on
# zero.
.fg_scale_endpoints <- function(x, codes, labels) {
  below <- codes[codes < x]
  above <- codes[codes > x]
  if (!length(below) || !length(above)) return(NULL)
  lo <- max(below)
  hi <- min(above)
  if (lo < 0 && lo != -hi) return(NULL)
  under <- codes[codes < lo]
  if (length(under) && (lo < 0 || any(under >= 0))) return(NULL)
  beyond <- codes[codes > hi]
  if (any(beyond <= hi + (hi - lo))) return(NULL)
  lo_label <- labels[match(lo, codes)]
  hi_label <- labels[match(hi, codes)]
  if (.fg_bound_label(lo_label) || .fg_bound_label(hi_label)) return(NULL)
  list(lo = lo, hi = hi, lo_label = lo_label, hi_label = hi_label)
}

.fg_default_scale_template <- paste(
  "On a scale of {lo} to {hi}, where {lo} is {lo_label} and {hi} is {hi_label},",
  "you place yourself at {value}.")

.fg_scale_placeholders <- c("lo", "hi", "lo_label", "hi_label", "value")

.fg_scale_text <- function(x, ends, template = .fg_default_scale_template) {
  values <- c(lo = .fg_number_text(ends$lo), hi = .fg_number_text(ends$hi),
              lo_label = .fg_label_body(ends$lo_label),
              hi_label = .fg_label_body(ends$hi_label),
              value = .fg_number_text(x))
  # regmatches<- inserts the labels literally; gsub() would read a backslash
  # in a label as an escape.
  for (name in names(values)) {
    at <- gregexpr(paste0("{", name, "}"), template, fixed = TRUE)
    if (at[[1]][1] > 0) {
      regmatches(template, at) <- list(rep(values[[name]], length(at[[1]])))
    }
  }
  template
}

# Validate `scale_template`: one template, or templates named by variable with
# at most one unnamed default. Every template must show the answer ({value})
# and use no placeholder other than those `.fg_scale_text()` fills.
.fg_check_template <- function(scale_template) {
  ok <- is.character(scale_template) && length(scale_template) > 0L &&
    !anyNA(scale_template) && all(nzchar(trimws(scale_template)))
  given <- names(scale_template) %||% rep("", length(scale_template))
  ok <- ok && !anyNA(given) && sum(!nzchar(given)) <= 1L &&
    !anyDuplicated(given[nzchar(given)]) &&
    (length(scale_template) == 1L || !is.null(names(scale_template)))
  if (!ok) {
    stop(paste("`scale_template` must be one template, or templates named by",
               "variable with at most one unnamed default, with no missing,",
               "empty, or repeated entries."), call. = FALSE)
  }
  used <- regmatches(scale_template, gregexpr("\\{[^{}]*\\}", scale_template))
  unknown <- setdiff(gsub("[{}]", "", unlist(used)), .fg_scale_placeholders)
  if (length(unknown)) {
    stop(sprintf("`scale_template` uses unknown placeholder(s) %s; the placeholders are %s.",
                 paste0("{", unknown, "}", collapse = ", "),
                 paste0("{", .fg_scale_placeholders, "}", collapse = ", ")), call. = FALSE)
  }
  if (!all(grepl("{value}", scale_template, fixed = TRUE))) {
    stop("Every `scale_template` must contain {value}, the respondent's answer.",
         call. = FALSE)
  }
  names(scale_template) <- given
  scale_template
}

# The template for one variable: its own, else the unnamed default given,
# else the package default.
.fg_template_for <- function(scale_template, variable) {
  given <- names(scale_template)
  if (variable %in% given) return(scale_template[[match(variable, given)]])
  if ("" %in% given) return(scale_template[[match("", given)]])
  .fg_default_scale_template
}

# The value labels of one column as a table (code, label, supplied), with the
# file's labels replaced or extended by `supplied`, the rows of the normalized
# `value_labels` for this variable. Labels of tagged missing values are kept
# apart in the `tagged` attribute.
.fg_label_table <- function(col, variable, supplied = NULL) {
  numeric_codes <- typeof(col) %in% c("double", "integer") && !is.factor(col)
  labels <- attr(col, "labels", exact = TRUE)
  tagged <- NULL
  if (length(labels) && numeric_codes) {
    if (is.double(labels)) {
      tag <- haven::na_tag(labels)
      tagged <- stats::setNames(names(labels)[!is.na(tag)], tag[!is.na(tag)])
    }
    labels <- labels[!is.na(labels)]
  }
  tab <- data.frame(
    code = if (numeric_codes) as.double(unname(labels)) else as.character(unname(labels)),
    label = as.character(names(labels) %||% character()),
    supplied = rep(FALSE, length(labels)),
    stringsAsFactors = FALSE
  )
  if (!is.null(supplied) && nrow(supplied)) {
    codes <- supplied$code
    if (numeric_codes) {
      codes <- suppressWarnings(as.numeric(codes))
      bad <- supplied$code[is.na(codes)]
      if (length(bad)) {
        stop(sprintf("`value_labels` gives code(s) %s for %s, which stores numbers.",
                     paste0("'", bad, "'", collapse = ", "), variable), call. = FALSE)
      }
    }
    hit <- match(codes, tab$code)
    tab$label[hit[!is.na(hit)]] <- supplied$label[!is.na(hit)]
    tab$supplied[hit[!is.na(hit)]] <- TRUE
    if (any(is.na(hit))) {
      tab <- rbind(tab, data.frame(code = codes[is.na(hit)],
                                   label = supplied$label[is.na(hit)],
                                   supplied = TRUE, stringsAsFactors = FALSE))
    }
  }
  attr(tab, "tagged") <- tagged
  tab
}

.fg_rendering <- function(code, text = NA_character_, rendered_as = NA_character_,
                          status = "dropped", reason = NA_character_,
                          template = NA_character_, issue = NA_character_,
                          flag = code) {
  list(code = code, text = text, rendered_as = rendered_as, status = status,
       reason = reason, template = template, issue = issue, flag = flag)
}

# Render every value of one survey column. Returns a data frame with one row
# per value: the stored `code` as text, the answer `text` passed to the
# persona (NA when left out), how it was rendered (`rendered_as`), its
# `status` ("rendered", "fallback", or "dropped"), and the `reason` for a
# fallback or a dropped answer, and the `template` of a scale answer. Two
# further columns drive the warnings and are
# not part of the report: `issue` is "missing" for answers left out as missing
# values, "negative" for unlabeled negative codes taken as missing,
# "negative_label" for labels of negative codes shown as answers, "code" for
# unlabeled codes shown as codes, and "long" for labels that may have been
# cut; `flag` is the code a warning names, which for a scale answer is the
# endpoint whose label may have been cut.
.fg_render_column <- function(col, variable, na_strings, supplied = NULL,
                              template = .fg_default_scale_template) {
  tab <- .fg_label_table(col, variable, supplied)
  if (!nrow(tab) && !length(attr(tab, "tagged"))) return(.fg_render_plain(col, na_strings))

  numeric_codes <- is.numeric(tab$code)
  values <- as.vector(unclass(col))
  missing_label <- .fg_is_missing_label(tab$label, na_strings)
  substantive <- tab[!missing_label, , drop = FALSE]
  tagged <- attr(tab, "tagged")

  render_one <- function(x) {
    code <- if (numeric_codes) .fg_number_text(x) else as.character(x)
    i <- match(x, tab$code)
    if (!is.na(i)) {
      label <- tab$label[i]
      if (missing_label[i]) {
        return(.fg_rendering(code, reason = paste("missing value:", label),
                             issue = "missing"))
      }
      if (.fg_no_value(.fg_tidy_value(label))) {
        return(.fg_rendering(code, reason = "the value label is empty",
                             issue = "missing"))
      }
      if (!tab$supplied[i] && numeric_codes && x < 0) {
        return(.fg_rendering(code, label, "label", "fallback", sprintf(paste(
          "code %s is negative, as missing-data codes are in some releases,",
          "and its label does not match `na_strings`"), code),
          issue = "negative_label"))
      }
      if (!tab$supplied[i] && nchar(label) == 80L) {
        return(.fg_rendering(code, label, "label", "fallback", sprintf(
          "the label of code %s is 80 characters long and may have been cut", code),
          issue = "long"))
      }
      return(.fg_rendering(code, label,
                           if (tab$supplied[i]) "supplied_label" else "label",
                           "rendered"))
    }
    unlabeled <- sprintf("code %s has no value label", code)
    if (!numeric_codes) {
      if (!nrow(substantive)) return(.fg_rendering(code, code, "value", "rendered"))
      return(.fg_rendering(code, code, "code", "fallback", unlabeled, issue = "code"))
    }
    codes <- substantive$code
    labels <- substantive$label
    ends <- .fg_scale_endpoints(x, codes, labels)
    if (!is.null(ends)) {
      at <- match(c(ends$lo, ends$hi), codes)
      long <- codes[at][!substantive$supplied[at] & nchar(labels[at]) == 80L]
      text <- .fg_scale_text(x, ends, template)
      if (length(long)) {
        long <- .fg_number_text(long)
        return(.fg_rendering(code, text, "scale", "fallback", sprintf(
          "the label of scale endpoint %s is 80 characters long and may have been cut",
          paste(long, collapse = " and ")), template = template, issue = "long",
          flag = paste(long, collapse = " and ")))
      }
      return(.fg_rendering(code, text, "scale", "rendered", template = template))
    }
    if (x < 0) {
      return(.fg_rendering(code, reason = sprintf(paste(
        "code %s is negative and has no value label;",
        "taken as a missing-data code"), code), issue = "negative"))
    }
    held <- codes >= 0
    if (sum(held) <= 1L || all(.fg_bound_label(labels[held]))) {
      return(.fg_rendering(code, code, "value", "rendered"))
    }
    .fg_rendering(code, code, "code", "fallback", unlabeled, issue = "code")
  }

  out <- .fg_dropped_rows(length(values))
  absent <- is.na(values)
  if (is.double(values) && any(absent) && length(tagged)) {
    tag <- haven::na_tag(values)
    named <- absent & !is.na(tag) & tag %in% names(tagged)
    out$code[named] <- paste0("NA(", tag[named], ")")
    out$reason[named] <- paste("missing value:", tagged[tag[named]])
  }
  if (!numeric_codes) absent <- absent | !nzchar(trimws(values))
  present <- unique(values[!absent])
  if (length(present)) {
    rendered <- lapply(present, render_one)
    at <- match(values[!absent], present)
    for (field in names(out)) {
      column <- vapply(rendered, function(r) as.character(r[[field]]), character(1))
      out[[field]][!absent] <- column[at]
    }
  }
  out
}

# `n` rows of answers left out for want of a value.
.fg_dropped_rows <- function(n, reason = "no value", issue = "missing") {
  none <- rep(NA_character_, n)
  data.frame(code = none, text = none, rendered_as = none,
             status = rep("dropped", n), reason = rep(reason, n),
             template = none, issue = rep(issue, n), flag = none,
             stringsAsFactors = FALSE)
}

# A column without value labels: the stored values, with numbers written out
# in full and `na_strings` applied to the values that read as labels.
.fg_render_plain <- function(col, na_strings) {
  values <- .fg_value_text(col)
  n <- length(values)
  out <- data.frame(code = values, text = values, rendered_as = rep("value", n),
                    status = rep("rendered", n), reason = rep(NA_character_, n),
                    template = rep(NA_character_, n), issue = rep(NA_character_, n),
                    flag = values, stringsAsFactors = FALSE)
  none <- .fg_no_value(values)
  flagged <- !none & .fg_is_missing_value(values, na_strings)
  out$text[none | flagged] <- NA_character_
  out$rendered_as[none | flagged] <- NA_character_
  out$status[none | flagged] <- "dropped"
  out$issue[none | flagged] <- "missing"
  out$reason[none] <- "no value"
  out$reason[flagged] <- paste("missing value:", values[flagged])
  out
}

.fg_check_wording <- function(question_wording) {
  if (is.null(question_wording)) return(NULL)
  ok <- is.character(question_wording) && length(question_wording) &&
    !is.null(names(question_wording)) && !anyNA(names(question_wording)) &&
    all(nzchar(names(question_wording))) && !anyDuplicated(names(question_wording)) &&
    !anyNA(question_wording) && all(nzchar(trimws(question_wording)))
  if (!ok) {
    stop(paste("`question_wording` must be a character vector of question texts",
               "named by variable, with no missing, empty, or repeated entries."),
         call. = FALSE)
  }
  question_wording
}

# Normalize `value_labels` to a data frame with character columns variable,
# code, and label. Accepted forms: a data frame with those columns, or a named
# list of named vectors in the form of haven value labels (label = code).
.fg_check_value_labels <- function(value_labels) {
  if (is.null(value_labels)) return(NULL)
  as_code <- function(x) {
    if (is.factor(x)) return(as.character(x))
    if (is.numeric(x)) return(.fg_number_text(as.vector(unclass(x))))
    as.character(x)
  }
  if (is.data.frame(value_labels)) {
    absent <- setdiff(c("variable", "code", "label"), names(value_labels))
    if (length(absent)) {
      stop("`value_labels` must have the columns variable, code, and label; missing: ",
           paste(absent, collapse = ", "), ".", call. = FALSE)
    }
    out <- data.frame(variable = as.character(value_labels$variable),
                      code = as_code(value_labels$code),
                      label = as.character(value_labels$label),
                      stringsAsFactors = FALSE)
  } else if (is.list(value_labels) && length(value_labels) &&
             !is.null(names(value_labels)) && !anyNA(names(value_labels)) &&
             all(nzchar(names(value_labels)))) {
    pieces <- lapply(names(value_labels), function(v) {
      x <- value_labels[[v]]
      if (!is.atomic(x) || !length(x) || is.null(names(x))) {
        stop(sprintf(paste("`value_labels$%s` must be a named vector in the form",
                           "of haven value labels, c(\"Full label\" = 1)."), v),
             call. = FALSE)
      }
      data.frame(variable = v, code = as_code(unname(x)), label = names(x),
                 stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, pieces)
  } else {
    stop(paste("`value_labels` must be a data frame with columns variable, code,",
               "and label, or a named list of named vectors (label = code)."),
         call. = FALSE)
  }
  bad <- which(is.na(out$variable) | !nzchar(out$variable) | is.na(out$code) |
                 is.na(out$label) | !nzchar(trimws(out$label)))
  if (length(bad)) {
    stop("`value_labels` has a missing or empty variable, code, or label in row(s) ",
         paste(utils::head(bad, 6L), collapse = ", "), ".", call. = FALSE)
  }
  dup <- duplicated(out[c("variable", "code")])
  if (any(dup)) {
    stop("`value_labels` gives more than one label for ",
         paste(sprintf("%s code %s", out$variable[dup], out$code[dup]), collapse = ", "),
         ".", call. = FALSE)
  }
  out
}

# Make the keys shown for one kind of field distinct. Variables that would be
# shown under the same question would overwrite one another in the persona, so
# each is told apart by its variable name, with a warning.
.fg_distinct_keys <- function(keys, variables, kind) {
  shared <- keys %in% keys[duplicated(keys)]
  if (any(shared)) {
    warning(sprintf(paste(
      "These %s share the text shown for them, so each is shown with its",
      "variable name appended: %s."), kind,
      paste(sprintf("%s (\"%s\")", variables[shared], keys[shared]), collapse = ", ")),
      call. = FALSE)
    keys[shared] <- sprintf("%s [%s]", keys[shared], variables[shared])
  }
  keys
}

.fg_list_items <- function(x, n = 10L) {
  x <- unique(x)
  shown <- paste(utils::head(x, n), collapse = "; ")
  if (length(x) > n) shown <- sprintf("%s; and %d more", shown, length(x) - n)
  shown
}

# Assemble the report for the drawn personas. `fields` is a data frame with
# one row per requested field or item (kind, variable, question,
# question_source); `rendered` is a list of `.fg_render_column()` results in
# the same order; `rows` gives the row of each participant in those results.
.fg_rendering_report <- function(ids, rows, fields, rendered) {
  if (!nrow(fields)) return(.fg_bind_report(list()))
  pieces <- lapply(seq_along(ids), function(a) {
    r <- rows[[a]]
    pick <- function(field) {
      vapply(rendered, function(x) x[[field]][r], character(1), USE.NAMES = FALSE)
    }
    text <- .fg_tidy_value(pick("text"))
    status <- pick("status")
    text[status == "dropped"] <- NA_character_
    data.frame(
      agent_id = ids[[a]], kind = fields$kind, variable = fields$variable,
      question = fields$question, question_source = fields$question_source,
      code = pick("code"), text = text, rendered_as = pick("rendered_as"),
      status = status, reason = pick("reason"), template = pick("template"),
      issue = pick("issue"), flag = pick("flag"),
      stringsAsFactors = FALSE
    )
  })
  .fg_bind_report(pieces)
}

# Confirm the report against each agent's persona text. Every answer recorded
# as shown must appear as a whole field under the question recorded for it,
# bounded by the persona's own delimiters, and no field recorded as dropped
# may appear. The delimiters are those of generate_persona(): demographics
# read "Background: q: v; q: v." (labeled style) or "Your background: ..."
# (paragraph style); items read "Question: q\n  Answer: v\n" or
# "said: q -- v; q -- v.". A mismatch is an error in the package, and the
# report would otherwise misdescribe the persona.
.fg_check_report <- function(report, agents) {
  if (!nrow(report)) return(invisible(report))
  heads <- function(kind, q) {
    if (identical(kind, "demographic")) {
      paste0(c("Background: ", "background: ", "; "), q, ": ")
    } else {
      c(paste0("Question: ", q, "\n  Answer: "), paste0(c("said: ", "; "), q, " -- "))
    }
  }
  fields <- function(kind, q, v) {
    if (identical(kind, "demographic")) {
      as.vector(outer(heads(kind, q), paste0(v, c(";", ".")), paste0))
    } else {
      c(paste0("Question: ", q, "\n  Answer: ", v, "\n"),
        as.vector(outer(paste0(c("said: ", "; "), q, " -- "), paste0(v, c(";", ".")), paste0)))
    }
  }
  has_any <- function(persona, needles) {
    any(vapply(needles, grepl, logical(1), x = persona, fixed = TRUE))
  }
  ok <- vapply(seq_len(nrow(report)), function(i) {
    persona <- agents[[report$agent_id[i]]]$persona_description
    if (identical(report$status[i], "dropped")) {
      !has_any(persona, heads(report$kind[i], report$question[i]))
    } else {
      has_any(persona, fields(report$kind[i], report$question[i], report$text[i]))
    }
  }, logical(1))
  if (!all(ok)) {
    bad <- report[!ok, , drop = FALSE]
    stop("The rendering report does not match the persona text for ",
         paste(sprintf("%s %s", bad$agent_id, bad$variable), collapse = ", "),
         ".", call. = FALSE)
  }
  invisible(report)
}

# Warn about the drawn answers that were replaced, flagged, or inferred to be
# missing, naming the variable, the code, and the participants. Answers left
# out as missing values are announced in a message. `source` says where the
# labels come from: a survey file, whose labels `value_labels` replaces, or
# the labelled columns of a data frame, whose labels are changed in the data.
.fg_rendering_conditions <- function(report, source = c("file", "data")) {
  source <- match.arg(source)
  holder <- if (source == "file") "the file" else "the column"
  supply <- function(what) {
    if (source == "file") sprintf("Supply %s in `value_labels`", what) else
      sprintf("Add %s to the column's value labels", what)
  }
  describe <- function(rows) {
    if (!nrow(rows)) return(character())
    key <- ifelse(rows$rendered_as %in% "scale",
                  paste(rows$variable, "scale endpoint", rows$flag),
                  paste(rows$variable, "code", rows$flag))
    agents <- tapply(rows$agent_id, factor(key, levels = unique(key)),
                     function(x) paste(unique(x), collapse = ", "))
    sprintf("%s (%s)", names(agents), unname(agents))
  }
  issue <- report$issue
  code <- describe(report[issue %in% "code", , drop = FALSE])
  if (length(code)) {
    warning("Shown as the bare code because ", holder, " has no value label for it: ",
            .fg_list_items(code), ". ", supply("labels"), ".", call. = FALSE)
  }
  long <- describe(report[issue %in% "long", , drop = FALSE])
  if (length(long)) {
    warning("Labels exactly 80 characters long may have been cut short: ",
            .fg_list_items(long), ". ", supply("the full labels"), ".",
            call. = FALSE)
  }
  negative_label <- describe(report[issue %in% "negative_label", , drop = FALSE])
  if (length(negative_label)) {
    warning(paste(
      "Labels of negative codes are shown as answers, though some releases,",
      "the ANES and WVS among them, use negative codes for missing data: "),
      .fg_list_items(negative_label),
      ". Add their wording to `na_strings` to leave them out",
      if (source == "file") ", or supply the labels in `value_labels` to keep them." else
        "; otherwise they stay as shown.", call. = FALSE)
  }
  negative <- describe(report[issue %in% "negative", , drop = FALSE])
  if (length(negative)) {
    warning("Negative codes without a value label were taken as missing and left out: ",
            .fg_list_items(negative), ". ",
            if (source == "file") "Label them in `value_labels`" else
              "Add their labels to the column's value labels",
            " if they are answers.", call. = FALSE)
  }
  missing <- report[issue %in% "missing", , drop = FALSE]
  if (nrow(missing)) {
    message(sprintf(paste(
      "%d answer(s) were left out of the personas as missing values (%s);",
      "persona_rendering_report() lists them."),
      nrow(missing), .fg_list_items(missing$variable)))
  }
  invisible(report)
}

.fg_question_conditions <- function(fields) {
  unworded <- fields[fields$kind == "item" &
                       fields$question_source %in% c("label", "name", "variable"), , drop = FALSE]
  if (!nrow(unworded)) return(invisible(NULL))
  shown <- sprintf("%s (\"%s\")", unworded$variable, unworded$question)
  warning(sprintf(paste(
    "No question wording was supplied for %d survey item(s); each is shown",
    "under its variable label, or, when it has none, the name given to it: %s.",
    "Supply `question_wording` to show the questions as asked."),
    nrow(unworded), .fg_list_items(shown)), call. = FALSE)
  invisible(NULL)
}

.fg_attach_report <- function(agents, report) {
  report$issue <- NULL
  report$flag <- NULL
  attr(agents, "rendering_report") <- report
  agents
}

#' Report how each persona was rendered
#'
#' Returns the record, kept with a roster, of how every requested demographic
#' field and survey item was rendered into each participant's persona: the
#' text shown, or the reason it was left out. Rosters built from respondent
#' records by [create_agents_from_survey()], [create_agents_from_data()], and
#' [create_agents()] carry the report; check it before a session runs.
#'
#' A field is "rendered" when the persona states it as the source gives it.
#' It is a "fallback" when the persona shows it in a form that needs
#' checking: a code that has no value label, shown as the code; a label
#' exactly 80 characters long, which some survey releases (the ANES 2024 Stata
#' file among them) produce by cutting longer labels; or the label of a
#' negative code that does not match `na_strings`, where the release may use
#' negative codes for missing data; or a field of a supplied record that
#' holds several values, of which the persona shows the first. It is
#' "dropped" when the persona leaves it out, which happens when there is no
#' answer to show: no value, a value whose label matches `na_strings` or
#' is empty, a value without a label that matches `na_strings` and reads as
#' a label (see [create_agents_from_data()]), a negative code without a
#' label, an SPSS user-defined missing value, a record field that does not
#' hold a single value, or a variable that is not in the file. An identifier
#' is dropped too, with the reason "identifier column": a column whose name
#' identifies respondents (`id`, `caseid`, `respondent_id`, `respid`,
#' `persona_id`, `uid`, `record_id`, or `row_id`, in any case and with or
#' without separators, as in `ID` or `CaseID`), or, among the items
#' [create_agents_from_survey()] chooses when `survey_vars` is `NULL`, a
#' variable that looks like a case ID, as described there. The agents
#' would otherwise read the respondent's ID; the roster keeps it in
#' `source_ids`.
#'
#' @param x A roster returned by one of the constructors, a [FocusGroup]
#'   built from such a roster, or the result of [run_focus_group()].
#' @return A data frame with one row per participant and rendered field or
#'   item, in the order requested. A variable requested more than once has
#'   one row, and a sampling-weight column, which is never rendered, has
#'   none. The columns are:
#'   \describe{
#'     \item{agent_id}{The participant ID.}
#'     \item{kind}{`"demographic"` or `"item"`.}
#'     \item{variable}{The variable in the file, or the column of the data.}
#'     \item{question}{The text shown for it: the field label of a
#'       demographic, the question of an item.}
#'     \item{question_source}{Where that text came from: `"wording"`
#'       (`question_wording`), `"name"` (a name given to the variable),
#'       `"dictionary"` (the data's dictionary), `"label"` (the variable
#'       label), or `"variable"` (the variable name itself).}
#'     \item{code}{The stored value, as text; `NA` when there is none.}
#'     \item{text}{The answer as it appears in the persona; `NA` when
#'       dropped.}
#'     \item{rendered_as}{`"label"`, `"supplied_label"` (from
#'       `value_labels`), `"scale"`, `"value"`, or `"code"`; `NA` when
#'       dropped.}
#'     \item{status}{`"rendered"`, `"fallback"`, or `"dropped"`.}
#'     \item{reason}{Why a field is a fallback or was dropped; `NA` otherwise.}
#'     \item{template}{For an answer shown on a scale, the `scale_template`
#'       used; `NA` otherwise.}
#'   }
#'   The call stops when `x` carries no report, as for a roster built from
#'   supplied persona text or a list subset from a roster.
#' @examples
#' toy <- data.frame(
#'   age = c(25, 41, 80),
#'   abortion = haven::labelled(c(2, 7, -9), c(
#'     "1. Always permitted" = 1, "7. Never permitted" = 7, "-9. Refused" = -9))
#' )
#' path <- file.path(tempdir(), "toy_scale.dta")
#' haven::write_dta(toy, path)
#' agents <- create_agents_from_survey(
#'   n_participants = 3, survey_path = path,
#'   config = LLMR::llm_config("groq", "openai/gpt-oss-20b",
#'                             api_key = LLMR::llm_api_key_env("GROQ_API_KEY")),
#'   demographic_vars = c(age = "age"), survey_vars = "abortion",
#'   question_wording = c(abortion = "Where would you place yourself on abortion?")
#' )
#' report <- persona_rendering_report(agents)
#' report[, c("agent_id", "variable", "text", "status")]
#' @export
persona_rendering_report <- function(x) {
  agents <- if (inherits(x, "focus_group_result")) {
    x$focus_group$agents
  } else if (inherits(x, "FocusGroup")) {
    x$agents
  } else {
    x
  }
  report <- if (is.list(agents)) attr(agents, "rendering_report", exact = TRUE)
  if (is.null(report)) {
    stop(paste("No rendering report is attached. Reports are kept on the roster",
               "returned by create_agents_from_survey(), create_agents_from_data(),",
               "or create_agents() when personas are rendered from respondent",
               "fields; a subset of that list does not keep it."), call. = FALSE)
  }
  report
}
