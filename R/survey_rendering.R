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
      out[[col]] <- as.character(v)
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
# record: every field of the record, its value, and the text the persona shows
# for it. `fields` is what the persona was rendered from.
.fg_supplied_rows <- function(id, kind, record, fields) {
  variables <- names(record) %||% character()
  if (!length(variables)) return(NULL)
  shown <- .fg_persona_fields(fields)
  value <- vapply(record, function(v) {
    if (length(v)) as.character(v[[1]]) else NA_character_
  }, character(1), USE.NAMES = FALSE)
  text <- vapply(variables, function(v) {
    if (v %in% names(shown)) .fg_tidy_value(shown[[v]]) else NA_character_
  }, character(1), USE.NAMES = FALSE)
  data.frame(
    agent_id = id, kind = kind, variable = variables, question = variables,
    question_source = "variable", code = value, text = text,
    rendered_as = ifelse(is.na(text), NA_character_, "value"),
    status = ifelse(is.na(text), "dropped", "rendered"),
    reason = ifelse(is.na(text), "no value", NA_character_),
    issue = ifelse(is.na(text), "missing", NA_character_), flag = value,
    stringsAsFactors = FALSE
  )
}

.fg_report_columns <- c("agent_id", "kind", "variable", "question",
                        "question_source", "code", "text", "rendered_as",
                        "status", "reason", "issue", "flag")

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
  x <- as.character(x)
  x <- gsub("^[0-9]+\\.\\s+", "", x)
  trimws(x)
}

# Strip any leading code, negative codes included, from a label used inside a
# sentence (the endpoints of a scale).
.fg_label_body <- function(x) {
  trimws(gsub("^\\s*-?[0-9]+\\.\\s+", "", as.character(x)))
}

.fg_number_text <- function(x) {
  out <- as.character(x)
  whole <- !is.na(x) & is.finite(x) & x == round(x) & abs(x) < 1e15
  out[whole] <- sprintf("%.0f", x[whole])
  out
}

.fg_is_missing_label <- function(x, na_strings) {
  if (!length(na_strings)) return(rep(FALSE, length(x)))
  grepl(paste(na_strings, collapse = "|"), x, ignore.case = TRUE)
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
# "-4. Error"); labels above the upper endpoint must lie at least the width of
# the scale beyond it ("99. Haven't thought much about this" after a 1 to 7
# scale). A scale with a negative lower endpoint must be centered on zero.
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
  if (any(beyond < hi + (hi - lo))) return(NULL)
  lo_label <- labels[match(lo, codes)]
  hi_label <- labels[match(hi, codes)]
  if (.fg_bound_label(lo_label) || .fg_bound_label(hi_label)) return(NULL)
  list(lo = lo, hi = hi, lo_label = lo_label, hi_label = hi_label)
}

.fg_scale_text <- function(x, ends) {
  sprintf("On a scale of %s to %s, where %s is %s and %s is %s, you place yourself at %s.",
          .fg_number_text(ends$lo), .fg_number_text(ends$hi),
          .fg_number_text(ends$lo), .fg_label_body(ends$lo_label),
          .fg_number_text(ends$hi), .fg_label_body(ends$hi_label),
          .fg_number_text(x))
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
                          issue = NA_character_, flag = code) {
  list(code = code, text = text, rendered_as = rendered_as, status = status,
       reason = reason, issue = issue, flag = flag)
}

# Render every value of one survey column. Returns a data frame with one row
# per value: the stored `code` as text, the answer `text` passed to the
# persona (NA when left out), how it was rendered (`rendered_as`), its
# `status` ("rendered", "fallback", or "dropped"), and the `reason` for a
# fallback or a dropped answer. Two further columns drive the warnings and are
# not part of the report: `issue` is "missing" for answers left out as missing
# values, "negative" for unlabeled negative codes taken as missing,
# "negative_label" for labels of negative codes shown as answers, "code" for
# unlabeled codes shown as codes, and "long" for labels that may have been
# cut; `flag` is the code a warning names, which for a scale answer is the
# endpoint whose label may have been cut.
.fg_render_column <- function(col, variable, na_strings, supplied = NULL) {
  tab <- .fg_label_table(col, variable, supplied)
  if (!nrow(tab)) return(.fg_render_plain(col, na_strings))

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
      if (length(long)) {
        long <- .fg_number_text(long)
        return(.fg_rendering(code, .fg_scale_text(x, ends), "scale", "fallback", sprintf(
          "the label of scale endpoint %s is 80 characters long and may have been cut",
          paste(long, collapse = " and ")), issue = "long",
          flag = paste(long, collapse = " and ")))
      }
      return(.fg_rendering(code, .fg_scale_text(x, ends), "scale", "rendered"))
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
  data.frame(code = rep(NA_character_, n), text = NA_character_,
             rendered_as = NA_character_, status = "dropped", reason = reason,
             issue = issue, flag = NA_character_, stringsAsFactors = FALSE)
}

# A column without value labels: the stored values, with `na_strings` applied.
.fg_render_plain <- function(col, na_strings) {
  values <- as.character(col)
  out <- data.frame(code = values, text = values, rendered_as = "value",
                    status = "rendered", reason = NA_character_,
                    issue = NA_character_, flag = values, stringsAsFactors = FALSE)
  none <- .fg_no_value(values)
  flagged <- !none & .fg_is_missing_label(values, na_strings)
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
      status = status, reason = pick("reason"), issue = pick("issue"),
      flag = pick("flag"),
      stringsAsFactors = FALSE
    )
  })
  .fg_bind_report(pieces)
}

# Confirm that every answer the report records as shown appears, under the
# question it records, in that agent's persona text. A mismatch is an error in
# the package, and the report would otherwise misdescribe the persona.
.fg_check_report <- function(report, agents) {
  shown <- report[report$status != "dropped", , drop = FALSE]
  if (!nrow(shown)) return(invisible(report))
  found <- vapply(seq_len(nrow(shown)), function(i) {
    persona <- agents[[shown$agent_id[i]]]$persona_description
    q <- shown$question[i]
    v <- shown$text[i]
    needles <- if (identical(shown$kind[i], "demographic")) {
      paste0(q, ": ", v)
    } else {
      c(paste0("Question: ", q, "\n  Answer: ", v), paste0(q, " -- ", v))
    }
    any(vapply(needles, grepl, logical(1), x = persona, fixed = TRUE))
  }, logical(1))
  if (!all(found)) {
    bad <- shown[!found, , drop = FALSE]
    stop("The rendering report does not match the persona text for ",
         paste(sprintf("%s %s", bad$agent_id, bad$variable), collapse = ", "),
         ".", call. = FALSE)
  }
  invisible(report)
}

# Warn about the drawn answers that were replaced, flagged, or inferred to be
# missing, naming the variable, the code, and the participants. Answers left
# out as missing values are announced in a message.
.fg_rendering_conditions <- function(report) {
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
    warning("Shown as the bare code because the file has no value label for it: ",
            .fg_list_items(code), ". Supply labels in `value_labels`.", call. = FALSE)
  }
  long <- describe(report[issue %in% "long", , drop = FALSE])
  if (length(long)) {
    warning("Labels exactly 80 characters long may have been cut short: ",
            .fg_list_items(long), ". Supply the full labels in `value_labels`.",
            call. = FALSE)
  }
  negative_label <- describe(report[issue %in% "negative_label", , drop = FALSE])
  if (length(negative_label)) {
    warning(paste(
      "Labels of negative codes are shown as answers, though some releases,",
      "the ANES and WVS among them, use negative codes for missing data: "),
      .fg_list_items(negative_label), paste(
        ". Add their wording to `na_strings` to leave them out, or supply the",
        "labels in `value_labels` to keep them."), call. = FALSE)
  }
  negative <- describe(report[issue %in% "negative", , drop = FALSE])
  if (length(negative)) {
    warning("Negative codes without a value label were taken as missing and left out: ",
            .fg_list_items(negative),
            ". Label them in `value_labels` if they are answers.", call. = FALSE)
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
                       fields$question_source %in% c("label", "variable"), , drop = FALSE]
  if (!nrow(unworded)) return(invisible(NULL))
  shown <- ifelse(unworded$question_source == "label",
                  sprintf("%s (\"%s\")", unworded$variable, unworded$question),
                  sprintf("%s (no label)", unworded$variable))
  warning(sprintf(paste(
    "No question wording was supplied for %d survey item(s); each is shown",
    "under its variable label, or its name when it has no label: %s. Supply",
    "`question_wording` to show the questions as asked."),
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
#' negative codes for missing data. It is "dropped" when the
#' persona leaves it out, which happens only for missing values: no value, a
#' value whose label matches `na_strings`, a negative code without a label, or
#' a variable that is not in the file.
#'
#' @param x A roster returned by one of the constructors, a [FocusGroup]
#'   built from such a roster, or the result of [run_focus_group()].
#' @return A data frame with one row per participant and requested field or
#'   item, in the order requested:
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
