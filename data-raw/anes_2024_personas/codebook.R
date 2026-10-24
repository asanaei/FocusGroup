# codebook.R ----------------------------------------------------------------
# Reads the ANES 2024 Time Series User Guide and Codebook (PDF) through
# pdftotext -raw and finds, for a variable, its block of the codebook: the
# lines from its name to the next variable's name, with the running page
# headers removed. make_personas.R uses these functions to take question
# wording verbatim from the codebook, to verify the full text of value labels
# that Stata cut at 80 characters, and to quote the label and universe of each
# variable in the build log. The package tests source this file too when
# ANES_CODEBOOK is set.
#
# Requires the pdftotext program (poppler). Its path is taken from the
# environment variable PDFTOTEXT, else from the search path.

# Transliterate the typographic characters of the PDF to ASCII: ligatures,
# curly quotes, and the several hyphens and dashes. Other characters are left
# alone, so a check for ASCII can still find them.
cb_ascii <- function(x) {
  map <- c("\ufb00" = "ff", "\ufb01" = "fi", "\ufb02" = "fl", "\ufb03" = "ffi",
           "\ufb04" = "ffl", "\u2018" = "'", "\u2019" = "'", "\u201a" = "'",
           "\u2032" = "'", "\u201c" = "\"", "\u201d" = "\"", "\u201e" = "\"",
           "\u2033" = "\"", "\u2010" = "-", "\u2011" = "-", "\u2012" = "-",
           "\u2013" = "-", "\u2212" = "-", "\u2014" = "--", "\u2015" = "--",
           "\u2026" = "...", "\u00a0" = " ")
  x <- enc2utf8(as.character(x))
  for (from in names(map)) x <- gsub(from, map[[from]], x, fixed = TRUE)
  x
}

# Collapse runs of whitespace to one space and trim the ends.
cb_squish <- function(x) trimws(gsub("[[:space:]]+", " ", x))

# Text as it is compared: ASCII-transliterated and whitespace-normalized.
cb_norm <- function(x) cb_squish(cb_ascii(x))

# Join the lines of a passage into one normalized string. A line that ends in
# a letter and a hyphen and is followed by a line starting with a letter is a
# hyphenated word broken at the margin ("self-" / "reliance"), so the two are
# joined without a space; other lines are joined with one space.
cb_join <- function(lines) {
  pieces <- cb_norm(lines)
  pieces <- pieces[nzchar(pieces)]
  if (!length(pieces)) return("")
  out <- pieces[[1]]
  for (piece in pieces[-1]) {
    tight <- grepl("[A-Za-z]-$", out) && grepl("^[A-Za-z]", piece)
    out <- paste0(out, if (tight) "" else " ", piece)
  }
  out
}

# Read the PDF. Returns a list with `lines`, a data frame of every text line
# (text, printed page number, whether the line is a page's running header),
# the row at which the codebook section starts, and the PDF's md5.
cb_read <- function(pdf, pdftotext = Sys.getenv("PDFTOTEXT", "")) {
  if (!nzchar(pdftotext)) pdftotext <- Sys.which("pdftotext")
  if (!nzchar(pdftotext) || !file.exists(pdftotext)) {
    stop("pdftotext was not found; set PDFTOTEXT to its path.", call. = FALSE)
  }
  if (!file.exists(pdf)) stop("Codebook PDF not found: ", pdf, call. = FALSE)
  out <- tempfile(fileext = ".txt")
  on.exit(unlink(out), add = TRUE)
  status <- system2(pdftotext, c("-raw", "-enc", "UTF-8", shQuote(pdf), shQuote(out)))
  if (!identical(as.integer(status), 0L) || !file.exists(out)) {
    stop("pdftotext failed on ", pdf, call. = FALSE)
  }
  raw <- readLines(out, encoding = "UTF-8", warn = FALSE)
  # pdftotext ends every page with a form feed, so the first line of a page
  # starts with one; blank pages give lines that are only form feeds.
  text <- paste(raw, collapse = "\n")
  pages <- strsplit(text, "\f", fixed = TRUE)[[1]]
  rows <- lapply(seq_along(pages), function(p) {
    lines <- strsplit(pages[[p]], "\n", fixed = TRUE)[[1]]
    if (!length(lines)) return(NULL)
    first <- which(nzchar(trimws(lines)))[1]
    header <- seq_along(lines) == first
    head_text <- if (is.na(first)) "" else cb_norm(lines[[first]])
    num <- regmatches(head_text, regexpr("(^[0-9]+\\b)|(\\b[0-9]+$)", head_text))
    page <- if (length(num)) as.integer(num) else NA_integer_
    data.frame(text = lines, page = page, pdf_page = p, header = header,
               stringsAsFactors = FALSE)
  })
  lines <- do.call(rbind, rows)
  lines$header[is.na(lines$header)] <- FALSE
  start <- which(cb_norm(lines$text) == "CODEBOOK: VARIABLES" & !lines$header &
                   !is.na(lines$page) & lines$page >= 21L)[1]
  if (is.na(start)) stop("The codebook section was not found in ", pdf, call. = FALSE)
  list(lines = lines, start = start, md5 = unname(tools::md5sum(pdf)))
}

# Letters and digits only, upper case: the form in which a variable label in
# the data file is compared with the label printed in the codebook.
cb_key <- function(x) toupper(gsub("[^A-Za-z0-9]", "", cb_ascii(x)))

# Find the line on which each variable's block begins. A block begins with a
# line whose first word is the variable's name and whose label (the rest of
# that line, or the lines after it) starts as the variable's label in the data
# file does. `labels` is a named character vector of the data file's variable
# labels, one per variable in the file.
cb_index <- function(cb, labels) {
  lines <- cb$lines
  body <- seq_len(nrow(lines)) >= cb$start & !lines$header
  first <- sub("^\\s*(\\S+).*$", "\\1", lines$text)
  cand <- which(body & first %in% names(labels))
  rest_of <- function(i) {
    rest <- sub("^\\s*\\S+\\s*", "", lines$text[[i]])
    j <- i + 1L
    while (nchar(cb_key(rest)) < 12L && j <= nrow(lines) && j <= i + 6L) {
      if (!lines$header[[j]]) rest <- paste(rest, lines$text[[j]])
      j <- j + 1L
    }
    rest
  }
  hit <- vapply(cand, function(i) {
    want <- substr(cb_key(labels[[first[[i]]]]), 1L, 12L)
    nzchar(want) && startsWith(cb_key(rest_of(i)), want)
  }, logical(1))
  idx <- data.frame(variable = first[cand][hit], line = cand[hit],
                    stringsAsFactors = FALSE)
  # A name printed twice with its label would make two blocks; keep the first.
  idx[!duplicated(idx$variable), , drop = FALSE]
}

# The block of one variable: its lines (running headers removed) and pages.
# Stops if a line inside the block looks like the start of another variable's
# block (a variable name alone on a line or followed by capitals), because then
# the boundary is uncertain and a wording could run into the next variable.
cb_block <- function(cb, index, variable, names_in_file) {
  at <- match(variable, index$variable)
  if (is.na(at)) stop("Variable ", variable, " was not found in the codebook.", call. = FALSE)
  from <- index$line[[at]]
  later <- index$line[index$line > from]
  to <- if (length(later)) min(later) - 1L else nrow(cb$lines)
  rows <- cb$lines[from:to, , drop = FALSE]
  rows <- rows[!rows$header, , drop = FALSE]
  tok <- sub("^\\s*(\\S+).*$", "\\1", rows$text)
  rest <- sub("^\\s*\\S+\\s*", "", rows$text)
  loose <- tok %in% names_in_file & (!nzchar(rest) | grepl("^[A-Z]{3}", rest))
  loose[1] <- FALSE
  if (any(loose)) {
    stop(sprintf("The codebook block of %s contains a line that may start another block: %s",
                 variable, paste(rows$text[loose], collapse = " | ")), call. = FALSE)
  }
  rows
}

# The whole block as one normalized string.
cb_block_text <- function(block) cb_join(block$text)

# Words that open a section of a block.
cb_section_pattern <- paste0(
  "^(Question|Value Labels|Survey Question\\(s\\)|Universe|Response Order|",
  "Variable Note|Note:|Randomization|Interviewer Instruction|Programming)")

# The text printed under "Question" in a block, normalized, and the page on
# which it begins; NULL when the block has no question.
cb_question <- function(block) {
  starts <- grepl(cb_section_pattern, cb_norm(block$text))
  q <- which(grepl("^Question\\b", cb_norm(block$text)) &
               !grepl("^Question\\(s\\)", cb_norm(block$text)))
  if (!length(q)) return(NULL)
  q <- q[[1]]
  nxt <- which(starts & seq_along(starts) > q)
  end <- if (length(nxt)) nxt[[1]] - 1L else nrow(block)
  first <- sub("^\\s*Question\\s*", "", block$text[[q]])
  text <- cb_join(c(first, if (end > q) block$text[(q + 1L):end]))
  pages <- c(if (nzchar(trimws(first))) block$page[[q]],
             if (end > q) block$page[(q + 1L):end][nzchar(trimws(block$text[(q + 1L):end]))])
  list(text = text, page = if (length(pages)) pages[[1]] else block$page[[q]])
}

# The text printed under "Universe", normalized, or NA when there is none.
cb_universe <- function(block) {
  norm <- cb_norm(block$text)
  starts <- grepl(cb_section_pattern, norm)
  u <- which(grepl("^Universe\\b", norm))
  if (!length(u)) return(NA_character_)
  u <- u[[1]]
  nxt <- which(starts & seq_along(starts) > u)
  end <- if (length(nxt)) nxt[[1]] - 1L else nrow(block)
  cb_join(c(sub("^\\s*Universe\\s*", "", block$text[[u]]),
            if (end > u) block$text[(u + 1L):end]))
}

# The label printed for one block: the variable's own line after its name,
# with the lines that follow it up to the first section.
cb_label <- function(block) {
  norm <- cb_norm(block$text)
  starts <- grepl(cb_section_pattern, norm)
  end <- which(starts)[1]
  end <- if (is.na(end)) nrow(block) else end - 1L
  first <- sub("^\\s*\\S+\\s*", "", block$text[[1]])
  cb_join(c(first, if (end > 1L) block$text[2:end]))
}

# Whether `text` (normalized) occurs in the block, and on which page the
# first line of the match lies. The block is joined as cb_join() joins it.
cb_find <- function(block, text) {
  target <- cb_norm(text)
  joined <- ""
  starts <- rep(NA_integer_, nrow(block))
  for (i in seq_len(nrow(block))) {
    piece <- cb_norm(block$text[[i]])
    if (!nzchar(piece)) next
    if (!nzchar(joined)) {
      starts[[i]] <- 1L
      joined <- piece
    } else {
      tight <- grepl("[A-Za-z]-$", joined) && grepl("^[A-Za-z]", piece)
      starts[[i]] <- nchar(joined) + if (tight) 1L else 2L
      joined <- paste0(joined, if (tight) "" else " ", piece)
    }
  }
  pos <- regexpr(target, joined, fixed = TRUE)[[1]]
  if (pos < 0) return(list(found = FALSE, page = NA_integer_))
  line <- max(which(!is.na(starts) & starts <= pos))
  list(found = TRUE, page = block$page[[line]])
}
