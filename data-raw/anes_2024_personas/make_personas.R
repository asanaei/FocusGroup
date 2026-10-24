# make_personas.R -------------------------------------------------------------
# Builds the dataset `anes_2024_personas` that FocusGroup and LLMRpanel ship:
# 100 respondents of the ANES 2024 Time Series Study, chosen for the spread of
# their profiles, with demographics and political attitudes written as text.
# Run from the FocusGroup package root:
#
#   ANES_DTA=/path/anes_timeseries_2024_stata_20260519.dta \
#   ANES_CODEBOOK=/path/anes_timeseries_2024_userguidecodebook_20260519.pdf \
#   PANEL_ROOT=/path/to/LLMRpanel \
#     Rscript data-raw/anes_2024_personas/make_personas.R
#
# The run writes data/anes_2024_personas.rda (xz, version 3) into both
# packages, reloads both copies and checks that they are identical, and
# writes the fingerprint tests/testthat/fixtures/anes_2024_personas.md5 into
# both. Neither raw file is bundled; both are available from
# www.electionstudies.org. pdftotext (poppler) must be on the search path or
# named by PDFTOTEXT. See README.md in this folder for the rules applied.
#
# The build stops, rather than writing a dataset, whenever a check fails.

options(warn = 1, stringsAsFactors = FALSE)

FG_ROOT <- normalizePath(".", mustWork = TRUE)
if (!file.exists(file.path(FG_ROOT, "DESCRIPTION")) ||
    !identical(unname(read.dcf(file.path(FG_ROOT, "DESCRIPTION"))[, "Package"]), "FocusGroup")) {
  stop("Run this script from the FocusGroup package root.", call. = FALSE)
}
BUILD_DIR <- file.path(FG_ROOT, "data-raw", "anes_2024_personas")
DTA <- Sys.getenv("ANES_DTA", "")
CODEBOOK <- Sys.getenv("ANES_CODEBOOK", "")
PANEL_ROOT <- Sys.getenv("PANEL_ROOT", "")
for (need in c("ANES_DTA", "ANES_CODEBOOK", "PANEL_ROOT")) {
  value <- Sys.getenv(need, "")
  if (!nzchar(value) || !file.exists(value)) {
    stop(sprintf("Set %s to an existing path (now: %s).", need,
                 if (nzchar(value)) value else "<unset>"), call. = FALSE)
  }
}
PANEL_ROOT <- normalizePath(PANEL_ROOT)
if (!identical(unname(read.dcf(file.path(PANEL_ROOT, "DESCRIPTION"))[, "Package"]), "LLMRpanel")) {
  stop("PANEL_ROOT must be the LLMRpanel package root.", call. = FALSE)
}
N_SHIP <- 100L
SEED <- 110L
COMPLETE_MIN <- 0.75

source(file.path(BUILD_DIR, "codebook.R"))
source(file.path(BUILD_DIR, "persona_spec.R"))

# The missing-data vocabulary and the scale-endpoint rule are FocusGroup's
# own, so the dataset is decoded as create_agents_from_survey() decodes a file.
suppressMessages(pkgload::load_all(FG_ROOT, quiet = TRUE, helpers = FALSE,
                                   export_all = FALSE))
fg <- asNamespace("FocusGroup")
na_strings <- FocusGroup::fg_default_na_strings()
is_missing_label <- function(x) fg$.fg_is_missing_label(x, na_strings)
scale_endpoints <- fg$.fg_scale_endpoints

say <- function(...) cat(sprintf(...), "\n", sep = "")
fail <- function(...) stop(sprintf(...), call. = FALSE)
section <- function(title) cat("\n== ", title, " ", strrep("=", max(3L, 70L - nchar(title))), "\n", sep = "")

# --- 1. The specification -----------------------------------------------------
section("Specification")
spec <- anes_fields
parse_codes <- function(x) {
  if (!nzchar(x)) return(numeric())
  as.numeric(strsplit(x, ",", fixed = TRUE)[[1]])
}
if (anyDuplicated(spec$handle)) fail("Duplicate handles: %s", paste(spec$handle[duplicated(spec$handle)], collapse = ", "))
if (anyDuplicated(spec$variable)) fail("A variable is used twice: %s", paste(spec$variable[duplicated(spec$variable)], collapse = ", "))
types <- c("label", "scale", "thermometer", "age_band", "any_children", "recode", "attendance")
if (!all(spec$type %in% types)) fail("Unknown type: %s", paste(setdiff(spec$type, types), collapse = ", "))
if (!all(spec$level %in% c("ordinal", "numeric", "nominal"))) fail("Unknown level.")
if (!all(spec$domain %in% c("demographic", "identity", "affect", "evaluation", "issue", "values"))) fail("Unknown domain.")
if (any(spec$level == "numeric" & !spec$type %in% c("scale", "thermometer"))) fail("Only scales and thermometers are numeric.")
if (any(spec$irt & spec$level == "nominal")) fail("A nominal item cannot enter the IRT model.")
if (any(spec$irt & spec$domain == "demographic")) fail("Demographics do not enter the IRT model.")
if (any(spec$variable %in% anes_excluded_identity)) fail("An excluded identity item is in the spec.")
if (!all(grepl("^(demo|att_(id|aff|eval|iss|val))_[a-z0-9_]+$", spec$handle))) fail("A handle has the wrong form.")
prefix <- c(demographic = "demo_", identity = "att_id_", affect = "att_aff_",
            evaluation = "att_eval_", issue = "att_iss_", values = "att_val_")
if (!all(startsWith(spec$handle, prefix[spec$domain]))) fail("A handle's prefix does not match its domain.")
say("%d fields: %d demographics, %d attitudes; %d enter the IRT model.",
    nrow(spec), sum(spec$domain == "demographic"), sum(spec$domain != "demographic"), sum(spec$irt))

# --- 2. Metadata, the refusal rule, and the codebook --------------------------
section("Variable labels and codebook")
meta <- haven::read_dta(DTA, n_max = 0)
file_labels <- vapply(meta, function(v) {
  lab <- attr(v, "label", exact = TRUE)
  if (is.null(lab)) "" else lab
}, character(1))
read_vars <- unique(c(spec$variable, unname(anes_helper_variables)))
missing_vars <- setdiff(read_vars, names(file_labels))
if (length(missing_vars)) fail("Not in the data file: %s", paste(missing_vars, collapse = ", "))
refused <- read_vars[grepl(anes_refused_label, file_labels[read_vars], ignore.case = TRUE)]
if (length(refused)) {
  fail("The spec uses variables the build refuses (SPS, PAPER, GSS, RESTRICTED): %s",
       paste(sprintf("%s (%s)", refused, file_labels[refused]), collapse = "; "))
}
# The refusal rule itself, tried on variables it exists to stop.
must_refuse <- c("V242340", "V242614", "V242617", "V242620", "V242619", "V241731",
                 "V241168", "V241711", "V241533")
caught <- grepl(anes_refused_label, file_labels[must_refuse], ignore.case = TRUE)
if (!all(caught)) fail("The refusal rule does not catch: %s", paste(must_refuse[!caught], collapse = ", "))
say("Refusal rule checked on V242340, V242614, V242617, V242620, V242619, V241731, V241168, V241711, V241533.")

cb <- cb_read(CODEBOOK)
cb_idx <- cb_index(cb, file_labels)
say("Codebook: %d pages; %d of %d variables in the data file located.",
    length(unique(cb$lines$pdf_page)), nrow(cb_idx), length(file_labels))
block_of <- function(v) cb_block(cb, cb_idx, v, names(file_labels))

# The label and universe of every variable read, from the codebook.
cb_info <- do.call(rbind, lapply(read_vars, function(v) {
  b <- block_of(v)
  data.frame(variable = v, label = cb_label(b), universe = cb_universe(b),
             pages = paste(unique(b$page), collapse = ","), stringsAsFactors = FALSE)
}))
for (v in c(anes_replaced$new_variable, "V242341", "V242419", "V242420", "V242422", "V241439")) {
  r <- cb_info[cb_info$variable == v, ]
  say("%s (p. %s): label \"%s\"; universe: %s", v, r$pages, r$label,
      if (is.na(r$universe)) "none printed" else sprintf("\"%s\"", r$universe))
}

# --- 3. Value labels: missing codes, 80-character labels, recodes -------------
section("Value labels")
raw <- haven::read_dta(DTA, col_select = dplyr::all_of(read_vars))
n_file <- nrow(raw)
codes_of <- function(v) as.numeric(haven::zap_labels(raw[[v]]))
labels_of <- function(v) {
  lab <- attr(raw[[v]], "labels", exact = TRUE)
  if (is.null(lab)) return(stats::setNames(numeric(), character()))
  lab <- lab[!is.na(lab)]
  stats::setNames(as.numeric(unname(lab)), cb_ascii(names(lab)))
}
clean_label <- function(x) {
  x <- cb_ascii(x)
  x <- sub("^\\s*-?[0-9]+\\.?\\s+", "", x)
  x <- gsub("\\s*\\{(VOL|SPECIFY)\\}", "", x)
  cb_squish(x)
}
is_ascii <- function(x) {
  x <- x[!is.na(x)]
  !length(x) || all(!grepl("[^\x01-\x7f]", x, useBytes = TRUE))
}

label_vars <- unique(c(spec$variable, anes_helper_variables[["attendance"]]))
problems <- character()
for (v in label_vars) {
  lab <- labels_of(v)
  neg <- lab[lab < 0]
  pos <- lab[lab >= 0]
  bad_neg <- names(neg)[!is_missing_label(names(neg))]
  if (length(bad_neg)) problems <- c(problems, sprintf("%s: negative-code label not in the missing vocabulary: %s",
                                                       v, paste(bad_neg, collapse = " | ")))
  thermo <- spec$type[match(v, spec$variable)] %in% "thermometer"
  if (thermo) {
    inside <- pos[pos <= 100]
    if (length(inside)) problems <- c(problems, sprintf("%s: thermometer labels a code inside 0-100: %s",
                                                        v, paste(names(inside), collapse = " | ")))
  } else {
    bad_pos <- names(pos)[is_missing_label(names(pos))]
    if (length(bad_pos)) problems <- c(problems, sprintf("%s: positive-code label matches the missing vocabulary: %s",
                                                         v, paste(bad_pos, collapse = " | ")))
  }
  if (!is_ascii(names(lab))) problems <- c(problems, sprintf("%s: a value label is not ASCII", v))
}
if (length(problems)) fail("Value label checks failed:\n%s", paste(problems, collapse = "\n"))
say("Every negative-code label of the %d variables matches fg_default_na_strings(); no positive label does (thermometer codes above 100 are missing by range).",
    length(label_vars))

overrides <- utils::read.csv(file.path(BUILD_DIR, "label_overrides.csv"), colClasses = "character")
overrides$code <- as.numeric(overrides$code)
overrides$page <- as.integer(overrides$page)
long_labels <- do.call(rbind, lapply(label_vars, function(v) {
  lab <- labels_of(v)
  lab <- lab[lab >= 0 & nchar(names(lab)) == 80L]
  if (!length(lab)) return(NULL)
  data.frame(variable = v, code = unname(lab), label = names(lab), stringsAsFactors = FALSE)
}))
key <- function(v, code) paste(v, code)
unverified <- long_labels[!key(long_labels$variable, long_labels$code) %in% key(overrides$variable, overrides$code), ]
if (nrow(unverified)) {
  fail("80-character labels without a verified override:\n%s",
       paste(sprintf("%s %s: %s", unverified$variable, unverified$code, unverified$label), collapse = "\n"))
}
unneeded <- overrides[!key(overrides$variable, overrides$code) %in% key(long_labels$variable, long_labels$code), ]
if (nrow(unneeded)) fail("Overrides for labels that are not 80 characters long: %s",
                         paste(key(unneeded$variable, unneeded$code), collapse = ", "))
for (i in seq_len(nrow(overrides))) {
  o <- overrides[i, ]
  cut_label <- long_labels$label[long_labels$variable == o$variable & long_labels$code == o$code]
  body <- clean_label(cut_label)
  if (!startsWith(cb_norm(o$full_label), cb_norm(body))) {
    fail("%s %s: the label in the file is not a prefix of the override.", o$variable, o$code)
  }
  printed <- paste0(format(o$code, scientific = FALSE), ". ", o$full_label)
  hit <- cb_find(block_of(o$variable), printed)
  if (!hit$found) fail("%s %s: \"%s\" is not in the variable's codebook block.", o$variable, o$code, printed)
  if (!identical(hit$page, o$page)) fail("%s %s: found on p. %s, not p. %s.", o$variable, o$code, hit$page, o$page)
  say("Verified override %s %s (p. %d): %s", o$variable, o$code, o$page, o$full_label)
}

# Value labels as rendered: overrides applied, then cleaned.
label_text <- function(v) {
  lab <- labels_of(v)
  text <- names(lab)
  for (i in which(overrides$variable == v)) {
    text[lab == overrides$code[[i]]] <- paste0(overrides$code[[i]], ". ", overrides$full_label[[i]])
  }
  stats::setNames(clean_label(text), as.character(unname(lab)))
}
full_label_body <- function(v) {
  lab <- labels_of(v)
  text <- names(lab)
  for (i in which(overrides$variable == v)) {
    text[lab == overrides$code[[i]]] <- paste0(overrides$code[[i]], ". ", overrides$full_label[[i]])
  }
  stats::setNames(cb_squish(sub("^\\s*-?[0-9]+\\.?\\s+", "", cb_ascii(text))), unname(lab))
}
for (v in names(anes_recodes)) {
  tab <- anes_recodes[[v]]
  body <- full_label_body(v)
  positive <- as.numeric(names(body))[as.numeric(names(body)) >= 0]
  if (!setequal(tab$code, positive)) fail("%s: the recode table does not cover exactly the labeled codes.", v)
  found <- unname(body[as.character(tab$code)])
  if (!identical(found, tab$anes_label)) {
    fail("%s: recode table labels differ from the file:\n%s", v,
         paste(sprintf("%s: table '%s' / file '%s'", tab$code, tab$anes_label, found)[found != tab$anes_label],
               collapse = "\n"))
  }
  say("Recode table for %s checked against its %d full labels.", v, nrow(tab))
}

# --- 4. Decode every field for every respondent --------------------------------
section("Decoding")
lookup <- function(lab, x) unname(lab[as.character(x)])
decode_field <- function(row) {
  v <- row$variable
  x <- codes_of(v)
  lab <- label_text(v)
  codes <- as.numeric(names(lab))
  off <- parse_codes(row$off_scale)
  missing <- is.na(x) | x < 0
  # Effective codes and the missing-data rule of each type.
  if (row$type == "thermometer") {
    missing <- missing | x > 100
    if (any(!missing & x != round(x))) fail("%s: a rating is not a whole number.", v)
  } else if (row$type == "attendance") {
    ever <- codes_of(anes_helper_variables[["attendance"]])
    if (any(ever %in% 2 & !is.na(x) & x >= 0)) fail("%s: a respondent who never attends has a frequency.", v)
    if (any(!is.na(ever) & ever >= 0 & !ever %in% c(1, 2))) fail("%s: unexpected codes in V241439.", v)
    never <- ever %in% 2
    missing <- is.na(ever) | ever < 0 | (ever %in% 1 & (is.na(x) | x < 0))
    x[never] <- 5
    if (!identical(lookup(lab, 5), "Never")) fail("%s: code 5 is not 'Never'.", v)
  } else if (row$type == "age_band") {
    if (any(!missing & (x < 18 | x > 80))) fail("%s: an age outside 18-80.", v)
  }
  observed <- sort(unique(x[!missing]), method = "radix")
  text <- rep(NA_character_, length(x))
  value <- rep(NA_real_, length(x))   # category rank, scale point, or rating
  if (row$type %in% c("label", "attendance", "any_children")) {
    unlabeled <- setdiff(observed, codes)
    if (length(unlabeled)) fail("%s: unlabeled codes in the data: %s", v, paste(unlabeled, collapse = ", "))
    text[!missing] <- if (row$type == "any_children") ifelse(x[!missing] > 0, "Yes", "No") else
      lookup(lab, x[!missing])
  } else if (row$type == "recode") {
    tab <- anes_recodes[[v]]
    unlabeled <- setdiff(observed, tab$code)
    if (length(unlabeled)) fail("%s: codes missing from the recode table: %s", v, paste(unlabeled, collapse = ", "))
    text[!missing] <- tab$text[match(x[!missing], tab$code)]
  } else if (row$type == "scale") {
    substantive <- codes >= 0
    ends <- lapply(2:6, function(k) scale_endpoints(k, codes[substantive], unname(lab[substantive])))
    if (any(vapply(ends, is.null, logical(1)))) fail("%s: not an endpoint-labeled scale.", v)
    e <- ends[[1]]
    if (!identical(c(e$lo, e$hi), c(1, 7)) ||
        !all(vapply(ends, function(z) identical(z, e), logical(1)))) {
      fail("%s: the scale endpoints are not 1 and 7.", v)
    }
    stray <- setdiff(observed, c(1:7, off))
    if (length(stray)) fail("%s: values off the 1-7 scale: %s", v, paste(stray, collapse = ", "))
    on <- !missing & x %in% 1:7
    text[on] <- sprintf("%d on a scale from 1 (%s) to 7 (%s)", as.integer(x[on]),
                        clean_label(e$lo_label), clean_label(e$hi_label))
    rest <- !missing & x %in% off
    text[rest] <- lookup(lab, x[rest])
    value[on] <- x[on]
  } else if (row$type == "thermometer") {
    text[!missing] <- sprintf("%d on a 0-100 feeling thermometer", as.integer(x[!missing]))
    value[!missing] <- x[!missing]
  } else if (row$type == "age_band") {
    text[!missing] <- as.character(cut(x[!missing], anes_age_breaks, anes_age_labels))
  }
  if (!identical(is.na(text), missing)) fail("%s: NA cells do not match the missing codes.", v)
  # Position in [0, 1] for the distance; category rank for the IRT model.
  n_cat <- NA_integer_
  if (row$level == "numeric") {
    coord <- if (row$type == "scale") (value - 1) / 6 else value / 100
  } else if (row$level == "ordinal") {
    if (row$type == "age_band") {
      scale_codes <- anes_age_labels
      k <- match(text, scale_codes)
    } else {
      scale_codes <- if (nzchar(row$order)) parse_codes(row$order) else
        sort(setdiff(codes[codes >= 0], off), method = "radix")
      if (!setequal(scale_codes, setdiff(codes[codes >= 0], off))) {
        fail("%s: `order` does not list every labeled code off `off_scale`.", v)
      }
      stray <- setdiff(observed, c(scale_codes, off))
      if (length(stray)) fail("%s: codes outside the ordinal scale: %s", v, paste(stray, collapse = ", "))
      k <- match(x, scale_codes)
      k[missing] <- NA
    }
    n_cat <- length(scale_codes)
    value <- k
    coord <- (k - 1) / (n_cat - 1)
  } else {
    coord <- ifelse(missing, NA_real_, x)
  }
  list(text = text, coord = coord, value = value, n_cat = n_cat, missing = missing, codes = x)
}
decoded <- lapply(seq_len(nrow(spec)), function(i) decode_field(spec[i, ]))
names(decoded) <- spec$handle
text_all <- as.data.frame(lapply(decoded, `[[`, "text"), check.names = FALSE, stringsAsFactors = FALSE)
coord_all <- do.call(cbind, lapply(decoded, `[[`, "coord"))
missing_all <- do.call(cbind, lapply(decoded, `[[`, "missing"))
say("Decoded %d fields for %d respondents; every NA cell is a missing-data code (negative, or a thermometer rating outside 0-100).",
    ncol(text_all), n_file)
for (h in spec$handle[spec$type == "thermometer"]) {
  x <- codes_of(spec$variable[spec$handle == h])
  over <- table(x[!is.na(x) & x > 100])
  if (length(over)) say("  %s: ratings above 100 set to NA: %s", h,
                        paste(sprintf("%s (n = %d)", names(over), as.integer(over)), collapse = ", "))
}

# --- 5. Eligibility -----------------------------------------------------------
section("Eligibility")
mode_pre <- codes_of(anes_helper_variables[["mode_pre"]])
mode_post <- codes_of(anes_helper_variables[["mode_post"]])
completion <- codes_of(anes_helper_variables[["completion"]])
sample_type <- codes_of(anes_helper_variables[["sample"]])
both <- completion %in% 2 & mode_post > 0
not_paper <- !mode_pre %in% 3 & !mode_post %in% 3
answered <- rowMeans(!missing_all)
eligible <- both & not_paper & answered >= COMPLETE_MIN
say("Respondents in the file: %d (sample types: %s).", n_file,
    paste(sprintf("%s = %d", names(table(sample_type)), as.integer(table(sample_type))), collapse = ", "))
say("Completed both interviews: %d; of these, neither by paper: %d; and at least %d%% of the %d fields answered: %d.",
    sum(both), sum(both & not_paper), round(100 * COMPLETE_MIN), nrow(spec), sum(eligible))
pool <- which(eligible)
n_pool <- length(pool)
P_text <- text_all[pool, , drop = FALSE]
P_coord <- coord_all[pool, , drop = FALSE]

# --- 6. Diversity selection: blocked Gower distance, greedy maximin ------------
section("Selection")
block <- ifelse(spec$domain == "demographic", "demographics", spec$domain)
block_w <- c(demographics = 0.25, identity = 0.25, affect = 0.20,
             evaluation = 0.15, issue = 0.15, values = 0.15)
active <- vapply(seq_len(ncol(P_coord)), function(j) {
  length(unique(P_coord[!is.na(P_coord[, j]), j])) > 1L
}, logical(1))
per_block <- table(block[active])
col_w <- ifelse(active, block_w[block] / sum(block_w[names(per_block)]) /
                  as.numeric(per_block[block]), 0)
nominal <- spec$level == "nominal"
say("Distance over %d columns; block weights %s, each split equally among the block's columns.",
    sum(active), paste(sprintf("%s %.2f", names(block_w), block_w), collapse = ", "))

dist_to <- function(r) {
  num <- numeric(n_pool)
  den <- numeric(n_pool)
  for (j in which(active)) {
    a <- P_coord[, j]
    b <- P_coord[r, j]
    if (is.na(b)) next
    ok <- !is.na(a)
    d <- if (nominal[j]) as.numeric(a[ok] != b) else abs(a[ok] - b)
    num[ok] <- num[ok] + col_w[j] * d
    den[ok] <- den[ok] + col_w[j]
  }
  ifelse(den > 0, num / den, 0)
}

set.seed(SEED)
seed_fields <- list(att_id_party_identification = 1:7, demo_race_ethnicity = 1:6,
                    att_id_liberal_conservative_self_pl = 1:7)
selected <- integer()
for (h in names(seed_fields)) {
  x <- codes_of(spec$variable[spec$handle == h])[pool]
  for (code in seed_fields[[h]]) {
    if (any(x[selected] %in% code)) next
    candidates <- setdiff(which(x %in% code), selected)
    if (!length(candidates)) fail("No eligible respondent has %s = %s.", h, code)
    selected <- c(selected, candidates[sample.int(length(candidates), 1L)])
  }
}
n_seed <- length(selected)
min_dist <- rep(Inf, n_pool)
for (s in selected) min_dist <- pmin(min_dist, dist_to(s))
min_dist[selected] <- -Inf
while (length(selected) < N_SHIP) {
  nxt <- which.max(min_dist)
  selected <- c(selected, nxt)
  min_dist <- pmin(min_dist, dist_to(nxt))
  min_dist[selected] <- -Inf
}
say("%d seeds (every code of party identification, race/ethnicity, and ideology), then greedy maximin to %d.",
    n_seed, N_SHIP)

# --- 7. ideology_score: a graded-response IRT model on the eligible pool -------
section("IRT model")
irt_handles <- spec$handle[spec$irt]
irt_matrix <- vapply(irt_handles, function(h) {
  row <- spec[spec$handle == h, ]
  value <- decoded[[h]]$value[pool]
  if (row$type == "thermometer") as.numeric(cut(value, anes_thermometer_irt_breaks, labels = FALSE)) else value
}, numeric(n_pool))
colnames(irt_matrix) <- irt_handles
party <- P_coord[, "att_id_party_identification"]
ideology <- P_coord[, "att_id_liberal_conservative_self_pl"]
anchor <- rowMeans(cbind(as.numeric(scale(party)), as.numeric(scale(ideology))), na.rm = TRUE)
anchor[is.nan(anchor)] <- NA
flipped <- character()
for (h in irt_handles) {
  r <- stats::cor(irt_matrix[, h], anchor, use = "pairwise.complete.obs")
  if (is.na(r)) fail("%s: no correlation with the anchor.", h)
  if (r < 0) {
    irt_matrix[, h] <- max(irt_matrix[, h], na.rm = TRUE) + 1 - irt_matrix[, h]
    flipped <- c(flipped, h)
  }
}
irt_items <- as.data.frame(irt_matrix)
cats <- vapply(irt_items, function(z) length(unique(z[!is.na(z)])), integer(1))
if (any(cats < 2L)) fail("IRT items with one category: %s", paste(names(cats)[cats < 2L], collapse = ", "))
set.seed(SEED)
fit <- mirt::mirt(irt_items, 1, itemtype = "graded", verbose = FALSE,
                  technical = list(NCYCLES = 2000))
if (!isTRUE(mirt::extract.mirt(fit, "converged"))) fail("The IRT model did not converge.")
theta <- as.numeric(mirt::fscores(fit, method = "EAP", full.scores = TRUE)[, 1])
if (stats::cor(theta, anchor, use = "pairwise.complete.obs") < 0) theta <- -theta
score <- as.numeric(scale(theta))
loadings <- as.numeric(methods::selectMethod("summary", class(fit))(fit, verbose = FALSE)$rotF[, 1])
zc <- scale(irt_matrix)
zc[is.na(zc)] <- 0
pc1 <- stats::prcomp(zc, center = FALSE)$x[, 1]
r_pc1 <- abs(stats::cor(pc1, score))
r_anchor_pool <- stats::cor(score, anchor, use = "pairwise.complete.obs")
r_anchor_sel <- stats::cor(score[selected], anchor[selected], use = "pairwise.complete.obs")
irt_fit <- list(
  items = length(irt_handles), respondents = n_pool,
  iterations = mirt::extract.mirt(fit, "iterations"),
  logLik = mirt::extract.mirt(fit, "logLik"), AIC = mirt::extract.mirt(fit, "AIC"),
  BIC = mirt::extract.mirt(fit, "BIC"),
  loadings = stats::quantile(abs(loadings), c(0, 0.25, 0.5, 0.75, 1)),
  weak_items = irt_handles[abs(loadings) < 0.3],
  flipped = flipped, cor_pc1 = r_pc1, cor_anchor_pool = r_anchor_pool,
  cor_anchor_selected = r_anchor_sel)
say("Graded-response model: %d items, %d respondents, converged in %d EM cycles; logLik %.1f, AIC %.1f, BIC %.1f.",
    irt_fit$items, n_pool, irt_fit$iterations, irt_fit$logLik, irt_fit$AIC, irt_fit$BIC)
say("Absolute loadings: min %.2f, quartiles %.2f / %.2f / %.2f, max %.2f; below 0.3: %s.",
    irt_fit$loadings[[1]], irt_fit$loadings[[2]], irt_fit$loadings[[3]], irt_fit$loadings[[4]],
    irt_fit$loadings[[5]], if (length(irt_fit$weak_items)) paste(irt_fit$weak_items, collapse = ", ") else "none")
say("Correlation of ideology_score with party ID plus ideology: %.3f in the pool, %.3f among the %d selected; with PC1: %.3f.",
    r_anchor_pool, r_anchor_sel, N_SHIP, r_pc1)
party_rank <- decoded$att_id_party_identification$value[pool]
by_party <- function(rows) {
  m <- tapply(score[rows], factor(party_rank[rows], 1:7), mean)
  paste(sprintf("%.2f", m), collapse = " ")
}
say("Mean ideology_score by party ID (strong Democrat to strong Republican): pool %s; selected %s.",
    by_party(seq_len(n_pool)), by_party(selected))
say("Standard deviation of ideology_score among the selected: %.2f (pool: 1).", stats::sd(score[selected]))

# --- 8. The shipped data frame --------------------------------------------------
section("Assembly")
personas <- P_text[selected, , drop = FALSE]
personas$ideology_score <- round(score[selected], 3)
ord <- order(personas$ideology_score, method = "radix")
personas <- personas[ord, , drop = FALSE]
rownames(personas) <- NULL
selected_missing <- missing_all[pool[selected][ord], , drop = FALSE]

wording_rows <- utils::read.csv(file.path(BUILD_DIR, "wording_overrides.csv"), colClasses = "character")
if (!all(wording_rows$handle %in% spec$handle)) fail("wording_overrides.csv names unknown handles.")
if (anyDuplicated(wording_rows$handle)) fail("wording_overrides.csv repeats a handle.")
statuses <- c("verbatim", "from_source_item", "stem_only", "none")
if (!all(wording_rows$wording_status %in% statuses)) fail("Unknown wording_status in wording_overrides.csv.")
dictionary <- do.call(rbind, lapply(seq_len(nrow(spec)), function(i) {
  row <- spec[i, ]
  w <- wording_rows[wording_rows$handle == row$handle, ]
  if (nrow(w)) {
    status <- w$wording_status
    wvar <- if (status == "none") NA_character_ else w$wording_variable
    wording <- if (status == "none") NA_character_ else cb_norm(w$wording)
  } else {
    q <- cb_question(block_of(row$variable))
    if (is.null(q)) fail("%s: %s has no question in the codebook and no row in wording_overrides.csv.",
                         row$handle, row$variable)
    status <- "verbatim"
    wvar <- row$variable
    wording <- q$text
  }
  page <- NA_integer_
  if (status != "none") {
    if (!nzchar(wording)) fail("%s: empty wording.", row$handle)
    hit <- cb_find(block_of(wvar), wording)
    if (!hit$found) fail("%s: the wording is not in the codebook block of %s.", row$handle, wvar)
    page <- hit$page
    if (status %in% c("verbatim", "stem_only") && wvar != row$variable) {
      fail("%s: a %s wording must come from the field's own variable.", row$handle, status)
    }
    if (status == "from_source_item" && wvar == row$variable) {
      fail("%s: a from_source_item wording must come from another variable.", row$handle)
    }
    asked <- cb_question(block_of(wvar))
    if (is.null(asked) || !grepl(wording, asked$text, fixed = TRUE)) {
      fail("%s: the wording is not part of the question printed for %s.", row$handle, wvar)
    }
  } else if (!is.null(cb_question(block_of(row$variable)))) {
    fail("%s: marked 'none' but %s has a question in the codebook.", row$handle, row$variable)
  }
  data.frame(handle = row$handle, question = row$question, anes_variable = row$variable,
             domain = row$domain, wording = wording, wording_variable = wvar,
             wording_page = page, wording_status = status, stringsAsFactors = FALSE)
}))
rownames(dictionary) <- NULL
say("Wording status: %s.", paste(sprintf("%s %d", names(table(dictionary$wording_status)),
                                         as.integer(table(dictionary$wording_status))), collapse = ", "))

build_audit <- data.frame(
  handle = c(spec$handle, "ideology_score"),
  anes_variable = c(spec$variable, NA_character_),
  n_na = c(vapply(spec$handle, function(h) sum(is.na(personas[[h]])), integer(1)), sum(is.na(personas$ideology_score))),
  n_raw_missing = c(as.integer(colSums(selected_missing)), 0L),
  stringsAsFactors = FALSE)
rownames(build_audit) <- NULL
if (!identical(build_audit$n_na, build_audit$n_raw_missing)) fail("NA cells differ from raw missing codes.")

citation <- paste("American National Election Studies. 2026. ANES 2024 Time Series Study",
                  "Full Release [dataset and documentation]. May 19, 2026 version.",
                  "www.electionstudies.org")
guide_text <- cb_join(cb$lines$text[!is.na(cb$lines$page) & cb$lines$page == 5L & !cb$lines$header])
if (!grepl(citation, guide_text, fixed = TRUE)) fail("The citation is not on p. 5 of the User Guide.")
say("Citation verified on User Guide p. 5.")

attr(personas, "dictionary") <- dictionary
attr(personas, "demographic_fields") <- spec$handle[spec$domain == "demographic"]
attr(personas, "source") <- citation
attr(personas, "citation") <- citation
attr(personas, "n_demographics") <- sum(spec$domain == "demographic")
attr(personas, "n_attitudes") <- sum(spec$domain != "demographic")
attr(personas, "build_audit") <- build_audit
attr(personas, "build") <- list(
  script = "FocusGroup: data-raw/anes_2024_personas/make_personas.R",
  R = R.version.string,
  packages = c(haven = as.character(utils::packageVersion("haven")),
               mirt = as.character(utils::packageVersion("mirt"))),
  inputs = data.frame(file = c(basename(DTA), basename(CODEBOOK)),
                      md5 = c(unname(tools::md5sum(DTA)), cb$md5), stringsAsFactors = FALSE),
  seed = SEED,
  eligible = n_pool,
  selection = sprintf("%d seeds by code, then greedy maximin", n_seed),
  irt = list(items = irt_handles, flipped = flipped, iterations = irt_fit$iterations,
             logLik = irt_fit$logLik, cor_anchor_pool = irt_fit$cor_anchor_pool),
  label_overrides = overrides)

# --- 9. Checks on the shipped text ----------------------------------------------
section("Checks")
cells <- unlist(personas[spec$handle], use.names = FALSE)
cells <- cells[!is.na(cells)]
dict_text <- unlist(dictionary[c("handle", "question", "anes_variable", "domain", "wording", "wording_variable", "wording_status")])
all_text <- c(names(personas), cells, dict_text, citation, attr(personas, "demographic_fields"),
              unlist(build_audit[c("handle", "anes_variable")]))
if (!is_ascii(all_text)) fail("Shipped text is not ASCII: %s", paste(head(unique(all_text[grepl("[^\x01-\x7f]", all_text, useBytes = TRUE)]), 5), collapse = " | "))
if (any(grepl("[\t\n\r]", all_text))) fail("Shipped text contains a tab or line break.")
scale_form <- "^[1-7] on a scale from 1 \\(.+\\) to 7 \\(.+\\)$"
thermo_form <- "^([0-9]|[1-9][0-9]|100) on a 0-100 feeling thermometer$"
sentences <- grepl(scale_form, cells) | grepl(thermo_form, cells)
bad <- c(
  missing_vocabulary = sum(is_missing_label(cells)),
  code_prefix = sum(grepl("^\\s*-?[0-9]+\\.?\\s", cells) & !sentences),
  braces = sum(grepl("[{}]", cells)),
  r_prefix = sum(grepl("^R [a-z]", cells)),
  dk_codes = sum(grepl("^(998|999)$", cells)))
if (any(bad > 0)) fail("Banned patterns in cells: %s", paste(names(bad)[bad > 0], bad[bad > 0], collapse = ", "))
for (h in spec$handle[spec$type == "scale"]) {
  v <- personas[[h]][!is.na(personas[[h]])]
  if (!all(grepl(scale_form, v) | v == "Haven't thought much about this")) fail("%s: a scale cell has the wrong form.", h)
}
for (h in spec$handle[spec$type == "thermometer"]) {
  v <- personas[[h]][!is.na(personas[[h]])]
  if (!all(grepl(thermo_form, v))) fail("%s: a thermometer cell has the wrong form.", h)
}
lengths_flagged <- sort(unique(cells[nchar(cells) %in% c(76L, 77L, 80L)]), method = "radix")
unexpected <- setdiff(lengths_flagged, anes_length_allowlist)
if (length(unexpected)) fail("Cells of 76, 77, or 80 characters not on the allowlist:\n%s", paste(unexpected, collapse = "\n"))
say("Text checks passed: ASCII, no missing vocabulary, code prefixes, braces, 'R ' prefixes, or 998/999; scale and thermometer forms; %d cell text(s) of 76, 77, or 80 characters, all allowlisted.",
    length(lengths_flagged))

na_total <- sum(build_audit$n_na)
say("NA cells: %d of %d, in %d columns:", na_total, N_SHIP * nrow(spec), sum(build_audit$n_na > 0))
na_rows <- build_audit[build_audit$n_na > 0, ]
shipped_rows <- pool[selected][ord]
for (i in seq_len(nrow(na_rows))) {
  h <- na_rows$handle[[i]]
  v <- spec$variable[spec$handle == h]
  x <- codes_of(v)[shipped_rows]
  if (spec$type[spec$handle == h] == "attendance") {
    x <- paste0("V241439=", codes_of(anes_helper_variables[["attendance"]])[shipped_rows], "/", x)
  }
  why <- table(x[decoded[[h]]$missing[shipped_rows]])
  say("  %-40s %3d  %s", h, na_rows$n_na[[i]],
      paste(sprintf("%s x%d", names(why), as.integer(why)), collapse = ", "))
}
vote <- personas$att_id_2024_presidential_vote
say("No 2024 presidential vote (NA) among the %d: %d; in the eligible pool: %.1f%%; in the file: %.1f%%.",
    N_SHIP, sum(is.na(vote)), 100 * mean(decoded$att_id_2024_presidential_vote$missing[pool]),
    100 * mean(decoded$att_id_2024_presidential_vote$missing))

# --- 10. Write both copies, reload, compare, fingerprint ------------------------
section("Output")
anes_dump_lines <- function(x) {
  cell <- function(v) {
    v <- if (is.numeric(v)) sprintf("%.3f", v) else as.character(v)
    v[is.na(v)] <- "NA"
    v
  }
  frame_lines <- function(d) {
    cols <- lapply(d, cell)
    c(paste(names(d), collapse = "\t"), do.call(paste, c(cols, sep = "\t")))
  }
  dict <- attr(x, "dictionary")
  dict$wording_page <- as.character(dict$wording_page)
  c("[data]", frame_lines(as.data.frame(unclass(x), stringsAsFactors = FALSE, check.names = FALSE)),
    "[dictionary]", frame_lines(dict),
    "[demographic_fields]", attr(x, "demographic_fields"),
    "[source]", attr(x, "source"),
    "[citation]", attr(x, "citation"),
    "[n_demographics]", as.character(attr(x, "n_demographics")),
    "[n_attitudes]", as.character(attr(x, "n_attitudes")),
    "[build_audit]", frame_lines(attr(x, "build_audit")))
}
anes_fingerprint <- function(x) {
  path <- tempfile(fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  con <- file(path, open = "wb")
  writeLines(anes_dump_lines(x), con, sep = "\n")
  close(con)
  unname(tools::md5sum(path))
}

anes_2024_personas <- personas
targets <- c(FocusGroup = FG_ROOT, LLMRpanel = PANEL_ROOT)
reloaded <- list()
for (pkg in names(targets)) {
  dir.create(file.path(targets[[pkg]], "data"), showWarnings = FALSE)
  path <- file.path(targets[[pkg]], "data", "anes_2024_personas.rda")
  save(anes_2024_personas, file = path, compress = "xz", version = 3)
  env <- new.env()
  load(path, envir = env)
  reloaded[[pkg]] <- env$anes_2024_personas
  say("Wrote %s (%s bytes, md5 %s).", path, format(file.size(path), big.mark = ","), unname(tools::md5sum(path)))
}
if (!identical(reloaded$FocusGroup, reloaded$LLMRpanel) || !identical(reloaded$FocusGroup, personas)) {
  fail("The two written copies differ from each other or from the built data.")
}
fingerprint <- anes_fingerprint(personas)
for (pkg in names(targets)) {
  dir.create(file.path(targets[[pkg]], "tests", "testthat", "fixtures"), recursive = TRUE, showWarnings = FALSE)
  writeLines(fingerprint, file.path(targets[[pkg]], "tests", "testthat", "fixtures", "anes_2024_personas.md5"))
}
say("Both copies identical; fingerprint %s written to both test folders.", fingerprint)
say("Shipped: %d respondents x %d columns (%d fields and ideology_score).",
    nrow(personas), ncol(personas), nrow(spec))
