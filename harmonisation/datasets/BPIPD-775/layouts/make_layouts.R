# Generate the fixed-width layouts for BPIPD-775 from the Stata dictionaries
# that NCES ships with the two ECLS-K public-use child files.
#
# Run once from the repository root:
#   Rscript harmonisation/datasets/BPIPD-775/layouts/make_layouts.R
#
# It reads the dictionaries in the study folder (read-only) and writes only
# into this folder: one layout CSV per data file. See README.md for what the
# columns mean, how the layouts were checked and why fields were left out.

study_dir <- "/data/BPIPD-775 - ECLS-B"
layout_dir <- "harmonisation/datasets/BPIPD-775/layouts"

# Parse a Stata infile dictionary ----------------------------------------
#
# Each field is a line of the form
#   _column(17)  str7  PSUID  %7s  "ORIGINAL SAMPLED PSU ID"
# under a `_line(k)` marker; `_lines(n)` says how many physical lines make up
# one record. The files are Windows-1252, so labels are re-encoded to UTF-8.
parse_stata_dct <- function(path) {
  x <- readLines(path, warn = FALSE, encoding = "latin1")
  x <- iconv(x, from = "windows-1252", to = "UTF-8")

  lines_per_record <- as.integer(
    sub(".*_lines\\((\\d+)\\).*", "\\1", grep("_lines\\(", x, value = TRUE))
  )
  stopifnot(length(lines_per_record) == 1)

  is_line <- grepl("^\\s*_line\\(", x)
  line_no <- integer(length(x))
  line_no[is_line] <- as.integer(
    sub(".*_line\\((\\d+)\\).*", "\\1", x[is_line])
  )
  line_no <- cummax(line_no)

  field_re <- paste0(
    "^\\s*_column\\((\\d+)\\)\\s+(\\S+)\\s+(\\S+)\\s+",
    "(%(\\d+)(\\.(\\d+))?[fs])\\s+\"(.*)\"\\s*$"
  )
  is_field <- grepl("^\\s*_column\\(", x)
  m <- regmatches(x[is_field], regexec(field_re, x[is_field]))
  stopifnot(all(lengths(m) == 9))
  m <- do.call(rbind, m)

  d <- data.frame(
    name = m[, 4],
    line = line_no[is_field],
    line_start = as.integer(m[, 2]),
    width = as.integer(m[, 6]),
    stata_type = m[, 3],
    format = m[, 5],
    decimals = ifelse(nzchar(m[, 8]), as.integer(m[, 8]), 0L),
    label = trimws(m[, 9])
  )
  d$line_end <- d$line_start + d$width - 1L

  # Every physical line is described gaplessly from column 1, so a line is
  # exactly as wide as its last field and a record is the lines end to end.
  line_width <- tapply(d$line_end, d$line, max)
  for (k in names(line_width)) {
    f <- d[d$line == as.integer(k), ]
    f <- f[order(f$line_start), ]
    stopifnot(all(f$line_start == c(1L, utils::head(f$line_end, -1) + 1L)))
  }
  stopifnot(length(line_width) == lines_per_record)

  offset <- c(0L, cumsum(as.integer(line_width)))[d$line]
  d$start <- d$line_start + offset
  d$end <- d$line_end + offset
  d$type <- ifelse(startsWith(d$stata_type, "str"), "c", "d")

  attr(d, "lines_per_record") <- lines_per_record
  attr(d, "line_width") <- as.integer(line_width)
  d
}

# Survey weights ---------------------------------------------------------
#
# A full-sample weight ends in 0 (`W1C0`, `C1CW0`, `BYCOMW0`) and its
# replicates are the same stem followed by 1, 2, ... (`W1C1`-`W1C80`,
# `C1CW1`-`C1CW90`). The Taylor-series stratum and PSU of each weight
# (`W1C0STR`, `W1C0PSU`; `C1TCWSTR`, `C1TCWPSU`) are named by suffix and
# labelled as Taylor-series design variables (labels are cut at 31 characters
# in the ECLS-K dictionary, so "TAYLOR" can appear as "TA"). Full-sample
# weights and their stratum and PSU are kept; the replicates are dropped.
#
# Replicates 10, 20, ... also end in 0, so a candidate counts as a full-sample
# weight only when it is not itself a replicate of another candidate.
replicates_of <- function(names) {
  as.vector(outer(sub("0$", "", names), 1:90, paste0))
}

full_sample_weight <- function(d) {
  candidate <- grepl("0$", d$name) &
    grepl("WEIGHT|WGT|\\bWT\\b|FULL SAMP", d$label) &
    paste0(sub("0$", "", d$name), "1") %in% d$name
  candidate & !d$name %in% replicates_of(d$name[candidate])
}

replicate_weight <- function(d) {
  d$name %in% replicates_of(d$name[full_sample_weight(d)])
}

taylor_design <- function(d) {
  grepl("(STR|PSU)$", d$name) &
    grepl("TAY|\\bTA$|\\bTS\\b|STRATUM|SAMPLING UNIT", d$label)
}

# Labels that name a screen, a device or media use, in any block.
media_label <- paste(
  "\\bTV\\b",
  "TELEVIS",
  "VIDEO",
  "VID GAME",
  "COMPUTER",
  "INTERNET",
  "ONLINE",
  "SCREEN",
  "MEDIA",
  "PHONE",
  "TABLET",
  "ELECTRONIC",
  "E-?MAIL",
  "TEXT MESS",
  "SOCIAL NETW",
  "DVD",
  "VCR",
  "\\bWEB\\b",
  sep = "|"
)

# Labelled selections ----------------------------------------------------
#
# A field is kept when it falls in one of these groups (the first that
# matches names it in the layout's `group` column). Each group is a block of
# the file that holds child-level data a dataschema row could use; the blocks
# left out are listed in README.md with the reason.
k5_groups <- list(
  ids = "^(CHILDID|PARENTID|PSUID|CC_ID|TWIN_ID)$|_ID$",
  design_weights = "weights",
  composites = "^X",
  imputation_flags = "^IF",
  field_management = "^F[1-9]",
  parent_interview = "^P[1-9]",
  child_assessment_and_questionnaire = "^C[1-9]",
  teacher_child_level = "^T[1-9]",
  subject_teacher_child_level = "^[GMN][89]",
  # Round 7 `O7` items (reading, mathematics and science skills such as
  # "identify central message"); the dictionary does not say whether they
  # rate the child or the class, so they are kept until that is known.
  o7_items_level_unconfirmed = "^O7",
  media_items_other_blocks = "media"
)

k8_groups <- list(
  ids = "^(CHILDID|PARENTID)$|_ID$",
  design_weights = "weights",
  child_level_composites = "nonround",
  round_composites = "^(W[1-8]|R[1-7])",
  school_context_composites = "school",
  field_management = "^F[1-7]",
  parent_interview = "^P[1-7]",
  child_assessment_and_questionnaire = "^C[1-7]",
  teacher_child_level = "^T[1-7]",
  subject_teacher_child_level = "^[GMN][67]",
  student_records = "^U[1-7]",
  media_items_other_blocks = "media"
)

select_fields <- function(d, groups, school_block) {
  rep_wt <- replicate_weight(d)
  is_weight <- !rep_wt & (full_sample_weight(d) | taylor_design(d))
  d$group <- NA_character_
  for (g in names(groups)) {
    hit <- switch(
      groups[[g]],
      weights = is_weight,
      media = grepl(media_label, d$label),
      nonround = !grepl("^[A-Z][0-9]", d$name),
      school = grepl(school_block, d$name) & !grepl(" Q[0-9]", d$label),
      grepl(groups[[g]], d$name)
    )
    d$group[is.na(d$group) & hit & !rep_wt] <- g
  }
  d[!is.na(d$group), ]
}

check_layout <- function(layout) {
  stopifnot(
    !anyDuplicated(layout$name),
    !is.unsorted(layout$start, strictly = TRUE),
    all(layout$start[-1] > utils::head(layout$end, -1)),
    all(layout$type %in% c("c", "d"))
  )
}

write_layout <- function(dct_file, groups, school_block, out_file) {
  d <- parse_stata_dct(file.path(study_dir, dct_file))
  layout <- select_fields(d, groups, school_block)
  layout <- layout[order(layout$start), ]
  check_layout(layout)
  cols <- c(
    "name",
    "start",
    "end",
    "type",
    "line",
    "line_start",
    "line_end",
    "stata_type",
    "format",
    "decimals",
    "group",
    "label"
  )
  utils::write.csv(
    layout[cols],
    file.path(layout_dir, out_file),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  message(
    out_file,
    ": ",
    nrow(layout),
    " of ",
    nrow(d),
    " fields; ",
    attr(d, "lines_per_record"),
    " lines per record, record width ",
    sum(attr(d, "line_width")),
    " characters"
  )
  print(table(layout$group))
  invisible(layout)
}

write_layout(
  "ECLSK2011_K5PUF.dct",
  k5_groups,
  school_block = "^$",
  out_file = "eclsk2011_k5_child.csv"
)
write_layout(
  "ECLSK_Kto8_child_STATA.dct",
  k8_groups,
  school_block = "^S[1-7]",
  out_file = "eclsk_k8_child.csv"
)
