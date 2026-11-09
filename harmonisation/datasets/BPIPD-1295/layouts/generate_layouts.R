# Generate the fixed-width layouts for BPIPD-1295 (High School and Beyond).
#
# The 1980 base-year (ICPSR 07896) and 1982 first follow-up (ICPSR 08297)
# files are headerless fixed-width ASCII files, one line per case, documented
# by SPSS setup files written in the card-image syntax of the early 1980s: the
# command name in columns 1-15, its arguments from column 16, and, when the
# file contains a NUMBERED command, a card sequence number in columns 73-80
# that is not part of the syntax. This script derives one layout CSV per
# declared fixed-width data file from the DATA LIST block(s) of its setup
# file, checks the layout against the data file itself, and writes it. The
# rules it applies are recorded in README.md next to this script.
#
# Run from the repository root:
#
#   Rscript harmonisation/datasets/BPIPD-1295/layouts/generate_layouts.R
#
# It reads the study folder (read-only) and writes only into this folder.

study_dir <- "/data/BPIPD-1295 - High School and Beyond"
layout_dir <- "harmonisation/datasets/BPIPD-1295/layouts"

# The data files dataset.yaml declares with `reader: "fwf"`, keyed by the
# layout (and resource) name. Every field each setup file defines is kept,
# except the school and student parts of a case ID (which lie inside the ID
# field) and the sophomore-only fields of the senior teacher file.
#
# The 1980 school file (07896 DS0001) is not declared: every one of its 988
# schools is in the 1982 school file (08297 DS0017) with identical values on
# all 231 shared fields, and the 1982 file also covers the other base-year
# schools, so it would only be a second copy (see README.md).
declared <- list(
  hsb_1980_student = list(
    setup = "ICPSR_07896/DS0002/07896-0002-Setup.sps",
    data = "ICPSR_07896/DS0002/07896-0002-Data.txt"
  ),
  hsb_1980_language = list(
    setup = "ICPSR_07896/DS0004/07896-0004-Setup.sps",
    data = "ICPSR_07896/DS0004/07896-0004-Data.txt"
  ),
  # The study ships one setup file for both teacher-comment files, written
  # for the sophomore file; it tells the user to remove the sophomore-only
  # variables (TYB11A to TYB14, positions 20-37) for the senior file, whose
  # records are 19 characters long. `keep_within_record` does exactly that.
  hsb_1980_teacher_senior = list(
    setup = "ICPSR_07896/DS0005/07896-0005-Setup.sps",
    data = "ICPSR_07896/DS0005/07896-0005-Data.txt",
    keep_within_record = TRUE
  ),
  hsb_1980_teacher_sophomore = list(
    setup = "ICPSR_07896/DS0006/07896-0006-Setup.sps",
    data = "ICPSR_07896/DS0006/07896-0006-Data.txt"
  ),
  # The twins file reuses the student file's setup, but it stores every
  # decimal-format field rounded to a whole number on its natural scale (a
  # weight of 90.21 reads `90`, an SES score of -0.981 reads `-1`), so none of
  # its decimals is implied. Measured against the student file for the 2,257
  # sampled twins and siblings: every value of all 45 such fields lies within
  # 0.5 of the student file's value (README.md).
  hsb_1980_twins_siblings = list(
    setup = "ICPSR_07896/DS0007/07896-0007-Setup.sps",
    data = "ICPSR_07896/DS0007/07896-0007-Data.txt",
    decimals_rounded = TRUE
  ),
  hsb_1982_sophomore = list(
    setup = "ICPSR_08297/DS0001/08297-0001-Setup.sps",
    data = "ICPSR_08297/DS0001/08297-0001-Data.txt"
  ),
  hsb_1982_senior = list(
    setup = "ICPSR_08297/DS0004/08297-0004-Setup.sps",
    data = "ICPSR_08297/DS0004/08297-0004-Data.txt"
  ),
  hsb_1982_labour_market = list(
    setup = "ICPSR_08297/DS0015/08297-0015-Setup.sps",
    data = "ICPSR_08297/DS0015/08297-0015-Data.txt"
  ),
  hsb_1982_school = list(
    setup = "ICPSR_08297/DS0017/08297-0017-Setup.sps",
    data = "ICPSR_08297/DS0017/08297-0017-Data.txt"
  )
)

# Identifier fields are read as text so that their digits survive exactly.
id_fields <- c(
  "ID",
  "CASEID",
  "SCHLID",
  "SCHOOLID",
  "STUDNTID",
  "TEACHID",
  "FAMID",
  "FSCHID",
  "CONSCHID"
)

# DATA LIST entries that contradict the study's own record layout. Each is
# dropped, and every other duplicate definition must agree exactly.
setup_errata <- data.frame(
  layout = "hsb_1982_sophomore",
  name = "BYTESTWT",
  start = 137L,
  end = 144L,
  reason = paste(
    "Block 2 of 08297-0001-Setup.sps defines BYTESTWT at 137-144, where",
    "block 1 defines FUPARWT; the codebook's record layout (Appendix D,",
    "p. D-1) and its entry for question WT3 place BYTESTWT at 145-152",
    "(F8.3), as block 1 does."
  )
)

# Decimal counts in a setup file that contradict the study's record layout.
# `decimals` keeps the setup's count; `rescale_decimals` takes the correction.
decimal_errata <- data.frame(
  layout = "hsb_1982_senior",
  name = c("BBCONCPT", "BBLOCUS", "BBWORK", "BBFAMILY", "BBCOMMUN"),
  setup_decimals = 3L,
  decimals = 2L,
  reason = paste(
    "08297-0004-Setup.sps declares (3) for the five base-year composite",
    "scales, but the codebook's record layout gives F4.2 for each, the",
    "1980 student file's setup declares (2) for the same fields, and their",
    "raw digits are identical to the 1980 student file's for every matched",
    "senior."
  )
)

# Read a setup file, dropping the card sequence numbers of NUMBERED files.
read_setup <- function(path) {
  x <- readLines(path, warn = FALSE)
  if (any(grepl("^NUMBERED", x))) {
    x <- substr(x, 1, 72)
  }
  x
}

# The argument text of every command `command` in a setup file: a command
# starts at column 1 and runs until the next line with text in column 1.
command_blocks <- function(x, command) {
  starts <- grep(paste0("^", command), x)
  commands <- grep("^[^ ]", x)
  lapply(starts, function(s) {
    end <- c(commands[commands > s], length(x) + 1L)[1] - 1L
    paste(substr(x[s:end], 16, 1000), collapse = " ")
  })
}

# Parse one DATA LIST block into name, start, end, decimals and alpha flag.
parse_data_list <- function(text) {
  text <- gsub("([0-9])\\s*-\\s*([0-9])", "\\1-\\2", text)
  text <- gsub("\\(\\s*([0-9A-Za-z]+)\\s*\\)", "(\\1)", text)
  tok <- strsplit(trimws(text), "\\s+")[[1]]
  is_header <- function(t) {
    t %in%
      c("FIXED", "/", "/1", "1", "(1)") ||
      grepl("^(FILE|RECORDS)=", t)
  }
  i <- 1L
  while (i <= length(tok) && is_header(tok[i])) {
    if (grepl("^RECORDS=", tok[i]) && tok[i] != "RECORDS=1") {
      stop("Only single-record DATA LIST blocks are supported: ", tok[i])
    }
    i <- i + 1L
  }
  rows <- list()
  while (i <= length(tok)) {
    name <- tok[i]
    pos <- tok[i + 1L]
    if (!grepl("^[A-Z][A-Z0-9_#@$]*$", name) || name == "TO") {
      stop("Unexpected DATA LIST token `", name, "`")
    }
    if (is.na(pos) || !grepl("^[0-9]+(-[0-9]+)?$", pos)) {
      stop("No column range after `", name, "`")
    }
    span <- as.integer(strsplit(pos, "-", fixed = TRUE)[[1]])
    i <- i + 2L
    fmt <- NA_character_
    if (i <= length(tok) && grepl("^\\(.+\\)$", tok[i])) {
      fmt <- gsub("[()]", "", tok[i])
      i <- i + 1L
    }
    if (!is.na(fmt) && !grepl("^([0-9]+|A)$", fmt)) {
      stop("Unexpected format `(", fmt, ")` for `", name, "`")
    }
    rows[[length(rows) + 1L]] <- data.frame(
      name = name,
      start = span[1],
      end = span[length(span)],
      decimals = if (!is.na(fmt) && fmt != "A") as.integer(fmt) else NA,
      alpha = identical(fmt, "A")
    )
  }
  do.call(rbind, rows)
}

# Variable labels from the VAR LABELS blocks: `NAME label /` entries.
parse_var_labels <- function(x) {
  text <- paste(unlist(command_blocks(x, "VAR LABELS")), collapse = " ")
  entries <- trimws(strsplit(text, "/", fixed = TRUE)[[1]])
  entries <- entries[nzchar(entries)]
  name <- sub("\\s.*$", "", entries)
  label <- trimws(sub("^\\S+", "", entries))
  keep <- !duplicated(name) & nzchar(label)
  stats::setNames(gsub("\\s+", " ", label[keep]), name[keep])
}

# Merge the blocks of one setup file into a single layout.
merge_blocks <- function(blocks, errata) {
  fields <- do.call(
    rbind,
    lapply(seq_along(blocks), function(k) cbind(blocks[[k]], block = k))
  )
  if (nrow(errata) > 0) {
    drop <- paste(fields$name, fields$start, fields$end) %in%
      paste(errata$name, errata$start, errata$end)
    stopifnot(sum(drop) == nrow(errata))
    fields <- fields[!drop, ]
  }
  # A field repeated in several blocks keeps its first block and the one
  # decimal count its definitions give; any disagreement stops the script.
  key <- paste(fields$name, fields$start, fields$end, fields$alpha)
  merged <- lapply(
    split(fields, factor(key, levels = unique(key))),
    function(d) {
      dec <- unique(stats::na.omit(d$decimals))
      if (length(dec) > 1) {
        stop("Conflicting decimals for ", d$name[1])
      }
      d <- d[1, ]
      d$decimals <- if (length(dec) == 1) dec else NA
      d
    }
  )
  fields <- do.call(rbind, merged)
  dupes <- unique(fields$name[duplicated(fields$name)])
  if (length(dupes) > 0) {
    stop("Conflicting definitions for ", paste(dupes, collapse = ", "))
  }
  fields[order(fields$start, -fields$end), ]
}

# Drop sub-fields that lie wholly inside another field (the school and
# student parts of a case ID); any other overlap stops the script.
drop_contained <- function(fields) {
  span <- paste(fields$start, fields$end)
  if (anyDuplicated(span)) {
    stop("Two fields share a span: ", span[duplicated(span)][1])
  }
  inside <- vapply(
    seq_len(nrow(fields)),
    function(j) {
      any(
        fields$start <= fields$start[j] &
          fields$end >= fields$end[j] &
          seq_len(nrow(fields)) != j
      )
    },
    logical(1)
  )
  kept <- fields[!inside, ]
  overlap <- which(kept$start[-1] <= kept$end[-nrow(kept)])
  if (length(overlap) > 0) {
    stop("Overlapping fields: ", paste(kept$name[overlap], collapse = ", "))
  }
  list(kept = kept, dropped = fields$name[inside])
}

# Choose each field's type from its setup format and its values in the file,
# and decide for each decimal-format field whether its decimals are implied.
assign_types <- function(fields, data_path) {
  raw <- readr::read_fwf(
    data_path,
    col_positions = readr::fwf_positions(
      fields$start,
      fields$end,
      fields$name
    ),
    col_types = readr::cols(.default = readr::col_character()),
    trim_ws = TRUE,
    progress = FALSE
  )
  number <- "^-?([0-9]+|[0-9]*\\.[0-9]+)$"
  bad_example <- vapply(
    raw,
    function(v) {
      v <- v[!is.na(v) & !grepl(number, v)]
      if (length(v) > 0) v[1] else NA_character_
    },
    character(1)
  )
  with_point <- vapply(
    raw,
    function(v) sum(grepl(".", v, fixed = TRUE)),
    integer(1)
  )
  non_missing <- vapply(raw, function(v) sum(!is.na(v)), integer(1))

  numeric_ok <- is.na(bad_example[fields$name])
  fields$type <- ifelse(
    fields$name %in% id_fields | fields$alpha | !numeric_ok,
    "c",
    "d"
  )

  # A decimal count in the setup is an instruction to rescale only when the
  # file stores the digits without a decimal point. Every value of a field
  # must agree; a field mixing both forms stops the script.
  has_dec <- !is.na(fields$decimals)
  points <- with_point[fields$name]
  values <- non_missing[fields$name]
  mixed <- has_dec & points > 0 & points < values
  if (any(mixed)) {
    stop("Fields mixing explicit and implied decimals: ", fields$name[mixed])
  }
  fields$rescale_decimals <- ifelse(
    has_dec & points == 0,
    fields$decimals,
    NA_integer_
  )
  list(
    fields = fields,
    rows = nrow(raw),
    bad_example = bad_example[!is.na(bad_example)],
    explicit = fields$name[has_dec & points > 0],
    implied = fields$name[has_dec & points == 0]
  )
}

summaries <- list()
for (layout_name in names(declared)) {
  item <- declared[[layout_name]]
  setup <- read_setup(file.path(study_dir, item$setup))
  data_path <- file.path(study_dir, item$data)

  blocks <- lapply(command_blocks(setup, "DATA LIST"), parse_data_list)
  errata <- setup_errata[setup_errata$layout == layout_name, ]
  fields <- merge_blocks(blocks, errata)
  contained <- drop_contained(fields)
  fields <- contained$kept

  widths <- unique(nchar(readLines(data_path, warn = FALSE), type = "bytes"))
  if (length(widths) != 1) {
    stop(item$data, " has records of several lengths")
  }
  beyond <- fields$name[fields$end > widths]
  if (isTRUE(item$keep_within_record)) {
    fields <- fields[fields$end <= widths, ]
  } else if (length(beyond) > 0) {
    stop(layout_name, " runs past the ", widths, "-byte record")
  }

  typed <- assign_types(fields, data_path)
  fields <- typed$fields
  if (isTRUE(item$decimals_rounded)) {
    fields$rescale_decimals <- NA_integer_
    typed$implied <- character()
  }
  fix <- decimal_errata[decimal_errata$layout == layout_name, ]
  if (nrow(fix) > 0) {
    at <- match(fix$name, fields$name)
    stopifnot(
      !anyNA(at),
      all(fields$decimals[at] == fix$setup_decimals),
      !anyNA(fields$rescale_decimals[at])
    )
    fields$rescale_decimals[at] <- fix$decimals
  }
  labels <- parse_var_labels(setup)

  stopifnot(
    !anyDuplicated(fields$name),
    !is.unsorted(fields$start, strictly = TRUE),
    all(fields$start[-1] > fields$end[-nrow(fields)]),
    max(fields$end) <= widths
  )

  layout <- data.frame(
    name = fields$name,
    start = fields$start,
    end = fields$end,
    type = fields$type,
    decimals = fields$decimals,
    rescale_decimals = fields$rescale_decimals,
    setup_block = fields$block,
    label = unname(labels[fields$name])
  )
  readr::write_csv(
    layout,
    file.path(layout_dir, paste0(layout_name, ".csv")),
    na = ""
  )

  summaries[[layout_name]] <- data.frame(
    layout = layout_name,
    blocks = length(blocks),
    fields = nrow(layout),
    records = typed$rows,
    width = widths,
    last_end = max(layout$end),
    text = sum(layout$type == "c"),
    explicit_dec = length(typed$explicit),
    implied_dec = length(typed$implied),
    unlabelled = sum(is.na(layout$label))
  )
  message(
    layout_name,
    "\n  dropped (inside another field): ",
    paste(contained$dropped, collapse = " "),
    "\n  dropped (beyond the record): ",
    if (isTRUE(item$keep_within_record)) paste(beyond, collapse = " "),
    "\n  decimal point in the file: ",
    paste(typed$explicit, collapse = " "),
    "\n  implied decimals: ",
    paste(typed$implied, collapse = " "),
    "\n  non-numeric values: ",
    if (length(typed$bad_example) > 0) {
      paste0(
        names(typed$bad_example),
        " `",
        typed$bad_example,
        "`",
        collapse = ", "
      )
    }
  )
  rm(typed)
  gc()
}

print(do.call(rbind, summaries), right = FALSE)
