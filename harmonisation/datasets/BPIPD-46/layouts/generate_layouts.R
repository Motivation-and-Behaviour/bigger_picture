# Generate the fixed-width layouts for BPIPD-46 (PSID-CDS).
#
# Every data file in the study folder is a headerless fixed-width ASCII file
# shipped with SPSS (.sps), Stata (.do) and SAS (.sas) setup files of the same
# stem. This script derives one layout CSV per declared data file from the
# DATA LIST block of the SPSS setup file, and writes it only if the Stata and
# SAS setup files and the data file itself agree with it (see README.md).
#
# Run from the repository root:
#
#   Rscript harmonisation/datasets/BPIPD-46/layouts/generate_layouts.R
#
# It reads the study folder (read-only) and writes only into this folder.

study_dir <- "/data/BPIPD-46 - PSID-CDS"
layout_dir <- "harmonisation/datasets/BPIPD-46/layouts"

# The data files dataset.yaml declares with `reader: "fwf"`: layout (and
# resource) name = file stem relative to the study folder. Every field of
# each file is kept; the whole set reads in well under a gigabyte.
declared <- c(
  # Cumulative person-level ID map with per-wave file-presence flags
  cdsind_2021 = "2021/CDSIND2021",
  # CDS-I, 1997
  demog_1997 = "1997/DEMOG1997",
  id_map_1997 = "1997/IDMAP97",
  pcg_child_1997 = "1997/PCG97_CHLD",
  pcg_hh_1997 = "1997/PCG97_HH",
  child_1997 = "1997/CHILD97",
  td_agg_1997 = "1997/TD97_ACT_AGG",
  td_media_1997 = "1997/TD97MEDIA",
  td_activity_1997 = "1997/TD97",
  td_followup_1997 = "1997/TDFUP97",
  teacher_ems_1997 = "1997/EMSTEACH97",
  teacher_pd_1997 = "1997/PDTEACH97",
  school_diary_es_1997 = "1997/ESDIARY97",
  school_diary_ms_1997 = "1997/MSDIARY97",
  school_diary_pd_1997 = "1997/PSDIARY97",
  # CDS-II, 2002
  demog_2002 = "2002/DEMOG",
  id_map_2002 = "2002/IDMAP02",
  pcg_child_2002 = "2002/PCG_CHLD",
  pcg_hh_2002 = "2002/PCG_HHLD",
  child_2002 = "2002/CHILD",
  assess_2002 = "2002/ASSESSMT",
  td_agg_2002 = "2002/TD02_ACT_AGG",
  td_media_2002 = "2002/TD02MEDIA",
  td_activity_2002 = "2002/TD_ACTIVITY",
  td_followup_2002 = "2002/TD_FOLLOWUP",
  teacher_ems_2002 = "2002/EMSTEACH",
  # CDS-III, 2007
  demog_2007 = "2007/DEMOG07",
  id_map_2007 = "2007/IDMAP07",
  pcg_child_2007 = "2007/PCG_CHILD07",
  pcg_hh_2007 = "2007/PCG_HH07",
  child_2007 = "2007/CHILD07",
  assess_2007 = "2007/ASSESS07",
  td_agg_2007 = "2007/TD_ACTAGG07",
  td_media_2007 = "2007/TD07MEDIA",
  td_activity_2007 = "2007/TD_ACT07",
  td_followup_2007 = "2007/TD_FOLLOW07",
  # CDS-2014
  demog_2014 = "2014/DEMOG14",
  id_map_2014 = "2014/IDMAP14",
  hh_roster_2014 = "2014/HHROSTER14",
  pcg_child_2014 = "2014/PCGCHILD14",
  pcg_hh_2014 = "2014/PCGHH14",
  child_2014 = "2014/CHILD14",
  assess_2014 = "2014/ASSESS14",
  td_agg_2014 = "2014/TD_ACTAGG14",
  td_media_2014 = "2014/TD14MEDIA",
  td_activity_2014 = "2014/TD_ACT14",
  td_followup_2014 = "2014/TD_FOLLOW14",
  # CDS-2019
  demog_2019 = "2019/DEMOG2019",
  hh_roster_2019 = "2019/HHROSTER2019",
  pcg_child_2019 = "2019/PCGCHILD2019",
  pcg_hh_2019 = "2019/PCGHH2019",
  child_2019 = "2019/CHILD2019",
  assess_2019 = "2019/ASSESS2019",
  td_agg_2019 = "2019/TD_AGG2019",
  td_media_2019 = "2019/TD19MEDIA",
  td_activity_2019 = "2019/TD_ACT2019",
  td_followup_2019 = "2019/TD_QN2019",
  # CDS-2020
  pcg_child_2020 = "2020/PCGCHILD2020",
  pcg_hh_2020 = "2020/PCGHH2020",
  td_agg_2020 = "2020/TD_AGG2020",
  td_media_2020 = "2020/TD20MEDIA",
  td_activity_2020 = "2020/TD_ACT2020",
  td_followup_2020 = "2020/TD_QN2020",
  # CDS-2021
  demog_2021 = "2021/DEMOG2021",
  hh_roster_2021 = "2021/HHROSTER2021",
  pcg_child_2021 = "2021/PCGCHILD2021",
  pcg_hh_2021 = "2021/PCGHH2021",
  child_2021 = "2021/CHILD2021"
)

fail <- function(stem, ...) {
  stop(stem, ": ", ..., call. = FALSE)
}

# The lines strictly between the line matching `from` and the first line
# after it matching `to`
block <- function(x, from, to, stem) {
  start <- grep(from, x, ignore.case = TRUE)
  if (length(start) != 1L) {
    fail(stem, "expected one line matching ", from, ", found ", length(start))
  }
  ends <- grep(to, x)
  end <- ends[ends > start][1]
  if (is.na(end)) {
    fail(stem, "no end of block after ", from)
  }
  x[seq_len(end - start - 1L) + start]
}

# `NAME start - end` fields in a DATA LIST / infix / INPUT block. A string
# field is marked `(A)` after it in SPSS, `str` before it in Stata and `$`
# between name and positions in SAS.
parse_fields <- function(lines) {
  text <- paste(lines, collapse = " ")
  rx <- paste0(
    "(str\\s+)?([A-Za-z_][A-Za-z0-9_]*)\\s+(\\$\\s+)?",
    "([0-9]+)\\s*-\\s*([0-9]+)(\\s*\\(A\\))?"
  )
  hits <- regmatches(text, gregexpr(rx, text, perl = TRUE))[[1]]
  parts <- regmatches(hits, regexec(rx, hits, perl = TRUE))
  part <- function(i) vapply(parts, `[`, character(1), i)
  data.frame(
    name = part(3),
    start = as.integer(part(5)),
    end = as.integer(part(6)),
    string = nzchar(part(2)) | nzchar(part(4)) | nzchar(part(7))
  )
}

header_value <- function(x, key, stem) {
  line <- grep(paste0("^\\s*", key, "\\s*:"), x, value = TRUE)
  if (length(line) != 1L) {
    fail(stem, "no `", key, "` line in the setup-file header")
  }
  as.integer(gsub("[^0-9]", "", sub("^[^:]*:", "", line)))
}

layout_for <- function(stem) {
  base <- file.path(study_dir, stem)
  sps <- readLines(paste0(base, ".sps"), warn = FALSE)
  do <- readLines(paste0(base, ".do"), warn = FALSE)
  sas <- readLines(paste0(base, ".sas"), warn = FALSE)

  rows <- header_value(sps, "Rows", stem)
  columns <- header_value(sps, "Columns", stem)
  lrecl <- as.integer(sub(
    ".*LRECL\\s*=\\s*([0-9]+).*",
    "\\1",
    grep("LRECL", sps, value = TRUE)
  ))

  # Positions: the SPSS DATA LIST block ends at the first line holding only
  # "."; the VARIABLE LABELS block after it can hold text that looks like a
  # field, so parsing must stop there.
  d <- parse_fields(block(sps, "^\\s*DATA LIST", "^\\s*\\.\\s*$", stem))
  do_fields <- parse_fields(block(do, "^\\s*infix", "^\\s*using", stem))
  sas_fields <- parse_fields(block(sas, "^\\s*INPUT\\s*$", "^\\s*;", stem))

  # Decimal formats: SPSS FORMATS (Fw.d), checked against the SAS ATTRIB block
  d$decimals <- 0L
  if (any(grepl("^\\s*FORMATS", sps))) {
    fmt <- paste(
      block(sps, "^\\s*FORMATS", "^\\s*\\.\\s*$", stem),
      collapse = " "
    )
    rx <- "([A-Za-z_][A-Za-z0-9_]*)\\s+\\(F[0-9]+\\.([0-9]+)\\)"
    found <- regmatches(fmt, gregexpr(rx, fmt))[[1]]
    hits <- regmatches(found, regexec(rx, found))
    fmt_name <- vapply(hits, `[`, character(1), 2)
    fmt_dec <- as.integer(vapply(hits, `[`, character(1), 3))
    if (!all(fmt_name %in% d$name)) {
      fail(stem, "FORMATS names a field missing from DATA LIST")
    }
    d$decimals[match(fmt_name, d$name)] <- fmt_dec
  }
  attrib <- grep("format=", sas, value = TRUE)
  sas_name <- sub("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s.*", "\\1", attrib)
  sas_fmt <- sub(".*format=(\\S+).*", "\\1", attrib)
  sas_dec <- rep(0L, length(sas_fmt))
  has_dec <- grepl("^f[0-9]+\\.[0-9]+$", sas_fmt)
  sas_dec[has_dec] <- as.integer(sub("^f[0-9]+\\.", "", sas_fmt[has_dec]))

  # Labels, for the tidier and the mapping; ignored by the reader
  labels <- block(sps, "^\\s*VARIABLE LABELS", "^\\s*\\.\\s*$", stem)
  label_name <- sub('^\\s*(\\S+)\\s+".*$', "\\1", labels)
  label_text <- sub('^\\s*\\S+\\s+"(.*)"\\s*$', "\\1", labels)
  d$label <- gsub("\\s+", " ", trimws(label_text[match(d$name, label_name)]))

  # The setup files must agree with each other and describe every byte
  n <- nrow(d)
  checks <- c(
    "field count matches the header" = n == columns,
    "names are unique" = !anyDuplicated(d$name),
    "first field starts at 1" = d$start[1] == 1L,
    "fields are ascending and gapless" = all(d$start[-1] == d$end[-n] + 1L),
    "last field ends at LRECL" = d$end[n] == lrecl,
    "Stata infix agrees" = identical(d[1:4], do_fields),
    "SAS INPUT agrees" = identical(d[1:4], sas_fields),
    "SAS formats name every field" = setequal(sas_name, d$name),
    "SAS decimals agree" = identical(
      d$decimals,
      sas_dec[match(d$name, sas_name)]
    ),
    "SAS string formats agree" = identical(
      d$string,
      grepl("^\\$", sas_fmt[match(d$name, sas_name)])
    ),
    "every field has a label" = !anyNA(d$label)
  )
  if (!all(checks)) {
    fail(stem, "failed: ", paste(names(checks)[!checks], collapse = "; "))
  }

  # The data file: CRLF records of exactly LRECL bytes, `Rows` of them
  txt <- paste0(base, ".txt")
  if (file.size(txt) != rows * (lrecl + 2)) {
    fail(stem, "file size is not Rows x (LRECL + 2) bytes")
  }
  # readLines() strips the CRLF, so each line must be exactly LRECL bytes;
  # with the file size above, every terminator is then two bytes
  lines <- readLines(txt, warn = FALSE)
  if (length(lines) != rows || !all(nchar(lines, type = "bytes") == lrecl)) {
    fail(stem, "records are not LRECL bytes each")
  }
  first_end <- readBin(txt, "raw", n = lrecl + 2)[lrecl + 1:2]
  if (!identical(first_end, as.raw(c(0x0d, 0x0a)))) {
    fail(stem, "records are not CRLF-terminated")
  }

  # What the characters of each field actually hold: whether a numeric field
  # parses as a number, and whether a decimal-format field carries its
  # decimal point (if it does, nothing is rescaled)
  d$type <- ifelse(d$string, "c", "d")
  d$decimal_point <- ""
  for (i in seq_len(n)) {
    v <- trimws(substr(lines, d$start[i], d$end[i]))
    v <- v[nzchar(v)]
    if (!d$string[i] && !all(grepl("^-?[0-9]*\\.?[0-9]+$|^-?[0-9]+\\.$", v))) {
      d$type[i] <- "c"
    }
    if (d$decimals[i] > 0L) {
      has_point <- grepl(".", v, fixed = TRUE)
      d$decimal_point[i] <- if (length(v) == 0L) {
        "no values"
      } else if (all(has_point)) {
        "present"
      } else if (any(has_point)) {
        "mixed"
      } else {
        "implied"
      }
    }
  }

  d$string <- NULL
  list(
    layout = d,
    summary = data.frame(
      stem = stem,
      rows = rows,
      fields = n,
      lrecl = lrecl,
      decimal_fields = sum(d$decimals > 0L),
      decimal_point_present = sum(d$decimal_point == "present"),
      decimal_point_other = sum(!d$decimal_point %in% c("", "present")),
      character_fields = sum(d$type == "c")
    )
  )
}

# `name`, `start`, `end` and `type` are read by the `fwf` reader; the rest is
# provenance for the tidier and the mapping
layout_columns <- c(
  "name",
  "start",
  "end",
  "type",
  "decimals",
  "decimal_point",
  "label"
)

summaries <- list()
for (layout_name in names(declared)) {
  out <- layout_for(declared[[layout_name]])
  readr::write_csv(
    out$layout[layout_columns],
    file.path(layout_dir, paste0(layout_name, ".csv")),
    na = ""
  )
  summaries[[layout_name]] <- cbind(layout = layout_name, out$summary)
}

report <- do.call(rbind, summaries)
rownames(report) <- NULL
print(report, row.names = FALSE, width = 200)

stale <- setdiff(
  tools::file_path_sans_ext(list.files(layout_dir, pattern = "\\.csv$")),
  names(declared)
)
if (length(stale) > 0) {
  warning("Layouts no longer declared: ", paste(stale, collapse = ", "))
}
