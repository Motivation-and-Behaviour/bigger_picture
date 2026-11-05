#' Tidier for BPIPD-706 (Tremolada)
#'
#' One workbook: an unlabelled `Dataset` sheet (patient arm, single occasion)
#' and a `Codebook` sheet whose variable and value labels are attached here.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant (`ResponseID`)
tidy_BPIPD_706 <- function(raw_dataset, spec) {
  codebook <- bp706_codebook(raw_dataset$codebook$codebook)

  bp706_label(tibble::as_tibble(raw_dataset$data$data), codebook)
}

#' Variable and value labels read from the workbook's `Codebook` sheet
bp706_codebook <- function(path) {
  cb <- readxl::read_excel(path, sheet = "Codebook")
  cb <- cb[!is.na(cb[["Variable Name"]]), ]

  values <- stats::setNames(cb[["Values"]], cb[["Variable Name"]])
  same <- !is.na(values) & grepl("^Same as ", values)
  values[same] <- values[sub("^Same as ", "", values[same])]

  labels <- stats::setNames(cb[["Label"]], cb[["Variable Name"]])
  platforms <- bp706_sns_platforms()
  labels[names(platforms)] <- paste0(
    labels[names(platforms)],
    " (",
    platforms,
    ")"
  )

  list(labels = labels, values = lapply(values, bp706_value_labels))
}

#' The platform each `SNS_DailyUse` item asks about
bp706_sns_platforms <- function() {
  # The codebook says only "platform <n>"; each item matches one `*_DailyTime` column row for row.
  c(
    SNS_DailyUse1 = "WhatsApp",
    SNS_DailyUse2 = "Instagram",
    SNS_DailyUse3 = "Facebook",
    SNS_DailyUse4 = "TikTok",
    SNS_DailyUse5 = "Ask/Tellonym",
    SNS_DailyUse6 = "YouTube",
    SNS_DailyUse7 = "Twitch"
  )
}

#' Parse one `Values` cell, or `NULL` when it does not list codes
bp706_value_labels <- function(text) {
  if (is.na(text)) {
    return(NULL)
  }

  parts <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
  matched <- regmatches(
    parts,
    regexec("^(-?[0-9]+)[[:space:]]*=[[:space:]]*(.+)$", parts)
  )

  # Free-text ranges such as "1-10 (Higher = worse distress)" are not code lists
  if (any(lengths(matched) != 3L)) {
    return(NULL)
  }

  stats::setNames(
    as.numeric(vapply(matched, `[`, character(1), 2L)),
    trimws(vapply(matched, `[`, character(1), 3L))
  )
}

#' Attach the codebook's labels to the columns it documents
bp706_label <- function(df, codebook) {
  for (name in intersect(names(df), names(codebook$labels))) {
    value_labels <- codebook$values[[name]]
    if (is.null(value_labels) || !is.numeric(df[[name]])) {
      attr(df[[name]], "label") <- codebook$labels[[name]]
    } else {
      df[[name]] <- haven::labelled(
        df[[name]],
        labels = value_labels,
        label = codebook$labels[[name]]
      )
    }
  }

  df
}
