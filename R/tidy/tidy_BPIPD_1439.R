#' Tidier for BPIPD-1439 (Bradt)
#'
#' Two recruitment rounds of Flemish adolescent gamers (`YEAR` 1 and 2) in one
#' sheet; the parent questionnaire and the round-1 seven-day diary are columns.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per adolescent (single occasion; `YEAR` is the round)
tidy_BPIPD_1439 <- function(raw_dataset, spec) {
  df <- bp1439_label(
    tibble::as_tibble(raw_dataset$data$data),
    raw_dataset$codebook$codebook
  )

  # Rows holding only survey paradata and consent answers are not observations.
  admin <- grepl("(Durationinseconds|Finished|Progress)$", names(df)) |
    names(df) %in%
      c(
        "PersonID",
        "YEAR",
        "PREKINDToestemming",
        "PREOUDERICjongere",
        "PREOUDERICouder"
      )
  df <- df[rowSums(!is.na(df[!admin])) > 0L, ]

  # A recorded refusal of consent removes the row even where the questionnaire
  # was completed; it returns only if the data owner says it was an error.
  refused <- df$PREKINDToestemming %in% 2 | df$PREOUDERICjongere %in% 2
  df <- df[!refused, ]

  # Parent questionnaires with no adolescent answer, diary or parent gaming
  # minutes hold no screen value, and they skipped the round-1-only parent items.
  adolescent <- grepl("^PREKIND", names(df)) & !admin
  diary <- grepl("^D[1-7]", names(df))
  parent_gaming <- names(df) %in%
    c("PREOUDERGametijdkind_4", "PREOUDERGametijdkind_5")
  df <- df[rowSums(!is.na(df[adolescent | diary | parent_gaming])) > 0L, ]

  # `PersonID` repeats across rounds for different families.
  df <- dplyr::mutate(
    df,
    participant_id = paste(YEAR, PersonID, sep = "_"),
    .before = 1
  )
  attr(df$participant_id, "label") <- "Recruitment round (YEAR) and PersonID"

  # One adolescent started the baseline twice; the unfinished attempt goes.
  repeated <- df$participant_id %in%
    df$participant_id[duplicated(df$participant_id)]
  df <- df[!(repeated & df$PREKINDFinished %in% 0), ]

  if (anyDuplicated(df$participant_id) > 0) {
    stop("BPIPD-1439: `participant_id` is not unique.", call. = FALSE)
  }

  df
}

#' Attach the Codebook sheet's construct, item and response labels
bp1439_label <- function(df, path) {
  codebook <- readxl::read_excel(path, sheet = "Codebook", col_types = "text")

  for (row in seq_len(nrow(codebook))) {
    entry <- codebook[row, ]
    column <- entry$Variable
    item <- if (is.na(entry$Item_text_EN)) {
      ""
    } else {
      paste(":", entry$Item_text_EN)
    }
    when <- stats::na.omit(c(entry$Respondent, entry$Timepoint))
    label <- paste0(entry$Construct_EN, item)
    if (length(when) > 0L) {
      label <- paste0(label, " [", toString(when), "]")
    }

    # `_REV` columns hold reverse-scored values, so the listed anchors would read backwards.
    reversed <- grepl("_REV$", column)
    if (reversed) {
      label <- paste(label, "(reverse-scored)")
    }

    codes <- bp1439_codes(entry$Response_options)
    observed <- stats::na.omit(df[[column]])
    # A code list is attached only where it covers every observed value.
    if (
      !reversed &&
        is.numeric(observed) &&
        !is.null(codes) &&
        all(observed %in% codes$all)
    ) {
      df[[column]] <- haven::labelled(
        df[[column]],
        labels = codes$labels,
        label = label
      )
    } else {
      attr(df[[column]], "label") <- label
    }
  }

  df
}

#' Parse a `1 = label; 2; 3 = label` response list into codes and labels
bp1439_codes <- function(options) {
  parts <- trimws(strsplit(options, ";", fixed = TRUE)[[1]])
  labelled <- grepl("^-?[0-9]+ *= *\\S", parts)
  bare <- grepl("^-?[0-9]+$", parts)
  if (is.na(options) || !any(labelled) || !all(labelled | bare)) {
    return(NULL)
  }

  codes <- as.numeric(sub(" *=.*$", "", parts))
  if (anyDuplicated(codes) > 0) {
    return(NULL)
  }

  list(
    all = codes,
    labels = stats::setNames(
      codes[labelled],
      sub("^-?[0-9]+ *= *", "", parts[labelled])
    )
  )
}
