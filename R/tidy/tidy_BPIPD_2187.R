#' Tidier for BPIPD-2187 (Rodman)
#'
#' Three files, one per smartphone metric, share one 26 x 12 participant-month
#' grid and column names, so metric columns are suffixed and joined side by side.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant per study month
tidy_BPIPD_2187 <- function(raw_dataset, spec) {
  key <- c("PX.ID", "MONTH")

  screentime <- bp2187_suffix_metric(
    raw_dataset$data$`SMU Screentime`,
    "screentime"
  )
  pickups <- bp2187_suffix_metric(raw_dataset$data$`SMU Pickups`, "pickups")
  notifications <- bp2187_suffix_metric(
    raw_dataset$data$`SMU Notification`,
    "notifications"
  )

  df <- screentime |>
    dplyr::left_join(
      bp2187_metric_columns(pickups, screentime, key),
      by = key,
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(
      bp2187_metric_columns(notifications, screentime, key),
      by = key,
      relationship = "one-to-one"
    )

  # Each file carries a full 26 x 12 grid; a month with neither a smartphone
  # metric nor a PANAS score was never observed.
  observed <- rowSums(!is.na(df[grep("^daily_average_", names(df))])) > 0 |
    !is.na(df$panas_total)
  df <- df[observed, , drop = FALSE]

  # Age is recorded only at study start; month k's PANAS was given at its end.
  df <- dplyr::mutate(
    df,
    age_years_at_month = AGE_START_YEARS + MONTH / 12,
    .after = "AGE_START_YEARS"
  )
  attr(df$age_years_at_month, "label") <- paste(
    "Age in years at the end of this study month;",
    "filled as AGE_START_YEARS + MONTH / 12"
  )

  if (anyDuplicated(df[key]) > 0) {
    stop(
      "BPIPD-2187: `PX.ID` and `MONTH` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  bp2187_label_columns(
    tibble::as_tibble(df),
    bp2187_dictionary_labels(raw_dataset$codebook[["Data Dictionary"]])
  )
}

#' Columns repeated unchanged in all three metric files
bp2187_shared_columns <- function() {
  c(
    "PX.ID",
    "MONTH",
    "GENDER",
    "AGE_START_MONTHS",
    "AGE_START_YEARS",
    "Race_Coded",
    "Hispanic.or.Latino",
    "P1_ed_recode",
    "P1_ed_recode_num",
    "P2_ed_recode",
    "P2_ed_recode_num",
    "P_ed_highest",
    "Household_income",
    "Household_income_coded",
    "panas_total",
    "panas_positive",
    "panas_negative",
    "pp.pmean",
    "pn.pmean",
    "pp.pmc",
    "pn.pmc",
    "pp.gmc",
    "pn.gmc",
    "pp.pmc.past",
    "pn.pmc.past"
  )
}

#' Name the metric-varying columns of one file after that file's metric
bp2187_suffix_metric <- function(tbl, metric) {
  shared <- dplyr::select(tbl, dplyr::all_of(bp2187_shared_columns()))
  metric_columns <- setdiff(names(tbl), names(shared))

  dplyr::rename_with(
    tbl,
    ~ paste0(.x, "_", metric),
    dplyr::all_of(metric_columns)
  )
}

#' The key plus the columns this file adds to the spine
bp2187_metric_columns <- function(tbl, spine, key) {
  dplyr::select(
    tbl,
    dplyr::all_of(c(key, setdiff(names(tbl), names(spine))))
  )
}

#' Column descriptions from the study's Excel data dictionary
bp2187_dictionary_labels <- function(path) {
  # The sheet has no header row and opens with one row per file.
  sheet <- readxl::read_excel(
    path,
    sheet = "Sheet1",
    col_names = FALSE,
    col_types = "text",
    .name_repair = "minimal"
  )[, 1:2]
  names(sheet) <- c("variable", "description")
  sheet <- sheet[!is.na(sheet$variable) & !is.na(sheet$description), ]

  stats::setNames(sheet$description, sheet$variable)
}

#' Attach each column's dictionary description as its label
bp2187_label_columns <- function(df, labels) {
  # The dictionary describes each metric column once, under its unsuffixed name.
  stems <- sub("_(screentime|pickups|notifications)$", "", names(df))

  for (column in which(stems %in% names(labels))) {
    attr(df[[column]], "label") <- unname(labels[[stems[[column]]]])
  }

  df
}
