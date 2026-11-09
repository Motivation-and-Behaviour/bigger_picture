#' Tidier for BPIPD-933 (British Preschool-children Play Survey)
#'
#' One unlabelled CSV of every YouGov omnibus screener respondent (UKDS SN
#' 8793); labels are attached from the variable map workbook.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per responding parent (one focal child, one occasion)
tidy_BPIPD_933 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$raw_survey)

  # The screener exits anyone with no child aged 5-11 (codes 7-13) to another
  # survey (questionnaire page 1; methods, Procedure), so those rows hold no item.
  ages_5_to_11 <- paste0("Omnibus_Parents_", 7:13)
  df <- df[rowSums(df[ages_5_to_11] == 1, na.rm = TRUE) > 0L, , drop = FALSE]

  if (anyDuplicated(df$RecordNo) > 0) {
    stop(
      "BPIPD-933: `RecordNo` does not uniquely identify rows.",
      call. = FALSE
    )
  }

  bp933_apply_labels(df, raw_dataset$codebook$variable_map)
}

#' Attach the variable map's variable and value labels
bp933_apply_labels <- function(df, path) {
  variables <- readxl::read_excel(
    path,
    sheet = "Variable",
    col_types = "text",
    .name_repair = "minimal"
  )
  label <- ifelse(
    is.na(variables$Measure),
    variables$Label,
    paste0(variables$Measure, ": ", variables$Label)
  )
  label <- stats::setNames(label, variables$Variable)
  label[names(bp933_screen_labels())] <- bp933_screen_labels()

  # The `Label` sheet names its variable on the first row of each code block.
  values <- readxl::read_excel(
    path,
    sheet = "Label",
    col_types = "text",
    .name_repair = "minimal"
  )[, c("Variables", "Value", "Label")]
  values <- tidyr::fill(values, "Variables", .direction = "down")
  codes <- split(
    stats::setNames(as.numeric(values$Value), values$Label),
    values$Variables
  )

  for (column in intersect(names(df), names(codes))) {
    df[[column]] <- haven::labelled(df[[column]], labels = codes[[column]])
  }
  for (column in intersect(names(df), names(label))) {
    attr(df[[column]], "label") <- unname(label[[column]])
  }

  df
}

#' Screen-time labels in the questionnaire's displayed wording (GUC_Q30)
bp933_screen_labels <- function() {
  stem <- paste0(
    "Time child spends looking at a screen (e.g. TV screen, computer ",
    "screen, etc.), before mid-February 2020: "
  )
  items <- c(
    GUC_Q30_1 = "on an average weekday for recreational purposes",
    GUC_Q30_2 = "on an average weekend day for recreational purposes",
    GUC_Q30_3 = "on an average weekday for homework/ learning/ study purposes",
    GUC_Q30_4 = "on an average weekend day for homework/ learning/ study purposes"
  )
  stats::setNames(paste0(stem, items), names(items))
}
