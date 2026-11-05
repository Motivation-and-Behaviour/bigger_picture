#' Tidier for BPIPD-666 (Mallawaarachchi)
#'
#' The spec's five "waves" are the author's analysis stages; the study itself is
#' one baseline survey and a one-year follow-up of 169 of the 536 children.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child per wave attended (`baseline`, `follow_up`)
tidy_BPIPD_666 <- function(raw_dataset, spec) {
  # BL_workingcopy is the master baseline plus the author's derived averages.
  baseline <- raw_dataset$data[["Working Baseline Data"]]
  baseline <- baseline[setdiff(names(baseline), c(".wave", ".wave_label"))]

  # Baseline kept the Qualtrics names for ECBQ items 29-38; the follow-up
  # crosswalk calls the same items `ecbq_<n>`.
  baseline <- dplyr::rename_with(
    baseline,
    ~ sub("^x([0-9]+)_ecbq$", "ecbq_\\1", .x)
  )

  # Data_Longit2022 repeats the baseline of the 169 followed up, with sleep,
  # DASS, SSIS and parental-control items that the other baseline files dropped.
  longit_2022 <- raw_dataset$data[[
    "Longitudinal Dataset - Data_Longit2022.csv"
  ]]
  baseline_only <- setdiff(
    grep("_wave2$", names(longit_2022), value = TRUE, invert = TRUE),
    c(names(baseline), ".wave", ".wave_label")
  )
  baseline <- dplyr::left_join(
    baseline,
    longit_2022[c("id", baseline_only)],
    by = "id",
    relationship = "one-to-one"
  )

  # data_longit_recoded_v3 is BL_workingcopy joined to the follow-up survey, so
  # only its `_w2` and `_wave2` columns are follow-up measurements.
  longit <- raw_dataset$data[[
    "Longitudinal Dataset - data_longit_recoded_v3.csv"
  ]]
  follow_up <- longit[c(
    "id",
    grep("(_w2|_wave2)$", names(longit), value = TRUE)
  )]

  # `_w2` is the author's crosswalk onto the baseline names; `_wave2` columns
  # were never aligned.
  follow_up <- dplyr::rename_with(follow_up, ~ sub("_w2$", "", .x))

  df <- dplyr::bind_rows(
    dplyr::mutate(baseline, wave = "baseline", .before = 1),
    dplyr::mutate(follow_up, wave = "follow_up", .before = 1)
  )

  if (anyDuplicated(df[c("id", "wave")]) > 0) {
    stop(
      "BPIPD-666: `id` and `wave` do not uniquely identify rows.",
      call. = FALSE
    )
  }

  tibble::as_tibble(bp666_carry(df))
}

#' Carry baseline parent education, income, family structure, employment and age forward
bp666_carry <- function(df) {
  order <- match(df$wave, c("baseline", "follow_up"))
  carried <- function(x) carry_within(x, df$id, order)

  # The follow-up is one year on (Quant_Tables v1 Table 3), inside the
  # 12-month window for household SES, family structure and maternal employment.
  df$parent_edu <- carried(df$parent_edu)
  df$household_income <- carried(df$household_income)
  df$no_of_children <- carried(df$no_of_children)

  # Follow-ups that re-recorded living status keep it in `living_628_wave2` or
  # `parent_living_wave2`, so those rows are not filled.
  fill <- is.na(df$living) &
    is.na(df$living_628_wave2) &
    is.na(df$parent_living_wave2)
  df$living[fill] <- carried(df$living)[fill]

  # Follow-ups that reported a change of employment keep it in
  # `parent_employment_631_wave2` or `parent_employment_2_wave2`.
  fill <- is.na(df$parent_employ) &
    is.na(df$parent_employment_631_wave2) &
    is.na(df$parent_employment_2_wave2)
  df$parent_employ[fill] <- carried(df$parent_employ)[fill]

  # The follow-up asked no age (`age_y_wave2` echoes baseline), so age is
  # baseline age plus the one-year interval.
  fill <- df$wave == "follow_up" & is.na(df$child_age_y)
  df$child_age_y[fill] <- carried(df$child_age_y)[fill] + 1
  df$child_age_m <- carried(df$child_age_m)

  attr(df$parent_edu, "label") <-
    "Parent education; follow-up carried from baseline (parent education always carried)"
  attr(df$household_income, "label") <-
    "Household income band; follow-up carried from baseline (12-month SES rule)"
  attr(df$no_of_children, "label") <-
    "Parent's number of children; follow-up carried from baseline (12-month sibling rule)"
  attr(df$living, "label") <-
    "Parent living status; follow-up carried from baseline where not re-recorded (12-month rule)"
  attr(df$parent_employ, "label") <-
    "Parent employment status; follow-up carried from baseline where not re-recorded (12-month rule)"
  attr(df$child_age_y, "label") <-
    "Child age, completed years; follow-up = baseline + 1 (documented one-year interval)"
  attr(df$child_age_m, "label") <-
    "Child age, months part (1-12); follow-up carried from baseline with child_age_y"
  df
}
