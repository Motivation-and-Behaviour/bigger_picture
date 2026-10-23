#' Tidier for BPIPD-774 (Young Minds Matter)
#'
#' Single 2013-14 cross-section: the parent file is the sample, joined to the
#' youth self-report and to the NAPLAN sittings widened by assessment year.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per study child, keyed on `householdID`
tidy_BPIPD_774 <- function(raw_dataset, spec) {
  # The .sav copies carry full labels; the .dta and .sas7bdat twins are ignored.
  parent <- tibble::as_tibble(raw_dataset$data$ymm_parent_data_spss)
  youth <- tibble::as_tibble(raw_dataset$data$ymm_youth_data_spss)

  # Columns repeated in the youth file are kept from the complete parent file.
  youth <- youth[c("householdID", setdiff(names(youth), names(parent)))]

  naplan <- bp774_naplan_wide(raw_dataset$data$ymm_naplan_data_spss)

  tbl <- parent |>
    dplyr::left_join(youth, by = "householdID", relationship = "one-to-one") |>
    dplyr::left_join(
      naplan,
      by = c("householdID" = "householdid"),
      relationship = "one-to-one"
    )

  if (anyDuplicated(tbl$householdID) > 0) {
    stop(
      "BPIPD-774: `householdID` does not uniquely identify rows.",
      call. = FALSE
    )
  }

  tbl
}

#' Widen the NAPLAN sittings file to one row per child
bp774_naplan_wide <- function(naplan) {
  keys <- c("householdid", "NAPLAN_Assessment_Year")

  wide <- tidyr::pivot_wider(
    tibble::as_tibble(naplan),
    id_cols = "householdid",
    names_from = "NAPLAN_Assessment_Year",
    values_from = setdiff(names(naplan), keys),
    names_glue = "{.value}_{NAPLAN_Assessment_Year}",
    names_sort = TRUE
  )

  # The pivoted copies share the source label, so the year is appended to it.
  year <- sub("^.*_(\\d{4})$", "\\1", names(wide))
  for (i in which(names(wide) != "householdid")) {
    attr(wide[[i]], "label") <- paste0(
      attr(wide[[i]], "label"),
      " (",
      year[i],
      ")"
    )
  }

  wide
}
