#' Add a file's columns that the spine does not already carry, keyed on CASEID
bp138_add <- function(base, tbl) {
  tbl <- tbl[, c("CASEID", setdiff(names(tbl), names(base)))]
  dplyr::left_join(base, tbl, by = "CASEID", relationship = "one-to-one")
}

#' Tidier for BPIPD-138 (Healthy Steps for Young Children)
#'
#' The 30-33 month interview, the only one with a screen item (`T2E1`), is the
#' spine; five other one-row-per-child files add columns on `CASEID`.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child interviewed at 30-33 months
tidy_BPIPD_138 <- function(raw_dataset, spec) {
  # DS0005-DS0008 are event logs, not one row per child (user guide vol. 1, Table 1).
  raw_dataset$data$data_dataset10 |>
    bp138_add(raw_dataset$data$data_dataset1) |>
    bp138_add(raw_dataset$data$data_dataset2) |>
    bp138_add(raw_dataset$data$data_dataset3) |>
    bp138_add(raw_dataset$data$data_dataset4) |>
    bp138_add(raw_dataset$data$data_dataset9)
}
