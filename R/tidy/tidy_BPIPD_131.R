#' Tidier for BPIPD-131 (NHES:2007)
#'
#' The two 2007 components, Parent and Family Involvement (PFI) and School
#' Readiness (SR), sample disjoint sets of children, so they stack rather than
#' join; `.wave` says which component a row came from.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child
tidy_BPIPD_131 <- function(raw_dataset, spec) {
  df <- dplyr::bind_rows(
    raw_dataset$data$nhes_2007_pfi_data,
    raw_dataset$data$nhes_2007_sr_data
  )

  # BASMID is the child identifier and does not overlap between the components;
  # BASEID is the household and repeats for siblings within and across them.
  if (anyDuplicated(df$BASMID) > 0) {
    stop("BPIPD-131: `BASMID` does not uniquely identify rows.", call. = FALSE)
  }

  tibble::as_tibble(df)
}
