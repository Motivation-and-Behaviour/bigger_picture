#' Tidier for BPIPD-1927 (ActiveBrains and GENOBEX)
#'
#' One SPSS file holds the pre-intervention assessment of the ActiveBrains
#' (overweight/obesity) and GENOBEX (normal-weight) children, told apart by `Project`.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child at the single pre-intervention occasion
tidy_BPIPD_1927 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$activebrains_genobex_data)

  if (anyDuplicated(df$ID2) > 0) {
    stop(
      "BPIPD-1927: `ID2` does not uniquely identify children.",
      call. = FALSE
    )
  }

  # Rows holding only `ID2` and `Project` carry no observation.
  measured <- setdiff(names(df), c("ID2", "Project"))
  df[rowSums(!is.na(df[measured])) > 0L, , drop = FALSE]
}
