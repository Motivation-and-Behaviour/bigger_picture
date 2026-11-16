#' Bind a dataset's imputations into one set of tables
#'
#' @param imputations A list of `impute_dataset()` results, one per
#'   imputation, or one such result; NULL entries (a dataset that is not
#'   included) are dropped.
#' @return A list of `cells` and `units`, each the imputations' tables
#'   bound by row.
bind_imputations <- function(imputations) {
  if (all(c("cells", "units") %in% names(imputations))) {
    imputations <- list(imputations)
  }
  imputations <- Filter(Negate(is.null), imputations)
  empty <- list(
    cells = tibble::tibble(
      imputation = integer(),
      row = integer(),
      variable = character(),
      value = character()
    ),
    units = tibble::tibble(
      imputation = integer(),
      unit = character(),
      variable = character(),
      rows = integer(),
      n_missing = integer(),
      n_imputed = integer(),
      n_age_group = integer(),
      status = character()
    )
  )
  part <- function(name) {
    tables <- lapply(imputations, function(one) one[[name]])
    dplyr::bind_rows(c(list(empty[[name]]), tables))
  }
  list(cells = part("cells"), units = part("units"))
}
