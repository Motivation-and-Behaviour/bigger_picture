#' Bind a dataset's imputations into one set of tables
#'
#' @param imputations A list of `impute_dataset()` results, one per
#'   imputation; NULL entries (a dataset that is not included) are dropped.
#' @return A list of `numeric`, `categorical` and `units`, each the
#'   imputations' tables bound by row.
bind_imputations <- function(imputations) {
  if (!is.null(imputations) && !is.null(names(imputations))) {
    if (all(c("numeric", "categorical", "units") %in% names(imputations))) {
      imputations <- list(imputations)
    }
  }
  imputations <- Filter(Negate(is.null), imputations)
  part <- function(name) {
    out <- dplyr::bind_rows(lapply(imputations, function(one) one[[name]]))
    if (nrow(out) > 0) {
      return(out)
    }
    switch(
      name,
      numeric = tibble::tibble(
        imputation = integer(),
        row = integer(),
        variable = character(),
        value = numeric()
      ),
      categorical = tibble::tibble(
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
        status = character()
      )
    )
  }
  list(
    numeric = part("numeric"),
    categorical = part("categorical"),
    units = part("units")
  )
}
