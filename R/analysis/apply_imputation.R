#' Fill a prepared dataset's covariates with one imputation
#'
#' Puts one imputation's cells into the prepared base table. Screen use is
#' imputed later, for each outcome's own model, and outcomes never are.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param imputed Output of `impute_dataset()`, for one imputation or
#'   several bound together.
#' @param index The imputation to apply.
#' @return `prepared` with the imputed covariates filled in.
apply_imputation <- function(prepared, imputed, index) {
  cells <- imputed$cells[imputed$cells$imputation %in% index, ]
  for (name in unique(cells$variable)) {
    own <- cells[cells$variable == name, ]
    if (!all(is.na(prepared$base[[name]][own$row]))) {
      stop(
        "apply_imputation(): an imputed cell of `",
        name,
        "` already has a value.",
        call. = FALSE
      )
    }
    prepared$base[[name]][own$row] <- own$value
  }
  prepared
}
