#' What a prepared dataset contributes to the combined review tables
#'
#' One small target per dataset, so the combined tables never load a prepared
#' dataset themselves.
#'
#' @param prepared Output of `prepare_analysis_data()`.
#' @param variables The dataset's `variables.csv` mapping table.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @return A named list: `summary`, `flags`, `row_audit` and `dropped`. Each
#'   element is NULL when the dataset is not included.
review_prepared <- function(
  prepared,
  variables,
  outcome_registry,
  settings = analysis_settings()
) {
  list(
    summary = summarise_prepared(prepared, settings),
    flags = flag_prepared_dataset(
      prepared,
      variables,
      outcome_registry,
      settings
    ),
    row_audit = prepared[["row_audit"]],
    dropped = prepared[["dropped"]]
  )
}
