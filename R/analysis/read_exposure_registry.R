#' Read and check the exposure registry
#'
#' @param path Path to `exposures.csv`.
#' @param dataschema The dataschema, as `read_dataschema()` returns it.
#' @return The exposures tibble, checked by `validate_exposure_registry()`.
read_exposure_registry <- function(
  path = bp_exposures_path(),
  dataschema = read_dataschema()
) {
  exposures <- read_registry_csv(
    path,
    bp_exposures_columns(),
    c("include", "headline")
  )
  validate_exposure_registry(exposures, dataschema)
  exposures
}
