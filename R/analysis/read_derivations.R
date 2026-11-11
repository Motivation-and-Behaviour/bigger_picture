#' Read and check the derivation rules
#'
#' @param path Path to `derivations.csv`.
#' @param dataschema The dataschema, as `read_dataschema()` returns it.
#' @return The derivations tibble, checked by `validate_derivations()`.
read_derivations <- function(
  path = bp_derivations_path(),
  dataschema = read_dataschema()
) {
  derivations <- read_registry_csv(path, bp_derivations_columns())
  validate_derivations(derivations, dataschema)
  derivations
}
