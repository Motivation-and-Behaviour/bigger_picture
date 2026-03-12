harmonise_from_spec <- function(
  analysis_base,
  spec,
  dataschema = NULL,
  harmonisation_config = NULL
) {
  if (is.null(dataschema) || is.null(harmonisation_config)) {
    stop(
      "Harmonisation requires both `dataschema` and `harmonisation_config`.",
      call. = FALSE
    )
  }

  harmonise_from_tables(
    analysis_base = analysis_base,
    spec = spec,
    dataschema = dataschema,
    harmonisation_config = harmonisation_config
  )
}
