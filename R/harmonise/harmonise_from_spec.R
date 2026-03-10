harmonise_from_spec <- function(tidied_dataset, spec, dataschema = NULL,
                                harmonisation_config = NULL) {
  if (is.null(dataschema) || is.null(harmonisation_config)) {
    stop(
      "Harmonisation requires both `dataschema` and `harmonisation_config`.",
      call. = FALSE
    )
  }

  harmonise_from_tables(
    tidied_dataset = tidied_dataset,
    spec = spec,
    dataschema = dataschema,
    harmonisation_config = harmonisation_config
  )
}
