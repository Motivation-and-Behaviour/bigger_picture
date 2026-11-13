#' Turn an outcome row's stored values into the score the models use
#'
#' The score each `score_type` refers to (see `analysis/README.md`):
#' continuous rows use their values; binary rows score `TRUE` or `Yes` as 1
#' and `FALSE` or `No` as 0; ordinal and diagnosis rows use a level's position
#' in the dataschema's `allowed_values`, the first level being 1. Level order
#' never comes from a factor, because `analysis_data` stores levels in byte
#' order. `Unknown` is missing. Nominal rows have no score.
#'
#' @param values The stored values.
#' @param score_type The row's `score_type`.
#' @param allowed_values The row's pipe-separated `allowed_values`.
#' @return A numeric vector the length of `values`.
score_outcome_values <- function(values, score_type, allowed_values = NA) {
  if (score_type %in% "continuous") {
    return(as.numeric(values))
  }
  values <- as.character(values)
  values[values %in% "Unknown"] <- NA_character_
  if (score_type %in% "binary") {
    out <- rep(NA_real_, length(values))
    out[values %in% c("TRUE", "Yes")] <- 1
    out[values %in% c("FALSE", "No")] <- 0
    return(out)
  }
  if (score_type %in% c("ordinal", "diagnosis") && !is.na(allowed_values)) {
    levels <- setdiff(
      strsplit(allowed_values, "|", fixed = TRUE)[[1]],
      "Unknown"
    )
    return(as.numeric(match(values, levels)))
  }
  rep(NA_real_, length(values))
}
