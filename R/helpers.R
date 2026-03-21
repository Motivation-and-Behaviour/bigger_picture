`%||%` <- function(x, y) if (is.null(x)) y else x

sum_nonmissing <- function(...) {
  values <- list(...)

  if (length(values) == 0) {
    stop("`sum_nonmissing()` requires at least one input.", call. = FALSE)
  }

  if (length(values) == 1L && (
    is.data.frame(values[[1]]) || is.matrix(values[[1]])
  )) {
    values <- values[[1]]
  } else {
    values <- as.data.frame(
      values,
      optional = TRUE,
      stringsAsFactors = FALSE
    )
  }

  observed_count <- rowSums(!is.na(values))
  totals <- rowSums(values, na.rm = TRUE)
  totals[observed_count == 0] <- NA_real_
  totals
}

lookup_values <- function(
  values,
  lookup,
  dataset_id = NULL,
  target_variable = NULL,
  source_col = "source_value",
  value_col = "harmonised_value",
  default = NA
) {
  if (!inherits(lookup, "data.frame")) {
    stop("`lookup` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(source_col, value_col)
  missing_cols <- setdiff(required_cols, names(lookup))
  if (length(missing_cols) > 0) {
    stop(
      "Lookup table is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(dataset_id) && "dataset_id" %in% names(lookup)) {
    lookup <- lookup[
      as.character(lookup$dataset_id) == as.character(dataset_id),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(target_variable) && "target_variable" %in% names(lookup)) {
    lookup <- lookup[
      as.character(lookup$target_variable) == as.character(target_variable),
      ,
      drop = FALSE
    ]
  }

  if (nrow(lookup) == 0) {
    stop(
      "Lookup table contains no rows after applying dataset/variable filters.",
      call. = FALSE
    )
  }

  lookup_keys <- as.character(lookup[[source_col]])
  duplicated_keys <- unique(lookup_keys[duplicated(lookup_keys) & !is.na(lookup_keys)])
  if (length(duplicated_keys) > 0) {
    stop(
      "Lookup table contains duplicate source values after filtering: ",
      paste(duplicated_keys, collapse = ", "),
      call. = FALSE
    )
  }

  matched <- match(as.character(values), lookup_keys)
  out <- lookup[[value_col]][matched]

  unmatched <- is.na(matched) & !is.na(values)
  if (any(unmatched)) {
    out[unmatched] <- default
  }

  out
}
