#' Distribution of every numeric variable, per dataset, wave and measure
#'
#' One pass that feeds the data health report, the diagnostics and the range
#' checks. Screen-time variables are summarised for every measure block;
#' other variables repeat across a participant's measure blocks, so they are
#' summarised on the primary rows only.
#'
#' @param harmonised A dataset's harmonised tibble.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param dataschema The dataschema.
#' @return A tibble with one row per variable, wave and measure that has any
#'   value, or NULL.
summarise_numeric <- function(harmonised, info, dataschema) {
  rows <- included_rows(harmonised, info)
  if (is.null(rows)) {
    return(NULL)
  }
  schema <- dataschema[dataschema$data_type %in% "double", ]
  schema <- schema[schema$variable_name %in% names(rows), ]
  wave <- as.character(rows$wave)
  measure <- as.character(rows$st_measure_id)
  primary <- measure %in% "primary"

  summarise_one <- function(i) {
    variable <- schema$variable_name[i]
    values <- rows[[variable]]
    keep <- !is.na(values)
    if (!startsWith(variable, "st_")) {
      keep <- keep & primary
    }
    if (!any(keep)) {
      return(NULL)
    }
    low <- suppressWarnings(as.numeric(schema$min_value[i]))
    high <- suppressWarnings(as.numeric(schema$max_value[i]))
    dt <- data.table::data.table(
      wave = wave[keep],
      st_measure_id = measure[keep],
      x = values[keep]
    )
    out <- dt[,
      c(
        list(
          n = .N,
          mean = mean(x),
          sd = stats::sd(x),
          mad = stats::mad(x),
          n_zero = sum(x == 0),
          n_distinct = data.table::uniqueN(x),
          n_below_min = if (is.na(low)) NA_integer_ else sum(x < low),
          n_above_max = if (is.na(high)) NA_integer_ else sum(x > high)
        ),
        as.list(stats::setNames(
          stats::quantile(x, bp_summary_probs(), names = FALSE),
          bp_summary_quantile_names()
        ))
      ),
      by = c("wave", "st_measure_id")
    ]
    out$variable_name <- variable
    out
  }

  parts <- lapply(seq_len(nrow(schema)), summarise_one)
  out <- tibble::as_tibble(data.table::rbindlist(parts))
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- as.character(info$dataset_id)
  dplyr::relocate(out, "dataset_id", "variable_name")
}

bp_summary_probs <- function() {
  c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)
}

bp_summary_quantile_names <- function() {
  c("min", "p01", "p05", "p25", "median", "p75", "p95", "p99", "max")
}
