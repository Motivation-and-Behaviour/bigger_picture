# Convert the low-cardinality character columns of the combined analysis table
# to factors. Materialising hundreds of millions of R strings is what makes
# `analysis_data` slow to load; a factor stores each distinct value once.
# Columns are chosen from the data, not from the schema's `allowed_values`
# (which is not kept in sync with the mappings): a character column becomes a
# factor when it has at most `max_levels` distinct non-missing values, so
# identifier-like columns (participant, cluster, strata) stay character.
# Levels are the observed values in byte (C-locale) order, so the result does
# not depend on the session's locale. `data.table::chmatch()` builds the codes
# about three times faster than `match()` (and so `factor()`) on these columns.
factorise_analysis_data <- function(data, max_levels = 1000) {
  for (column in names(data)) {
    values <- data[[column]]
    if (!is.character(values)) {
      next
    }

    distinct <- unique(values)
    if (sum(!is.na(distinct)) > max_levels) {
      next
    }

    observed <- sort(distinct, method = "radix")
    data[[column]] <- structure(
      data.table::chmatch(values, observed),
      levels = observed,
      class = "factor"
    )
  }

  data
}
