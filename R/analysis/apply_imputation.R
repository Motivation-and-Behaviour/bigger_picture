#' Fill a prepared dataset with one imputation
#'
#' Puts one imputation's cells into the prepared tables, works out a
#' summed total again from its parts where one of them was imputed, and
#' works out the rest of a child's screen use again (`other_<exposure>`) where the
#' exposure or the total was imputed, as the total less the exposure.
#' Outcomes are never filled: a row without an outcome is not analysed.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param imputed Output of `impute_dataset()`, for one imputation or
#'   several bound together.
#' @param index The imputation to apply.
#' @param max_hours The plausibility limit a summed total is held to.
#' @return `prepared` with the imputed cells filled in.
apply_imputation <- function(prepared, imputed, index, max_hours = 24) {
  numeric <- imputed$numeric[imputed$numeric$imputation %in% index, ]
  categorical <- imputed$categorical[
    imputed$categorical$imputation %in% index,
  ]
  check <- function(target, row, name) {
    if (!all(is.na(target[row]))) {
      stop(
        "apply_imputation(): an imputed cell of `",
        name,
        "` already has a value.",
        call. = FALSE
      )
    }
  }
  for (name in unique(numeric$variable)) {
    own <- numeric[numeric$variable == name, ]
    check(prepared$exposures[[name]], own$row, name)
    prepared$exposures[[name]][own$row] <- own$value
  }
  for (name in unique(categorical$variable)) {
    own <- categorical[categorical$variable == name, ]
    check(prepared$base[[name]], own$row, name)
    prepared$base[[name]][own$row] <- own$value
  }
  # A summed total with a part imputed: the sum of the unit's summed rows.
  exposures <- prepared$exposures
  open <- which(
    exposures$total_source %in%
      "summed" &
      is.na(exposures[[bp_total_exposure()]]) &
      seq_len(nrow(exposures)) %in% numeric$row
  )
  summed_now <- integer()
  for (parts in unique(exposures$total_rows[open])) {
    rows <- open[exposures$total_rows[open] == parts]
    names_of <- strsplit(parts, "|", fixed = TRUE)[[1]]
    sums <- rowSums(as.matrix(exposures[rows, names_of, drop = FALSE]))
    prepared$exposures[[bp_total_exposure()]][rows] <- pmin(sums, max_hours)
    summed_now <- c(summed_now, rows[!is.na(sums)])
  }
  total <- prepared$exposures[[bp_total_exposure()]]
  touched <- c(
    summed_now,
    numeric$row[numeric$variable %in% bp_total_exposure()]
  )
  for (name in grep("^other_", names(prepared$exposures), value = TRUE)) {
    exposure <- sub("^other_", "st_", name)
    if (!exposure %in% names(prepared$exposures)) {
      next
    }
    rows <- union(touched, numeric$row[numeric$variable %in% exposure])
    rows <- rows[is.na(prepared$exposures[[name]][rows])]
    prepared$exposures[[name]][rows] <- pmax(
      total[rows] - prepared$exposures[[exposure]][rows],
      0
    )
  }
  prepared
}
