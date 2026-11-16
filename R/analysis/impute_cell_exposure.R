#' Impute the screen use missing from one unit's rows for one outcome
#'
#' The exposure of a model is imputed for that model: among the unit's rows
#' that have the outcome, a missing exposure is drawn by predictive mean
#' matching from a model holding that outcome and the covariates the
#' analysis model can use (DR-0019). The imputation model therefore
#' contains the relation the analysis estimates, whichever outcome it is.
#' Where the analysis also adjusts for the rest of the child's screen use
#' (`other`), the two are imputed in turn, each given the other.
#'
#' An exposure is imputed only where its missingness is sporadic:
#'
#' - it is known for at least `impute_min_observed` of the rows, on at
#'   least the minimum number of rows, with two values or more;
#' - and, row by row, only for children of an age (in whole years) at which
#'   the unit observed it for at least `impute_min_observed_age` of the
#'   rows. An item never asked of one age group is not imputed for that
#'   group.
#'
#' Rows left without the exposure are dropped by the model as before.
#'
#' The predictors are the outcome, age, the survey weight, in a panel the
#' child's mean exposure over their other waves, and sex and the tier
#' covariates where they are complete and vary in these rows.
#'
#' @param data The unit's rows that have the outcome: `x`, `y`, the columns
#'   of `stage1_frame()`, `x_other_waves` (NA where there is none), and
#'   `other` where the model adjusts for it.
#' @param seed An integer seed for this unit, cell and imputation.
#' @param settings From `analysis_settings()`.
#' @return `data` with imputed values of `x` (and `other`) filled in, and
#'   the attribute `imputed`, the number of rows whose `x` was imputed. If
#'   the draw failed nothing is filled, and the attribute `failed` holds
#'   the reason.
impute_cell_exposure <- function(
  data,
  seed,
  settings = default_analysis_settings()
) {
  n <- nrow(data)
  targets <- intersect(c("x", "other"), names(data))
  targets <- targets[vapply(targets, function(name) anyNA(data[[name]]), NA)]
  done <- function(data, imputed = 0L) {
    attr(data, "imputed") <- as.integer(imputed)
    data
  }
  if (length(targets) == 0 || n < settings$min_unit_rows) {
    return(done(data))
  }
  # Where each target may be imputed: the unit observed enough of it, and
  # enough of it at the child's age.
  age_year <- floor(data$age)
  eligible <- lapply(targets, function(name) {
    seen <- !is.na(data[[name]])
    enough <- mean(seen) >= settings$impute_min_observed &&
      sum(seen) >= settings$min_unit_rows &&
      dplyr::n_distinct(data[[name]], na.rm = TRUE) >= 2
    if (!enough) {
      return(rep(FALSE, n))
    }
    by_age <- stats::ave(as.numeric(seen), age_year, FUN = mean)
    !seen & !is.na(age_year) & by_age >= settings$impute_min_observed_age
  })
  names(eligible) <- targets
  targets <- targets[vapply(eligible, any, logical(1))]
  if (length(targets) == 0) {
    return(done(data))
  }

  # Predictors: complete in these rows, and varying.
  predictors <- data.frame(y = as.numeric(data$y), age = data$age)
  if (any(!is.na(data$weight) & data$weight > 0)) {
    predictors$weight <- log(data$weight)
  }
  if ("x_other_waves" %in% names(data)) {
    has <- !is.na(data$x_other_waves)
    predictors$other_waves <- ifelse(
      has,
      data$x_other_waves,
      mean(data$x_other_waves[has])
    )
    predictors$no_other_wave <- as.numeric(!has)
  }
  for (name in c("sex", bp_stage1_covariates())) {
    predictors[[name]] <- data[[name]]
  }
  usable <- vapply(
    predictors,
    function(column) {
      !anyNA(column) &&
        all(is.finite(column) | is.character(column)) &&
        dplyr::n_distinct(column) >= 2
    },
    logical(1)
  )
  predictors <- predictors[usable]
  if (!"y" %in% names(predictors)) {
    return(done(data))
  }
  design <- stats::model.matrix(~., predictors)[, -1, drop = FALSE]

  # A predictor that repeats others (minority status beside ethnicity, a
  # group with no rows among those observed) is left out, as `mice` does.
  independent <- function(matrix, rows) {
    decomposed <- qr(cbind(1, matrix[rows, , drop = FALSE]))
    keep <- decomposed$pivot[seq_len(decomposed$rank)]
    matrix[, setdiff(keep, 1) - 1, drop = FALSE]
  }
  draw <- function(name, extra = NULL) {
    values <- data[[name]]
    seen <- !is.na(values)
    mice::mice.impute.pmm(
      values,
      ry = seen,
      x = independent(cbind(design, extra), seen),
      wy = eligible[[name]]
    )
  }
  filled <- tryCatch(
    withr::with_seed(seed, {
      if (length(targets) == 1) {
        stats::setNames(list(draw(targets)), targets)
      } else {
        # Two targets: each in turn given the other, from a random start.
        current <- lapply(targets, function(name) {
          values <- data[[name]]
          gaps <- is.na(values)
          values[gaps] <- sample(values[!gaps], sum(gaps), replace = TRUE)
          values
        })
        names(current) <- targets
        drawn <- list()
        for (iteration in seq_len(settings$imputation_iterations)) {
          for (name in targets) {
            other <- setdiff(targets, name)
            drawn[[name]] <- draw(name, extra = current[[other]])
            current[[name]][eligible[[name]]] <- drawn[[name]]
          }
        }
        drawn
      }
    }),
    error = function(e) conditionMessage(e)
  )
  # A draw that fails leaves the rows as they were, and says so.
  if (is.character(filled)) {
    attr(data, "failed") <- filled
    return(done(data))
  }
  for (name in targets) {
    range_seen <- range(data[[name]], na.rm = TRUE)
    stopifnot(
      length(filled[[name]]) == sum(eligible[[name]]),
      !anyNA(filled[[name]]),
      all(filled[[name]] >= range_seen[1] & filled[[name]] <= range_seen[2])
    )
    data[[name]][eligible[[name]]] <- filled[[name]]
  }
  done(data, if ("x" %in% targets) sum(eligible[["x"]]) else 0L)
}

# A seed from text, for a unit's or a cell's imputation. `digest2int()` can
# return NA, though almost never; a seed must not be.
imputation_seed <- function(...) {
  seed <- digest::digest2int(paste(..., sep = "|"))
  if (is.na(seed)) 1L else seed
}
