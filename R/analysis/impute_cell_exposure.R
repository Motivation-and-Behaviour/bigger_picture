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
#' child's mean exposure over their other waves, sex and the tier
#' covariates, with a covariate's unknown values as a group of their own,
#' and the rest of the child's screen use where the model adjusts for it
#' and it has no gap to fill.
#'
#' Nothing is imputed for a row the model cannot use: one with no age, no
#' weight in a weighted unit, or a rest of screen use that is missing and
#' cannot be imputed.
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
  done <- function(data, imputed = 0L) {
    attr(data, "imputed") <- as.integer(imputed)
    data
  }
  if (!anyNA(data$x) || n < settings$min_unit_rows) {
    return(done(data))
  }
  # Rows the model cannot use whatever is imputed: no weight in a weighted
  # unit, or no age to place the child by.
  weighted <- any(!is.na(data$weight) & data$weight > 0)
  usable <- !is.na(data$age) &
    (!weighted | (!is.na(data$weight) & data$weight > 0))
  # Where each target may be imputed: the rows observed enough of it, and
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
    !seen & usable & by_age >= settings$impute_min_observed_age
  })
  names(eligible) <- targets
  if ("other" %in% targets) {
    # A row whose rest of screen use is missing and cannot be imputed is
    # dropped by the model, so its exposure is not imputed and it does not
    # inform the draws.
    usable <- usable & (!is.na(data$other) | eligible$other)
    eligible$x <- eligible$x & usable
  }
  if (!any(eligible$x)) {
    return(done(data))
  }
  joint <- "other" %in% targets && any(eligible$other)

  # Predictors. A covariate's gaps are a group of their own, so a
  # covariate the model adjusts for is always in the exposure's model.
  predictors <- data.frame(y = as.numeric(data$y), age = data$age)
  if (weighted) {
    predictors$weight <- log(ifelse(usable, data$weight, 1))
  }
  if ("x_other_waves" %in% names(data) && any(!is.na(data$x_other_waves))) {
    has <- !is.na(data$x_other_waves)
    predictors$other_waves <- ifelse(
      has,
      data$x_other_waves,
      mean(data$x_other_waves[has])
    )
    predictors$no_other_wave <- as.numeric(!has)
  }
  for (name in c("sex", bp_stage1_covariates())) {
    values <- data[[name]]
    if (!all(is.na(values))) {
      predictors[[name]] <- dplyr::coalesce(values, "(unknown)")
    }
  }
  predictors$age[is.na(predictors$age)] <- mean(predictors$age, na.rm = TRUE)
  varies <- vapply(
    predictors,
    function(column) dplyr::n_distinct(column[usable]) >= 2,
    logical(1)
  )
  predictors <- predictors[varies]
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
  # One draw of a target by predictive mean matching. The match is between
  # draws of the regression for both the donors and the rows imputed, which
  # keeps the donors varied where the outcome takes few values.
  draw <- function(name, extra = NULL) {
    values <- data[[name]]
    seen <- !is.na(values) & usable
    mice::mice.impute.pmm(
      values,
      ry = seen,
      x = independent(cbind(design, extra), seen),
      wy = eligible[[name]],
      matchtype = 2L
    )
  }
  filled <- tryCatch(
    withr::with_seed(seed, {
      if (!joint) {
        # The rest of the child's screen use, where the model adjusts for
        # it and it is known, is a predictor like any other.
        rest <- if ("other" %in% targets) {
          ifelse(usable, data$other, 0)
        }
        list(x = draw("x", extra = rest))
      } else {
        # Both have gaps: each in turn given the other, from a random
        # start.
        current <- lapply(targets, function(name) {
          values <- data[[name]]
          gaps <- eligible[[name]]
          values[gaps] <- sample(
            values[!is.na(values)],
            sum(gaps),
            replace = TRUE
          )
          ifelse(usable, values, 0)
        })
        names(current) <- targets
        drawn <- list()
        for (iteration in seq_len(settings$imputation_iterations)) {
          for (name in targets) {
            if (!any(eligible[[name]])) {
              next
            }
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
  for (name in names(filled)) {
    stopifnot(
      length(filled[[name]]) == sum(eligible[[name]]),
      !anyNA(filled[[name]])
    )
    data[[name]][eligible[[name]]] <- filled[[name]]
  }
  done(data, sum(eligible$x))
}

# A seed from text, for a unit's or a cell's imputation. `digest2int()` can
# return NA, though almost never; a seed must not be.
imputation_seed <- function(...) {
  seed <- digest::digest2int(paste(..., sep = "|"))
  if (is.na(seed)) 1L else seed
}
