#' How strongly two outcome rows' slopes in one unit share their error
#'
#' Two rows of one specific outcome fitted in one unit are fitted on many of
#' the same children, so their slopes on screen use err together. The
#' correlation between the two slopes is estimated the way a robust standard
#' error is: each child's contribution to a slope is the product of their
#' screen use, with the model's other terms taken out, and their residual;
#' the contributions are summed within the clusters stage 1 uses, and the
#' two rows' sums are correlated across clusters. Children with only one of
#' the two scores contribute to one slope only, so partial overlap lowers
#' the correlation by itself, and two rows with no cluster in common get
#' zero.
#'
#' Each row is prepared as `fit_stage1_unit()` prepares it (weights, the age
#' term, sex, the rows those rules leave out), but every score is treated as
#' numeric in a linear model, so for a binary or short ordinal row the
#' correlation is an approximation. It sets the weights of the working
#' model in `pool_tree_group()`, whose inference is robust to a wrong
#' covariance.
#'
#' Only pairs within one specific outcome are estimated, because pooling
#' uses no other.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param cells A tibble of `exposure` and `outcome` rows.
#' @param outcome_registry The outcome registry.
#' @param settings From `analysis_settings()`.
#' @return A tibble with one row per unit, exposure and pair of outcome rows
#'   the unit could fit: `r`, the correlation between the two slopes (NA
#'   where too few rows have both scores to estimate it); `overlap`, the
#'   share of rows the two fits have in common; `n_shared`; and `basis`
#'   (`estimated`, `no shared rows` or `too few shared rows`). NULL when the
#'   dataset is not included or holds no pair.
stage1_correlations <- function(
  prepared,
  cells,
  outcome_registry,
  settings = analysis_settings()
) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  cells <- cells[
    cells$exposure %in%
      names(prepared$exposures) &
      cells$outcome %in% names(prepared$outcomes),
  ]
  registry <- outcome_registry$rows
  cells$node <- registry$node[match(cells$outcome, registry$variable_name)]
  cells <- cells[!is.na(cells$node), ]
  if (nrow(cells) < 2) {
    return(NULL)
  }
  column <- function(name) {
    if (name %in% names(base)) base[[name]] else rep(NA, nrow(base))
  }
  frame <- data.frame(
    row = seq_len(nrow(base)),
    age = as.numeric(base$age_years),
    sex = as.character(column("sex")),
    weight = as.numeric(base$survey_weight_norm),
    cluster = as.character(column("survey_cluster")),
    family = as.character(column("family_id")),
    stringsAsFactors = FALSE
  )
  value <- function(outcome) {
    if (outcome %in% names(prepared$outcomes_z)) {
      prepared$outcomes_z[[outcome]]
    } else {
      as.numeric(prepared$outcomes[[outcome]])
    }
  }
  unit_rows <- split(seq_len(nrow(base)), base$unit)

  results <- list()
  for (group in split(cells, paste(cells$exposure, cells$node))) {
    if (nrow(group) < 2) {
      next
    }
    x <- prepared$exposures[[group$exposure[1]]]
    values <- lapply(group$outcome, value)
    pairs <- utils::combn(nrow(group), 2)
    for (unit in names(unit_rows)) {
      scores <- lapply(values, function(y) {
        index <- unit_rows[[unit]]
        index <- index[!is.na(x[index]) & !is.na(y[index])]
        data <- frame[index, ]
        data$x <- x[index]
        data$y <- y[index]
        stage1_scores(data, settings)
      })
      for (p in seq_len(ncol(pairs))) {
        a <- scores[[pairs[1, p]]]
        b <- scores[[pairs[2, p]]]
        if (is.null(a) || is.null(b)) {
          next
        }
        n_shared <- length(intersect(a$row, b$row))
        overlap <- n_shared / sqrt(as.numeric(nrow(a)) * nrow(b))
        r <- NA_real_
        basis <- "too few shared rows"
        if (n_shared == 0 && !any(a$cluster %in% b$cluster)) {
          r <- 0
          basis <- "no shared rows"
        } else if (n_shared >= settings$min_unit_rows) {
          sum_a <- rowsum(a$score, a$cluster)
          sum_b <- rowsum(b$score, b$cluster)
          clusters <- intersect(rownames(sum_a), rownames(sum_b))
          r <- sum(sum_a[clusters, 1] * sum_b[clusters, 1]) /
            sqrt(sum(sum_a^2) * sum(sum_b^2))
          basis <- "estimated"
        }
        results[[length(results) + 1]] <- tibble::tibble(
          unit = unit,
          exposure = group$exposure[1],
          outcome_a = group$outcome[pairs[1, p]],
          outcome_b = group$outcome[pairs[2, p]],
          r = r,
          overlap = overlap,
          n_shared = n_shared,
          basis = basis
        )
      }
    }
  }
  out <- dplyr::bind_rows(results)
  if (nrow(out) == 0) {
    return(NULL)
  }
  out$dataset_id <- rep(as.character(base$dataset_id[1]), nrow(out))
  dplyr::relocate(out, "dataset_id")
}

# Each row's contribution to the slope of a linear model of `y` on `x`, on
# the rows and with the terms and clusters `fit_stage1_unit()` would use.
# NULL where stage 1 would not fit the unit, or the score does not vary.
stage1_scores <- function(data, settings = analysis_settings()) {
  if (nrow(data) == 0) {
    return(NULL)
  }
  weighted <- any(!is.na(data$weight) & data$weight > 0)
  if (weighted) {
    data <- data[!is.na(data$weight) & data$weight > 0, ]
  }
  terms <- "x"
  age_values <- dplyr::n_distinct(data$age, na.rm = TRUE)
  age_span <- if (age_values == 0) {
    0
  } else {
    diff(stats::quantile(data$age, c(0.05, 0.95), na.rm = TRUE, names = FALSE))
  }
  spline_works <- age_span >= 2 &&
    !inherits(try(splines::ns(data$age, df = 2), silent = TRUE), "try-error")
  if (age_values > 1 && age_values <= 5) {
    terms <- c(terms, "factor(age)")
  } else if (spline_works) {
    terms <- c(terms, "splines::ns(age, df = 2)")
  } else if (age_values > 1) {
    terms <- c(terms, "age")
  }
  if (age_values > 1) {
    data <- data[!is.na(data$age), ]
  }
  sex_known <- data$sex %in% c("Female", "Male")
  sex_counts <- table(data$sex[sex_known])
  if (
    length(sex_counts) == 2 &&
      min(sex_counts) >= 5 &&
      mean(sex_known) >= settings$sex_known_min
  ) {
    data <- data[sex_known, ]
    terms <- c(terms, "sex")
  }
  n <- nrow(data)
  if (
    n < settings$min_unit_rows ||
      stats::sd(data$x) == 0 ||
      stats::sd(data$y) == 0
  ) {
    return(NULL)
  }
  w <- if (weighted) data$weight else rep(1, n)
  design <- stats::model.matrix(
    stats::reformulate(terms),
    data = data
  )
  fit <- stats::lm.wfit(design, data$y, w)
  others <- design[, colnames(design) != "x", drop = FALSE]
  x_left <- stats::lm.wfit(others, data$x, w)$residuals
  cluster <- if (dplyr::n_distinct(data$cluster, na.rm = TRUE) > 1) {
    ifelse(is.na(data$cluster), paste0("\r", data$row), data$cluster)
  } else if (!anyNA(data$family) && dplyr::n_distinct(data$family) < n) {
    data$family
  } else {
    paste0("\r", data$row)
  }
  data.frame(
    row = data$row,
    cluster = cluster,
    score = w * x_left * fit$residuals,
    stringsAsFactors = FALSE
  )
}
