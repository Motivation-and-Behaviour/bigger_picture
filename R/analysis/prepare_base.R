#' The base table of one dataset: one eligible row per participant and wave
#'
#' Applies the row rules of the preparation step (ANALYSIS_PLAN.md, Step 4):
#'
#' - rows aged under the limit are kept; older rows and rows with no age are
#'   dropped (DR-0012);
#' - in trials, baseline rows and control-arm rows are kept;
#' - `Unknown` becomes NA on every categorical column;
#' - each row gets its stage 1 unit (dataset by wave by country), the order
#'   of its wave in time, a school stage and a weight normalised within unit.
#'
#' @param rows A dataset's primary-measure harmonised rows.
#' @param info The dataset's row from `analysis_dataset_info()`.
#' @param carry Names of the columns to carry into the base table.
#' @param settings From `analysis_settings()`.
#' @return A list: `base` (the kept rows), `keep` (a logical index into
#'   `rows`) and `dropped` (rows dropped per wave and reason).
prepare_base <- function(rows, info, carry, settings = analysis_settings()) {
  n <- nrow(rows)
  dataset_id <- as.character(info$dataset_id)
  wave <- as.character(rows$wave)
  age <- as.numeric(rows$age_years)
  arm <- if ("rct_arm" %in% names(rows)) rows$rct_arm else rep(NA, n)
  arm <- as.character(arm)
  baseline <- as.character(info$baseline_wave)
  has_baseline <- length(baseline) == 1 && !is.na(baseline)

  reason <- rep(NA_character_, n)
  reason[is.na(age)] <- "age_missing"
  reason[!is.na(age) & age >= settings$age_limit] <- "age_over_limit"
  trial <- is_trial_arm(arm)
  is_baseline <- if (has_baseline) wave %in% baseline else rep(FALSE, n)
  if (has_baseline && !any(is_baseline)) {
    stop(
      "Dataset ",
      dataset_id,
      ": baseline_wave '",
      baseline,
      "' in analysis/datasets.csv is not a wave in the data.",
      call. = FALSE
    )
  }
  if (any(trial)) {
    documented <- !is.na(info$notes) && nzchar(info$notes)
    if (!has_baseline && !documented) {
      stop(
        "Dataset ",
        dataset_id,
        " has allocated trial arms but analysis/datasets.csv gives neither",
        " a baseline_wave nor a note saying why there is none.",
        call. = FALSE
      )
    }
    after <- trial & !is_baseline & !tolower(arm) %in% "control"
    reason[is.na(reason) & after] <- "trial_arm_after_baseline"
  }
  keep <- is.na(reason)

  dropped <- dplyr::count(
    tibble::tibble(wave = wave[!keep], reason = reason[!keep]),
    .data$wave,
    .data$reason
  )
  dropped$dataset_id <- rep(dataset_id, nrow(dropped))

  carry <- intersect(carry, names(rows))
  base <- rows[keep, carry]
  for (column in names(base)) {
    values <- base[[column]]
    if (is.factor(values)) {
      values <- as.character(values)
    }
    if (is.character(values)) {
      values[values %in% "Unknown"] <- NA_character_
      base[[column]] <- values
    }
  }
  if (all(c("fam_siblings_any", "fam_siblings_n") %in% names(base))) {
    # The guide leaves this row to the analysis where a count is recorded.
    derived <- ifelse(as.numeric(base$fam_siblings_n) > 0, "TRUE", "FALSE")
    base$fam_siblings_any <- dplyr::coalesce(base$fam_siblings_any, derived)
  }
  base$dataset_id <- rep(dataset_id, nrow(base))
  base$wave <- wave[keep]
  country <- if ("country" %in% names(base)) base$country else NA_character_
  base$unit <- paste(
    dataset_id,
    dplyr::coalesce(base$wave, ""),
    dplyr::coalesce(as.character(country), ""),
    sep = "|"
  )
  base$is_baseline <- is_baseline[keep]
  base$wave_order <- order_waves(base, info$wave_order)
  base$school_stage <- as.character(cut(
    as.numeric(base$age_years),
    breaks = settings$school_stage_breaks,
    labels = settings$school_stage_labels,
    right = FALSE
  ))
  base$survey_weight_norm <- normalise_weight(base)

  list(base = base, keep = keep, dropped = dropped)
}

# Rank a dataset's occasions in time. `wave` is a free-text label whose string
# order is not always time order, so the rank is by median collection year,
# then median age, then label. Where that is wrong (waves within one year
# that different subsamples attended), analysis/datasets.csv declares the
# order, as wave labels separated by `|`.
order_waves <- function(base, declared = NA_character_) {
  if (nrow(base) == 0) {
    return(integer())
  }
  if (length(declared) == 1 && !is.na(declared) && nzchar(declared)) {
    labels <- strsplit(declared, "|", fixed = TRUE)[[1]]
    unlisted <- setdiff(unique(base$wave), labels)
    if (length(unlisted) > 0) {
      stop(
        "Dataset ",
        base$dataset_id[1],
        ": wave_order in analysis/datasets.csv does not list wave(s) ",
        paste(unlisted, collapse = ", "),
        call. = FALSE
      )
    }
    return(match(base$wave, labels))
  }
  year <- if ("data_year" %in% names(base)) base$data_year else NA_real_
  waves <- dplyr::summarise(
    tibble::tibble(
      wave = base$wave,
      year = as.numeric(year),
      age = as.numeric(base$age_years)
    ),
    year = bp_stat(.data$year, stats::median),
    age = bp_stat(.data$age, stats::median),
    .by = "wave"
  )
  waves <- waves[order(waves$year, waves$age, waves$wave, na.last = TRUE), ]
  waves$wave_order <- seq_len(nrow(waves))
  waves$wave_order[match(base$wave, waves$wave)]
}

# The released weight divided by its mean within unit, zeros kept as zero.
# Released weights change scale by orders of magnitude between waves and
# countries, so a model pooling units on the raw weight would drop some.
normalise_weight <- function(base) {
  if (!"survey_weight" %in% names(base) || nrow(base) == 0) {
    return(rep(NA_real_, nrow(base)))
  }
  weight <- as.numeric(base$survey_weight)
  unit_mean <- stats::ave(weight, base$unit, FUN = function(x) {
    mean(x, na.rm = TRUE)
  })
  out <- weight / unit_mean
  out[!is.finite(out)] <- NA_real_
  out
}
