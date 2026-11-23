#' Put a pooled effect in the evidence platform's impact bands
#'
#' The platform rates the size of a relationship in bands (DR-0020). For
#' this analysis the bands are applied to the contrast of 1 against 4
#' hours a day, in SD, and are read from the estimate and its interval,
#' never from a p-value alone:
#'
#' - 0, no clear relationship: the equivalence test shows the effect lies
#'   within the smallest effect of interest;
#' - 1 to 3, small, moderate or strong: the estimate is at least the
#'   smallest effect of interest and its 95% interval excludes no
#'   difference, with the band taken from the estimate's size and the sign
#'   from its direction for the child;
#' - not rated: anything else, which is too uncertain to say. An estimate
#'   that is neither shown to be negligible nor clearly different from
#'   zero is not called "no relationship".
#'
#' Estimates are aligned, positive where more screen use goes with a
#' better outcome, so a negative rating is harmful.
#'
#' @param estimate,low,high The effect on the contrast and its 95%
#'   interval, in SD.
#' @param p_equivalence The equivalence test's p-value.
#' @param settings From `analysis_settings()`: `rating_bands` and
#'   `smallest_effect`.
#' @return A tibble, one row per estimate: `rating` (-3 to 3, NA where not
#'   rated) and `rating_label`.
rate_effect <- function(
  estimate,
  low,
  high,
  p_equivalence,
  settings = default_analysis_settings()
) {
  bands <- settings$rating_bands
  if (
    is.null(bands) ||
      is.unsorted(bands, strictly = TRUE) ||
      !isTRUE(all.equal(bands[[1]], settings$smallest_effect))
  ) {
    stop(
      "rate_effect(): `rating_bands` must rise from the smallest effect of ",
      "interest.",
      call. = FALSE
    )
  }
  known <- !is.na(estimate) & !is.na(low) & !is.na(high)
  negligible <- known & !is.na(p_equivalence) & p_equivalence < 0.05
  clear <- known &
    !negligible &
    abs(estimate) >= bands[[1]] &
    (low > 0 | high < 0)
  size <- findInterval(abs(estimate), bands)
  rating <- rep(NA_integer_, length(estimate))
  rating[negligible] <- 0L
  rating[clear] <- as.integer(sign(estimate[clear]) * size[clear])
  words <- c("no clear relationship", names(bands))
  label <- ifelse(
    is.na(rating),
    "not rated: too uncertain to say",
    ifelse(
      rating == 0L,
      words[1],
      paste0(
        words[abs(rating) + 1L],
        ifelse(rating < 0, ", harmful", ", beneficial")
      )
    )
  )
  tibble::tibble(rating = rating, rating_label = label)
}
