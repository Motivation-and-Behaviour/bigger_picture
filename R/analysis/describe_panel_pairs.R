#' Who is kept and lost between a panel's successive waves
#'
#' For each exposure and each pair of successive waves that have it, the
#' children with the exposure at the earlier wave, how many are seen again
#' at the later wave, and how the screen use of those who stay differs from
#' that of those who leave (DR-0018). It fits no model and relates screen
#' use to no outcome.
#'
#' @param prepared Output of `prepare_analysis_data()` for one dataset.
#' @param exposures Exposure columns to describe.
#' @return NULL where the dataset follows nobody; otherwise a tibble, one
#'   row per exposure and pair of waves: `n_earlier`, `n_retained`,
#'   `retained`, `difference_sd` (mean screen use of those who stay less
#'   that of those who leave, in SD of the earlier wave) and
#'   `age_difference` (their mean age less the leavers', in years).
describe_panel_pairs <- function(prepared, exposures) {
  if (is.null(prepared) || nrow(prepared$base) == 0) {
    return(NULL)
  }
  base <- prepared$base
  if (!anyDuplicated(base$participant_id) > 0 || anyNA(base$wave)) {
    return(NULL)
  }
  waves <- dplyr::distinct(base[c("wave", "wave_order")])
  waves <- waves$wave[order(waves$wave_order)]
  wave_rows <- split(seq_len(nrow(base)), base$wave)
  out <- list()
  for (exposure in intersect(exposures, names(prepared$exposures))) {
    x <- prepared$exposures[[exposure]]
    measured <- waves[vapply(
      waves,
      function(wave) any(!is.na(x[wave_rows[[wave]]])),
      logical(1)
    )]
    for (j in seq_len(max(length(measured) - 1, 0))) {
      from <- wave_rows[[measured[j]]]
      from <- from[!is.na(x[from])]
      to <- wave_rows[[measured[j + 1]]]
      stays <- base$participant_id[from] %in% base$participant_id[to]
      spread <- stats::sd(x[from])
      gap <- function(values) {
        if (!any(stays) || all(stays)) {
          return(NA_real_)
        }
        mean(values[stays], na.rm = TRUE) - mean(values[!stays], na.rm = TRUE)
      }
      out[[length(out) + 1]] <- tibble::tibble(
        dataset_id = as.character(base$dataset_id[1]),
        exposure = exposure,
        wave_from = measured[j],
        wave_to = measured[j + 1],
        n_earlier = length(from),
        n_retained = sum(stays),
        retained = mean(stays),
        difference_sd = gap(x[from]) / spread,
        age_difference = gap(base$age_years[from])
      )
    }
  }
  if (length(out) == 0) {
    return(NULL)
  }
  dplyr::bind_rows(out)
}
