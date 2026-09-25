#' Tidier for BPIPD-97 (ESPAD)
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per responding student per wave (each wave is an
#'   independent cross-section)
#'
#' Keeps 2015 and 2019, the waves with screen-use items, and drops the
#' country-cycles that fielded no gaming-hours item (C40a, C40b, X15_C40b).
tidy_BPIPD_97 <- function(raw_dataset, spec) {
  espad <- tibble::as_tibble(raw_dataset$data[[1]])
  espad <- dplyr::filter(espad, ESPAD_Year >= 2015)
  has_screen <- !is.na(espad$C40a) | !is.na(espad$C40b) | !is.na(espad$X15_C40b)
  cycle <- paste(espad$ESPAD_Year, as.numeric(unclass(espad$COUNTRY)))
  espad[cycle %in% unique(cycle[has_screen]), ]
}
