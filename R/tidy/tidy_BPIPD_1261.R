#' Tidier for BPIPD-1261 (JTUC Research Institute for Advancement of Living Standards)
#'
#' Five files (child and parent questionnaires, three one-day diaries) share
#' respondent order but carry no id, so they are bound side by side, prefixed.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per child (single cross-section, September 1995)
tidy_BPIPD_1261 <- function(raw_dataset, spec) {
  # The .dta copies carry the variable and value labels; the .csv twins hold the
  # same data without labels and are ignored.
  parts <- list(
    kodomo = raw_dataset$data$kodomo_stata,
    haha = raw_dataset$data$haha_stata,
    jikan9 = raw_dataset$data$jikan9_stata,
    jikan10 = raw_dataset$data$jikan10_stata,
    jikan13 = raw_dataset$data$jikan13_stata
  )

  # Every file numbers its items from F1/Q1, so the names collide across files.
  prefixed <- Map(
    function(tbl, source) {
      dplyr::rename_with(tbl, function(x) paste0(source, "_", x))
    },
    parts,
    names(parts)
  )

  dplyr::bind_cols(prefixed)
}
