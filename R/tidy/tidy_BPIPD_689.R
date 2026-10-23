#' Tidier for BPIPD-689 (Sucha)
#'
#' A single 2018 cross-section in one data sheet; the workbook's `variables`
#' sheet supplies the English question wording, attached as column labels.
#'
#' Input:
#' - `raw_dataset`: output of `read_dataset_from_spec()`
#' - `spec`: parsed dataset YAML
#'
#' Output:
#' - one tibble, one row per participant (single occasion)
tidy_BPIPD_689 <- function(raw_dataset, spec) {
  df <- tibble::as_tibble(raw_dataset$data$data)

  if (anyDuplicated(df$id) > 0) {
    stop("BPIPD-689: `id` does not uniquely identify rows.", call. = FALSE)
  }

  # Later blocks of the sheet repeat the names without a description.
  codebook <- readxl::read_excel(
    raw_dataset$codebook$codebook,
    sheet = "variables",
    col_types = "text"
  )
  described <- !is.na(codebook$variable) & !is.na(codebook$description)
  wording <- stats::setNames(
    codebook$description[described],
    codebook$variable[described]
  )

  # `sids_24` is `5 - sids_23` in every complete row, so the 24th wording does
  # not describe it.
  wording <- wording[names(wording) != "sids_24"]

  for (column in intersect(names(df), names(wording))) {
    attr(df[[column]], "label") <- wording[[column]]
  }

  # The released total equals the item sum in every row where both exist.
  df$igd <- dplyr::coalesce(df$igd, rowSums(df[paste0("igd_", 1:9)]))
  attr(df$igd, "label") <- paste(
    wording[["igd"]],
    "filled with the item sum where blank and all nine items are answered",
    sep = "; "
  )

  df
}
