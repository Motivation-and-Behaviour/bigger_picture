#' Generates a list of datasets in the data directory that match the pattern
#'
#' @title List datasets
#' @param data_dir
#' @param pattern
#' @return
#' @author Taren Sanders
#' @export
list_datasets <- function(
  data_dir = bp_data_dir(),
  pattern = bp_dataset_dir_pattern()
) {
  dirs <- fs::dir_ls(data_dir, type = "directory")
  dirs <- dirs[stringr::str_detect(fs::path_file(dirs), pattern)]

  dplyr::tibble(
    dataset_dir = dirs,
    folder_name = fs::path_file(dirs),
    dataset_id = stringr::str_match(folder_name, "^BPIPD-(\\d+)")[, 2],
    dataset_name = stringr::str_trim(
      stringr::str_replace(folder_name, "^BPIPD-\\d+\\s-\\s", "")
    )
  ) |>
    dplyr::arrange(as.integer(dataset_id))
}
