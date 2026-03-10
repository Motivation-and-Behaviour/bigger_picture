create_dataset_plan <- function(dataset_list, dataset_specs) {
  # Extract key fields from dataset specs
  specs_tbl <- dplyr::bind_rows(lapply(dataset_specs, function(s) {
    dplyr::tibble(
      dataset_id = as.character(s$dataset_id),
      status = as.character(s$status %||% NA_character_),
      spec_path = as.character(s$.spec_file %||% NA_character_),
      has_harmonisation = dataset_has_harmonisation(as.character(s$dataset_id))
    )
  }))

  # Join dataset specs to discovered dataset folders
  plan <- dplyr::inner_join(
    dataset_list,
    specs_tbl,
    by = "dataset_id"
  )

  # Filter to the ones you want in the pipeline
  plan <- dplyr::filter(plan, status %in% c("in_progress", "harmonised"))

  # Create nice stable names for static branching
  plan <- dplyr::mutate(
    plan,
    dataset_key = paste0("BPIPD_", dataset_id)
  )

  dplyr::arrange(plan, as.integer(dataset_id))
}
