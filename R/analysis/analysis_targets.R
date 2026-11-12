#' The analysis section of the pipeline
#'
#' Everything downstream of harmonisation (DR-0007). Per-dataset targets read
#' that dataset's harmonised table and its one-row `dataset_info`, so a new or
#' changed dataset reruns only its own targets and the combined targets they
#' feed. Nothing here names a dataset: the plan comes from the dataset specs.
#'
#' @param harmonise_plan The dataset plan rows that have a harmonisation
#'   mapping, with `dataset_key` and `dataset_id`.
#' @return A list of targets.
analysis_targets <- function(harmonise_plan) {
  shared <- list(
    targets::tar_target_raw("settings", quote(analysis_settings())),
    targets::tar_target_raw(
      "dataset_spec_files",
      quote(list_dataset_spec_files()),
      format = "file"
    ),
    targets::tar_target_raw(
      "analysis_datasets_file",
      quote(bp_analysis_datasets_path()),
      format = "file"
    ),
    targets::tar_target_raw(
      "analysis_datasets",
      bquote(read_analysis_datasets(
        analysis_datasets_file,
        .(as.character(harmonise_plan$dataset_id))
      ))
    )
  )
  shared <- c(
    shared,
    list(
      targets::tar_target_raw(
        "development_cells_file",
        quote(bp_development_cells_path()),
        format = "file"
      ),
      targets::tar_target_raw(
        "development_cells",
        quote(read_development_cells(
          development_cells_file,
          outcome_registry,
          exposure_registry
        ))
      ),
      targets::tar_target_raw(
        "invariants_file",
        quote(bp_invariants_path()),
        format = "file"
      ),
      targets::tar_target_raw(
        "invariants",
        quote(read_registry_csv(invariants_file, bp_invariants_columns()))
      )
    )
  )
  if (nrow(harmonise_plan) == 0) {
    return(shared)
  }

  values <- tibble::tibble(
    dataset_key = harmonise_plan$dataset_key,
    dataset_id = as.character(harmonise_plan$dataset_id),
    harmonised = rlang::syms(paste0("harmonised_", harmonise_plan$dataset_key)),
    harmonisation_config = rlang::syms(
      paste0("harmonisation_config_", harmonise_plan$dataset_key)
    )
  )
  row_level <- targets::tar_resources(
    qs = targets::tar_resources_qs(nthreads = 4L),
    crew = targets::tar_resources_crew(controller = "row_level")
  )
  # Three targets per dataset read row-level data: the summaries, the
  # prepared tables and their review. Keeping it to three bounds how many
  # copies of one dataset are in memory at once. They are transient, with a
  # garbage collection first, so a worker does not keep each dataset it has
  # loaded for the rest of the run, and they run in the small `row_level`
  # worker pool.
  per_dataset <- tarchetypes::tar_map(
    values = values,
    names = "dataset_key",
    targets::tar_target_raw(
      "dataset_info",
      quote(analysis_dataset_info(analysis_datasets, dataset_id))
    ),
    targets::tar_target_raw(
      "dataset_summary",
      quote(summarise_dataset(
        harmonised,
        dataset_info,
        dataschema,
        invariants,
        exposure_registry,
        settings
      )),
      memory = "transient",
      garbage_collection = TRUE,
      resources = row_level
    ),
    targets::tar_target_raw(
      "exceptions_file",
      quote(find_analysis_exceptions(dataset_id)),
      format = "file",
      # Looked for on every run: a dataset with no file tracks no path, so
      # nothing else would notice one being added.
      cue = targets::tar_cue(mode = "always")
    ),
    targets::tar_target_raw(
      "exceptions",
      quote(read_analysis_exceptions(
        exceptions_file,
        dataschema,
        outcome_registry
      ))
    ),
    targets::tar_target_raw(
      "prepared",
      quote(prepare_analysis_data(
        harmonised,
        dataset_info,
        outcome_registry,
        exposure_registry,
        derivation_registry,
        dataschema,
        exceptions,
        settings
      )),
      memory = "transient",
      garbage_collection = TRUE,
      resources = row_level
    ),
    targets::tar_target_raw(
      "stage1",
      quote(fit_stage1(
        prepared,
        development_cells,
        outcome_registry,
        dataschema,
        settings
      )),
      memory = "transient",
      garbage_collection = TRUE,
      resources = row_level
    ),
    targets::tar_target_raw(
      "onestage_rows",
      quote(extract_cell_rows(
        prepared,
        settings$onestage_cell[["exposure"]],
        settings$onestage_cell[["outcome"]]
      )),
      memory = "transient",
      garbage_collection = TRUE,
      resources = row_level
    ),
    targets::tar_target_raw(
      "prepared_review",
      quote(review_prepared(
        prepared,
        harmonisation_config$variables,
        outcome_registry,
        settings
      )),
      memory = "transient",
      garbage_collection = TRUE,
      resources = row_level
    )
  )

  bind <- function(name, from, part) {
    tarchetypes::tar_combine_raw(
      name,
      per_dataset[[from]],
      command = bquote(bind_part(list(!!!.x), .(part)))
    )
  }
  summaries <- function(name, part) bind(name, "dataset_summary", part)
  reviews <- function(name, part) bind(name, "prepared_review", part)
  combined <- list(
    tarchetypes::tar_combine_raw(
      "data_manifest",
      per_dataset[["dataset_summary"]],
      command = quote(combine_data_manifest(bind_part(list(!!!.x), "manifest")))
    ),
    summaries("grain_all", "grain"),
    summaries("numeric_summary_all", "numeric_summary"),
    summaries("level_summary_all", "level_summary"),
    summaries("unit_summary_all", "unit_summary"),
    summaries("unit_counts_all", "unit_counts"),
    summaries("invariant_check_all", "invariant_check"),
    summaries("day_check_all", "day_check"),
    reviews("analysis_row_audit", "row_audit"),
    reviews("rows_dropped_all", "dropped"),
    reviews("prepared_summary_all", "summary"),
    reviews("prepared_flags_all", "flags"),
    tarchetypes::tar_combine_raw(
      "stage1_all",
      per_dataset[["stage1"]],
      command = quote(dplyr::bind_rows(!!!.x))
    ),
    tarchetypes::tar_combine_raw(
      "onestage_rows_all",
      per_dataset[["onestage_rows"]],
      command = quote(dplyr::bind_rows(!!!.x))
    ),
    targets::tar_target_raw(
      "onestage_check",
      quote(fit_onestage_check(onestage_rows_all, settings))
    ),
    targets::tar_target_raw(
      "stage2_development",
      quote(pool_stage2_cells(stage1_all, settings))
    ),
    targets::tar_target_raw(
      "derived_codebook",
      quote(derived_variable_codebook(derivation_registry, settings))
    ),
    targets::tar_target_raw(
      "violations",
      quote(check_analysis_data(
        numeric_summary_all,
        level_summary_all,
        grain_all,
        invariant_check_all,
        day_check_all,
        invariants,
        dataschema,
        exposure_registry
      ))
    ),
    targets::tar_target_raw(
      "coverage",
      quote(summarise_coverage(
        numeric_summary_all,
        level_summary_all,
        grain_all,
        dataschema
      ))
    ),
    targets::tar_target_raw(
      "missingness",
      quote(summarise_missingness(unit_counts_all, unit_summary_all))
    ),
    targets::tar_target_raw(
      "dataset_flow",
      bquote(summarise_dataset_flow(
        dataset_spec_files,
        .(as.character(harmonise_plan$dataset_id)),
        data_manifest
      ))
    )
  )

  reports <- list(
    tarchetypes::tar_quarto_raw(
      "report_data_health",
      path = fs::path(bp_analysis_reports_dir(), "data_health.qmd")
    ),
    tarchetypes::tar_quarto_raw(
      "report_harmonisation_diagnostics",
      path = fs::path(
        bp_analysis_reports_dir(),
        "harmonisation_diagnostics.qmd"
      )
    ),
    tarchetypes::tar_quarto_raw(
      "report_preparation",
      path = fs::path(bp_analysis_reports_dir(), "preparation.qmd")
    ),
    tarchetypes::tar_quarto_raw(
      "report_thin_slice",
      path = fs::path(bp_analysis_reports_dir(), "thin_slice.qmd")
    )
  )

  list(shared, per_dataset, combined, reports)
}
