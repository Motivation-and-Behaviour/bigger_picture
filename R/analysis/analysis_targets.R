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
  per_dataset <- tarchetypes::tar_map(
    values = values,
    names = "dataset_key",
    targets::tar_target_raw(
      "dataset_info",
      quote(analysis_dataset_info(analysis_datasets, dataset_id))
    ),
    targets::tar_target_raw(
      "manifest",
      quote(manifest_dataset(harmonised, dataset_info))
    ),
    targets::tar_target_raw(
      "grain",
      quote(summarise_grain(harmonised, dataset_info))
    ),
    targets::tar_target_raw(
      "numeric_summary",
      quote(summarise_numeric(harmonised, dataset_info, dataschema))
    ),
    targets::tar_target_raw(
      "level_summary",
      quote(summarise_levels(harmonised, dataset_info, dataschema))
    ),
    targets::tar_target_raw(
      "unit_summary",
      quote(summarise_units(harmonised, dataset_info))
    ),
    targets::tar_target_raw(
      "invariant_check",
      quote(check_invariants(harmonised, dataset_info, invariants))
    ),
    targets::tar_target_raw(
      "exceptions_file",
      quote(find_analysis_exceptions(dataset_id)),
      format = "file"
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
      ))
    ),
    targets::tar_target_raw(
      "prepared_summary",
      quote(summarise_prepared(prepared))
    ),
    targets::tar_target_raw(
      "prepared_flags",
      quote(flag_prepared_dataset(
        prepared,
        harmonisation_config$variables,
        outcome_registry
      ))
    ),
    # `[[`, not `$`: tar_map() renames every symbol that matches a mapped
    # target, and `prepared$row_audit` would become `$row_audit_<key>`.
    targets::tar_target_raw(
      "row_audit",
      quote(prepared[["row_audit"]])
    ),
    targets::tar_target_raw(
      "rows_dropped",
      quote(prepared[["dropped"]])
    )
  )

  bind <- function(name, from) {
    tarchetypes::tar_combine_raw(
      name,
      per_dataset[[from]],
      command = quote(dplyr::bind_rows(!!!.x))
    )
  }
  combined <- list(
    tarchetypes::tar_combine_raw(
      "data_manifest",
      per_dataset[["manifest"]],
      command = quote(combine_data_manifest(!!!.x))
    ),
    bind("grain_all", "grain"),
    bind("numeric_summary_all", "numeric_summary"),
    bind("level_summary_all", "level_summary"),
    bind("unit_summary_all", "unit_summary"),
    targets::tar_target_raw(
      "coverage",
      quote(summarise_coverage(
        numeric_summary_all,
        level_summary_all,
        grain_all,
        dataschema
      ))
    ),
    bind("invariant_check_all", "invariant_check"),
    bind("analysis_row_audit", "row_audit"),
    bind("rows_dropped_all", "rows_dropped"),
    bind("prepared_summary_all", "prepared_summary"),
    bind("prepared_flags_all", "prepared_flags"),
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
        dataschema,
        exposure_registry
      ))
    ),
    targets::tar_target_raw(
      "missingness",
      quote(summarise_missingness(
        numeric_summary_all,
        level_summary_all,
        grain_all
      ))
    ),
    targets::tar_target_raw(
      "dataset_flow",
      bquote(summarise_dataset_flow(
        read_dataset_specs(),
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
      path = fs::path(bp_analysis_reports_dir(), "harmonisation_diagnostics.qmd")
    ),
    tarchetypes::tar_quarto_raw(
      "report_preparation",
      path = fs::path(bp_analysis_reports_dir(), "preparation.qmd")
    )
  )

  list(shared, per_dataset, combined, reports)
}
