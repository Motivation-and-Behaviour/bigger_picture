library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("dplyr"),
  controller = crew::crew_controller_local(
    workers = min(parallel::detectCores() - 2, 20), seconds_idle = 15
  )
)

tar_source()

# Static map expansion object for tar_map()
.dataset_plan_full <- create_dataset_plan(
  list_datasets(),
  read_dataset_specs()
)
.dataset_map_plan <- dplyr::select(
  .dataset_plan_full,
  dataset_key,
  dataset_id,
  dataset_dir,
  spec_path,
  has_harmonisation
)

.dataschema_targets <- list(
  tar_target(
    dataschema_file,
    bp_dataschema_path(),
    format = "file"
  ),
  tar_target(
    dataschema,
    read_dataschema(dataschema_file)
  )
)

if (nrow(.dataset_map_plan) == 0) {
  .dataset_targets <- list()
  .harmonisation_file_targets <- list()
  .harmonisation_config_targets <- list()
  .harmonised_targets <- list()
  .analysis_target <- tar_target(analysis_data, tibble::tibble())
} else {
  .dataset_targets <- tar_map(
    values = .dataset_map_plan,
    names = dataset_key,
    list(
      tar_target(
        spec_file,
        spec_path,
        format = "file"
      ),
      tar_target(
        spec,
        yaml::read_yaml(spec_file)
      ),

      # Track DATA files only
      tar_target(
        data_files,
        list_data_files_from_spec(dataset_dir, spec),
        format = "file"
      ),
      tar_target(
        raw,
        {
          data_files
          read_dataset_from_spec(dataset_dir, spec)
        }
      ),
      tar_target(
        tidied,
        tidy_from_spec(raw, spec)
      )
    )
  )

  .harmonise_plan <- dplyr::filter(
    .dataset_map_plan,
    has_harmonisation
  )

  if (nrow(.harmonise_plan) == 0) {
    .harmonisation_file_targets <- list()
    .harmonisation_config_targets <- list()
    .harmonised_targets <- list()
    .analysis_target <- tar_target(analysis_data, tibble::tibble())
  } else {
    .harmonisation_file_targets <- list()
    .harmonisation_config_targets <- list()
    .harmonised_targets <- list()

    for (i in seq_len(nrow(.harmonise_plan))) {
      row <- .harmonise_plan[i, ]

      dataset_id_value <- as.character(row$dataset_id)

      harmonisation_files_target <- as.name(
        paste0("harmonisation_files_", row$dataset_key)
      )
      harmonisation_config_target <- as.name(
        paste0("harmonisation_config_", row$dataset_key)
      )
      harmonised_target <- as.name(paste0("harmonised_", row$dataset_key))
      tidied_target <- as.name(paste0("tidied_", row$dataset_key))
      spec_target <- as.name(paste0("spec_", row$dataset_key))

      .harmonisation_file_targets[[
        length(.harmonisation_file_targets) + 1
      ]] <- eval(
        bquote(
          tar_target(
            .(harmonisation_files_target),
            list_harmonisation_files(.(dataset_id_value)),
            format = "file"
          )
        )
      )

      .harmonisation_config_targets[[
        length(.harmonisation_config_targets) + 1
      ]] <- eval(
        bquote(
          tar_target(
            .(harmonisation_config_target),
            {
              .(harmonisation_files_target)
              read_harmonisation_config(.(dataset_id_value), dataschema)
            }
          )
        )
      )

      harmonise_command <- bquote(
        harmonise_from_spec(
          .(tidied_target),
          .(spec_target),
          dataschema,
          .(harmonisation_config_target)
        )
      )

      .harmonised_targets[[length(.harmonised_targets) + 1]] <- eval(
        bquote(
          tar_target(
            .(harmonised_target),
            .(harmonise_command)
          )
        )
      )
    }

    .analysis_target <- tar_combine(
      analysis_data,
      .harmonised_targets,
      command = dplyr::bind_rows(!!!.x)
    )
  }
}

list(
  .dataschema_targets,
  .dataset_targets,
  .harmonisation_file_targets,
  .harmonisation_config_targets,
  .harmonised_targets,
  .analysis_target
)
