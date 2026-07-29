# Contract tests over the repo's harmonisation metadata. These run entirely
# from files tracked in the repository and never touch the mounted data
# directory, so they are safe to run in CI.

dataset_dirs <- withr::with_dir(
  repo_root,
  fs::dir_ls(bp_dataset_specs_dir(), type = "directory")
)

test_that("dataschema.csv is valid", {
  withr::with_dir(repo_root, {
    expect_no_error(read_dataschema())
  })
})

test_that("every dataset harmonisation config reads and validates", {
  withr::with_dir(repo_root, {
    dataschema <- read_dataschema()

    for (dataset_dir in dataset_dirs) {
      if (!fs::file_exists(fs::path(dataset_dir, "variables.csv"))) {
        next
      }
      dataset_id <- sub("^BPIPD-", "", fs::path_file(dataset_dir))
      outcome <- tryCatch(
        {
          read_harmonisation_config(dataset_id, dataschema)
          NULL
        },
        error = identity
      )
      if (is.null(outcome)) {
        succeed()
      } else {
        fail(paste0(
          "read_harmonisation_config failed for ",
          dataset_dir,
          ": ",
          conditionMessage(outcome)
        ))
      }
    }
  })
})

test_that("every active expression parses and only calls known functions", {
  withr::with_dir(repo_root, {
    # Formatting is advisory and deliberately not enforced here; CI should
    # fail on expressions that cannot run, not on their layout.
    issues <- check_harmonisation_exprs(check_format = FALSE)

    expect_identical(
      nrow(issues),
      0L,
      label = paste0(
        "variables.csv expression problems:\n",
        paste0(
          "  ",
          issues$variables_file,
          " [",
          issues$target_variable,
          "] ",
          issues$issue,
          ": ",
          issues$message,
          collapse = "\n"
        )
      )
    )
  })
})

test_that("every dataset.yaml is well-formed and has a tidier", {
  allowed_statuses <- c("planned", "in_progress", "harmonised", "blocked")

  withr::with_dir(repo_root, {
    for (dataset_dir in dataset_dirs) {
      spec_file <- fs::path(dataset_dir, "dataset.yaml")
      if (!fs::file_exists(spec_file)) {
        next
      }

      spec <- yaml::read_yaml(spec_file)
      folder_id <- sub("^BPIPD-", "", fs::path_file(dataset_dir))

      expect_identical(
        as.character(spec$dataset_id),
        folder_id,
        label = paste0("dataset_id in ", spec_file)
      )
      expect_true(
        !is.null(spec$dataset_name) && nzchar(spec$dataset_name),
        label = paste0("dataset_name in ", spec_file)
      )
      expect_true(
        spec$status %in% allowed_statuses,
        label = paste0("status `", spec$status, "` in ", spec_file)
      )
      expect_no_error(find_tidier_file(folder_id))
    }
  })
})

test_that("the dataset template variables.csv has the expected columns", {
  withr::with_dir(repo_root, {
    template <- readr::read_csv(
      fs::path(bp_dataset_template_dir(), "variables.csv"),
      show_col_types = FALSE
    )
    expect_true(all(harmonisation_variable_columns() %in% names(template)))
  })
})

test_that("the dataset template variables.csv is in sync with the dataschema", {
  withr::with_dir(repo_root, {
    result <- sync_harmonisation_vars_file(
      fs::path(bp_dataset_template_dir(), "variables.csv"),
      write = FALSE
    )
    expect_false(
      result$changed,
      label = paste0(
        "The dataset template is out of sync with dataschema.csv. ",
        "Run `sync_harmonisation_vars()` to update it. Missing: ",
        result$added_variables
      )
    )
  })
})
