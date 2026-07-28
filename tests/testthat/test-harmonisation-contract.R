# Contract tests over the repo's harmonisation metadata. These run entirely
# from files tracked in the repository and never touch the mounted data
# directory, so they are safe to run in CI.

active_expression_rows <- function(variables) {
  active <- !variables$status %in%
    c("incompatible", "unavailable", "in_progress")
  has_expression <- !is.na(variables$expression) &
    nzchar(variables$expression)
  variables[active & has_expression, , drop = FALSE]
}

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
  eval_env <- harmonisation_eval_env(
    spec = make_test_spec(),
    analysis_base = tibble::tibble(),
    lookups = list(),
    tbl = tibble::tibble()
  )

  withr::with_dir(repo_root, {
    for (dataset_dir in dataset_dirs) {
      variables_file <- fs::path(dataset_dir, "variables.csv")
      if (!fs::file_exists(variables_file)) {
        next
      }

      variables <- readr::read_csv(variables_file, show_col_types = FALSE)
      rows <- active_expression_rows(variables)

      for (i in seq_len(nrow(rows))) {
        expression <- rows$expression[[i]]
        label <- paste0(
          fs::path_file(dataset_dir),
          "::",
          rows$target_variable[[i]]
        )

        calls <- tryCatch(
          harmonisation_expression_calls(expression),
          error = identity
        )
        if (inherits(calls, "error")) {
          fail(paste0(
            "Expression failed to parse for ",
            label,
            ": ",
            conditionMessage(calls)
          ))
          next
        }
        succeed()

        for (call_name in calls) {
          if (grepl("::", call_name, fixed = TRUE)) {
            parts <- strsplit(call_name, ":{2,3}")[[1]]
            expect_true(
              requireNamespace(parts[[1]], quietly = TRUE) &&
                parts[[2]] %in% getNamespaceExports(parts[[1]]),
              label = paste0(
                "`",
                call_name,
                "` (used by ",
                label,
                ") resolves to an installed package export"
              )
            )
          } else {
            expect_true(
              exists(call_name, envir = eval_env, mode = "function"),
              label = paste0(
                "`",
                call_name,
                "()` (used by ",
                label,
                ") is available in the harmonisation environment; add it to ",
                "bp_harmonisation_functions() if it should be allowed"
              )
            )
          }
        }
      }
    }
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
      fs::path("harmonisation", "templates", "dataset", "variables.csv"),
      show_col_types = FALSE
    )
    expect_true(all(harmonisation_variable_columns() %in% names(template)))
  })
})
