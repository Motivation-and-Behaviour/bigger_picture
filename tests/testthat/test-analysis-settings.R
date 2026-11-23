# Settings reach every function as an argument. A function's default,
# `default_analysis_settings()`, is hidden from the pipeline's dependency
# tracking on purpose (see its documentation), so a call that relied on it
# would not rerun when a setting changed. These tests make sure none does.

settings_code <- function() {
  files <- list.files(
    file.path(repo_root, "R"),
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )
  files <- c(files, file.path(repo_root, c("_targets.R", "make.R")))
  lapply(stats::setNames(files, files), parse, keep.source = FALSE)
}

# The arguments of each function whose default comes from the settings.
settings_arguments <- function(code) {
  found <- list()
  for (expressions in code) {
    for (expression in as.list(expressions)) {
      defines <- is.call(expression) &&
        identical(expression[[1]], as.name("<-")) &&
        is.name(expression[[2]]) &&
        is.call(expression[[3]]) &&
        identical(expression[[3]][[1]], as.name("function"))
      if (!defines) {
        next
      }
      formals <- expression[[3]][[2]]
      from_settings <- vapply(
        formals,
        function(default) {
          "default_analysis_settings" %in% all.names(default)
        },
        logical(1)
      )
      if (any(from_settings)) {
        found[[as.character(expression[[2]])]] <- list(
          definition = eval(expression[[3]]),
          arguments = names(formals)[from_settings]
        )
      }
    }
  }
  found
}

# Every call in `expression` to one of `guarded`, and every place one is
# handed to another function by name.
settings_calls <- function(expression, guarded) {
  out <- list()
  walk <- function(node) {
    if (!is.call(node)) {
      return(invisible())
    }
    head <- node[[1]]
    if (is.name(head) && as.character(head) %in% guarded) {
      out[[length(out) + 1]] <<- list(kind = "call", call = node)
    }
    for (i in seq_along(node)[-1]) {
      # An empty argument, as in `x[, 1]`, has nothing to look at.
      if (identical(deparse(node[[i]]), "")) {
        next
      }
      argument <- node[[i]]
      # A function's own definition names it without calling it.
      defined <- i == 2 &&
        is.name(head) &&
        as.character(head) %in% c("<-", "=")
      passed <- !defined &&
        is.name(argument) &&
        as.character(argument) %in% guarded
      if (passed) {
        out[[length(out) + 1]] <<- list(
          kind = "passed",
          call = node,
          name = as.character(argument)
        )
      }
      walk(argument)
    }
    walk(head)
  }
  walk(expression)
  out
}

test_that("functions take their settings as an argument, never by default", {
  code <- settings_code()
  guarded <- settings_arguments(code)
  expect_gt(length(guarded), 30)
  # No function names the full settings in its code or its defaults: only
  # the `settings` target and the fallback do.
  direct <- unlist(lapply(names(code), function(file) {
    if (grepl("analysis_settings\\.R$", file)) {
      return(NULL)
    }
    calls <- sum(all.names(code[[file]]) == "analysis_settings")
    if (calls > 0) paste(basename(file), calls) else NULL
  }))
  expect_equal(direct, "analysis_targets.R 1")

  problems <- character()
  for (file in names(code)) {
    for (expression in as.list(code[[file]])) {
      for (found in settings_calls(expression, names(guarded))) {
        if (found$kind == "call") {
          name <- as.character(found$call[[1]])
          matched <- tryCatch(
            names(as.list(match.call(guarded[[name]]$definition, found$call))),
            error = function(e) NULL
          )
          missing_arguments <- setdiff(guarded[[name]]$arguments, matched)
          if (is.null(matched) || length(missing_arguments) > 0) {
            problems <- c(
              problems,
              paste0(
                basename(file),
                ": ",
                name,
                "() called without ",
                paste(missing_arguments, collapse = ", ")
              )
            )
          }
        } else {
          # Handed to `lapply()` or the like: the settings must be handed
          # over beside it.
          carried <- any(vapply(
            seq_along(found$call)[-1],
            function(i) {
              any(grepl("settings", all.names(found$call[[i]])))
            },
            logical(1)
          ))
          if (!carried) {
            problems <- c(
              problems,
              paste0(
                basename(file),
                ": ",
                found$name,
                " passed to ",
                deparse(found$call[[1]])[1],
                "() without settings"
              )
            )
          }
        }
      }
    }
  }
  expect_identical(paste(unique(problems), collapse = " | "), "")
})

test_that("the fallback gives the full settings", {
  expect_identical(default_analysis_settings(), analysis_settings())
})

test_that("the pipeline does not follow the fallback to the settings", {
  # What `targets` would see a function with this default depend on.
  uses <- function(x, settings = default_analysis_settings()) {
    x + settings$min_datasets
  }
  seen <- codetools::findGlobals(uses, merge = TRUE)
  expect_true("default_analysis_settings" %in% seen)
  expect_false("analysis_settings" %in% seen)
  inside <- codetools::findGlobals(default_analysis_settings, merge = TRUE)
  expect_false("analysis_settings" %in% inside)
})

test_that("the build sections match the pools the targets run in", {
  skip_if_not_installed("tarchetypes")
  plan <- tibble::tibble(dataset_key = "BPIPD_9991", dataset_id = "9991")
  withr::local_dir(repo_root)
  targets_list <- unlist(analysis_targets(plan))
  name <- vapply(targets_list, function(target) target$settings$name, "")
  pool <- vapply(
    targets_list,
    function(target) {
      controller <- target$settings$resources$crew$controller
      if (is.null(controller)) "main" else controller
    },
    character(1)
  )
  sections <- bp_build_sections()
  prefix <- sub("_BPIPD_9991$", "", name)
  per_dataset <- grepl("_BPIPD_9991$", name)
  # Every target in the model pool is built in the models section, and
  # nothing in that section loads a whole dataset.
  expect_setequal(prefix[pool == "model"], sections$models)
  expect_true(all(pool[per_dataset & prefix %in% sections$models] == "model"))
  # Every target that loads a whole dataset is built in the datasets
  # section, before any model.
  expect_true(all(
    prefix[pool == "row_level" & per_dataset] %in% sections$datasets
  ))
  expect_length(intersect(sections$datasets, sections$models), 0)
  expect_true(all(unlist(sections) %in% prefix))
})

test_that("the model pool is four unless a build asks for more", {
  # The pool never takes more than all but two of the machine's cores, so
  # on a small machine (such as a CI runner) the default is below four.
  expected <- max(1L, min(parallel::detectCores() - 2L, 4L))
  withr::local_envvar(BP_MODEL_WORKERS = NA)
  expect_equal(bp_model_workers(), expected)
  withr::local_envvar(BP_MODEL_WORKERS = "16")
  expect_equal(bp_model_workers(), min(parallel::detectCores() - 2L, 16L))
  withr::local_envvar(BP_MODEL_WORKERS = "many")
  expect_equal(bp_model_workers(), expected)
})

test_that("the number of imputations follows the freeze", {
  settings <- analysis_settings()
  expect_equal(settings$imputations_confirmatory, 50L)
  expect_equal(
    bp_imputations(settings),
    if (bp_specification_frozen()) 50L else settings$imputations
  )
})
