# tar_source() and helper-setup.R source every file under R/ into one
# environment, in path order, so a function defined twice is silently replaced
# by whichever file loads last.
test_that("top-level function names are unique across R/", {
  files <- list.files(
    file.path(repo_root, "R"),
    pattern = "\\.R$",
    recursive = TRUE,
    full.names = TRUE
  )

  defined <- lapply(files, function(file) {
    exprs <- parse(file, keep.source = FALSE)
    is_function_def <- vapply(
      exprs,
      function(e) {
        is.call(e) &&
          is.name(e[[1]]) &&
          as.character(e[[1]]) %in% c("<-", "=") &&
          is.name(e[[2]]) &&
          is.call(e[[3]]) &&
          identical(e[[3]][[1]], as.name("function"))
      },
      logical(1)
    )
    fn_names <- vapply(
      exprs[is_function_def],
      function(e) as.character(e[[2]]),
      character(1)
    )
    data.frame(
      name = fn_names,
      file = rep(fs::path_rel(file, repo_root), length(fn_names))
    )
  })
  defined <- do.call(rbind, defined)

  duplicated_names <- unique(defined$name[duplicated(defined$name)])
  clashes <- defined[defined$name %in% duplicated_names, ]
  expect(
    nrow(clashes) == 0,
    paste0(
      "Functions defined more than once:\n",
      paste0("  ", clashes$name, " in ", clashes$file, collapse = "\n")
    )
  )
})
