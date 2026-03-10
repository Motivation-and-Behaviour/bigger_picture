resolve_glob_paths <- function(base_dir, glob) {
  if (!fs::dir_exists(base_dir)) {
    warning("Base directory does not exist: ", base_dir, call. = FALSE)
    return(character(0))
  }

  # glob can be a scalar or a list/array
  if (is.list(glob)) {
    glob <- unlist(glob, recursive = TRUE, use.names = FALSE)
  }
  glob <- as.character(glob)

  # list everything once
  files <- fs::dir_ls(base_dir, recurse = TRUE, type = "file")
  if (length(files) == 0) {
    return(character(0))
  }

  rel <- fs::path_rel(files, start = base_dir)

  # match any pattern
  keep <- rep(FALSE, length(rel))
  for (g in glob) {
    rx <- utils::glob2rx(g)
    keep <- keep | grepl(rx, rel)
  }

  unique(files[keep])
}
