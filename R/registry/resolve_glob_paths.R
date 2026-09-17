# Expand brace alternatives (e.g. "{a.sav,b.sav}") into one plain glob per
# option; glob2rx() would otherwise treat the braces as literal characters.
expand_brace_glob <- function(pattern) {
  match <- regexpr("\\{[^{}]*\\}", pattern)
  if (match == -1) {
    return(pattern)
  }
  start <- as.integer(match)
  len <- attr(match, "match.length")
  before <- substr(pattern, 1, start - 1)
  inside <- substr(pattern, start + 1, start + len - 2)
  after <- substr(pattern, start + len, nchar(pattern))
  options <- trimws(strsplit(inside, ",", fixed = TRUE)[[1]])
  if (length(options) == 0) {
    options <- ""
  }
  unlist(
    lapply(options, function(option) {
      expand_brace_glob(paste0(before, option, after))
    }),
    use.names = FALSE
  )
}

#' List every file under a directory, warning if the directory is missing
#'
#' The one place the pipeline walks a study tree. Callers that resolve many
#' globs against the same directory should call this once and pass the result
#' to `resolve_glob_paths(files = )`.
list_files_under <- function(base_dir) {
  if (!fs::dir_exists(base_dir)) {
    warning("Base directory does not exist: ", base_dir, call. = FALSE)
    return(fs::path())
  }
  fs::dir_ls(base_dir, recurse = TRUE, type = "file")
}

#' Resolve a resource glob against the files under a directory
#'
#' `files` and `rel` let a caller supply work it has already done. Both depend
#' only on `base_dir`, but a spec can declare hundreds of globs against the same
#' one, so recomputing them per glob is what made ingestion slow: for a 748-file
#' study tree the walk costs about a second and `fs::path_rel()` about 0.13 s,
#' against 0.3 ms to actually match a pattern. `build_resource_index()` computes
#' both once per `base_dir`; leaving them NULL keeps the standalone behaviour.
resolve_glob_paths <- function(base_dir, glob, files = NULL, rel = NULL) {
  # glob can be a scalar or a list/array
  if (is.list(glob)) {
    glob <- unlist(glob, recursive = TRUE, use.names = FALSE)
  }
  glob <- as.character(glob)
  glob <- unlist(lapply(glob, expand_brace_glob), use.names = FALSE)

  if (is.null(files)) {
    files <- list_files_under(base_dir)
  }
  if (length(files) == 0) {
    return(character(0))
  }

  if (is.null(rel)) {
    rel <- relative_to_base(files, base_dir)
  }

  # match any pattern
  keep <- rep(FALSE, length(rel))
  for (g in glob) {
    rx <- utils::glob2rx(g)
    keep <- keep | grepl(rx, rel)
  }

  unique(files[keep])
}

#' Paths relative to `base_dir`, for glob matching
#'
#' Split out so callers resolving many globs against one directory can compute
#' it once. See `resolve_glob_paths()`.
relative_to_base <- function(files, base_dir) {
  fs::path_rel(files, start = base_dir)
}
