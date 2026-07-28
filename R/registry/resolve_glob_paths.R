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
  glob <- unlist(lapply(glob, expand_brace_glob), use.names = FALSE)

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
