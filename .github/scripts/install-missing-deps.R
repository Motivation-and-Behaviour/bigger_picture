# Installs any package DESCRIPTION declares that the CI container image lacks,
# so a pull request that adds a dependency passes before the image is rebuilt
# with it. Binaries come from Posit Package Manager, the image's default
# repository. Version constraints are ignored: the image tracks current CRAN.
fields <- c("Imports", "Suggests", "Config/Needs/website")
declared <- read.dcf("DESCRIPTION", fields = fields)
declared <- unlist(strsplit(declared[!is.na(declared)], ","))
packages <- trimws(sub("\\(.*\\)", "", declared))
packages <- unique(packages[nzchar(packages)])

missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing) == 0) {
  message("All DESCRIPTION packages are installed in the image.")
} else {
  message("Installing packages missing from the image: ", toString(missing))
  install.packages(missing)
  failed <- setdiff(missing, rownames(installed.packages()))
  if (length(failed) > 0) {
    stop("Could not install: ", toString(failed), call. = FALSE)
  }
}
