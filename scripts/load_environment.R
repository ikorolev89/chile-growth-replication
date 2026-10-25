# Use the restored project library; CHILE_R_LIBS is an optional audit override.
root <- normalizePath(getwd())
if (!file.exists(file.path(root, "renv.lock")))
  stop("Run this command from the replication archive root")
extra <- Sys.getenv("CHILE_R_LIBS", "")
if (nzchar(extra)) {
  .libPaths(c(strsplit(extra, .Platform$path.sep, fixed=TRUE)[[1]], .libPaths()))
} else {
  bootstrap <- file.path(root, ".bootstrap-library")
  if (dir.exists(bootstrap)) .libPaths(c(bootstrap, .libPaths()))
  if (requireNamespace("renv", quietly=TRUE))
    .libPaths(c(renv::paths$library(project=root), .libPaths()))
}
