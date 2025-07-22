using_renv <- function(path = getwd()) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    return(FALSE)
  }
  project <- renv::project(default = NULL)
  if (is.null(project)) {
    return(FALSE)
  }
  ## For now at least, we don't support running in a subdirectory of
  ## an renv project (that would be the case here if we did the test
  ## fs::path_has_parent(path, project), and if we made sure that renv
  ## explored up to find its files. However, it complicates things
  ## like finding the lockfile.
  fs::path_norm(path) == fs::path_norm(project)
}


renv_hash <- function(path) {
  path_lockfile <- file.path(path, "renv.lock")
  if (!file.exists(path_lockfile)) {
    cli::cli_warn(
      c("Did not find lockfile at '{path}'",
        "!" = "If you install from this point, things will probably fail",
        "i" = "Have you forgotten to run 'renv::snapshot()'?"))
    ## We return a random number here; guaranteeing that we are never
    ## current.
    return(rlang::hash(stats::runif(1)))
  }
  rlang::hash_file(path_lockfile)
}
