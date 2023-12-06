local({
  message("Bootstrapping from: {{path_bootstrap}}")
  message("Installing into library: {{path_lib}}")

  loadNamespace("remotes", "{{path_bootstrap}}")
  # we might need pkgbuild too here, really

  if ({{delete_first}}) {
    message()
    message("Deleting previous library; this will fail if packages are in use")
    message("Then your running jobs may also fail.")
    unlink("{{path_lib}}", recursive = TRUE)
  }

  dir.create("{{path_lib}}", FALSE, TRUE)
  .libPaths(file.path(getwd(), "{{path_lib}}"))

  ## We need a CRAN mirror set or nothing works. The user is free to
  ## replace this with something else if they want within their script,
  ## but this saves every script needing one.
  options(repos = c(CRAN = "https://cloud.r-project.org"))
})

message("Logs from your installation script '{{script}}' follow:")
message()
source("{{script}}", echo = TRUE, max.deparse.length = Inf)

# TODO: print a summary of package versions now available in the
# library, go through all packages.
