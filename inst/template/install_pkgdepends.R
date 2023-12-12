local({
  message("Bootstrapping from: {{path_bootstrap}}")
  message("Installing into library: {{path_lib}}")
  message(sprintf("Running in path: %s", getwd()))

  ## TODO: we can work this out from pkgdepends and have it tell us;
  ## all its (recursive) hard dependencies)
  preload <- c("ps", "cli", "curl", "filelock", "pkgdepends", "pkgcache",
               "processx", "lpSolve", "jsonlite", "withr", "desc", "zip",
               "pkgbuild", "callr")
  for (pkg in preload) {
    if (!requireNamespace(pkg, "{{path_bootstrap}}")) {
      stop(sprintf("Failed to load '%s' from the bootstrap library", pkg))
    }
  }

  if ({{delete_first}}) {
    message()
    message("Deleting previous library; this will fail if packages are in use")
    message("Then your running jobs may also fail.")
    unlink("{{path_lib}}", recursive = TRUE)
  }

  dir.create("{{path_lib}}", showWarnings = FALSE, recursive = TRUE)
  .libPaths(file.path(getwd(), "{{path_lib}}"))

  message("Library paths:")
  message(paste(sprintf("  - %s", .libPaths()), collapse = "\n"))

  options(repos = {{repos}})
})

message("Logs from pkgdepends follow:")
message()
message(strrep("-", 79))
message()

proposal <- pkgdepends::new_pkg_installation_proposal(
  refs = {{refs}},
  policy = "{{policy}}")
proposal$solve()
proposal$stop_for_solution_error()
proposal$show_solution()
proposal$download()
proposal$stop_for_download_error()
proposal$install()

message()
message(strrep("-", 79))

# TODO: print a summary of package versions now available in the
# library, go through all packages.
