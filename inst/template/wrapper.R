local({
  message("/`-'\\  _______  ___  ___ ____")
  message("\\,T./ / __/ _ \\/ _ \\/ _ `/ _ \\")
  message("  |   \\__/\\___/_//_/\\_,_/_//_/")
  message("  |   ---- THE  LIBRARIAN ----")
  message()
  message("Bootstrapping from: {{path_bootstrap}}")
  message("Installing into library: {{path_lib}}")
  message("Using method {{method}}")
  message(sprintf("Running in path: %s", getwd()))

  preload <- {{preload}}
  for (pkg in preload) {
    if (!requireNamespace(pkg, "{{path_bootstrap}}", quietly = TRUE)) {
      if ("{{method}}" == "script") {
        msg <- paste("Failed to load 'remotes' from the bootstrap library.",
                     "If you are using 'remotes::install_github()' etc, then",
                     "your script will fail. However, you can always install",
                     "'remotes' yourself within your script and try again")
        message()
        message(paste(strwrap(msg), collapse = "\n"))
      } else {
        stop(sprintf("Failed to load '%s' from the bootstrap library", pkg))
      }
    }
  }

{{set_envvars}}

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

  id <- strftime(Sys.time(), "%Y%m%d%H%M%S", "GMT")
  message(sprintf("id: %s", id))

  ## So that we can access this later:
  options(conan.id = id)
})

message("Logs from {{what}} follow:")
message()
message(strrep("-", 79))
message()

{{implementation}}

message()
message(strrep("-", 79))

local({
{{conan_describe_definition}}

  dest <- file.path("{{path_lib}}", ".conan", getOption("conan.id"))
  message(sprintf("Writing library description to '%s'", dest))
  dir.create(dirname(dest), FALSE, TRUE)
  info <- list(method = "{{method}}",
               args = {{args_str}},
               hash = "{{hash}}",
               description = conan_describe("{{path_lib}}"))
  saveRDS(info, dest)
  message("Done!")
})
