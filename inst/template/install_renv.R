## We can rely here on RENV_ACTIVATE_PROJECT or RENV_AUTOLOADER_ENABLED
if (Sys.getenv("RENV_AUTOLOADER_ENABLED", "") != "FALSE") {
  stop(
    "Expected environment variable 'RENV_AUTOLOADER_ENABLED' to be 'FALSE'")
}

## We might want to pass through packages/exclude here later,
## possibly also clean?
renv::restore(library = "{{path_lib}}",
              prompt = FALSE)
