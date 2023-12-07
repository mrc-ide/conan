##' Run a conan installation, in another process, blocking from this
##' process.
##'
##' @title Run a conan installation
##'
##' @inheritParams conan_write
##'
##' @return Nothing
##' @export
conan_run <- function(config) {
  ## TODO: this *must* be called from the same directory passed
  ## through to conan_configure, which is weird.
  path <- tempfile(pattern = "conan")
  dir_create(path)
  path_script <- file.path(path, "conan.R")
  path_log <- file.path(path, "log")
  conan_write(config, path_script)
  callr::rscript(path_script, stdout = path_log, stderr = path_log,
                 show = config$show_log)
  invisible()
}


##' Write a conan installation script
##'
##' @title Write conan installation script
##'
##' @param path The path to write to
##'
##' @param config Conan config, from [conan_configure()]
##'
##' @return Nothing
##' @export
conan_write <- function(config, path) {
  assert_is(config, "conan_config")
  template <- read_string(
    conan_file(sprintf("template/install_%s.R", config$method)))
  str <- glue_whisker(template, template_data(config))
  dir_create(dirname(path))
  writeLines(str, path)
}


template_data <- function(config) {
  ret <- config
  default_repo <- "https://cloud.r-project.org"
  if (config$method == "script") {
    ret$repos <- vector_to_str(default_repo)
  } else if (config$method == "pkgdepends") {
    ret$repos <- vector_to_str(c(config$pkgdepends$repos, default_repo))
    ret$refs <- vector_to_str(config$pkgdepends$refs)
  }
  ret
}
