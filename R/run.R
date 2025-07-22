##' Run a conan installation, in another process, blocking from this
##' process.
##'
##' @title Run a conan installation
##'
##' @inheritParams conan_write
##'
##' @param show_log Show output from running the script (passed
##'   through to [callr::rscript] as `show`)
##'
##' @return Nothing
##' @export
conan_run <- function(config, show_log = TRUE) {
  ## TODO: this *must* be called from the same directory passed
  ## through to conan_configure, which is weird.
  path <- tempfile(pattern = "conan")
  ## If the integration tests fail, it will be useful to set this path
  ## to something like: /home/rich/tmp/conan-testing (or some other
  ## absolute path) so that the logs are not swallowed by the check.
  dir_create(path)
  path_script <- file.path(path, "conan.R")
  path_log <- file.path(path, "log")
  conan_write(config, path_script)
  if (config$method == "renv") {
    ## This all plays quite poorly with callr because it does a great
    ## job of setting libPaths for us, after renv has finished setting
    ## up the libPaths, so we never end up with a loaded version!
    ##
    ## I've confirmed that we can disable autoload by setting one of
    ## these:
    ##
    ## RENV_AUTOLOADER_ENABLED
    ## RENV_ACTIVATE_PROJECT
    ## RENV_CONFIG_AUTOLOADER_ENABLED
    ##
    ## To anything other than true/t/1 (case insensitive).
    withr::local_envvar(RENV_AUTOLOADER_ENABLED = "FALSE")
  }
  callr::rscript(path_script, stdout = path_log, stderr = path_log,
                 show = show_log)
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
  data <- template_data(config)

  name <- if (config$method == "auto") "pkgdepends" else config$method
  template <- read_string(conan_file(sprintf("template/install_%s.R", name)))
  data$implementation <- glue_whisker(template, data)

  wrapper <- read_string(conan_file("template/wrapper.R"))
  str <- glue_whisker(wrapper, data)

  dir_create(dirname(path))
  writeLines(str, path)
}


template_data <- function(config) {
  ret <- config
  ret$hash <- config$hash
  ret$args_str <- list_to_str(
    config[setdiff(names(config), c("method", "hash"))])
  preload <- if (any(config$envvars$secret)) "openssl" else NULL
  if (config$method == "script") {
    ret$repos <- vector_to_str(config$cran)
    ret$preload <- vector_to_str(c("remotes", preload))
    ret$what <- sprintf("your installation script '%s'", config$script)
  } else if (config$method %in% c("pkgdepends", "auto")) {
    ret$repos <- vector_to_str(c(config$pkgdepends$repos, config$cran))
    ret$refs <- vector_to_str(config$pkgdepends$refs)
    ret$preload <- vector_to_str(
      c("ps", "cli", "curl", "filelock", "pkgdepends", "pkgcache",
        "processx", "lpSolve", "jsonlite", "withr", "desc", "zip",
        "pkgbuild", "callr", preload))
    ret$what <- "pkgdepends"
  } else if (config$method == "renv") {
    ret$preload <- vector_to_str(c("renv", preload))
    ret$what <- "renv"
  }
  ret$conan_describe_definition <- deparse_fn("conan_describe", indent = 2)
  ret$set_envvars <- generate_set_envvars(config$envvars)
  ret
}


generate_set_envvars <- function(envvars) {
  if (is.null(envvars) || nrow(envvars) == 0) {
    return("")
  }
  f <- function(name, value, secret) {
    if (secret) {
      sprintf('Sys.setenv("%s" = decrypt("%s"))', name, value)
    } else {
      sprintf('Sys.setenv("%s" = "%s")', name, value)
    }
  }
  if (any(envvars$secret)) {
    key <- gsub("\\", "/", attr(envvars, "key", exact = TRUE), fixed = TRUE)
    decrypt <- c(
      "decrypt <- function(value) {",
      "  rawToChar(openssl::rsa_decrypt(openssl::base64_decode(value),",
      sprintf('                                 "%s"))', key),
      "}")
  } else {
    decrypt <- NULL
  }

  ret <- Map(f, envvars$name, envvars$value, envvars$secret)
  paste0("  ", c(decrypt, vcapply(ret, identity)), collapse = "\n")
}
