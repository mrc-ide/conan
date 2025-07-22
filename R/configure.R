##' Configuration for running conan. Some common options and some
##' specific to different provisioning methods.
##'
##' Different methods support different additional arguments:
##'
##' * method `script` supports the argument `script`, which is the
##'   name of the script to run, defaults to "provision.R"
##' * method `pkgdepends` supports the arguments `filename`, which can
##'   be a filename to the input data,`refs`, which can be a character
##'   vector of references (rather than reading from the file
##'   `pkgdepends.txt` or the file referred to by `filename`),
##'   `policy` which is passed through to
##'   [`pkgdepends::new_pkg_installation_proposal()`].
##' * method `auto` takes an argument `environment` which contains a
##'   list of packages to install and source files to scan for
##'   dependencies.
##' * method `renv` takes no arguments.
##'
##' Setting environment variables while running the installation comes
##' via the `envvars` argument; this system is designed to play well
##' with `hipercow`, though it does not require it.  We expect a
##' `data.frame` with columns `name`, `value` and (optionally)
##' `secret`.  If `secret` is given, it must be a logical value
##' indicating that `value` has been encrypted with an `rsa` public
##' key.  If any `secret` is `TRUE`, then `envvvars` must also have an
##' *attribute* `key` that contains the path to private rsa key to
##' decrypt the secrets (i.e., `attr(envvars, "key")`).  If you use
##' secret environment variables, then the `openssl` package must be
##' present in conan's bootstrap.
##'
##' @title Configuration for conan
##'
##' @param method The method to use; currently `script`,
##'   `pkgdepends`, `auto` and `renv` are supported.
##'
##' @param ... Additional arguments, method specific. See Details.
##'
##' @param path_lib The library to install into. Could be an absolute
##'   or a relative path.
##'
##' @param path_bootstrap The path to a bootstrap library to use. This
##'   needs to contain all the packages required for the method you
##'   are using. For `script` this is just `remotes`, but for
##'   `pkgdepends` it must contain the full recursive dependencies of
##'   `pkgdepends`.
##'
##' @param cran URL for use as the CRAN repo. If not given we will use
##'   the RStudio CRAN mirror.  This option has no effect when using
##'   renv, as the URLs in your lock file determine the locations that
##'   packages are fetched from.  The intended use of this option is
##'   for where a CRAN repo is misbehaving (e.g., returning 500
##'   errors, or has an invalid/incomplete/out of date index).  The
##'   most likely alternative version to use is `cran =
##'   "https://cran.r-project.org"`
##'
##' @param delete_first Should we delete the library before installing
##'   into it?
##'
##' @param path Path to the root where you would run conan from;
##'   typically this is the same path is the root of the project,
##'   often as the working directory.
##'
##' @param envvars Environment variables to set before running the
##'   installation.  See Details for format.
##'
##' @return A list with class `conan_config`. Do not modify
##'   this object.
##'
##' @export
conan_configure <- function(method, ..., path_lib, path_bootstrap, cran = NULL,
                            delete_first = FALSE, path = ".", envvars = NULL) {
  call <- rlang::current_env()
  if (is.null(method)) {
    method <- detect_method(path, call = call)
    cli::cli_alert_success("Selected provisioning method '{method}'")
  }

  args <- list(...)
  assert_scalar_character(method)
  if (is.null(cran)) {
    cran <- "https://cloud.r-project.org"
  } else {
    assert_scalar_character(cran, call = call)
  }

  if (method == "script") {
    valid_args <- "script"
    args$script <- args$script %||% "provision.R"
    assert_scalar_character(args$script, "script", call = call)
    if (!file.exists(file.path(path, args$script))) {
      cli::cli_abort(
        "provision script '{args$script}' does not exist at path '{path}'")
    }
  } else if (method == "pkgdepends") {
    valid_args <- c("filename", "refs", "policy")
    args$policy <- args$policy %||% "lazy"
    if (!is.null(args$filename)) {
      assert_scalar_character(args$filename, "filename", call = call)
    }
    if (is.null(args$refs)) {
      if (is.null(args$filename)) {
        args$filename <- "pkgdepends.txt"
      }
    } else {
      if (!is.null(args$filename)) {
        cli::cli_abort("Don't both 'filename' and 'refs'", call = call)
      }
      assert_character(args$refs, "refs", call = call)
    }
    assert_scalar_character(args$policy, "policy", call = call)
  } else if (method == "renv") {
    valid_args <- NULL
  } else if (method == "auto") {
    valid_args <- NULL
  } else {
    cli::cli_abort("Unknown provision method '{method}'")
  }

  extra <- setdiff(names(args), c("environment", valid_args))
  if (length(extra) > 0) {
    cli::cli_abort(
      "Unknown arguments in '...' for method '{method}': {collapseq(extra)}")
  }

  assert_scalar_character(path_lib)
  if (fs::is_absolute_path(path_lib)) {
    cli::cli_abort(c(
      "'path_lib' must be a relative path",
      i = "We interpret 'path_lib' relative to 'path' ({path})"))
  }

  if (method == "pkgdepends") {
    if (is.null(args$refs)) {
      path_pkgdepends <- file.path(path, args$filename)
      if (!file.exists(path_pkgdepends)) {
        cli::cli_abort(
          "Expected a file '{args$filename}' to exist at path '{path}'")
      }
      refs <- read_lines(path_pkgdepends)
    } else {
      refs <- args$refs
    }
    args$pkgdepends <- pkgdepends_parse(refs)
    args$hash <- rlang::hash(args$pkgdepends)
  } else if (method == "auto") {
    args$pkgdepends <- build_pkgdepends_auto(args$environment, path)
    args$policy <- "lazy" # always lazy
    args$hash <- rlang::hash(args$pkgdepends)
  } else if (method == "script") {
    args$hash <- rlang::hash_file(file.path(path, args$script))
  } else if (method == "renv") {
    args$hash <- renv_hash(path)
  }

  args$method <- method
  args$path_lib <- path_lib
  args$path_bootstrap <- assert_scalar_character(path_bootstrap)
  args$delete_first <- assert_scalar_logical(delete_first)
  args$cran <- cran
  args$envvars <- check_envvars(envvars)

  class(args) <- "conan_config"

  args
}


detect_method <- function(path, call = NULL) {
  if (using_renv(path)) {
    "renv"
  } else if (file.exists(file.path(path, "provision.R"))) {
    "script"
  } else if (file.exists(file.path(path, "pkgdepends.txt"))) {
    "pkgdepends"
  } else {
    "auto"
  }
}


## Practically we won't see this from hipercow, but we might reuse
## this in twinkle.  We probably need a nice way of creating one of
## these too.
check_envvars <- function(envvars) {
  if (is.null(envvars)) {
    return(NULL)
  }
  if (!inherits(envvars, "data.frame")) {
    cli::cli_abort("Expected 'envvars' to be a data.frame")
  }
  msg <- setdiff(c("name", "value"), names(envvars))
  if (length(msg) > 0) {
    cli::cli_abort("Missing column{?s} from 'envvars': {squote(msg)}")
  }
  if (is.null(envvars$secret)) {
    envvars$secret <- FALSE
  }
  key <- attr(envvars, "key", exact = TRUE)
  if (any(envvars$secret) && is.null(key)) {
    cli::cli_abort(
      "Secret environment variables in 'envvars', but key not found")
  }
  envvars
}
