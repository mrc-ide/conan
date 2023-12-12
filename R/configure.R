##' Configuration for running conan. Some common options and some
##' specific to different provisioning methods.
##'
##' Different methods support different additional arguments:
##'
##' * method `script` supports the argument `script`, which is the
##'   name of the script to run, defaults to "provision.R"
##' * method `pkgdepends` supports the arguments `refs`, which can be
##'   a character vector of references (rather than reading from the
##'   file `pkgdepends.txt`) and `policy` which is passed through to
##'   [pkgdepends::new_pkg_installation_proposal].
##'
##' @title Configuration for conan
##'
##' @param method The method to use; currently "script" and
##'   "pkgdepends" are supported.
##'
##' @param ... Additional arguments, method specific. See Details.
##'
##' @param path_lib The library to install into. Could be an absolute
##'   or a relative path.
##'
##' @param path_bootstrap The path to a bootstrap library to use. This
##'   needs to contain all the packages required for the method you
##'   are using. For "script" this is just "remotes", but for
##'   "pkgdepends" it'll be more.
##'
##' @param delete_first Should we delete the library before installing
##'   into it?
##'
##' @param show_log Should we show the log as the installation runs?
##'
##' @param poll Polling interval for logs in seconds
##'
##' @param path Path to the root where you would run conan from;
##'   typically this is the same path is the root of the project,
##'   often as the working directory.
##'
##' @return A list with class `conan_config`. Do not modify
##'   this object.
##'
##' @export
conan_configure <- function(method, ..., path_lib, path_bootstrap,
                            delete_first = FALSE, show_log = TRUE,
                            poll = 1, path = ".") {
  if (is.null(method)) {
    method <- detect_method(path, call = rlang::current_env())
  }

  args <- list(...)
  assert_scalar_character(method)

  if (method == "script") {
    valid_args <- "script"
    args$script <- args$script %||% "provision.R"
    assert_scalar_character(args$script, "script", call = rlang::current_env())
    if (!file.exists(file.path(path, args$script))) {
      cli::cli_abort(
        "provision script '{args$script}' does not exist at path '{path}'")
    }
  } else if (method == "pkgdepends") {
    valid_args <- c("refs", "policy")
    args$policy <- args$policy %||% "lazy"
    if (!is.null(args$refs)) {
      assert_scalar_character(args$refs, "refs", call = rlang::current_env())
    }
    assert_scalar_character(args$policy, "policy", call = rlang::current_env())
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
      path_pkgdepends <- file.path(path, "pkgdepends.txt")
      if (!file.exists(path_pkgdepends)) {
        cli::cli_abort(
          "Expected a file 'pkgdepends.txt' to exist at path '{path}'")
      }
      refs <- readLines(path_pkgdepends)
    } else {
      refs <- args$refs
    }
    args$pkgdepends <- pkgdepends_parse(refs)
  } else if (method == "auto") {
    args$pkgdepends <- build_pkgdepends_auto(args$environment, path)
    args$policy <- "lazy" # always lazy
  }

  args$method <- method
  args$path_lib <- path_lib
  args$path_bootstrap <- assert_scalar_character(path_bootstrap)
  args$delete_first <- assert_scalar_logical(delete_first)
  args$show_log <- assert_scalar_logical(show_log)
  args$poll <- assert_scalar_numeric(poll)

  class(args) <- "conan_config"

  args
}


detect_method <- function(path, call = NULL) {
  if (file.exists(file.path(path, "provision.R"))) {
    "script"
  } else if (file.exists(file.path(path, "pkgdepends.txt"))) {
    "pkgdepends"
  } else {
    "auto"
  }
}
