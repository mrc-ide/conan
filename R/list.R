##' List conan installations, and optionally test if they are current.
##'
##' @title Test if a conan installation is current
##'
##' @param path_lib Path to the library to compare
##'
##' @param hash A hash to compare; if given (not `NULL`) then we
##'   highlight installations that match this hash.
##'
##' @return A [data.frame] with columns:
##'
##' * `name`: the name of the installation. This might be useful with
##'   `conan_compare`
##' * `time`: the time the installation was started
##' * `hash`: the installation hash
##' * `method`: the method used for the installation
##' * `args`: the arguments to the installation (as a list column)
##' * `current`: Matches the hash passed in the argument `hash`
##'
##' This object also has class `conan_list` so that it prints nicely,
##'   but you can drop this with `as.data.frame`.
##'
##' @export
conan_list <- function(path_lib, hash = NULL) {
  contents <- dir(file.path(path_lib, ".conan"))
  d <- lapply(file.path(path_lib, ".conan", contents), readRDS)
  ret <- data.frame(
    name = contents,
    time = unlist_times(lapply(d, function(x) x$description$time)),
    hash = vcapply(d, function(x) x$hash),
    method = vcapply(d, function(x) x$method),
    args = I(lapply(d, function(x) x$args)))
  if (!is.null(hash)) {
    ret$current <- ret$hash == hash
  }
  rownames(ret) <- NULL
  class(ret) <- c("conan_list", class(ret))
  ret
}


##' @export
print.conan_list <- function(x, ...) {
  n <- nrow(x)
  if (n == 0) {
    cli::cli_alert_warning("No conan installations recorded")
  } else {
    cli::cli_alert_info("{n} conan installation{?s} recorded")
    ago <- prettyunits::time_ago(x$time)
    i <- seq_len(n)
    txt <- sprintf("%d: {.strong %s} (%s) [%d]", i, x$name, ago, i - n)
    if (any(x$current)) {
      txt[x$current] <- paste(txt[x$current], "({.strong *})")
    }
    cli::cli_li(txt)
    if (any(x$current)) {
      cli::cli_alert_info(paste(
        "{cli::qty(sum(x$current))}{?The entry/The entries} marked with",
        "'{.strong *}' {?matches/match} the provided installation hash"))
    } else if (!is.null(x$current)) {
      cli::cli_alert_info("No entry matches the provided installation hash")
    }
  }
  invisible(x)
}
