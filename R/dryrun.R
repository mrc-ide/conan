##' Dry run of package installation.
##'
##' @title Dry run of package installation
##'
##' @inheritParams conan_install
##'
##' @param error Logical, indicating if error should be thrown on
##'   failure. If `FALSE` then we return the proposal object instead.
##'
##' @return Invisibly, the resolution
##' @export
##' @examples
##' @examplesIf nzchar(Sys.getenv("R_USER_CACHE_DIR"))
##' conan::conan_dryrun("cpp11")
conan_dryrun <- function(packages, policy = "upgrade", repos = NULL,
                         cran = NULL, lib = NULL, error = TRUE) {
  lib <- lib %||% tempfile()
  dir.create(lib, FALSE, TRUE)

  config <- list(library = lib)
  loadNamespace("pkgdepends")
  loadNamespace("pkgcache")

  withr::with_libpaths(
    lib,
    withr::with_options(
      c(repos = clean_repos(repos, cran)), {
        proposal <- conan_proposal(packages, config, policy)
        proposal$solve()
        if (error) {
          proposal$stop_for_solution_error()
        } else {
          tryCatch(proposal$stop_for_solution_error(),
                   error = function(e) message(e$message))
        }
      }))

  proposal
}
