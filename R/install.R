##' Create a self-contained library, using pkgdepends.
##'
##' @title Create a self-contained library
##'
##' @param packages A character vector of packages to install. These
##'   can be names of cran packages or github references etc; see
##'   [pkgdepends::new_pkg_installation_proposal()] for more details
##'
##' @param lib The path to install into. We will create a
##'   self-contained library at this address.
##'
##' @param path_bootstrap The path to find the "bootstrap library" at
##'   (created via [conan::conan_bootstrap()]; this contains the
##'   packages required to install your packages, kept separate from
##'   the `lib`. If not given then a per-session path will be
##'   used (within [tempdir()]) but installation can be sped up by
##'   using a persistent path. If `NULL` but the environment variable
##'   `CONAN_PATH_BOOTSTRAP` we will use the directory pointed at by
##'   that environment variable.
##'
##' @param path_cache Path to the pkgdepends cache. Falls back on
##'   environment variable `CONAN_PATH_CACHE`, or if that is not set
##'   to a temporary directory.
##'
##' @param policy Should be either "lazy" or "upgrade", with a default
##'   of "upgrade"
##'
##' @param repos A character vector of repositories to use when
##'   installing. A suitable CRAN repo will be added if not detected
##'   (using the `cran` argument if provided)
##'
##' @param cran Fallback CRAN repo to use. If not given we will use
##'   `https://cloud.r-project.org`
##'
##' @return Nothing, called for side effects of creating a library at
##'   `lib`.
##'
##' @export
##' @author Richard Fitzjohn
conan_install <- function(lib, packages, policy = "upgrade", repos = NULL,
                          cran = NULL, path_bootstrap = NULL,
                          path_cache = NULL) {
  path_bootstrap <- conan_path_bootstrap(path_bootstrap)
  path_cache <- conan_path_cache(path_cache)

  conan_bootstrap(path_bootstrap)

  dir.create(lib, FALSE, TRUE)
  config <- list(library = lib)

  if (!is.null(path_cache)) {
    ## Path manipulation here seems to be needed to get sensible
    ## location of cache
    path_cache <- file.path(path_cache, "pkg")
    dir.create(path_cache, FALSE, TRUE)
    config$package_cache_dir <- path_cache
  }

  repos <- clean_repos(repos, cran)

  message("CONAN THE LIBRARIAN")
  message("Library:   ", lib)
  message("Bootstrap: ", path_bootstrap)
  message("Cache:     ", path_cache %||% "(unset)")
  message("Policy:    ", policy)
  message("Repos:\n", paste(sprintf(" * %s", repos), collapse = "\n"))
  message("Packages:\n", paste(sprintf(" * %s", packages), collapse = "\n"))

  prev <- .libPaths()
  .libPaths(c(lib, path_bootstrap))
  on.exit(.libPaths(prev), add = TRUE)

  oo <- options(repos = repos)
  on.exit(options(oo), add = TRUE)

  proposal <- conan_proposal(packages, config, policy)
  proposal$solve()
  proposal$stop_for_solution_error()
  proposal$download()
  proposal$stop_for_download_error()
  proposal$install()
}


conan_path <- function(path, var, fallback = tempfile("conan_")) {
  if (!is.null(path)) {
    return(path)
  }
  path <- Sys.getenv(var, NA_character_)
  if (!is.na(path)) {
    return(path)
  }

  if (is.null(fallback)) {
    return(fallback)
  }

  path <- fallback
  names(fallback) <- var
  do.call(Sys.setenv, as.list(fallback))

  fallback
}


conan_path_bootstrap <- function(path) {
  conan_path(path, "CONAN_PATH_BOOTSTRAP", tempfile("conan_"))
}


conan_path_cache <- function(path) {
  conan_path(path, "CONAN_PATH_CACHE", NULL)
}


conan_proposal <- function(packages, config, policy) {
  pkgdepends::new_pkg_installation_proposal(
    filter_packages(packages), config, policy = policy)
}


filter_packages <- function(packages) {
  refs <- pkgdepends::parse_pkg_refs(packages)
  package_names <- vapply(refs, ref_to_package_name, "", USE.NAMES = FALSE)
  is_bare <- packages == package_names
  drop <- is_bare & (packages %in% package_names[!is_bare])
  packages[!drop]
}
