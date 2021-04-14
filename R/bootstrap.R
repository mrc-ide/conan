##' Create a bootstrap library
##'
##' @title Create a bootstrap library
##'
##' @param path Path to create library
##'
##' @param upgrade Logical, indicating if the bootstrap packages
##'   should be upgraded if newer versions are available.
##'
##' @return Invisibly, `path`, but primarily called for its side
##'   effects, which is to create or update the library at `path`
##'
##' @export
##' @examples
##' if (interactive()) {
##'   path <- tempfile()
##'   conan::conan_bootstrap(path)
##'   dir(path)
##' }
conan_bootstrap <- function(path, upgrade = FALSE) {
  dir.create(path, FALSE, TRUE)
  prev <- .libPaths()
  on.exit(.libPaths(prev))
  .libPaths(path)
  ## TODO: I've empirically added R6, curl and jsonlite here, but I
  ## think it's because they were already found in some system
  ## library, but then others *were* found.
  req <- c("R6", "curl", "docopt", "jsonlite", "pkgcache", "pkgdepends")
  if (!upgrade) {
    req <- missing_packages(req, path)
  }
  if (length(req) > 0) {
    prefix_message(
      img_boots(), 1, "conan is bootstrapping...",
      list("working" = getwd(),
           "target" = path))
    install_packages(req, path, default_cran())
  }
  for (i in req) {
    loadNamespace(i, path)
  }
  invisible(path)
}


docopt_bootstrap <- function() {
  path_bs <- Sys.getenv("CONAN_PATH_BOOTSTRAP", NA_character_)
  lib <- .libPaths()
  if (!is.na(path_bs)) {
    lib <- c(path_bs, lib)
  }
  tryCatch(
    loadNamespace("docopt", lib),
    error = function(e) {
      lib <- tempfile()
      dir.create(lib, FALSE, TRUE)
      install_packages("docopt", lib, default_cran())
      loadNamespace("docopt", lib)
    })
}


install_packages <- function(packages, lib, repos) {
  if (length(packages) == 0) {
    return()
  }
  utils::install.packages(packages, lib, repos)
  msg <- missing_packages(packages, lib)
  if (length(msg) > 0) {
    stop("Failed to install package: ",
         paste(sprintf("'%s'", msg), collapse = ", "))
  }
}


missing_packages <- function(packages, lib) {
  setdiff(packages, .packages(TRUE, lib))
}
