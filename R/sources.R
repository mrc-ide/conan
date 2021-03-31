##' Describe sources
##'
##' @title Describe sources
##'
##' @inheritParams conan
##'
##' @return An object of type `conan_sources` which can be used later
##'   with [conan::conan], [conan::conan_install] etc
##'
##' @author Rich FitzJohn
##' @export
##' @examples
##' conan::conan_sources(c("pkg1", "pkg2"))
conan_sources <- function(packages, repos = NULL, cran = NULL) {
  ## Standardise references as this will make working with them later
  ## easier:
  for (i in seq_along(packages)) {
    ## Cope with windows paths
    if (grepl("^[A-Za-z]:", packages[[i]])) {
      packages[[i]] <- paste0("local::", packages[[i]])
    }
    dat <- pkgdepends::parse_pkg_ref(packages[[i]])
    if (dat$type == "local") {
      if (!file.exists(dat$path)) {
        stop(sprintf("Local package source '%s' does not exist", dat$path))
      }
      if (!grepl("^local::", packages[[i]])) {
        packages[[i]] <- sprintf("local::%s", packages[[i]])
      }
    }
  }
  ret <- list(packages = packages,
              repos = clean_repos(repos, cran))
  class(ret) <- "conan_sources"
  ret
}
