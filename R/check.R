##' Check to see if all packages are available in a given library
##'
##' @title Check if packages in library
##'
##' @param packages A character vector of package names or pkgdepends
##'   "references"
##'
##' @param library A path to a library
##'
##' @return A list with elements
##' * `complete`: logical, indicating if all packages were found
##' * `found`: A character vector of found packages
##' * `missing`: A character vector of missing packages
##'
##' @export
##' @examples
##' # Simple usage:
##' conan::conan_check(c("conan", "pkgdepends"), .libPaths())
##'
##' # While we parse references, we don't check version information:
##' conan::conan_check("github::mrc-ide/conan@v2.0.0", .libPaths())
##'
##' # Missing packages will be returned as the inferred package name
##' conan::conan_check("github::org/unknownpkg", .libPaths())
conan_check <- function(packages, library) {
  refs <- pkgdepends::parse_pkg_refs(packages)
  pkgs <- vapply(refs, ref_to_package_name, "", USE.NAMES = FALSE)
  found <- sort(intersect(.packages(TRUE, library), pkgs))
  msg <- setdiff(pkgs, found)
  list(complete = length(msg) == 0,
       found = sort(found),
       missing = sort(msg))
}
