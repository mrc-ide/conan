clean_paths <- function(p) {
  vapply(p, normalizePath, "", USE.NAMES = FALSE)
}


skip_unless_ci <- function() {
  testthat::skip_on_cran()
  if (isTRUE(as.logical(Sys.getenv("CI")))) {
    return(invisible(TRUE))
  }
  testthat::skip("Not on CI")
}
