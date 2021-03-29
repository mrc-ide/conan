clean_paths <- function(p) {
  vapply(p, normalizePath, "", USE.NAMES = FALSE)
}
