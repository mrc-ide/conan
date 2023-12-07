bootstrap_library <- function(pkg) {
  if (length(pkg) == 0) {
    .libPaths()[[1]]
  } else {
    dirname(find.package(pkg))
  }
}
