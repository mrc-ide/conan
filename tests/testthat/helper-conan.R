bootstrap_library <- function(pkg) {
  if (length(pkg) == 0) {
    .libPaths()[[1]]
  } else {
    dirname(find.package(pkg))
  }
}


example_installations <- function(path, n = NULL) {
  fs::dir_create(file.path(path, ".conan"))
  path_rds <- "examples/installations.rds"
  if (file.exists("tests/testthat")) {
    path_rds <- file.path("tests/testthat", path_rds)
  }
  d <- readRDS(path_rds)
  n <- n %||% length(d)
  nms <- names(d)[seq_len(n)]
  for (nm in nms) {
    saveRDS(d[[nm]], file.path(path, ".conan", nm))
  }
  nms
}
