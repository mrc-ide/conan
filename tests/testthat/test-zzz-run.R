test_that("can run an installation", {
  testthat::skip_on_covr() # no idea why this fails...
  path <- withr::local_tempdir()
  writeLines('install.packages("R6")', file.path(path, "provision.R"))
  path_lib <- "lib"
  path_bootstrap <- .libPaths()[[1]]
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap, show_log = FALSE)
  withr::with_dir(path, conan_run(cfg))
  expect_true(file.exists(file.path(path, "lib", "R6")))
})
