test_that("can run a script-based installation", {
  skip("ignore")
  path <- withr::local_tempdir()
  writeLines('install.packages("R6")', file.path(path, "provision.R"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library(NULL)
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap, show_log = FALSE)
  withr::with_dir(path, conan_run(cfg))
  expect_true(file.exists(file.path(path, "lib", "R6")))
})


test_that("can run a pkgdepends-based installation", {
  skip("ignore")
  path <- withr::local_tempdir()
  writeLines("R6", file.path(path, "pkgdepends.txt"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap, show_log = FALSE)
  withr::with_dir(path, conan_run(cfg))
  expect_true(file.exists(file.path(path, "lib", "R6")))
})
