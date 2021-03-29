context("integration")

## This one is quite slow so we'll need to mostly mock out the bits of it
test_that("Can create bootstrap library", {
  testthat::skip_if_offline()
  skip_unless_ci()

  path_scripts <- tempfile()
  conan_scripts(path_scripts)
  path_bootstrap <- tempfile()

  pkgs <- c("docopt", "pkgcache", "pkgdepends")
  expect_setequal(missing_packages(pkgs, path_bootstrap), pkgs)

  callr::rscript(
    c("--vanilla", file.path(path_scripts, "conan-bootstrap")),
    path_bootstrap,
    echo = FALSE,
    show = FALSE)

  expect_true(
    all(pkgs %in% .packages(TRUE, path_bootstrap)))
  expect_equal(missing_packages(pkgs, path_bootstrap), character(0))
})


test_that("Can install packages", {
  testthat::skip_if_offline()
  skip_unless_ci()

  path_bootstrap <- .libPaths()[[1]]
  path_scripts <- tempfile()
  conan_scripts(path_scripts)

  path_plan <- tempfile()
  conan_write_plan(path_plan,
                   c("cpp11", "dde"),
                   "https://mrc-ide.github.io/drat")

  path_lib <- tempfile()

  env <- c(callr::rcmd_safe_env(),
           "CONAN_PATH_BOOTSTRAP" = path_bootstrap)
  callr::rscript(
    c("--vanilla", file.path(path_scripts, "conan-install"),
      path_lib, path_plan),
    echo = TRUE, env = env)

  expect_true(all(c("cpp11", "dde", "ring") %in% dir(path_lib)))
})


test_that("High level interface", {
  testthat::skip_if_offline()
  skip_unless_ci()

  path_bootstrap <- .libPaths()[[1]]

  path <- conan(tempfile(),
                c("cpp11", "dde"),
                "https://mrc-ide.github.io/drat")
  expect_true(file.exists(path))

  path_lib <- tempfile()
  env <- c(callr::rcmd_safe_env(),
           "CONAN_PATH_BOOTSTRAP" = path_bootstrap)
  callr::rscript(c("--vanilla", path, path_lib),
                 echo = TRUE, env = env)

  expect_true(all(c("cpp11", "dde", "ring") %in% dir(path_lib)))
})
