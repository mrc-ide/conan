context("integration")

test_that("Can create bootstrap library", {
  skip_unless_ci()
  testthat::skip_if_offline()

  path_bootstrap <- tempfile()

  pkgs <- c("docopt", "pkgcache", "pkgdepends")
  expect_setequal(missing_packages(pkgs, path_bootstrap), pkgs)

  callr::r(
    function(path_bootstrap) conan::conan_bootstrap(path_bootstrap),
    list(path_bootstrap))

  expect_true(
    all(pkgs %in% .packages(TRUE, path_bootstrap)))
  expect_equal(missing_packages(pkgs, path_bootstrap), character(0))
})


test_that("High level interface", {
  skip_unless_ci()
  testthat::skip_if_offline()

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


test_that("Watch a conan installation", {
  skip_unless_ci()
  testthat::skip_if_offline()

  path <- conan(tempfile(),
                c("cpp11", "dde"),
                "https://mrc-ide.github.io/drat")
  expect_true(file.exists(path))

  path_lib <- tempfile()
  path_log <- tempfile()
  path_bootstrap <- .libPaths()[[1]]
  env <- c(callr::rcmd_safe_env(),
           "CONAN_PATH_BOOTSTRAP" = path_bootstrap)

  ## There is no callr::rscript_bg so we need to do a bit of a faff
  ## here to simulate it.
  px <- callr::r_bg(
    function(path, path_lib, path_log, env) {
      callr::rscript(c("--vanilla", path, path_lib),
                     stdout = path_log, stderr = path_log, env = env)
    }, list(path, path_lib, path_log, env))

  ## Assume can't fail for now
  get_status <- function() {
    if (px$is_alive()) "RUNNING" else "COMPLETE"
  }

  get_log <- function() {
    if (!file.exists(path_log)) {
      return(NULL)
    }
    readLines(path_log, warn = FALSE)
  }

  res <- evaluate_promise(
    conan_watch(get_status, get_log, show_progress = FALSE, show_log = TRUE))

  expect_equal(res$result, "COMPLETE")
  expect_match(res$messages, "CONAN THE LIBRARIAN", all = FALSE)
  expect_true(all(c("cpp11", "dde") %in% dir(path_lib)))
})
