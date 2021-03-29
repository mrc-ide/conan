context("conan")

test_that("Run high level interface", {
  path <- conan(tempfile(),
                c("cpp11", "dde"),
                "https://mrc-ide.github.io/drat")
  expect_true(file.exists(path))
})


test_that("Dryrun before write", {
  testthat::skip_if_offline()
  path <- tempfile()
  expect_error(conan(path,
                     c("cpp11", "nonexistantpackage"),
                     dryrun = TRUE))
  expect_false(file.exists(path))

  conan(path, "cpp11", dryrun = TRUE)
  expect_true(file.exists(path))
})
