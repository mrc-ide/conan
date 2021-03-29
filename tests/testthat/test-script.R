context("script")

test_that("can write scripts", {
  path <- tempfile()
  conan_scripts(path)
  expect_setequal(dir(path), c("conan-bootstrap", "conan-install"))
})
