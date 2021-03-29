context("conan")

test_that("Run high level interface", {
  path <- conan(tempfile(),
                c("cpp11", "dde"),
                "https://mrc-ide.github.io/drat")
  expect_true(file.exists(path))
})
