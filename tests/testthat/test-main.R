context("main")

test_that("Parse bootstrap args", {
  expect_error(parse_main_bootstrap(character(0)), "Usage:.*conan-bootstrap")
  expect_error(parse_main_bootstrap("--help"), "Usage:.*conan-bootstrap")
  expect_error(parse_main_bootstrap("--upgrade"), "Usage:.*conan-bootstrap")
  expect_error(parse_main_bootstrap(c("a", "b")), "Usage:.*conan-bootstrap")

  expect_equal(parse_main_bootstrap("lib"),
               list(lib = "lib", upgrade = FALSE))
  expect_equal(parse_main_bootstrap(c("--upgrade", "lib")),
               list(lib = "lib", upgrade = TRUE))
})


test_that("Pass arguments through to target", {
  mock_main <- mockery::mock(cycle = TRUE)
  mockery::stub(main_bootstrap, "conan_bootstrap", mock_main)
  main_bootstrap("mylib")
  main_bootstrap(c("mylib", "--upgrade"))

  mockery::expect_called(mock_main, 2)
  expect_equal(mockery::mock_args(mock_main)[[1]],
               list("mylib", FALSE))
  expect_equal(mockery::mock_args(mock_main)[[2]],
               list("mylib", TRUE))
})


test_that("parse_main_conan", {
  expect_equal(parse_main_conan("lib", "name"), list(lib = "lib"))

  expect_error(
    parse_main_conan(c("path", "lib"), "name"),
    "Usage:.*name <lib>")
  expect_error(
    parse_main_conan(c("--path-bootstrap", "path", "lib"), "name"),
    "Usage:.*name <lib>")
})
