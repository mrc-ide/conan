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


test_that("parse install args", {
  expect_error(parse_main_install(parse_main_install(character())), "Usage:")

  expect_mapequal(
    parse_main_install(c("path", "plan.json")),
    list(lib = "path", plan = "plan.json", path_bootstrap = NULL,
         path_cache = NULL))

  expect_mapequal(
    parse_main_install(c("path", "--path-bootstrap=bs", "plan.json")),
    list(lib = "path", plan = "plan.json", path_bootstrap = "bs",
         path_cache = NULL))
  expect_mapequal(
    parse_main_install(c("path", "--path-cache=cache", "plan.json")),
    list(lib = "path", plan = "plan.json", path_bootstrap = NULL,
         path_cache = "cache"))
})


test_that("Pass arguments through to target", {
  mock_bootstrap <- mockery::mock(cycle = TRUE)
  mock_main <- mockery::mock(cycle = TRUE)
  mockery::stub(main_install, "docopt_bootstrap", mock_bootstrap)
  mockery::stub(main_install, "conan_install_plan", mock_main)

  main_install(c("mylib", "plan.json"))

  mockery::expect_called(mock_bootstrap, 1)
  mockery::expect_called(mock_main, 1)
  expect_equal(mockery::mock_args(mock_bootstrap)[[1]], list())
  expect_mapequal(mockery::mock_args(mock_main)[[1]],
                  list(lib = "mylib", plan = "plan.json",
                       path_bootstrap = NULL, path_cache = NULL))
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
