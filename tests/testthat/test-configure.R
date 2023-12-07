test_that("can create basic configuration", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  expect_s3_class(cfg, "conan_config")
  expect_equal(cfg$script, "provision.R")
  expect_equal(cfg$method, "script")
  expect_equal(cfg$path_lib, "path/lib")
  expect_equal(cfg$path_bootstrap, "path/bootstrap")
  expect_false(cfg$delete_first)
  expect_true(cfg$show_log)
  expect_equal(cfg$poll, 1)
})


test_that("require that provisioning script exists", {
  path <- withr::local_tempdir()
  expect_error(
    conan_configure("script", path = path,
                    path_lib = "path/lib", path_bootstrap = "path/bootstrap"),
    "provision script 'provision.R' does not exist at path")
  expect_error(
    conan_configure("script", script = "foo.R", path = path,
                    path_lib = "path/lib", path_bootstrap = "path/bootstrap"),
    "provision script 'foo.R' does not exist at path")
})


test_that("validate no extra args are given", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "provision.R"))
  expect_error(
    conan_configure("script", path = path, other = "a",
                    path_lib = "path/lib", path_bootstrap = "path/bootstrap"),
    "Unknown arguments in '...' for method 'script': 'other'",
    fixed = TRUE)
  expect_error(
    conan_configure("script", path = path, other = "a", x = 1,
                    path_lib = "path/lib", path_bootstrap = "path/bootstrap"),
    "Unknown arguments in '...' for method 'script': 'other', 'x'",
    fixed = TRUE)
})


test_that("reject unknown provisioning method", {
  expect_error(
    conan_configure("magic", path = path),
    "Unknown provision method 'magic'")
})


test_that("error if desired provisioning method unclear", {
  path <- withr::local_tempdir()
  expect_error(
    conan_configure(NULL, path = path, path_lib = "path/lib",
                    path_bootstrap = "path/bootstrap"),
    "Could not detect provisioning method for path")
})


test_that("Require that path_lib is relative", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "provision.R"))
  expect_error(
    conan_configure(NULL, path = path, path_lib = "/path/lib",
                    path_bootstrap = "path/bootstrap"),
    "'path_lib' must be a relative path")
})
