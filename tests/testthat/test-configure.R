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


test_that("can configure pkgdepends with character vector", {
  cfg <- conan_configure("pkgdepends", refs = "foo", path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  expect_equal(cfg$method, "pkgdepends")
  expect_equal(cfg$pkgdepends, list(repos = NULL, refs = "foo"))
})


test_that("can detect a pkgdepends installation", {
  path <- withr::local_tempdir()
  writeLines(c("repo::https://mrc-ide.r-universe.dev", "ids", "odin"),
             file.path(path, "pkgdepends.txt"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  expect_equal(cfg$method, "pkgdepends")
  expect_equal(cfg$pkgdepends,
               list(repos = "https://mrc-ide.r-universe.dev",
                    refs = c("ids", "odin")))
  expect_equal(
    conan_configure("pkgdepends", path = path, path_lib = "path/lib",
                    path_bootstrap = "path/bootstrap"),
    cfg)
})


test_that("require pkgdepends.txt to exist", {
  path <- withr::local_tempdir()
  expect_error(
    conan_configure("pkgdepends", path = path, path_lib = "path/lib",
                    path_bootstrap = "path/bootstrap"),
    "Expected a file 'pkgdepends.txt' to exist at path")
})


test_that("prefer script over pkgdepends", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "pkgdepends.txt"))
  file.create(file.path(path, "provision.R"))
  expect_equal(detect_method(path), "script")
})
