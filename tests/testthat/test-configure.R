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
  expect_equal(cfg$hash, rlang::hash_file(file.path(path, "provision.R")))
  expect_false(cfg$delete_first)
  expect_equal(cfg$cran, "https://cloud.r-project.org")
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
    "I could not work out anything to install automatically")
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
  expect_equal(cfg$hash, "ca8aa9ed287947861b086201509bd545")
})


test_that("can detect a pkgdepends installation", {
  path <- withr::local_tempdir()
  writeLines(c("repo::https://mrc-ide.r-universe.dev", "ids", "odin"),
             file.path(path, "pkgdepends.txt"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  expect_equal(cfg$method, "pkgdepends")
  expect_equal(cfg$hash, "db76fa4412cef127825620f141019980")
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


test_that("prefer script over pkgdepends", {
  path <- withr::local_tempdir()
  expect_equal(detect_method(path), "auto")
})


test_that("prefer renv the most", {
  path <- withr::local_tempdir()
  mock_using_renv <- mockery::mock(TRUE, cycle = TRUE)
  mockery::stub(detect_method, "using_renv", mock_using_renv)
  expect_equal(detect_method(path), "renv")
  file.create(file.path(path, "pkgdepends.txt"))
  file.create(file.path(path, "provision.R"))
  expect_equal(detect_method(path), "renv")
})


test_that("can fall back on automatic installation", {
  path <- withr::local_tempdir()
  env <- list(packages = c("apple", "banana"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap",
                         environment = env)
  expect_equal(cfg$method, "auto")
  expect_equal(cfg$hash, "4f900c3aa5950fde464772a0e38bbb22")
  expect_mapequal(cfg$pkgdepends,
                  list(repos = character(), refs = c("apple", "banana")))
  expect_equal(
    conan_configure("auto", path = path, path_lib = "path/lib",
                    path_bootstrap = "path/bootstrap", environment = env),
    cfg)
})


test_that("can set an alternative repo", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "provision.R"))
  cfg <- suppressMessages(
    conan_configure(NULL, path = path, path_lib = "path/lib",
                    path_bootstrap = "path/bootstrap",
                    cran = "https://cran.example.com"))
  expect_equal(cfg$cran, "https://cran.example.com")
})


test_that("can validate environment variables", {
  expect_null(check_envvars(NULL))
  expect_error(check_envvars(c("A" = "x")),
               "Expected 'envvars' to be a data.frame")
  expect_error(check_envvars(data.frame(nm = "A", vl = "x")),
               "Missing columns from 'envvars': 'name' and 'value'")
  expect_error(check_envvars(data.frame(name = "A", vl = "x")),
               "Missing column from 'envvars': 'value'")

  expect_equal(
    check_envvars(data.frame(name = "A", value = "x")),
    data.frame(name = "A", value = "x", secret = FALSE))
  expect_equal(
    check_envvars(data.frame(name = c("A", "B"), value = c("x", "y"))),
    data.frame(name = c("A", "B"), value = c("x", "y"), secret = FALSE))

  d <- data.frame(name = c("A", "B"), value = c("x", "y"),
                  secret = c(TRUE, FALSE))
  expect_error(
    check_envvars(d),
    "Secret environment variables in 'envvars', but key not found")
  attr(d, "key") <- "/path/to/key"
  expect_equal(check_envvars(d), d)
})


test_that("can create configuration with envars", {
  path <- withr::local_tempdir()
  envvars <- data.frame(name = c("ENV_A", "ENV_B"),
                        value = c("val_a", "val_b"))
  file.create(file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap",
                         envvars = envvars)
  expect_s3_class(cfg, "conan_config")
  expect_equal(cfg$script, "provision.R")
  expect_equal(cfg$method, "script")
  expect_equal(cfg$path_lib, "path/lib")
  expect_equal(cfg$path_bootstrap, "path/bootstrap")
  expect_equal(cfg$hash, rlang::hash_file(file.path(path, "provision.R")))
  expect_false(cfg$delete_first)
  expect_equal(cfg$envvars, cbind(envvars, secret = FALSE))
})


test_that("can use pkgdepends in alternative path", {
  path <- withr::local_tempdir()
  writeLines(c("pkg1", "pkg2"), file.path(path, "mypkgs.txt"))

  cfg <- conan_configure("pkgdepends", filename = "mypkgs.txt",
                         path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  expect_equal(cfg$filename, "mypkgs.txt")
  expect_mapequal(cfg$pkgdepends, list(refs = c("pkg1", "pkg2"), repos = NULL))
})


test_that("don't specify both refs and filename", {
  path <- withr::local_tempdir()
  writeLines(c("pkg1", "pkg2"), file.path(path, "mypkgs.txt"))

  expect_error(
    conan_configure("pkgdepends", refs = c("pkg1", "pkg2"),
                    filename = "mypkgs.txt",
                    path = path, path_lib = "path/lib",
                    path_bootstrap = "path/bootstrap"),
    "Don't both 'filename' and 'refs'")
})
