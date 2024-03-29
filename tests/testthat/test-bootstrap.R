context("bootstrap")

test_that("upgrade installs all packages without checking", {
  mock_missing <- mockery::mock()
  mock_install <- mockery::mock()
  mock_load_namespace <- mockery::mock()
  mockery::stub(conan_bootstrap, "missing_packages", mock_missing)
  mockery::stub(conan_bootstrap, "install_packages", mock_install)
  mockery::stub(conan_bootstrap, "loadNamespace", mock_load_namespace)

  path <- tempfile()
  expect_message(
    conan_bootstrap(path, TRUE),
    "conan is bootstrapping...")

  mockery::expect_called(mock_missing, 0)
  mockery::expect_called(mock_install, 1)
  packages <- c("R6", "curl", "docopt", "jsonlite", "pkgcache", "pkgdepends")
  expect_equal(
    mockery::mock_args(mock_install)[[1]],
    list(packages, path, default_cran()))
  mockery::expect_called(mock_load_namespace, length(packages))
  expect_equal(
    mockery::mock_args(mock_load_namespace),
    Map(list, packages, path, USE.NAMES = FALSE))
})


test_that("Skip installed packages", {
  mock_missing <- mockery::mock("docopt")
  mock_install <- mockery::mock()
  mock_load_namespace <- mockery::mock()

  mockery::stub(conan_bootstrap, "missing_packages", mock_missing)
  mockery::stub(conan_bootstrap, "install_packages", mock_install)
  mockery::stub(conan_bootstrap, "loadNamespace", mock_load_namespace)

  path <- tempfile()
  conan_bootstrap(path)

  mockery::expect_called(mock_missing, 1)
  packages <- c("R6", "curl", "docopt", "jsonlite", "pkgcache", "pkgdepends")
  expect_equal(
    mockery::mock_args(mock_missing)[[1]],
    list(packages, path))
  expect_equal(
    mockery::mock_args(mock_install)[[1]],
    list("docopt", path, default_cran()))
})


test_that("Bootstrap is silent if nothing needs doing", {
  mock_missing <- mockery::mock()
  mock_install <- mockery::mock()
  mock_load_namespace <- mockery::mock()

  mockery::stub(conan_bootstrap, "missing_packages", mock_missing)
  mockery::stub(conan_bootstrap, "install_packages", mock_install)
  mockery::stub(conan_bootstrap, "loadNamespace", mock_load_namespace)

  path <- tempfile()
  expect_silent(conan_bootstrap(path))

  mockery::expect_called(mock_missing, 1)
  packages <- c("R6", "curl", "docopt", "jsonlite", "pkgcache", "pkgdepends")
  expect_equal(
    mockery::mock_args(mock_missing)[[1]],
    list(packages, path))
  mockery::expect_called(mock_install, 0)
})


test_that("Can load docopt in bootstrap", {
  mock_load_ns <- mockery::mock(cycle = TRUE)
  mock_install <- mockery::mock(cycle = TRUE)

  mockery::stub(docopt_bootstrap, "loadNamespace", mock_load_ns)
  mockery::stub(docopt_bootstrap, "install_packages", mock_install)

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  lib_a <- file.path(tmp, "a")
  lib_b <- file.path(tmp, "b")
  lib_bs <- file.path(tmp, "x")
  lib_base <- c(lib_a, lib_b)
  dir.create(lib_bs, FALSE, TRUE)
  dir.create(lib_a, FALSE, TRUE)
  dir.create(lib_b, FALSE, TRUE)
  withr::with_envvar(
    c(CONAN_PATH_BOOTSTRAP = lib_bs),
    withr::with_libpaths(
      lib_base,
      docopt_bootstrap()))

  mockery::expect_called(mock_load_ns, 1)
  mockery::expect_called(mock_install, 0)
  args <- mockery::mock_args(mock_load_ns)[[1]]
  expect_equal(args[[1]], "docopt")
  expect_equal(clean_paths(args[[2]][1:3]),
               clean_paths(c(lib_bs, lib_base)))
})


test_that("Can install docopt in bootstrap", {
  mock_load_ns <- mockery::mock(stop("there is no package called 'docopt'"),
                                NULL)
  mock_install <- mockery::mock(cycle = TRUE)

  mockery::stub(docopt_bootstrap, "loadNamespace", mock_load_ns)
  mockery::stub(docopt_bootstrap, "install_packages", mock_install)

  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  lib_a <- file.path(tmp, "a")
  lib_b <- file.path(tmp, "b")
  lib_bs <- file.path(tmp, "x")
  lib_base <- c(lib_a, lib_b)
  dir.create(lib_bs, FALSE, TRUE)
  dir.create(lib_a, FALSE, TRUE)
  dir.create(lib_b, FALSE, TRUE)
  withr::with_envvar(
    c(CONAN_PATH_BOOTSTRAP = lib_bs),
    withr::with_libpaths(
      lib_base,
      docopt_bootstrap()))

  mockery::expect_called(mock_load_ns, 2)
  mockery::expect_called(mock_install, 1)

  args <- mockery::mock_args(mock_install)[[1]]
  lib_tmp <- args[[2]]
  expect_false(lib_tmp %in% c(.libPaths(), lib_base, lib_bs))
  expect_equal(args, list("docopt", lib_tmp, default_cran()))

  args <- mockery::mock_args(mock_load_ns)[[1]]
  expect_equal(args[[1]], "docopt")
  expect_equal(clean_paths(args[[2]][1:3]),
               clean_paths(c(lib_bs, lib_base)))

  expect_equal(mockery::mock_args(mock_load_ns)[[2]],
               list("docopt", lib_tmp))
})


test_that("Install packages will skip over empty list", {
  mock_install <- mockery::mock(cycle = TRUE)
  mockery::stub(install_packages, "utils::install.packages", mock_install)
  lib <- tempfile()
  expect_silent(install_packages(character(0), lib, default_cran()))
  expect_false(file.exists(lib))
  mockery::expect_called(mock_install, 0)
})


test_that("Install packages will error if installation fails", {
  mock_install <- mockery::mock(cycle = TRUE)
  mockery::stub(install_packages, "utils::install.packages", mock_install)
  lib <- tempfile()
  dir.create(lib, FALSE, TRUE)
  expect_error(
    install_packages(c("pkg.a", "pkg.b"), lib, default_cran()))
  mockery::expect_called(mock_install, 1)
  expect_equal(mockery::mock_args(mock_install)[[1]],
               list(c("pkg.a", "pkg.b"), lib, default_cran()))
})
