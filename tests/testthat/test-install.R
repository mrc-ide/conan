context("install")

test_that("Can write basic plan", {
  plan <- tempfile()
  conan_write_plan(plan, c("x", "y", "z"))
  expect_true(file.exists(plan))
  expect_mapequal(
    conan_read_plan(plan),
    list(packages = c("x", "y", "z"),
         repos = character(0),
         policy = "upgrade"))
})


test_that("Can basic plan with extra repo", {
  plan <- tempfile()
  conan_write_plan(plan, c("x", "y", "z"), "https://mrc-ide.github.io/drat")
  expect_true(file.exists(plan))
  expect_mapequal(
    conan_read_plan(plan),
    list(packages = c("x", "y", "z"),
         repos = "https://mrc-ide.github.io/drat",
         policy = "upgrade"))
})


test_that("Can write fancy plan", {
  plan <- tempfile()
  repos <- c("https://mrc-ide.github.io/drat",
             CRAN = "https://cran.example.com")
  conan_write_plan(plan, c("x", "y", "z"), repos, "lazy")
  expect_true(file.exists(plan))
  expect_mapequal(
    conan_read_plan(plan),
    list(packages = c("x", "y", "z"),
         repos = repos,
         policy = "lazy"))
})


test_that("Error if plan not found", {
  plan <- tempfile()
  expect_error(
    conan_read_plan(plan),
    "File does not exist at '.+'")
})


test_that("Path handling", {
  bs <- tempfile()
  withr::with_envvar(
    c("CONAN_PATH_BOOTSTRAP" = bs), {
      expect_equal(conan_path_bootstrap(NULL), bs)
      expect_equal(conan_path_bootstrap("other"), "other")
    })

  withr::with_envvar(
    c("CONAN_PATH_BOOTSTRAP" = NA_character_), {
      tmp <- conan_path_bootstrap(NULL)
      expect_false(file.exists(tmp))
      expect_match(basename(tmp), "conan_")
      expect_equal(conan_path_bootstrap("other"), "other")
    })

  cache <- tempfile()
  withr::with_envvar(
    c("CONAN_PATH_CACHE" = cache), {
      expect_equal(conan_path_cache(NULL), cache)
      expect_equal(conan_path_cache("other"), "other")
    })

  withr::with_envvar(
    c("CONAN_PATH_CACHE" = NA_character_), {
      expect_null(conan_path_cache(NULL))
      expect_equal(conan_path_cache("other"), "other")
    })
})


test_that("Can run an installation from a saved plan", {
  plan <- tempfile()
  conan_write_plan(plan, c("x", "y", "z"), "https://mrc-ide.github.io/drat")
  lib <- tempfile()
  path_bootstrap <- tempfile()
  path_cache <- tempfile()

  mock_install <- mockery::mock(cycle = TRUE)
  mockery::stub(conan_install_plan, "conan_install", mock_install)

  conan_install_plan(lib, plan)
  conan_install_plan(lib, plan, path_bootstrap, path_cache)

  mockery::expect_called(mock_install, 2)

  expect_equal(
    mockery::mock_args(mock_install)[[1]],
    list(lib, c("x", "y", "z"), "upgrade", "https://mrc-ide.github.io/drat",
         NULL, NULL))
  expect_equal(
    mockery::mock_args(mock_install)[[2]],
    list(lib, c("x", "y", "z"), "upgrade", "https://mrc-ide.github.io/drat",
         path_bootstrap, path_cache))
})



test_that("Can run installation", {
  mock_obj <- list(
    solve = mockery::mock(),
    stop_for_solution_error = mockery::mock(),
    download = mockery::mock(),
    stop_for_download_error = mockery::mock(),
    install = mockery::mock())
  mock_pkgdepends <- mockery::mock(mock_obj)
  mock_bootstrap <- mockery::mock()

  mockery::stub(conan_install, "conan_proposal", mock_pkgdepends)
  mockery::stub(conan_install, "conan_bootstrap", mock_bootstrap)

  lib <- tempfile()
  packages <- c("x", "y", "z")
  msg <- capture_messages(conan_install(lib, packages))

  mockery::expect_called(mock_bootstrap, 1)
  mockery::expect_called(mock_pkgdepends, 1)
  expect_equal(
    mockery::mock_args(mock_pkgdepends)[[1]],
    list(packages, list(library = lib), "upgrade"))
  mockery::expect_called(mock_obj$solve, 1)
  mockery::expect_called(mock_obj$stop_for_solution_error, 1)
  mockery::expect_called(mock_obj$download, 1)
  mockery::expect_called(mock_obj$stop_for_download_error, 1)
  mockery::expect_called(mock_obj$install, 1)
})


test_that("Can run installation with cache", {
  mock_obj <- list(
    solve = mockery::mock(),
    stop_for_solution_error = mockery::mock(),
    download = mockery::mock(),
    stop_for_download_error = mockery::mock(),
    install = mockery::mock())
  mock_pkgdepends <- mockery::mock(mock_obj)
  mock_bootstrap <- mockery::mock()

  mockery::stub(conan_install, "conan_proposal", mock_pkgdepends)
  mockery::stub(conan_install, "conan_bootstrap", mock_bootstrap)

  lib <- tempfile()
  packages <- c("x", "y", "z")
  cache <- tempfile()
  bootstrap <- tempfile()
  msg <- capture_messages(
    conan_install(lib, packages, path_bootstrap = bootstrap,
                  path_cache = cache))

  mockery::expect_called(mock_bootstrap, 1)
  expect_equal(
    mockery::mock_args(mock_bootstrap)[[1]],
    list(bootstrap))
  mockery::expect_called(mock_pkgdepends, 1)
  expect_equal(
    mockery::mock_args(mock_pkgdepends)[[1]],
    list(packages, list(library = lib,
                        package_cache_dir = file.path(cache, "pkg")),
         "upgrade"))
})


test_that("Can filter redundant packages", {
  expect_equal(filter_packages(character(0)), character(0))
  expect_equal(filter_packages("pkg"), "pkg")
  expect_equal(filter_packages(c("pkg1", "pkg2")), c("pkg1", "pkg2"))
  expect_equal(filter_packages(c("pkg1", "user/pkg1")), "user/pkg1")
  expect_equal(filter_packages(c("pkg1", "pkg2", "user/pkg1")),
               c("pkg2", "user/pkg1"))
  expect_equal(filter_packages(c("pkg1", "pkg2", "user/pkg1", "user/pkg3")),
               c("pkg2", "user/pkg1", "user/pkg3"))
})
