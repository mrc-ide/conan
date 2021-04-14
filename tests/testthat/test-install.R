context("install")

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


test_that("Can run installation", {
  mock_obj <- list(
    solve = mockery::mock(),
    stop_for_solution_error = mockery::mock(),
    download = mockery::mock(),
    stop_for_download_error = mockery::mock(),
    install = mockery::mock())
  mock_proposal <- mockery::mock(mock_obj)
  mock_bootstrap <- mockery::mock()

  mockery::stub(conan_install, "conan_proposal", mock_proposal)
  mockery::stub(conan_install, "conan_bootstrap", mock_bootstrap)

  lib <- tempfile()
  packages <- c("x", "y", "z")
  msg <- capture_messages(conan_install(lib, packages))

  mockery::expect_called(mock_bootstrap, 1)
  mockery::expect_called(mock_proposal, 1)
  expect_equal(
    mockery::mock_args(mock_proposal)[[1]],
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
  mock_proposal <- mockery::mock(mock_obj)
  mock_bootstrap <- mockery::mock()

  mockery::stub(conan_install, "conan_proposal", mock_proposal)
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
  mockery::expect_called(mock_proposal, 1)
  expect_equal(
    mockery::mock_args(mock_proposal)[[1]],
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
