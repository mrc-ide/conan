test_that("can run a script-based installation", {
  testthat::skip_if_offline()
  path <- withr::local_tempdir()
  writeLines('install.packages("R6")', file.path(path, "provision.R"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library(NULL)
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))
  expect_true(file.exists(file.path(path, "lib", "R6")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "script")
  expect_equal(d$hash, rlang::hash_file(file.path(path, "provision.R")))
  expect_equal(d$hash, cfg$hash)
  expect_equal(d$args$script, "provision.R")
  expect_false(d$args$delete_first)
  expect_s3_class(d$description, "conan_describe")
  expect_true("R6" %in% names(d$description$packages))

  h <- cfg$hash
  d <- conan_list(file.path(path, path_lib))
  expect_equal(d$hash, h)
  expect_null(d$current)

  d <- conan_list(file.path(path, path_lib), h)
  expect_equal(d$hash, h)
  expect_true(d$current)
})


test_that("can run a pkgdepends-based installation", {
  testthat::skip_if_offline()
  path <- withr::local_tempdir()
  writeLines("R6", file.path(path, "pkgdepends.txt"))
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))
  expect_true(file.exists(file.path(path, "lib", "R6")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "pkgdepends")
  expect_equal(d$hash, cfg$hash)
  expect_mapequal(d$args$pkgdepends, list(refs = "R6", repos = NULL))
  expect_s3_class(d$description, "conan_describe")
  expect_true("R6" %in% names(d$description$packages))
})


test_that("can run an automatic installation", {
  testthat::skip_if_offline()
  path <- withr::local_tempdir()
  environment <- list(packages = "R6")
  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         delete_first = TRUE,
                         path_bootstrap = path_bootstrap,
                         environment = environment)
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))
  expect_true(file.exists(file.path(path, "lib", "R6")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "auto")
  expect_equal(d$hash, cfg$hash)
  expect_equal(d$args$pkgdepends$refs, "R6") # repos may differ
  expect_true(d$args$delete_first)
  expect_s3_class(d$description, "conan_describe")
  expect_true("R6" %in% names(d$description$packages))
})


test_that("can run an renv installation", {
  testthat::skip_if_offline()
  path <- withr::local_tempdir()
  writeLines("library(R6)", file.path(path, "code.R"))
  res <- withr::with_dir(path, callr::r(function() {
    renv::init()
    install.packages("R6")
    renv::snapshot()
    list(pkgs = .packages(TRUE), libs = .libPaths())
  }))

  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("renv")
  cfg <- conan_configure("renv", path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap)
  expect_equal(cfg$hash, rlang::hash_file(file.path(path, "renv.lock")))
  withr::with_dir(path, conan_run(cfg, show_log = FALSE))

  expect_true(file.exists(file.path(path, "lib", "R6")))
  expect_true(file.exists(file.path(path, "lib", "renv")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "renv")
  expect_equal(d$hash, cfg$hash)
  expect_s3_class(d$description, "conan_describe")
  expect_true(all(c("R6", "renv") %in% names(d$description$packages)))
})


## This test requires the TEST_CONAN_GITHUB_TOKEN to exist, and
## installs a trivial private package with it; you need access to
## reside-ic/ to run this test.
test_that("can install a private package", {
  pat <- Sys.getenv("TEST_CONAN_GITHUB_TOKEN", NA)
  if (is.na(pat)) {
    testthat::skip("TEST_CONAN_GITHUB_TOKEN not found")
  }
  withr::local_envvar(GITHUB_TOKEN = NA, GITHUB_PAT = NA)

  path <- withr::local_tempdir()
  writeLines("reside-ic/secretsquirrel", file.path(path, "pkgdepends.txt"))
  path_key <- withr::local_tempfile()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key, path_key)
  pub <- as.list(key)$pubkey
  pat <- openssl::base64_encode(openssl::rsa_encrypt(charToRaw(pat), pub))
  envvars <- data.frame(name = "GITHUB_TOKEN", value = pat, secret = TRUE)
  attr(envvars, "key") <- path_key

  path_lib <- "lib"
  path_bootstrap <- bootstrap_library("pkgdepends")
  cfg <- conan_configure(NULL, path = path, path_lib = path_lib,
                         path_bootstrap = path_bootstrap, envvars = envvars)

  withr::with_dir(path, conan_run(cfg, show_log = FALSE))

  expect_true(file.exists(file.path(path, "lib", "secretsquirrel")))

  expect_true(file.exists(file.path(path, "lib", ".conan")))
  expect_length(dir(file.path(path, "lib", ".conan")), 1)
  d <- readRDS(dir(file.path(path, "lib", ".conan"), full.names = TRUE))

  expect_equal(d$method, "pkgdepends")
  expect_equal(d$hash, cfg$hash)
  expect_mapequal(d$args$pkgdepends,
                  list(refs = "reside-ic/secretsquirrel", repos = NULL))
  expect_s3_class(d$description, "conan_describe")
  expect_true("secretsquirrel" %in% names(d$description$packages))
})
