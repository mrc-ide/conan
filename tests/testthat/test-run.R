test_that("can write out script based on configuration", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  dest <- file.path(path, "tmp", "script.R")
  conan_write(cfg, dest)
  expect_true(file.exists(dest))

  dat <- template_data(cfg)
  expect_setequal(setdiff(names(dat), names(cfg)),
                  c("repos", "preload", "what", "args_str",
                    "conan_describe_definition", "set_envvars"))
  expect_equal(dat$repos, vector_to_str("https://cloud.r-project.org"))
  expect_equal(dat$preload, vector_to_str("remotes"))
  expect_equal(dat$what, "your installation script 'provision.R'")
  expect_equal(dat$conan_describe_definition, deparse_fn("conan_describe", 2))
  expect_equal(dat$hash, cfg$hash)
  expect_equal(dat$args_str,
               list_to_str(cfg[setdiff(names(cfg), c("method", "hash"))]))
  expect_equal(dat$set_envvars, "")
})


test_that("can write out pkgdepends script based on configuration", {
  path <- withr::local_tempdir()
  writeLines("foo", file.path(path, "pkgdepends.txt"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap",
                         cran = "https://cran.example.com")
  dest <- file.path(path, "tmp", "script.R")
  conan_write(cfg, dest)
  expect_true(file.exists(dest))

  dat <- template_data(cfg)
  expect_setequal(setdiff(names(dat), names(cfg)),
                  c("repos", "refs", "preload", "what", "args_str",
                    "conan_describe_definition", "set_envvars"))
  expect_equal(dat$repos, vector_to_str("https://cran.example.com"))
  expect_equal(dat$refs, vector_to_str("foo"))
  expect_equal(dat$what, "pkgdepends")
  expect_equal(dat$hash, cfg$hash)
  expect_equal(dat$args_str,
               list_to_str(cfg[setdiff(names(cfg), c("method", "hash"))]))
})


test_that("can run an installation", {
  mock_rscript <- mockery::mock()
  mockery::stub(conan_run, "callr::rscript", mock_rscript)
  path <- withr::local_tempdir()
  file.create(file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  withr::with_dir(path, conan_run(cfg))
  mockery::expect_called(mock_rscript, 1)
  args <- mockery::mock_args(mock_rscript)[[1]]
  expect_setequal(names(args), c("", "stdout", "stderr", "show"))
  expect_equal(args$stdout, args$stderr)
  expect_true(args$show)
  expect_equal(dirname(args$stdout), dirname(args[[1]]))
  expect_true(file.exists(dirname(args$stdout)))
})


test_that("can generate code to set environment variables", {
  withr::defer(Sys.unsetenv(c("CONAN_A", "CONAN_B")))
  envvars <- data.frame(name = c("CONAN_A", "CONAN_B"),
                        value = c("x", "y"),
                        secret = FALSE)
  expect_equal(
    generate_set_envvars(envvars),
    "  Sys.setenv(\"CONAN_A\" = \"x\")\n  Sys.setenv(\"CONAN_B\" = \"y\")")

  code <- generate_set_envvars(envvars)
  tmp <- withr::local_tempfile()
  writeLines(code, tmp)
  env <- new.env()
  sys.source(tmp, env)
  expect_equal(names(env), character())
  expect_equal(Sys.getenv("CONAN_A"), "x")
  expect_equal(Sys.getenv("CONAN_B"), "y")
})


test_that("can generate code to set secret environment variables", {
  withr::defer(Sys.unsetenv("CONAN_SECRET"))
  path_key <- withr::local_tempfile()
  key <- openssl::rsa_keygen()
  openssl::write_pem(key, path_key)
  pub <- as.list(key)$pubkey
  secret <-
    openssl::base64_encode(openssl::rsa_encrypt(charToRaw("secret"), pub))
  envvars <- data.frame(name = "CONAN_SECRET", value = secret, secret = TRUE)
  attr(envvars, "key") <- path_key

  code <- generate_set_envvars(envvars)
  tmp <- withr::local_tempfile()
  writeLines(code, tmp)
  env <- new.env()
  sys.source(tmp, env)
  expect_equal(Sys.getenv("CONAN_SECRET"), "secret")
  expect_equal(names(env), "decrypt")
  expect_equal(env$decrypt(secret), "secret")
})
