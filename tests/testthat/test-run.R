test_that("can write out script based on configuration", {
  path <- withr::local_tempdir()
  file.create(file.path(path, "provision.R"))
  cfg <- conan_configure(NULL, path = path, path_lib = "path/lib",
                         path_bootstrap = "path/bootstrap")
  dest <- file.path(path, "tmp", "script.R")
  conan_write(cfg, dest)
  expect_true(file.exists(dest))
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
