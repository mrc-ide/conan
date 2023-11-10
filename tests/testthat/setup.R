withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = testthat::teardown_env()
)
