## See pkgdepends readme; this is required for the integration tests
## to succeed under R CMD check
withr::local_envvar(
  R_USER_CACHE_DIR = tempfile(),
  .local_envir = teardown_env()
)
