context("sources")

test_that("conan_sources creates sensible structure", {
  expect_equal(
    conan_sources("ape"),
    structure(list(packages = "ape",
                   repos = clean_repos(NULL)),
              class = "conan_sources"))
})


test_that("conan_sources rewrites local references", {
  path <- tempfile(fileext = ".tar.gz")
  expect_error(
    conan_sources(path),
    "Local package source '.*' does not exist")
  expect_error(
    conan_sources(c("abc", path, "xyz")),
    "Local package source '.*' does not exist")
  file.create(path)
  res <- conan_sources(path)
  expect_equal(res$packages, paste0("local::", path))

  res <- conan_sources(c("abc", path, "xyz"))
  expect_equal(res$packages, c("abc", paste0("local::", path), "xyz"))
})


test_that("allow windows paths", {
  path <- "C:\\file\\to\\package.tar.gz"
  expect_error(
    conan_sources(path),
    "Local package source 'C:\\file\\to\\package.tar.gz' does not exist",
    fixed = TRUE)
})
