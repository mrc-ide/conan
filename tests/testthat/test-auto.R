test_that("can build a very straightforward case", {
  path <- withr::local_tempfile()
  dir.create(path)
  expect_equal(
    build_pkgdepends_auto(list(packages = c("apple", "banana")), path),
    list(refs = c("apple", "banana"),
         repos = character()))
  expect_equal(
    build_pkgdepends_auto(list(packages = "apple"), path),
    list(refs = "apple",
         repos = character()))
  expect_error(
    build_pkgdepends_auto(list(), path),
    "I could work out anything to install automatically")
})


test_that("can find dependencies in source files", {
  path <- withr::local_tempfile()
  dir.create(path)
  writeLines(
    c("banana::split()",
      "library(cucumber)",
      "if (smelly) {",
      '  require("durian")',
      "}"),
    file.path(path, "src.R"))
  res <- build_pkgdepends_auto(list(packages = "apple", sources = "src.R"),
                               path)
  expect_setequal(res$refs, c("apple", "banana", "cucumber", "durian"))
  expect_equal(res$repos, character())
})


test_that("can detect complex refs", {
  desc <- function(...) {
    structure(list(...), class = "packageDescription")
  }
  mock_pkg_desc <- mockery::mock(
    desc(Repository = "CRAN"),
    desc(Repository = "https://mrc-ide.r-universe.dev"),
    desc(RemoteRef = "HEAD", RemoteUsername = "user-c", RemoteRepo = "repo-c"),
    desc(RemoteRef = "branch", RemoteUsername = "user-d", RemoteRepo = "repo-d",
         RemoteSubdir = "path/to/src"))
  mockery::stub(packages_to_pkgdepends, "utils::packageDescription",
                mock_pkg_desc)
  res <- packages_to_pkgdepends(c("a", "b", "c", "d"))
  expect_mapequal(
    res,
    list(repos = "https://mrc-ide.r-universe.dev",
         refs = c("a", "b", "user-c/repo-c@HEAD",
                  "user-d/repo-d@branch/path/to/src")))
  mockery::expect_called(mock_pkg_desc, 4)
  expect_equal(mockery::mock_args(mock_pkg_desc),
               list(list("a"), list("b"), list("c"), list("d")))
})
