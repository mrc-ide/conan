test_that("can parse pkgdepends lists", {
  expect_equal(
    pkgdepends_parse(character()),
    list(repos = NULL, refs = character()))
  expect_equal(
    pkgdepends_parse("foo"),
    list(repos = NULL, refs = "foo"))
  expect_equal(
    pkgdepends_parse(c("foo", "bar")),
    list(repos = NULL, refs = c("foo", "bar")))
  expect_equal(
    pkgdepends_parse(c("# a comment", "foo", "bar")),
    list(repos = NULL, refs = c("foo", "bar")))
  expect_equal(
    pkgdepends_parse(c("foo", "", "bar")),
    list(repos = NULL, refs = c("foo", "bar")))
  expect_equal(
    pkgdepends_parse(c(" foo", "   ", "bar ")),
    list(repos = NULL, refs = c("foo", "bar")))
  expect_equal(
    pkgdepends_parse(c("repo::https://mrc-ide.r-universe.dev", "foo")),
    list(repos = "https://mrc-ide.r-universe.dev", refs = "foo"))
  expect_error(
    pkgdepends_parse("foo-bar"),
    "Failed to parse some package references")
  expect_error(
    pkgdepends_parse(c("foo", "bar", "foo-bar")),
    "Failed to parse some package references")
})
