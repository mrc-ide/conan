context("dryrun")

test_that("Can run a dryrun", {
  ans <- conan_dryrun("cpp11")
  expect_s3_class(ans, "pkg_installation_proposal")
})


test_that("Can fail a dryrun", {
  expect_error(
    conan_dryrun("nonexistantpackage"))
  expect_message(
    ans <- conan_dryrun("nonexistantpackage", error = FALSE))
  expect_s3_class(ans, "pkg_installation_proposal")
})
