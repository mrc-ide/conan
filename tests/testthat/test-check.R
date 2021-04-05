context("check")

test_that("conan_check finds packages", {
  expect_equal(
    conan_check("stats", .libPaths()),
    list(complete = TRUE,
         found = "stats",
         missing = character(0)))
})


test_that("conan_check works with references", {
  expect_equal(
    conan_check(c("cran::stats", "org/unknownpkg"), .libPaths()),
    list(complete = FALSE,
         found = "stats",
         missing = "unknownpkg"))
})