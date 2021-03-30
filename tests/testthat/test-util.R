context("util")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("clean repos fixes broken repos", {
  cran_rcloud <- "https://cloud.r-project.org"
  expect_equal(clean_repos(NULL), c(CRAN = cran_rcloud))
  expect_equal(clean_repos(c(CRAN = "@CRAN@")), c(CRAN = cran_rcloud))
  expect_equal(clean_repos(c(CRAN = "https://cran.example.com")),
               c(CRAN = "https://cran.example.com"))
  expect_equal(clean_repos("https://example.com"),
               c("https://example.com", CRAN = cran_rcloud))
})
