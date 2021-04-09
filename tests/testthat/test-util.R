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



test_that("throttle", {
  a <- 0
  f <- function(n) {
    a <<- a + n
  }
  throttled <- throttle(0.05)
  t1 <- Sys.time() + 0.5
  while (Sys.time() < t1) {
    throttled(f(1))
  }
  expect_lte(a, 11)
  expect_gte(a, 5)
})


test_that("Can extract package name", {
  f <- function(x) {
    ref_to_package_name(pkgdepends::parse_pkg_ref(x))
  }
  expect_equal(f("./pkg.tar.gz"), "pkg")
  expect_equal(f("./pkg_0.1-2.tar.gz"), "pkg")
  expect_equal(f("./pkg_0.1-2.tgz"), "pkg")
  expect_equal(f("./pkg_0.1-2.zip"), "pkg")
  expect_equal(f("local::pkg.tar.gz"), "pkg")
  expect_equal(f("user/repo"), "repo")
  expect_equal(f("std"), "std")
})
