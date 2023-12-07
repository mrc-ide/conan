test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
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
  expect_gte(a, 2) # this is hard on the very slow mac runner
})
