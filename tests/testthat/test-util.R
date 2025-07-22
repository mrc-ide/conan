test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("can convert a vector to string representation", {
  expect_equal(vector_to_str("x"), '"x"')
  expect_equal(vector_to_str(c("x", "y")), 'c("x", "y")')
})


test_that("can convert numbers to ordinals", {
  expect_equal(ordinal(1), "1st")
  expect_equal(ordinal(2), "2nd")
  expect_equal(ordinal(3), "3rd")
  expect_equal(ordinal(4), "4th")
  expect_equal(ordinal(11), "11th")
  expect_equal(ordinal(12), "12th")
  expect_equal(ordinal(13), "13th")
  expect_equal(ordinal(14), "14th")
  expect_equal(ordinal(21), "21st")
  expect_equal(ordinal(22), "22nd")
  expect_equal(ordinal(23), "23rd")
  expect_equal(ordinal(24), "24th")
  expect_equal(ordinal(43221), "43221st")
})


test_that("don't warn when reading from files without newlines", {
  path <- withr::local_tempfile()
  writeLines("hello", sep = "", path)
  expect_warning(readLines(path))
  expect_no_warning(read_lines(path))
})
