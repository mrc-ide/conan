test_that("can list conan installations", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)

  d <- conan_list(path)
  expect_equal(names(d), c("name", "time", "hash", "method", "args"))
  expect_equal(nrow(d), 5)
  res <- evaluate_promise(withVisible(print(d)))
  expect_equal(res$result, list(value = d, visible = FALSE))

  msg <- res$messages
  expect_length(msg, 6)
  expect_match(msg[[1]], "5 conan installations recorded")
  expect_match(msg[[2]], sprintf("1: %s \\(.+ago\\) \\[-4\\]", nms[[1]]))
  expect_match(msg[[6]], sprintf("5: %s \\(.+ago\\) \\[0\\]", nms[[5]]))
})


test_that("can list conan installations and compare hash", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)

  h <- conan_list(path)$hash[[4]]

  d <- conan_list(path, h)
  expect_equal(names(d), c("name", "time", "hash", "method", "args", "current"))
  expect_equal(nrow(d), 5)
  res <- evaluate_promise(withVisible(print(d)))
  expect_equal(res$result, list(value = d, visible = FALSE))

  msg <- res$messages
  expect_length(msg, 7)
  expect_match(msg[[1]], "5 conan installations recorded")
  expect_match(msg[[2]], sprintf("1: %s \\(.+ago\\) \\[-4\\]", nms[[1]]))
  expect_match(msg[[6]], sprintf("5: %s \\(.+ago\\) \\[0\\]", nms[[5]]))
  expect_match(msg[[7]], "The entry marked with '*' matches the provided",
               fixed = TRUE)
})


test_that("can list conan installations and compare hash", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)

  d <- conan_list(path, "abc123")
  expect_equal(names(d), c("name", "time", "hash", "method", "args", "current"))
  expect_equal(nrow(d), 5)
  res <- evaluate_promise(withVisible(print(d)))
  expect_equal(res$result, list(value = d, visible = FALSE))

  msg <- res$messages
  expect_length(msg, 7)
  expect_match(msg[[1]], "5 conan installations recorded")
  expect_match(msg[[2]], sprintf("1: %s \\(.+ago\\) \\[-4\\]", nms[[1]]))
  expect_match(msg[[6]], sprintf("5: %s \\(.+ago\\) \\[0\\]", nms[[5]]))
  expect_match(msg[[7]], "No entry matches the provided")
})


test_that("can list conan installations when only one present", {
  path <- withr::local_tempdir()
  nms <- example_installations(path, 1)

  d <- conan_list(path)
  expect_equal(names(d), c("name", "time", "hash", "method", "args"))
  expect_equal(nrow(d), 1)
  res <- evaluate_promise(withVisible(print(d)))
  expect_equal(res$result, list(value = d, visible = FALSE))

  msg <- res$messages
  expect_length(msg, 2)
  expect_match(msg[[1]], "1 conan installation recorded")
  expect_match(msg[[2]], sprintf("1: %s \\(.+ago\\) \\[0\\]", nms[[1]]))
})


test_that("can list conan installations when none are present", {
  path <- withr::local_tempdir()
  nms <- example_installations(path, 0)

  d <- conan_list(path)
  expect_equal(names(d), c("name", "time", "hash", "method", "args"))
  expect_equal(nrow(d), 0)
  res <- evaluate_promise(withVisible(print(d)))
  expect_equal(res$result, list(value = d, visible = FALSE))

  msg <- res$messages
  expect_length(msg, 1)
  expect_match(msg[[1]], "No conan installations recorded")
})
