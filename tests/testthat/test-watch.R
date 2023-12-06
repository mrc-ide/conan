test_that("collect logs as we run", {
  get_status <- mockery::mock("waiting",
                              "running", "running", "running",
                              "success")
  get_log <- mockery::mock(NULL,
                           "a", "a", c("a", "b"),
                           c("a", "b", "c"))

  res <- testthat::evaluate_promise(
    conan_watch(get_status, get_log, poll = 0))
  expect_equal(res$result, "success")
  expect_equal(res$output, "")
  expect_true(all(paste0(c("a", "b", "c"), "\n") %in% res$messages))

  mockery::expect_called(get_status, 5)
  mockery::expect_called(get_log, 5)
  expect_equal(mockery::mock_args(get_status), rep(list(list()), 5))
  expect_equal(mockery::mock_args(get_log), rep(list(list()), 5))
})


test_that("conan_watch: no output", {
  get_status <- mockery::mock("waiting",
                              "running", "running", "running",
                              "success")
  get_log <- mockery::mock(NULL,
                           "a", "a", c("a", "b"),
                           c("a", "b", "c"))
  res <- testthat::evaluate_promise(
    conan_watch(get_status, get_log, poll = 0, show_log = FALSE))
  expect_equal(res$result, "success")
  expect_equal(res$output, "")
  expect_false(any(paste0(c("a", "b", "c"), "\n") %in% res$messages))

  mockery::expect_called(get_status, 5)
  mockery::expect_called(get_log, 0)
  expect_equal(mockery::mock_args(get_status), rep(list(list()), 5))
})


test_that("throw on error", {
  get_status <- mockery::mock("waiting", "running", "running", "failure")
  get_log <- mockery::mock(NULL, "a", c("a", "b"), c("a", "b", "c"))
  msg <- capture_messages(
    expect_error(
      conan_watch(get_status, get_log, poll = 0),
      "Installation failed"))
  expect_true(all(paste0(c("a", "b", "c"), "\n") %in% msg))
})


test_that("allow error if requested", {
  get_status <- mockery::mock("waiting", "running", "running", "failure")
  get_log <- mockery::mock(NULL, "a", c("a", "b"), c("a", "b", "c"))
  expect_equal(
    suppressMessages(conan_watch(get_status, get_log, poll = 0, error = FALSE)),
    "failure")
})


test_that("show log differences", {
  expect_silent(res <- show_new_log(NULL, NULL))
  expect_null(res)

  msg <- capture_messages(
    expect_equal(show_new_log(c("a", "b"), NULL), c("a", "b")))
  expect_equal(msg, c("a\nb\n"))

  msg <- capture_messages(
    expect_equal(show_new_log(c("a", "b"), c("a", "b")), c("a", "b")))
  expect_equal(msg, character())

  msg <- capture_messages(
    expect_equal(show_new_log(c("a", "b", "c"), c("a", "b")), c("a", "b", "c")))
  expect_equal(msg, "c\n")
})
