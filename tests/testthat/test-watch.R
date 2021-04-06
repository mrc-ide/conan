context("watch")


test_that("conan_watch: logs but no progress", {
  get_status <- mockery::mock("PENDING",
                              "RUNNING", "RUNNING", "RUNNING",
                              "COMPLETE")
  get_log <- mockery::mock(NULL,
                           "a", "a", c("a", "b"),
                           c("a", "b", "c"))

  res <- testthat::evaluate_promise(
    conan_watch(get_status, get_log, poll = 0, show_progress = FALSE))
  expect_equal(res$result, "COMPLETE")
  expect_equal(res$output, "")
  expect_equal(res$messages,
               paste0(c("a", "b", "c"), "\n"))

  mockery::expect_called(get_status, 5)
  mockery::expect_called(get_log, 5)
  expect_equal(mockery::mock_args(get_status), rep(list(list()), 5))
  expect_equal(mockery::mock_args(get_log), rep(list(list()), 5))
})


test_that("conan_watch: no output", {
  get_status <- mockery::mock("PENDING",
                              "RUNNING", "RUNNING", "RUNNING",
                              "COMPLETE")
  get_log <- mockery::mock(NULL,
                           "a", "a", c("a", "b"),
                           c("a", "b", "c"))
  res <- testthat::evaluate_promise(
    conan_watch(get_status, get_log, poll = 0,
                show_progress = FALSE, show_log = FALSE))
  expect_equal(res$result, "COMPLETE")
  expect_equal(res$output, "")
  expect_equal(res$messages, character(0))

  mockery::expect_called(get_status, 5)
  mockery::expect_called(get_log, 0)
  expect_equal(mockery::mock_args(get_status), rep(list(list()), 5))
})


test_that("conan_watch: all output", {
  get_status <- mockery::mock("PENDING",
                              "RUNNING", "RUNNING", "RUNNING",
                              "COMPLETE")
  get_log <- mockery::mock(NULL,
                           "a", "a", c("a", "b"),
                           c("a", "b", "c"))
  res <- testthat::evaluate_promise(
    conan_watch(get_status, get_log, poll = 0))
  expect_equal(res$result, "COMPLETE")
  expect_equal(res$output, "")

  ## Basically not possible to test this nicely:
  expect_true("[-]  0s ..." %in% res$messages)
  expect_true("a\n" %in% res$messages)
})


test_that("throw on error", {
  get_status <- mockery::mock("PENDING", "RUNNING", "RUNNING", "ERROR")
  get_log <- mockery::mock(NULL, "a", c("a", "b"), c("a", "b", "c"))
  msg <- capture_messages(
    expect_error(
      conan_watch(get_status, get_log, poll = 0, show_progress = FALSE),
      "Installation failed"))
  expect_equal(msg, paste0(c("a", "b", "c"), "\n"))
})


test_that("allow error if requested", {
  get_status <- mockery::mock("PENDING", "RUNNING", "RUNNING", "ERROR")
  get_log <- mockery::mock(NULL, "a", c("a", "b"), c("a", "b", "c"))
  res <- evaluate_promise(
    conan_watch(get_status, get_log, poll = 0, show_progress = FALSE,
                error = FALSE))
  expect_equal(res$result, "ERROR")
})
