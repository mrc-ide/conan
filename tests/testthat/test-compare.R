test_that("can select sensible comparisons by index", {
  nms <- c("a", "b", "c", "d", "e")

  ## Simple use with negative index:
  expect_equal(compare_select("lib", nms, 0, -1), list(curr = 5, prev = 4))
  expect_equal(compare_select("lib", nms, 0, -4), list(curr = 5, prev = 1))
  expect_equal(compare_select("lib", nms, 0, -5), list(curr = 5, prev = NULL))
  expect_equal(compare_select("lib", nms, 0, -Inf), list(curr = 5, prev = NULL))
  expect_equal(compare_select("lib", nms, 0, NULL), list(curr = 5, prev = NULL))
  expect_equal(compare_select("lib", nms, -3, -4), list(curr = 2, prev = 1))

  ## Simple use with positive index:
  expect_equal(compare_select("lib", nms, 2, 1), list(curr = 2, prev = 1))
  expect_equal(compare_select("lib", nms, 0, 1), list(curr = 5, prev = 1))
  expect_equal(compare_select("lib", nms, -1, 2), list(curr = 4, prev = 2))
  err <- expect_error(compare_select("lib", nms, 6, 1),
                      "Invalid entry '6' for 'curr'")
  expect_equal(err$body, c(i = "Maximum allowed value is 5"))
})


test_that("can select sensible comparisons by name", {
  expect_equal(compare_select("lib", letters[1:5], "b", "a"),
               list(curr = 2, prev = 1))
  err <- expect_error(
    compare_select("lib", letters[1:5], "foo", "a"),
    "Invalid entry 'foo' for 'curr'")
  expect_equal(
    err$body,
    c(i = "Valid entries are: 'a', 'b', 'c', 'd', and 'e'"))
  err <- expect_error(
    compare_select("lib", letters[1:3], "foo", "a"),
    "Invalid entry 'foo' for 'curr'")
  expect_equal(
    err$body,
    c(i = "Valid entries are: 'a', 'b', and 'c'"))
  err <- expect_error(
    compare_select("lib", letters, "foo", "a"),
    "Invalid entry 'foo' for 'curr'")
  expect_equal(
    err$body,
    c(i = "Valid entries include: 'v', 'w', 'x', 'y', and 'z'"))
})


test_that("comparisons must follow the arrow of time concept", {
  expect_error(compare_select("lib", letters[1:5], 3, 5),
               "'curr' (installation 3) is earlier than 'prev' (5)",
               fixed = TRUE)
  expect_error(compare_select("lib", letters[1:5], 3, 3),
               "'curr' (installation 3) is the same as 'prev'",
               fixed = TRUE)
})

test_that("prevent problematic comparisons", {
  expect_error(compare_select("lib", character(), 0, -1),
               "No conan installations found at 'lib'")
  expect_error(compare_select("lib", letters[1:5], -Inf, -Inf),
               "'curr' must be a real installation")
})


test_that("compare empty and initial", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path, 1, NULL)
  expect_s3_class(res, "conan_compare")

  cmp <- readRDS(file.path(path, ".conan", nms[[1]]))
  expect_equal(res$curr$name, nms[[1]])
  expect_equal(res$curr$index, 1)
  expect_equal(res$curr$age, -4)
  expect_equal(res$curr$packages, as.data.frame(cmp$description))

  expect_null(res$prev)
  expect_equal(res$status, c(R6 = "added"))

  expect_equal(res$changes$added, "{.strong R6} ({.new 2.5.1}) CRAN")
  expect_equal(res$changes$unchanged, character())
  expect_equal(res$changes$removed, character())
  expect_equal(res$changes$updated, character())
})


test_that("compare two with additions only", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path, 2, 1)
  expect_s3_class(res, "conan_compare")

  cmp1 <- readRDS(file.path(path, ".conan", nms[[1]]))
  cmp2 <- readRDS(file.path(path, ".conan", nms[[2]]))

  expect_equal(res$curr$name, nms[[2]])
  expect_equal(res$curr$index, 2)
  expect_equal(res$curr$age, -3)
  expect_equal(res$curr$packages, as.data.frame(cmp2$description))

  expect_equal(res$prev$name, nms[[1]])
  expect_equal(res$prev$index, 1)
  expect_equal(res$prev$age, -4)
  expect_equal(res$prev$packages, as.data.frame(cmp1$description))

  expect_mapequal(res$status,
                  c(R6 = "unchanged",
                    askpass = "added",
                    ids = "added",
                    openssl = "added",
                    sys = "added",
                    uuid = "added"))

  expect_equal(res$changes$unchanged, "{.strong R6} (2.5.1) CRAN")
  expect_equal(res$changes$added,
               c("{.strong askpass} ({.new 1.2.0}) CRAN",
                 "{.strong ids} ({.new 1.0.1}) CRAN",
                 "{.strong openssl} ({.new 2.1.1}) CRAN",
                 "{.strong sys} ({.new 3.4.2}) CRAN",
                 "{.strong uuid} ({.new 1.1.1}) CRAN"))
  expect_equal(res$changes$removed, character())
  expect_equal(res$changes$updated, character())
})


test_that("compare two with deletions", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path)
  expect_s3_class(res, "conan_compare")
  expect_mapequal(res$status,
                  c(R6 = "unchanged",
                    askpass = "removed",
                    ids = "unchanged",
                    openssl = "removed",
                    sys = "unchanged",
                    uuid = "unchanged"))
  expect_setequal(res$changes$unchanged,
                  c("{.strong R6} (2.5.1) CRAN",
                    "{.strong ids} (1.2.2) github: richfitz/ids (07e7325)",
                    "{.strong sys} (3.4.2) CRAN",
                    "{.strong uuid} (1.1.1) CRAN"))
  expect_equal(res$changes$removed,
               c("{.strong askpass} ({.old 1.2.0}) CRAN",
                 "{.strong openssl} ({.old 2.1.1}) CRAN"))
  expect_equal(res$changes$added, character())
  expect_equal(res$changes$updated, character())
})


test_that("compare two with updates", {
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  res <- conan_compare(path, -1, -2)
  expect_s3_class(res, "conan_compare")
  expect_mapequal(res$status,
                  c(R6 = "unchanged",
                    askpass = "unchanged",
                    ids = "updated",
                    openssl = "unchanged",
                    sys = "unchanged",
                    uuid = "unchanged"))
  expect_length(res$changes$unchanged, 5)
  expect_length(res$changes$added, 0)
  expect_length(res$changes$removed, 0)
  expect_equal(
    res$changes$updated,
    paste("{.strong ids} ({.old 1.1.3} -> {.new 1.2.2})",
          "github: richfitz/ids{{{.old @tidy (651433d)} ->",
          "{.new  (07e7325)}}}"))
})


test_that("can describe changes in detail", {
  expect_equal(
    details_changes("user/repo (x)", "user/repo@foo (x)"),
    "user/repo{{{.old  (x)} -> {.new @foo (x)}}}")
  expect_equal(
    details_changes("user/repo@foo (x)", "user/repo@bar (y)"),
    "user/repo@{{{.old foo (x)} -> {.new bar (y)}}}")
  expect_equal(
    details_changes("user/repo@foo (x)", "other/repo@bar (y)"),
    "{.old user/repo@foo (x)} -> {.new other/repo@bar (y)}")
})


test_that("can print simple comparison", {
  withr::local_options(cli.num_colors = 0)
  path <- withr::local_tempdir()
  nms <- example_installations(path, 1)
  msg <- capture_messages(print(conan_compare(path)))
  msg <- msg[msg != "\n"] # work around theming issues
  expect_match(msg[[1]], "Comparing conan installations")
  expect_match(msg[[2]], "(empty installation)", fixed = TRUE)
  expect_match(msg[[3]], "[0-9]{14} 1st; current installation")
  expect_match(msg[[4]], "1 added package")
  expect_match(msg[[5]], "R6 (2.5.1) CRAN", fixed = TRUE)
})


test_that("can print complex comparison", {
  withr::local_options(cli.num_colors = 0)
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  msg <- capture_messages(print(conan_compare(path)))
  msg <- msg[msg != "\n"] # work around theming issues
  expect_match(msg[[1]], "Comparing conan installations")
  expect_match(msg[[2]], "[0-9]{14} 4th; previous installation")
  expect_match(msg[[3]], "[0-9]{14} 5th; current installation")
  expect_match(msg[[4]], "4 unchanged packages")
  expect_match(msg[[5]], "To show unchanged packages, print with",
               fixed = TRUE)
  expect_match(msg[[6]], "2 removed packages")
  expect_match(msg[[7]], "askpass (1.2.0) CRAN", fixed = TRUE)
  expect_match(msg[[8]], "openssl (2.1.1) CRAN", fixed = TRUE)
})


test_that("can print long comparison", {
  withr::local_options(cli.num_colors = 0)
  path <- withr::local_tempdir()
  nms <- example_installations(path)
  msg <- capture_messages(print(conan_compare(path, prev = -4)))
  expect_match(msg[[1]], "Comparing conan installations")
  expect_match(msg[[2]], "[0-9]{14} 1st; 4 installations ago")
  expect_match(msg[[3]], "[0-9]{14} 5th; current installation")
})


test_that("can generate updated changes", {
  nms <- "a"
  curr1 <- data.frame(
    package = "a", version = "1.2.3", source = "CRAN", details = NA)
  curr2 <- data.frame(
    package = "a", version = "1.2.3", source = "github", details = "bob/repo")
  prev1 <- data.frame(
    package = "a", version = "1.2.3", source = "CRAN", details = NA)
  prev2 <- data.frame(
    package = "a", version = "1.1.3", source = "CRAN", details = NA)
  prev3 <- data.frame(
    package = "a", version = "1.2.3", source = "github", details = "user/repo")
  ## we might change this one later
  expect_equal(compare_changes_updated(nms, curr1, prev1),
               "{.strong a} (1.2.3) CRAN")
  expect_equal(compare_changes_updated(nms, curr1, prev2),
               "{.strong a} ({.old 1.1.3} -> {.new 1.2.3}) CRAN")
  expect_equal(compare_changes_updated(nms, curr1, prev3),
               "{.strong a} (1.2.3) {.old github: user/repo} -> {.new CRAN}")
  expect_equal(
    compare_changes_updated(nms, curr2, prev3),
    "{.strong a} (1.2.3) github: {.old user/repo} -> {.new bob/repo}")
})
