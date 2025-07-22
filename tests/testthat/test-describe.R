test_that("can parse package sources", {
  cran <- list(
    Package = "ape",
    Repository = "CRAN",
    Built = "R 4.3.1; x86_64-pc-linux-gnu; 2023-07-23 15:04:29 UTC; unix")
  expect_equal(package_source(cran), c("CRAN", NA))

  bioconductor <- list(
    Package = "BiocVersion",
    biocViews = "Infrastructure")
  expect_equal(package_source(bioconductor), c("Bioconductor", NA))

  local <- list(
    Package = "conan")
  expect_equal(package_source(local), c("local", NA))

  universe <- list(
    Package = "orderly2",
    Repository = "https://mrc-ide.r-universe.dev")
  expect_equal(package_source(universe), c("Universe", "mrc-ide"))

  custom <- list(
    Package = "something",
    Repository = "https://example.com")
  expect_equal(package_source(custom), c("Custom", "https://example.com"))

  ## Real install with remotes::install_github("richfitz/rfiglet")
  gh <- list(
    Package = "rfiglet",
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteRepo = "rfiglet",
    RemoteUsername = "richfitz",
    RemoteRef = "HEAD",
    RemoteSha = "d713c1b8c7cb6dc73e966f3e4f5f2647168a9060")
  expect_equal(package_source(gh), c("github", "richfitz/rfiglet (d713c1b)"))

  ## Real install with
  ## pak::pkg_install("mrc-ide/hipercow/drivers/windows@mrc-4796")
  insubdir <- list(
    Package = "hipercow.windows",
    RemoteType = "github",
    RemoteHost = "api.github.com",
    RemoteRepo = "hipercow",
    RemoteUsername = "mrc-ide",
    RemotePkgRef = "mrc-ide/hipercow/drivers/windows@mrc-4796",
    RemoteRef = "mrc-4796",
    RemoteSha = "707ef195f4cf37fe241e42600d147a4e8abd8c34",
    RemoteSubdir = "drivers/windows")
  expect_equal(
    package_source(insubdir),
    c("github", "mrc-ide/hipercow/drivers/windows@mrc-4796 (707ef19)"))

  insubdir$RemotePkgRef <- NULL
  expect_equal(
    package_source(insubdir),
    c("github", "mrc-ide/hipercow/drivers/windows@mrc-4796 (707ef19)"))

  ## pak::pkg_install(
  ## https://cran.r-project.org/src/contrib/Archive/tibble/tibble_3.1.7.tar.gz)
  url_pak <- list(
    Package = "tibble",
    RemotePkgRef = "url::https://example.org/src/tibble_3.1.7.tar.gz",
    RemoteType = "url")
  expect_equal(
    package_source(url_pak),
    c("url", "https://example.org/src/tibble_3.1.7.tar.gz"))

  url_remotes <- list(
    Package = "tibble",
    RemoteType = "url",
    RemoteUrl = "https://example.org/src/tibble_3.1.7.tar.gz")
  expect_equal(
    package_source(url_remotes),
    c("url", "https://example.org/src/tibble_3.1.7.tar.gz"))

  base <- list(
    Package = "utils",
    Priority = "base")
  expect_equal(
    package_source(base),
    c("base", NA))

  unknown <- list(
    Package = "potato",
    RemoteType = "garden")
  expect_warning(
    res <- package_source(unknown),
    "Could not determine source details for 'potato'")
  expect_equal(res, c("garden", "(unknown)"))
})


test_that("can extract times", {
  pkg <- list(
    Package = "foo",
    Packaged = "2023-12-22 06:41:05 UTC; root",
    Built = "R 4.3.2; ; 2024-01-04 15:55:09 UTC; unix")
  expect_equal(
    package_time(pkg),
    structure(1703227265, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
  expect_equal(
    package_time(pkg[c(1, 2)]),
    structure(1703227265, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
  expect_equal(
    package_time(pkg[c(1, 3)]),
    structure(1704383709, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
  expect_warning(
    res <- package_time(pkg[1]),
    "Could not determine time for package 'foo'")
  expect_equal(
    res,
    structure(NA_real_, class = c("POSIXct", "POSIXt"), tzone = "UTC"))
})


test_that("can convert a library description into a df", {
  x <- conan_describe(.Library)
  df <- as.data.frame(x)
  expect_equal(nrow(df), length(x$packages))
  expect_equal(names(df), c("package", "version", "time", "source", "details"))
})


test_that("can describe an empty library", {
  path <- withr::local_tempfile()
  dir_create(path)
  res <- conan_describe(path)
  expect_s3_class(res, "conan_describe")
  expect_setequal(names(res), c("packages", "time"))
  expect_s3_class(res$time, "POSIXt")
  expect_equal(res$packages, setNames(list(), character()))
  df <- as.data.frame(res)
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("package", "version", "time", "source", "details"))
})
