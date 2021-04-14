# conan <img src=man/figures/logo.gif' align="right" />

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/mrc-ide/conan/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/conan/actions)
[![Build status]()](https://buildkite.com/mrc-ide/mrcide/conan?branch=main)
[![codecov.io](https://codecov.io/github/mrc-ide/conan/coverage.svg?branch=main)](https://codecov.io/github/mrc-ide/conan?branch=main)
<!-- badges: end -->

> For us, there is no spring. Just the wind that smells fresh before the storm

`conan` provides a light wrapper around [`pkgdepends`](https://r-lib.github.io/pkgdepends/) in order to create standalone libraries. Our use case is creating libraries for use with HPC systems. It is not as broad or designed for interactive use like [`renv`](https://rstudio.github.io/renv/articles/renv.html), [`packrat`](https://rstudio.github.io/packrat/) or [`pak`](https://rstudio.github.io/packrat/). Instead it tries to address the narrow problem of "*how do you install packages from diverse sources without necessarily already having your package installer present?*".

To solve this problem, create a standlone script that can install its own dependencies and then install a required set of packages:


```r
conan::conan(
  "script",
  c("cpp11",
    "mrc-ide/dust",
    "mrc-ide/mcstate@some-feature"))
```

That will create a file like:

```r
#!/usr/bin/env Rscript
`%||%` <- function (x, y)
{
    if (is.null(x))
        y
    else x
[... skip ...]
.dat <- parse_main_conan(name = "script")
.packages <- c("cpp11", "mrc-ide/dust", "mrc-ide/mcstate@some-feature")
.repos <- c(CRAN = "https://cloud.r-project.org")
.policy <- "upgrade"
.lib <- .dat$lib
conan_install(.lib, .packages, policy = .policy, repos = .repos)
```

Then this script can be run from the command line:

```bash
./script lib
```

(or on Windows `Rscript script lib`), which will create a library at path `lib`



## Dependency resolution

All the dependency resolution is done by [`pkgdepends`](https://r-lib.github.io/pkgdepends/). Use of the Remotes field in DESCRIPTION can create impossible installation situations, beware. In particular, if a package is intalled from a cran-like repo and also via a `Remotes:` field in a package you are installing via a remotes-style reference, then you will get a "conflict" from pkgdepends.

## Installation

To install `conan`:

```r
remotes::install_github("mrc-ide/conan", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
