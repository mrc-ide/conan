# conan <img src="man/figures/logo.gif" align="right" />

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R-CMD-check](https://github.com/mrc-ide/conan/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mrc-ide/conan/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/mrc-ide/conan/coverage.svg?branch=main)](https://app.codecov.io/github/mrc-ide/conan?branch=main)
<!-- badges: end -->

> For us, there is no spring. Just the wind that smells fresh before the storm

## What is this?

Package installation in R is a double edged sword. On one hand, installation is pretty easy; especially for packages on CRAN, users interactively run `install.packages(...)` and they are good to go. Packages typically work well together, which means that it's safe (and typical) to install packages into a single library, one per user.

Things start getting creaky though:

* while `install.packages()` works well interactively, it works poorly programmatically, and does not reliably error when it fails! This makes running a script to install a set of packages ahead of a cluster run problematic.
* increasingly, our users make use of their own packages, created with the help of [`devtools`](https://devtools.r-lib.org/) and typically installed via [`remotes`](https://remotes.r-lib.org/); these packages typically have more breaking changes, users are less disciplined about updating version numbers.
* where users have a collection of interdependent packages installed via `remotes` using the `Remotes:` metadata in `DESCRIPTION`, the final versions of packages installed can be unreliable.

Our needs in an HPC/HTC context are slightly different again:

* it is not safe to start multiple package installations in parallel, e.g., in an HPC setting; we need a system where installation can be reliably performed on a single job, then used by all the
* we want a system to reliably reconstruct a library of packages on a remote machine, based on the set that the user has installed locally, and do this in a way that does not accidentally trigger installation at the point where the user has launched 1,000 jobs in parallel.
* we need to use libraries that are user-, project-, and machine- scoped; so if the user has two different projects they are isolated, if the user's local machine is a Mac but the remote machine is Linux they use different packages, and that different users don't interact

The [`renv` package](https://rstudio.github.io/renv/) solves many of these problems, but it is not to everyone's taste; we will support it here but try and work around it updating the library unexpectedly when jobs launch.

We aim for a system where the users can switch between a few different modes of package installation (see below) and where we can add new modes as these become popular/available.  For each mode we need to be able to:

* install a set of packages into a library
* determine if the library is up to date, even if it was constructed for a different system
* force rerunning the installation
* destroy and recreate a library
* provide detailed logging and debugging information when things go wrong
* offer an escape hatch (run some script to tweak the library when things go wrong)
* easily created minimally reproducible examples when sets of packages create impossible-to-resolve dependency sets

The back-ends we want to support are:

* `renv`, which theoretically meets all our requirements, but which is a little fiddly to use in practice and requires that users adapt their workflows considerably. This is pretty fiddly to work with, so we might do this last?
* `script`, some sort of escape hatch where the user provides a script that just runs through with whatever installation approach they fancy.
* `pkgdepends`, some sort of metadata format, using `pkgdepends` to try and install everything. For this, we might try and replicate the versions found locally, and report on differences we find

Once we have this, we want to support being able to run this remotely and watch the output - we have some reasonable support for this already in the old version of `conan` with the "watcher"; we can adapt this to work on a shared filesystem and also over ssh.

In the current version, debugging failed installation (especially when `pkgdepends` solver fails) is really hard, so we need to improve:

* setting up the bootstrap library so that it's easy to report back on the version of `pkgdepends` used remotely vs the one we debug with locally
* converting out of `conan` and into plain `pkgdepends` code so that we can get bug reports sorted

## Installation

To install `conan`:

```r
remotes::install_github("mrc-ide/conan", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
