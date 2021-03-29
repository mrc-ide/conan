parse_main_bootstrap <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
conan-bootstrap [options] <lib>

Options:

--upgrade  Upgrade packages if already present?"

  upgrade <- "--upgrade" %in% args
  lib <- setdiff(args, "--upgrade")
  if (length(lib) != 1 || grepl("^-", lib)) {
    stop(usage, call. = FALSE)
  }
  list(lib = lib, upgrade = upgrade)
}


main_bootstrap <- function(args = commandArgs(TRUE)) {
  dat <- parse_main_bootstrap(args)
  conan_bootstrap(dat$lib, dat$upgrade)
}


parse_main_install <- function(args = commandArgs(TRUE)) {
  usage <- "Usage:
conan-install [options] <lib> <plan>

--path-bootstrap=PATH  Path to the conan bootstrap (or set CONAN_PATH_BOOTSTRAP)
--path-cache=PATH      Path to the package cache (or set CONAN_PATH_CACHE)"

  dat <- docopt::docopt(usage, args)
  names(dat) <- gsub("-", "_", names(dat))

  list(lib = dat$lib,
       plan = dat$plan,
       path_bootstrap = dat$path_bootstrap,
       path_cache = dat$path_cache)
}


main_install <- function(args = commandArgs(TRUE)) {
  docopt_bootstrap()
  dat <- parse_main_install(args)
  conan_install_plan(lib = dat$lib,
                     plan = dat$plan,
                     path_bootstrap = dat$path_bootstrap,
                     path_cache = dat$path_cache)
}


parse_main_conan <- function(args = commandArgs(TRUE), name = "conan") {
  usage <- "Usage:
%s <lib>"
  usage <- sprintf(usage, name)
  if (length(args) != 1 || grepl("^-", args)) {
    stop(usage, call. = FALSE)
  }

  list(lib = args)
}
