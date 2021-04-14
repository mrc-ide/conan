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


parse_main_conan <- function(args = commandArgs(TRUE), name = "conan") {
  usage <- "Usage:
%s <lib>"
  usage <- sprintf(usage, name)
  if (length(args) != 1 || grepl("^-", args)) {
    stop(usage, call. = FALSE)
  }

  list(lib = args)
}
