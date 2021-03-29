##' High-level conan interface. This function creates a standalone
##' script that can provision a library at any location. The script
##' has no dependencies and can be copied to another system and create
##' a library given an empty R installation.
##'
##' @title Conan the Librarian
##'
##' @param filename Path to write the install script to. Any directory
##'   components will be created as needed.
##'
##' @param dryrun Logical, indicating if we should try a dryrun with
##'   [conan::conan_dryrun()]; if this passes your requested packages
##'   seem satisfiable.
##'
##' @inheritParams conan_install
##'
##' @return Invisibly, the path to the created script. This can be run
##'   via `Rscript` to create a standalone library. The script will
##'   require exactly one argument: the path to the library to
##'   create. It will respond to the environment variables
##'   `CONAN_PATH_BOOTSTRAP` and `CONAN_PATH_CACHE`.
##'
##' @export
##' @examples
##' path <- conan::conan(tempfile(), "cpp11")
##' writeLines(tail(readLines(path)))
conan <- function(filename, packages, repos = NULL, policy = "upgrade",
                  dryrun = FALSE) {
  if (dryrun) {
    conan_dryrun(packages, repos, policy)
  }
  code <- c(
    "#!/usr/bin/env Rscript",
    sprintf('cran_rcloud <- "%s"', cran_rcloud),
    extract_code(c("conan_install", "parse_main_conan")),
    sprintf('.dat <- parse_main_conan(name = "%s")', basename(filename)),
    sprintf(".packages <- %s", deparse_str(unname(packages))),
    sprintf(".repos <- %s", deparse_str(clean_repos(repos))),
    sprintf(".policy <- %s", deparse_str(policy)),
    ".lib <- .dat$lib",
    "conan_install(.lib, .packages, policy = .policy, repos = .repos)")

  dir.create(dirname(filename), FALSE, TRUE)
  write_script_exec(code, filename)
}
