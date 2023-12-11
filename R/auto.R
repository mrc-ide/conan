build_pkgdepends_auto <- function(environment, path_root, verbose) {
  packages <- environment$packages
  sources <- environment$sources

  if (length(sources) > 0) {
    packages <- union(packages,
                      find_additional_packages(file.path(path_root, sources)))
  }

  if (length(packages) == 0) {
    cli::cli_abort(
      "I could work out anything to install automatically",
      i = paste("Your environment does not list any packages, so I have",
                "nothing to install; this is probably not what you want!"))
  }

  packages_to_pkgdepends(packages)
}


packages_to_pkgdepends <- function(packages) {
  repos <- character()
  refs <- packages %||% character()

  for (i in seq_along(packages)) {
    p <- packages[[i]]
    desc <- suppressWarnings(utils::packageDescription(p))
    if (!inherits(desc, "packageDescription")) { # insane defaults
      desc <- NULL
    }
    has_repository <- !is.null(desc$Repository) && desc$Repository != "CRAN"
    has_remote <- !is.null(desc$RemoteRef)
    if (has_repository) {
      repos <- union(repos, desc$Repository)
    } else if (has_remote) {
      ref <- sprintf("%s/%s@%s", desc$RemoteUsername, desc$RemoteRepo,
                     desc$RemoteRef)
      if (!is.null(desc$RemoteSubdir)) {
        ref <- sprintf("%s/%s", ref, desc$RemoteSubdir)
      }
      refs[[i]] <- ref
    }
  }

  list(refs = refs, repos = repos)
}


find_additional_packages <- function(paths) {
  unique(unlist(lapply(paths, find_additional_packages_path)))
}


find_additional_packages_path <- function(path) {
  pkgs <- character()

  loads_package <- c("library", "require", "loadNamespace", "requireNamespace")

  ## TODO: could cope with source/sys.source and be recursive, but be
  ## careful about missing files and directories.
  analyse <- function(expr) {
    if (is.recursive(expr)) {
      nm <- if (is.symbol(expr[[1]])) as.character(expr[[1]]) else ""
      if (nm %in% c("::", ":::")) {
        pkgs <<- c(pkgs, as.character(expr[[2]]))
      }  else if (nm %in% loads_package) {
        pkgs <<- c(pkgs, as.character(match.call(match.fun(nm), expr)$package))
      }
      for (e in as.list(expr)) {
        analyse(e)
      }
    }
  }

  analyse(parse(file = path))

  unique(pkgs)
}
