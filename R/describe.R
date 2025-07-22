##' Describe a library.  This creates a summary of version information
##' from a library.  This may be slightly slow on network filesystems
##' with large libraries.
##'
##' @title Describe a library
##'
##' @param path_lib Path to the library
##'
##' @return A list with class `conan_describe`. We'll write some
##'   tooling to work with these soon!
##'
##' @export
conan_describe <- function(path_lib) {
  ## This set is determined fairly empirically, but should be enough
  ## to work out where a package comes from fairly comprehensively
  ## while saving the least information.
  pkg_description <- function(name) {
    fields <- c("Package", "Version", "Priority", "Repository", "biocViews",
                "Date", "Packaged", "Built",
                "RemoteType", "RemoteUsername", "RemoteRepo", "RemoteRef",
                "RemoteSubdir", "RemoteUrl", "RemoteSha")
    d <- utils::packageDescription(name, path_lib, fields)
    d[!vapply(d, is.na, TRUE)]
  }
  nms <- .packages(TRUE, path_lib)
  pkgs <- lapply(nms, pkg_description)
  names(pkgs) <- nms
  ret <- list(packages = pkgs, time = Sys.time())
  class(ret) <- "conan_describe"
  ret
}


##' @export
as.data.frame.conan_describe <- function(x, ...) {
  pkgs <- x$packages[order(names(x$packages))]
  time <- unlist_times(lapply(pkgs, package_time))
  src <- vapply(pkgs, package_source, character(2))
  version <- numeric_version(vcapply(pkgs, "[[", "Version"))
  ret <- data.frame(package = names(pkgs),
                    version = version,
                    time = time,
                    source = src[1, ],
                    details = src[2, ],
                    stringsAsFactors = FALSE)
  rownames(ret) <- NULL
  ret
}


package_time <- function(pkg) {
  parse_time <- function(s) {
    tz <- if (grepl(" UTC$", s)) "UTC" else ""
    as.POSIXct(strptime(sub(";.*", "", s), "%Y-%m-%d %H:%M:%S", tz = tz))
  }
  if (!is.null(pkg[["Packaged"]])) {
    time <- parse_time(sub(";.*", "", pkg[["Packaged"]]))
    if (!is.na(time)) {
      return(time)
    }
  }
  if (!is.null(pkg[["Built"]])) {
    time <- parse_time(strsplit(pkg[["Built"]], "; ", fixed = TRUE)[[1]][[3]])
    if (!is.na(time)) {
      return(time)
    }
  }
  cli::cli_warn("Could not determine time for package '{pkg$Package}'")
  as.POSIXct(NA, "UTC")
}


package_source <- function(pkg) {
  if (!is.null(pkg$Priority) && pkg$Priority == "base") {
    c("base", NA_character_)
  } else if (!is.null(pkg$Repository)) {
    re_universe <- "^https://(.+)\\.r-universe\\.dev$"
    repo <- pkg$Repository
    if (repo == "CRAN") {
      c(repo, NA_character_)
    } else if (grepl(re_universe, repo)) {
      c("Universe", sub(re_universe, "\\1", repo))
    } else {
      c("Custom", repo)
    }
  } else if (!is.null(pkg$biocViews) && pkg$biocViews != "") {
    c("Bioconductor", NA_character_)
  } else if (!is.null(pkg$RemoteType)) {
    remote_type <- pkg$RemoteType
    sha <- pkg$RemoteSha

    if (is.null(pkg$RemotePkgRef)) {
      user <- pkg$RemoteUsername
      repo <- pkg$RemoteRepo
      subdir <- pkg$RemoteSubdir
      ref <- pkg$RemoteRef
      if (!is.null(user) && !is.null(repo)) {
        details <- sprintf("%s/%s", user, repo)
      } else if (!is.null(pkg$RemoteUrl)) {
        details <- pkg$RemoteUrl
      } else {
        cli::cli_warn("Could not determine source details for '{pkg$Package}'")
        details <- "(unknown)"
      }
      if (!is.null(subdir)) {
        details <- sprintf("%s/%s", details, subdir)
      }
      if (!is.null(ref) && !(ref %in% c("main", "master", "HEAD"))) {
        details <- sprintf("%s@%s", details, ref)
      }
    } else {
      ## Installation with pak/pkgdepends results in RemotePkgRef, so
      ## most of the time we'll likely see this, which is much nicer:
      details <- sub(".+?::", "", pkg$RemotePkgRef)
    }
    if (!is.null(sha)) {
      details <- sprintf("%s (%s)", details, short_sha(sha))
    }
    c(remote_type, details)
  } else {
    c("local", NA_character_)
  }
}
