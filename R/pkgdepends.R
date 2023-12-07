pkgdepends_parse <- function(refs) {
  refs <- trimws(refs[!grepl("^\\s*(#|\\s*$)", refs)])
  re_repo <- "^repo::"
  is_repo <- grepl(re_repo, refs)
  if (any(is_repo)) {
    repos <- sub(re_repo, "", refs[is_repo])
    refs <- refs[!is_repo]
  } else {
    repos <- NULL
    refs <- refs
  }

  parsed <- lapply(refs, function(x) {
    tryCatch(pkgdepends::parse_pkg_ref(x), error = identity)
  })
  is_error <- vlapply(parsed, inherits, "error")
  if (any(is_error)) {
    msg <- vcapply(parsed[is_error], "[[", "message")
    details <- sprintf("%s: %s", refs[is_error], msg)
    names(details) <- rep("x", length(details))
    cli::cli_abort(c("Failed to parse some package references", details))
  }

  list(repos = repos, refs = refs)
}
