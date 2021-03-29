`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


find_functions <- function(fun, env) {
  ours <- names(env)
  ours <- ours[vapply(ours, function(x) is.function(env[[x]]), TRUE)]
  seen <- fun
  test <- lapply(fun, function(x) env[[x]])
  while (length(test) > 0L) {
    new <- setdiff(intersect(all.vars(body(test[[1L]]), TRUE), ours), seen)
    seen <- c(seen, new)
    test <- c(test[-1L], lapply(new, get, env, inherits = FALSE))
  }
  sort(unique(seen))
}


deparse_fn <- function(nm, ...) {
  value <- trimws(deparse(get(nm, ...)), "right")
  if (grepl("%", nm)) {
    nm <- sprintf("`%s`", nm)
  }
  value[[1]] <- sprintf("%s <- %s", nm, value[[1]])
  value
}


extract_code <- function(nm, env = NULL) {
  if (is.null(env)) {
    env <- environment(extract_code)
  }
  fns <- find_functions(nm, env)
  unlist(lapply(fns, deparse_fn, envir = env))
}


clean_repos <- function(repos) {
  if (is.null(repos)) {
    return(c(CRAN = cran_rcloud))
  }
  if (!("CRAN" %in% names(repos)) || repos[["CRAN"]] == "@CRAN@") {
    repos[["CRAN"]] <- cran_rcloud
  }
  repos
}


list_to_character <- function(x) {
  vapply(x, identity, "", USE.NAMES = FALSE)
}


write_script_exec <- function(code, path) {
  writeLines(code, path)
  Sys.chmod(path, "755")
  invisible(path)
}


deparse_str <- function(x) {
  paste(deparse(x), collapse = "\n")
}



cran_rcloud <- "https://cloud.r-project.org"
