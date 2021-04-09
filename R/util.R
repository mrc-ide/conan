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


clean_repos <- function(repos, cran = NULL) {
  if (is.null(repos)) {
    return(c(CRAN = default_cran(cran)))
  }
  if (!("CRAN" %in% names(repos)) || repos[["CRAN"]] == "@CRAN@") {
    repos[["CRAN"]] <- default_cran(cran)
  }
  repos
}


default_cran <- function(cran = NULL) {
  cran %||% "https://cloud.r-project.org"
}


ref_to_package_name <- function(ref) {
  if (ref$type == "local") {
    sub("(_.*)?\\.(tar.gz|tgz|zip)", "", basename(ref$path))
  } else {
    ref$package
  }
}


write_script_exec <- function(code, path) {
  writeLines(code, path)
  Sys.chmod(path, "755")
  invisible(path)
}


deparse_str <- function(x) {
  paste(deparse(x), collapse = "\n")
}


throttle <- function(interval) {
  last <- Sys.time() - interval
  function(expr) {
    wait <- interval - (Sys.time() - last)
    if (wait > 0) {
      Sys.sleep(wait)
    }
    last <<- Sys.time()
    force(expr)
  }
}


new_log <- function(curr, prev) {
  if (length(prev) == 0) {
    curr
  } else {
    curr[-seq_along(prev)]
  }
}


clear_progress_bar <- function(p) {
  private <- environment(p$tick)$private
  if (is.null(private)) {
    return()
  }
  if (nchar(private$last_draw) > 0) {
    str <- paste0(c("\r", rep(" ", private$width)), collapse = "")
    message(str, appendLF = FALSE)
  }
  message("\r", appendLF = FALSE)
}
