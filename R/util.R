`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


find_functions <- function(fun, env) {
  ours <- names(env)
  ours <- ours[vapply(ours, function(x) is.function(env[[x]]), TRUE)]
  seen <- fun
  test <- list(env[[fun]])
  while (length(test) > 0L) {
    new <- setdiff(intersect(all.vars(body(test[[1L]]), TRUE), ours), seen)
    seen <- c(seen, new)
    test <- c(test[-1L], lapply(new, get, env, inherits = FALSE))
  }
  sort(seen)
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


cran_rcloud <- "https://cloud.r-project.org"
