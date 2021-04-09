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


img_boots <- function() {
  ## https://www.asciiart.eu/clothing-and-accessories/footwear
  c("     ._......",
    "     |X/.*| |",
    "     |X/+ | |",
    "     |X/* | |",
    "____/     ; ;",
    "\\_____/|_/_/")
}


img_axe <- function() {
  ## https://www.asciiart.eu/weapons/axes
  c("  ,:\\      /:.",
    " //  \\_()_/  \\\\",
    "||   |    |   ||",
    "||   |    |   ||",
    "||   |____|   ||",
    " \\\\  / || \\  //",
    "  `:/  ||  \\;'",
    "       ||",
    "       ||",
    "       XX",
    "       XX",
    "       XX",
    "       XX",
    "       OO",
    "       `'")
}


pad_right <- function(x) {
  n <- nchar(x)
  paste0(x, strrep(" ", max(n) - n))
}


prefix_message_build <- function(img, skip, title, data) {
  txt <- vector("list", length(data))
  lhs <- pad_right(paste0(names(data), ":"))
  i <- vapply(data, is.list, TRUE)
  if (any(i)) {
    txt[i] <- Map(c,
                  trimws(lhs[i]),
                  lapply(unname(data[i]), function(x) paste("  *", x)))
  }
  txt[!i] <- paste(lhs[!i], vapply(data[!i], identity, "", USE.NAMES = FALSE))
  txt <- unlist(c(list(title), txt))

  npad_vertical <- 1 + max(0, length(txt) + skip - length(img))
  ret <- pad_right(c(img, rep("", npad_vertical)))
  i <- seq_along(txt) + skip
  ret[i] <- paste(ret[i], txt, sep = "  ")
  ret <- trimws(ret, "right")

  ret
}


prefix_message <- function(img, skip, title, data) {
  for (line in prefix_message_build(img, skip, title, data)) {
    message(line)
  }
}
