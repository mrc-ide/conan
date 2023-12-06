`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


glue_whisker <- function(template, data) {
  transformer <- function(...) {
    ## This transformer prevents a NULL entry destroying the string
    glue::identity_transformer(...) %||% ""
  }
  glue::glue(template, .envir = data, .open = "{{", .close = "}}",
             .trim = FALSE, .transformer = transformer)
}


conan_file <- function(path) {
  system.file(path, package = "conan", mustWork = TRUE)
}


read_string <- function(path) {
  paste(readLines(path), collapse = "\n")
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


squote <- function(x) {
  sprintf("'%s'", x)
}


collapseq <- function(x, last = NULL) {
  paste(squote(x), collapse = ", ")
}
