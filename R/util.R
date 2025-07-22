`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


glue_whisker <- function(template, data) {
  transformer <- function(...) {
    ## This transformer prevents a NULL entry destroying the string
    glue::identity_transformer(...) %||% ""
  }
  glue::glue_data(data, template, .open = "{{", .close = "}}", .trim = FALSE,
                  .transformer = transformer)
}


conan_file <- function(path) {
  system.file(path, package = "conan", mustWork = TRUE)
}


read_lines <- function(path) {
  ## never warn about trailing newlines.  Unfortunately this will also
  ## not warn about embedded null's but that's rare and also usually
  ## not a very helpful warning for users to see.
  readLines(path, warn = FALSE)
}


read_string <- function(path) {
  paste(read_lines(path), collapse = "\n")
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vector_to_str <- function(x) {
  str <- paste(sprintf('"%s"', x), collapse = ", ")
  if (length(x) > 1) {
    str <- sprintf("c(%s)", str)
  }
  str
}


list_to_str <- function(x) {
  paste(trimws(utils::capture.output(dput(x)), "right"), collapse = "\n")
}


collapseq <- function(x, last = NULL) {
  paste(squote(x), collapse = ", ")
}


dir_create <- function(path) {
  dir.create(path, FALSE, TRUE)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


deparse_fn <- function(nm, indent = 0) {
  value <- trimws(deparse(get(nm)), "right")
  value[[1]] <- sprintf("%s <- %s", nm, value[[1]])
  paste0(strrep(" ", indent), value, collapse = "\n")
}


short_sha <- function(x) {
  sub("([[:xdigit:]]{7})[[:xdigit:]]{33}", "\\1", x)
}


## R time objects really want me poke my eyes out.  Perhaps there is a
## better way of doing this?  Who knows?
unlist_times <- function(x) {
  if (length(x) == 0L) {
    empty_time()
  } else {
    i <- vapply(x, inherits, TRUE, "POSIXlt")
    x[i] <- lapply(x[i], as.POSIXct)
    ret <- vapply(x, as.numeric, numeric(1))
    attributes(ret) <- attributes(x[[1L]])
    ret
  }
}


empty_time <- function() {
  Sys.time()[-1]
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


ordinal <- function(n) {
  if (n == 1 || n > 20 && n %% 10 == 1) {
    suffix <- "st"
  } else if (n == 2 || n > 20 && n %% 10 == 2) {
    suffix <- "nd"
  } else if (n == 3 || n > 20 && n %% 10 == 3) {
    suffix <- "rd"
  } else {
    suffix <- "th"
  }
  paste0(n, suffix)
}
