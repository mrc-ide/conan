assert_scalar <- function(x, name = deparse(substitute(x)), arg = name,
                          call = NULL) {
  if (length(x) != 1) {
    cli::cli_abort(c("'{name}' must be a scalar",
                     i = "{name} has length {length(x)}"),
                   call = call, arg = arg)
  }
  invisible(x)
}


assert_character <- function(x, name = deparse(substitute(x)),
                             arg = name, call = NULL) {
  if (!is.character(x)) {
    cli::cli_abort("'{name}' must be character", call = call, arg = arg)
  }
  invisible(x)
}


assert_logical <- function(x, name = deparse(substitute(x)), arg = name,
                           call = NULL) {
  if (!is.logical(x)) {
    cli::cli_abort("'{name}' must be logical", call = call, arg = arg)
  }
  invisible(x)
}


assert_numeric <- function(x, name = deparse(substitute(x)), arg = name,
                           call = NULL) {
  if (!is.numeric(x)) {
    cli::cli_abort("'{name}' must be numeric", call = call, arg = arg)
  }
  invisible(x)
}


assert_scalar_character <- function(x, name = deparse(substitute(x)),
                                    arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_character(x, name, arg = arg, call = call)
}


assert_scalar_numeric <- function(x, name = deparse(substitute(x)),
                                    arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_numeric(x, name, arg = arg, call = call)
}


assert_scalar_logical <- function(x, name = deparse(substitute(x)),
                                  arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_logical(x, name, arg = arg, call = call)
}


assert_is <- function(x, what, name = deparse(substitute(x)),
                      arg = name, call = NULL) {
  if (!inherits(x, what)) {
    cli::cli_abort(
      c("'{name}' must be a {paste(what, collapse = ' / ')}",
        "{name} was a {paste(class(x), collapse = ' / ')}"),
      call = call, arg = arg)
  }
}
