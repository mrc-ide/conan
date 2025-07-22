##' Compare conan installations.
##'
##' @title Compare conan installations
##'
##' @inheritParams conan_list
##'
##' @param curr The previous installation to compare against. Can be a
##'   name (see [conan_list] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th
##'   installation". The default value of 0 corresponds to the current
##'   installation.
##'
##' @param prev The previous installation to compare against. Can be a
##'   name (see [conan_list] to get names), a negative
##'   number where `-n` indicates "`n` installations ago" or a
##'   positive number where `n` indicates "the `n`th installation".
##'   The default of -1 indicates the previous installation. Must
##'   refer to an installation before `curr`. Use `NULL` or `-Inf` if
##'   you want to compare against the empty installation.
##'
##' @return An object of class `conan_compare`, which can be printed
##'   nicely.
##'
##' @export
conan_compare <- function(path_lib, curr = 0, prev = -1) {
  contents <- sort(dir(file.path(path_lib, ".conan")))
  index <- compare_select(path_lib, contents, curr, prev, rlang::current_env())

  info_curr <- readRDS(file.path(path_lib, ".conan", contents[[index$curr]]))
  curr <- list(name = contents[[index$curr]],
               index = index$curr,
               age = index$curr - length(contents),
               time = info_curr$description$time,
               method = info_curr$method,
               packages = as.data.frame(info_curr$description))

  if (is.null(index$prev)) {
    prev <- NULL
    status <- set_names(rep("added", nrow(curr$packages)),
                        curr$packages$package)
  } else {
    info_prev <- readRDS(file.path(path_lib, ".conan", contents[[index$prev]]))
    prev <- list(name = contents[[index$prev]],
                 index = index$prev,
                 age = index$prev - length(contents),
                 time = info_prev$description$time,
                 method = info_prev$method,
                 packages = as.data.frame(info_prev$description))

    nms <- sort(union(curr$packages$package, prev$packages$package))
    status <- set_names(rep("updated", length(nms)), nms)
    status[setdiff(curr$packages$package, prev$packages$package)] <- "added"
    status[setdiff(prev$packages$package, curr$packages$package)] <- "removed"
    is_unchanged <- (apply(prev$packages, 1, paste, collapse = "\r") %in%
                     apply(curr$packages, 1, paste, collapse = "\r"))
    status[prev$packages$package[is_unchanged]] <- "unchanged"
  }

  changes <- compare_changes(status, curr$packages, prev$packages)

  ret <- list(curr = curr, prev = prev, status = status, changes = changes)
  class(ret) <- "conan_compare"
  ret
}


##' @export
print.conan_compare <- function(x, ..., show_unchanged = FALSE) {
  ## Unfortunately, this theme does not always seem to activate, and I
  ## don't understand what the difference is between when it does and
  ## when it does not. However, it's only meant to emphasise things,
  ## and the text should be interpretable without any emphasis.
  cli::cli_div(
    theme = list(
      span.old = list(color = "grey", "font-style" = "italic"),
      span.new = list(color = "hotpink", "font-weight" = "bold"),
      h2 = list("margin-top" = 1, "margin-bottom" = 0),
      h1 = list("margin-top" = 0, "margin-bottom" = 0)))
  print_conan_compare_header(x$prev, x$curr)
  for (nm in c("unchanged", "added", "removed", "updated")) {
    if (length(x$changes[[nm]]) > 0) {
      n <- length(x$changes[[nm]])
      cli::cli_h2(paste("{n} {nm} {cli::qty(n)}package{?s}"))
      if (nm == "unchanged" && !show_unchanged) {
        cli::cli_alert_info(
          "To show unchanged packages, print with 'show_unchanged = TRUE'")
      } else {
        cli::cli_li(x$changes[[nm]])
      }
    }
  }
  invisible(x)
}


print_conan_compare_header <- function(prev, curr) {
  age <- function(n) {
    if (n == 0) {
      "current installation"
    } else if (n == -1) {
      "previous installation"
    } else {
      paste(-n, "installations ago")
    }
  }
  cli::cli_h1("Comparing conan installations")
  if (is.null(prev)) {
    cli::cli_li("({.old empty installation})")
  } else {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_li(paste(
      "{.strong {prev$name}} {.old {ordinal(prev$index)}; {age(prev$age)}",
      "({prettyunits::time_ago(prev$time)})}"))
  }
  cli::cli_li(paste(
    "{.strong {curr$name}} {.new {ordinal(curr$index)}; {age(curr$age)}",
    "({prettyunits::time_ago(curr$time)})}"))
}


details_changes <- function(prev, curr) {
  parts <- strsplit(c(prev, curr), "(?=[^[:alnum:]])", perl = TRUE)
  for (i in seq_len(min(lengths(parts)))) {
    if (parts[[1]][[i]] != parts[[2]][[i]]) {
      break
    }
  }
  if (i == 1) {
    sprintf("{.old %s} -> {.new %s}", prev, curr)
  } else {
    shared <- paste(parts[[1]][seq_len(i - 1)], collapse = "")
    prev <- paste(utils::tail(parts[[1]], -i + 1), collapse = "")
    curr <- paste(utils::tail(parts[[2]], -i + 1), collapse = "")
    sprintf("%s{{{.old %s} -> {.new %s}}}", shared, prev, curr)
  }
}


compare_select <- function(path_lib, contents, curr, prev, call = NULL) {
  if (length(contents) == 0) {
    cli::cli_abort(
      "No conan installations found at '{path_lib}'")
  }
  i_curr <- compare_select_index(curr, contents, "curr", call)
  if (is.null(i_curr)) {
    hint <- paste("You provided '{curr}' which refers to the state",
                  "before any installation")
    cli::cli_abort(c("'curr' must be a real installation", i = hint),
                   arg = "curr", call = call)
  }
  i_prev <- compare_select_index(prev %||% -Inf, contents, "prev", call)
  if (!is.null(i_prev) && i_curr < i_prev) {
    cli::cli_abort(
      "'curr' (installation {i_curr}) is earlier than 'prev' ({i_prev})",
      call = call)
  }
  if (!is.null(i_prev) && i_curr == i_prev) {
    cli::cli_abort("'curr' (installation {i_curr}) is the same as 'prev'",
                   call = call)
  }

  list(curr = i_curr, prev = i_prev)
}


compare_select_index <- function(x, contents, name, call) {
  assert_scalar(x, name = name, call = call)
  if (is.character(x)) {
    i <- match(x, contents)
    if (is.na(i)) {
      if (length(contents) <= 5) {
        hint <- "Valid entries are: {squote(utils::tail(contents))}"
      } else {
        hint <- "Valid entries include: {squote(utils::tail(contents, 5))}"
      }
      cli::cli_abort(
        c("Invalid entry '{x}' for '{name}'", i = hint),
        call = call, arg = name)
    }

  } else if (is.numeric(x)) {
    if (x <= 0) {
      i <- length(contents) + x
      if (i <= 0) {
        i <- NULL
      }
    } else {
      if (x > length(contents)) {
        cli::cli_abort(
          c("Invalid entry '{x}' for '{name}'",
            i = "Maximum allowed value is {length(contents)}"),
          call = call, arg = name)
      }
      i <- x
    }
  }
  i
}


compare_changes <- function(status, curr, prev) {
  status <- split(names(status), status)
  list(unchanged = compare_changes_unchanged(status$unchanged, curr),
       added = compare_changes_added(status$added, curr),
       removed = compare_changes_removed(status$removed, prev),
       updated = compare_changes_updated(status$updated, curr, prev))
}

compare_changes_unchanged <- function(nms, curr) {
  compare_changes_simple(nms, curr, "")
}


compare_changes_added <- function(nms, curr) {
  compare_changes_simple(nms, curr, "new")
}


compare_changes_removed <- function(nms, prev) {
  compare_changes_simple(nms, prev, "old")
}


compare_changes_simple <- function(nms, packages, class) {
  ret <- character(length(nms))
  for (i in seq_along(nms)) {
    p <- as.list(packages[packages$package == nms[[i]], ])
    src <- format_package_source(p$source, p$details)
    if (nzchar(class)) {
      ret[[i]] <- sprintf("{.strong %s} ({.%s %s}) %s",
                          p$package, class, p$version, src)
    } else {
      ret[[i]] <- sprintf("{.strong %s} (%s) %s",
                          p$package, p$version, src)
    }
  }
  ret
}


compare_changes_updated <- function(nms, curr, prev) {
  ret <- character(length(nms))
  for (i in seq_along(nms)) {
    p_prev <- as.list(prev[prev$package == nms[[i]], ])
    p_curr <- as.list(curr[curr$package == nms[[i]], ])
    if (p_prev$version == p_curr$version) {
      version <- p_curr$version
      ## Could warn about this here...
    } else {
      version <- sprintf("{.old %s} -> {.new %s}",
                         p_prev$version, p_curr$version)
    }
    if (p_prev$source != p_curr$source) {
      src <- sprintf("{.old %s} -> {.new %s}",
                     format_package_source(p_prev$source, p_prev$details),
                     format_package_source(p_curr$source, p_curr$details))
    } else if (identical(p_prev$details, p_curr$details)) {
      ## Could warn about this here...
      src <- format_package_source(p_curr$source, p_curr$details)
    } else {
      details <- details_changes(p_prev$details, p_curr$details)
      src <- format_package_source(p_curr$source, details)
    }
    ret[[i]] <- sprintf("{.strong %s} (%s) %s",
                        p_curr$package, version, src)
  }
  ret
}


format_package_source <- function(source, details) {
  if (is.na(details)) {
    src <- source
  } else {
    src <- sprintf("%s: %s", source, details)
  }
}
