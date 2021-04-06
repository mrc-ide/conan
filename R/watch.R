##' Watch a conan installation from another process. This can be used
##' to stream the status and progress back to the current R process.
##'
##' @title Watch a conan installation
##'
##' @param get_status A callback to get the status of the
##'   installation. The callback will be called with no arguments and
##'   must return `COMPLETE` on successful completion, `ERROR` on
##'   failure and any other value to continue.
##'
##' @param get_log A callback to read logs of the installation
##'   (something like `function() readLines(filename, warn = FALSE)`
##'   may be sufficient)
##'
##' @param show_progress Logical, indicating if we should show a
##'   progress bar. This requires the progress package.
##'
##' @param show_log Logical, indicating if the installation log should
##'   be printed
##'
##' @param poll Time, in seconds, used to throttle calls to the status
##'   function. The default is 1 second
##'
##' @param error Logical, indicating if we should throw an error if
##'   installation fails.
##'
##' @return The final status (`COMPLETE` or `ERROR`)
##'
##' @author Richard Fitzjohn
conan_watch <- function(get_status, get_log, show_progress = TRUE,
                        show_log = TRUE, poll = 1, error = TRUE) {
  p <- conan_watch_progress(show_progress)
  throttled <- throttle(poll)

  log_prev <- NULL
  repeat {
    status <- throttled(get_status())

    if (show_log) {
      log <- get_log()
      if (length(log) > length(log_prev)) {
        clear_progress_bar(p)
        message(paste(new_log(log, log_prev), collapse = "\n"))
        log_prev <- log
      }
    }

    if (status %in% c("COMPLETE", "ERROR")) {
      p$terminate()
      break
    } else {
      p$tick(tokens = list(status = status))
    }
  }

  if (error && status == "ERROR") {
    stop("Installation failed")
  }

  status
}


conan_watch_progress <- function(show, force = FALSE) {
  fmt <- "[:spin] :elapsed :status"
  if (show && requireNamespace("progress", quietly = TRUE)) {
    p <- progress::progress_bar$new(fmt, NA, show_after = 0, force = TRUE)
    p$tick(0, list(status = "..."))
  } else {
    list(tick = function(...) NULL,
         terminate = function(...) NULL)
  }
}
