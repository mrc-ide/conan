##' Watch a conan installation from another process. This can be used
##' to stream the status and progress back to the current R process.
##'
##' @title Watch a conan installation
##'
##' @param get_status A callback to get the status of the
##'   installation. The callback will be called with no arguments and
##'   must return values of "waiting", "running", "success", or
##'   "failure".
##'
##' @param get_log A callback to read logs of the installation
##'   (something like `function() readLines(filename, warn = FALSE)`
##'   may be sufficient)
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
##' @return The final status (`success` or `failure`)
##'
##' @export
conan_watch <- function(get_status, get_log, show_log = TRUE, poll = 1,
                        error = TRUE) {
  throttled <- throttle(poll)

  t0 <- Sys.time()
  cli::cli_h1("Running installation script")

  logs <- NULL
  status <- "waiting"
  while (status %in% c("waiting", "running")) {
    status <- throttled(get_status())
    if (show_log) {
      logs <- show_new_log(get_log(), logs)
    }
  }

  elapsed <- round(as.numeric(Sys.time() - t0, "secs"), 1)
  if (status == "success") {
    cli::cli_h1("Installation script finished successfully in {elapsed} s")
  } else {
    cli::cli_h1("Installation script failed in {elapsed} s")
    if (error) {
      cli::cli_abort("Installation failed")
    }
  }
  status
}


show_new_log <- function(curr, prev) {
  if (length(prev) == 0) {
    show <- curr
  } else {
    show <- curr[-seq_along(prev)]
  }
  if (length(show) > 0) {
    message(paste(show, collapse = "\n"))
  }
  curr
}
