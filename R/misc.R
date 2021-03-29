#' Pass command lines to a shell
#'
#' Cross-platform function to pass a command to the shell, using either
#' \code{\link[base]{system}} or
#' (Windows-only) \code{\link[base]{shell}}, depending on the operating system.
#'
#' @param commandstring
#' The system command to be invoked, as a string.
#' Multiple commands can be combined in this single string, e.g. with a
#' multiline string.
#' @param ... Other arguments passed to \code{\link[base]{system}} or
#' \code{\link[base]{shell}}.
#'
#' @inheritParams base::system
#'
#' @keywords internal
#'
execshell <- function(commandstring, intern = FALSE, ...) {
  if (.Platform$OS.type == "windows") {
    res <- shell(commandstring, intern = TRUE, ...)# nolint
  } else {
    res <- system(commandstring, intern = TRUE, ...)
  }
  if (!intern) {
    if (length(res) > 0) cat(res, sep = "\n") else return(invisible())
    } else return(res)
}
