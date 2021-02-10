#' Pass command lines to a shell
#'
#' Cross-platform function to pass a command to the shell, using either
#' \code{\link[base]{system}} or
#' (Windows-only) \code{\link[base]{shell}}, depending on the operating system.
#'
#' @param commandstring String.
#' One or multiple commandlines as one would enter in the shell.
#' @inheritParams base::system
#'
#' @keywords internal
#'
execshell <- function(commandstring, intern = FALSE) {
  if (.Platform$OS.type == "windows") {
    res <- shell(commandstring, intern = TRUE)
  } else {
    res <- system(commandstring, intern = TRUE)
  }
  if (!intern) cat(res, sep = "\n") else return(res)
}
