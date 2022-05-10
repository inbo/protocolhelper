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
#' @param path The path from where the commandstring needs to be executed
#' @param ... Other arguments passed to \code{\link[base]{system}} or
#' \code{\link[base]{shell}}.
#'
#' @inheritParams base::system
#'
#' @keywords internal
#'
execshell <- function(commandstring, intern = FALSE, path = ".", ...) {
  old_wd <- setwd(path)
  on.exit(setwd(old_wd), add = TRUE)

  if (.Platform$OS.type == "windows") {
    res <- shell(commandstring, intern = TRUE, ...)# nolint
  } else {
    res <- system(commandstring, intern = TRUE, ...)
  }
  if (!intern) {
    if (length(res) > 0) {
      cat(res, sep = "\n")
    } else {
      return(invisible())
    }
  } else {
    return(res)
  }
}



#' set path to html template to be used by gitbook
#' @export
protocol_css <- function() {
  source_dir <- system.file(
    "css",
    package = "protocolhelper"
  )
  if (!interactive()) {
    file.copy(source_dir, getwd(), recursive = TRUE, overwrite = TRUE)
  }
  return(file.path("css", "gitbook.html"))
}
