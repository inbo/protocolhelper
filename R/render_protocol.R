#' @title Function to render a protocol to html and pdf.
#'
#' @description This function is a simple wrapper around
#' `bookdown::render_book()` and can be used to render a protocol to html and
#' pdf in order to preview updates that have been made.
#'
#'
#' @inheritParams get_path_to_protocol
#' @inheritParams bookdown::render_book
#' @param ... additional parameters passed on to `bookdown::render_book()`
#'
#' @details The rendered html and pdf file and associated files needed by the
#' html file will be put in the directory implied by the `output_dir` parameter.
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom fs path dir_exists dir_copy
#' @importFrom withr defer local_dir
#'
#' @export
#' @family render
#'
#'
#' @examples
#' \dontrun{
#' render_protocol(protocol_code = "sfp_401-nl")
#' }
render_protocol <- function(protocol_code = NULL,
                            output_dir = NULL,
                            ...) {
  assert_that(is.string(protocol_code))

  path_to_protocol <- get_path_to_protocol(
    protocol_code = protocol_code
  )

  # copy css
  if (!dir_exists(file.path(path_to_protocol, "css"))) {
    dir_copy(
      system.file("css", package = "protocolhelper"),
      file.path(path_to_protocol, "css")
    )
  }
  withr::defer(
    unlink(
      file.path(path_to_protocol, "css"),
      recursive = TRUE
    )
  )
  # copy pandoc
  if (!dir_exists(file.path(path_to_protocol, "pandoc"))) {
    dir_copy(
      system.file("pandoc", package = "protocolhelper"),
      file.path(path_to_protocol, "pandoc")
    )
  }
  withr::defer(
    unlink(file.path(path_to_protocol, "pandoc"),
      recursive = TRUE
    )
  )
  withr::local_dir(path_to_protocol)
  suppressWarnings(
    # render pdf
    render_book(
      input = "index.Rmd",
      output_dir = output_dir,
      output_format = "bookdown::pdf_book",
      envir = new.env(),
      ...
    )
  )
  suppressWarnings(
    # render html
    render_book(
      input = "index.Rmd",
      output_dir = output_dir,
      output_file = "index.html",
      envir = new.env(),
      ...
    )
  )
}
