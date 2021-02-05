#' @title Function to render a protocol to html.
#'
#' @description This function is a simple wrapper around
#' `bookdown::render_book()` and can be used to render a protocol to html
#' in order to preview updates that have been made.
#'
#'
#' @inheritParams get_path_to_protocol
#' @inheritParams bookdown::render_book
#'
#' @details The rendered html file and associated files needed by the html file
#' will be put in the directory implied by the output_dir parameter.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom assertthat assert_that is.string
#' @importFrom yaml read_yaml
#' @importFrom fs path path_ext_set
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' render_protocol(protocol_code = "sfp_401-nl")
#'}
render_protocol <- function(protocol_code = NULL,
                            output_dir = NULL) {
  assert_that(is.string(protocol_code))

  path_to_protocol <- get_path_to_protocol(
      protocol_code = protocol_code
    )

  # render html
  old_wd <- getwd()
  setwd(dir = path_to_protocol)
  render_book(input = "index.Rmd",
              output_dir = output_dir)
  # rename html to index.html
  yml <- read_yaml("_bookdown.yml")
  if (is.null(output_dir)) {
    output_dir <- yml$output_dir
  }
  original_name <- yml$book_filename
  file.rename(from = path_ext_set(path(output_dir, original_name),
                                  ext = "html"),
              to = path(output_dir, "index.html"))
  setwd(old_wd)

}
