#' @title Function to render a protocol to html.
#'
#' @description This function is a simple wrapper around bookdown::render_book() and can be used to render a protocol to html in order to preview updates that have been made.
#'
#'
#' @inheritParams get_path_to_protocol
#' @inheritParams bookdown::render_book
#'
#' @return The rendered html file and associated files needed by the html file.
#' They will be put in the directory implied by the output_dir parameter.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom assertthat assert_that is.string
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' render_protocol(theme = "vegetation",
#' protocol_folder_name = "sfp_401-vegopn-terrestr_nl")
#'}
render_protocol <- function(protocol_folder_name = NULL,
                            output_dir = NULL) {
  assert_that(is.string(protocol_folder_name))

  path_to_protocol <- get_path_to_protocol(
      protocol_folder_name = protocol_folder_name
    )

  # render html
  old_wd <- getwd()
  setwd(path_to_protocol)
  render_book(input = "index.Rmd",
              output_dir = output_dir)
  setwd(old_wd)
}
