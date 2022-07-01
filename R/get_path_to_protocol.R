#' @title Function to get (or set) the full path to a protocol
#'
#' @description
#' A function that is used by other functions and should normally not be used
#' directly.
#'
#' For existing protocol codes, arguments `theme` and `project_name` are
#' always ignored.
#' The function will return the absolute path for that protocol.
#'
#' For new protocols, also either the `theme` or the `project_name` argument
#' and `short_title` are required apart from the `protocol_code`.
#' The function will construct the absolute path where the source code for that
#' new protocol will be written.
#'
#' @param protocol_code Character string giving the protocol code
#' @param theme A character string equal to one of `"generic"`,
#' `"water"`, `"air"`, `"soil"`, `"vegetation"` or `"species"`.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists and
#' the request is for a thematic protocol.
#' @param project_name Character string giving the name of the project folder.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists and
#' the request is for a project-specific protocol.
#' @param short_title A character string of less than 20 characters to use in
#' folder and filenames.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists.
#'
#' @return A character vector containing the full path to the protocol.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom assertthat assert_that is.string
#' @importFrom stringr str_subset
#'
#'
#' @examples
#' \dontrun{
#' get_path_to_protocol(protocol_code = "sfp-401-nl")
#'}
get_path_to_protocol <- function(protocol_code,
                                 theme = NULL,
                                 project_name = NULL,
                                 short_title = NULL) {
  assert_that(is.string(protocol_code))

  # first case: the path exists already
  project_root <- find_root(is_git_root)
  ld <- list.dirs(path = file.path(project_root, "src"),
                  full.names = TRUE,
                  recursive = TRUE)
  ld <- str_subset(string = ld,
                   pattern = protocol_code)
  if (!identical(ld, character(0))) {
    path_to_protocol <- ld[[1]]
    return(path_to_protocol)
  } else {
    # second case: the path does not yet exist
    if (is.null(theme) && is.null(project_name) ||
        (is.string(theme) && is.string(project_name))) {
      stop(
        paste0("Check the spelling of protocol_code - or - provide ",
               "a string value for theme or project, not both.")
      )
    }

    if (is.null(short_title)) {
      stop("Provide a short title")
    } else {
      protocol_folder_name <- paste(protocol_code, short_title, sep = "_")
    }

    if (is.string(theme)) {
      subfolder_of <- "thematic"
      protocol_leading_number <- themes_df[themes_df$theme == theme,
                                           "theme_number"]
      theme <- paste0(protocol_leading_number, "_", theme)
      path_to_protocol <- file.path(project_root,
                                    "src",
                                    subfolder_of,
                                    theme,
                                    protocol_folder_name)
      return(path_to_protocol)
    } else {
      subfolder_of <- "project"
      path_to_protocol <- file.path(project_root,
                                    "src",
                                    subfolder_of,
                                    project_name,
                                    protocol_folder_name)
      return(path_to_protocol)
    }
  }
}

