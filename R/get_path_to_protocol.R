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
#' For new `sfp` or `spp` protocols, also either the `theme` or the
#' `project_name` argument and `short_title` are required apart from the
#' `protocol_code`.
#' The function will construct the absolute path where the source code for that
#' new protocol will be written.
#'
#' @param protocol_code Character string giving the protocol code
#' @param theme A character string equal to one of `"generic"`,
#' `"water"`, `"air"`, `"soil"`, `"vegetation"` or `"species"`.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists and
#' the request is for a `sfp` protocol.
#' @param project_name Character string giving the name of the project folder.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists and
#' the request is for a `spp` protocol.
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
#' @export
#' @examples
#' \dontrun{
#' get_path_to_protocol(protocol_code = "sfp-401-nl")
#'}
get_path_to_protocol <- function(protocol_code,
                                 theme = NULL,
                                 project_name = NULL,
                                 short_title = NULL) {
  assert_that(is.string(protocol_code))
  protocol_type <- regmatches(protocol_code,
                              regexpr("^s[f|p|i|o|a]p", protocol_code))
  assert_that(protocol_type %in% c("sfp", "spp", "sip", "sap", "sop"))

  # first case: the path exists already
  project_root <- find_root(is_git_root)
  ld <- list.dirs(path = file.path(project_root, "source"),
                  full.names = TRUE,
                  recursive = TRUE)
  ld <- str_subset(string = ld,
                   pattern = str_replace_all(protocol_code, "-", "_"))
  if (!identical(ld, character(0))) {
    path_to_protocol <- ld[[1]]
    return(path_to_protocol)
  }

  # second case: the path does not yet exist
  if (is.null(short_title)) {
    stop("Provide a short title")
  } else {
    protocol_folder_name <- paste(
      str_replace_all(protocol_code, "-", "_"), short_title, sep = "_")
  }

  if (protocol_type == "sfp") {
    assert_that(is.string(theme),
                theme %in% themes_df$theme)
    protocol_leading_number <- themes_df[themes_df$theme == theme,
                                         "theme_number"]
    theme <- paste0(protocol_leading_number, "_", theme)
  }

  if (protocol_type == "spp") {
    assert_that(is.string(project_name))
  }

  path_to_protocol <- switch(
    protocol_type,
    "sfp" = file.path(project_root,
                      "source",
                      "sfp",
                      theme,
                      protocol_folder_name),
    "spp" = file.path(project_root,
                      "source",
                      "spp",
                      project_name,
                      protocol_folder_name),
    "sap" = file.path(project_root,
                      "source",
                      "sap",
                      protocol_folder_name),
    "sip" = file.path(project_root,
                      "source",
                      "sip",
                      protocol_folder_name),
    "sop" = file.path(project_root,
                      "source",
                      "sop",
                      protocol_folder_name)
  )
  return(path_to_protocol)
}
