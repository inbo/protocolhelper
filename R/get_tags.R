#' Get protocol tags and optionally bump version number
#'
#' Searches all index.Rmd files for version numbers and constructs tags based
#' on the protocol-code and an incremented version number.
#' The function assumes that you have the development branch checked out and
#' that this branch is aligned with the main branch.
#'
#' @param protocol_code Protocol code for which you want to get the tags
#' @param bump_version whether or not to update the development version number
#' to the incremented version number in the YAML header of the index.Rmd file
#' of the target protocol
#'
#' @importFrom ymlthis as_yml use_index_rmd
#' @importFrom fs dir_ls
#' @importFrom purrr map map_chr
#' @importFrom assertthat assert_that has_name
#' @importFrom stringr str_extract str_detect
#' @importFrom rmarkdown yaml_front_matter
#'
#' @return A message containing both the general and specific tag
#' @export
#'
get_tags <- function(
  protocol_code,
  bump_version = FALSE
) {

  # get all non-dev version-numbers
  # list all index.Rmd files
  indexpaths <- fs::dir_ls(path = ".", recurse = TRUE, regexp = "index\\.Rmd")

  # read YAML front matter
  yamllists <- purrr::map(indexpaths, rmarkdown::yaml_front_matter)

  # extract version-numbers
  versions <- purrr::map_chr(yamllists, "version_number")

  new_version <- increment_version_number(versions = versions)


  #determine and print the next protocols tag
  #determine and print the next protocol-specific tag(s), i.e. based on the
  #occurrence of dev version(s) in the working directory of current checked out
  #branch
  path_to_protocol <- get_path_to_protocol(
    protocol_code = protocol_code)

  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))

  assert_that(has_name(yml, "version_number"))
  assert_that(str_detect(yml$version_number, "dev$"),
              msg = paste0("Did you already bump the version number?\n",
                           "The version number does not end on .dev"))

  # construct protocol tags
  specific_tag <- paste0(protocol_code, "-", new_version)
  general_tag <- paste0("protocols-", new_version)

  message("The general tag is: ", general_tag,
          "\nThe specific tag is: ", specific_tag)

  #(optionally:) bump version number of the involved protocol accordingly,
  #and return message on this
  if (bump_version) {
    old_version <- yml$version_number
    yml$version_number <- new_version

    # overwrite old yaml sections
    index_rmd <- file.path(path_to_protocol, "index.Rmd")
    copy_rmd <-  file.path(path_to_protocol, "copy.Rmd")
    file.copy(from = index_rmd, to = copy_rmd)
    unlink(index_rmd)
    ymlthis::use_index_rmd(
      .yml = ymlthis::as_yml(yml),
      path = path_to_protocol,
      template = copy_rmd,
      include_body = TRUE,
      include_yaml = FALSE,
      quiet = TRUE,
      open_doc = FALSE)
    unlink(copy_rmd)

    message("Bumped ", old_version, " to ", new_version,
            " in index.Rmd")

  }

}
