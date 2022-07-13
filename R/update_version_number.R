#' @title Updates the version number in the YAML section of a protocol
#' index.Rmd file
#'
#' @param protocol_code The protocol_code corresponding with the name of the
#' branch that contains the new or updated protocol.
#' @param path Default is current working directory. Should correspond with
#' root directory of protocolsource repo.
#' @param commit Logical. Default TRUE. Whether or not to add and commit the
#' changes to the protocol branch
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom fs is_dir
#' @importFrom assertthat assert_that
#' @importFrom ymlthis use_index_rmd as_yml
#' @importFrom gert git_commit_all
#'
#' @return invisible NULL
#' @export
#'
update_version_number <- function(
    protocol_code,
    commit = TRUE,
    path = ".") {
  # assertions
  check_protocolcode(protocol_code)
  assert_that(is_dir(path))

  # what should be the version number?
  new_version <- get_version_number(path = path)

  # what is the version number?
  path_to_protocol <- get_path_to_protocol(protocol_code)
  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))
  old_version <- yml$version_number

  if (new_version == old_version) {
    message("The version number is up to date")
    return(invisible(NULL))
  } else {
    yml$version_number <- new_version

    # overwrite old yaml sections
    index_rmd <- file.path(path_to_protocol, "index.Rmd")
    copy_rmd <-  file.path(path_to_protocol, "copy.Rmd")
    file.copy(from = index_rmd, to = copy_rmd)
    unlink(index_rmd)
    use_index_rmd(
      .yml = as_yml(yml),
      path = path_to_protocol,
      template = copy_rmd,
      include_body = TRUE,
      include_yaml = FALSE,
      quiet = TRUE,
      open_doc = FALSE)
    unlink(copy_rmd)

    message_text <- paste0("Bumped ", old_version, " to ", new_version,
                     " in index.Rmd")
    message(message_text)

    if (commit) {
      git_commit_all(message_text, repo = path)
    }
    return(invisible(NULL))
  }
}
