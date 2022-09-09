#' Preparatory steps to start the update of a pre-existing version of a protocol
#'
#' The function creates a branch named after the protocol_code
#'
#' @inheritParams get_path_to_protocol
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_subset str_replace_all
#' @importFrom assertthat assert_that has_name
#' @importFrom ymlthis as_yml use_index_rmd
#' @importFrom checklist new_branch
#'
#' @return NULL invisibly
#' @export
#'
update_protocol <- function(protocol_code) {
  # check if protocol_code exists
  check_protocolcode(protocol_code)
  project_root <- find_root(is_git_root)
  ld <- list.dirs(path = file.path(project_root, "source"),
                  full.names = TRUE,
                  recursive = TRUE)
  ld <- str_subset(string = ld,
                   pattern = str_replace_all(protocol_code, "-", "_"))
  assert_that(!identical(ld, character(0)),
              msg = "The protocol code does not exist.")

  new_branch(branch = protocol_code,
             repo = project_root)

  #increment version number
  path_to_protocol <- get_path_to_protocol(
    protocol_code = protocol_code)
  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))
  assert_that(has_name(yml, "version_number"))
  yml$version_number <- get_version_number()
  yml <- as_yml(yml)
  # overwrite old yaml sections
  parent_rmd <- file.path(path_to_protocol, "index.Rmd")
  template_rmd <- file.path(path_to_protocol, "template.rmd")
  file.copy(from = parent_rmd, to = template_rmd)
  unlink(parent_rmd)
  use_index_rmd(
    .yml = yml,
    path = path_to_protocol,
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE)
  unlink(template_rmd)

  return(invisible(NULL))
}
