#' Preparatory steps to start the update of a pre-existing version of a protocol
#'
#' The function creates a branch with the same name as the `protocol_code` and
#' checks out that branch.
#' Next, it will update the version number in the YAML of `index.Rmd` and
#' update the protocol-specific `NEWS.md` file.
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
#' @family creation
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

  # update version number in yaml frontmatter
  update_version_number(protocol_code = protocol_code,
                        commit = FALSE,
                        update_news = FALSE,
                        path = project_root)
  # start new header in NEWS
  path_to_protocol <- get_path_to_protocol(protocol_code)
  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))
  version_number <- yml$version_number
  news <- xfun::read_utf8(file.path(path_to_protocol, "NEWS.md"))
  news <- append(x = news,
                 values = c(
                   sprintf("## [%1$s](../%1$s/index.html)", version_number),
                   "",
                   "-   ...",
                   ""
                   ),
                 after = 2)
  xfun::write_utf8(news, file.path(path_to_protocol, "NEWS.md"))

  return(invisible(NULL))
}
