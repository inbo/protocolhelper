#' @title Function to render all protocols into a folder `publish/version_number`.
#'
#' @description This function renders all protocols (which have been merged to
#' the master branch) into a folder `publish/version_number`, where
#' `version_number` is a tag of the form `YYYY.NN` (year, number giving order of
#'  release in a year).
#'
#'
#' @param output_root A character string giving the root folder without version
#' number. Default is "publish".
#' @param version_number A character string of the form `YYYY.NN` (year, number
#' giving order of release in a year).
#'
#' @return The rendered html files and associated files needed by the html file
#' for all protocols will be written to subfolders of `publish/version_number`.
#' The subfolder structure mirrors the structure of `src/thematic` and
#' `src/project`.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom assertthat assert_that is.string
#' @importFrom fs dir_ls path path_rel
#' @importFrom stringr str_detect
#' @importFrom bookdown render_book
#' @importFrom purrr pwalk
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' render_all(version_number = "2000.01")
#'}


render_all <- function(output_root = "publish",
                       version_number) {

  assert_that(is.string(output_root))
  assert_that(str_detect(version_number, "^\\d{4}\\.\\d{2}$"),
              msg = "Please provide a valid version number")

  git_root <- find_root(is_git_root)
  output_new_root <- path(git_root, output_root, version_number)

  # Thematic protocols
  thematic_protocols <- dir_ls(path = path(git_root,
                                           "src",
                                           "thematic"),
                               recurse = 1,
                               type = "directory")
  theme_dirs <- dir_ls(path = file.path(git_root,
                                        "src",
                                        "thematic"),
                       recurse = 0,
                       type = "directory")
  thematic_protocols <- thematic_protocols[!thematic_protocols %in% theme_dirs]
  thematic_protocols_index <- path(thematic_protocols, "index", ext = "Rmd")
  thematic_protocols_rel <- path_rel(thematic_protocols, path(git_root,
                                                              "src",
                                                              "thematic"))
  pwalk(.l = list(x = thematic_protocols_index,
                  y = thematic_protocols_rel,
                  z = thematic_protocols),
        .f = function(x, y, z) {
         render_book(input = x,
                     output_dir =  path(output_new_root, y),
                     knit_root_dir = z)
         })

  # Project-specific protocols
  project_protocols <- dir_ls(path = path(git_root,
                                           "src",
                                           "project"),
                               recurse = 1,
                               type = "directory")
  project_dirs <- dir_ls(path = file.path(git_root,
                                        "src",
                                        "project"),
                       recurse = 0,
                       type = "directory")
  project_protocols <- project_protocols[!project_protocols %in% project_dirs]
  project_protocols_index <- path(project_protocols, "index", ext = "Rmd")
  project_protocols_rel <- path_rel(project_protocols, path(git_root,
                                                              "src",
                                                              "project"))
  pwalk(.l = list(x = project_protocols_index,
                  y = project_protocols_rel,
                  z = project_protocols),
        .f = function(x, y, z) {
          old_wd <- getwd()
          setwd(dir = z)
          render_book(input = x,
                      output_dir =  path(output_new_root, y))
          setwd(old_wd)
        })

}
