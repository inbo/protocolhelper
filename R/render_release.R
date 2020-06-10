#' @title Function to render the protocols that are ready for a new release.
#'
#' @description This function renders the protocols which are ready for a new
#' release into a folder `publish/version_number/language`, where
#' `version_number` is of the form `YYYY.NN` (year, number giving order of
#'  release in a year) and `language` can be `nl` or `en` (both folders will
#'  be created in case of multi-language protocols). In addition, earlier
#'  released and older versions of protocols remain available in the `publish`
#'  folder (see also \code{details}).
#'
#'
#' @param output_root A character string giving the root folder without version
#' number. Default is "publish".
#' @param version_number A character string of the form `YYYY.NN` (year, number
#' giving order of release in a year).
#'
#' @details The links to earlier releases and older versions can be found
#' through a subfolder structure below `publish/version_number` that mirrors the
#' structure of `src/thematic` and `src/project`.
#'
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom assertthat assert_that is.string
#' @importFrom fs dir_ls path path_rel path_ext_set
#' @importFrom stringr str_detect
#' @importFrom bookdown render_book
#' @importFrom purrr pwalk map_chr map
#' @importFrom yaml read_yaml
#' @importFrom rmarkdown yaml_front_matter render
#'
#' @keywords internal
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' protocolhelper:::render_release(version_number = "2000.01")
#'}

render_release <- function(output_root = "publish",
                           version_number) {


  assert_that(is.string(output_root))
  assert_that(str_detect(version_number, "^\\d{4}\\.\\d{2}$"),
              msg = "Please provide a valid version number")

  git_root <- find_root(is_git_root)
  output_new_root <- path(git_root, output_root, version_number)

  # Search thematic and project-specific protocols
  # for those which have version_number == version_number
  # normally one protocol, except if multiple language-versions
  # thematic protocols
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

  # search for version number
  all_protocols_index <- c(thematic_protocols_index, project_protocols_index)
  all_protocols <- c(thematic_protocols, project_protocols)
  all_protocols_rel <- c(thematic_protocols_rel, project_protocols_rel)
  version_numbers <- map_chr(all_protocols_index, function(x) {
    yml <- yaml_front_matter(x)
    vn <- as.character(yml$params$version_number)
    vn
    }
    )

  to_publish <- all_protocols[version_numbers == version_number]
  to_publish_index <- all_protocols_index[version_numbers == version_number]
  to_publish_rel <- all_protocols_rel[version_numbers == version_number]

  # check front matter
  yml <- map(to_publish, check_frontmatter)

  # render the protocol(s) in the publish/version_number folder
  pwalk(.l = list(x = to_publish_index,
                  y = to_publish,
                  z = yml),
        .f = function(x, y, z) {
          old_wd <- getwd()
          setwd(dir = y)
          render_book(input = x,
                      output_dir =  path(output_new_root, z$params$language))
          # rename html to index.html
          yml <- read_yaml("_bookdown.yml")
          original_name <- yml$book_filename
          file.rename(from = path_ext_set(path(output_new_root,
                                               z$params$language, original_name),
                                          ext = "html"),
                      to = path(output_new_root,
                                z$params$language, "index.html"))
          setwd(old_wd)
        })

  # render the protocol-specific NEWS.Rmd
  pwalk(.l = list(x = to_publish,
                  y = to_publish_rel),
        .f = function(x, y) {
          old_wd <- getwd()
          setwd(dir = x)
          render(input = path(x, "NEWS.Rmd"),
                 output_format = "html_document",
                 output_file = "index",
                 output_dir = path(git_root, output_root, y))
          setwd(old_wd)
        })

  # also render the repo NEWS.md?? into a file publish/index.html
  # which then becomes the homepage??

}
