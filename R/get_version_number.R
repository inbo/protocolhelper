#' @title Get version number for a protocol
#'
#' @description
#' Looks up pre-existing version numbers of protocols in the current branch.
#' The current branch should be aligned with the main branch.
#'
#' @param path Defaults to current working directory.
#' This should correspond with the root directory of the protocolsource repo.
#'
#' @importFrom checklist clean_git
#' @importFrom fs dir_ls
#' @importFrom purrr map map_chr
#' @importFrom rmarkdown yaml_front_matter
#'
#' @return A string containing the next (incremented) version number
#' @export
get_version_number <- function(path = ".") {
  clean_git(repo = path)
  # list all index.Rmd files
  indexpaths <- dir_ls(path = path, recurse = TRUE, regexp = "index\\.Rmd")

  # read YAML front matter
  yamllists <- map(indexpaths, yaml_front_matter)

  # extract version-numbers
  versions <- map_chr(yamllists, "version_number")

  new_version <- increment_version_number(versions = versions)
  return(new_version)
}


#' Increment version number
#'
#' Given a set of published protocol version numbers, calculate the next version
#' number
#'
#' @param versions a character vector with previously published version numbers
#'
#' @return A string containing the next (incremented) version number
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_detect
#' @importFrom purrr map
increment_version_number <- function(versions) {

  assert_that(is.character(versions))
  map(versions, check_versionnumber)

  if (length(versions) >= 1) {
    versions <- sort(versions)

    # last version
    last <- versions[length(versions)]

    # increase version number
    lastyear <- str_extract(last, "\\d{4}")
    lastincrement <- str_extract(last, "\\d{2}$")
    currentyear <- format(Sys.Date(), "%Y")
    increment <- formatC(as.numeric(lastincrement) + 1,
                         width = 2, flag = 0)
    new_version <- ifelse(
      lastyear == currentyear,
      paste0(lastyear, ".", increment),
      paste0(currentyear, ".01"))
    return(new_version)
  } else {
    new_version <- paste0(format(Sys.Date(), "%Y"), ".01")
    return(new_version)
  }
}
