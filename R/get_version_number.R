#' @title Get version number for a protocol
#'
#' @description
#' Looks up pre-existing version numbers of protocols in the main branch and
#' calculates an incremented (next) version number for the currently checkout
#' branch containing the created/in development/updated/ready to be released
#' protocol.
#'
#' @param path Defaults to current working directory.
#' This should correspond with the root directory of the protocolsource repo.
#'
#' @importFrom checklist clean_git
#' @importFrom fs dir_ls is_dir
#' @importFrom purrr map map_chr
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom gert git_branch git_branch_checkout git_branch_list git_stash_pop
#' git_stash_save git_stash_list git_status
#' @importFrom assertthat assert_that
#'
#' @return A string containing the next (incremented) version number
#' @export
get_version_number <- function(path = ".") {
  assert_that(is_dir(path))
  if (interactive()) {
    clean_git(repo = path)
  }


  # checkout main branch
  current_branch <- git_branch(repo = path)
  branch_info <- git_branch_list(repo = path)
  main_branch <- ifelse(any(branch_info$name == "origin/main"),
                        "main", ifelse(any(branch_info$name == "origin/master"),
                                       "master", "unknown"))
  assert_that(main_branch %in% c("main", "master"),
              msg = "no branch `origin/main` or `origin/master` found.")
  status <- git_status(repo = path)
  status <- status[status$status != "new", ]
  if (nrow(status)) git_stash_save(repo = path)
  git_branch_checkout(branch = main_branch)

  # list all index.Rmd files
  indexpaths <- dir_ls(path = path, recurse = TRUE, regexp = "index\\.Rmd")

  # read YAML front matter
  yamllists <- map(indexpaths, yaml_front_matter)

  # extract version-numbers
  versions <- map_chr(yamllists, "version_number")

  new_version <- increment_version_number(versions = versions)

  # switch back to current branch
  git_branch_checkout(current_branch)
  if (nrow(status) && nrow(git_stash_list(repo = path))) {
    git_stash_pop(repo = path)
  }

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
