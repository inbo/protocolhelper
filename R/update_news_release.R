#' @title Updates the protocolsource repo NEWS.md file
#'
#' @description
#' Constructs and writes a NEWS.md file based on version numbers, protocol codes
#' and protocol titles.
#' Not intended for interactive use.
#' Should be used in a GitHub Action.
#' Reads the NEWS.md from the main branch and switches back to current
#' protocol branch to write the updated NEWS.md
#'
#'
#' @param protocol_code protocol code of the protocol that is to be published in
#' this release
#' @param path Default current working directory. The root folder of the
#' protocolsource repo.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom assertthat assert_that is.string
#' @importFrom stringr str_detect
#' @importFrom gert git_branch git_branch_list git_branch_checkout
#'
#' @return invisible null
#'
#' @keywords internal
#'
update_news_release <- function(protocol_code, path = ".") {
  assert_that(is.string(path))
  check_protocolcode(protocol_code)

  # checkout main branch
  current_branch <- git_branch(repo = path)
  branch_info <- git_branch_list(repo = path)
  main_branch <- ifelse(any(branch_info$name == "origin/main"),
                        "main", ifelse(any(branch_info$name == "origin/master"),
                                       "master", "unknown"))
  assert_that(main_branch %in% c("main", "master"),
              msg = "no branch `origin/main` or `origin/master` found.")
  git_branch_checkout(branch = main_branch)

  news_file <- file.path(path, "NEWS.md")
  news_contents <- readLines(news_file)

  # switch back to current branch
  git_branch_checkout(current_branch)

  # protocol metadata
  path_protocol <- get_path_to_protocol(protocol_code)

  # read YAML front matter
  yaml <- yaml_front_matter(file.path(path_protocol, "index.Rmd"))

  # contents of NEWS.md file
  news <- c(
    paste0(
      "## Version ", yaml$version_number, "\n\n",
      "- Protocol code: ", yaml$protocol_code, "\n",
      "- Title: ", yaml$title, "\n"),
    news_contents)

  writeLines(news,
             file.path(path, "NEWS.md"))
  return(invisible(NULL))

}
