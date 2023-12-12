#' @title Updates the `protocolsource` repo `NEWS.md` file
#'
#' @description
#' Constructs and writes a `NEWS.md` file based on version numbers,
#' protocol codes and protocol titles.
#' Not intended for interactive use.
#' Should be used in a GitHub Action.
#' Reads the `NEWS.md` from the main branch and switches back to current
#' protocol branch to write the updated `NEWS.md`
#'
#'
#' @param protocol_code protocol code of the protocol that is to be published in
#' this release
#' @param path Default current working directory. The root folder of the
#' `protocolsource` repo.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom assertthat assert_that is.string
#' @importFrom stringr str_detect
#' @importFrom gert git_branch git_branch_list git_branch_checkout
#'
#' @return invisible null
#'
#' @noRd
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

  # check if new protocol or update
  update <- any(grepl(yaml$protocol_code, news_contents, fixed = TRUE))

  # contents of NEWS.md file
  today <- Sys.Date()
  news <- c(
    paste0(
      "### ", "Update of protocol: "[update],
      "First version of protocol: "[!update],
      yaml$protocol_code, "\n\n",
      "- Title: ", yaml$title, "\n",
      "- Published on: ", today, "\n",
      "- Version number: ", yaml$version_number, "\n",
      "- Link to this version: [", yaml$protocol_code, " version ",
      yaml$version_number, "](", yaml$version_number, "/index.html)\n"
      ),
    news_contents)

  writeLines(news,
             file.path(path, "NEWS.md"))
  return(invisible(NULL))

}
