#' @title Updates the protocolsource repo NEWS.md file
#'
#' @description
#' Constructs and writes a NEWS.md file based on version numbers, protocol codes
#' and protocol titles.
#' Not intended for interactive use.
#' Should be used in a GitHub Action.
#' Assumes the branch containing the protocol for the next release is checked
#' out, and it is up to date with the main branch.
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
#'
#' @export
#'
update_news_release <- function(protocol_code, path = ".") {
  assert_that(is.string(path))
  check_protocolcode(protocol_code)

  news_file <- file.path(path, "NEWS.md")
  news_contents <- readLines(news_file)

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

}