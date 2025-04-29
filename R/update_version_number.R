#' @title Updates the version number in the YAML section of a protocol
#' `index.Rmd` file and optionally in protocol `NEWS.md`
#'
#' @description Makes use of `get_version_number` to get a new version number
#' and changes this accordingly in the YAML section of `index.Rmd` file and
#' optionally in `NEWS.md`.
#'
#' @param protocol_code The protocol_code corresponding with the name of the
#' branch that contains the new or updated protocol.
#' @param path Default is current working directory. Should correspond with
#' root directory of `protocolsource` repo.
#' @param commit Logical. Default TRUE. Whether or not to add and commit the
#' changes to the protocol branch
#' @param update_news Logical. Default TRUE. Whether or not to find and replace
#' old version number by new version number in the `NEWS.md` heading 2.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom fs is_dir
#' @importFrom assertthat assert_that
#' @importFrom gert git_status git_add git_commit
#' @importFrom stringr str_replace_all
#' @importFrom xfun read_utf8 write_utf8
#'
#' @return TRUE if version number in yaml is updated. FALSE otherwise.
#' @export
#' @family creation
#'
update_version_number <- function(
    protocol_code,
    commit = TRUE,
    update_news = TRUE,
    path = ".") {
  # assertions
  check_protocolcode(protocol_code)
  assert_that(is_dir(path))
  assert_that(is.logical(update_news))

  # what should be the version number?
  new_version <- get_version_number(path = path)

  # what is the version number?
  path_to_protocol <- get_path_to_protocol(protocol_code)
  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))
  old_version <- yml$version_number

  if (new_version == old_version) {
    message("The version number is up to date")
    return(FALSE)
  } else {
    index <- read_utf8(file.path(path_to_protocol, "index.Rmd"))
    index[grepl("version_number:", index)] <- paste0(
      "version_number: '", new_version, "'"
    )
    write_utf8(index, file.path(path_to_protocol, "index.Rmd"))

    message_text <- paste0(
      "Bumped ", old_version, " to ", new_version,
      " in index.Rmd"
    )

    # update in NEWS.md
    if (update_news) {
      old_news <- read_utf8(file.path(path_to_protocol, "NEWS.md"))
      news <- str_replace_all(
        string = old_news,
        pattern = sprintf("##\\s\\[%1$s\\]\\(\\.\\.\\/%1$s\\/", old_version),
        replacement = sprintf("## [%1$s](../%1$s/", new_version)
      )
      write_utf8(news, file.path(path_to_protocol, "NEWS.md"))

      message_text <- paste0(
        message_text,
        " and NEWS.md"[!identical(old_news, news)]
      )
    }
    message(message_text)

    if (commit) {
      unstaged <- git_status(staged = FALSE, repo = path)
      changes <- unstaged$file[unstaged$status == "modified"]
      changes <- grep(pattern = "index|NEWS", x = changes, value = TRUE)
      if (length(changes)) {
        git_add(changes, repo = path)
      }
      git_commit(
        message = message_text,
        repo = path
      )
    }
    return(TRUE)
  }
}
