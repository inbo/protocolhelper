#' @title Set general and specific protocol tags
#'
#' @description
#' This function is a part of the GitHub Action.
#' Therefore it only works when run in a GitHub Action on the main or master
#' branch.
#' Otherwise it will only return a message.
#' It sets a general protocol tag at the current commit.
#' The tag message is generated from the related entry from `NEWS.md` as
#' message.
#' This tag will turn into a release.
#'
#' @inheritParams update_version_number
#'
#' @importFrom fs dir_ls
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom gert git_tag_create git_config git_config_set
#'
#' @return invisible NULL
#' @keywords internal
#'
set_tags <- function(
  protocol_code,
  commit = TRUE,
  path = ".") {

  if (
    !as.logical(Sys.getenv("GITHUB_ACTIONS", "false")) ||
    !Sys.getenv("GITHUB_REF") %in% c("refs/heads/main", "refs/heads/master") ||
    Sys.getenv("GITHUB_EVENT_NAME") != "push"
  ) {
    message("Not on GitHub, not a push or not on main or master.")
    return(invisible(NULL))
  }

  assert_that(is_dir(path))
  check_protocolcode(protocol_code)

  old_config <- git_config(repo = path)
  on.exit(
    git_config_set(
      "user.name",
      old_config$value[old_config$name == "user.name"],
      repo = path),
    add = TRUE
  )
  on.exit(
    git_config_set(
      "user.email",
      old_config$value[old_config$name == "user.email"],
      repo = path),
    add = TRUE
  )
  git_config_set(
    "user.name", "Protocolhelper bot",
    repo = path
  )
  git_config_set(
    "user.email", "protocolhelper@inbo.be",
    repo = path
  )

  #determine and print the next protocols tag
  #determine and print the next protocol-specific tag(s)
  path_to_protocol <- get_path_to_protocol(
    protocol_code = protocol_code)

  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))

  # construct protocol tags
  specific_tag <- paste0(protocol_code, "-", yml$version_number)
  general_tag <- paste0("protocols-", yml$version_number)

  message("The general tag is: ", general_tag,
          "\nThe specific tag is: ", specific_tag)

  general_tag_message <- paste0("Title: ", yml$title)

  git_tag_create(
    name = general_tag,
    message = general_tag_message,
    repo = path)

  git_tag_create(
    name = specific_tag,
    message = "See protocol-specific NEWS.md file for details",
    repo = path)

  return(invisible(NULL))
}
