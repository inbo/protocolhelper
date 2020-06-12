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
#'
#' @details The links to earlier releases and older versions can be found
#' through a subfolder structure below `publish/version_number` that mirrors the
#' structure of `src/thematic` and `src/project`.
#'
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom bookdown render_book
#' @importFrom fs dir_delete dir_exists dir_ls file_delete
#' @importFrom purrr map map_chr map_lgl
#' @importFrom rmarkdown render
#' @importFrom rprojroot find_root
#' @importFrom stringr str_replace_all
#' @importFrom utils tail
#' @importFrom yaml as.yaml
#'
#' @keywords internal
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' protocolhelper:::render_release()
#'}

render_release <- function(output_root = "publish") {
  assert_that(is.string(output_root))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  git_root <- find_root(is_git_root)
  output_root <- file.path(git_root, output_root)

  protocol_index <- dir_ls(
    file.path(git_root, "src"),
    type = "file",
    recurse = 3,
    regexp = "index\\.Rmd"
  )
  yaml <- map(protocol_index, yaml_front_matter)
  parameters <- map(yaml, "params")
  version <- map(parameters, "version_number")
  missing_version <- !map_lgl(version, is.string)
  if (any(missing_version)) {
    stop(
      "version not a string in: ",
      paste(protocol_index[missing_version], collapse = "; ")
    )
  }
  version <- map_chr(parameters, "version_number")
  wrong_format <- !grepl("[0-9]{4}\\.[0-9]{2}", version)
  if (any(wrong_format)) {
    stop(
      "version not in YYYY-XX format: ",
      paste(protocol_index[wrong_format], collapse = "; ")
    )
  }
  protocol_index <- protocol_index[order(version)]
  yaml <- yaml[order(version)]
  parameters <- parameters[order(version)]
  version <- sort(version)
  for (i in seq_along(protocol_index)) {
    target_dir <- file.path(output_root, version[i])
    if (dir_exists(target_dir)) {
      if (i < length(protocol_index)) {
        next
      }
      dir_delete(target_dir)
    }
    setwd(dirname(protocol_index[i]))
    render_book(
      input = ".",
      output_format = "bookdown::gitbook",
      output_file = "index.html",
      output_dir = target_dir,
      envir = new.env()
    )
    yaml[[i]][["output"]] <- list(`rmarkdown::html_document` = "default")
    protocol_code <- map_chr(parameters, "protocol_code")
    relevant <- protocol_code == protocol_code[[i]]
    news <- map_chr(
      dirname(names(protocol_code[relevant])),
      ~paste(tail(readLines(file.path(.x, "NEWS.Rmd")), -1), collapse = "\n")
    )
    if (length(news) > 1) {
      lang <- map_chr(parameters[relevant], "language")
      lang <- factor(
        lang,
        c("nl", "en"),
        c("\n# Nederlandse versie\n", "\n# English version \n")
      )
      news <- paste(lang, news, collapse = "\n")
    }
    writeLines(
      c("---", as.yaml(yaml[[i]]), "---", "", news),
      "render_NEWS.Rmd"
    )
    target_dir <- file.path(
      output_root, yaml[[i]][["params"]][["protocol_code"]]
    )
    if (dir_exists(target_dir)) {
      dir_delete(target_dir)
    }
    render(
      "render_NEWS.Rmd", output_file = "index.html", envir = new.env(),
      output_dir = target_dir
    )
    file_delete("render_NEWS.Rmd")
  }
}
