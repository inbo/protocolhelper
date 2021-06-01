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
#' @importFrom bookdown gitbook render_book
#' @importFrom fs dir_copy dir_delete dir_exists dir_ls file_delete file_exists
#' @importFrom purrr map map_chr map_lgl
#' @importFrom rmarkdown html_document pandoc_variable_arg render
#' @importFrom rprojroot find_root
#' @importFrom stats aggregate
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
  version <- map(yaml, "version_number")
  missing_version <- !map_lgl(version, is.string)
  if (any(missing_version)) {
    stop(
      "version not a string in: ",
      paste(protocol_index[missing_version], collapse = "; ")
    )
  }
  version <- map_chr(yaml, "version_number")
  wrong_format <- !grepl("[0-9]{4}\\.[0-9]{2}", version)
  if (any(wrong_format)) {
    stop(
      "version not in YYYY.XX format: ",
      paste(protocol_index[wrong_format], collapse = "; ")
    )
  }
  protocol_index <- protocol_index[order(version)]
  yaml <- yaml[order(version)]
  version <- sort(version)
  for (i in seq_along(protocol_index)) {
    target_dir <- file.path(output_root, version[i])
    if (dir_exists(target_dir)) {
      if (i < length(protocol_index)) {
        next
      }
      dir_delete(target_dir)
    }
    if (!dir_exists(file.path(dirname(protocol_index[i]), "css"))) {
      dir_copy(
        system.file("css", package = "protocolhelper"),
        file.path(dirname(protocol_index[i]), "css")
      )
    }
    setwd(dirname(protocol_index[i]))
    render_book(
      input = ".",
      output_format = gitbook(
        split_by = yaml[[i]][["output"]][["bookdown::gitbook"]][["split_by"]],
        split_bib = yaml[[i]][["output"]][["bookdown::gitbook"]][["split_bib"]],
        pandoc_args = c(
          as.vector(
            sapply(
              yaml[[i]][["reviewers"]],
              pandoc_variable_arg,
              name = "reviewer"
            )
          ),
          pandoc_variable_arg(
            "file_manager", yaml[[i]][["file_manager"]]
          ),
          pandoc_variable_arg(
            "protocol_code", yaml[[i]][["protocol_code"]]
          ),
          pandoc_variable_arg(
            "version_number", yaml[[i]][["version_number"]]
          ),
          pandoc_variable_arg(
            "thema",
            c(yaml[[i]][["theme"]], yaml[[i]][["project_name"]])[1]
          ),
          pandoc_variable_arg("lang", yaml[[i]][["language"]])
        ),
        template = "css/gitbook.html"
      ),
      output_file = "index.html",
      output_dir = target_dir,
      envir = new.env()
    )
    yaml[[i]][["output"]] <- list(`rmarkdown::html_document` = "default")
    protocol_code <- map_chr(yaml, "protocol_code")
    relevant <- protocol_code == protocol_code[[i]]
    news <- map_chr(
      dirname(names(protocol_code[relevant])),
      ~paste(tail(readLines(file.path(.x, "NEWS.md")), -1), collapse = "\n")
    )
    if (length(news) > 1) {
      lang <- map_chr(yaml[relevant], "language")
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
      output_root, yaml[[i]][["protocol_code"]]
    )
    if (dir_exists(target_dir)) {
      dir_delete(target_dir)
    }
    render(
      "render_NEWS.Rmd", output_file = "index.html", envir = new.env(),
      output_dir = target_dir, output_format = html_document(
        css = "css/inbo_rapport.css"
      )
    )
    file_delete("render_NEWS.Rmd")
  }
  protocols <- dir_ls(
    output_root, recurse = TRUE, type = "file",
    regexp = "[0-9]{4}\\.[0-9]{2}/index.html"
  )
  meta <- map(
    protocols,
    function(x) {
      meta <- readLines(x, n = 40)
      meta <- str_subset(meta, pattern = "<meta name=\"protocol")
      z <- as.list(str_replace(meta, ".*content=\"(.*?)\".*", "\\1"))
      names(z) <- str_replace(meta, ".*name=\"protocol-(.*?)\".*", "\\1")
      as.data.frame(z)
    }
  )
  meta <- do.call(rbind, meta)
  meta_order <- order(meta$theme, meta$code, -as.integer(factor(meta$version)))
  meta <- meta[meta_order, ]
  meta$Rmd <- sprintf(
    "- [%1$s](%1$s/index.html) (%2$s)", meta$version, meta$language
  )
  meta <- aggregate(
    Rmd ~ theme + code + title + subtitle,
    data = meta, FUN = paste, collapse = "\n"
  )
  meta$Rmd <- sprintf(
    "## %1$s [(`%2$s`)](%2$s/index.html)\n\n%3$s\n\n%4$s",
    meta$title, meta$code, meta$subtitle, meta$Rmd
  )
  meta <- aggregate(Rmd ~ theme, data = meta, FUN = paste, collapse = "\n\n")
  meta$Rmd <- sprintf("# %s\n\n%s", meta$theme, meta$Rmd)
  setwd(file.path(git_root, "src"))
  if (!file_exists("homepage.Rmd")) {
    writeLines(
      "---\ntitle: INBO protocols\ndate: '`r Sys.Date()`'\noutput: html_document
---\n\n",
      "homepage.Rmd"
    )
  }
  homepage <- readLines("homepage.Rmd")
  writeLines(c(homepage, paste(meta$Rmd, collapse = "\n\n")), "homepage.Rmd")
  if (!dir_exists(file.path(git_root, "src", "css"))) {
    dir_copy(
      system.file("css", package = "protocolhelper"),
      file.path(git_root, "src", "css")
    )
  }
  render(
    "homepage.Rmd", output_file = "index.html", envir = new.env(),
    output_dir = output_root, output_format = html_document(
      css = "css/inbo_rapport.css"
    )
  )
  writeLines(homepage, "homepage.Rmd")
}

