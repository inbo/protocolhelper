#' @title Function to render the protocols that are ready for a new release.
#'
#' @description This function renders the protocols which are ready for a new
#' release into a folder `publish/version_number`, where
#' `version_number` is of the form `YYYY.NN` (year, number giving order of
#'  release in a year). In addition, earlier
#'  released and older versions of protocols remain available in the `publish`
#'  folder (see also \code{details}).
#'  This function is for internal use only.
#'  If you are looking for a function to preview your protocol,
#'  see `protocolhelper::render_protocol()`.
#' The website also contains a welcoming page, a news page and separate
#' pages per protocol-type with overview tables of all versions of published
#' protocols.
#'
#' @param output_root A character string giving the root folder without version
#' number. Default is "publish".
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom bookdown gitbook render_book pdf_book
#' @importFrom fs dir_copy dir_delete dir_exists dir_ls file_delete file_exists
#' @importFrom purrr map map_chr map_lgl
#' @importFrom rmarkdown html_document pandoc_variable_arg render
#' @importFrom rprojroot find_root
#' @importFrom stats aggregate
#' @importFrom stringr str_replace_all
#' @importFrom utils tail write.csv
#' @importFrom yaml as.yaml read_yaml
#'
#' @noRd
#'
#'
#' @examples
#' \dontrun{
#' protocolhelper:::render_release()
#'}

render_release <- function(output_root = "publish") {
  assert_that(is.string(output_root))
  assert_that(
    requireNamespace("reactable", quietly = TRUE),
    requireNamespace("slickR", quietly = TRUE)
  )

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  git_root <- find_root(is_git_root)
  output_root <- file.path(git_root, output_root)

  # write zenodo metadata file
  fs::dir_create(output_root)
  adapt_zenodo(json = ".zenodo.json",
               write = TRUE,
               path_from = git_root,
               path_to = output_root)

  protocol_index <- dir_ls(
    file.path(git_root, "source"),
    type = "file",
    recurse = TRUE,
    regexp = "source\\/s[fpioa]p\\/.+\\/index\\.Rmd")
  output_yml_files <- dir_ls(
    file.path(git_root, "source"),
    type = "file",
    recurse = TRUE,
    regexp = "source\\/s[fpioa]p\\/.+\\/_output\\.yml")

  yaml <- map(protocol_index, yaml_front_matter)
  output_yaml <- map(output_yml_files, read_yaml, eval.expr = TRUE)
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
  output_yaml <- output_yaml[order(version)]
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
    on.exit(unlink(file.path(dirname(protocol_index[i]), "css"),
                   recursive = TRUE), add = TRUE)
    if (!dir_exists(file.path(dirname(protocol_index[i]), "pandoc"))) {
      dir_copy(
        system.file("pandoc", package = "protocolhelper"),
        file.path(dirname(protocol_index[i]), "pandoc")
      )
    }
    on.exit(unlink(file.path(dirname(protocol_index[i]), "pandoc"),
                   recursive = TRUE), add = TRUE)
    setwd(dirname(protocol_index[i]))
    pdf_name <- paste0(yaml[[i]][["protocol_code"]], "_",
                       yaml[[i]][["version_number"]], ".pdf")
    render_book(
      input = ".",
      output_format = pdf_book(
        template = "pandoc/inbo_protocol.tex", # nolint
        pandoc_args = c(
          "--top-level-division=chapter",
          pandoc_variable_arg(
            "reviewer", yaml[[i]][["reviewer"]]
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
        )
      ),
      output_file = pdf_name,
      output_dir = target_dir,
      envir = new.env()
    ) |> suppressWarnings()
    render_book(
      input = ".",
      output_format = gitbook(
        split_by = output_yaml[[i]][["bookdown::gitbook"]][["split_by"]],
        split_bib = output_yaml[[i]][["bookdown::gitbook"]][["split_bib"]],
        pandoc_args = c(
          pandoc_variable_arg(
            "reviewer", yaml[[i]][["reviewer"]]
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
        template = "css/gitbook.html",
        css = "css/inbo_rapport.css",
        config = list(
          download = list(pdf_name),
          toc = list(
            before =
              ifelse(yaml[[i]][["language"]] == "en",
                       '<li class="toc-logo"><a href="https://www.vlaanderen.be/inbo/en-gb/homepage/"><img src="css/img/inbo-en.jpg"></a></li>\n<li class="toc-logo"><a href="https://inbo.github.io/protocols/"><button class="btn"><i class="fa fa-home"></i> Protocols homepage</button></li>\n', # nolint start
                       '<li class="toc-logo"><a href="https://www.vlaanderen.be/inbo/home/"><img src="css/img/inbo-nl.jpg"></a></li>\n<li class="toc-logo"><a href="https://inbo.github.io/protocols/"><button class="btn"><i class="fa fa-home"></i> Protocols homepage</button></li>\n'
              ),
            after = '<li class="cc"><a href="http://creativecommons.org/licenses/by/4.0/"><img src="css/img/cc-by.png"></a></li>' # nolint end
            )
          )
      ),
      output_file = "index.html",
      output_dir = target_dir,
      envir = new.env()
    ) |> suppressWarnings()
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
  meta$type <- get_protocol_type(meta$code)
  type_abbr <- c("sfp", "sip", "sap", "sop", "spp")
  meta_order <- order(meta$type, meta$theme, meta$code,
                      -as.integer(factor(meta$version)))
  meta <- meta[meta_order, ]
  rownames(meta) <- NULL
  meta <- meta[, c("type", "version", "code", "title", "theme")]
  meta$address <- paste0(meta$version, "/index.html")

  meta <- split(meta, ~type)
  meta <- map(meta, function(x) x[, !(names(x) %in% "type")])
  meta <- map(meta, function(x) {
    x$striping <- factor(x$code)
    levels(x$striping) <- rep_len(c(1, 0), length(levels(x$striping)))
    x$striping <- as.logical(as.numeric(as.character(x$striping)))
    return(x)
  })

  if (!dir_exists(file.path(git_root, "source", "homepage"))) {
    dir_copy(
      system.file("rmarkdown/homepage", package = "protocolhelper"),
      file.path(git_root, "source", "homepage"))
  }
  setwd(file.path(git_root, "source", "homepage"))
  for (i in seq_along(meta)) {
    write.csv(x = meta[[i]],
              file = paste0(type_abbr[i], ".csv"),
              row.names = FALSE)
  }
  index <- readLines("index.Rmd")
  repo_news <- readLines("../../NEWS.md")
  writeLines(
    c(index,
      "",
      "# NEWS",
      "",
      repo_news
      ),
    "index.Rmd"
  )
  if (!dir_exists(file.path(git_root, "source", "homepage", "css"))) {
    dir_copy(
      system.file("css", package = "protocolhelper"),
      file.path(git_root, "source", "homepage", "css")
    )
  }
  on.exit(unlink(file.path(git_root, "source", "homepage"),
                 recursive = TRUE), add = TRUE)
  render_book(
    "index.Rmd",
    output_file = "index.html",
    envir = new.env(),
    output_dir = output_root,
    output_format = gitbook(
      toc_depth = 2,
      number_sections = FALSE,
      split_by = "chapter",
      template = "css/gitbook.html",
      css = "css/inbo_rapport.css",
      config = list(
        toc = list(
          before =
            '<li class="toc-logo"><a href="https://www.vlaanderen.be/inbo/home/"><img src="css/img/inbo-en.jpg"></a></li>', # nolint
          after = '<li class="cc"><a href="http://creativecommons.org/licenses/by/4.0/"><img src="css/img/cc-by.png"></a></li>' # nolint
        )
      )
    )
  )
}


#' Adapt the `.zenodo.json` file from `protocolsource` to `protocols` repository
#'
#' @param json a `JSON` string, url or file
#' @param write whether to write a `.zenodo.json` file (TRUE) or to return a
#' `json` string
#' @param path_from Default current working directory. The root folder of the
#' `protocolsource` repo.
#' @param path_to Path to where the file should be written.
#'
#' @noRd
adapt_zenodo <- function(json = ".zenodo.json", write = TRUE, path_from = ".",
                         path_to) {
  # read `.zenodo.json` from protocolsource
  zenodo <- jsonlite::fromJSON(file.path(path_from, json),
                               simplifyVector = FALSE)
  # adapt it
  zenodo$title <- "Protocols website of the Research Institute for Nature and Forest (INBO)" # nolint
  zenodo$description <- "This archive contains the rendered html and pdf files of protocols used at the Research Institute for Nature and Forest (INBO), Brussels, Belgium (<a href=\"https://www.inbo.be/en\">www.inbo.be</a>). The website with the compiled protocols (including older versions) is at <a href=\"https://protocols.inbo.be\">https://protocols.inbo.be</a>."# nolint

  if (write) {
    # write it to publish folder
    jsonlite::write_json(x = zenodo,
                         file.path(path_to, ".zenodo.json"),
                         pretty = TRUE,
                         auto_unbox = TRUE)
  } else {
    jsonlite::toJSON(x = zenodo,
                     pretty = TRUE,
                     auto_unbox = TRUE)
  }
}
