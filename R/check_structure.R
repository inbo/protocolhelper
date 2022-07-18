#' @title Checks protocol document structure
#'
#' @description This function reads the protocol and checks if the document
#' structure is correct: chunks have head and tail, required headings are
#' present and in the right order,...
#' This function is intended for checking if a protocol is ready to be rendered
#' and published.
#'
#' @inheritParams get_path_to_protocol
#' @param fail Should the function drop an error in case of a problem?
#' Defaults to `TRUE` in a non-interactive session and `FALSE` in an interactive
#' session.
#'
#' @return A report of all failed checks.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom utils head tail
#' @importFrom assertthat assert_that is.flag noNA
#' @importFrom stringr str_detect
#'
#' @export
#'
check_structure <- function(protocol_code, fail = !interactive()) {
  assert_that(str_detect(protocol_code, "^s[fioap]p-\\d{3}-(nl|en)$"))
  assert_that(is.flag(fail), noNA(fail))

  x <- load_protocolcheck(x = protocol_code)
  template_name <-
    gsub(
      pattern = "(s\\w{1}p)-\\d*-(\\w{2})",
      replacement = "template_\\1_\\2",
      protocol_code
    )
  path_to_template <-
    system.file(
      file.path("rmarkdown", "templates", template_name, "skeleton"),
      package = "protocolhelper")
  files_protocol <- list.files(path = x$path)
  files_template <- list.files(path = path_to_template)

  # check if file(name)s from template are conserved
  files_template_i <-
    c(files_template[files_template != "skeleton.Rmd"], "index.Rmd")
  difffiles <- files_template_i[!files_template_i %in% files_protocol]
  difffiles <-
    difffiles[!grepl("^\\d{2}_appendices.Rmd", difffiles)]
  difffiles <-
    difffiles[!grepl("^\\d{2}_subprocotols.Rmd", difffiles)]
  x$add_error(msg = sprintf("the protocol lacks file(s) %s", difffiles))

  protocol_rmds <- files_protocol[grepl(".Rmd$", files_protocol)]
  for (file in protocol_rmds) {
    x <- check_file(file, x, files_template, path_to_template)
  }

  # check protocol-specific NEWS.md file
  x <- check_news_protocol(x = x)

  # check if multiple Rmd files with the same chapter number exist
  chapters <- protocol_rmds[grepl("^\\d{2}", protocol_rmds)]
  chapter_numbers <- gsub(
    pattern = "(?<=^\\d{2})(\\w|\\.)+",
    replacement = "",
    x = chapters,
    perl = TRUE)
  chapter_numbers <- as.numeric(chapter_numbers)
  x$add_error(msg =
                sprintf("multiple file names starting with %s",
                        formatC(
                          chapter_numbers[duplicated(chapter_numbers)],
                          width = 2, flag = "0"
                        )
                )
  )
  # check numbers are in order
  if (!all(sort(chapter_numbers) == seq_along(chapter_numbers))) {
    x$add_error(msg = "Chapter numbers are not in order")
  }

  # references
  if (!file.exists(file.path(x$path, "index.Rmd"))) {
    x$add_error(
      msg = "No index.Rmd file, cannot check bibliography yaml field."
    )
  } else {
    yml <- yaml_front_matter(file.path(x$path, "index.Rmd"))
    if (has_name(yml, "bibliography")) {
      x$add_error(
        msg =
          sprintf("%s not found (needed for references)",
                  yml$bibliography[!yml$bibliography %in% files_protocol])
      )
    }
  }

  return(x$check(fail = fail))
}



check_file <- function(filename, x, files_template, path_to_template) {
  # check if file is present in template
  x$add_error(
    msg =
      sprintf(
        "file %s should be removed (after moving the content)",
        filename[!filename %in% c(files_template, "index.Rmd") &&
                   grepl("^\\d{2}_", filename)]
      )
  )

  # check if chunks in Rmd files are correct
  rmd <- readLines(file.path(x$path, filename))
  start_chunk <- grep("^```\\{r.*}", rmd)
  end_chunk <- grep("^```[:space:]?$", rmd)
  x$add_error(
    msg =
      paste(", file", filename, "has a problem with R chunks")[
        !(length(start_chunk) == length(end_chunk) &&
            all(start_chunk < end_chunk))
      ]
  )

  for (i in rev(seq_along(start_chunk))) {
    rmd <- c(
      head(rmd, start_chunk[i] - 1),
      "{r code chunk}",
      tail(rmd, length(rmd) - end_chunk[i])
    )
  }

  # check headings general
  headings <- rmd[grepl("^[[:space:]]?#", rmd)]
  x$add_error(
    msg = sprintf(
      "file %s: Headings have to start with a '#',
        remove the leading whitespace in headings %s",
      filename, headings[grepl("^[[:space:]]+#", headings)])
  )
  x$add_error(
    msg = sprintf(
      "file %s: Whitespace at the end of a heading is not allowed,
        remove them in headings %s",
      filename, headings[grepl("[[:space:]]+$", headings)])
  )

  # compare headings with template
  if (filename %in% files_template) {
    template <- readLines(file.path(path_to_template, filename))
    headings_template <- template[grepl("^[[:space:]]?#", template)]
    headings_template <-
      headings_template[!grepl("^### Subtit", headings_template)]
    x$add_error(
      msg = sprintf(
        "Heading(s) %s lack(s) in file %s",
        headings_template[!headings_template %in% headings],
        filename
      )
    )

    headings1 <- headings[grepl("^# .*", headings)]
    headings1_template <- headings_template[grepl("^# .*", headings_template)]
    x$add_error(
      msg = sprintf(
        "Heading 1 %s is not allowed in file %s",
        headings1[!headings1 %in% headings1_template],
        filename
      )
    )

    if (
      length(headings[headings %in% headings_template]) ==
      length(headings_template) &&
      !all(headings[headings %in% headings_template] == headings_template)
    ) {
      x$add_error(
        msg = paste("Headings of file", filename,
                    "are not in this order:",
                    paste(headings_template, collapse = " > ")
        )
      )
    }

  } else {
    if (!grepl("^\\d{2}_", filename) && filename != "index.Rmd") {
      headings1 <- headings[grepl("^# .*", headings)]
      x$add_error(
        msg = sprintf(
          paste(filename,
            "should not have headings of level 1, please adapt header(s): %s"),
          headings1
        )
      )
    }
  }
  if (filename == "index.Rmd") {
    template <- readLines(file.path(path_to_template, "skeleton.Rmd"))
    template_end <-
      template[max(grep("^[[:space:]]?#", template)):length(template)]
    rmd_end <- rmd[max(grep("^[[:space:]]?#", rmd)):length(rmd)]
    if (!all(template_end == rmd_end)) {
      x$add_error(
        msg = "Heading 'Metadata' or the table below have changed"
      )
    }
  }
  return(x)
}


#' @importFrom commonmark markdown_xml
#' @importFrom xml2 xml_attr xml_text read_xml xml_find_all xml_children
#' @importFrom utils file_test
check_news_protocol <- function(x) {
  # check file
  doc_error <- "Use NEWS.md instead of NEWS.Rmd"[
    file_test("-f", file.path(x$path, "NEWS.Rmd"))
  ]
  md_file <- file.path(x$path, "NEWS.md")
  if (!file_test("-f", md_file)) {
    doc_error <- c(doc_error, "Missing NEWS.md")
    x$add_error <- doc_error
    return(x)
  }
  # check file contents for problems
  news_file <- readLines(md_file, encoding = "UTF8")
  xml <- markdown_xml(news_file)
  xml <- read_xml(xml)
  all_headings <- xml_find_all(xml,
                               xpath = ".//d1:heading")
  headings_level_2 <- all_headings[xml_attr(all_headings, "level") == "2"]
  headings_text <- xml_text(headings_level_2)
  headings_link <- xml_attr(xml_children(headings_level_2), "destination")

  if (!file.exists(file.path(x$path, "index.Rmd"))) {
    x$add_error(
      msg = "No index.Rmd file, cannot check version number field."
    )
  } else {
    yml <- yaml_front_matter(file.path(x$path, "index.Rmd"))
    version_number <- yml$version_number

    if (!any(str_detect(version_number, headings_text))) {
        x$add_error(
          "An entry is missing in NEWS.md for this version of the protocol")
    }

    problems <- sprintf(
      paste0(
        "Mismatch detected between link text '%1$s' ",
        "and link destination '%2$s' /nin level 2 headings of NEWS.md file"),
      headings_text,
      headings_link)[
        !str_detect(headings_link, headings_text)
      ]

    current_link <- headings_link[grepl(version_number, headings_link)]

    problems <- c(
      problems,
      sprintf(
        "URL for version %s of the protocol is not correct in NEWS.md",
        version_number
      )[!identical(current_link,
                   paste0("../",
                          version_number,
                          "/index.html"))]
    )

    problems <- c(
      problems,
      "The entries in NEWS.md are not sorted from most recent to oldest"[
        !identical(headings_text, sort(headings_text, decreasing = TRUE))
      ]
    )
    x$add_error(problems)
  }
  return(x)
}
