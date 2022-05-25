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
#' @importFrom assertthat assert_that
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom utils head tail
#'
#' @export
#'
check_structure <- function(protocol_code, fail = !interactive()) {
  x <- load_protocolcheck(x = protocol_code)
  template_name <-
    gsub(
      pattern = "(s\\wp)-\\d*-(\\w{2})_.*",
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
  x$add_error(msg = sprintf("%s lacks file(s) %s", protocol_code, difffiles))

  for (file in files_protocol[grepl(".Rmd$", files_protocol)]) {

    # check if chunks in Rmd files are correct
    rmd <- readLines(file.path(x$path, file))
    start_chunk <- grep("^```\\{r.*}", rmd)
    end_chunk <- grep("^```[:space:]?$", rmd)
    if (
      !(length(start_chunk) == length(end_chunk) &&
        all(start_chunk < end_chunk))
    ) {
      x$add_error(
        msg =
          paste(protocol_code, ", file", file, "has a problem with R chunks"))
    }

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
        "%s: file %s: Headings have to start with a '#',
        remove the leading whitespace in headings %s",
        protocol_code, file, headings[grepl("^[[:space:]]+#", headings)])
    )
    x$add_error(
      msg = sprintf(
        "%s: file %s: Whitespace at the end of a heading is not allowed,
        remove them in headings %s",
        protocol_code, file, headings[grepl("[[:space:]]+$", headings)])
    )

    # compare headings with template
    if (file %in% files_template) {
      template <- readLines(file.path(path_to_template, file))
      headings_template <- template[grepl("^[[:space:]]?#", template)]
      headings_template <-
        headings_template[!grepl("^### Subtit", headings_template)]
      x$add_error(
        msg = sprintf(
          "%s: Heading(s) %s lack(s) in file %s",
          protocol_code,
          headings_template[!headings_template %in% headings],
          file
        )
      )

      headings1 <- headings[grepl("^# .*", headings)]
      headings1_template <- headings_template[grepl("^# .*", headings_template)]
      x$add_error(
        msg = sprintf(
          "%s: Heading 1 %s is not allowed in file %s",
          protocol_code,
          headings1[!headings1 %in% headings1_template],
          file
        )
      )

      if (
        length(headings[headings %in% headings_template]) ==
          length(headings_template) &&
        !all(headings[headings %in% headings_template] == headings_template)
      ) {
        x$add_error(
          msg = paste(protocol_code, "Headings of file", file,
                      "are not in this order:",
                      paste(headings_template, collapse = " > ")
          )
        )
      }

    }
    if (file == "index.Rmd") {
      template <- readLines(file.path(path_to_template, "skeleton.Rmd"))
      template_end <-
        template[max(grep("^[[:space:]]?#", template)):length(template)]
      rmd_end <- rmd[max(grep("^[[:space:]]?#", rmd)):length(rmd)]
      if (!all(template_end == rmd_end)) {
        x$add_error(
          msg =
            paste(
              protocol_code,
              "Heading 'Metadata' or the table below have changed"
            )
        )
      }
    }
  }

  # references
  yml <- yaml_front_matter(file.path(x$path, "index.Rmd"))
  if (has_name(yml, "bibliography")) {
    x$add_error(
      msg =
        sprintf("%s not found (needed for references)",
                yml$bibliography[!yml$bibliography %in% files_protocol])
    )
  }

  return(x$check <- fail)
}
