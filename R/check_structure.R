#' @title Checks protocol document structure
#'
#' @description This function reads the protocol and checks if the document
#' structure is correct: chunks have head and tail, required headings are
#' present and in the right order,...
#' This function is intended for checking if a protocol is ready to be rendered
#' and published.
#'
#' @inheritParams get_path_to_protocol
#'
#' @return If one of the checks fails, an error message will be returned.
#'
#' @importFrom assertthat assert_that
#' @importFrom utils head tail
#'
#' @export
#'
check_structure <- function(protocol_code) {
  path_to_protocol <- get_path_to_protocol(
    protocol_code = protocol_code)
  template_name <-
    gsub(
      pattern = "(s\\wp)-\\d*-(\\w{2})", replacement = "template_\\1_\\2",
      protocol_code
    )
  path_to_template <-
    system.file(
      file.path("rmarkdown", "templates", template_name, "skeleton"),
      package = "protocolhelper")
  files_protocol <- list.files(path = path_to_protocol)
  files_template <- list.files(path = path_to_template)

  # check if file(name)s from template are conserved
  files_template_i <-
    c(files_template[files_template != "skeleton.Rmd"], "index.Rmd")
  difffiles <- files_template_i[!files_template_i %in% files_protocol]
  difffiles <- difffiles[!grep("^\\d{2}_appendices.Rmd", difffiles)]
  difffiles <- difffiles[!grep("^\\d{2}_subprocotols.Rmd", difffiles)]
  assert_that(
    length(difffiles) == 0,
    msg = paste(protocol_code, "lacks file(s)", difffiles)
  )

  for (file in files_protocol[grepl(".Rmd$", files_protocol)]) {

    # check if chunks in Rmd files are correct
    rmd <- readLines(file.path(path_to_protocol, file))
    start_chunk <- grep("^```\\{r.*}", rmd)
    end_chunk <- grep("^```[:space:]?$", rmd)
    assert_that(
      length(start_chunk) == length(end_chunk),
      all(start_chunk < end_chunk),
      msg = paste(protocol_code, ", file", file, "has a problem with R chunks")
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
    assert_that(
      !any(grepl("^[[:space:]]+#", headings)),
      msg =
        paste(
          protocol_code, ", file", file,
          "Headings have to start with a '#', remove the leading whitespace."
        )
    )
    assert_that(
      !any(grepl("[[:space:]]+$", headings)),
      msg = paste(
        protocol_code,
        "Whitespace at the end of a heading is not allowed",
        paste(headings[grepl("[[:space:]]+$", headings)], collapse = "\n"),
        sep = "\n"
      )
    )

    # compare headings with template
    if (file %in% files_template) {
      template <- readLines(file.path(path_to_template, file))
      headings_template <- template[grepl("^[[:space:]]?#", template)]
      headings_template <-
        headings_template[!grepl("^### Subtit", headings_template)]
      assert_that(
        all(headings_template %in% headings),
        msg =
          paste(protocol_code, "Heading(s)",
                headings_template[!headings_template %in% headings],
                "lack(s) in file", file)
      )
      headings1 <- headings[grepl("^# .*", headings)]
      headings1_template <- headings_template[grepl("^# .*", headings_template)]
      assert_that(
        all(headings1 %in% headings1_template),
        msg = paste(protocol_code, "Heading 1",
                    headings1[!headings1 %in% headings1_template],
                    "is not allowed in file", file)
      )
      assert_that(
        all(headings[headings %in% headings_template] == headings_template),
        msg = paste(protocol_code, "Headings of file", file,
                    "are not in this order:", headings_template)
      )
    }
    if (file == "index.Rmd") {
      template <- readLines(file.path(path_to_template, "skeleton.Rmd"))
      template_end <-
        template[max(grep("^[[:space:]]?#", template)):length(template)]
      rmd_end <- rmd[max(grep("^[[:space:]]?#", rmd)):length(rmd)]
      assert_that(
        all(template_end == rmd_end),
        msg =
          paste(
            protocol_code,
            "Heading 'Metadata' or the table below have changed"
          )
      )
    }
  }

  # references
  if (
    any(grepl("^\\d{2}_referen",
              files_protocol[grepl(".Rmd$", files_protocol)]))
  ) {
    #should we also check for this in the yaml heading???
    assert_that(
      any(grep("\\.bib$", files_protocol)),
      msg = paste(protocol_code, ".bib-file needed for references")
    )
  }
}
