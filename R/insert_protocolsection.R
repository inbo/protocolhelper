#' @title Function to add a chapter or a section of a published protocol for
#' re-use in another protocol
#'
#' @description The idea is to execute this function in an R chunk with knitr
#' option \code{results="asis"}.
#'
#' @return The function will return Rmarkdown
#'
#' @param code_subprotocol Character string giving the protocol code from
#' which a subprotocol will be made (usually a sfp-type protocol)
#' @param version_number Character string with format YYYY.NN
#' @param file_name Character string with the name of the Rmarkdown
#' file (a chapter starting with a level 1 heading).
#' @param section Optional character string with the name of a section within an
#' Rmarkdown file.
#' Can also be a unique substring of a section title.
#' If not specified (the default): the whole file is taken. It
#' is assumed that the section has a level 2 heading.
#' @param demote_header Number of '#' to prefix to all titles before inserting
#' in current protocol. Default is 0.
#' A negative value can be used to remove
#' '#' from all section titles.
#' Allowed values are visible in the usage section.
#' @param fetch_remote Whether or not to fetch the remote. Default TRUE.
#'
#'
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom fs path_rel
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_remove str_replace_all str_extract_all
#' @importFrom purrr map map2 %>%
#' @importFrom knitr knit_child opts_knit
#' @importFrom rmarkdown yaml_front_matter
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' insert_protocolsection(
#'   code_subprotocol = "sfp-401-nl",
#'   version_number = "2021.01",
#'   file_name = "07_stappenplan.Rmd"
#' )
#'}
insert_protocolsection <-
  function(code_subprotocol,
           version_number,
           file_name,
           section = NULL,
           demote_header = c(0, 1, 2, -1),
           fetch_remote = TRUE) {

    check_versionnumber(version_number)
    check_protocolcode(code_subprotocol)

    demote_choices <- eval(formals()$demote_header)
    if (missing(demote_header)) {
      demote_header <- demote_choices[1]
      } else {
        assert_that(demote_header %in% demote_choices,
                    msg = paste("demote_header must be one of",
                                paste(demote_choices, collapse = ", ")))
      }
    assert_that(is.string(file_name))
    if (!missing(section)) {
      assert_that(is.string(section))
    }
    assert_that(is.flag(fetch_remote), noNA(fetch_remote))

    git_filepath <-
      get_path_to_protocol(code_subprotocol) %>%
      file.path(file_name) %>%
      path_rel(start = find_root(is_git_root))

    if (fetch_remote) {
      firstremote <- execshell("git remote", intern = TRUE)[1]
      execshell(paste0("git fetch ", firstremote),
                ignore.stdout = TRUE,
                ignore.stderr = TRUE)
    }
    existing_tags <- execshell("git tag", intern = TRUE)
    tag <- paste(code_subprotocol, version_number, sep = "-")
    assert_that(tag %in% existing_tags,
                msg = paste("The combination of code_subprotocol and",
                            "version_number does not refer to an existing",
                            "released protocol."))

    gitcommand <- paste0("git show ",
                         tag, ":",
                         git_filepath)

    # get the content of the Rmd file
    # this will return a character vector (each element is one sentence)
    rmd_content <- execshell(gitcommand,
           intern = TRUE)
    # What happens if this fails?

    # avoid looking in chunks
    # is_chunk will be TRUE when the line is a chunk
    is_chunk <- mark_chunks(rmd_content)

    if (!missing(section)) {
      has_section <- grepl(section, rmd_content, fixed = TRUE)
      assert_that(any(has_section), msg = "The section was not found.")

      # assuming section is header level 2
      h2 <- grepl("^##\\s[A-Z]", rmd_content) & !is_chunk # nolint: nonportable_path_linter, line_length_linter.
      # grab the section
      start_section <- which(has_section)
      if (length(start_section) > 1) {
        stop(
          "section does not uniquely identify one section"
        )
      }
      h2_rows <- which(h2)
      end_section <- min(h2_rows[h2_rows > start_section]) - 1
      rmd_content <- rmd_content[start_section:end_section]
      is_chunk <- is_chunk[start_section:end_section]
    }
    # demote headers
    if (demote_header != 0) {
      h1 <- grepl("^#\\s[A-Z]", rmd_content) & !is_chunk # nolint: nonportable_path_linter, line_length_linter.
      h2 <- grepl("^##\\s[A-Z]", rmd_content) & !is_chunk # nolint: nonportable_path_linter, line_length_linter.
      h3 <- grepl("^###\\s[A-Z]", rmd_content) & !is_chunk # nolint: nonportable_path_linter, line_length_linter.
      if (demote_header == -1) {
        if (any(h1)) {
          stop("demote header -1 not possible when a level 1 header is present")
        } else {
          rmd_content[h2] <- str_remove(rmd_content[h2], "^#")
          rmd_content[h3] <- str_remove(rmd_content[h3], "^#")
        }
      } else {
        rmd_content[h1] <- paste0(
          paste0(rep("#", demote_header), collapse = ""), rmd_content[h1])
        rmd_content[h2] <- paste0(
          paste0(rep("#", demote_header), collapse = ""), rmd_content[h2])
        rmd_content[h3] <- paste0(
          paste0(rep("#", demote_header), collapse = ""), rmd_content[h3])
      }
    }

    get_data_media(rmd_content, code_subprotocol, tag)

    # return rmd content
    # the following is not strictly necessary, but useful to test
    # insert_protocolsection() outside render_protocol()
    if (!is.null(opts_knit$get("output.dir"))) {
      res <- knit_child(text = rmd_content, quiet = TRUE)
    }  else {
      res <- rmd_content
    }


    return(cat(res, sep = "\n"))
  }




#' @title Internal function to get media and data files
#'
#' @description
#' Extracts all paths to files in data and media from rmd_content and uses git
#' show to copy paste the files from the subprotocol to the main protocol
#'
#' @param rmd_content Character vector containing lines read from an Rmarkdown
#' file
#' @param code_subprotocol Character string giving the protocol code from
#' which a section or sections will be extracted
#' @param tag A release tag to identify the version of the protocol in the git
#' repository
#'
#' @return Silent.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_extract_all
#' @importFrom fs path_rel
#' @importFrom purrr map pmap
#'
#' @keywords internal
get_data_media <- function(rmd_content, code_subprotocol, tag) {
  # dealing with external figures and tabular data
  # extract all paths to data or media
  pat_data <- "(data\\/\\w+\\.(csv|tsv|xls|xlsx))"
  data_files <- str_extract_all(rmd_content[grepl(pat_data, rmd_content)],
                                pat_data)
  pat_media <- "(media\\/\\w+\\.(png|jpg))"
  media_files <- str_extract_all(rmd_content[grepl(pat_media, rmd_content)],
                                 pat_media)
  all_files <- c(data_files, media_files)
  if (length(all_files) > 0) {
    git_filepaths <-
      get_path_to_protocol(code_subprotocol) %>%
      file.path(all_files) %>%
      path_rel(start = find_root(is_git_root))

    # use git show to get the contents of data and media
    # and copy it to the project protocol
    create_command <- function(file_path, dest_path, tag) {
      paste0("git show ",
             tag, ":",
             file_path, " > ",
             dest_path
      )
    }
    git_commands <- pmap(list(git_filepaths, all_files, tag), create_command)
    map(git_commands, execshell, intern = FALSE)
  }
}

#' @title Check the version number format
#'
#' @description
#' Check if version number is of format YYYY.NN
#'
#' @param version_number Character string with format YYYY.NN#'
#'
#' @importFrom assertthat assert_that is.string
#'
#' @keywords internal
check_versionnumber <- function(version_number) {
  assert_that(is.string(version_number))
  right_format <- grepl("[0-9]{4}\\.[0-9]{2}", version_number) # nolint: nonportable_path_linter, line_length_linter.
  assert_that(
    right_format,
    msg = "version number not in YYYY.XX format"
  )
}



#' @title Check the protocolcode format
#'
#' @description
#' Check if protocolcode is of format `s[f|p|i|o|a]p-###-[nl|en]`
#'
#' @param version_number Character string with format YYYY.NN#'
#'
#' @importFrom assertthat assert_that is.string
#'
#' @keywords internal
check_protocolcode <- function(protocolcode) {
  assert_that(is.string(protocolcode))
  right_format <- grepl("s[fpioa]p-[0-9]{3}-[nl|en]", protocolcode)
  assert_that(
    right_format,
    msg = "protocol code not in s*f-###-nl or s*f-###-en format"
  )
}


#' @title Mark line as part of chunk
#'
#' @description
#' The function will search for beginning and end of chunk and mark all lines
#' in between as TRUE, otherwise FALSE
#'
#' @return A logical vector
#'
#' @param rmd_content Character vector (each element is a line read from an
#' Rmarkdown file)
#'
#' @keywords internal
mark_chunks <- function(rmd_content) {
  # handling the section arguments
  # avoid looking in chunks which can have lines starting with '#'
  is_chunk <- grepl("^`{3}", rmd_content)
  i <- 1
  while (i < length(is_chunk)) {
    if (isTRUE(is_chunk[i]) && isTRUE(is_chunk[i + 1])) {
      i <- i + 2
    } else if (isTRUE(is_chunk[i]) && isFALSE(is_chunk[i + 1])) {
      is_chunk[i + 1] <- TRUE
      i <- i + 1
    } else {
      i <- i + 1
    }
  }
  return(is_chunk)
}
