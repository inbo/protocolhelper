#' @title Function to add a (section of) a published protocol as a subprotocol
#' to a standard project protocol.
#'
#' @description The idea is to execute this function in an R chunk with knitr
#' option \code{results="asis"}.
#'
#' @return The function will return Rmarkdown
#'
#' @param protocol_code Character string giving the protocol code
#' @param version_number Character string with format YYYY.NN
#' @param file_name Character string with the name of the Rmarkdown
#' file (a chapter starting with a level 1 heading).
#' @param section Optional character string with the name of a section within an
#' Rmarkdown file. If not specified (the default): the whole file is taken. It
#' is assumed that the section has a level 2 heading.
#' @param demote_header Number of '#' to prefix to all titles before inserting
#' in current protocol. Default is 0. A negative value can be used to remove
#' a '#' from all section titles
#' @param params A list of parameter name-value pairs in case you need to use
#' non-default values in parameterized protocols.
#'
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom fs path_rel
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_remove str_replace_all
#' @importFrom purrr map %>%
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' add_subprotocol(
#'   protocol_code = "sfp-401-nl",
#'   version_number = "2021.01",
#'   file_name = "07_stappenplan.Rmd",
#'   params = list(
#'      shape = "square",
#'      width = "3",
#'      height = "3"
#'   )
#' )
#'}
add_subprotocol <-
  function(protocol_code,
           version_number,
           file_name,
           section,
           demote_header = 0,
           params) {

    execshell <- function(commandstring, intern = FALSE) {
      if (.Platform$OS.type == "windows") {
        res <- shell(commandstring, intern = TRUE)
      } else {
        res <- system(commandstring, intern = TRUE)
      }
      if (!intern) cat(res, sep = "\n") else return(res)
    }

    assert_that(is.string(protocol_code))
    assert_that(is.string(version_number))
    wrong_format <- !grepl("[0-9]{4}\\.[0-9]{2}", version_number)
    if (wrong_format) {
      stop(
        "version number not in YYYY.XX format"
      )
    }
    wrong_format <- !grepl("s[fpioa]p-[0-9]{3}-[nl|en]", protocol_code)
    if (wrong_format) {
      stop(
        "protocol code not in s*f-###-nl or s*f-###-en format"
      )
    }
    assert_that(is.numeric(demote_header))
    demote_choices <- c(0, 1, 2, -1)
    if (!(demote_header %in% demote_choices)) {
      stop(paste0("demote header must be one of ", demote_choices))
    }
    if (!missing(params)) {
      assert_that(is.list(params))
    }
    assert_that(is.string(file_name))
    if (!missing(section)) {
      assert_that(is.string(section))
    }

    git_filepath <-
      get_path_to_protocol(protocol_code) %>%
      file.path(file_name) %>%
      path_rel(start = find_root(is_git_root))

    tag <- paste(protocol_code, version_number, sep = "-")
    gitcommand <- paste0("git show ",
                         tag, ":",
                         git_filepath)

    # #Fetching the git repo
    # # this usually returns "origin"
    # firstremote <- execshell("git remote", intern = TRUE)[1]
    # execshell(paste0("git fetch ", firstremote))

    # get the content of the Rmd file
    # this will return a character vector (each element is one sentence)
    rmd_content <- execshell(gitcommand,
           intern = TRUE)
    # What happens if this fails?
    # for instance because protocol_code and version_number don't match

    # handling the section arguments
    # avoid looking in chunks which can have lines starting with '#'
    is_chunk <- grepl("^`{3}", rmd_content)
    i <- 1
    while (i < length(is_chunk)) {
      if (isTRUE(is_chunk[i]) && isTRUE(is_chunk[i+1])) {
        i <- i + 2
      } else if (isTRUE(is_chunk[i]) && isFALSE(is_chunk[i+1])) {
        is_chunk[i+1] <- TRUE
        i <- i + 1
      } else {
        i <- i + 1
      }
    }

    if (!missing(section)) {
      has_section <- grepl(section, rmd_content, fixed = TRUE)

      # assuming section is header level 2
      h2 <- grepl("^##\\s[A-Z]", rmd_content) & !is_chunk
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
      h1 <- grepl("^#\\s[A-Z]", rmd_content) & !is_chunk
      h2 <- grepl("^##\\s[A-Z]", rmd_content) & !is_chunk
      h3 <- grepl("^###\\s[A-Z]", rmd_content) & !is_chunk
      if (demote_header == -1) {
        if(any(h1)) {
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


    # insert a yaml section with params name-value pairs
    if (!missing(params)) {
      pairs <- sprintf("  %s: %s", names(params), params)
      rmd_content <- c("---",
                       "params:",
                       pairs,
                       "---",
                       rmd_content)
    }

    # dealing with external figures and tabular data
    # extract all paths to data or media
    data_files <- grep("data\\/\\w+\\.(csv|tsv|xls|xlsx)", rmd_content)
    media_files <- grep("media\\/\\w+\\.(png|jpg)", rmd_content)
    all_files <- c(data_files, media_files)

    # use git show to get the contents of data and media?
    create_command <- function(path) {
      paste0("git show ",
             tag, ":",
             path)
    }
    git_commands <- map(all_files, create_command)
    all_files_content <- map(git_commands, execshell, intern = FALSE)

    # copy data and media
    map(all_files_content, file.create)

    # return rmd content
    return(rmd_content %>%
             paste0(collapse = "\n"))
  }



