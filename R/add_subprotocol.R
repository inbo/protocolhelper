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
#' file
#' @param section Optional character string with the name of a section within an
#' Rmarkdown file. If not specified (the default): the whole file is taken.
#' @param demote_header Number of '#' to prefix to all titles before inserting
#' in current protocol. Default is 0. A negative value can be used to remove
#' a '#' from all section titles
#' @param params A list of parameter value pairs in case of parameterized
#' protocols.
#'
#' @details
#'
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>%
#' @importFrom fs path_rel
#' @importFrom rprojroot find_root is_git_root
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
           demote_header = 0L,
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
    if (!is.integer(demote_header)) {
      demote_header <- as.integer(demote_header)
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

    tag <- paste(protocol_code, version_number, "-")
    gitcommand <- paste0("git show ",
                         tag, ":",
                         git_filepath)

    #Fetching the git repo
    # this usually returns "origin"
    firstremote <- execshell("git remote", intern = TRUE)[1]
    execshell(paste0("git fetch ", firstremote))

    # get the content of the Rmd file
    rmd_content <- execshell(gitcommand,
           intern = TRUE) %>%
      paste0(collapse = "\n")
    # What happens if this fails?
    # for instance because protocol_code and version_number don't match

    # handling the section and demote arguments


    # change params$... by the values supplied in params list
    if (!missing(params)) {
      for (i in 1:length(params)) {
        rmd_content <- str_replace_all(
          rmd_content,
          paste0("params$", names(params)[i]),
          params[[i]]
        )
      }
    }

    # dealing with external figures and tabular data
    # extract all paths to data or media

    # use git show to get the contents of data and media?

    # copy data and media

    # return rmd content
    return(rmd_content)
  }



