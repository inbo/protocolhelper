#' @title Helper function to add one subprotocol to a project-specific protocol
#' of which it is a dependency
#'
#' @description The function renders the subprotocol to
#' [bookdown::markdown_document2()]
#' saves the resulting md file (and any associated files) to the directory of
#' the project-specific protocol.
#'
#' @return An r-chunk will be written that uses [knitr::knit_child()] to render
#' the md-file of the subprotocol as an appendix when the main protocol is
#' rendered.
#'
#' @param code_subprotocol Character string giving the protocol code from
#' which a subprotocol will be made (usually a sfp-type protocol)
#' @param version_number Character string with format YYYY.NN
#' @param params A list of parameter key-value pairs.
#' @param fetch_remote Whether or not to fetch the remote. Default TRUE.
#'
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom rprojroot find_root is_git_root
#' @importFrom fs path_rel
#' @importFrom purrr map map2
#' @importFrom bookdown render_book markdown_document2
#' @importFrom rmarkdown pandoc_variable_arg
#' @importFrom stringr str_extract
#' @importFrom knitr knit_child
#'
#'
#' @export
#'

add_one_subprotocol <-
  function(code_subprotocol,
           version_number,
           params = NULL,
           fetch_remote = TRUE) {

  assert_that(is.string(version_number))
  right_format <- grepl("[0-9]{4}\\.[0-9]{2}", version_number)
  assert_that(
    right_format,
    msg = "version number not in YYYY.XX format"
  )
  assert_that(is.string(code_subprotocol))
  right_format <- grepl("s[fpioa]p-[0-9]{3}-[nl|en]", code_subprotocol)
  assert_that(
    right_format,
    msg = "protocol code not in s*f-###-nl or s*f-###-en format"
  )

  if (!missing(params)) {
    # parse params
    params <- eval(str2lang(params))
    assert_that(is.list(params))
  }

  assert_that(is.flag(fetch_remote), noNA(fetch_remote))

  protocol_path_rel <-
    get_path_to_protocol(code_subprotocol) %>%
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

  #use git show to list the protocol files
  gitcommand <- paste0("git show ",
                       tag, ":",
                       protocol_path_rel)

  protocol_files <- execshell(gitcommand,
                           intern = TRUE)
  protocol_files <- protocol_files[3:length(protocol_files)]
  #git show to copy paste all files to a temp location
  z <- tempdir()
  if (length(protocol_files) > 0) {
    git_filepaths <-
      get_path_to_protocol(code_subprotocol) %>%
      file.path(protocol_files) %>%
      path_rel(start = find_root(is_git_root))

    create_command <- function(file_path, dest_path) {
      paste0("git show ",
             tag, ":",
             file_path, " > ",
             dest_path
      )
    }
    dest_paths <- file.path(z, protocol_files)
    git_commands <- map2(git_filepaths, dest_paths, create_command)
    map(git_commands, execshell, intern = FALSE)
  } else {
    stop("no protocol files found")
  }


  #render the protocol
  #with params and with bookdown::md_document2() as output format
  #and output to the main bookdown working directory
  mdfile <- paste0(code_subprotocol,"-", version_number, ".md")
  old_wd <- getwd()
  setwd(dir = z)
  if (length(params >= 1)) {
    render_book(input = "index.Rmd",
                output_format = markdown_document2(
                  pandoc_args = c(
                    pandoc_variable_arg("markdown-headings", "atx")
                  )
                ),
                output_dir = old_wd,
                output_file = mdfile,
                params = params,
                envir = new.env())
  } else {
    render_book(input = "index.Rmd",
                output_format = markdown_document2(
                  pandoc_args = c(
                    pandoc_variable_arg("markdown-headings", "atx")
                  )
                ),
                output_dir = old_wd,
                output_file = mdfile,
                envir = new.env())
  }
  # use mdfile as child
  knit_child(mdfile)
  setwd(old_wd)

}


#' @title Render all subprotocols
#'
#' @return Each subprotocol is an appendix chapter
#'
#' @param .dependencies a data.frame with columns version_number, params and
#' appendix.
#' @param ... additional parameters passed on to add_one_subprotocol
#'
#'
#' @export
add_subprotocols <-
  function(.dependencies,
           ...) {
  for (i in 1:nrow(.dependencies)) {
    if (.dependencies$appendix[i]) {
      add_one_subprotocol(.dependencies$protocol_code[i],
                          .dependencies$version_number[i],
                          .dependencies$params[i],
                          ...)
    }
  }
}
