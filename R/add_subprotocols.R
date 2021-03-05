#' @title Helper function to add one subprotocol to a project-specific protocol
#' of which it is a dependency
#'
#' @description The function renders the subprotocol to
#' [bookdown::markdown_document2()] and
#' saves the resulting md file (and any associated media and data files) in a
#' subfolder of the directory of the project-specific protocol.
#' This function should normally not be called directly.
#' Use [protocolhelper::add_subprotocols()] instead.
#'
#' @param code_subprotocol Character string giving the protocol code from
#' which a subprotocol will be made (usually a sfp-type protocol)
#' @param version_number Character string with format YYYY.NN
#' @param params A list of parameter key-value pairs.
#' @param code_mainprotocol Character string giving the protocol code for the
#' main protocol
#' @param fetch_remote Whether or not to fetch the remote. Default TRUE.
#'
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom rprojroot find_root is_git_root
#' @importFrom fs path_rel
#' @importFrom purrr map map2 map2_chr
#' @importFrom bookdown render_book markdown_document2
#' @importFrom stringr str_extract str_replace_all coll
#' @importFrom knitr knit_child
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom commonmark markdown_xml
#' @importFrom xml2 xml_attr xml_text read_xml xml_find_all
#'
#'
#' @export
#'

add_one_subprotocol <-
  function(code_subprotocol,
           version_number,
           params = NULL,
           code_mainprotocol,
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
    assert_that(is.string(code_mainprotocol))
    right_format <- grepl("s[fpioa]p-[0-9]{3}-[nl|en]", code_mainprotocol)
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

    mainprotocol_path_abs <- get_path_to_protocol(code_mainprotocol)

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

    #use git ls-tree to list the protocol files
    #-r for recursive listing
    #--name-only to only return relative path filenames
    gitcommand <- paste0("git ls-tree -r --name-only ",
                         tag, ":",
                         protocol_path_rel)

    protocol_files <- execshell(gitcommand,
                                intern = TRUE)
    #git show to copy paste all files to a subdir of main protocol location
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
      # use version_number as folder name instead of code_subprotocol
      # to avoid problems with get_path_to_protocol()
      dir.create(file.path(mainprotocol_path_abs,
                           version_number))
      dir.create(file.path(mainprotocol_path_abs,
                           version_number,
                           "data"))
      dir.create(file.path(mainprotocol_path_abs,
                           version_number,
                           "media"))
      dest_paths <- file.path(mainprotocol_path_abs, version_number,
                              protocol_files) %>%
        path_rel(start = find_root(is_git_root))
      git_commands <- map2(git_filepaths, dest_paths, create_command)
      map(git_commands, execshell, intern = FALSE)
    } else {
      stop("no protocol files found")
    }


    #render the protocol
    #with params and with bookdown::markdown_document2() as output format
    #and output to the main bookdown working directory
    mdfile <- paste0(code_subprotocol,"-", version_number, ".md")
    old_wd <- getwd()
    setwd(dir = file.path(mainprotocol_path_abs, version_number))
    yaml_sub <- yaml_front_matter("index.Rmd")
    if (length(params) >= 1) {
      render_book(input = "index.Rmd",
                  output_format = markdown_document2(
                    variant = "markdown",
                    number_sections = FALSE,
                    keep_md = TRUE,
                    pandoc_args = c(
                      "--atx-headers",
                      "--base-header-level=2"
                    )
                  ),
                  output_file = mdfile,
                  output_dir = ".",
                  params = params,
                  envir = new.env())
    } else {
      render_book(input = "index.Rmd",
                  output_format = markdown_document2(
                    variant = "markdown",
                    number_sections = FALSE,
                    keep_md = TRUE,
                    pandoc_args = c(
                      "--atx-headers",
                      "--base-header-level=2"
                    )
                  ),
                  output_file = mdfile,
                  output_dir = ".",
                  envir = new.env())
    }

    # post-processing
    # add title and replace paths media and data
    mdcontents <- readLines(mdfile, encoding = "UTF8")
    mdcontents <- str_replace_all(mdcontents,
                                  "\\.\\/media\\/",
                                  paste0("./", version_number, "/media/"))
    mdcontents <- str_replace_all(mdcontents,
                                  "\\.\\/data\\/",
                                  paste0("./", version_number, "/data/"))
    title <- paste0("# ", yaml_sub$title, "\n")
    mdcontents <- c(title, mdcontents)
    # alternative to pandoc arg --id-prefix which doesn't work for md output
    # to make sure main protocol has no duplicated identifiers
    xml <- markdown_xml(mdcontents)
    xml <- read_xml(xml)
    all_headings <- xml_find_all(xml, xpath = ".//d1:heading")
    heading_level <- xml_attr(all_headings, "level")
    heading_text <- xml_text(all_headings)
    add_atx <- function(level, text) {
      level <- as.double(level)
      atx <- paste(rep("#", level), collapse = "")
      paste(atx, text)
    }
    add_identifiers <- function(header_text, prepend) {
      autoidentifier <- !grepl("\\{\\#.+\\}", header_text)
      special_header <- grepl("\\(.+\\)", header_text)
      if (autoidentifier) {
        identifier <- tolower(header_text)
        identifier <- gsub("\\s", "-", identifier)
        if (special_header) {gsub("\\(.+\\)", "", identifier)}
        identifier <- paste0("{#", prepend, "-", identifier, "}")
        paste(header_text, identifier)
      } else {
        header_text <- sub("\\s\\{\\#.+\\}", "", header_text)
        identifier <- tolower(header_text)
        identifier <- gsub("\\s", "-", identifier)
        if (special_header) {gsub("\\(.+\\)", "", identifier)}
        identifier <- paste0("{#", prepend, "-", identifier, " .unnumbered}")
        paste(header_text, identifier)
      }
    }
    headings_modif <- map2_chr(heading_text, code_subprotocol, add_identifiers)
    headings_modif <- map2_chr(heading_level, headings_modif, add_atx)
    headings_orig <- map2_chr(heading_level, heading_text, add_atx)
    names(headings_modif) <- headings_orig
    md_string <- paste(mdcontents, collapse = "||")
    md_string <- str_replace_all(md_string, stringr::coll(headings_modif))
    mdcontents <- unlist(strsplit(md_string, split = "||", fixed = TRUE))
    writeLines(mdcontents, con = mdfile)
    # remove interim files
    rmd_files <- list.files(path = ".", pattern = "\\.Rmd$")
    yml_files <- list.files(path = ".", pattern = "\\.yml$")
    file.remove(rmd_files)
    file.remove(yml_files)

    setwd(old_wd)
  }


#' @title Render all subprotocols to single markdown files
#'
#' @description The function should be called in a separate R script.
#' For each subprotocol a single markdown file and associated media and
#' data files will be written.
#' Each subprotocol will be written to a subfolder of the main
#' protocol.
#' The subfolder name is the same as the version number of the
#' subprotocol.
#'
#' @param .dependencies a data.frame with columns version_number, params and
#' appendix. Each column corresponds with an argument of
#' [protocolhelper::add_one_subprotocol()]
#' @param ... additional parameters passed on to
#' [protocolhelper::add_one_subprotocol()].
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
