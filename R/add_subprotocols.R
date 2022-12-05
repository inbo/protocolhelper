#' @title Helper function to add one sub-protocol to a project-specific protocol
#' of which it is a dependency
#'
#' @description The function renders the sub-protocol to
#' [bookdown::markdown_document2()] and
#' saves the resulting `md` file (and any associated media and data files) in a
#' subfolder of the directory of the project-specific protocol.
#' This function should normally not be called directly.
#' Use [protocolhelper::add_subprotocols()] instead.
#'
#' @param code_subprotocol Character string giving the protocol code from
#' which a sub-protocol will be made (usually a `sfp`-type protocol)
#' @param version_number Character string with format `YYYY.NN`
#' @param params2 A list of parameter key-value pairs.
#' @inheritParams add_subprotocols
#'
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom rprojroot find_root is_git_root
#' @importFrom fs path_rel path_dir dir_create
#' @importFrom purrr map map2_chr pmap
#' @importFrom bookdown render_book markdown_document2
#' @importFrom stringr str_extract str_replace_all coll
#' @importFrom knitr knit_child
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom commonmark markdown_xml
#' @importFrom xml2 xml_attr xml_text read_xml xml_find_all
#' @importFrom ymlthis as_yml yml_replace use_index_rmd
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   add_subprotocols(code_mainprotocol = 'spp-999-en')
#' }

add_one_subprotocol <-
  function(code_subprotocol,
           version_number,
           params2 = NULL,
           code_mainprotocol,
           fetch_remote = TRUE) {

    check_versionnumber(version_number)
    check_protocolcode(code_subprotocol)
    check_protocolcode(code_mainprotocol)

    if (!missing(params2)) {
      assert_that(is.list(params2) | is.na(params2))
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
      subdirs <- path_dir(protocol_files)
      subdirs <- subdirs[grepl("\\w+", subdirs)]
      git_filepaths <-
        get_path_to_protocol(code_subprotocol) %>%
        file.path(protocol_files) %>%
        path_rel(start = find_root(is_git_root))

      create_command <- function(file_path, dest_path, tag) {
        paste0("git show ",
               tag, ":",
               file_path, " > ",
               dest_path
        )
      }
      # use version_number as folder name instead of code_subprotocol
      # to avoid problems with get_path_to_protocol()
      dir_create(file.path(mainprotocol_path_abs,
                           version_number))
      dir_create(file.path(mainprotocol_path_abs,
                           version_number,
                           subdirs),
                 recurse = TRUE)
      dest_paths <- file.path(mainprotocol_path_abs, version_number,
                              protocol_files) %>%
        path_rel(start = find_root(is_git_root))
      git_commands <- pmap(list(git_filepaths, dest_paths, tag), create_command)
      map(git_commands, execshell, intern = FALSE)
      map(dest_paths[grepl("Rmd$", dest_paths)], fence_all_chunks)
    } else {
      stop("no protocol files found")
    }


    #render the protocol
    #with params and with bookdown::markdown_document2() as output format
    #and output to the main bookdown working directory
    mdfile <- paste0(code_subprotocol, "-", version_number, ".md")
    old_wd <- getwd()
    setwd(dir = file.path(mainprotocol_path_abs, version_number))
    yaml_sub <- yaml_front_matter("index.Rmd")
    if (!is.na(params2)) {
      render_book(input = "index.Rmd",
                  output_format = markdown_document2(
                    variant = "markdown",
                    number_sections = FALSE,
                    keep_md = TRUE,
                    pandoc_args = c(
                      "--markdown-headings=atx",
                      "--shift-heading-level-by=1",
                      "--metadata=suppress-bibliography[:TRUE]"
                    )
                  ),
                  output_file = mdfile,
                  output_dir = ".",
                  params = params2,
                  envir = new.env())
    } else {
      render_book(input = "index.Rmd",
                  output_format = markdown_document2(
                    variant = "markdown",
                    number_sections = FALSE,
                    keep_md = TRUE,
                    pandoc_args = c(
                      "--markdown-headings=atx",
                      "--shift-heading-level-by=1",
                      "--metadata=suppress-bibliography[:TRUE]"
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
                                  "media\\/",
                                  paste0(version_number, "/media/"))
    mdcontents <- str_replace_all(mdcontents,
                                  "data\\/",
                                  paste0(version_number, "/data/"))
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
        identifier <- gsub("[[:punct:]]", "", identifier)
        identifier <- gsub("\\s", "-", identifier)
        if (special_header) {
          # remove (APPENDIX) or (PART)
          identifier <- gsub("\\(.+\\)", "", identifier)
          header_text <- gsub("\\(.+\\)", "", header_text)
        }
        identifier <- paste0("{#", prepend, "-", identifier, "}")
        paste(header_text, identifier)
      } else {
        # deal with sub-sub protocols
        # which already have protocolcode prepended
        if (grepl("\\s\\{\\#s[fpiao]p.+\\}", header_text)) {
          header_text #keep as is
        } else {
          header_text <- sub("\\s\\{\\#.+\\}", "", header_text)
          identifier <- tolower(header_text)
          identifier <- gsub("[[:punct:]]", "", identifier)
          identifier <- gsub("\\s", "-", identifier)
          if (special_header) {
            # remove (APPENDIX) or (PART)
            identifier <- gsub("\\(.+\\)", "", identifier)
            header_text <- gsub("\\(.+\\)", "", header_text)
          }
          identifier <- paste0("{#", prepend, "-", identifier, " .unnumbered}")
          paste(header_text, identifier)
        }
      }
    }
    headings_modif <- map2_chr(heading_text, code_subprotocol, add_identifiers)
    headings_modif <- map2_chr(heading_level, headings_modif, add_atx)
    headings_modif <- map2_chr("||", headings_modif, paste, sep = "")
    headings_modif <- map2_chr(headings_modif, "||", paste, sep = "")
    headings_orig <- map2_chr(heading_level, heading_text, add_atx)
    headings_orig <- map2_chr("||", headings_orig, paste, sep = "")
    headings_orig <- map2_chr(headings_orig, "||", paste, sep = "")
    names(headings_modif) <- headings_orig
    md_string <- paste(mdcontents, collapse = "||")
    md_string <- str_replace_all(md_string, stringr::coll(headings_modif))
    mdcontents <- unlist(strsplit(md_string, split = "||", fixed = TRUE))
    writeLines(mdcontents, con = mdfile)
    # remove interim files
    rmd_files <- list.files(path = ".", pattern = "\\.Rmd$")
    yml_files <- list.files(path = ".", pattern = "\\.yml$")
    yml_files <- yml_files[!yml_files %in% yaml_sub$bibliography]
    file.remove(rmd_files)
    file.remove(yml_files)

    setwd(old_wd)

    # add reference files to bibliography key in main protocol
    old_wd <- setwd(dir = mainprotocol_path_abs)
    yaml_main <- yaml_front_matter("index.Rmd")
    unlink("css", recursive = TRUE)
    yaml_main <- as_yml(yaml_main)
    yaml_sub <- as_yml(yaml_sub)
    bibliographies <- unique(
      c(
        yaml_main$bibliography,
        paste0(version_number, "/", yaml_sub$bibliography)
      )
    )
    yaml_main <- yml_replace(yaml_main, bibliography = bibliographies)
    # overwrite old yaml sections
    template_rmd <- file.path(mainprotocol_path_abs, "template.rmd")
    file.copy(from = "index.Rmd", to = template_rmd)
    unlink("index.Rmd")
    use_index_rmd(
      .yml = yaml_main,
      path = mainprotocol_path_abs,
      template = template_rmd,
      include_body = TRUE,
      include_yaml = FALSE,
      quiet = TRUE,
      open_doc = FALSE)
    unlink(template_rmd)

    setwd(old_wd)
  }


#' @title Render all sub-protocols belonging to a main protocol to single
#' markdown files
#'
#' @description The function should be called interactively (in the console)
#' after the dependencies section in the `YAML` header of the `index.Rmd` file
#' of the main protocol has been filled in with the aid of the
#' `protocolhelper::add_dependencies()` function.
#' For reproducibility, it is good practice to save the call in a
#' separate R script.
#' For each sub-protocol a single markdown file and associated media and
#' data files will be written.
#' Each sub-protocol will be written to a subfolder of the main
#' protocol.
#' The subfolder name is the same as the version number of the
#' sub-protocol.
#'
#' @param code_mainprotocol Character string giving the protocol code for the
#' main protocol
#' @param fetch_remote Whether or not to fetch the remote. Default TRUE.
#'
#' @importFrom assertthat assert_that is.string is.flag noNA
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom purrr map map_lgl
#'
#' @export
add_subprotocols <-
  function(code_mainprotocol,
           fetch_remote = TRUE) {

    check_protocolcode(code_mainprotocol)

    assert_that(is.flag(fetch_remote), noNA(fetch_remote))

    mainprotocol_path_abs <- get_path_to_protocol(code_mainprotocol)

    yml <- yaml_front_matter(file.path(mainprotocol_path_abs, "index.Rmd"))
    unlink("css", recursive = TRUE)

    app <- map(yml$params$dependencies$value, "appendix")
    app_logical <- map_lgl(app, is.logical)
    app_notna <- map_lgl(app, noNA)
    assert_that(
      all(app_logical),
      all(app_notna)
    )
    # note: right format assertions are done in add_one_subprotocol()
    pc <- map(yml$params$dependencies$value, "protocol_code")
    pc_string <- map_lgl(pc, is.string)
    assert_that(all(pc_string))

    vn <- map(yml$params$dependencies$value, "version_number")
    vn_string <- map_lgl(vn, is.string)
    assert_that(all(vn_string))

    prm <- map(yml$params$dependencies$value, "params")
    prm_list <- map_lgl(prm, is.list)
    prm_na <- map_lgl(prm, is.na)
    assert_that(all(prm_list | prm_na))

    for (i in seq_len(length(yml$params$dependencies$value))) {
      if (yml$params$dependencies$value[[i]]$appendix) {
        add_one_subprotocol(
          code_subprotocol = yml$params$dependencies$value[[i]]$protocol_code,
          version_number = yml$params$dependencies$value[[i]]$version_number,
          params2 = yml$params$dependencies$value[[i]]$params,
          code_mainprotocol = code_mainprotocol,
          fetch_remote = fetch_remote)
      }
    }
  }
