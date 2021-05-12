#' @title Create a folder with a bookdown (R markdown) template to start a new
#' protocol and optionally render to html
#'
#' @description This function will create a new folder based on values that are
#' passed on via the parameters and creates a R-markdown (bookdown) skeleton
#' based on a template file to start working on a new protocol.
#' Optionally, the rmarkdown chapters are rendered to an html file which will
#' be saved in a matching subfolder of the `docs` folder.
#'
#'
#' @details It is assumed that the `src` folder is a subfolder of an RStudio
#' project with git version control.
#' A target folder to which files will be written will be created as
#' subdirectories beneath `src`.
#' The subfolder structure is of the form
#' `/thematic/<theme>/<sfp>-<protocolnumber>-<language>_<short_title>/` for
#' standard field protocols.
#' Or `/project/<project_name>/<spp>-<protocolnumber>-<language>_<short_title>/`
#' for standard project protocols.
#' The folder names are determined by the corresponding arguments of the
#' function.
#' A matching subfolder structure will be created beneath the `docs` folder (and
#' output files needed for rendering to html output will be placed in it if
#' `render = TRUE`.
#' The template Rmarkdown files, or the Rmarkdown files that result from
#' converting a docx protocol (see `from_docx` argument), will be written to
#' the target folder beneath `src`.
#' Besides Rmarkdown files, this target folder will also contain files needed to
#' render to a Bookdown gitbook such as a `_bookdown.yml`.
#' The `NEWS.md` file must be used to document the changes between revisions
#' of the protocol.
#' Furthermore, a `data` and a `media` folder will be created as subdirectories
#' of the target folder.
#' The `media` folder can be used to store image files and will contain image
#' files extracted from the docx protocol when the `from_docx` argument is used.
#' The `data` folder can be used to store tabular data that are needed for the
#' protocol.
#'
#' @inheritParams create_protocol_code
#' @param title A character string giving the main title of the protocol
#' @param subtitle A character string for an optional subtitle
#' @param short_title A character string of less than 20 characters to use in
#' folder and filenames
#' @param authors A character vector for authors of the form First name Last
#' name
#' @param date A character string of the date in ISO 8601 format (YYYY-MM-DD)
#' @param reviewers A character vector for reviewers of the form First name
#' Last name
#' @param file_manager A character string for the name of the document
#' maintainer of the form First name Last name
#' @param version_number A version number of the form `YYYY.##`.
#' For development versions `.dev` is added.
#' The default is `paste0(format(Sys.Date(), "%Y"), ".00.dev")`
#' (See `from_docx`).
#' When the protocol is ready to be released, this should be changed by a repo-
#' admin.
#' @param project_name A character string that is used as the folder location
#' (`src/project/project_name`) where project-specific protocols that belong to
#' the same project will be stored. Preferably a short name or acronym. If the
#' folder does not exist, it will be created.
#' Ignored if protocol_type = `"sfp"`.
#' @param from_docx A character string with the path (absolute or relative) to
#' a `.docx` file containing a pre-existing protocol.
#' Please make sure to copy-paste all relevant meta-data from the `.docx` file
#' to the corresponding parameters of this function.
#' If nothing is provided (i.e. default = NULL), an empty template will be used.
#' @param render Whether or not to render the protocol to html.
#' Defaults to FALSE.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_replace_all
#' @importFrom assertthat assert_that is.string is.date is.flag noNA
#' @importFrom rmarkdown draft
#' @importFrom bookdown render_book
#' @importFrom purrr map_chr
#' @importFrom whisker whisker.render
#' @importFrom fs path_rel dir_create dir_ls
#'
#'
#' @export
create_protocol <- function(
  protocol_type = c("sfp", "spp"),
  title,
  subtitle,
  short_title,
  authors,
  date = Sys.Date(),
  reviewers,
  file_manager,
  version_number = paste0(format(Sys.Date(), "%Y"), ".00.dev"),
  theme = c("generic", "water", "air", "soil", "vegetation", "species"),
  project_name,
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  render = FALSE) {

  # check parameters
  protocol_type <- match.arg(protocol_type)
  assert_that(is.string(title))
  assert_that(is.string(subtitle))
  assert_that(is.string(short_title), nchar(short_title) <= 20)
  assert_that(is.date(as.Date(date)))
  assert_that(is.character(authors))
  assert_that(is.character(reviewers))
  assert_that(is.string(file_manager))
  check_versionnumber(gsub("\\.dev", "", version_number)) # nolint
  if (protocol_type == "sfp") {
    theme <- match.arg(theme)
    protocol_leading_number <- themes_df[themes_df$theme == theme,
                                         "theme_number"]
  }
  if (protocol_type == "spp") {
    assert_that(is.string(project_name))
  }
  language <- match.arg(language)
  if (!is.null(protocol_number)) {
    assert_that(
      is.string(protocol_number),
      !(protocol_number %in% get_protocolnumbers(protocol_type = protocol_type,
                                                 language = language))
      )
  }
  assert_that(is.flag(render), noNA(render))

  # create protocol code
  protocol_code <- create_protocol_code(
    protocol_type,
    theme,
    protocol_number,
    language
    )


  short_title <- tolower(short_title)
  short_title <- str_replace_all(short_title, " ", "-")
  short_titles <- get_short_titles(protocol_type = protocol_type,
                                   language = language)
  assert_that(!(short_title %in% short_titles),
              msg = "The given short title already exists.
              Give a short title that is not in use.
              Use get_short_titles() to get an overview of short titles
              that are in use.")
  folder_name <- paste0(protocol_code, "_", short_title)
  folder_name <- tolower(folder_name)
  protocol_filename <- folder_name
  # set _bookdown.yml values
  book_filename <- paste0(protocol_filename, ".Rmd")
  # the output_dir should be set as a relative path to make it reproducible on
  # other machines: it should be relative to path_to_protocol
  # first get the absolute path
  project_root <- find_root(is_git_root)

  # directory setup
  if (protocol_type == "sfp") {
    path_to_protocol <- get_path_to_protocol(theme = theme,
                                             protocol_code = protocol_code,
                                             short_title = short_title)
    nr_theme <- paste0(protocol_leading_number, "_", theme)
    output_dir <- file.path(project_root, "docs", "thematic", nr_theme,
                            folder_name)
  }
  if (protocol_type == "spp") {
    path_to_protocol <- get_path_to_protocol(project_name = project_name,
                                             protocol_code = protocol_code,
                                             short_title = short_title)
    output_dir <- file.path(project_root, "docs", "project", project_name,
                            folder_name)
  }

  # next make it relative to path_to_protocol
  output_dir_rel <- path_rel(output_dir, path_to_protocol)

  # check for existence of non-empty folders
  if (dir.exists(path_to_protocol) &&
      !identical(
        unname(
          unclass(
            dir_ls(
              path_to_protocol,
              type = file)
          )
        ),
        character(0)
      )
  ) {
    stop(sprintf(paste0("The protocol repository already has ",
                        "a folder %s!"), path_to_protocol))
  }
  if (dir.exists(output_dir) &&
      !identical(
        unname(
          unclass(
            dir_ls(
              output_dir,
              type = file)
          )
        ),
        character(0)
      )
  ) {
    stop(sprintf(paste0("The protocol repository already has ",
                        "a folder %s!"), output_dir))
  }
  # create new directories
  dir_create(file.path(path_to_protocol),
             recursive = TRUE)
  dir_create(file.path(output_dir),
             recursive = TRUE)
  # create subfolders data and media
  dir_create(file.path(path_to_protocol, "data"))
  dir_create(file.path(path_to_protocol, "media"))

  # create from empty template
  if (is.null(from_docx)) {
    # move all files from the template folder
    parent_rmd <- file.path(path_to_protocol, "index.Rmd")
    template <- paste("template", protocol_type, language, sep = "_")
    draft(file = parent_rmd,
          template = template,
          package = "protocolhelper",
          edit = FALSE)

    # create a character vector with the names of all rmd_files
    # in correct order for compilation
    rmd_files <- c(
      "index.Rmd",
      "NEWS.md",
      list.files(path = path_to_protocol,
                 pattern = "^\\d{2}.+Rmd$"))


    # change values in parent rmarkdown and _bookdown.yml
    filenames <- c(parent_rmd,
                   file.path(path_to_protocol, "_bookdown.yml"))
    for (filename in filenames) {
      original_file <- file(filename, "r")
      original_file_content <- readLines(filename)
      data <- list(title = title,
                   subtitle = subtitle,
                   authors = authors,
                   date = date,
                   reviewers = reviewers,
                   file_manager = file_manager,
                   version_number = version_number,
                   protocol_code = protocol_code,
                   language = language,
                   book_filename = book_filename,
                   output_dir = output_dir_rel,
                   rmd_files = rmd_files
      )
      if (protocol_type == "sfp") {
        data$theme <- theme
      }
      if (protocol_type == "spp") {
        data$project_name <- project_name
      }
      writeLines(map_chr(original_file_content,
                         whisker.render,
                         data),
                 filename)
      close(original_file)
    }
  } else {
    assert_that(is.string(from_docx),
                file.exists(from_docx))
    create_from_docx(from_docx = from_docx,
                     path_to_protocol = path_to_protocol)
  }

    # render html
  if (render) {
    render_protocol(protocol_code = protocol_code)
  }
  # return a message
  if (!is.null(from_docx)) {
    message(ifelse(
      render,
      "Rendering may fail if Rmarkdown files do not correspond with
      those listed in the rmd_files field in the _bookdown.yml file.",
      "Please check if the names of the Rmarkdown files comply with
      those listed in the rmd_files field in the _bookdown.yml file.")
    )
  }
}


#' @rdname create_protocol
#' @export

create_sfp <- function(
  title,
  subtitle,
  short_title,
  authors,
  date = Sys.Date(),
  reviewers,
  file_manager,
  version_number = paste0(format(Sys.Date(), "%Y"), ".00.dev"),
  theme = c("generic", "water", "air", "soil", "vegetation", "species"),
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  render = FALSE) {
  create_protocol(protocol_type = "sfp",
                  title = title,
                  subtitle = subtitle,
                  short_title = short_title,
                  authors = authors,
                  date = date,
                  reviewers = reviewers,
                  file_manager = file_manager,
                  version_number = version_number,
                  theme = theme,
                  language = language,
                  from_docx = from_docx,
                  protocol_number = protocol_number,
                  render = render)
}

#' @rdname create_protocol
#' @export
create_spp <- function(
  title,
  subtitle,
  short_title,
  authors,
  date = Sys.Date(),
  reviewers,
  file_manager,
  version_number = paste0(format(Sys.Date(), "%Y"), ".00.dev"),
  project_name,
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  render = FALSE) {
  create_protocol(protocol_type = "spp",
                  title = title,
                  subtitle = subtitle,
                  short_title = short_title,
                  authors = authors,
                  date = date,
                  reviewers = reviewers,
                  file_manager = file_manager,
                  version_number = version_number,
                  project_name = project_name,
                  language = language,
                  from_docx = from_docx,
                  protocol_number = protocol_number,
                  render = render)
}



#' @title Function to list all occupied protocol numbers
#'
#' @description This function will search for protocol numbers in filenames of
#' Rmarkdown files listed underneath the src folder.
#' The search will be restricted to files of a given protocol type and given
#' language.
#'
#' @param protocol_type A character string equal to sfp (default), sip, sap,
#' sop or spp.
#' @param language Language of the protocol, either `"nl"` (Dutch),
#' the default, or `"en"` (English).
#'
#'
#' @return A character vector with occupied protocol numbers for a specific
#' protoocol type
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_subset str_replace str_extract
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_protocolnumbers()
#'}
get_protocolnumbers <- function(
  protocol_type = c("sfp", "sip", "sap", "sop", "spp"),
  language = c("nl", "en")) {

  protocol_type <- match.arg(protocol_type)
  language <- match.arg(language)

  project_root <- find_root(is_git_root)
  path_to_src <- file.path(project_root, "src")
  ld <- list.dirs(path = path_to_src,
                   recursive = TRUE,
                   full.names = FALSE
  )
  ld <- str_subset(string = ld,
                   pattern = protocol_type)
  ld <- str_subset(string = ld,
                   pattern = paste0("-", language, "_"))
  ld <- str_extract(string = ld,
                    pattern = "(?<=p-)\\d{3}")
  ld <- ld[!is.na(ld)]
  ld <- unique(ld)

  return(ld)
}


#' @title Function to list all short titles that are already in use.
#'
#' @description This function will search for short titles in filenames of
#' Rmarkdown files listed underneath the src folder.
#' The search will be restricted to files of a given protocol type and given
#' language.
#'
#' @param protocol_type A character string equal to sfp (default), sip, sap, sop
#' or spp.
#' @param language Language of the protocol, either `"nl"` (Dutch),
#' the default, or `"en"` (English).
#'
#' @return A character vector with short titles that are in use for a given
#' protocol type.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom assertthat assert_that is.string
#' @importFrom stringr str_subset str_extract str_replace
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_short_titles()
#'}
get_short_titles <- function(
  protocol_type = c("sfp", "sip", "sap", "sop", "spp"),
  language = c("nl", "en")) {

  protocol_type <- match.arg(protocol_type)
  language <- match.arg(language)

  project_root <- find_root(is_git_root)
  path_to_src <- file.path(project_root, "src")
  ld <- list.dirs(path = path_to_src,
                   recursive = TRUE,
                   full.names = FALSE
  )
  ld <- str_subset(string = ld,
                   pattern = protocol_type)
  ld <- str_subset(string = ld,
                   pattern = paste0("(", language, ")$"))
  ld <- str_extract(string = ld,
                    pattern = "(?<=\\d{3}_)([a-z]|-)*")
  ld <- ld[!is.na(ld)]

  return(ld)
}

#' @title Internal function to get (or set) the full path to a protocol.
#'
#' Either provide the full path of an existing, given folder name,
#' or construct one for non-existing protocols using (new) folder name plus
#' the theme or project name.
#'
#' For existing protocol folders, arguments `theme` and `project_name` are
#' always ignored.
#'
#' To create a new protocol folder,
#' also either the `theme` or the `project_name` argument are required apart
#' from the `protocol_folder_name`.
#'
#' @param protocol_code Character string giving the protocol code
#' @param theme A character string equal to one of `"generic"`,
#' `"water"`, `"air"`, `"soil"`, `"vegetation"` or `"species"`.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists and
#' the request is for a thematic protocol.
#' @param project_name Character string giving the name of the project folder.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists and
#' the request is for a project-specific protocol.
#' @param short_title A character string of less than 20 characters to use in
#' folder and filenames.
#' Defaults to NULL.
#' Only needed if no folder with the name of the protocol code exists.
#'
#' @return A character vector containing the full path to the protocol.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom assertthat assert_that is.string
#' @importFrom stringr str_subset
#'
#' @keywords internal
#'
#'
#' @examples
#' \dontrun{
#' get_path_to_protocol(protocol_code = "sfp-401-nl")
#'}
get_path_to_protocol <- function(protocol_code,
                                 theme = NULL,
                                 project_name = NULL,
                                 short_title = NULL) {
  assert_that(is.string(protocol_code))

  # first case: the path exists already
  project_root <- find_root(is_git_root)
  ld <- list.dirs(path = file.path(project_root, "src"),
                  full.names = TRUE,
                  recursive = TRUE)
  ld <- str_subset(string = ld,
                   pattern = protocol_code)
  if (!identical(ld, character(0))) {
    path_to_protocol <- ld[[1]]
    return(path_to_protocol)
  } else {
    # second case: the path does not yet exist
    if (is.null(theme) & is.null(project_name) |
        is.string(theme) & is.string(project_name)) {
      stop(
        paste0("Check the spelling of protocol_code - or - provide ",
        "a string value for theme or project, not both.")
        )
    }

    if (is.null(short_title)) {
      stop("Provide a short title")
    } else {
      protocol_folder_name <- paste(protocol_code, short_title, sep = "_")
    }

    if (is.string(theme)) {
      subfolder_of <- "thematic"
      protocol_leading_number <- themes_df[themes_df$theme == theme,
                                           "theme_number"]
      theme <- paste0(protocol_leading_number, "_", theme)
      path_to_protocol <- file.path(project_root,
                                    "src",
                                    subfolder_of,
                                    theme,
                                    protocol_folder_name)
      return(path_to_protocol)
    } else {
      subfolder_of <- "project"
      path_to_protocol <- file.path(project_root,
                                    "src",
                                    subfolder_of,
                                    project_name,
                                    protocol_folder_name)
      return(path_to_protocol)
    }
  }
}



#' Create protocol code from it's components
#'
#' A protocol code of format `s[f|p]p-###-[nl|en]` will be created.
#' The number will be determined automatically based on theme (in case of sfp)
#' and a rank order of all existing protocol numbers
#'
#' @param protocol_type Either `sfp` (standard field protocol) or `spp` (
#' standard project protocol)
#' @param theme A character string equal to one of `"generic"` (default),
#' `"water"`, `"air"`, `"soil"`, `"vegetation"` or `"species"`. It is used as
#' the folder location (`src/thematic/theme`) where standard field protocols
#' that belong to the same theme will be stored.
#' Ignored if protocol_type = `"spp"`.
#' @param protocol_number A character string giving the protocol number.
#' This parameter should normally not be specified (i.e. NULL), unless
#' `from_docx` is specified.
#' A protocol number is a three digit string where the first digit corresponds
#' with a theme and the last two digits identify a protocol within a theme for
#' standard field protocols. A protocol number for a project-specific protocol
#' is just a three digit string.
#' If NULL (the default), a protocol number will be determined automatically
#' based on pre-existing protocol numbers.
#' Protocol numbers that are already in use can be retrieved with
#' `get_protocolnumbers()`.
#' @param language Language of the protocol, either `"nl"` (Dutch),
#' the default, or `"en"` (English).
#'
#' @importFrom stringr str_subset str_extract
#'
#' @return A character string containing the protocol_code
#'
#' @keywords internal
create_protocol_code <- function(
  protocol_type, theme, protocol_number, language
  ) {
  if (protocol_type == "sfp") {
    protocol_leading_number <- themes_df[themes_df$theme == theme,
                                         "theme_number"]
    if (is.null(protocol_number)) {
      all_numbers <- get_protocolnumbers(protocol_type = protocol_type,
                                         language = language)

      if (length(all_numbers) == 0) {
        protocol_trailing_number <- "01"
      } else {
        numbers <- str_subset(all_numbers, paste0("^", protocol_leading_number))
        protocol_trailing_number <- max(
          as.integer(
            str_extract(numbers, "\\d{2}$")),
          0,
          na.rm = TRUE
        ) + 1
        protocol_trailing_number <- formatC(protocol_trailing_number,
                                            width = 2, format = "d", flag = "0")
      }

      protocol_number <- paste0(protocol_leading_number,
                                protocol_trailing_number)
    }
  }
  if (protocol_type == "spp") {
    if (is.null(protocol_number)) {
      all_numbers <- get_protocolnumbers(protocol_type = protocol_type,
                                         language = language)
      if (length(all_numbers) == 0) {
        protocol_number <- "001"
      } else {
        protocol_number <- max(
          as.integer(all_numbers),
          0,
          na.rm = TRUE
        ) + 1
        protocol_number <- formatC(protocol_number,
                                   width = 3, format = "d", flag = "0")
      }
    }
  }
  protocol_code <- paste(protocol_type, protocol_number, language, sep = "-")
  return(protocol_code)
}



#' Create an Rmarkdown version from an existing docx protocol
#'
#' The docx file is first converted to a single Rmd file with the aid of pandoc
#' (called from convert_docx_to_rmd).
#' Next, the file is split by chapter in multiple Rmd files.
#' Any emf images are converted to png.
#' All graphics files will be stored in a ./media folder.
#'
#' @param from_docx A character string with the path (absolute or relative) to
#' a `.docx` file containing a pre-existing protocol.
#' Please make sure to copy-paste all relevant meta-data from the `.docx` file
#' to the corresponding parameters of this function.
#' If nothing is provided (i.e. default = NULL), an empty template will be used.
#' @param path_to_protocol Absolute path to the protocol folder where the
#' protocol created from docx needs to be written to
#'
#' @importFrom stringr str_replace str_replace_all str_detect str_remove
#'
#' @keywords internal
create_from_docx <- function(
  from_docx,
  path_to_protocol) {
  temp_filename <- "temp.Rmd"
  convert_docx_to_rmd(
    from = from_docx,
    to = temp_filename,
    dir = path_to_protocol,
    wrap = 80,
    overwrite = FALSE,
    verbose = FALSE)
  # convert emf to png
  emf_images <- list.files(path = file.path(path_to_protocol, "media"),
                           pattern = ".emf",
                           full.names = TRUE)
  if (length(emf_images) > 0) {
    if (!requireNamespace("magick", quietly = TRUE)) {
      stop("Package \"magick\" needed for docx protocols with emf images. ",
           "Please install it with 'install.packages(\"magick\")'.",
           call. = FALSE)
    }
    for (img in emf_images) {
      img_emf <- magick::image_read(path = img)
      magick::image_write(image = img_emf,
                          format = "png",
                          path = str_replace(img, ".emf", ".png"))
      file.remove(img)
    }
  }
  # move relevant sections
  contents <- readLines(con = file.path(path_to_protocol,
                                        temp_filename))
  contents <- str_replace_all(contents, ".emf", ".png")
  # replace absolute path to media folder by relative path
  contents <- str_replace_all(contents, path_to_protocol, ".")
  is_title <- str_detect(string = contents, pattern = "^(#{1}\\s{1})")
  title_numbers <- formatC(x = cumsum(is_title),
                           width = 2, format = "d", flag = "0")
  filenames <- str_remove(string = tolower(contents[is_title]),
                          pattern = "^(#{1}\\s{1})")
  filenames <- str_remove(string = filenames,
                          pattern = "\\s$")
  filenames <- str_replace_all(filenames, pattern = "\\s", replacement = "_")
  filenames <- paste0(unique(title_numbers), "_", filenames, ".Rmd")
  # create new chapters
  file.create(file.path(path_to_protocol, filenames))
  # and add chapter contents from docx
  for (chapter in unique(cumsum(is_title))) {
    chapter_file <- file.path(path_to_protocol, filenames[chapter])
    chapter_contents <- contents[chapter == cumsum(is_title)]
    writeLines(text = chapter_contents,
               con = chapter_file)
  }
  # delete the complete Rmd (output of convert_docx_rmd)
  file.remove(file.path(path_to_protocol, temp_filename))
}
