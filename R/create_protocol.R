#' @title Create a folder with a bookdown (R markdown) template to start a new
#' fieldwork protocol
#'
#' @description This function will create a new folder based on values that are
#' passed on via the parameters and creates a R-markdown (bookdown) skeleton
#' based on a template file to start working on a new protocol.
#'
#' @details The created folder is a new subfolder beneath `src` and contains a
#' bookdown project.
#' It is assumed that the src folder is a subfolder of an RStudio project with
#' git version control.
#'
#' @param title A character string giving the main title of the protocol
#' @param subtitle A character string for an optional subtitle
#' @param short_title A character string of less than 20 characters to use in
#' folder and filenames
#' @param authors A character string for authors of the form First name Last
#' name and multiple authors separated by a comma
#' @param date A character string of the date in ISO 8601 format (YYYY-MM-DD)
#' @param reviewers A character string for reviewers of the form First name
#' Last name and multiple authors separated by a comma
#' @param file_manager A character string for the name of the document
#' maintainer of the form First name Last name
#' @param revision A semantic version number of the form `major.minor.patch`.
#' For development versions a fourth component is appended starting from
#' `.9000`.
#' The default is `0.0.0.9000` and should normally only be changed if a
#' pre-existing protocol is used (See `from_docx`).
#' @param theme A character string equal to one of `"generic"` (default),
#' `"water"`, `"air"`, `"soil"`, `"vegetation"` or `"species"`.
#' @param language Language of the protocol, either `"nl"` (Dutch),
#' the default, or `"en"` (English).
#' @param from_docx A character string with the path (absolute or relative) to
#' a `.docx` file containing a pre-existing protocol.
#' Please make sure to copy-paste all relevant meta-data from the `.docx` file
#' to the corresponding parameters of this function.
#' If nothing is provided (i.e. default = NULL), an empty template will be used.
#' @param protocol_number A character string giving the protocol number.
#' This parameter should normally not be specified (i.e. NULL), unless
#' `from_docx` is specified.
#' A protocol number is a three digit string where the first digit corresponds
#' with a theme and the last two digits identify a protocol within a theme.
#' If NULL (the default), a protocol number will be determined automatically
#' based on pre-existing protocol numbers.
#' Protocol numbers that are already in use can be retrieved with
#' `get_protocolnumbers()`.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_replace_all str_extract str_remove str_detect
#' @importFrom assertthat assert_that is.string is.date
#' @importFrom rmarkdown draft
#' @importFrom bookdown render_book
#' @importFrom purrr map_chr
#' @importFrom whisker whisker.render
#' @importFrom fs path_rel
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_sfp()
#'}
create_sfp <- function(
  title,
  subtitle,
  short_title,
  authors,
  date = Sys.Date(),
  reviewers,
  file_manager,
  revision = "0.0.0.9000",
  theme = c("generic", "water", "air", "soil", "vegetation", "species"),
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL) {

  # check parameters
  assert_that(is.string(title))
  assert_that(is.string(subtitle))
  assert_that(is.string(short_title), nchar(short_title) <= 20)
  assert_that(is.date(as.Date(date)))
  assert_that(is.string(authors))
  assert_that(is.string(reviewers))
  assert_that(is.string(file_manager))
  assert_that(is.string(revision))
  theme <- match.arg(theme)
  language <- match.arg(language)
  if (!is.null(from_docx)) {
    assert_that(is.string(from_docx),
                file.exists(from_docx))
  }
  protocol_type <- "sfp"
  if (!is.null(protocol_number)) {
    assert_that(
      is.string(protocol_number),
      !(protocol_number %in% get_protocolnumbers(protocol_type = protocol_type))
      )
  }

  # create protocol name
  if (is.null(protocol_number)) {
    protocol_leading_number <- themes_df[themes_df$theme == theme,
                                         "theme_number"]

    all_numbers <- get_protocolnumbers(protocol_type = protocol_type)

    if (length(all_numbers) == 0) {
      protocol_trailing_number <- "01"
    } else {
      numbers <- str_subset(all_numbers, paste0("^", protocol_leading_number))
      protocol_trailing_number <- max(
        as.integer(
          str_extract(numbers, "\\d{2}$"))
      ) + 1
      protocol_trailing_number <- formatC(protocol_trailing_number,
                                          width = 2, format = "d", flag = "0")
    }

    protocol_number <- paste0(protocol_leading_number,
                              protocol_trailing_number)
  }

  short_title <- tolower(short_title)
  short_title <- str_replace_all(short_title, " ", "-")
  short_titles <- get_short_titles(protocol_type = protocol_type)
  assert_that(!(short_title %in% short_titles),
              msg = "The given short title already exists.
              Give a short title that is not in use.
              Use get_short_titles() to get an overview of short titles
              that are in use.")

  protocol_code <- paste0(protocol_type, "-", protocol_number)
  folder_name <- paste0(protocol_code, "_", short_title)
  folder_name <- tolower(folder_name)
  protocol_filename <- paste0(folder_name, "_", language)

  # directory setup
  project_root <- find_root(is_git_root)
  theme <- paste0(protocol_leading_number, "_", theme)
  path_to_protocol <- file.path(project_root, "src", "thematic", theme,
                                  folder_name)

  # set _bookdown.yml values
  book_filename <- paste0(protocol_filename, ".Rmd")
  # the output_dir should be set as a relative path to make it reproducible on
  # other machines: it should be relative to path_to_protocol
  # first get the absolute path
  output_dir <- file.path(project_root, "docs", "thematic", theme,
                          folder_name)
  # next make it relative to path_to_protocol
  output_dir <- path_rel(output_dir, path_to_protocol)


  # check for existence of the folders
  if (dir.exists(path_to_protocol)) {
    stop(sprintf(paste0("The protocol repository already has ",
                        "a folder %s!"), path_to_protocol))
  }
  if (dir.exists(output_dir)) {
    stop(sprintf(paste0("The protocol repository already has ",
                        "a folder %s!"), output_dir))
  }
  # create new directories
  dir.create(file.path(path_to_protocol),
               recursive = TRUE)
  dir.create(file.path(output_dir),
             recursive = TRUE)
  # create subfolders data and media
  dir.create(file.path(path_to_protocol, "data"))
  dir.create(file.path(path_to_protocol, "media"))

  # move all files from the template folder
  parent_rmd <- file.path(path_to_protocol, "index.Rmd")
  if (language == "nl") {
    draft(file = parent_rmd,
          template = "template_sfp_nl",
          package = "protocolhelper",
          edit = FALSE)
  } else {
    draft(file = parent_rmd,
          template = "template_sfp_en",
          package = "protocolhelper",
          edit = FALSE)
  }


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
                 revision = revision,
                 procedure = protocol_code,
                 theme = theme,
                 book_filename = book_filename,
                 output_dir = output_dir
    )
    writeLines(map_chr(original_file_content,
                       whisker.render,
                       data),
               filename)
    close(original_file)
  }

  # docx
  if (!is.null(from_docx)) {
    convert_docx_to_rmd(
      from = from_docx,
      to = book_filename,
      dir = path_to_protocol,
      wrap = 80,
      overwrite = FALSE,
      verbose = FALSE)
    # move relevant sections
    contents <- readLines(con = file.path(path_to_protocol,
                                          book_filename))
    is_title <- str_detect(string = contents, pattern = "^(#{1}\\s{1})")
    title_numbers <- formatC(x = cumsum(is_title),
                             width = 2, format = "d", flag = "0")
    filenames <- str_remove(string = tolower(contents[is_title]),
                            pattern = "^(#{1}\\s{1})")
    filenames <- str_remove(string = filenames,
                            pattern = "\\s$")
    filenames <- str_replace_all(filenames, pattern = "\\s", replacement = "_")
    filenames <- paste0(unique(title_numbers), "_", filenames, ".Rmd")
    # delete the empty template chapters
    file.remove(
      list.files(
        path = path_to_protocol,
        pattern = "^\\d{2}_",
        full.names = TRUE
      )
    )
    # create new chapters
    file.create(file.path(path_to_protocol, filenames))
    # and add chapter contents from docx
    for (chapter in unique(cumsum(is_title))) {
      chapter_file <- file.path(path_to_protocol, filenames[chapter])
      chapter_contents <- contents[chapter == cumsum(is_title)]
      writeLines(text = chapter_contents,
                 con = chapter_file)
    }
  }

  # render html
  old_wd <- getwd()
  setwd(path_to_protocol)
  render_book(input = "index.Rmd")
  setwd(old_wd)
}




#' @title Function to list all occupied protocol numbers
#'
#' @description This function will search for protocol numbers in filenames of
#' Rmarkdown files listed underneath the src folder.
#' The search will be restricted to files of a given protocol type.
#'
#' @param protocol_type A character string equal to sfp (default), sip, sap,
#' sop or spp.
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
  protocol_type = c("sfp", "sip", "sap", "sop", "spp")) {

  protocol_type <- match.arg(protocol_type)

  project_root <- find_root(is_git_root)
  path_to_src <- file.path(project_root, "src")
  lf <- list.files(path = path_to_src,
                   recursive = TRUE,
                   full.names = FALSE,
                   ignore.case = TRUE
  )
  lf <- str_subset(string = lf,
                   pattern = "^(.*)\\/(.*)(\\.Rmd)$")
  lf <- str_replace(string = lf,
                    pattern = "^(.*)\\/(.*)(\\.Rmd)$",
                    replacement = "\\2")
  lf <- str_subset(string = lf,
                   pattern = protocol_type)
  lf <- str_extract(string = lf,
                    pattern = "(?<=p-)\\d{3}")
  lf <- lf[!is.na(lf)]

  return(lf)
}


#' @title Function to list all short titles that are already in use.
#'
#' @description This function will search for short titles in filenames of
#' Rmarkdown files listed underneath the src folder.
#' The search will be restricted to files of a given protocol type.
#'
#' @param protocol_type A character string equal to sfp (default), sip, sap, sop
#' or spp.
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
  protocol_type = c("sfp", "sip", "sap", "sop", "spp")) {

  protocol_type <- match.arg(protocol_type)
  assert_that(is.string(protocol_type))

  project_root <- find_root(is_git_root)
  path_to_src <- file.path(project_root, "src")
  lf <- list.files(path = path_to_src,
                   recursive = TRUE,
                   full.names = FALSE,
                   ignore.case = TRUE
  )
  lf <- str_subset(string = lf,
                   pattern = "^(.*)\\/(.*)(\\.Rmd)$")
  lf <- str_replace(string = lf,
                    pattern = "^(.*)\\/(.*)(\\.Rmd)$",
                    replacement = "\\2")
  lf <- str_subset(string = lf,
                   pattern = protocol_type)
  lf <- str_extract(string = lf,
                    pattern = "(?<=\\d{3}_).*")
  lf <- lf[!is.na(lf)]

  return(lf)
}
