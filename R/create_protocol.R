#' @title Create a folder with a bookdown (R markdown) template to start a new fieldwork protocol
#'
#' @description This function will create a new folder based on values that are passed on via the parameters and creates a R-markdown (bookdown) skeleton based on a template file to start working on a new protocol.
#'
#' @details The created folder is a new subfolder beneath `src` and contains a bookdown project. It is assumed that the src folder is a subfolder of an RStudio project with git version control.
#'
#' @param title A character string giving the main title of the protocol
#' @param subtitle A character string for an optional subtitle
#' @param short_title A character string of less than 20 characters to use in folder and filenames
#' @param authors A character string for authors of the form First name Last name and multiple authors separated by a comma
#' @param date A character string of the date in ISO 8601 format (YYYY-MM-DD)
#' @param reviewers A character string for reviewers of the form First name Last name and multiple authors separated by a comma
#' @param file_manager A character string for the name of the document maintainer of the form First name Last name
#' @param revision A semantic version number of the form major.minor.patch. For development versions a fourth component is appended starting from .9000. The default is 0.0.0.9000 and should normally not be changed.
#' @param theme A character string equal to one of `"generic"` (default),
#' `"water"`, `"air"`, `"soil"`, `"vegetation"` or `"species"`.
#' @param language Language of the protocol, either `"nl"` (Dutch), the default, or `"en"` (English).
#'
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_replace_all str_extract
#' @importFrom assertthat assert_that is.string is.date
#' @importFrom rmarkdown draft
#' @importFrom bookdown render_book
#' @importFrom purrr map_chr
#' @importFrom whisker whisker.render
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
  language = c("nl", "en")) {

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

  # create protocol name

  protocol_type <- "sfp"

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

  short_title <- tolower(short_title)
  short_title <- str_replace_all(short_title, " ", "-")
  short_titles <- get_short_titles(protocol_type = protocol_type)
  assert_that(!(short_title %in% short_titles),
              msg = "The given short title already exists. Give a short title that is not in use. Use get_short_titles() to get an overview of short titles that are in use.")

  protocol_code <- paste0(protocol_type, "-", protocol_number)
  folder_name <- paste0(protocol_code, "_", short_title)
  folder_name <- tolower(folder_name)
  protocol_filename <- paste0(folder_name, "_", language)

  # directory setup
  project_root <- find_root(is_git_root)
  path_to_protocol <- file.path(project_root, "src", "thematic", theme,
                                  folder_name)
  # set _bookdown.yml values
  book_filename <- paste0(protocol_filename, ".Rmd")
  output_dir <- file.path(project_root, "docs", "thematic", theme,
                          folder_name)

  # check for existence of the folders
  if (dir.exists(path_to_protocol)) {
    stop(sprintf(paste0("The protocol repository already has ",
                        "a folder %s!"), path_to_protocol))
  }
  if (dir.exists(output_dir)) {
    stop(sprintf(paste0("The protocol repository already has ",
                        "a folder %s!"), path_to_protocol))
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
          package = "protocolshelper",
          edit = FALSE)
  } else {
    draft(file = parent_rmd,
          template = "template_sfp_en",
          package = "protocolshelper",
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
  # render html
  setwd(path_to_protocol)
  render_book(input = "index.Rmd")
}




#' @title Function to list all occupied protocol numbers
#'
#' @description This function will search for protocol numbers in filenames of Rmarkdown files listed underneath the src folder. The search will be restricted to files of a given protocol type.
#'
#' @param protocol_type A character string equal to sfp (default), sip, sap or sop.
#'
#' @return A character vector with occupied protocol numbers for a specific protoocol type
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
get_protocolnumbers <- function(protocol_type = c("sfp", "sip", "sap", "sop")) {

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
#' @description This function will search for short titles in filenames of Rmarkdown files listed underneath the src folder. The search will be restricted to files of a given protocol type.

#' @param protocol_type A character string equal to sfp (default), sip, sap or sop.
#'
#' @return A character vector with short titles that are in use for a given protocol type.
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
get_short_titles <- function(protocol_type = c("sfp", "sip", "sap", "sop")) {

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
