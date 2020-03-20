#' @title Create a folder with a markdown template to start working on a new fieldwork protocol
#'
#' @description This function will create a new folder based on values that are passed on via the parameters and creates a R-markdown skeleton based on a template file to start working on a new protocol.
#'
#' @param title A character string giving the main title of the protocol
#' @param subtitle A character string for an optional subtitle
#' @param short_title A character string of less than 20 characters to use in folder and filenames
#' @param authors A character string for authors of the form First name Last name and multiple authors separated by a comma
#' @param date A character string of the date in ISO 8601 format (YYYY-MM-DD)
#' @param reviewers A character string for reviewers of the form First name Last name and multiple authors separated by a comma
#' @param file_manager A character string for the name of the document maintainer of the form First name Last name
#' @param revision A semantic version number of the form major.minor.patch. For development versions a fourth component is appended starting from .9000. The default is 0.0.0.9000 and should normally not be changed.
#' @param projectname A character string giving the project name. Best to use an acronym for this.
#' @param theme A character string equal to one of generiek (default), water, lucht, bodem, vegetatie or soorten.
#'
#' @return A new subfolder beneath src with an Rmarkdown file
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_replace_all str_extract
#' @importFrom assertthat assert_that is.string is.date
#' @importFrom rmarkdown draft render
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
  projectname = "",
  theme = c("generiek", "water", "lucht", "bodem", "vegetatie", "soorten")) {

  # check parameters
  assert_that(is.string(title))
  assert_that(is.string(subtitle))
  assert_that(is.string(short_title), nchar(short_title) <= 20)
  assert_that(is.date(as.Date(date)))
  assert_that(is.string(authors))
  assert_that(is.string(reviewers))
  assert_that(is.string(file_manager))
  assert_that(is.string(revision))
  assert_that(is.string(projectname))
  theme <- match.arg(theme)

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
  protocol_filename <- folder_name

  # directory setup
  project_root <- find_root(is_git_root)
  if (!nzchar(projectname)) {
    path_to_protocol <- file.path(project_root, "src", "thematic", theme,
                                  folder_name)
  } else {
    path_to_protocol <- file.path(project_root, "src", "project", projectname,
                                  theme, folder_name)
  }

  # check for existence of the folder
  if (dir.exists(path_to_protocol)) {
    stop(sprintf(paste0("The protocol repository already has ",
                        "a folder %s!"), path_to_protocol))
  }
  # create a new directory
  dir.create(file.path(path_to_protocol),
               recursive = TRUE)

  # create subfolders data and media
  dir.create(file.path(path_to_protocol, "data"))
  dir.create(file.path(path_to_protocol, "media"))

  # move all files from the template folder
  parent_rmd <- file.path(path_to_protocol, paste0(protocol_filename, ".Rmd"))
  draft(file = parent_rmd,
        template = "template_sfp",
        package = "protocolshelper",
        edit = FALSE)
  # change values in parent rmarkdown
  original_file <- file(parent_rmd, "r")
  original_file_content <- readLines(parent_rmd)
  data <- list(title = title,
               subtitle = subtitle,
               authors = authors,
               date = date,
               reviewers = reviewers,
               file_manager = file_manager,
               revision = revision,
               procedure = protocol_code,
               theme = theme,
               projectname = projectname
  )
  writeLines(map_chr(original_file_content,
                     whisker.render,
                     data),
             parent_rmd)
  close(original_file)

  # render html
  render(input = parent_rmd)
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
