#' @title Checks protocol metadata
#'
#' @description This function reads metadata from the yaml front matter stored
#' the index.Rmd file of a protocol and checks if the metadata format is correct.
#' This function is intended for checking if a protocol is ready to be rendered
#' and published (for instance, it will fail if version number is `YYYY.NN.dev`).
#'
#' @inheritParams get_path_to_protocol
#'
#' @return Silent, or an error message stating which check failed.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom stringr str_detect
#'
#' @export
#'
#'
check_frontmatter <- function(protocol_folder_name) {

  path_to_protocol <- get_path_to_protocol(
    protocol_folder_name = protocol_folder_name)

  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))

  assert_that(has_name(yml, "title"))
  assert_that(is.string(yml$title))

  if (has_name(yml, "subtitle")) {
    assert_that(is.string(yml$subtitle))
  }

  assert_that(has_name(yml, "authors"))
  assert_that(is.character(yml$authors))

  assert_that(has_name(yml, "date"))
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package \"lubridate\" needed for checking of date. ",
         "Please install it with 'install.packages(\"lubridate\")'.",
         call. = FALSE)
  }
  assert_that(all.equal(yml$date,
                        lubridate::format_ISO8601(as.Date(yml$date))),
              msg = "'date' must be in YYYY-MM-DD format")

  assert_that(has_name(yml, "reviewers"))
  assert_that(is.character(yml$reviewers))

  assert_that(has_name(yml, "file_manager"))
  assert_that(is.string(yml$file_manager))

  assert_that(has_name(yml, "protocol_code"))
  assert_that(str_detect(yml$protocol_code, "^s[fioap]p_\\d{3}$"))

  assert_that(has_name(yml, "version_number"))
  assert_that(str_detect(yml$version_number, "^\\d{4}\\.\\d{2}$"))

  assert_that(has_name(yml, "language"))
  assert_that(yml$language %in% c("nl", "en"),
              is.string(yml$language),
              msg = "'lang' must be 'nl' or 'en'")

  if (has_name(yml, "theme")) {
    assert_that(yml$theme %in% c("generic", "water", "air", "soil", "vegetation",
                                 "species"),
                is.string(yml$theme))
  }

  if (has_name(yml, "project")) {
    assert_that(is.string(yml$project))
  }
}
