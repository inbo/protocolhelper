#' @title Checks protocol metadata
#'
#' @description This function reads metadata from the yaml front matter stored
#' the index.Rmd file of a protocol and checks if the metadata format is correct.
#' This function is intended for checking if a protocol is ready to be rendered
#' and published (for instance, it will fail if version number is `YYYY.NN.dev`).
#'
#' @inheritParams get_path_to_protocol
#' @param yaml Boolean. If \code{FALSE} the function will only print error messages,
#' otherwise (default) the parsed yml front matter will be returned
#'
#' @return parsed yml front matter if \code{yaml} is \code{TRUE}, otherwise
#' nothing is returned.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom stringr str_detect
#'
#' @export
#'
#'
check_frontmatter <- function(
  protocol_code,
  yaml = TRUE
  ) {

  path_to_protocol <- get_path_to_protocol(
    protocol_code = protocol_code)

  yml <- yaml_front_matter(file.path(path_to_protocol, "index.Rmd"))

  assert_that(has_name(yml, "title"))
  assert_that(is.string(yml$title))

  if (has_name(yml, "subtitle")) {
    assert_that(is.string(yml$subtitle))
  }

  assert_that(has_name(yml, "author"))
  assert_that(is.character(yml$author))

  assert_that(has_name(yml, "date"))
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package \"lubridate\" needed for checking of date. ",
         "Please install it with 'install.packages(\"lubridate\")'.",
         call. = FALSE)
  }
  assert_that(all.equal(yml$date,
                        lubridate::format_ISO8601(as.Date(yml$date))),
              msg = "'date' must be in YYYY-MM-DD format")

  assert_that(has_name(yml$params, "reviewers"))
  assert_that(is.character(yml$params$reviewers))

  assert_that(has_name(yml$params, "file_manager"))
  assert_that(is.string(yml$params$file_manager))

  assert_that(has_name(yml$params, "protocol_code"))
  assert_that(str_detect(yml$params$protocol_code, "^s[fioap]p-\\d{3}$"))

  assert_that(has_name(yml$params, "version_number"))
  assert_that(str_detect(yml$params$version_number, "^\\d{4}\\.\\d{2}$"))

  assert_that(has_name(yml$params, "language"))
  assert_that(yml$params$language %in% c("nl", "en"),
              is.string(yml$params$language),
              msg = "'lang' must be 'nl' or 'en'")

  if (has_name(yml$params, "theme")) {
    assert_that(yml$params$theme %in% c("generic", "water", "air", "soil", "vegetation",
                                 "species"),
                is.string(yml$params$theme))
  }

  if (has_name(yml$params, "project_name")) {
    assert_that(is.string(yml$params$project_name))
  }

  if (yaml) {
    return(yml)
  }

}
