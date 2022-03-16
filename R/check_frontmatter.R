#' @title Checks protocol metadata
#'
#' @description This function reads metadata from the yaml front matter stored
#' in the index.Rmd file of a protocol and checks if the metadata format is
#' correct.
#' This function is intended for checking if a protocol is ready to be rendered
#' and published (for instance, it will fail if version number is
#' `YYYY.NN.dev`).
#'
#' @inheritParams get_path_to_protocol
#' @param yaml Boolean. If \code{FALSE} the function will only print error
#' messages, otherwise (default) the parsed yml front matter will be returned
#'
#' @return If one of the checks fails, an error message will be returned.
#' Otherwise, the parsed yml front matter if \code{yaml} is
#' \code{TRUE} is returned, or a message that everything is OK otherwise.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom assertthat assert_that is.string has_name
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl
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
    assert_that(is.string(yml$subtitle),
                msg = "Subtitle is not a string, maybe empty, please remove in the yaml header if not needed.")
  }

  assert_that(has_name(yml, "author"))
  assert_that(
    all(
      purrr::map_lgl(yml$author, ~has_name(., "name"))
      ),
    all(
      purrr::map_lgl(yml$author, ~has_name(., "orcid"))
    )
  )
  assert_that(
    all(
      purrr::map_lgl(yml$author, ~is.string(.$name))
    ),
    all(
      purrr::map_lgl(yml$author, ~is.string(.$orcid))
    )
  )
  assert_that(has_name(yml, "date"))
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package \"lubridate\" needed for checking of date. ",
         "Please install it with 'install.packages(\"lubridate\")'.",
         call. = FALSE)
  }
  assert_that(isTRUE(
    all.equal(yml$date,
              lubridate::format_ISO8601(as.Date(yml$date)))
  ),
  msg = "'date' must be in YYYY-MM-DD format")

  assert_that(has_name(yml, "reviewers"))
  assert_that(is.character(yml$reviewers))

  assert_that(has_name(yml, "file_manager"))
  assert_that(is.string(yml$file_manager))

  assert_that(has_name(yml, "protocol_code"))
  assert_that(str_detect(yml$protocol_code, "^s[fioap]p-\\d{3}-(nl|en)$"))

  assert_that(has_name(yml, "version_number"))
  assert_that(str_detect(yml$version_number, "^\\d{4}\\.\\d{2}$")) # nolint: nonportable_path_linter, line_length_linter.

  assert_that(has_name(yml, "language"))
  assert_that(any(yml$language %in% c("nl", "en")),
              is.string(yml$language),
              msg = "'lang' must be 'nl' or 'en'")

  if (has_name(yml, "theme")) {
    assert_that(
      any(yml$theme %in% c("generic", "water", "air", "soil", "vegetation",
                              "species")),
      is.string(yml$theme),
      msg = paste0(
        "Please check theme in yaml metadata\n",
        "It should be one of generic, water, air, soil or vegetation")
      )
  }

  if (has_name(yml, "project_name")) {
    assert_that(is.string(yml$project_name))
  }

  if (yaml) {
    return(yml)
  } else {
    message("Everything is OK")
  }

}
