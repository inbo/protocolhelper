#' @title Checks protocol metadata
#'
#' @description This function reads metadata from the yaml front matter stored
#' in the `index.Rmd` file of a protocol and checks if the metadata format is
#' correct.
#' This function is intended for checking if a protocol is ready to be rendered
#' and published (for instance, it will fail if version number is
#' `YYYY.NN.dev`).
#'
#' @inheritParams get_path_to_protocol
#' @param fail Should the function drop an error in case of a problem?
#' Defaults to `TRUE` in a non-interactive session and `FALSE` in an interactive
#' session.
#'
#' @return A report of all failed checks.
#'
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom assertthat assert_that is.string has_name is.flag noNA
#' @importFrom stringr str_detect
#' @importFrom purrr map_lgl map_chr
#' @importFrom checklist citation_meta
#'
#' @export
#' @family check
#'
check_frontmatter <- function(
    protocol_code,
    fail = !interactive()
) {
  check_protocolcode(protocol_code)
  assert_that(is.flag(fail), noNA(fail))

  x <- load_protocolcheck(x = protocol_code)

  if (!file.exists(file.path(x$path, "index.Rmd"))) {
    x$add_error(msg = paste0(file.path(x$path, "index.Rmd"),
                             " does not exist."))
    return(x$check(fail = fail))
  }

  yml_protocol <- yaml_front_matter(input = file.path(x$path, "index.Rmd"))

  if (!(is.string(yml_protocol$template_name) &&
        is.string(yml_protocol$language))) {
    x$add_error(msg = sprintf("yaml keys `template_name` and `language`
                              should be present in the yaml section of index.Rmd
                              and their values should be strings."))
    return(x$check(fail = fail))
  }

  template_name <-
    paste("template", yml_protocol$template_name,
          yml_protocol$language, sep = "_")

  path_to_template <-
    system.file(
      file.path("rmarkdown", "templates", template_name, "skeleton"),
      package = "protocolhelper")

  if (!file.exists(file.path(path_to_template,
                             "skeleton.Rmd"))) {
    x$add_error(msg = paste0(file.path(path_to_template,
                                       "skeleton.Rmd"),
                             " does not exist."))
    return(x$check(fail = fail))
  }

  yml_template <- yaml_front_matter(
    input = file.path(
      path_to_template,
      "skeleton.Rmd"))

  # check if all yaml keys are present
  yml_missing <- yml_template[!names(yml_template) %in% names(yml_protocol)]
  yml_missing$subtitle <- NULL
  problems <- sprintf(
    "The yaml-key '%s' is missing",
    names(yml_missing)
  )

  # checks common to all protocol types
  yml_string <- list("title" = yml_protocol$title)
  problems <- c(problems,
                sprintf(
                  "'%s' must be a string",
                  names(yml_string)[!map_lgl(yml_string, is.string)])
  )
  problems <-
    c(problems,
      "bibliography in yaml header should refer to a bibliography file"[
        !is.character(yml_protocol$bibliography)
      ])

  problems <- c(problems,
                paste0(
                  "subtitle is not a string, NULL, or an empty string, ",
                  "please remove in the yaml header if not needed."
                )[has_name(yml_protocol, "subtitle") &&
                    (!is.string(yml_protocol$subtitle) ||
                    nchar(yml_protocol$subtitle) <= 1)]
  )

  # check persons
  cit_meta <- citation_meta$new(x$path)
  problems <- c(problems, cit_meta$get_errors)

  problems <- check_all_person_info(
    person_list = yml_protocol$author,
    problems_vect = problems)

  problems <- check_all_person_info(
    person_list = yml_protocol$reviewer,
    problems_vect = problems)

  problems <- check_all_person_info(
    person_list = yml_protocol$file_manager,
    problems_vect = problems)

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package \"lubridate\" needed for checking of date. ",
         "Please install it with 'install.packages(\"lubridate\")'.",
         call. = FALSE)
  }

  problems <-
    c(problems,
      "'date' must be in YYYY-MM-DD format"[
        !grepl(pattern = "r Sys.Date()", x = yml_protocol$date) &&
          !isTRUE(
            all.equal(yml_protocol$date,
                      lubridate::format_ISO8601(as.Date(yml_protocol$date)))
          )
      ])


  right_format <- grepl("^s[fpioa]p-\\d{3}-(?:nl|en)$",
                        yml_protocol$protocol_code)
  is_reserved <- any(
    yml_protocol$protocol_code %in% reserved_codes$protocolcode)
  problems <-
    c(problems,
      "protocol code has wrong format"[
        !(right_format | is_reserved)
      ])

  problems <- c(
      problems,
      "version_number should be YYYY.NN with NN a 2 digit number above 0"[
        !str_detect(yml_protocol$version_number, "^\\d{4}\\.\\d{2}$")])

  problems <- c(
    problems,
    paste0("version number in the YAML of index.Rmd needs to be updated.\n",
           "Please use protocolhelper::update_version_number().")[
      !identical(get_version_number(), yml_protocol$version_number)
    ]
  )

  problems <- c(problems,
                "'lang' must be 'nl' or 'en'"[
                  !any(yml_protocol$language %in% c("nl", "en"))]
                )

  # protocol type specific checks
  problems <- c(
    problems,
    paste0(
      "Please check theme in yaml metadata\n",
      "It should be one of generic, water, air, soil or vegetation")[
        has_name(yml_protocol, "theme") &&
          !any(yml_protocol$theme %in%
                 c("generic", "water", "air", "soil", "vegetation", "species"))
      ]
  )

  problems <- c(
    problems,
    paste0(
      "Please check project_name in yaml metadata\n",
      "It should be a character string")[
        has_name(yml_protocol, "project_name") &&
          !is.string(yml_protocol$project_name)
      ]
  )

  x$add_error(problems)

  return(x$check(fail = fail))
}
