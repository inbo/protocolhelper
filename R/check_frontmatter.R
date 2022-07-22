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
#'
#' @export
#'
#'
check_frontmatter <- function(
    protocol_code,
    fail = !interactive()
) {
  assert_that(str_detect(protocol_code, "^s[fioap]p-\\d{3}-(nl|en)$"))
  assert_that(is.flag(fail), noNA(fail))

  x <- load_protocolcheck(x = protocol_code)
  template_name <-
    gsub(
      pattern = "(s\\w{1}p)-\\d*-(\\w{2})",
      replacement = "template_\\1_\\2",
      protocol_code
    )
  path_to_template <-
    system.file(
      file.path("rmarkdown", "templates", template_name, "skeleton"),
      package = "protocolhelper")

  yml_protocol <- yaml_front_matter(input = file.path(x$path, "index.Rmd"))
  yml_template <- yaml_front_matter(input = file.path(path_to_template,
                                                      "skeleton.Rmd"))

  # check if all yaml keys are present
  yml_missing <- yml_template[!names(yml_template) %in% names(yml_protocol)]
  yml_missing$subtitle <- NULL
  problems <- sprintf(
    "The yaml-key '%s' is missing",
    names(yml_missing)
  )

  # checks common to all protocol types
  yml_string <- list("title" = yml_protocol$title,
                  "file_manager" = yml_protocol$file_manager)
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

  if (has_name(yml_protocol, "subtitle")) {
    problems <- c(problems,
                    paste0(
                      "subtitle is not a string, NULL, or an empty string, ",
                      "please remove in the yaml header if not needed."
                    )[!is.string(yml_protocol$subtitle) ||
                        nchar(yml_protocol$subtitle) <= 1]
    )
  }
  author_name <- map_lgl(yml_protocol$author, ~is.string(.$name))
  author_orcid <- map_lgl(yml_protocol$author, ~is.string(.$orcid))
  problems <-
    c(problems,
      sprintf(
        "Author nr %s had an invalid name (no string)",
        seq_along(author_name)[!author_name]
      ))
  problems <-
    c(problems,
      sprintf(
        "Author nr %s had an invalid orcid (no string)",
        seq_along(author_orcid)[!author_orcid]
      ))

  if (all(author_name)) {
    names <- map_chr(yml_protocol$author, "name")
    problems <- c(
      problems,
      "A single author should be passed as: c(\"lastname1, firstname1\")"[
        !((is.string(names) & all(str_detect(names, ",{1}"))) |
            is.character(names))])
    problems <- c(
      problems,
      paste0("Multiple commas detected in author string.",
             "Multiple authors should be passed as: ",
             "c(\"lastname1, firstname1\", \"lastname2, firstname2\")")[
               !((is.string(names) & !all(str_detect(names, ",{2,}"))) |
                   is.character(names))])
  }
  if (all(author_orcid)) {
    orcids <- map_chr(yml_protocol$author, "orcid")
    problems <-
      c(problems,
        "Please provide `orcids` in the `0000-0000-0000-0000` format."[
          !all(nchar(orcids) == 19)])
    problems <- c(
      problems,
      "Multiple orcids should be passed as c(\"orcid1\", \"orcid2\")"[
        any(str_detect(orcids, ",|;"))])
    valid_orcids <- map_lgl(orcids, validate_orcid)
    problems <- c(
      problems,
      sprintf("protocolhelper::validate_orcid() indicates %s is not valid",
              orcids)[!valid_orcids]
    )
  }
  if (all(author_name) && all(author_orcid)) {
    problems <- c(problems,
                  "No authors provided, please provide at least one"[
                    length(names) == 0
                  ])
    problems <- c(problems,
                  "Number of author names not equal to number of orcids"[
                    length(names) != length(orcids)])
  }

  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package \"lubridate\" needed for checking of date. ",
         "Please install it with 'install.packages(\"lubridate\")'.",
         call. = FALSE)
  }

  problems <-
    c(problems,
      "'date' must be in YYYY-MM-DD format"[
        !grepl(pattern = "`r Sys.Date()`", x = yml_protocol$date) &&
          !isTRUE(
            all.equal(yml_protocol$date,
                      lubridate::format_ISO8601(as.Date(yml_protocol$date)))
          )
      ])


  problems <-
    c(problems,
      "'reviewers' must be a character vector"[
        !is.character(yml_protocol$reviewers)])

  problems <-
    c(problems,
      "protocol code has wrong format"[
        !str_detect(yml_protocol$protocol_code,
                    "^s[fioap]p-\\d{3}-(nl|en)$")
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

  if (!any(yml_protocol$language %in% c("nl", "en"))) {
    problems <- c(problems,
                  "'lang' must be 'nl' or 'en'")
  }

  # protocol type specific checks
  if (has_name(yml_protocol, "theme")) {
    if (!any(yml_protocol$theme %in%
             c("generic", "water", "air", "soil", "vegetation", "species"))) {
      problems <- c(
        problems,
        paste0(
          "Please check theme in yaml metadata\n",
          "It should be one of generic, water, air, soil or vegetation")
        )
    }
  }

  if (has_name(yml_protocol, "project_name")) {
    if (!is.string(yml_protocol$project_name)) {
      problems <- c(
        problems,
        paste0(
          "Please check project_name in yaml metadata\n",
          "It should be a character string")
        )
    }
  }

  x$add_error(problems)

  return(x$check(fail = fail))
}
