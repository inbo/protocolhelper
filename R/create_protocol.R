#' @title Create a folder with a bookdown (R markdown) template to start a new
#' protocol and optionally render to html
#'
#' @description This function will create a new folder based on values that are
#' passed on via the parameters and creates a R-markdown (bookdown) skeleton
#' based on a template file to start working on a new protocol.
#' The function is interactive and will ask for the title, optional subtitle,
#' the authors, reviewers, file manager and keywords.
#' These metadata (YAML section of `index.Rmd` file) will then be filled in
#' automatically.
#' Optionally, the rmarkdown chapters are rendered to an html file which will
#' be saved in a matching subfolder of the `docs` folder.
#'
#'
#' @details It is assumed that the `source` folder is a subfolder of an RStudio
#' project with git version control.
#' A target folder to which files will be written will be created as
#' subdirectories beneath `source`.
#' The subfolder structure is of the form
#' `/sfp/<theme>/<sfp>_<protocolnumber>_<language>_<short_title>/` for
#' standard field protocols.
#' Or `/spp/<project_name>/<spp>_<protocolnumber>_<language>_<short_title>/`
#' for standard project protocols.
#' Or `/sip/<sip>_<protocolnumber>_<language>_<short_title>/` for sips (and
#' analogous for sop and sap).
#' The folder names are determined by the corresponding arguments of the
#' function.
#' A matching subfolder structure will be created beneath the `docs` folder (and
#' output files needed for rendering to html output will be placed in it if
#' `render = TRUE`.
#' The template Rmarkdown files and the Rmarkdown files that result from
#' converting a docx protocol (see `from_docx` argument), will be written to
#' the target folder beneath `source`.
#' Template Rmarkdown files with the same name as Rmarkdown files that result
#' from converting a docx protocol will be overwritten by the latter.
#' Besides Rmarkdown files, this target folder will also contain files needed to
#' render to a Bookdown `gitbook` such as a `_bookdown.yml` and `_output.yml`.
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
#' @param version_number A version number of the form `YYYY.##`.
#' The default is a function which will determine this number automatically.
#' It should normally not be changed.
#' @param project_name A character string that is used as the folder location
#' (`source/spp/project_name`) where project-specific protocols that belong to
#' the same project will be stored. Preferably a short name or acronym. If the
#' folder does not exist, it will be created.
#' Ignored if protocol_type is other than `"spp"`.
#' @param short_title A character string of less than 20 characters to use in
#' folder and file names
#' @param from_docx A character string with the path (absolute or relative) to
#' a `.docx` file containing a pre-existing protocol.
#' Please make sure to copy-paste all relevant meta-data from the `.docx` file
#' to the corresponding parameters of this function.
#' If nothing is provided (i.e. default = NULL), an empty template will be used.
#' @param template Which template to use?
#' Default is set equal to protocol_type.
#' However, you can also set this to `"generic"` in which case a simplified
#' template will be used that can be used as an alternative to the default
#' templates.
#' @param render Whether or not to render the protocol to html.
#' Defaults to FALSE.
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_replace_all str_detect
#' @importFrom assertthat assert_that is.string is.date is.flag noNA
#' @importFrom rmarkdown draft
#' @importFrom bookdown render_book
#' @importFrom fs path_rel dir_create dir_ls file_copy
#' @importFrom checklist use_author citation_meta ask_yes_no
#' @importFrom cli cli_alert_success cli_alert cli_alert_info cli_alert_danger
#' cli_fmt
#'
#' @export
#' @family creation
#' @examples
#' \dontrun{
#' protocolhelper::create_protocol(
#'   protocol_type = "sfp",
#'   short_title = "water 1",
#'   theme = "water", language = "en")
#' }
create_protocol <- function(
  protocol_type = c("sfp", "spp", "sap", "sop", "sip"),
  short_title,
  version_number = get_version_number(),
  theme = NULL,
  project_name = NULL,
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = protocol_type,
  render = FALSE) {

  # check parameters
  protocol_type <- match.arg(protocol_type)
  assert_that(template %in% c("sfp", "spp", "sap", "sop", "sip", "generic"))
  assert_that(is.string(short_title), nchar(short_title) <= 20)
  check_versionnumber(version_number)
  if (protocol_type == "sfp") {
    assert_that(
      is.string(theme),
      theme %in% themes_df$theme)
  }
  if (protocol_type == "spp") {
    assert_that(is.string(project_name))
  }
  language <- match.arg(language)
  if (!is.null(protocol_number)) {
    assert_that(
      is.string(protocol_number),
      !(protocol_number %in% get_protocolnumbers(
        protocol_type = protocol_type,
        language = language)),
      msg = cli_fmt(cli_alert_danger(
        "The protocolnumber {protocol_number} is already in use
        for protocol type {protocol_type} and language {language}."
      ))
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
  short_title <- str_replace_all(short_title, " ", "_")
  short_titles <- get_short_titles(
    protocol_type = protocol_type,
    language = language)
  assert_that(
    !(short_title %in% short_titles),
    msg = "The given short title already exists.
              Give a short title that is not in use.
              Use get_short_titles() to get an overview of short titles
              that are in use.")
  folder_name <- paste0(
    str_replace_all(protocol_code, "-", "_"),
    "_", short_title)
  folder_name <- tolower(folder_name)
  protocol_filename <- folder_name
  # set _bookdown.yml values
  book_filename <- paste0(protocol_filename, ".Rmd")
  # the output_dir should be set as a relative path to make it reproducible on
  # other machines: it should be relative to path_to_protocol

  # directory setup
  path_to_protocol <- get_path_to_protocol(
    protocol_code,
    theme = theme,
    project_name = project_name,
    short_title = short_title)
  output_dir <- gsub("source", "docs", path_to_protocol)

  # next make it relative to path_to_protocol
  output_dir_rel <- path_rel(output_dir, path_to_protocol)

  # check for existence of non-empty folders
  assert_that(!(dir.exists(path_to_protocol) &&
      !identical(
        unname(
          unclass(
            dir_ls(
              path_to_protocol,
              type = "file")
          )
        ),
        character(0)
      )),
      msg = cli_fmt(cli_alert_danger(
        "The protocol repository already has a non-empty folder
        {.path {path_to_protocol}}!")
      ))
  assert_that(!(dir.exists(output_dir) &&
      !identical(
        unname(
          unclass(
            dir_ls(
              output_dir,
              type = "file")
          )
        ),
        character(0)
      )),
      msg = cli_fmt(cli_alert_danger(
        "The protocol repository already has a non-empty folder
        {.path {output_dir}}!")
      ))
  # create new directories
  cli_alert_info("Creating folder structure")
  dir_create(
    file.path(path_to_protocol),
    recurse = TRUE)
  dir_create(
    file.path(output_dir),
    recurse = TRUE)
  # create subfolders data and media
  dir_create(file.path(path_to_protocol, "data"))
  dir_create(file.path(path_to_protocol, "media"))

  # create from empty template
  # move all files from the template folder
  parent_rmd <- file.path(path_to_protocol, "index.Rmd")
  template_folder <- paste("template", template, language, sep = "_")
  cli_alert_info("Writing template files")
  draft(
    file = parent_rmd,
    template = template_folder,
    package = "protocolhelper",
    edit = FALSE)

  # write _bookdown.yml
  cli_alert_info("Writing _bookdown.yml")
  write_bookdown_yml(
    language = language,
    book_filename = book_filename,
    path_to_protocol = path_to_protocol,
    output_dir_rel = output_dir_rel)
  # write _output.yml
  cli_alert_info("Writing _output.yml")
  write_output_yml(language = language, path_to_protocol = path_to_protocol)

  # add LICENSE file
  cli_alert_info("Writing CC-BY license file")
  path("generic_template", "cc_by_4_0.md") |>
    system.file(package = "checklist") |>
    file_copy(file.path(path_to_protocol, "LICENSE.md"))

  # build new yaml
  readline(prompt = cli_fmt(cli_alert("Enter the title: "))) |>
    gsub(pattern = "[\"|']", replacement = "") |>
    sprintf(fmt = "title: \"%s\"") -> yaml
  readline(
    prompt = cli_fmt(
      cli_alert(
        "Enter the optional subtitle (leave empty to omit): "
      )
    )
  ) |>
    gsub(pattern = "[\"|']", replacement = "") -> subtitle
  yaml <- c(yaml, sprintf(fmt = "subtitle: \"%s\"", subtitle)[subtitle != ""])
  cli_alert("Please select the corresponding author")
  authors <- use_author()
  c(yaml, "author:", author2yaml(authors, corresponding = TRUE)) -> yaml
  while (
    isTRUE(
      ask_yes_no(
        cli_fmt(
          cli_alert(
            "Add another author?"
            )
          )
      )
    )
  ) {
    author <- use_author()
    authors[, c("given", "family", "email")] |>
      rbind(author[, c("given", "family", "email")]) |>
      anyDuplicated() -> duplo
    if (duplo > 0) {
      cli_alert_danger(
        "{author$given} {author$family} is already listed as author"
      )
      next
    }
    c(yaml, author2yaml(author, corresponding = FALSE)) -> yaml
    authors <- rbind(authors, author)
  }
  cli_alert("Please select a reviewer")
  reviewer <- use_reviewer()
  authors[, c("given", "family", "email")] |>
    rbind(reviewer[, c("given", "family", "email")]) |>
    anyDuplicated() -> duplo
  if (duplo > 0) {
    cli_alert_danger(
      "{reviewer$given} {reviewer$family} is already listed as author"
    )
  }
  c(yaml, "reviewer:", author2yaml(reviewer, corresponding = FALSE)) -> yaml
  while (
    isTRUE(
      ask_yes_no(
        cli_fmt(
          cli_alert(
            "Add another reviewer?"
          )
        )
      )
    )
  ) {
    reviewer <- use_reviewer()
    authors[, c("given", "family", "email")] |>
      rbind(reviewer[, c("given", "family", "email")]) |>
      anyDuplicated() -> duplo
    if (duplo > 0) {
      cli_alert_danger(
        "{reviewer$given} {reviewer$family} is already listed as author"
      )
      next
    }
    c(yaml, author2yaml(reviewer, corresponding = FALSE)) -> yaml
  }
  cli_alert("Please select the file manager")
  file_manager <- use_file_manager()

  readline(prompt = cli_fmt(
    cli_alert("Enter one or more keywords separated by `;`"))) |>
    gsub(pattern = "[\"|']", replacement = "") |>
    strsplit(";") |>
    unlist() |>
    gsub(pattern = "^\\s+", replacement = "") |>
    gsub(pattern = "\\s+$", replacement = "") |>
    paste(collapse = "; ") |>
    sprintf(fmt = "keywords: \"%s\"") -> keywords

  c(
    yaml,
    "file_manager:", author2yaml(file_manager, corresponding = FALSE),
    paste("language:", language),
    paste("date:", "\"`r Sys.Date()`\""),
    paste("protocol_code:", protocol_code),
    paste0("version_number: \"", version_number, "\""),
    paste("template_name:", template),
    paste("theme:", theme)[!is.null(theme)],
    paste("project_name:", project_name)[!is.null(project_name)],
    keywords,
    "community: \"inbo\"",
    "site: bookdown::bookdown_site",
    "bibliography: references.yaml"[language == "en"],
    "bibliography: referenties.yaml"[language == "nl"],
    "link-citations: TRUE",
    "csl: https://raw.githubusercontent.com/citation-style-language/styles/master/research-institute-for-nature-and-forest.csl" #nolint
  ) -> yaml

  # read index template
  path(path_to_protocol, "index.Rmd") |>
    readLines() -> index
  # remove existing yaml
  index <- tail(index, -grep("---", index)[2])
  # add new yaml
  index <- c("---", yaml, "---", index)
  writeLines(index, path(path_to_protocol, "index.Rmd"))

  # create zenodo json file
  cli_alert_info("Writing .zenodo.json file")
  citation_meta$new(path_to_protocol)

  # start new header in NEWS
  news <- xfun::read_utf8(file.path(path_to_protocol, "NEWS.md"))
  news <- append(
    x = news,
    values = c(
      sprintf("## [%1$s](../%1$s/index.html)", version_number),
      "",
      "-   ...",
      ""
    ),
    after = 2)
  xfun::write_utf8(news, file.path(path_to_protocol, "NEWS.md"))


  if (!is.null(from_docx)) {
    assert_that(file.exists(from_docx))
    create_from_docx(
      from_docx = from_docx,
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
  cli_alert_success(
    "Your protocol has been created in folder {.path {path_to_protocol}}."
  )
}


#' @rdname create_protocol
#' @export
create_sfp <- function(
  short_title,
  version_number = get_version_number(),
  theme = c("generic", "water", "air", "soil", "vegetation", "species"),
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = c("sfp", "generic"),
  render = FALSE) {
  template <- match.arg(template)
  create_protocol(
    protocol_type = "sfp",
    short_title = short_title,
    version_number = version_number,
    theme = theme,
    language = language,
    from_docx = from_docx,
    protocol_number = protocol_number,
    template = template,
    render = render)
}

#' @rdname create_protocol
#' @export
create_spp <- function(
  short_title,
  version_number = get_version_number(),
  project_name,
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = c("spp"),
  render = FALSE) {
  create_protocol(
    protocol_type = "spp",
    short_title = short_title,
    version_number = version_number,
    project_name = project_name,
    language = language,
    from_docx = from_docx,
    protocol_number = protocol_number,
    template = template,
    render = render)
}

#' @rdname create_protocol
#' @export
create_sap <- function(
    short_title,
    version_number = get_version_number(),
    language = c("nl", "en"),
    from_docx = NULL,
    protocol_number = NULL,
    template = c("sap", "generic"),
    render = FALSE) {
  template <- match.arg(template)
  create_protocol(
    protocol_type = "sap",
    short_title = short_title,
    version_number = version_number,
    language = language,
    from_docx = from_docx,
    protocol_number = protocol_number,
    template = template,
    render = render)
}

#' @rdname create_protocol
#' @export
create_sip <- function(
    short_title,
    version_number = get_version_number(),
    language = c("nl", "en"),
    from_docx = NULL,
    protocol_number = NULL,
    template = c("sip", "generic"),
    render = FALSE) {
  template <- match.arg(template)
  create_protocol(
    protocol_type = "sip",
    short_title = short_title,
    version_number = version_number,
    language = language,
    from_docx = from_docx,
    protocol_number = protocol_number,
    template = template,
    render = render)
}

#' @rdname create_protocol
#' @export
create_sop <- function(
    short_title,
    version_number = get_version_number(),
    language = c("nl", "en"),
    from_docx = NULL,
    protocol_number = NULL,
    template = c("sop", "generic"),
    render = FALSE) {
  template <- match.arg(template)
  create_protocol(
    protocol_type = "sop",
    short_title = short_title,
    version_number = version_number,
    language = language,
    from_docx = from_docx,
    protocol_number = protocol_number,
    template = template,
    render = render)
}
