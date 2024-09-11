#' @title Function to list all occupied protocol numbers
#'
#' @description This function will search for protocol numbers in filenames of
#' Rmarkdown files listed underneath the source folder.
#' The search will be restricted to files of a given protocol type and given
#' language.
#'
#' @param protocol_type A character string equal to `sfp` (default), `sip`,
#' `sap`, `sop` or `spp`.
#' @param language Language of the protocol, either `"nl"` (Dutch),
#' the default, or `"en"` (English).
#'
#'
#' @return A character vector with occupied protocol numbers for a specific
#' protocol type
#'
#' @importFrom rprojroot find_root is_git_root
#' @importFrom stringr str_subset str_replace str_extract
#'
#' @export
#' @family utility
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
  path_to_source <- file.path(project_root, "source")
  ld <- list.dirs(
    path = path_to_source,
    recursive = TRUE,
    full.names = FALSE
  )
  ld <- str_subset(
    string = ld,
    pattern = protocol_type)
  ld <- str_subset(
    string = ld,
    pattern = paste0("_", language, "_"))
  ld <- str_extract(
    string = ld,
    pattern = paste0("(?<=", protocol_type, "_)\\d{3}"))
  ld <- ld[!is.na(ld)]
  ld <- unique(ld)

  return(ld)
}


#' @title Function to list all short titles that are already in use.
#'
#' @description This function will search for short titles in filenames of
#' Rmarkdown files listed underneath the source folder.
#' The search will be restricted to files of a given protocol type and given
#' language.
#'
#' @param protocol_type A character string equal to `sfp` (default), `sip`,
#' `sap`, `sop` or `spp`.
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
#' @family utility
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
  path_to_source <- file.path(project_root, "source")
  ld <- list.dirs(
    path = path_to_source,
    recursive = TRUE,
    full.names = FALSE
  )
  ld <- str_subset(
    string = ld,
    pattern = str_replace_all(protocol_type, "-", "_"))
  ld <- str_extract(
    string = ld,
    pattern = paste0(
      "(?<=\\w{3,6}_", language,
      "_)([a-z]|_|[:digit:])*"))
  ld <- ld[!is.na(ld)]
  ld <- unique(ld)

  return(ld)
}



#' Create protocol code from it's components
#'
#' A protocol code of format `s[fpioa]p-###-[nl|en]` will be created.
#' The number will be determined automatically based on theme (in case of `sfp`)
#' and a rank order of all existing and reserved protocol numbers, unless
#' the protocol number is passed directly to the `protocol_number` argument.
#'
#' @param protocol_type Either `sfp` (standard field protocol), `spp` (
#' standard project protocol), `sap` (standard analytical protocol), `sip` (
#' standard instrument protocol), `sop` (standard operating protocol)
#' @param theme A character string equal to one of `"generic"` (default),
#' `"water"`, `"air"`, `"soil"`, `"vegetation"` or `"species"`. It is used as
#' the folder location (`source/sfp/theme`) where standard field protocols
#' that belong to the same theme will be stored.
#' Ignored if protocol_type is other than `"sfp"`.
#' @param protocol_number A character string giving the protocol number.
#' This parameter should normally not be specified (i.e. NULL), unless
#' `from_docx` is specified.
#' A protocol number is a three digit string where the first digit corresponds
#' with a theme and the last two digits identify a protocol within a theme for
#' standard field protocols. A protocol number for other protocol types
#' is just a three digit string.
#' If NULL (the default), a protocol number will be determined automatically
#' based on pre-existing protocol numbers.
#' Note that for backwards compatibility with protocol numbers that were already
#' in use at INBO, we made a list of reserved numbers.
#' These reserved numbers will not be used when `protocol_number` is NULL.
#' The only time you will need to explicitly pass a protocol number to the
#' `protocol_number` argument is when you want to migrate a pre-existing INBO
#' protocol to `protocolsource` and hence use one of the reserved numbers.
#' Protocol numbers that are already in use in `protocolsource` can be retrieved
#' with `get_protocolnumbers()`.
#' @param language Language of the protocol, either `"nl"` (Dutch),
#' the default, or `"en"` (English).
#'
#' @importFrom stringr str_subset str_extract
#' @importFrom assertthat assert_that validate_that
#' @importFrom cli cli_fmt cli_alert_danger
#'
#' @return A character string containing the protocol_code
#'
#' @export
#' @keywords internal
create_protocol_code <- function(
    protocol_type, theme, protocol_number, language
) {

  reserved_codes$bare <- as.integer(reserved_codes$protocolnumber_bare)
  reserved_codes$theme_number <- ifelse(
    reserved_codes$protocoltype ==  "sfp",
    as.character(str_extract(reserved_codes$protocolnumber_bare, "^\\d")),
    NA)
  bare_numbers <- unique(
    reserved_codes[, c("protocoltype", "theme_number", "bare")])

  if (protocol_type == "sfp" && is.null(protocol_number)) {
    protocol_leading_number <-
      themes_df[themes_df$theme == theme,
                "theme_number"]
    sfp_reserved <- bare_numbers$bare[
      bare_numbers$protocoltype == protocol_type &
        bare_numbers$theme_number == protocol_leading_number] -
      as.numeric(protocol_leading_number) * 100
    all_numbers <- get_protocolnumbers(
      protocol_type = protocol_type,
      language = language)
    theme_numbers <- str_subset(
      all_numbers, paste0("^", protocol_leading_number))
    in_use <- as.numeric(theme_numbers) -
      as.numeric(protocol_leading_number) * 100
    full_sequence <- seq(1, max(sfp_reserved, in_use, 1), 1)
    not_reserved_or_in_use <-
      full_sequence[!full_sequence %in% c(sfp_reserved, in_use)]
    gapfill_number <- min(not_reserved_or_in_use)
    next_number <- max(full_sequence) + 1

    protocol_trailing_number <- max(
      c(1)[length(in_use) == 0],
      gapfill_number[length(not_reserved_or_in_use) > 0],
      next_number[length(not_reserved_or_in_use) == 0]
    )
    protocol_trailing_number <- formatC(
      protocol_trailing_number,
      width = 2, format = "d", flag = "0")
    protocol_number <- paste0(
      protocol_leading_number,
      protocol_trailing_number)
  }
  if (protocol_type == "sfp" && !is.null(protocol_number)) {
    expected_leading_number <-
      themes_df[themes_df$theme == theme,
                "theme_number"]
    observed_leading_number <- str_extract(protocol_number, "^\\d")
    assert_that(expected_leading_number == observed_leading_number)
    sfp_reserved <- as.character(bare_numbers$bare[
      bare_numbers$protocoltype == protocol_type &
        bare_numbers$theme_number == observed_leading_number])
    validate_that(
      protocol_number %in% sfp_reserved,
      msg = cli_alert_danger(
        "The protocol number {protocol_number} is not on the list of
        reserved numbers.
        Are you sure you want to pass a number manually?") |> cli_fmt())
  }
  if (protocol_type %in% c("spp", "sap", "sip", "sop")) {
    reserved <- bare_numbers$bare[
      bare_numbers$protocoltype == protocol_type]
    if (is.null(protocol_number)) {
      all_numbers <- get_protocolnumbers(
        protocol_type = protocol_type,
        language = language)
      in_use <- as.numeric(all_numbers)
      full_sequence <- seq(1, max(reserved, in_use, 1), 1)
      not_reserved_or_in_use <-
        full_sequence[!full_sequence %in% c(reserved, in_use)]
      gapfill_number <- min(not_reserved_or_in_use)
      next_number <- max(full_sequence) + 1

      protocol_number <- max(
        c(1)[length(in_use) == 0],
        gapfill_number[length(not_reserved_or_in_use) > 0],
        next_number[length(not_reserved_or_in_use) == 0]
      )

      protocol_number <- formatC(
        protocol_number,
        width = 3, format = "d", flag = "0")
    } else {
      reserved <- as.character(reserved)
      validate_that(
        protocol_number %in% reserved,
        msg = cli_alert_danger(
          "The protocol number {protocol_number} is not on the list
          of reserved numbers. Are you sure you want to pass a number manually?"
        ) |> cli_fmt()
      )
    }
  }
  protocol_code <- paste(protocol_type, protocol_number, language, sep = "-")
  return(protocol_code)
}



#' Create an Rmarkdown version from an existing `docx` protocol
#'
#' The `docx` file is first converted to a single `Rmd` file with the aid of
#' `pandoc` (called from `convert_docx_to_rmd`).
#' Any emf images are converted to png.
#' Next, the file is split by chapter in multiple `Rmd` files.
#' All graphics files will be stored in a ./media folder.
#' Bookdown compatible captions and cross-references for Figures and Tables are
#' added if and only if `'Figuur'` and `'Tabel'` is used in the original
#' document.
#'
#' @param from_docx A character string with the path (absolute or relative) to
#' a `.docx` file containing a pre-existing protocol.
#' @param path_to_protocol Absolute path to the protocol folder where the
#' protocol created from `docx` needs to be written to
#'
#' @importFrom stringr str_replace str_replace_all str_detect str_remove
#' @export
#' @keywords internal
create_from_docx <- function(
    from_docx,
    path_to_protocol) {
  temp_filename <- "temp.Rmd"
  convert_docx_to_rmd(
    from = from_docx,
    to = file.path(path_to_protocol, temp_filename),
    dir_media = ".",
    wrap = NA,
    overwrite = FALSE,
    verbose = FALSE)
  # add captions
  temp2_filename <- "temp2.Rmd"
  add_captions(
    from = file.path(path_to_protocol, temp_filename),
    to = file.path(path_to_protocol, temp2_filename)
  )
  # move relevant sections
  contents <- readLines(
    con = file.path(
      path_to_protocol,
      temp2_filename))
  # replace absolute path to media folder by relative path
  contents <- str_replace_all(contents, path_to_protocol, ".")
  is_title <- str_detect(string = contents, pattern = "^(#{1}\\s{1})")
  title_numbers <- formatC(
    x = cumsum(is_title),
    width = 2, format = "d", flag = "0")
  filenames <- str_remove(
    string = tolower(contents[is_title]),
    pattern = "^(#{1}\\s{1})")
  filenames <- str_remove(
    string = filenames,
    pattern = "\\s$")
  filenames <- str_replace_all(filenames, pattern = "\\s", replacement = "_")
  filenames <- paste0(unique(title_numbers), "_", filenames, ".Rmd")
  # create new chapters
  file.create(file.path(path_to_protocol, filenames))
  # and add chapter contents from docx
  for (chapter in unique(cumsum(is_title))) {
    chapter_file <- file.path(path_to_protocol, filenames[chapter])
    chapter_contents <- contents[chapter == cumsum(is_title)]
    writeLines(
      text = chapter_contents,
      con = chapter_file)
  }
  # delete the complete Rmd (output of convert_docx_rmd)
  file.remove(file.path(path_to_protocol, temp_filename))
  file.remove(file.path(path_to_protocol, temp2_filename))
}


#' Writes a `_bookdown.yml` file
#'
#' Creates contents from its arguments and writes to file `_bookdown.yml`
#'
#' @param language the language of the book
#' @param book_filename the filename of the book
#' @param path_to_protocol the path to the protocol
#' @param output_dir_rel relative output directory
#'
#' @importFrom ymlthis use_yml_file yml_empty yml_bookdown_opts
#'
#' @noRd
write_bookdown_yml <- function(
    language,
    book_filename,
    path_to_protocol,
    output_dir_rel) {
  # create a character vector with the names of all rmd_files
  # in correct order for compilation
  rmd_files <- c(
    "index.Rmd",
    "NEWS.md",
    list.files(path = path_to_protocol,
               pattern = "^\\d{2}.+Rmd$"))


  if (language == "en") {
    labels <- list(
      fig = 'Figure ', # nolint start
      tab = 'Table ',
      eq = 'Equation ',
      thm = 'Theorem ',
      lem = 'Lemma ',
      def = 'Definition ',
      cor = 'Corrolary ',
      prp = 'Proposition ',
      ex = 'Example ',
      proof = 'Proof. ',
      remark = 'Remark. ')
  }
  if (language == "nl") {
    labels <- list(
      fig = 'Figuur ',
      tab = 'Tabel ',
      eq = 'Vergelijking ',
      thm = 'Theorema ',
      lem = 'Lemma ',
      def = 'Definitie ',
      cor = 'Bijgevolg ',
      prp = 'Propositie ',
      ex = 'Voorbeeld ',
      proof = 'Bewijs. ',
      remark = 'Opmerking. ') # nolint end
  }
  bookdown_yml <- yml_empty()
  bookdown_yml <- yml_bookdown_opts(
    bookdown_yml,
    book_filename = book_filename,
    output_dir = output_dir_rel,
    rmd_files = rmd_files,
    delete_merged_file = TRUE,
    language = list(
      label = labels
    )
  )

  use_yml_file(
    .yml = bookdown_yml,
    path = file.path(path_to_protocol, "_bookdown.yml"),
    quiet = TRUE)
}


#' Writes `_output.yml` file
#'
#' Creates and writes a `_output.yml` file
#'
#' @param language language of the protocol
#' @param path_to_protocol path to the protocol
#'
#' @importFrom ymlthis use_output_yml yml_empty yml_output
#' @importFrom bookdown gitbook pdf_book
#' @importFrom xfun read_utf8 write_utf8
#'
#' @noRd
#'
write_output_yml <- function(language, path_to_protocol) {
  output_yml <- yml_empty()
  if (language == "en") {
    output_yml <- yml_output(
      output_yml,
      bookdown::gitbook(
        split_by = "none",
        split_bib = FALSE,
        template = "!expr protocolhelper:::protocol_css()",
        css = "css/inbo_rapport.css",
        config = list(
          toc = list(
            before = list( # nolint start
              '<li class="toc-logo"><a href="https://www.vlaanderen.be/inbo/en-gb/homepage/"><img src="css/img/inbo-en.jpg"></a></li>',
              '<li class="toc-logo"><a href="https://inbo.github.io/protocols/"><button class="btn"><i class="fa fa-home"></i> Protocols homepage</button></li>'
            ),
            after = list(
              '<li class="cc"><a href="http://creativecommons.org/licenses/by/4.0/"><img src="css/img/cc-by.png"></a></li>'
            )
          )
        )
      ),
      bookdown::pdf_book(
        keep_tex = FALSE,
        pandoc_args = c("--top-level-division=chapter"),
        template = "!expr protocolhelper:::protocol_tex()"
      )
    )
  }
  if (language == "nl") {
    output_yml <- yml_output(
      output_yml,
      bookdown::gitbook(
        split_by = "none",
        split_bib = FALSE,
        template = "!expr protocolhelper:::protocol_css()",
        css = "css/inbo_rapport.css",
        config = list(
          toc = list(
            before = list(
              '<li class="toc-logo"><a href="https://www.vlaanderen.be/inbo/home/"><img src="css/img/inbo-nl.jpg"></a></li>',
              '<li class="toc-logo"><a href="https://inbo.github.io/protocols/"><button class="btn"><i class="fa fa-home"></i> Protocols homepage</button></li>'
            ),
            after = list(
              '<li class="cc"><a href="http://creativecommons.org/licenses/by/4.0/"><img src="css/img/cc-by.png"></a></li>'
            ) # nolint end
          )
        )
      ),
      bookdown::pdf_book(
        keep_tex = FALSE,
        pandoc_args = c("--top-level-division=chapter"),
        template = "!expr protocolhelper:::protocol_tex()"
      )
    )
  }

  use_output_yml(
    .yml = output_yml,
    path = path_to_protocol,
    quiet = TRUE)
  # remove single quotes
  x <- read_utf8(file.path(path_to_protocol, "_output.yml"))
  x <- gsub(pattern = "'", "", x)
  write_utf8(x, file.path(path_to_protocol, "_output.yml"))
}


#' Fills in values from key-value pairs in `yaml` front matter
#'
#' Overwrites the `index.Rmd` file
#'
#' @param parent_rmd original `index.Rmd` file
#' @param path_to_protocol path to the protocol
#' @param protocol_code the protocol code
#' @param protocol_type the protocol type
#' @inheritParams create_protocol
#'
#' @importFrom ymlthis yml_replace yml_discard as_yml yml_author yml_date
#' yml_toplevel use_index_rmd
#' @importFrom rmarkdown yaml_front_matter
#'
#' @noRd
#'
write_yaml_front_matter <- function(
    parent_rmd,
    path_to_protocol,
    title,
    subtitle,
    date,
    version_number,
    protocol_code,
    language,
    protocol_type,
    template,
    theme,
    project_name
) {
  # change values in parent rmarkdown
  index_yml <- yaml_front_matter(parent_rmd)
  unlink("css", recursive = TRUE)
  index_yml <- as_yml(index_yml)
  index_yml <- yml_replace(
    index_yml,
    title = title,
    subtitle = subtitle,
    version_number = version_number,
    protocol_code = protocol_code,
    language = language
  )
  if (is.null(subtitle)) {
    index_yml <- yml_discard(index_yml, "subtitle")
  }
  index_yml <- yml_date(
    index_yml,
    date = date)
  if (protocol_type == "sfp" && template == protocol_type) {
    index_yml <- yml_replace(
      index_yml,
      theme = theme)
  }
  if (protocol_type == "sfp" && template == "generic") {
    index_yml <- yml_toplevel(
      index_yml,
      theme = theme)
  }
  if (protocol_type == "spp") {
    index_yml <- yml_replace(
      index_yml,
      project_name = project_name)
  }
  # set url and github_repo
  index_yml <- yml_toplevel(
    index_yml,
    url = "https://inbo.github.io/protocols/",
    github_repo = "inbo/protocolsource"
  )

  # overwrite old yaml sections

  template_rmd <- file.path(path_to_protocol, "template.rmd")
  file.copy(from = parent_rmd, to = template_rmd)
  unlink(parent_rmd)
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = path_to_protocol,
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE)
  unlink(template_rmd)
}

#' @importFrom assertthat assert_that
author2yaml <- function(author, corresponding = FALSE) {
  assert_that(is.flag(corresponding), noNA(corresponding))
  c(
    "  - name:", sprintf("      given: \"%s\"", author$given),
    sprintf("      family: \"%s\"", author$family)
  ) -> yaml
  if (!is.na(author$email) && author$email != "") {
    yaml <- c(yaml, sprintf("    email: \"%s\"", author$email))
  }
  if (!is.na(author$orcid) && author$orcid != "") {
    yaml <- c(yaml, sprintf("    orcid: \"%s\"", author$orcid))
  }
  if (!is.na(author$affiliation) && author$affiliation != "") {
    yaml <- c(yaml, sprintf("    affiliation: \"%s\"", author$affiliation))
  }
  if (!corresponding) {
    return(paste(yaml, collapse = "\n"))
  }
  assert_that(
    noNA(author$email), author$email != "",
    msg = "please provide an email for the corresponding author"
  )
  paste(c(yaml, "    corresponding: true"), collapse = "\n")
}

#' @importFrom checklist use_author
#' @noRd
use_reviewer <- use_file_manager <- use_author
