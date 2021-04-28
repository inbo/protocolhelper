#' @title Convert a docx-file (a protocol) to an (R)markdown file
#'
#' @description This function is derived from the
#' [`redoc::dedoc()`](https://noamross.github.io/redoc/reference/dedoc.html)
#' function and uses pandoc to convert between docx and markdown.
#' Several options are preset to end-up with a markdown document that is in
#' syntax as close as possible to Rmarkdown files in RStudio.
#' During conversion, graphics (e.g. png, jpg) will be extracted from the docx
#' archive and placed in a folder `./media` and named `image1`, `image2`,
#' etcetera.
#'
#' @details Metadata in the page headers and footers of the docx are ignored
#' and will thus be lost during conversion.
#' In case the header or footer did contain important metadata, it will need to
#' be recovered manually.
#' Usually header information will go inside a yaml section of an Rmarkdown.
#'
#' @param from The `.docx` file to convert.
#' Can be given as an absolute or relative path.
#' @param to The filename to write the resulting `.Rmd` file (without path).
#' The default is to use the same basename as the `.docx` document.
#' @param dir The directory to write the `.Rmd` to.
#' Defaults to current working directory.
#' Any images will be written to `dir/media`.
#' @param wrap The width at which to wrap text.
#' If `NA`, text is not wrapped.
#' Defaults to 80.
#' @param overwrite Whether or not to overwrite the `to` file if it already
#' existed.
#' Defaults to `FALSE`.
#' @param verbose Whether to print pandoc progress text.
#' Defaults to `FALSE`.
#'
#' @importFrom rmarkdown pandoc_convert
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_replace_all
#' @export
convert_docx_to_rmd <- function(
  from,
  to,
  dir,
  wrap = 80,
  overwrite = FALSE,
  verbose = FALSE) {

  if (missing(to)) {
    to <- paste0(file_path_sans_ext(basename(from)), ".Rmd")
  } else {
    assert_that(is.string(to))
  }
  if (missing(dir)) dir <- "."
  to <- file.path(dir, to)
  if (!overwrite && file.exists(to)) stop(to, " exists and overwrite = FALSE")

  md <- pandoc_docx_to_md(from, wrap, dir, verbose)
  md <- str_replace_all(md, pattern = "\\r", replacement = "")

  writeLines(md, con = to)
  return(to)
}

#' @importFrom rmarkdown pandoc_convert
pandoc_docx_to_md <- function(from,
                              wrap,
                              dir,
                              verbose) {
  from <- normalizePath(from)

  if (missing(wrap)) {
    wrap_opts <- "--wrap=none"
  } else {
    wrap_opts <- c("--wrap=auto", paste0("--columns=", wrap))
  }
  filter_opts <- character(0)
  from_format <- "docx"
  other_opts <- c("--standalone",
                  "--atx-headers",
                  paste0("--extract-media=", dir)
                  )
  opts <- c(filter_opts, wrap_opts, other_opts)
  md_tmp <- tempfile(fileext = ".md")
  pandoc_convert(input = from,
                 from = from_format,
                 to = "markdown",
                 output = md_tmp,
                 options = opts,
                 verbose = verbose
  )
  return(readfile(md_tmp))
}

readfile <- function(x) {
  readChar(x, file.info(x)$size)
}
