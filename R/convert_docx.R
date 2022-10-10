#' @title Convert a docx-file (a protocol) to an (R)markdown file
#'
#' @description This function is derived from the
#' [`redoc::dedoc()`](https://noamross.github.io/redoc/reference/dedoc.html)
#' function and uses `pandoc` to convert between docx and markdown.
#' Several options are preset to end-up with a markdown document that is in
#' syntax as close as possible to Rmarkdown files in RStudio.
#' During conversion, graphics (e.g. png, jpg) will be extracted from the docx
#' archive and placed in a folder `./media` and named `image1`, `image2`,
#' etcetera.
#' Additionally, .emf files will be converted to .png.
#'
#' @details Metadata in the page headers and footers of the docx are ignored
#' and will thus be lost during conversion.
#' In case the header or footer did contain important metadata, it will need to
#' be recovered manually.
#' Usually header information will go inside a yaml section of an Rmarkdown.
#'
#' @param from The `.docx` file to convert.
#' Can be given as an absolute or relative path.
#' @param to The filename including path to write the resulting `.Rmd` file.
#' The default is to use the same name and path as the `.docx` document.
#' @param dir_media The directory to write the folder `media` with images to,
#' relative to the folder where the `.Rmd` is written.
#' Defaults to '.' (the path where the `.Rmd` file is written).
#' @param wrap The width at which to wrap text.
#' If `NA` (default), text is not wrapped.
#' @param overwrite Whether or not to overwrite the `to` file if it already
#' existed.
#' Defaults to `FALSE`.
#' @param verbose Whether to print `pandoc` progress text.
#' Defaults to `FALSE`.
#' @param wd Current working directory (used to handle relative paths).
#'
#' @importFrom rmarkdown pandoc_convert
#' @importFrom stringr str_replace_all
#' @importFrom assertthat assert_that
#' @importFrom fs path_rel
#' @export
convert_docx_to_rmd <- function(
  from,
  to = sub("docx$", "Rmd", from),
  dir_media = ".",
  wrap = NA,
  overwrite = FALSE,
  verbose = FALSE,
  wd = getwd()) {

  assert_that(is.string(from))
  assert_that(grepl("\\.docx$", from))
  assert_that(is.string(to))
  assert_that(grepl("\\.Rmd$", to))

  dir_to <- dirname(to)
  dir_to <- path_rel(dir_to, wd)
  wd <- file.path(wd, dir_to)
  if (!dir.exists(wd)) dir.create(wd, recursive = TRUE)
  if (!overwrite && file.exists(to)) stop(to, " exists and overwrite = FALSE")

  md <- pandoc_docx_to_md(from, wrap, dir_media, verbose, wd)
  md <- str_replace_all(md, pattern = "\\r", replacement = "")

  # convert emf to png
  emf_images <- list.files(path = file.path(wd,
                                            ifelse(dir_media == ".",
                                                   "",
                                                   dir_media),
                                            "media"),
                           pattern = ".emf",
                           full.names = TRUE)
  if (length(emf_images) > 0) {
    if (!requireNamespace("magick", quietly = TRUE)) {
      stop("Package \"magick\" needed for docx protocols with emf images. ",
           "Please install it with 'install.packages(\"magick\")'.",
           call. = FALSE)
    }
    for (img in emf_images) {
      img_emf <- magick::image_read(path = img)
      magick::image_write(image = img_emf,
                          format = "png",
                          path = str_replace(img, ".emf", ".png"))
      file.remove(img)
    }
  }
  md <- str_replace_all(md, "\\.emf", ".png")
  md <- str_replace_all(md, "%5C", "/")

  writeLines(md, con = to)
  return(to)
}

#' @importFrom rmarkdown pandoc_convert
pandoc_docx_to_md <- function(from,
                              wrap,
                              dir,
                              verbose,
                              wd) {
  from <- normalizePath(from)

  if (is.na(wrap)) {
    wrap_opts <- "--wrap=none"
  } else {
    wrap_opts <- c("--wrap=auto", paste0("--columns=", wrap))
  }
  filter_opts <- character(0)
  from_format <- "docx"
  other_opts <- c("--standalone",
                  "--markdown-headings=atx",
                  paste0("--extract-media=", dir)
                  )
  opts <- c(filter_opts, wrap_opts, other_opts)
  md_tmp <- tempfile(fileext = ".md")
  pandoc_convert(input = from,
                 from = from_format,
                 to = "markdown",
                 output = md_tmp,
                 options = opts,
                 verbose = verbose,
                 wd = wd
  )
  return(readfile(md_tmp))
}

readfile <- function(x) {
  readChar(x, file.info(x)$size)
}
