# Convert a docx-file (a protocol) to an (R)markdown file

This function is derived from the `redoc::dedoc()` function and uses
`pandoc` to convert between docx and markdown. Several options are
preset to end-up with a markdown document that is in syntax as close as
possible to Rmarkdown files in RStudio. During conversion, graphics
(e.g. png, jpg) will be extracted from the docx archive and placed in a
folder `./media` and named `image1`, `image2`, etcetera. Additionally,
.emf files will be converted to .png.

## Usage

``` r
convert_docx_to_rmd(
  from,
  to = sub("docx$", "Rmd", from),
  dir_media = ".",
  wrap = NA,
  overwrite = FALSE,
  verbose = FALSE,
  wd = getwd()
)
```

## Arguments

- from:

  The `.docx` file to convert. Can be given as an absolute or relative
  path.

- to:

  The filename including path to write the resulting `.Rmd` file. The
  default is to use the same name and path as the `.docx` document.

- dir_media:

  The directory to write the folder `media` with images to, relative to
  the folder where the `.Rmd` is written. Defaults to '.' (the path
  where the `.Rmd` file is written).

- wrap:

  The width at which to wrap text. If `NA` (default), text is not
  wrapped.

- overwrite:

  Whether or not to overwrite the `to` file if it already existed.
  Defaults to `FALSE`.

- verbose:

  Whether to print `pandoc` progress text. Defaults to `FALSE`.

- wd:

  Current working directory (used to handle relative paths).

## Details

Metadata in the page headers and footers of the docx are ignored and
will thus be lost during conversion. In case the header or footer did
contain important metadata, it will need to be recovered manually.
Usually header information will go inside a yaml section of an
Rmarkdown.

## See also

Other convert:
[`add_captions()`](https://inbo.github.io/protocolhelper/reference/add_captions.md)
