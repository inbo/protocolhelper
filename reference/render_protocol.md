# Function to render a protocol to html and pdf.

This function is a simple wrapper around
[`bookdown::render_book()`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)
and can be used to render a protocol to html and pdf in order to preview
updates that have been made.

## Usage

``` r
render_protocol(protocol_code = NULL, output_dir = NULL, ...)
```

## Arguments

- protocol_code:

  Character string giving the protocol code

- output_dir:

  The output directory. If `NULL`, a field named `output_dir` in the
  configuration file `_bookdown.yml` will be used (possibly not
  specified, either, in which case a directory name `_book` will be
  used).

- ...:

  additional parameters passed on to
  [`bookdown::render_book()`](https://pkgs.rstudio.com/bookdown/reference/render_book.html)

## Details

The rendered html and pdf file and associated files needed by the html
file will be put in the directory implied by the `output_dir` parameter.

## Examples

``` r
if (FALSE) { # \dontrun{
render_protocol(protocol_code = "sfp_401-nl")
} # }
```
