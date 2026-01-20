# Create an Rmarkdown version from an existing `docx` protocol

The `docx` file is first converted to a single `Rmd` file with the aid
of `pandoc` (called from `convert_docx_to_rmd`). Any emf images are
converted to png. Next, the file is split by chapter in multiple `Rmd`
files. All graphics files will be stored in a ./media folder. Bookdown
compatible captions and cross-references for Figures and Tables are
added if and only if `'Figuur'` and `'Tabel'` is used in the original
document.

## Usage

``` r
create_from_docx(from_docx, path_to_protocol)
```

## Arguments

- from_docx:

  A character string with the path (absolute or relative) to a `.docx`
  file containing a pre-existing protocol.

- path_to_protocol:

  Absolute path to the protocol folder where the protocol created from
  `docx` needs to be written to
