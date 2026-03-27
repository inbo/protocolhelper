# Checks protocol metadata

This function reads metadata from the yaml front matter stored in the
`index.Rmd` file of a protocol and checks if the metadata format is
correct. This function is intended for checking if a protocol is ready
to be rendered and published (for instance, it will fail if version
number is `YYYY.NN.dev`).

## Usage

``` r
check_frontmatter(protocol_code, fail = !interactive())
```

## Arguments

- protocol_code:

  Character string giving the protocol code

- fail:

  Should the function drop an error in case of a problem? Defaults to
  `TRUE` in a non-interactive session and `FALSE` in an interactive
  session.

## Value

A report of all failed checks.

## See also

Other check:
[`check_all()`](https://inbo.github.io/protocolhelper/reference/check_all.md),
[`check_all_person_info()`](https://inbo.github.io/protocolhelper/reference/check_all_person_info.md),
[`check_structure()`](https://inbo.github.io/protocolhelper/reference/check_structure.md),
[`protocolcheck`](https://inbo.github.io/protocolhelper/reference/protocolcheck.md),
[`validate_orcid()`](https://inbo.github.io/protocolhelper/reference/validate_orcid.md)
