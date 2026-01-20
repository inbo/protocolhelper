# Checks protocol document structure

This function reads the protocol and checks if the document structure is
correct: chunks have head and tail, required headings are present and in
the right order,... This function is intended for checking if a protocol
is ready to be rendered and published.

## Usage

``` r
check_structure(protocol_code, fail = !interactive())
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
[`check_frontmatter()`](https://inbo.github.io/protocolhelper/reference/check_frontmatter.md),
[`protocolcheck`](https://inbo.github.io/protocolhelper/reference/protocolcheck.md),
[`validate_orcid()`](https://inbo.github.io/protocolhelper/reference/validate_orcid.md)
