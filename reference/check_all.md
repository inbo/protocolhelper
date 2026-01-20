# Check protocol `frontmatter` and `structure`

Combines
[`check_frontmatter()`](https://inbo.github.io/protocolhelper/reference/check_frontmatter.md)
and
[`check_structure()`](https://inbo.github.io/protocolhelper/reference/check_structure.md)
in one function.

## Usage

``` r
check_all(protocol_code, fail = !interactive())
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
[`check_all_person_info()`](https://inbo.github.io/protocolhelper/reference/check_all_person_info.md),
[`check_frontmatter()`](https://inbo.github.io/protocolhelper/reference/check_frontmatter.md),
[`check_structure()`](https://inbo.github.io/protocolhelper/reference/check_structure.md),
[`protocolcheck`](https://inbo.github.io/protocolhelper/reference/protocolcheck.md),
[`validate_orcid()`](https://inbo.github.io/protocolhelper/reference/validate_orcid.md)
