# Helper function to check if person information is correct

Checks the format of person names and `orcid` ids.

## Usage

``` r
check_all_person_info(person_list, problems_vect)
```

## Arguments

- person_list:

  the yaml front matter part containing person info

- problems_vect:

  character vector of previously encountered problems

## Value

a character vector of previously encountered problems and problems
identified for person names and `orcid` ids.

## See also

Other check:
[`check_all()`](https://inbo.github.io/protocolhelper/reference/check_all.md),
[`check_frontmatter()`](https://inbo.github.io/protocolhelper/reference/check_frontmatter.md),
[`check_structure()`](https://inbo.github.io/protocolhelper/reference/check_structure.md),
[`protocolcheck`](https://inbo.github.io/protocolhelper/reference/protocolcheck.md),
[`validate_orcid()`](https://inbo.github.io/protocolhelper/reference/validate_orcid.md)
