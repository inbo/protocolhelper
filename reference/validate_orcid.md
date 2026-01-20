# validate an ORCID string

Generates check digit as per ISO 7064 11,2. The last character in the
`ORCID ID` is a checksum. In accordance with ISO/IEC 7064:2003, MOD
11-2, this checksum must be "0-9" or "X", a capital letter X which
represents the value 10.

## Usage

``` r
validate_orcid(orcid)
```

## Arguments

- orcid:

  An `orcid` in the `0000-0000-0000-0000` format

## Value

Logical. TRUE if check digit is correct.

## See also

Other check:
[`check_all()`](https://inbo.github.io/protocolhelper/reference/check_all.md),
[`check_all_person_info()`](https://inbo.github.io/protocolhelper/reference/check_all_person_info.md),
[`check_frontmatter()`](https://inbo.github.io/protocolhelper/reference/check_frontmatter.md),
[`check_structure()`](https://inbo.github.io/protocolhelper/reference/check_structure.md),
[`protocolcheck`](https://inbo.github.io/protocolhelper/reference/protocolcheck.md)

## Examples

``` r
validate_orcid("0000-0002-6378-6229")
#> [1] TRUE
```
