# Get protocol type from protocol code

The protocol type corresponds to the first 3 letters of the protocol
code

## Usage

``` r
get_protocol_type(protocol_code, labels = TRUE, auto_identifier = FALSE)
```

## Arguments

- protocol_code:

  Character vector giving the protocol code(s)

- labels:

  Logical. If `TRUE` return full labels, else return just the three
  letter abbreviation.

- auto_identifier:

  Logical. If `TRUE` returns labels following [`Pandoc's`
  auto-identifier](https://pandoc.org/MANUAL.html#extension-auto_identifiers)
  rules.

## Value

A factor with 5 levels corresponding to `sfp`, `sip`, `sap`, `sop` and
`spp`. The labels depend on `auto_identifier` setting.

## See also

Other utility:
[`add_label()`](https://inbo.github.io/protocolhelper/reference/add_label.md),
[`get_path_to_protocol()`](https://inbo.github.io/protocolhelper/reference/get_path_to_protocol.md),
[`get_protocolnumbers()`](https://inbo.github.io/protocolhelper/reference/get_protocolnumbers.md),
[`get_short_titles()`](https://inbo.github.io/protocolhelper/reference/get_short_titles.md),
[`get_version_number()`](https://inbo.github.io/protocolhelper/reference/get_version_number.md),
[`increment_version_number()`](https://inbo.github.io/protocolhelper/reference/increment_version_number.md)
