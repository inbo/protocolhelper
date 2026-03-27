# Function to list all short titles that are already in use.

This function will search for short titles in filenames of Rmarkdown
files listed underneath the source folder. The search will be restricted
to files of a given protocol type and given language.

## Usage

``` r
get_short_titles(
  protocol_type = c("sfp", "sip", "sap", "sop", "spp"),
  language = c("nl", "en")
)
```

## Arguments

- protocol_type:

  A character string equal to `sfp` (default), `sip`, `sap`, `sop` or
  `spp`.

- language:

  Language of the protocol, either `"nl"` (Dutch), the default, or
  `"en"` (English).

## Value

A character vector with short titles that are in use for a given
protocol type.

## See also

Other utility:
[`add_label()`](https://inbo.github.io/protocolhelper/reference/add_label.md),
[`get_path_to_protocol()`](https://inbo.github.io/protocolhelper/reference/get_path_to_protocol.md),
[`get_protocol_type()`](https://inbo.github.io/protocolhelper/reference/get_protocol_type.md),
[`get_protocolnumbers()`](https://inbo.github.io/protocolhelper/reference/get_protocolnumbers.md),
[`get_version_number()`](https://inbo.github.io/protocolhelper/reference/get_version_number.md),
[`increment_version_number()`](https://inbo.github.io/protocolhelper/reference/increment_version_number.md)

## Examples

``` r
if (FALSE) { # \dontrun{
get_short_titles()
} # }
```
