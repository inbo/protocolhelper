# Helper function to create labelled captions for `pander` tables

The function adds a reference label to the caption so that `pander`
tables can be cross-referenced in `bookdown` using the
`\@ref(tab:label)` syntax. The function should only be used in
`pander(x, caption = add_label())`.

## Usage

``` r
add_label(caption = "", tag = "tab")
```

## Arguments

- caption:

  The caption text as a string

- tag:

  The tag to use as a prefix. Default is `tab`.

## Value

The caption text prefixed with a reference label.

## See also

Other utility:
[`get_path_to_protocol()`](https://inbo.github.io/protocolhelper/reference/get_path_to_protocol.md),
[`get_protocol_type()`](https://inbo.github.io/protocolhelper/reference/get_protocol_type.md),
[`get_protocolnumbers()`](https://inbo.github.io/protocolhelper/reference/get_protocolnumbers.md),
[`get_short_titles()`](https://inbo.github.io/protocolhelper/reference/get_short_titles.md),
[`get_version_number()`](https://inbo.github.io/protocolhelper/reference/get_version_number.md),
[`increment_version_number()`](https://inbo.github.io/protocolhelper/reference/increment_version_number.md)

## Examples

``` r
add_label("caption text")
#> [1] "(\\#tab:)caption text"
```
