# Function to get (or set) the full path to a protocol

A function that is used by other functions and should normally not be
used directly.

For existing protocol codes, arguments `theme` and `project_name` are
always ignored. The function will return the absolute path for that
protocol.

For new `sfp` or `spp` protocols, also either the `theme` or the
`project_name` argument and `short_title` are required apart from the
`protocol_code`. The function will construct the absolute path where the
source code for that new protocol will be written.

## Usage

``` r
get_path_to_protocol(
  protocol_code,
  theme = NULL,
  project_name = NULL,
  short_title = NULL
)
```

## Arguments

- protocol_code:

  Character string giving the protocol code

- theme:

  A character string equal to one of `"generic"`, `"water"`, `"air"`,
  `"soil"`, `"vegetation"` or `"species"`. Defaults to NULL. Only needed
  if no folder with the name of the protocol code exists and the request
  is for a `sfp` protocol.

- project_name:

  Character string giving the name of the project folder. Defaults to
  NULL. Only needed if no folder with the name of the protocol code
  exists and the request is for a `spp` protocol.

- short_title:

  A character string of less than 20 characters to use in folder and
  filenames. Defaults to NULL. Only needed if no folder with the name of
  the protocol code exists.

## Value

A character vector containing the full path to the protocol.

## See also

Other utility:
[`add_label()`](https://inbo.github.io/protocolhelper/reference/add_label.md),
[`get_protocol_type()`](https://inbo.github.io/protocolhelper/reference/get_protocol_type.md),
[`get_protocolnumbers()`](https://inbo.github.io/protocolhelper/reference/get_protocolnumbers.md),
[`get_short_titles()`](https://inbo.github.io/protocolhelper/reference/get_short_titles.md),
[`get_version_number()`](https://inbo.github.io/protocolhelper/reference/get_version_number.md),
[`increment_version_number()`](https://inbo.github.io/protocolhelper/reference/increment_version_number.md)

## Examples

``` r
if (FALSE) { # \dontrun{
get_path_to_protocol(protocol_code = "sfp-401-nl")
} # }
```
