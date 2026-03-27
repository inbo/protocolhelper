# Adds dependencies to the YAML of an `index.Rmd` file

Adds dependencies to the YAML of an `index.Rmd` file

## Usage

``` r
add_dependencies(
  code_mainprotocol,
  protocol_code,
  version_number,
  params,
  appendix = !is.na(params)
)
```

## Arguments

- code_mainprotocol:

  Protocol code of the protocol for which dependencies need to be
  declared in the YAML of its `index.Rmd` file

- protocol_code:

  Character vector of protocol codes that are dependencies to the main
  protocol.

- version_number:

  Character vector of version numbers corresponding with protocol_code.

- params:

  List of lists with protocol-specific parameters corresponding with
  parameters from the protocols in protocol_code. Use `NA` if no
  parameters should be set for a protocol.

- appendix:

  Logical vector indicating whether or not a dependency needs to be
  included as a subprotocol (at the end of the main protocol in an
  appendix). Default is `!is.na(params)`. When `params` is not `NA`,
  `appendix` will always be set to `TRUE` even if the user passes
  another value.

## See also

Other creation:
[`add_one_subprotocol()`](https://inbo.github.io/protocolhelper/reference/add_one_subprotocol.md),
[`add_subprotocols()`](https://inbo.github.io/protocolhelper/reference/add_subprotocols.md),
[`create_protocol()`](https://inbo.github.io/protocolhelper/reference/create_protocol.md),
[`insert_protocolsection()`](https://inbo.github.io/protocolhelper/reference/insert_protocolsection.md),
[`update_protocol()`](https://inbo.github.io/protocolhelper/reference/update_protocol.md),
[`update_version_number()`](https://inbo.github.io/protocolhelper/reference/update_version_number.md)

## Examples

``` r
if (FALSE) { # \dontrun{
protocolhelper::add_dependencies(
  code_mainprotocol = "spp-999-en",
  protocol_code = c("sfp-123-en", "spp-124-en"),
  version_number = c("2020.01", "2020.02"),
  params = list(NA, list(width = 8, height = 8))
)
} # }
```
