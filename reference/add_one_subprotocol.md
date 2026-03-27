# Helper function to add one sub-protocol to a project-specific protocol of which it is a dependency

The function renders the sub-protocol to
[`bookdown::markdown_document2()`](https://pkgs.rstudio.com/bookdown/reference/html_document2.html)
and saves the resulting `md` file (and any associated media and data
files) in a subfolder of the directory of the project-specific protocol.
This function should normally not be called directly. Use
[`add_subprotocols()`](https://inbo.github.io/protocolhelper/reference/add_subprotocols.md)
instead.

## Usage

``` r
add_one_subprotocol(
  code_subprotocol,
  version_number,
  params2 = NULL,
  code_mainprotocol,
  fetch_remote = TRUE
)
```

## Arguments

- code_subprotocol:

  Character string giving the protocol code from which a sub-protocol
  will be made (usually a `sfp`-type protocol)

- version_number:

  Character string with format `YYYY.NN`

- params2:

  A list of parameter key-value pairs.

- code_mainprotocol:

  Character string giving the protocol code for the main protocol

- fetch_remote:

  Whether or not to fetch the remote. Default TRUE.

## See also

Other creation:
[`add_dependencies()`](https://inbo.github.io/protocolhelper/reference/add_dependencies.md),
[`add_subprotocols()`](https://inbo.github.io/protocolhelper/reference/add_subprotocols.md),
[`create_protocol()`](https://inbo.github.io/protocolhelper/reference/create_protocol.md),
[`insert_protocolsection()`](https://inbo.github.io/protocolhelper/reference/insert_protocolsection.md),
[`update_protocol()`](https://inbo.github.io/protocolhelper/reference/update_protocol.md),
[`update_version_number()`](https://inbo.github.io/protocolhelper/reference/update_version_number.md)

## Examples

``` r
if (FALSE) { # \dontrun{
add_subprotocols(code_mainprotocol = "spp-999-en")
} # }
```
