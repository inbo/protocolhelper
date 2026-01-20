# Render all sub-protocols belonging to a main protocol to single markdown files

The function should be called interactively (in the console) after the
dependencies section in the `YAML` header of the `index.Rmd` file of the
main protocol has been filled in with the aid of the
[`protocolhelper::add_dependencies()`](https://inbo.github.io/protocolhelper/reference/add_dependencies.md)
function. For reproducibility, it is good practice to save the call in a
separate R script. For each sub-protocol a single markdown file and
associated media and data files will be written. Each sub-protocol will
be written to a subfolder of the main protocol. The subfolder name is
the same as the version number of the sub-protocol.

## Usage

``` r
add_subprotocols(code_mainprotocol, fetch_remote = TRUE)
```

## Arguments

- code_mainprotocol:

  Character string giving the protocol code for the main protocol

- fetch_remote:

  Whether or not to fetch the remote. Default TRUE.

## See also

Other creation:
[`add_dependencies()`](https://inbo.github.io/protocolhelper/reference/add_dependencies.md),
[`add_one_subprotocol()`](https://inbo.github.io/protocolhelper/reference/add_one_subprotocol.md),
[`create_protocol()`](https://inbo.github.io/protocolhelper/reference/create_protocol.md),
[`insert_protocolsection()`](https://inbo.github.io/protocolhelper/reference/insert_protocolsection.md),
[`update_protocol()`](https://inbo.github.io/protocolhelper/reference/update_protocol.md),
[`update_version_number()`](https://inbo.github.io/protocolhelper/reference/update_version_number.md)
