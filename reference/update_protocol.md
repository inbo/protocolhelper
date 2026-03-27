# Preparatory steps to start the update of a pre-existing version of a protocol

The function creates a branch with the same name as the `protocol_code`
and checks out that branch. Next, it will update the version number in
the YAML of `index.Rmd` and update the protocol-specific `NEWS.md` file.

## Usage

``` r
update_protocol(protocol_code)
```

## Arguments

- protocol_code:

  Character string giving the protocol code

## Value

NULL invisibly

## See also

Other creation:
[`add_dependencies()`](https://inbo.github.io/protocolhelper/reference/add_dependencies.md),
[`add_one_subprotocol()`](https://inbo.github.io/protocolhelper/reference/add_one_subprotocol.md),
[`add_subprotocols()`](https://inbo.github.io/protocolhelper/reference/add_subprotocols.md),
[`create_protocol()`](https://inbo.github.io/protocolhelper/reference/create_protocol.md),
[`insert_protocolsection()`](https://inbo.github.io/protocolhelper/reference/insert_protocolsection.md),
[`update_version_number()`](https://inbo.github.io/protocolhelper/reference/update_version_number.md)
