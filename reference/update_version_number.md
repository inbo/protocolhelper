# Updates the version number in the YAML section of a protocol `index.Rmd` file and optionally in protocol `NEWS.md`

Makes use of `get_version_number` to get a new version number and
changes this accordingly in the YAML section of `index.Rmd` file and
optionally in `NEWS.md`.

## Usage

``` r
update_version_number(
  protocol_code,
  commit = TRUE,
  update_news = TRUE,
  path = "."
)
```

## Arguments

- protocol_code:

  The protocol_code corresponding with the name of the branch that
  contains the new or updated protocol.

- commit:

  Logical. Default TRUE. Whether or not to add and commit the changes to
  the protocol branch

- update_news:

  Logical. Default TRUE. Whether or not to find and replace old version
  number by new version number in the `NEWS.md` heading 2.

- path:

  Default is current working directory. Should correspond with root
  directory of `protocolsource` repo.

## Value

TRUE if version number in yaml is updated. FALSE otherwise.

## See also

Other creation:
[`add_dependencies()`](https://inbo.github.io/protocolhelper/reference/add_dependencies.md),
[`add_one_subprotocol()`](https://inbo.github.io/protocolhelper/reference/add_one_subprotocol.md),
[`add_subprotocols()`](https://inbo.github.io/protocolhelper/reference/add_subprotocols.md),
[`create_protocol()`](https://inbo.github.io/protocolhelper/reference/create_protocol.md),
[`insert_protocolsection()`](https://inbo.github.io/protocolhelper/reference/insert_protocolsection.md),
[`update_protocol()`](https://inbo.github.io/protocolhelper/reference/update_protocol.md)
