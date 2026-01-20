# Function to add a chapter or a section of a published protocol for re-use in another protocol

The idea is to execute this function in an R chunk with knitr option
`results="asis"`.

## Usage

``` r
insert_protocolsection(
  code_subprotocol,
  version_number,
  file_name,
  section = NULL,
  demote_header = c(0, 1, 2, -1),
  fetch_remote = TRUE
)
```

## Arguments

- code_subprotocol:

  Character string giving the protocol code from which a sub-protocol
  will be made (usually a `sfp`-type protocol)

- version_number:

  Character string with format `YYYY.NN`

- file_name:

  Character string with the name of the Rmarkdown file (a chapter
  starting with a level 1 heading).

- section:

  Optional character string with the name of a section within an
  Rmarkdown file. Can also be a unique substring of a section title. If
  not specified (the default): the whole file is taken. It is assumed
  that the section has a level 2 heading.

- demote_header:

  Number of '#' to prefix to all titles before inserting in current
  protocol. Default is 0. A negative value can be used to remove '#'
  from all section titles. Allowed values are visible in the usage
  section.

- fetch_remote:

  Whether or not to fetch the remote. Default TRUE.

## Value

The function will return Rmarkdown

## See also

Other creation:
[`add_dependencies()`](https://inbo.github.io/protocolhelper/reference/add_dependencies.md),
[`add_one_subprotocol()`](https://inbo.github.io/protocolhelper/reference/add_one_subprotocol.md),
[`add_subprotocols()`](https://inbo.github.io/protocolhelper/reference/add_subprotocols.md),
[`create_protocol()`](https://inbo.github.io/protocolhelper/reference/create_protocol.md),
[`update_protocol()`](https://inbo.github.io/protocolhelper/reference/update_protocol.md),
[`update_version_number()`](https://inbo.github.io/protocolhelper/reference/update_version_number.md)

## Examples

``` r
if (FALSE) { # \dontrun{
insert_protocolsection(
  code_subprotocol = "sfp-401-nl",
  version_number = "2021.01",
  file_name = "07_stappenplan.Rmd"
)
} # }
```
