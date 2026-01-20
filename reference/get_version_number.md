# Get version number for a protocol

Looks up pre-existing version numbers of protocols in the main branch
and calculates an incremented (next) version number for the currently
checkout branch containing the created/in development/updated/ready to
be released protocol. Your local main branch needs to up to date
(aligned with remote) for this. If this is not the case, or other issues
are detected regarding a non-clean local git repository - informative
error messages will be given.

## Usage

``` r
get_version_number(path = ".")
```

## Arguments

- path:

  Defaults to current working directory. This should correspond with the
  root directory of the `protocolsource` repo.

## Value

A string containing the next (incremented) version number

## See also

Other utility:
[`add_label()`](https://inbo.github.io/protocolhelper/reference/add_label.md),
[`get_path_to_protocol()`](https://inbo.github.io/protocolhelper/reference/get_path_to_protocol.md),
[`get_protocol_type()`](https://inbo.github.io/protocolhelper/reference/get_protocol_type.md),
[`get_protocolnumbers()`](https://inbo.github.io/protocolhelper/reference/get_protocolnumbers.md),
[`get_short_titles()`](https://inbo.github.io/protocolhelper/reference/get_short_titles.md),
[`increment_version_number()`](https://inbo.github.io/protocolhelper/reference/increment_version_number.md)
