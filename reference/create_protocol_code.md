# Create protocol code from it's components

A protocol code of format `s[fpioa]p-###-[nl|en]` will be created. The
number will be determined automatically based on theme (in case of
`sfp`) and a rank order of all existing and reserved protocol numbers,
unless the protocol number is passed directly to the `protocol_number`
argument.

## Usage

``` r
create_protocol_code(protocol_type, theme, protocol_number, language)
```

## Arguments

- protocol_type:

  Either `sfp` (standard field protocol), `spp` ( standard project
  protocol), `sap` (standard analytical protocol), `sip` ( standard
  instrument protocol), `sop` (standard operating protocol)

- theme:

  A character string equal to one of `"generic"` (default), `"water"`,
  `"air"`, `"soil"`, `"vegetation"` or `"species"`. It is used as the
  folder location (`source/sfp/theme`) where standard field protocols
  that belong to the same theme will be stored. Ignored if protocol_type
  is other than `"sfp"`.

- protocol_number:

  A character string giving the protocol number. This parameter should
  normally not be specified (i.e. NULL), unless `from_docx` is
  specified. A protocol number is a three digit string where the first
  digit corresponds with a theme and the last two digits identify a
  protocol within a theme for standard field protocols. A protocol
  number for other protocol types is just a three digit string. If NULL
  (the default), a protocol number will be determined automatically
  based on pre-existing protocol numbers. Note that for backwards
  compatibility with protocol numbers that were already in use at INBO,
  we made a list of reserved numbers. These reserved numbers will not be
  used when `protocol_number` is NULL. The only time you will need to
  explicitly pass a protocol number to the `protocol_number` argument is
  when you want to migrate a pre-existing INBO protocol to
  `protocolsource` and hence use one of the reserved numbers. Protocol
  numbers that are already in use in `protocolsource` can be retrieved
  with
  [`get_protocolnumbers()`](https://inbo.github.io/protocolhelper/reference/get_protocolnumbers.md).

- language:

  Language of the protocol, either `"nl"` (Dutch), the default, or
  `"en"` (English).

## Value

A character string containing the protocol_code
