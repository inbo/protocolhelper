# Create a folder with a bookdown (R markdown) template to start a new protocol and optionally render to html

This function will create a new folder based on values that are passed
on via the parameters and creates a R-markdown (bookdown) skeleton based
on a template file to start working on a new protocol. The function is
(partly) interactive and will ask for the title, optional subtitle, the
authors, reviewers, file manager and keywords. These metadata (YAML
section of `index.Rmd` file) will then be filled in automatically. The
other metadata still need to be passed to the arguments of the function.
See examples section. Optionally, the rmarkdown chapters are rendered to
an html file which will be saved in a matching subfolder of the `docs`
folder.

## Usage

``` r
create_protocol(
  protocol_type = c("sfp", "spp", "sap", "sop", "sip"),
  short_title,
  version_number = get_version_number(),
  theme = NULL,
  project_name = NULL,
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = protocol_type,
  render = FALSE
)

create_sfp(
  short_title,
  version_number = get_version_number(),
  theme = c("generic", "water", "air", "soil", "vegetation", "species"),
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = c("sfp", "generic"),
  render = FALSE
)

create_spp(
  short_title,
  version_number = get_version_number(),
  project_name,
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = c("spp"),
  render = FALSE
)

create_sap(
  short_title,
  version_number = get_version_number(),
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = c("sap", "generic"),
  render = FALSE
)

create_sip(
  short_title,
  version_number = get_version_number(),
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = c("sip", "generic"),
  render = FALSE
)

create_sop(
  short_title,
  version_number = get_version_number(),
  language = c("nl", "en"),
  from_docx = NULL,
  protocol_number = NULL,
  template = c("sop", "generic"),
  render = FALSE
)
```

## Arguments

- protocol_type:

  Either `sfp` (standard field protocol), `spp` ( standard project
  protocol), `sap` (standard analytical protocol), `sip` ( standard
  instrument protocol), `sop` (standard operating protocol)

- short_title:

  A character string of less than 20 characters to use in folder and
  file names

- version_number:

  A version number of the form `YYYY.##`. The default is a function
  which will determine this number automatically. It should normally not
  be changed.

- theme:

  A character string equal to one of `"generic"` (default), `"water"`,
  `"air"`, `"soil"`, `"vegetation"` or `"species"`. It is used as the
  folder location (`source/sfp/theme`) where standard field protocols
  that belong to the same theme will be stored. Ignored if protocol_type
  is other than `"sfp"`.

- project_name:

  A character string that is used as the folder location
  (`source/spp/project_name`) where project-specific protocols that
  belong to the same project will be stored. Preferably a short name or
  acronym. If the folder does not exist, it will be created. Ignored if
  protocol_type is other than `"spp"`.

- language:

  Language of the protocol, either `"nl"` (Dutch), the default, or
  `"en"` (English).

- from_docx:

  A character string with the path (absolute or relative) to a `.docx`
  file containing a pre-existing protocol. Please make sure to
  copy-paste all relevant meta-data from the `.docx` file to the
  corresponding parameters of this function. If nothing is provided
  (i.e. default = NULL), an empty template will be used.

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

- template:

  Which template to use? Default is set equal to protocol_type. However,
  you can also set this to `"generic"` in which case a simplified
  template will be used that can be used as an alternative to the
  default templates.

- render:

  Whether or not to render the protocol to html. Defaults to FALSE.

## Details

It is assumed that the `source` folder is a subfolder of an RStudio
project with git version control. A target folder to which files will be
written will be created as subdirectories beneath `source`. The
subfolder structure is of the form
`/sfp/<theme>/<sfp>_<protocolnumber>_<language>_<short_title>/` for
standard field protocols. Or
`/spp/<project_name>/<spp>_<protocolnumber>_<language>_<short_title>/`
for standard project protocols. Or
`/sip/<sip>_<protocolnumber>_<language>_<short_title>/` for sips (and
analogous for sop and sap). The folder names are determined by the
corresponding arguments of the function. A matching subfolder structure
will be created beneath the `docs` folder (and output files needed for
rendering to html output will be placed in it if `render = TRUE`. The
template Rmarkdown files and the Rmarkdown files that result from
converting a docx protocol (see `from_docx` argument), will be written
to the target folder beneath `source`. Template Rmarkdown files with the
same name as Rmarkdown files that result from converting a docx protocol
will be overwritten by the latter. Besides Rmarkdown files, this target
folder will also contain files needed to render to a Bookdown `gitbook`
such as a `_bookdown.yml` and `_output.yml`. The `NEWS.md` file must be
used to document the changes between revisions of the protocol.
Furthermore, a `data` and a `media` folder will be created as
subdirectories of the target folder. The `media` folder can be used to
store image files and will contain image files extracted from the docx
protocol when the `from_docx` argument is used. The `data` folder can be
used to store tabular data that are needed for the protocol.

## See also

Other creation:
[`add_dependencies()`](https://inbo.github.io/protocolhelper/reference/add_dependencies.md),
[`add_one_subprotocol()`](https://inbo.github.io/protocolhelper/reference/add_one_subprotocol.md),
[`add_subprotocols()`](https://inbo.github.io/protocolhelper/reference/add_subprotocols.md),
[`insert_protocolsection()`](https://inbo.github.io/protocolhelper/reference/insert_protocolsection.md),
[`update_protocol()`](https://inbo.github.io/protocolhelper/reference/update_protocol.md),
[`update_version_number()`](https://inbo.github.io/protocolhelper/reference/update_version_number.md)

## Examples

``` r
if (FALSE) { # \dontrun{
protocolhelper::create_protocol(
  protocol_type = "sfp",
  short_title = "water 1",
  theme = "water", language = "en"
)
} # }
```
