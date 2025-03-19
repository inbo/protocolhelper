# protocolhelper 0.7.0

## Enhancements

* `create_protocol` now works interactively (#117)
* we make use of `checklist::use_author()` for author, reviewer and file_manager
  fields in YAML.

## Breaking change

* the author, reviewer and file_manager YAML fields now have a different format

# protocolhelper 0.6.1

## Bug fixes

* Overwriting placeholder headers resulted in false alarm from `check_structure`
  (issue #124)

## Enhancements

* Added appendix chapters to `spp` templates (issue #121)

# protocolhelper 0.6.0

## Bug fixes

* Updates were needed to `LaTeX` template to comply with `pandoc`  3.1.8+
  (issue #122)

## Enhancements

* internal reserved codes list has been augmented with codes from genetics lab
* improved error and other messages for `create_protocol()`

# protocolhelper 0.5.1

## Enhancements

* removed html comments from templates beneath part headers, which causes layout
  issues when rendering
* automatically update version numbers in protocol-specific `NEWS.md` files

# protocolhelper 0.5.0

## New features

* new function `add_label` for use with caption argument of `pander` tables

## Enhancements

* better handle pre-existing protocol numbers

# protocolhelper 0.4.13

## Minor changes

* internal function `render_release()` now also writes a `.zenodo.json` file
* protocol authors are now added as contributors instead of creators in 
  `.zenodo.json` file

# protocolhelper 0.4.12

## Minor changes

* switched theme number of air and soil protocols

# protocolhelper 0.4.11

## Minor changes

* removed unneeded docker file
* improved layout of website overview tables

# protocolhelper 0.4.10

## Minor changes

* improved code to generate metadata in templates

# protocolhelper 0.4.9

## Bug fixes

* fixed a bug in protocol code check in `check_frontmatter()`

# protocolhelper 0.4.8

## Minor changes

* various improvements to website homepage, including Dutch and English version

## Bug fixes

* fixed bug in passing metadata to `pandoc`

# protocolhelper 0.4.7

## Minor changes

* removed `_bookdown.yml` template files which are now created programmatically
* website contains a NEWS page
* each protocol now has a home button to return to protocols website homepage

## Bug fixes

* fixes a problem that caused references to appear twice in bibliography in case
  of subprotocols

# protocolhelper 0.4.6

## Bug fixes

* fixes a problem in `check_structure` when a subprotocol is present

# protocolhelper 0.4.5

## Minor changes

* new function `check_all` which combines `frontmatter` and `structure` checks

## Bug fixes

* fixes a problem in ´add_one_subprotocol()´ so the correct path to media and
  data is used
* fixes a problem with bibliography field when a subprotocol is added
* fixes a problem with figure and table cross-references in case of a
  subprotocol

# protocolhelper 0.4.4

## Minor changes

* fixes a problem with checking of dates
* improve error message when URL in `NEWS.md` is incorrect

# protocolhelper 0.4.3

## Minor changes

* all `(R)md` template files are rewritten in canonical (`pandoc`) markdown
  format
* fix check for code chunks which failed on html blocks

# protocolhelper 0.4.2

## Minor changes

* better error handling in `check_structure` and `check_frontmatter`

# protocolhelper 0.4.1

## Minor changes

* fixed a bug which occurred when `check_structure` and `check_frontmatter` ran
  on protocols based on the `generic` template
* each template gains a `template_name` metadata field
* added `pandoc` minimal system requirements to description
* each template now has a `path_to_protocol` R object defined which resolves
  to the full path of the protocol

# protocolhelper 0.4.0

## breaking changes

* reworked folder structure (`src` becomes `source`, `thematic` becomes `sfp`,
  `project` becomes `spp`) #82

## New features

* added a generic template which can be used as alternative to `sfp`, `sip`,
  `sap` and `sop` templates #78
* changed create_protocol to handle sap, sip, sop and generic templates
* handle reserved (in use) protocol codes from lab and field work inventory
  lists which have not been migrated to `protocolsource`

## Minor changes

* reworked internal function `protocolhelper:::render_release` to improve
  homepage of website #84
* fixed numerous spelling issues

# protocolhelper 0.3.2

* patch to update checklist machinery

# protocolhelper 0.3.1

## Minor changes

* `update_version_number` only commits a modified `Index.Rmd`
* `get_version_number` temporarily stashes changes so they don't get lost when
  switching between branches

# protocolhelper 0.3.0

## New features

* templates now also have a `references.yaml` file and corresponding
  bibliography fields in yaml front matter
* added templates for standard analysis procedures (sap)
* added templates for standard instrument procedures (sip)
* added templates for standard operating procedures (sop)
* added function `check_structure()` to check document structure of protocols
* added function `get_version_number()` to determine version number

## Minor changes

* various fixes to comply with new version of `checklist` package
* improved checks when passing authors and `orcids` in `create_protocol()`
* some template markdown files or chapter titles have been changed so that file
  name and chapter title are similar
* functions `clean_git` and `new_branch` are now imported from `checklist`
  package

## Bug fixes

* fix a problem with backslashes in path to media files after converting docx to
  markdown

# protocolhelper 0.2.3

* fix problem in `github` action workflow (install more recent version of
  `pandoc`)

# protocolhelper 0.2.2

## New features

* function to update a protocol (start development of a new version)
* internal function to get protocol tags and optionally bump development version
  number (#27)
* internal function to update `.zenodo.json` file (add new authors) (#25)

## Minor changes

* fixed relative links in template `NEWS.md` html comments
* removed function render_all(); it is superseded by the internal function 
  protocolhelper:::render_release(). This function is internal because it should
  only be used by administrators as part of the workflow to publish a new
  protocol or update an existing protocol. The exported function
  protocolhelper::render_protocol() can be used to render an individual
  protocol.

## Bug fixes

* fixed bugs in `check_frontmatter()` due to changes in the way YAML metadata
  are stored

# protocolhelper 0.2.1

## New features

* protocol templates include CC-BY license

## Minor changes

* arguments determining paths to save results of `convert_docx_to_rmd()` are
  reconsidered (#52 and #53)
* change default of argument wrap in `convert_docx_to_rmd()` to NA
* .emf files will converted to .png in `convert_docx_to_rmd()`

# protocolhelper 0.2.0

## Breaking changes

* restructured yaml of `skeleton.Rmd` in templates: moved metadata under
  `params` section to top level of yaml
* new YAML syntax to add dependencies to the `params` section the YAML

## New features

* adds `orcid` information (#32)
* adds a link to dependencies table when `params` are missing (#46)
* adds a new function to aid adding dependencies to YAML header
* adds a logo to the sidebar (#44)

## Bug fixes

* remove unnecessary failing check from check on different OS (#49)

# protocolhelper 0.1.5

* adds function add_captions()

# protocolhelper 0.1.4

* fixes a bug introduced in 0.1.3

# protocolhelper 0.1.3

* When a protocol is created from an existing docx version, template files will
  be generated as well (and overwritten by the docx chapter if the filename is
  the same, i.e. confirms to current template standards)

# protocolhelper 0.1.2

* removed redundant pkgdown workflow

# protocolhelper 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Minor code improvements to meet quality criteria of checklist::check_package()

# protocolhelper 0.1.0

* Added new function `add_subprotocols()`
