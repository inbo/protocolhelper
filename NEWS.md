# protocolhelper 0.2.4

* various fixes to comply with new version of `checklist` package

# protocolhelper 0.2.3

* fix problem in github action workflow (install more recent version of pandoc)

# protocolhelper 0.2.2

## New features

* function to update a protocol (start development of a new version)
* internal function to get protocol tags and optionally bump development version
  number (#27)
* internal function to update .zenodo.json file (add new authors) (#25)

## Minor changes

* fixed relative links in template NEWS.md html comments
* removed function render_all(); it is superseded by the internal function
  protocolhelper:::render_release().
  This function is internal because it should only be used by administrators
  as part of the workflow to publish a new protocol or update an existing
  protocol.
  The exported function protocolhelper::render_protocol() can be used to render
  an individual protocol.

## Bug fixes

* fixed bugs in check_frontmatter() due to changes in the way YAML metadata are
  stored

# protocolhelper 0.2.1

## New features

* protocol templates include CC-BY license

## Minor changes

* arguments determining paths to save results of convert_docx_to_rmd() are
  reconsidered (#52 and #53)
* change default of argument wrap in convert_docx_to_rmd() to NA
* .emf files will converted to .png in convert_docx_to_rmd()

# protocolhelper 0.2.0

## Breaking changes

* restructured yaml of skeleton.Rmd in templates: moved metadata under params
  section to toplevel of yaml
* new YAML syntax to add dependencies to the params section the YAML

## New features

* adds orcid information (#32)
* adds a link to dependencies table when params are missing (#46)
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
  be generated as well (and overwritten by the docx chapter if the filename
  is the same, i.e. confirms to current template standards)

# protocolhelper 0.1.2

* removed redundant pkgdown workflow

# protocolhelper 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Minor code improvements to meet quality criteria of checklist::check_package()

# protocolhelper 0.1.0

* Added new function `add_subprotocols()`
