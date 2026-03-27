# The `protocolcheck` R6 class

A class that collects and shows all check results.

## See also

Other check:
[`check_all()`](https://inbo.github.io/protocolhelper/reference/check_all.md),
[`check_all_person_info()`](https://inbo.github.io/protocolhelper/reference/check_all_person_info.md),
[`check_frontmatter()`](https://inbo.github.io/protocolhelper/reference/check_frontmatter.md),
[`check_structure()`](https://inbo.github.io/protocolhelper/reference/check_structure.md),
[`validate_orcid()`](https://inbo.github.io/protocolhelper/reference/validate_orcid.md)

## Public fields

- `protocol_code`:

  Character string giving the protocol code.

- `path`:

  Character string giving the relative path to the protocol.

- `error`:

  Character vector containing all errors found in the protocol

## Methods

### Public methods

- [`protocolcheck$new()`](#method-Protocolcheck-new)

- [`protocolcheck$add_error()`](#method-Protocolcheck-add_error)

- [`protocolcheck$check()`](#method-Protocolcheck-check)

- [`protocolcheck$clone()`](#method-Protocolcheck-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new `Protocolcheck` object.

#### Usage

    protocolcheck$new(protocol_code)

#### Arguments

- `protocol_code`:

  Character string giving the protocol code.

#### Returns

A new `Protocolcheck` object

------------------------------------------------------------------------

### Method `add_error()`

Add a new error to the `Protocolcheck` object.

#### Usage

    protocolcheck$add_error(msg)

#### Arguments

- `msg`:

  Error message to be added.

------------------------------------------------------------------------

### Method `check()`

Give error report from `Protocolcheck` object.

#### Usage

    protocolcheck$check(fail)

#### Arguments

- `fail`:

  Should an error be dropped if the report contains errors?

#### Returns

An error report (and if desired an error is dropped).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    protocolcheck$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
