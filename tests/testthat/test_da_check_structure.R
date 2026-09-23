# ==============================================================================
# Unit Tests: check_structure()
#
# What is tested:
#   - Validation of protocol file and folder structure against template standards.
#   - Detection of invalid/mismatched chapter titles in template Rmd files.
#   - Detection of missing mandatory files (references.yaml, template chapters).
#   - Detection of duplicate chapter numbering prefixes (e.g. multiple 01_*.Rmd).
#   - Support for generic template protocol structures.
#   - Differential behavior of fail parameter (error when TRUE, warning/message when FALSE).
#
# How it is tested:
#   - Sets up mock contributors and repository.
#   - Scaffolds a valid protocol and asserts clean check output ("No problems").
#   - Successively introduces intentional defects (corrupted chapter title, missing
#     references.yaml, missing chapter, duplicate file prefix) and asserts error
#     or expected console diagnostic output.
#   - Verifies structure check passes cleanly for generic template protocols.
# ==============================================================================

test_that("check structure works", {
  language <- "en"
  setup_mock_contributors(language=language)
  setup_mock_repo(with_origin = FALSE)

  # create a protocol
  version_number <- "2021.01"
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )

  expect_output(
    check_structure("sfp-101-en", fail = TRUE),
    "No problems"
  )

  # wrong title
  x <- readLines(file.path(
    get_path_to_protocol("sfp-101-en"),
    "01_dependencies.Rmd"
  ))
  x[[1]] <- "# afhankelijkheden"
  writeLines(x, con = file.path(
    get_path_to_protocol("sfp-101-en"),
    "01_dependencies.Rmd"
  ))

  expect_error(
    check_structure("sfp-101-en", fail = TRUE),
    "Some problems"
  )
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "Dependencies lack"
  )

  # fix title
  x <- readLines(file.path(
    get_path_to_protocol("sfp-101-en"),
    "01_dependencies.Rmd"
  ))
  x[[1]] <- "# Dependencies"
  writeLines(x, con = file.path(
    get_path_to_protocol("sfp-101-en"),
    "01_dependencies.Rmd"
  ))

  # reference file missing
  file.remove(file.path(
    get_path_to_protocol("sfp-101-en"),
    "references.yaml"
  ))
  expect_error(
    check_structure("sfp-101-en", fail = TRUE),
    "Some problems"
  )
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "references.yaml not found"
  )

  # add reference file back again and remove a template Rmd file
  file.create(file.path(
    get_path_to_protocol("sfp-101-en"),
    "references.yaml"
  ))
  x <- readLines(file.path(
    get_path_to_protocol("sfp-101-en"),
    "02_subject.Rmd"
  ))
  file.remove(file.path(
    get_path_to_protocol("sfp-101-en"),
    "02_subject.Rmd"
  ))
  expect_error(
    check_structure("sfp-101-en", fail = TRUE),
    "Some problems"
  )
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "02_subject.Rmd"
  )
  writeLines(x, file.path(
    get_path_to_protocol("sfp-101-en"),
    "02_subject.Rmd"
  ))

  # add Rmd file with duplicate chapter number
  file.create(file.path(
    get_path_to_protocol("sfp-101-en"),
    "01_afhankelijkheden.Rmd"
  ))
  expect_error(
    check_structure("sfp-101-en", fail = TRUE),
    "Some problems"
  )
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "01"
  )
  file.remove(file.path(
    get_path_to_protocol("sfp-101-en"),
    "01_afhankelijkheden.Rmd"
  ))


  # create a protocol with generic template
  version_number <- "2021.02"
  create_sfp(
    short_title = "water 2",
    theme = "water", language = "en", version_number = version_number,
    template = "generic"
  )

  expect_output(
    check_structure("sfp-102-en", fail = TRUE),
    "No problems"
  )
})
