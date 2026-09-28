# Tests for check_structure(): validates protocol file structure, chapter titles, and numbering.
# How it tests:
# - Scaffolds a protocol and confirms clean validation output ("No problems").
# - Simulates errors: alters chapter titles, removes references.yaml, and deletes template chapters.
# - Introduces duplicate chapter numbers (01_) and asserts validation errors and diagnostic messages.
# - Tests a generic template protocol to ensure structural rules apply across template variants.

test_that("check structure works", {
  language <- "en"
  setup_mock_contributors(language=language)
  #setup_mock_local_repo()
  setup_mock_bare_origin_repo()
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
