# Tests for get_short_titles(): retrieves registered short titles and prevents duplicate naming.
# How it tests:
# - Sets up a mock repository and creates an initial protocol with short title "water 1".
# - Queries get_short_titles() for sfp protocols in English and asserts "water_1" is returned.
# - Attempts to create a second protocol with the same short title and asserts an informative error is raised.

test_that("Get short title works", {
  language <- "en"
  setup_mock_contributors(language= language)
  #setup_mock_local_repo()
  setup_mock_bare_origin_repo()
  # create a protocol
  version_number <- "2021.01"
  protocolhelper::create_protocol(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )

  expect_identical(
    get_short_titles("sfp", "en"),
    "water_1"
  )
  expect_error(
    protocolhelper::create_protocol(
      short_title = "water 1",
      version_number = version_number, theme = "water", language = language
    ),
    "The given short title already exists"
  )
})
