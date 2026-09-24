# ==============================================================================
# Unit Tests: get_short_titles() & Title Uniqueness
#
# What is tested:
#   - Retrieval of existing protocol short titles filtered by protocol type ('sfp')
#     and language ('en').
#   - Enforcement of uniqueness: prevention of duplicate protocol creation with
#     an already used short title.
#
# How it is tested:
#   - Sets up a temporary git repository with mock origin remote and contributor metadata.
#   - Scaffolds an initial protocol with short title 'water 1'.
#   - Asserts get_short_titles("sfp", "en") returns "water_1".
#   - Attempts to scaffold a duplicate protocol with the same short title and asserts
#     that an informative error is thrown.
# ==============================================================================

test_that("Get short title works", {
  language <- "en"
  setup_mock_contributors(language= language)
  setup_mock_repo(with_origin=FALSE)
  
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
