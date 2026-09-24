# ==============================================================================
# Unit Tests: get_short_titles() & Title Uniqueness
#
# What is tested:
#   - Whether get_short_titles("sfp", "en") returns the existing title "water_1".
#   - Whether creating another protocol with the same short title raises the
#     expected duplicate-title error.
#
# How it is tested:
#   - Sets up mock contributors and a local test git repository without a bare
#     origin clone.
#   - Scaffolds one protocol, checks the returned title, then attempts to create
#     a duplicate and checks the error message.
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
