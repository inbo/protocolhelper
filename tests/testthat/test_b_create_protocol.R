# Tests for create_sfp(): scaffolds a new Standard Field Protocol directory structure and template files.
# How it tests:
# - Sets up mock contributor metadata and an isolated bare-origin Git repository.
# - Determines the next version number using get_version_number().
# - Invokes create_sfp() with generic template and water theme in English.
# - Asserts that protocol scaffolding completes without error.

test_that("add author works", {
  language <- "en"
  setup_mock_contributors(language=language)

  setup_mock_bare_origin_repo()
  
  # create a protocol
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    template = "generic",
    version_number = version_number,
    theme = "water",
    language = language
  ) |> expect_no_error()
})
