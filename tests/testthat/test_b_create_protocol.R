# ==============================================================================
# Unit Tests: Protocol Creation & Scaffolding (create_sfp)
#
# What is tested:
#   - Smoke test that create_sfp() scaffolds an English SFP using the generic
#     template and water theme.
#
# How it is tested:
#   - Mocks contributor prompts and creates an isolated git repository with a
#     bare origin remote.
#   - Gets a version number and calls create_sfp() with explicit parameters.
#   - Asserts only that the call completes without error; metadata values are
#     not separately checked.
# ==============================================================================

test_that("add author works", {
  language <- "en"
  setup_mock_contributors(language=language)

  setup_mock_repo(with_origin = TRUE)
  
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
