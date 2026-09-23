# ==============================================================================
# Unit Tests: Protocol Creation & Scaffolding (create_sfp)
#
# What is tested:
#   - Initializing a new Standard Field Protocol (SFP) using create_sfp().
#   - Scaffolding with generic template, specified theme ('water'), and language ('en').
#   - Population of author, reviewer, and file manager metadata during creation.
#
# How it is tested:
#   - Configures mock contributor data frames via setup_mock_contributors().
#   - Initializes an isolated mock git repository with a bare origin remote.
#   - Retrieves the initial version number via get_version_number().
#   - Invokes create_sfp() with explicit parameters.
#   - Verifies that protocol creation completes without errors (expect_no_error()).
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
