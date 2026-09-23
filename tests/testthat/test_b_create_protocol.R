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
