test_that("Get short title works", {
  language <- "en"
  setup_mock_contributors(language= language)

  old_wd <- getwd()
  withr::defer(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()
  url = "https://github.com/inbo/unittests"
  gert::git_remote_add(url = url, repo = ".")
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")

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
