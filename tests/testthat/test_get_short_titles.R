test_that("Get short title works", {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("please install 'gert' package for these tests to work")
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")

  # create a protocol
  version_number <- "2021.01"
  protocolhelper::create_protocol(
    title = "Test 1", short_title = "water 1",
    authors = c("Someone, Else", "Another, One"),
    orcids = c("0000-0001-2345-6789", "0000-0002-2345-6789"),
    reviewers = "me", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )

  expect_identical(get_short_titles("sfp", "en"),
                   "water-1")
  expect_error(
    protocolhelper::create_protocol(
      title = "Test 1", short_title = "water 1",
      authors = c("Someone, Else", "Another, One"),
      orcids = c("0000-0001-2345-6789", "0000-0002-2345-6789"),
      reviewers = "me", file_manager = "who?",
      version_number = version_number, theme = "water", language = "en"),
    "The given short title already exists"
    )
})
