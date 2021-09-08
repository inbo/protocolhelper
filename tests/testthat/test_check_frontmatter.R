test_that("Check frontmatter works", {
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
  protocolhelper::create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = c("Someone, Else", "Another, One"),
    orcids = c("0000-0001-2345-6789", "0000-0002-2345-6789"),
    reviewers = "me", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )
  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water-1")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  yamlcheck <- check_frontmatter(protocol_code = "sfp-101-en",
                                 yaml = FALSE)

  expect_equal(yamlcheck,
               message("Everything is OK"))
})
