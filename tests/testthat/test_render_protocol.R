test_that("render_protocol works as expected", {

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")

  # create a protocol to be used as subprotocol
  version_number <- "2021.01"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", orcids = "0000-0001-2345-6789",
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )

  render_protocol(protocol_code = "sfp-101-en")
  expect_true(file.exists("docs/sfp/1_water/sfp_101_en_water_1/index.html"))
  expect_true(
    file.exists("docs/sfp/1_water/sfp_101_en_water_1/sfp_101_en_water_1.pdf"))

  # Cleanup
  unlink(repo, recursive = TRUE)

})
