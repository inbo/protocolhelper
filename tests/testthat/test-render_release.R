test_that("render protocols for release", {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  git2r::init()

  # create first protocol
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = "2020.01", theme = "water", lang = "nl"
  )
  render_release()

  # create second protocol
  create_sfp(
    title = "Air test 1", subtitle = "subtitle", short_title = "air 2",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = "2020.02", theme = "air", lang = "nl"
  )
  render_release()

  # create third protocol
  create_sfp(
    title = "water test 2", subtitle = "subtitle", short_title = "water 2",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = "2021.01", theme = "water", lang = "nl"
  )
  render_release()

  # upgrade first protocol
  index_file <- readLines(
    file.path("src", "thematic", "1_water", "sfp-101_water-1_nl", "index.Rmd")
  )
  index_file <- gsub(
    "version_number: \"2020.01\"",  "version_number: \"2021.02\"",
    index_file
  )
  writeLines(
    index_file,
    file.path("src", "thematic", "1_water", "sfp-101_water-1_nl", "index.Rmd")
  )
  render_release()

  # add translation of first protocol
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = "2021.03", theme = "water", lang = "en"
  )
  render_release()
})
