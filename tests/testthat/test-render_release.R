test_that("render protocols for release", {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  git2r::init()

  update_news <- function(path, version_number) {
    news <- readLines(file.path(path, "NEWS.Rmd"))
    writeLines(
      c(
        head(news, 2),
        sprintf("\n## [%1$s](../%1$s/index.html)\n", version_number),
        rep("- blabla blabla", 1 + rpois(1, lambda = 3)),
        tail(news, -2)
      ),
      file.path(path, "NEWS.Rmd")
    )
  }

  # create first protocol
  version_number <- "2020.01"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", lang = "nl"
  )
  update_news(
    path = file.path("src", "thematic", "1_water", "sfp-101-nl_water-1"),
    version_number = version_number
  )
  render_release()

  # create second protocol
  version_number <- "2020.02"
  create_sfp(
    title = "Air test 1", subtitle = "subtitle", short_title = "air 2",
    authors = c("first author", "second author"),
    reviewers = c("first reviewer", "second reviewer"), file_manager = "who?",
    version_number = version_number, theme = "air", lang = "nl"
  )
  update_news(
    path = file.path("src", "thematic", "2_air", "sfp-201-nl_air-2"),
    version_number = version_number
  )
  render_release()

  # create third protocol
  version_number <- "2021.01"
  create_sfp(
    title = "water test 2", subtitle = "subtitle", short_title = "water 2",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", lang = "nl"
  )
  update_news(
    path = file.path("src", "thematic", "1_water", "sfp-102-nl_water-2"),
    version_number = version_number
  )
  render_release()

  # upgrade first protocol
  version_number <- "2021.02"
  index_file <- readLines(
    file.path("src", "thematic", "1_water", "sfp-101-nl_water-1", "index.Rmd")
  )
  index_file <- gsub(
    "version_number: \"[0-9]{4}.[0-9]{2}\"",
    paste0("version_number: \"", version_number, "\""),
    index_file
  )
  writeLines(
    index_file,
    file.path("src", "thematic", "1_water", "sfp-101-nl_water-1", "index.Rmd")
  )
  update_news(
    path = file.path("src", "thematic", "1_water", "sfp-101-nl_water-1"),
    version_number = version_number
  )
  render_release()

  # add translation of first protocol
  version_number <- "2021.03"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", lang = "en"
  )
  update_news(
    path = file.path("src", "thematic", "1_water", "sfp-101-en_water-1"),
    version_number = version_number
  )
  render_release()
})
