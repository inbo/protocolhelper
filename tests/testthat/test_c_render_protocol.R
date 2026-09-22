test_that("render_protocol works as expected", {
  skip_if(Sys.getenv("MY_UNIVERSE") != "") # skip test on r-universe.dev
  language <- "en"
  setup_mock_contributors(language=language)

  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  old_wd <- setwd(test_repo)
  withr::defer(setwd(old_wd))
  repo <- gert::git_init()
  withr::defer(unlink(repo, recursive = TRUE))
  url = "https://github.com/inbo/unittests"
  gert::git_remote_add(url = url, repo = ".")
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")

  # create a protocol to be used as subprotocol
  version_number <- "2021.01"
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )

  expect_no_error(
    render_protocol(protocol_code = "sfp-101-en")
  )
  expect_true(
    file.exists("docs/sfp/1_water/sfp_101_en_water_1/index.html")
  )
  expect_true(
    file.exists("docs/sfp/1_water/sfp_101_en_water_1/sfp_101_en_water_1.pdf")
  )
})
