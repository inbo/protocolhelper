# Tests for render_protocol(): compiles a protocol into HTML and PDF outputs.
# How it tests:
# - Sets up an isolated mock origin repository with contributor metadata.
# - Scaffolds a standard field protocol (sfp-101-en) via create_sfp().
# - Executes render_protocol() and verifies error-free completion.
# - Asserts that both build artifacts (index.html and .pdf) are generated in docs/.

test_that("render_protocol works as expected", {
  skip_if(Sys.getenv("MY_UNIVERSE") != "") # skip test on r-universe.dev
  language <- "en"
  setup_mock_contributors(language=language)

  #setup_mock_local_repo()
  setup_mock_bare_origin_repo()

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
