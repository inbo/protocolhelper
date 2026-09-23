# ==============================================================================
# Unit Tests: render_protocol()
#
# What is tested:
#   - Local rendering of a protocol into HTML (gitbook) and PDF formats via Bookdown.
#   - Output file generation in the standard 'docs/' folder hierarchy.
#
# How it is tested:
#   - Skips test on r-universe.dev.
#   - Sets up mock contributors and a temporary mock git repository.
#   - Scaffolds an SFP protocol (sfp-101-en) via create_sfp().
#   - Executes render_protocol("sfp-101-en") and asserts no errors occur.
#   - Verifies the physical existence of both generated output targets:
#     'docs/.../index.html' and 'docs/.../sfp_101_en_water_1.pdf'.
# ==============================================================================

test_that("render_protocol works as expected", {
  skip_if(Sys.getenv("MY_UNIVERSE") != "") # skip test on r-universe.dev
  language <- "en"
  setup_mock_contributors(language=language)

  setup_mock_repo(with_origin=FALSE)


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
