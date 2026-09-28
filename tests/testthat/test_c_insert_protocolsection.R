# Tests for insert_protocolsection(): inserts chapters or sections from a tagged subprotocol.
# How it tests:
# - Scaffolds and Git-tags a subprotocol (sfp-101-nl) in an isolated mock repository.
# - Inserts an entire chapter and verifies text output with and without header demotion.
# - Extracts a specific subsection by header title and tests header level adjustments.
# - Updates the subprotocol with custom parameters and verifies insertion across protocol versions.

test_that("Test that insert_protocolsection works", {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("please install 'png' package for these tests to work")
  }
  language <- "nl"
  setup_mock_contributors(language = language)
  #setup_mock_local_repo()
  setup_mock_bare_origin_repo()
  # create a protocol to be used as subprotocol
  version_number <- "2020.01"
  create_sfp(
    short_title = "water 1",
    version_number = version_number,
    theme = "water",
    language = language
  )

  # add, commit and tag it

  git_commit_and_tag_protocol(
    message = "sfp-101-nl_water-1",
    protocol_code = "sfp-101-nl",
    version_number = version_number,
    tag_message = "bla"
  )
  git_push_current_branch()

  # test addition of a chapter
  expect_output(
    insert_protocolsection(
      code_subprotocol = "sfp-101-nl",
      version_number = "2020.01",
      file_name = "07_werkwijze.Rmd",
      fetch_remote = FALSE
    )
  )

  # test addition of a chapter + demote_header
  expect_output(
    insert_protocolsection(
      code_subprotocol = "sfp-101-nl",
      version_number = "2020.01",
      file_name = "07_werkwijze.Rmd",
      demote_header = 1,
      fetch_remote = FALSE
    )
  )

  # test add a section from a chapter
  expect_output(
    insert_protocolsection(
      code_subprotocol = "sfp-101-nl",
      version_number = "2020.01",
      file_name = "07_werkwijze.Rmd",
      section = "## Uitvoering",
      fetch_remote = FALSE
    )
  )

  # test add a section from a chapter + demote_header by -1
  expect_output(
    insert_protocolsection(
      code_subprotocol = "sfp-101-nl",
      version_number = "2020.01",
      file_name = "07_werkwijze.Rmd",
      section = "## Uitvoering",
      demote_header = -1,
      fetch_remote = FALSE
    )
  )

  # test add a chapter with non-default params
  test_params <- "\nCheck if the value changed: `r params$protocolspecific`"
  write(
    x = test_params,
    file = "source/sfp/1_water/sfp_101_nl_water_1/07_werkwijze.Rmd",
    append = TRUE
  )
  # add the protocolspecific parameter to index yaml
  index <- readLines(
    "source/sfp/1_water/sfp_101_nl_water_1/index.Rmd"
  )
  index <- c(
    index[1:14],
    "  protocolspecific: defaultvalue",
    index[15:length(index)]
  )
  writeLines(
    index,
    con = "source/sfp/1_water/sfp_101_nl_water_1/index.Rmd"
  )
  version_number <- "2020.02"

  git_commit_and_tag_protocol(
    message = "sfp-101-nl_water-1",
    protocol_code = "sfp-101-nl",
    version_number = version_number,
    tag_message = "bla"
  )


  # non-default params values need to be passed via render_...() functions
  # insert_protocolsection does not deal with it
  expect_output(
    insert_protocolsection(
      code_subprotocol = "sfp-101-nl",
      version_number = "2020.02",
      file_name = "07_werkwijze.Rmd",
      fetch_remote = FALSE
    )
  )
})
