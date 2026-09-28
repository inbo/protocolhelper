# Tests for update_protocol(): prepares an existing protocol for a new version release.
# How it tests:
# - Scaffolds, commits, tags, and pushes an initial protocol (sfp-101-en, version 2021.01).
# - Calls update_protocol("sfp-101-en") to check out the protocol branch and bump the version.
# - Asserts Git switches to the protocol branch ("sfp-101-en").
# - Verifies index.Rmd YAML frontmatter is updated with the current year's version number.

test_that("Update of a protocol works", {
  
  language <- "en"
  setup_mock_contributors(language = language)

  mock_repo <- setup_mock_bare_origin_repo()

  # create a protocol
  version_number <- "2021.01"
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )

  # Push the initial protocol commit on the base branch. update_protocol() then
  # creates the protocol branch from this starting point.
  git_commit_and_tag_protocol(
    message = "sfp-101-en_water-1",
    protocol_code = "sfp-101-en",
    version_number = version_number,
    tag_message = "bla",
    repo = mock_repo$repo
  )
  git_push_current_branch(repo = mock_repo$repo)
  
  # prepare to start an update of the protocol
  update_protocol("sfp-101-en")
  gert::git_commit_all(message = "update version number sfp-101-en_water-1")
  git_push_current_branch(repo = mock_repo$repo)


  expect_identical(
    gert::git_branch(repo = mock_repo$repo),
    "sfp-101-en"
  )

  # check if the year on the updated protocol is the same as the year at time of testing
  expect_identical(
    yaml_front_matter(
      file.path(
        get_path_to_protocol("sfp-101-en"),
        "index.Rmd"
      )
    )$version,
    paste0(format(Sys.Date(), "%Y"), ".01")
  )
})
