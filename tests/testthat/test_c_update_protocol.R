# ==============================================================================
# Unit Tests: update_protocol()
#
# What is tested:
#   - Protocol update workflow: initiating a new version cycle for an existing protocol.
#   - Creation and checkout of the protocol-specific git branch.
#   - Automatic incrementing of version number in index.Rmd YAML front matter
#     to the current calendar year format (YYYY.NN).
#
# How it is tested:
#   - Initializes a mock repository with bare origin remote and mock contributors.
#   - Scaffolds, commits, tags (2021.01), and pushes an initial protocol to origin.
#   - Calls update_protocol("sfp-101-en"), commits, and pushes changes.
#   - Asserts git branch is checked out to "sfp-101-en".
#   - Asserts the index.Rmd YAML front matter version matches the current year.
# ==============================================================================

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
