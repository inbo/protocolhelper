# Tests for check_all(): runs composite validation across structure, frontmatter, and NEWS.
# How it tests:
# - Scaffolds, tags, and pushes a standard field protocol (sfp-101-en) in a mock repository.
# - Runs check_all() and asserts clean passage without errors.
# - Injects a version heading mismatch in NEWS.md using make_news_error() and verifies check_all() fails.
# - Tests an invalid protocol code ("sfp-111-nl") and verifies error handling for non-existent protocols.

test_that("Test if check all works", {
  language <- "en"
  setup_mock_contributors()
  mock_repo <- setup_mock_bare_origin_repo()
  repo <- mock_repo$repo
  # create a protocol
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number,
    theme = "water",
    language = language
  )

  # add, commit and tag it
  checklist::new_branch("sfp-101-en", repo = repo)

  git_commit_and_tag_protocol(
    protocol_code = "sfp-101-en",
    message = "sfp-101-en_water-1",
    version_number=version_number,
    tag_message = "bla"
  )
  git_push_current_branch()

  # no function fails
  expect_no_error(check_all("sfp-101-en", fail = TRUE))

  make_news_error(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number
  )

  gert::git_commit_all(message = "sfp-101-en_water-1")
  git_push_current_branch()


  # fails
  expect_error(check_all("sfp-101-en", fail = TRUE))

  # both functions fail
  expect_error(check_all("sfp-111-nl"))
})
