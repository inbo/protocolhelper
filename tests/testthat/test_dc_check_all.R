# ==============================================================================
# Unit Tests: check_all()
#
# What is tested:
#   - Whether check_all() passes for a valid protocol, errors after a NEWS.md
#     version-header mismatch, and errors for a nonexistent protocol code.
#
# How it is tested:
#   - Sets up mock contributors and a mock git repository with an origin remote.
#   - Scaffolds, tags, and pushes a valid protocol and checks that check_all()
#     completes without error.
#   - Changes the NEWS.md version header, commits and pushes the change, and checks
#     that check_all(..., fail = TRUE) errors.
#   - Calls check_all() with an unknown protocol code and checks that it errors.
#   - Does not introduce front-matter or structure defects through check_all().
# ==============================================================================

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
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water-1")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )

  # no function fails
  expect_no_error(check_all("sfp-101-en", fail = TRUE))

  make_news_error(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number
  )

  gert::git_commit_all(message = "sfp-101-en_water-1")
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )


  # fails
  expect_error(check_all("sfp-101-en", fail = TRUE))

  # both functions fail
  expect_error(check_all("sfp-111-nl"))
})
