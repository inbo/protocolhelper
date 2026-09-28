# Tests for update_doi(): synchronizes Zenodo DOI identifiers into protocol frontmatter.
# How it tests:
# - Scaffolds an initial protocol version in a mock repository configured with Zenodo sandbox credentials.
# - Calls update_doi() to mint a concept/version DOI and asserts sandbox prefix (10.5072) and YAML update.
# - Confirms idempotency by verifying the DOI is retained unchanged on repeated update_doi() calls.
# - Updates to a second protocol version, verifying DOI versioning and persistence across release cycles.

test_that("update doi works", {
  skip_if_offline()
  skip_if(Sys.getenv("MY_UNIVERSE") != "") # skip test on r-universe.dev
  skip_if_not_installed("zen4R")
  skip_if_not_installed("keyring")
  skip_if(
    assertthat::is.error(try(keyring::key_get("ZENODO_SANDBOX"), silent = TRUE))
  )
  language <- "en"
  setup_mock_contributors(language=language)

  mock_repo <- setup_mock_bare_origin_repo(include_zenodo_files = TRUE)
  repo <- mock_repo$repo
  origin_repo <- mock_repo$origin_repo
  main_branch <- mock_repo$main_branch

  # create a protocol
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )
  checklist::new_branch("sfp-101-en", repo = repo)

  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number,
    n_bullets = 3
  )

  # the following is run in GHA when reviewer conditions are met
  protocolhelper:::update_news_release("sfp-101-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-101-en")
  expect_true(grepl("^10.5072", doi))
  check_doi <- rmarkdown::yaml_front_matter(
    file.path("source", "sfp", "1_water", "sfp_101_en_water_1", "index.Rmd")
  )$doi
  expect_equal(doi, check_doi)

  # extra test to check if doi is retained in cases where an extra approval is
  # needed - for instance, when a branch is out of date
  doi <- protocolhelper:::update_doi("sfp-101-en")
  expect_equal(doi, check_doi)

  # add, commit and tag it
  git_commit_and_tag_protocol(
    protocol_code = "sfp-101-en",
    message = "sfp-101-en_water-1",
    tag_message = "bla",
    version_number=version_number
  )
  git_push_current_branch()

  # merge into main
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_branch_checkout(main_branch)
  gert::git_merge(ref = refspec, repo = repo)

  git_push_current_branch()


  gert::git_branch_delete("sfp-101-en", repo = origin_repo)
  gert::git_branch_delete("sfp-101-en", repo = repo)

  expect_no_error(protocolhelper:::render_release())


  # prepare to start an update of the protocol (new version doi)
  update_protocol("sfp-101-en")

  #branch_info <- gert::git_branch_list(repo = repo)
  #refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_commit_all(message = "update version number sfp-101-en_water-1")
  git_push_current_branch()

  version_number <- get_version_number(path = repo)
  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number,
    n_bullets = 5
  )
  gert::git_commit_all(message = "update version number sfp-101-en_water-1")
  git_push_current_branch()

  protocolhelper:::update_news_release("sfp-101-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-101-en")
  expect_true(grepl("^10.5072", doi))
  check_doi <- rmarkdown::yaml_front_matter(
    file.path("source", "sfp", "1_water", "sfp_101_en_water_1", "index.Rmd")
  )$doi
  expect_equal(doi, check_doi)

  # extra test to check if doi is retained in cases where an extra approval is
  # needed - for instance, when a branch is out of date
  doi <- protocolhelper:::update_doi("sfp-101-en")
  expect_equal(doi, check_doi)

  # add, commit and tag it
  git_commit_and_tag_protocol(
    protocol_code = "sfp-101-en",
    message = "sfp-101-en_water-1",
    version_number=version_number,
    tag_message = "bla"
  )
  git_push_current_branch()

  # merge into main
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_branch_checkout(main_branch)
  gert::git_merge(ref = refspec, repo = repo)

  git_push_current_branch()
  
  gert::git_branch_delete("sfp-101-en", repo = origin_repo)
  gert::git_branch_delete("sfp-101-en", repo = repo)

  skip_on_ci()
  expect_no_error(protocolhelper:::render_release())
})
