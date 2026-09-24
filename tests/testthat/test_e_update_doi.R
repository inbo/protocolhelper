# ==============================================================================
# Unit Tests: update_doi() & DOI Lifecycle
#
# What is tested:
#   - DOI prefix format and synchronization between update_doi()'s result and the
#     protocol's index.Rmd YAML for an initial and an updated version.
#   - Retention of the DOI when update_doi() is called again for each version.
#   - The test does not assert that the DOI for the updated version differs from
#     the initial DOI.
#
# How it is tested:
#   - Skips when offline, MY_UNIVERSE is nonempty, required packages are missing,
#     or the ZENODO_SANDBOX credential is unavailable.
#   - Sets up a mock repository with origin and Zenodo metadata, scaffolds an SFP,
#     and checks the DOI and YAML after update_doi() and a repeated call.
#   - Merges the initial version, runs update_protocol(), then repeats the DOI/YAML
#     checks for the updated version.
#   - Calls render_release() after both releases; the final render check is skipped
#     on CI.
# ==============================================================================

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

  # merge into main
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_branch_checkout(main_branch)
  gert::git_merge(ref = refspec, repo = repo)
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )
  gert::git_branch_delete("sfp-101-en", repo = origin_repo)
  gert::git_branch_delete("sfp-101-en", repo = repo)

  expect_no_error(protocolhelper:::render_release())


  # prepare to start an update of the protocol (new version doi)
  update_protocol("sfp-101-en")
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_commit_all(message = "update version number sfp-101-en_water-1")
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )
  version_number <- get_version_number(path = repo)
  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number,
    n_bullets = 5
  )
  gert::git_commit_all(message = "update version number sfp-101-en_water-1")
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )

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

  # merge into main
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_branch_checkout(main_branch)
  gert::git_merge(ref = refspec, repo = repo)
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )
  gert::git_branch_delete("sfp-101-en", repo = origin_repo)
  gert::git_branch_delete("sfp-101-en", repo = repo)

  skip_on_ci()
  expect_no_error(protocolhelper:::render_release())
})
