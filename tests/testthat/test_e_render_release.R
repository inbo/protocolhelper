# Tests for render_release(): builds release documentation for newly merged
# protocol versions.
# How it tests:
# - Sets up a mock repository with Zenodo sandbox files and scaffolds
#   sfp-101-en.
# - Updates index.Rmd reviewers, updates NEWS.md, and assigns a DOI via
#   update_doi().
# - Commits, tags, merges the protocol branch into main, and pushes to origin.
# - Executes render_release() on main and asserts error-free compilation of
#   release artifacts.

test_that("complete workflow works", {
  skip_if_offline()
  skip_if(Sys.getenv("MY_UNIVERSE") != "") # skip test on r-universe.dev
  skip_if_not_installed("zen4R")
  skip_if_not_installed("keyring")
  skip_if(
    assertthat::is.error(try(keyring::key_get("ZENODO_SANDBOX"), silent = TRUE))
  )
  language <- "en"
  setup_mock_contributors(language = language)

  mock_repo <- setup_mock_bare_origin_repo(include_zenodo_files = TRUE)
  origin_repo <- mock_repo$origin_repo
  main_branch <- mock_repo$main_branch
  repo <- mock_repo$repo

  # create a protocol
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )
  checklist::new_branch("sfp-101-en", repo = repo)

  path_to_protocol <- get_path_to_protocol("sfp-101-en")
  parent_rmd <- file.path(path_to_protocol, "index.Rmd")
  index_yml <- rmarkdown::yaml_front_matter(parent_rmd)
  second_reviewer <- list(
    name = list(given = "Pieter", family = "Verschelde"),
    email = "pieter.verschelde@inbo.be",
    orcid = "0000-0002-9199-421X",
    affiliation = "Research Institute for Nature and Forest (INBO)"
  )
  index_yml$reviewer <- append(list(second_reviewer), index_yml$reviewer)
  index_yml <- ymlthis::as_yml(index_yml)
  withr::with_options(list(usethis.overwrite = TRUE), {
    ymlthis::use_index_rmd(
      .yml = index_yml,
      path = path_to_protocol,
      template = parent_rmd,
      include_body = TRUE,
      include_yaml = FALSE,
      quiet = TRUE,
      open_doc = FALSE
    )
  })


  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number
  )

  protocolhelper:::update_news_release("sfp-101-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-101-en")

  # add, commit and tag it
  git_commit_and_tag_protocol(
    message = "sfp-101-en_water-1",
    protocol_code = "sfp-101-en",
    tag_message = "bla",
    version_number = version_number
  )
  git_push_current_branch()

  # merge into main and delete feature branch
  git_merge_to_main_and_delete(
    branch_name = "sfp-101-en",
    main_branch = main_branch,
    repo = repo,
    origin_repo = origin_repo
  )

  expect_no_error(protocolhelper:::render_release())
})
