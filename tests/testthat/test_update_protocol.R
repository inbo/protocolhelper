test_that("Update of a protocol works", {
  origin_repo <- gert::git_init(tempfile("protocol_origin"), bare = TRUE)
  on.exit(unlink(origin_repo, recursive = TRUE), add = TRUE)
  repo <- gert::git_clone(url = origin_repo,
                          path = tempfile("protocol_local"), verbose = FALSE)
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)

  gert::git_config_set(name = "user.name", value = "someone", repo = repo)
  gert::git_config_set(name = "user.email", value = "someone@example.org",
                       repo = repo)
  # create a protocol
  old_wd <- setwd(repo)
  on.exit(setwd(old_wd), add = TRUE)
  version_number <- "2021.01"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "John, Doe", orcids = "0000-0001-2345-6789",
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )

  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water-1")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")
  branch_info <- git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == git_branch(repo = repo)]
  git_push(remote = "origin",
           refspec =  refspec,
           set_upstream = TRUE,
           repo = repo)

  # prepare to start an update of the protocol
  update_protocol("sfp-101-en")

  expect_identical(
    git_branch(repo = repo),
    "sfp-101-en"
  )
  expect_identical(
    yaml_front_matter(
      file.path(protocolhelper:::get_path_to_protocol("sfp-101-en"),
                "index.Rmd"))$version,
    paste0(format(Sys.Date(), "%Y"), ".00.dev")
  )

})
