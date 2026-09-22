test_that("Update of a protocol works", {
  
  language <- "en"
  setup_mock_contributors(language=language)

  origin_repo <- gert::git_init(tempfile("protocol_origin"), bare = TRUE)
  url = "https://github.com/inbo/unittests"
  gert::git_remote_add(url = url, repo = origin_repo)
  withr::defer(unlink(origin_repo, recursive = TRUE))
  repo <- gert::git_clone(
    url = origin_repo,
    path = tempfile("protocol_local"), verbose = FALSE
  )
  withr::defer(unlink(repo, recursive = TRUE))

  gert::git_config_set(name = "user.name", value = "someone", repo = repo)
  gert::git_config_set(
    name = "user.email", value = "someone@example.org",
    repo = repo
  )
  # create a protocol
  old_wd <- setwd(repo)
  withr::defer(setwd(old_wd))
  version_number <- "2021.01"
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )

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

  # prepare to start an update of the protocol
  update_protocol("sfp-101-en")
  gert::git_commit_all(message = "update version number sfp-101-en_water-1")
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )


  expect_identical(
    gert::git_branch(repo = repo),
    "sfp-101-en"
  )
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
