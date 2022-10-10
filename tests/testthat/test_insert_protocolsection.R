test_that("Test that insert_protocolsection works", {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("please install 'png' package for these tests to work")
  }
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("please install 'gert' package for these tests to work")
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")

  # create a protocol to be used as subprotocol
  version_number <- "2020.01"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me",
    orcids = "0000-0001-2345-6789",
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", lang = "nl"
  )

  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-nl_water-1")
  specific_tag <- paste("sfp-101-nl", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  # test addition of a chapter
  insert_protocolsection(code_subprotocol = "sfp-101-nl",
                  version_number = "2020.01",
                  file_name = "07_werkwijze.Rmd",
                  fetch_remote = FALSE)

  # test addition of a chapter + demote_header
  insert_protocolsection(code_subprotocol = "sfp-101-nl",
                  version_number = "2020.01",
                  file_name = "07_werkwijze.Rmd",
                  demote_header = 1,
                  fetch_remote = FALSE)

  # test add a section from a chapter
  insert_protocolsection(code_subprotocol = "sfp-101-nl",
                  version_number = "2020.01",
                  file_name = "07_werkwijze.Rmd",
                  section = "## Uitvoering",
                  fetch_remote = FALSE)

  # test add a section from a chapter + demote_header by -1
  insert_protocolsection(code_subprotocol = "sfp-101-nl",
                  version_number = "2020.01",
                  file_name = "07_werkwijze.Rmd",
                  section = "## Uitvoering",
                  demote_header = -1,
                  fetch_remote = FALSE)

  # test add a chapter with non-default params
  test_params <- "\nCheck if the value changed: `r params$protocolspecific`"
  write(
    x = test_params,
    file = "source/sfp/1_water/sfp_101_nl_water_1/07_werkwijze.Rmd",
    append = TRUE)
  # add the protocolspecific parameter to index yaml
  index <- readLines(
    "source/sfp/1_water/sfp_101_nl_water_1/index.Rmd")
  index <- c(index[1:14],
             "  protocolspecific: defaultvalue",
             index[15:length(index)])
  writeLines(index,
             con = "source/sfp/1_water/sfp_101_nl_water_1/index.Rmd")
  version_number <- "2020.02"
  gert::git_commit_all(message = "sfp-101-nl_water-1")
  specific_tag <- paste("sfp-101-nl", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  # non-default params values need to be passed via render_...() functions
  # insert_protocolsection does not deal with it
  insert_protocolsection(code_subprotocol = "sfp-101-nl",
                  version_number = "2020.02",
                  file_name = "07_werkwijze.Rmd",
                  fetch_remote = FALSE)

  # Cleanup
  unlink(repo, recursive = TRUE)

})
