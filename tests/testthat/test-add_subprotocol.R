test_that("Test that add subprotocol works", {
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()

  # create a protocol to be used as subprotocol
  version_number <- "2020.01"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", reviewers = "someone else", file_manager = "who?",
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
  add_subprotocol(protocol_code='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd')

  # test addition of a chapter + demote_header
  add_subprotocol(protocol_code='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd',
                  demote_header = 1)

  # test add a section from a chapter
  add_subprotocol(protocol_code='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd',
                  section = "## Uitvoering")

  # test add a section from a chapter + demote_header by -1
  add_subprotocol(protocol_code='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd',
                  section = "## Uitvoering",
                  demote_header = -1)

  # test add a chapter with non-default params
  test_params <- "\nThe protocol-code is not `r params$protocol_code`"
  write(
   x = test_params,
   file = "src/thematic/1_water/sfp-101-nl_water-1/07_stappenplan.Rmd",
   append = TRUE)
  version_number <- "2020.02"
  gert::git_commit_all(message = "sfp-101-nl_water-1")
  specific_tag <- paste("sfp-101-nl", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  add_subprotocol(protocol_code='sfp-101-nl',
                  version_number='2020.02',
                  file_name='07_stappenplan.Rmd',
                  params = list(protocol_code = "paramvalue"))




  # # create a project protocol
  # version_number <- "2020.02"
  # create_spp(
  #   title = "project protocol", subtitle = "subtitle",
  #   short_title = "mne protocol",
  #   authors = "me", reviewers = "someone else", file_manager = "who?",
  #   version_number = version_number, project_name = "nme", lang = "nl"
  # )
  #
  # # add and commit it
  # spp_staged <- gert::git_add(files = ".")
  # gert::git_commit_all(message = "spp-001-nl_mne-protocol")
  #
  #
  # # add a subprotocol to
  # # src/project/nme/spp-001-nl_mne-protocol/08_appendices.Rmd
  # # via a chunk
  # chunk <- "```{r results='asis'}\nprotocolhelper::add_subprotocol(protocol_code='sfp-101-nl', version_number='2020.01', file_name='07_stappenplan.Rmd')\n```"
  # write(
  #   x = chunk,
  #   file = "src/project/nme/spp-001-nl_mne-protocol/08_appendices.Rmd",
  #   append = TRUE)
  #
  # # render spp-001-nl including the subprotocol
  # render_protocol(protocol_code = "spp-001-nl")






  # Cleanup
  unlink(repo, recursive = TRUE)

})
