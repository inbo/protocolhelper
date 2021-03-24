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
  insert_protocolsection(code_subprotocol ='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd',
                  fetch_remote = FALSE)

  # test addition of a chapter + demote_header
  insert_protocolsection(code_subprotocol ='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd',
                  demote_header = 1,
                  fetch_remote = FALSE)

  # test add a section from a chapter
  insert_protocolsection(code_subprotocol ='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd',
                  section = "## Uitvoering",
                  fetch_remote = FALSE)

  # test add a section from a chapter + demote_header by -1
  insert_protocolsection(code_subprotocol ='sfp-101-nl',
                  version_number='2020.01',
                  file_name='07_stappenplan.Rmd',
                  section = "## Uitvoering",
                  demote_header = -1,
                  fetch_remote = FALSE)

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

  # non-default params values need to be passed via render_...() functions
  # insert_protocolsection does not deal with it
  insert_protocolsection(code_subprotocol ='sfp-101-nl',
                  version_number='2020.02',
                  file_name='07_stappenplan.Rmd',
                  fetch_remote = FALSE)

  # test data and media
  # need a project protocol to see if data and media are copied
  # first add some data and media
  write.csv(x = cars, file = "src/thematic/1_water/sfp-101-nl_water-1/data/cars.csv")
  z <- tempfile()
  download.file("https://www.r-project.org/logo/Rlogo.png",
                z,
                mode="wb")
  pic <- png::readPNG(z)
  png::writePNG(pic,"src/thematic/1_water/sfp-101-nl_water-1/media/Rlogo.png")
  data_media_staged <- gert::git_add(files = ".")
  chunk1 <- "```{r}\nknitr::include_graphics(path = './media/Rlogo.png')\n```"
  chunk2 <- "```{r}\nread.csv('./data/cars.csv')\n```"
  write(
    x = chunk1,
    file = "src/thematic/1_water/sfp-101-nl_water-1/07_stappenplan.Rmd",
    append = TRUE)
  write(
    x = chunk2,
    file = "src/thematic/1_water/sfp-101-nl_water-1/07_stappenplan.Rmd",
    append = TRUE)
  gert::git_commit_all(message = "sfp-101-nl_water-1")
  version_number <- "2020.03"
  specific_tag <- paste("sfp-101-nl", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  # create a project protocol
  version_number <- "2020.04"
  create_spp(
    title = "project protocol", subtitle = "subtitle",
    short_title = "mne protocol",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, project_name = "mne", lang = "nl"
  )

  # add and commit it
  spp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "spp-001-nl_mne-protocol")

  # add a subprotocol to
  # src/project/mne/spp-001-nl_mne-protocol/08_subprotocols.Rmd
  # via a chunk
  chunk <- "```{r results='asis'}\nprotocolhelper::insert_protocolsection(code_subprotocol='sfp-101-nl', version_number='2020.03', file_name='07_stappenplan.Rmd', fetch_remote = FALSE)\n```"
  write(
    x = chunk,
    file = "src/project/mne/spp-001-nl_mne-protocol/08_subprotocols.Rmd",
    append = TRUE)
  gert::git_commit_all(message = "spp-001-nl_mne-protocol")

  # render spp-001-nl including the subprotocol
  render_protocol(protocol_code = "spp-001-nl",
                  params = list(protocol_code = 'paramvalue'))

  # Cleanup
  unlink(repo, recursive = TRUE)

})
