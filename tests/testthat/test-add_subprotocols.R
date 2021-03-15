test_that("add dependencies of a project-specific protocol as appendix chapters works", {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("please install 'gert' package for these tests to work")
  }

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
    version_number = version_number, theme = "water", lang = "en"
  )

  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water-1")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  # create a protocol to be used as subprotocol
  version_number <- "2020.02"
  create_sfp(
    title = "subsubprotocoltest", subtitle = "subtitle",
    short_title = "vegetation 1",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "vegetation", lang = "en"
  )

  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-401-en_vegetation-1")
  specific_tag <- paste("sfp-401-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  # create a project protocol
  version_number <- "2020.03"
  create_spp(
    title = "project protocol", subtitle = "subtitle",
    short_title = "mne protocol",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, project_name = "mne", lang = "en"
  )

  # add and commit it
  spp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "spp-001-en_mne-protocol")


  # create a second protocol to be used as subprotocol
  version_number <- "2020.04"
  create_sfp(title = "Second subprotocol",
             subtitle = "",
             short_title = "second subprotocol",
             authors = "me",
             reviewers = "someone else",
             file_manager = "who?",
             version_number = version_number,
             theme = "water",
             lang = "en"
             )
  # test non-default params
  test_params <- "\nThe reviewers are `r params$reviewers`"
  write(
    x = test_params,
    file = "src/thematic/1_water/sfp-102-en_second-subprotocol/07_stappenplan.Rmd",
    append = TRUE)
  # test data and media
  write.csv(
    x = cars,
    file = "src/thematic/1_water/sfp-102-en_second-subprotocol/data/cars.csv")
  z <- tempfile()
  download.file("https://www.r-project.org/logo/Rlogo.png",
                z,
                mode="wb")
  pic <- png::readPNG(z)
  png::writePNG(pic,"src/thematic/1_water/sfp-102-en_second-subprotocol/media/Rlogo.png")
  data_media_staged <- gert::git_add(files = ".")
  chunk1 <- "```{r, out.width='25%'}\nknitr::include_graphics(path = './media/Rlogo.png')\n```"
  chunk2 <- "```{r}\nread.csv('./data/cars.csv')\n```"
  write(
    x = chunk1,
    file = "src/thematic/1_water/sfp-102-en_second-subprotocol/07_workflow.Rmd",
    append = TRUE)
  write(
    x = chunk2,
    file = "src/thematic/1_water/sfp-102-en_second-subprotocol/07_workflow.Rmd",
    append = TRUE)

  # add a sub-subprotocol to
  # src/thematic/1_water/sfp-102-en_second-subprotocol
  #debugonce(add_subprotocols)
  index <- readLines(
    con = "src/thematic/1_water/sfp-102-en_second-subprotocol/index.Rmd")
  index[grepl("  dependencies_protocolcode:", index)] <-
    "  dependencies_protocolcode: ['sfp-401-en']"
  index[grepl("  dependencies_versionnumber:", index)] <-
    "  dependencies_versionnumber: ['2020.02']"
  index[grepl("  dependencies_params:", index)] <-
    "  dependencies_params: ['list()']"
  index[grepl("  dependencies_appendix:", index)] <-
    "  dependencies_appendix: [TRUE]"
  writeLines(
    index,
    con = "src/thematic/1_water/sfp-102-en_second-subprotocol/index.Rmd")

  add_subprotocols(
    fetch_remote = FALSE,
    code_mainprotocol = 'sfp-102-en')


  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-102-en_second-subprotocol")
  specific_tag <- paste("sfp-102-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")


  # add subprotocols to
  # src/project/mne/spp-001-en_mne-protocol/
  #debugonce(add_subprotocols)
  index <- readLines(
    con = "src/project/mne/spp-001-en_mne-protocol/index.Rmd")
  index[grepl("  dependencies_protocolcode:", index)] <-
    "  dependencies_protocolcode: ['sfp-101-en', 'sfp-102-en']"
  index[grepl("  dependencies_versionnumber:", index)] <-
    "  dependencies_versionnumber: ['2020.01', '2020.04']"
  index[grepl("  dependencies_params:", index)] <-
    "  dependencies_params: ['list()', 'list(reviewers = c(\"reviewer1\", \"reviewer2\"))']"
  index[grepl("  dependencies_appendix:", index)] <-
    "  dependencies_appendix: [TRUE, TRUE]"
  writeLines(
    index,
    con = "src/project/mne/spp-001-en_mne-protocol/index.Rmd")
  add_subprotocols(
    fetch_remote = FALSE,
    code_mainprotocol = 'spp-001-en')

  # render spp-001-en including the subprotocol
  render_protocol(protocol_code = "spp-001-en")


  # Cleanup
  unlink(repo, recursive = TRUE)

})
