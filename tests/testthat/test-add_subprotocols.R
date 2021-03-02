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

  # create a project protocol
  version_number <- "2020.02"
  create_spp(
    title = "project protocol", subtitle = "subtitle",
    short_title = "mne protocol",
    authors = "me", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, project_name = "mne", lang = "en"
  )

  # add and commit it
  spp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "spp-001-en_mne-protocol")

  # debugonce(add_one_subprotocol)
  # add_one_subprotocol(code_subprotocol = "sfp-101-en",
  #                     version_number = "2020.01",
  #                     fetch_remote = FALSE)
  # add a subprotocol to
  # src/project/mne/spp-001-nl_mne-protocol/08_appendices.Rmd
  # via a chunk
  chunk <- "```{r}\nprotocolhelper::add_subprotocols(.dependencies = dependencies,fetch_remote=FALSE)\n```"
  write(
    x = chunk,
    file = "src/project/mne/spp-001-en_mne-protocol/08_appendices.Rmd",
    append = TRUE)
  gert::git_commit_all(message = "spp-001-en_mne-protocol")

  # render spp-001-en including the subprotocol

  render_protocol(protocol_code = "spp-001-en",
                  params = list(
                    dependencies_protocolcode = c("sfp-101-en"),
                    dependencies_versionnumber = c("2020.01"),
                    dependencies_params = c("list()"),
                    dependencies_appendix = c("TRUE")
                  ))


  # Cleanup
  unlink(repo, recursive = TRUE)



})
