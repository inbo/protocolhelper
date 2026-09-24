# ==============================================================================
# Unit Tests: Multi-Protocol End-to-End Workflow
#
# What is tested:
#   - A multi-protocol workflow that scaffolds and updates SFPs and an SPP,
#     connects protocols with dependencies, and runs release-render steps.
#   - This is an integration smoke test: it checks release renders for errors but
#     does not directly assert the resulting DOI, metadata, or dependency content.
#
# How it is tested:
#   - Skips when offline, MY_UNIVERSE is nonempty, required packages are missing,
#     or the ZENODO_SANDBOX credential is unavailable.
#   - Creates a mock repository with origin and Zenodo metadata.
#   - Releases a water SFP, a vegetation SFP, a second water SFP containing data
#     and media and depending on the vegetation SFP, and a composite SPP depending
#     on the two water SFPs; then updates the first water SFP.
#   - Runs render_release() after each release workflow and checks for no error.
#     The final render check is skipped on CI.
# ==============================================================================

test_that("complete workflow works", {
  skip_if_offline()
  skip_if(Sys.getenv("MY_UNIVERSE") != "") # skip test on r-universe.dev
  skip_if_not_installed("zen4R")
  skip_if_not_installed("keyring")
  skip_if(
    assertthat::is.error(try(keyring::key_get("ZENODO_SANDBOX"), silent = TRUE))
  )
  language <- "en"
  setup_mock_contributors(
    language=language, 
    readline_value = function(...) paste("Tekst", Sys.time()))

  mock_repo <- setup_mock_bare_origin_repo(include_zenodo_files = TRUE)
  repo <- mock_repo$repo
  origin_repo <- mock_repo$origin_repo
  main_branch <- mock_repo$main_branch
  
  # create a protocol to be used as subprotocol
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )
  checklist::new_branch("sfp-101-en", repo = repo)

  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number
  )

  protocolhelper:::update_news_release("sfp-101-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-101-en")

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


  # create a protocol which will also be used as subprotocol
  version_number_2 <- get_version_number(path = repo)
  create_sfp(
    short_title = "vegetation 1",
    version_number = version_number_2, theme = "vegetation", language = "en"
  )
  checklist::new_branch("sfp-407-en", repo = repo)

  update_news(
    path = file.path(
      "source", "sfp", "4_vegetation",
      "sfp_407_en_vegetation_1"
    ),
    version_number = version_number_2,
    n_bullets = 4
  )

  protocolhelper:::update_news_release("sfp-407-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-407-en")

  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-407-en_vegetation-1")
  specific_tag <- paste("sfp-407-en", version_number_2, sep = "-")
  generic_tag <- paste("protocols", version_number_2, sep = "-")
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
  gert::git_branch_delete("sfp-407-en", repo = origin_repo)
  gert::git_branch_delete("sfp-407-en", repo = repo)

  expect_no_error(protocolhelper:::render_release())

  # create a second protocol to be used as subprotocol
  version_number_3 <- get_version_number(path = repo)
  create_sfp(
    short_title = "second subprotocol",
    version_number = version_number_3,
    theme = "water",
    language = "en"
  )
  checklist::new_branch("sfp-102-en", repo = repo)
  # test non-default params
  test_params <- "\nCheck if the value changed: `r params$protocolspecific`"
  write(
    x = test_params,
    file = file.path(
      "source/sfp/1_water/sfp_102_en_second_subprotocol",
      "07_stappenplan.Rmd"
    ),
    append = TRUE
  )
  # add the projectspecific parameter to index yaml
  index_yml <- rmarkdown::yaml_front_matter(
    "source/sfp/1_water/sfp_102_en_second_subprotocol/index.Rmd"
  )
  unlink("css", recursive = TRUE)
  index_yml <- ymlthis::as_yml(index_yml)
  index_yml <- ymlthis::yml_params(index_yml, protocolspecific = "defaultvalue")
  template_rmd <-
    "source/sfp/1_water/sfp_102_en_second_subprotocol/template.Rmd"
  file.copy(
    from = "source/sfp/1_water/sfp_102_en_second_subprotocol/index.Rmd",
    to = template_rmd
  )
  unlink("source/sfp/1_water/sfp_102_en_second_subprotocol/index.Rmd")
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = "source/sfp/1_water/sfp_102_en_second_subprotocol/",
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE
  )
  unlink(template_rmd)


  # test data and media
  write.csv(
    x = cars,
    file = "source/sfp/1_water/sfp_102_en_second_subprotocol/data/cars.csv"
  )
  z <- tempfile()
  download.file(
    "https://www.r-project.org/logo/Rlogo.png",
    z,
    mode = "wb"
  )
  pic <- png::readPNG(z)
  png::writePNG(
    pic,
    "source/sfp/1_water/sfp_102_en_second_subprotocol/media/Rlogo.png"
  )
  data_media_staged <- gert::git_add(files = ".")
  chunk1 <- paste0(
    "```{r, out.width='25%'}\nknitr::include_graphics(path",
    " = './media/Rlogo.png')\n```"
  )
  chunk2 <- "```{r}\nread.csv('./data/cars.csv')\n```"
  write(
    x = chunk1,
    file = "source/sfp/1_water/sfp_102_en_second_subprotocol/07_workflow.Rmd",
    append = TRUE
  )
  write(
    x = chunk2,
    file = "source/sfp/1_water/sfp_102_en_second_subprotocol/07_workflow.Rmd",
    append = TRUE
  )

  # add a sub-subprotocol to
  # source/sfp/1_water/sfp_102_en_second_subprotocol
  add_dependencies(
    code_mainprotocol = "sfp-102-en",
    protocol_code = "sfp-407-en",
    version_number = version_number_2,
    params = NA,
    appendix = TRUE
  )

  add_subprotocols(
    fetch_remote = TRUE,
    code_mainprotocol = "sfp-102-en"
  )

  update_news(
    path = file.path(
      "source", "sfp", "1_water",
      "sfp_102_en_second_subprotocol"
    ),
    version_number = version_number_3,
    n_bullets= 6
  )

  protocolhelper:::update_news_release("sfp-102-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-102-en")

  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-102-en_second_subprotocol")
  specific_tag <- paste("sfp-102-en", version_number_3, sep = "-")
  generic_tag <- paste("protocols", version_number_3, sep = "-")
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
  gert::git_branch_delete("sfp-102-en", repo = origin_repo)
  gert::git_branch_delete("sfp-102-en", repo = repo)

  expect_no_error(protocolhelper:::render_release())


  # create a project protocol
  version_number_4 <- get_version_number(path = repo)
  create_spp(
    short_title = "mne protocol",
    version_number = version_number_4, project_name = "mne", language = "en"
  )
  checklist::new_branch("spp-001-en", repo = repo)

  # add subprotocols to
  # source/spp/mne/spp_001_en_mne_protocol/
  add_dependencies(
    code_mainprotocol = "spp-001-en",
    protocol_code = c("sfp-101-en", "sfp-102-en"),
    version_number = c(version_number, version_number_3),
    params = list(NA, list(protocolspecific = "newvalue")),
    appendix = c(TRUE, TRUE)
  )

  add_subprotocols(
    fetch_remote = TRUE,
    code_mainprotocol = "spp-001-en"
  )

  update_news(
    path = file.path(
      "source", "spp", "mne",
      "spp_001_en_mne_protocol"
    ),
    version_number = version_number_4,
    n_bullets=1
  )

  protocolhelper:::update_news_release("spp-001-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("spp-001-en")

  # add, commit and tag it
  spp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "spp-001-en_mne-protocol")
  specific_tag <- paste("spp-001-en", version_number_4, sep = "-")
  generic_tag <- paste("protocols", version_number_4, sep = "-")
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
  gert::git_branch_delete("spp-001-en", repo = origin_repo)
  gert::git_branch_delete("spp-001-en", repo = repo)

  expect_no_error(protocolhelper:::render_release())

  # update first protocol
  version_number_5 <- get_version_number()
  protocolhelper::update_protocol("sfp-101-en")
  protocolhelper:::update_news_release("sfp-101-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-101-en")

  # add, commit and tag it
  spp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water")
  specific_tag <- paste("sfp-101-en", version_number_5, sep = "-")
  generic_tag <- paste("protocols", version_number_5, sep = "-")
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
