test_that("complete workflow works", {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("please install 'gert' package for these tests to work")
  }

  update_news <- function(path, version_number) {
    news <- readLines(file.path(path, "NEWS.md"))
    writeLines(
      c(
        head(news, 2),
        sprintf("\n## [%1$s](../%1$s/index.html)\n", version_number),
        rep("- blabla blabla", 1 + rpois(1, lambda = 3)),
        tail(news, -2)
      ),
      file.path(path, "NEWS.md")
    )
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")

  # create empty NEWS.md repo file
  file.create("NEWS.md")

  # create a protocol to be used as subprotocol
  version_number <- "2021.01"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "lastname, firstname", orcids = "0000-0001-2345-6789",
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )

  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number
  )



  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water-1")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  protocolhelper:::render_release()

  # create a protocol which will also be used as subprotocol
  version_number <- "2021.02"
  create_sfp(
    title = "subsubprotocoltest", subtitle = "subtitle",
    short_title = "vegetation 1",
    authors = c("Someone, Else", "Another, One"),
    orcids = c("0000-0001-2345-6789", "0000-0001-2345-6789"),
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "vegetation", language = "en"
  )

  update_news(
    path = file.path("source", "sfp", "4_vegetation",
                     "sfp_401_en_vegetation_1"),
    version_number = version_number
  )


  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-401-en_vegetation-1")
  specific_tag <- paste("sfp-401-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")


  protocolhelper:::render_release()

  # create a second protocol to be used as subprotocol
  version_number <- "2021.03"
  create_sfp(title = "Second subprotocol",
             subtitle = "subtitle",
             short_title = "second subprotocol",
             authors = "me, again",
             orcids = "0000-0001-2345-6789",
             reviewers = "someone else",
             file_manager = "who?",
             version_number = version_number,
             theme = "water",
             language = "en"
             )
  # test non-default params
  test_params <- "\nCheck if the value changed: `r params$protocolspecific`"
  write(
    x = test_params,
    file = file.path("source/sfp/1_water/sfp_102_en_second_subprotocol",
                     "07_stappenplan.Rmd"),
    append = TRUE)
  # add the projectspecific parameter to index yaml
  index_yml <- rmarkdown::yaml_front_matter(
    "source/sfp/1_water/sfp_102_en_second_subprotocol/index.Rmd")
  unlink("css", recursive = TRUE)
  index_yml <- ymlthis::as_yml(index_yml)
  index_yml <- ymlthis::yml_params(index_yml, protocolspecific = "defaultvalue")
  template_rmd <-
    "source/sfp/1_water/sfp_102_en_second_subprotocol/template.Rmd"
  file.copy(
    from = "source/sfp/1_water/sfp_102_en_second_subprotocol/index.Rmd",
    to = template_rmd)
  unlink("source/sfp/1_water/sfp_102_en_second_subprotocol/index.Rmd")
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = "source/sfp/1_water/sfp_102_en_second_subprotocol/",
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE)
  unlink(template_rmd)


  # test data and media
  write.csv(
    x = cars,
    file = "source/sfp/1_water/sfp_102_en_second_subprotocol/data/cars.csv")
  z <- tempfile()
  download.file("https://www.r-project.org/logo/Rlogo.png",
                z,
                mode = "wb")
  pic <- png::readPNG(z)
  png::writePNG(
    pic,
    "source/sfp/1_water/sfp_102_en_second_subprotocol/media/Rlogo.png")
  data_media_staged <- gert::git_add(files = ".")
  chunk1 <- paste0("```{r, out.width='25%'}\nknitr::include_graphics(path",
                   " = './media/Rlogo.png')\n```")
  chunk2 <- "```{r}\nread.csv('./data/cars.csv')\n```"
  write(
    x = chunk1,
    file = "source/sfp/1_water/sfp_102_en_second_subprotocol/07_workflow.Rmd",
    append = TRUE)
  write(
    x = chunk2,
    file = "source/sfp/1_water/sfp_102_en_second_subprotocol/07_workflow.Rmd",
    append = TRUE)

  # add a sub-subprotocol to
  # source/sfp/1_water/sfp_102_en_second_subprotocol
  add_dependencies(
    code_mainprotocol = "sfp-102-en",
    protocol_code = "sfp-401-en",
    version_number = "2021.02",
    params = NA,
    appendix = TRUE
  )

  add_subprotocols(
    fetch_remote = FALSE,
    code_mainprotocol = "sfp-102-en")

  update_news(
    path = file.path("source", "sfp", "1_water",
                     "sfp_102_en_second_subprotocol"),
    version_number = version_number
  )


  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-102-en_second-subprotocol")
  specific_tag <- paste("sfp-102-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  protocolhelper:::render_release()


  # create a project protocol
  version_number <- "2021.04"
  create_spp(
    title = "project protocol", subtitle = "subtitle",
    orcids = "0000-0001-2345-6789",
    short_title = "mne protocol",
    authors = "John, Doe", reviewers = "someone else", file_manager = "who?",
    version_number = version_number, project_name = "mne", language = "en"
  )

  # add subprotocols to
  # source/spp/mne/spp_001_en_mne_protocol/
  add_dependencies(
    code_mainprotocol = "spp-001-en",
    protocol_code = c("sfp-101-en", "sfp-102-en"),
    version_number = c("2021.01", "2021.03"),
    params = list(NA, list(protocolspecific = "newvalue")),
    appendix = c(TRUE, TRUE)
  )

  add_subprotocols(
    fetch_remote = FALSE,
    code_mainprotocol = "spp-001-en")

  update_news(
    path = file.path("source", "spp", "mne",
                     "spp_001_en_mne_protocol"),
    version_number = version_number
  )


  # add, commit and tag it
  spp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "spp-001-en_mne-protocol")
  specific_tag <- paste("spp-001-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")


  # render release which will output to publish folder
  protocolhelper:::render_release()

  # update first protocol
  version_number <- "2021.05"
  index_file <- readLines(
    file.path("source", "sfp", "1_water", "sfp_101_en_water_1", "index.Rmd")
  )
  index_file <- gsub(
    "version_number: '[0-9]{4}.[0-9]{2}'",
    paste0("version_number: '", version_number, "'"),
    index_file
  )
  writeLines(
    index_file,
    file.path("source", "sfp", "1_water", "sfp_101_en_water_1", "index.Rmd")
  )
  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number
  )


  # add, commit and tag it
  spp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")

  protocolhelper:::render_release()



  # Cleanup
  unlink(repo, recursive = TRUE)

})
