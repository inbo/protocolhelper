test_that("Get tags works", {

  versions1 <- c("2021.01", "2021.02")
  versions2 <- character(0)
  currentyear <- format(Sys.Date(), "%Y")
  expect_equal(
    protocolhelper:::increment_version_number(versions1),
    ifelse(currentyear == "2021",
           "2021.03",
           paste0(currentyear, ".01"))
    )
  expect_equal(
    protocolhelper:::increment_version_number(versions2),
    paste0(currentyear, ".01")
  )


  old_wd <- getwd()
  on.exit(setwd(old_wd))
  test_repo <- tempfile("test_protocol")
  dir.create(test_repo)
  setwd(test_repo)
  repo <- gert::git_init()
  gert::git_config_set(name = "user.name", value = "someone")
  gert::git_config_set(name = "user.email", value = "someone@example.org")


  # create a first protocol
  version_number <- "2021.01"
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", orcids = "0000-0001-2345-6789",
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


  # create a second protocol
  version_number <- "2021.02"
  create_sfp(
    title = "protocoltest", subtitle = "subtitle",
    short_title = "vegetation 1",
    authors = c("me", "you"),
    orcids = c("0000-0001-2345-6789", "0000-0001-2345-6789"),
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "vegetation", language = "en"
  )
  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-401-en_vegetation-1")
  specific_tag <- paste("sfp-401-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")


  #and a third development protocol
  version_number <- "2021.00.dev"
  create_sfp(
    title = "Test 3", subtitle = "subtitle", short_title = "water 2",
    authors = "me", orcids = "0000-0001-2345-6789",
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )

  # test get_tags

  test1 <- protocolhelper:::get_tags(protocol_code = "sfp-102-en",
                                     bump_version = FALSE)
  test2 <- protocolhelper:::get_tags(protocol_code = "sfp-102-en",
                                     bump_version = TRUE)


  expect_equal(test1,
               message("The general tag is: ",
                       ifelse(currentyear == "2021",
                              "protocols-2021.03",
                              paste0("protocols-", currentyear, ".01")
                              ),
                       "\nThe specific tag is: ",
                       ifelse(currentyear == "2021",
                              "sfp-102-en-2021.03",
                              paste0("sfp-102-en-", currentyear, ".01")
                              )
                       )
  )

  yml <- rmarkdown::yaml_front_matter(
    file.path(protocolhelper:::get_path_to_protocol("sfp-102-en"),
              "index.Rmd")
  )

  expect_equal(
    yml$version_number,
    ifelse(currentyear == "2021",
           "2021.03",
           paste0(currentyear, ".01")
    )
  )









})
