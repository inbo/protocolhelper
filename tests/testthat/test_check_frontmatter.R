test_that("Check frontmatter works", {
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

  # create a protocol
  version_number <- "2021.01"
  protocolhelper::create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = c("Someone, Else", "Another, One"),
    orcids = c("0000-0001-2345-6789", "0000-0002-2345-6789"),
    reviewers = "me", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )

  expect_output(check_frontmatter(protocol_code = "sfp-101-en",
                                  fail = FALSE),
               "Well done! No problems found")

  # create some problems
  path_to_protocol <- get_path_to_protocol("sfp-101-en")
  index_yml <- rmarkdown::yaml_front_matter(
    file.path(path_to_protocol, "index.Rmd"))
  index_yml <- ymlthis::as_yml(index_yml)
  index_yml <- ymlthis::yml_replace(
    index_yml,
    title = c("bla", "bla"),
    version_number = "2020.01.dev",
    language = "espagnol"
  )
  template_rmd <- file.path(path_to_protocol, "template.rmd")
  parent_rmd <- file.path(path_to_protocol, "index.Rmd")
  file.copy(from = parent_rmd, to = template_rmd)
  unlink(parent_rmd)
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = path_to_protocol,
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE)
  unlink(template_rmd)

  expect_error(check_frontmatter(protocol_code = "sfp-101-en",
                                 fail = TRUE),
               "Some problems occur")

  expect_output(check_frontmatter(protocol_code = "sfp-101-en",
                    fail = FALSE),
                "Errors in protocol sfp-101-en:")

})
