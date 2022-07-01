test_that("check structure works", {
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
  create_sfp(
    title = "Test 1", subtitle = "subtitle", short_title = "water 1",
    authors = "me", orcids = "0000-0001-2345-6789",
    reviewers = "someone else", file_manager = "who?",
    version_number = version_number, theme = "water", language = "en"
  )

  expect_output(
    check_structure("sfp-101-en", fail = TRUE),
    "No problems")

  # wrong title
  x <- readLines(file.path(get_path_to_protocol("sfp-101-en"),
                      "01_dependencies.Rmd"))
  x[[1]] <- "# afhankelijkheden"
  writeLines(x, con = file.path(get_path_to_protocol("sfp-101-en"),
                                "01_dependencies.Rmd"))

  expect_error(check_structure("sfp-101-en", fail = TRUE),
               "Some problems")
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "Dependencies lack")

  # fix title
  x <- readLines(file.path(get_path_to_protocol("sfp-101-en"),
                           "01_dependencies.Rmd"))
  x[[1]] <- "# Dependencies"
  writeLines(x, con = file.path(get_path_to_protocol("sfp-101-en"),
                                "01_dependencies.Rmd"))

  # reference file missing
  file.remove(file.path(get_path_to_protocol("sfp-101-en"),
                        "references.yaml"))
  expect_error(check_structure("sfp-101-en", fail = TRUE),
               "Some problems")
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "references.yaml not found")

})
