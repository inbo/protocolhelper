test_that("check structure works", {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("please install 'gert' package for these tests to work")
  }
  author_df <- data.frame(
    stringsAsFactors = FALSE,
    given = c("Hans"),
    family = c("Van Calster"),
    email = c("hans.vancalster@inbo.be"),
    orcid = c("0000-0001-8595-8426"),
    affiliation = c("Research Institute for Nature and Forest (INBO)")
  )
  reviewer_df <- data.frame(
    stringsAsFactors = FALSE,
    given = c("Els"),
    family = c("Lommelen"),
    email = c("els.lommelen@inbo.be"),
    orcid = c("0000-0002-3481-5684"),
    affiliation = c("Research Institute for Nature and Forest (INBO)")
  )
  file_manager_df <- data.frame(
    stringsAsFactors = FALSE,
    given = c("Pieter"),
    family = c("Verschelde"),
    email = c("pieter.verschelde@inbo.be"),
    orcid = c("0000-0002-9199-421X"),
    affiliation = c("Instituut voor Natuur- en Bosonderzoek (INBO)")
  )


  local_mocked_bindings(
    ui_yeah = function(...) FALSE,
    use_author = function(...) author_df,
    use_reviewer = function(...) reviewer_df,
    use_file_manager = function(...) file_manager_df,
    readline = function(...) "Een titel"
  )


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
    short_title = "water 1",
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

  # add reference file back again and remove a template Rmd file
  file.create(file.path(get_path_to_protocol("sfp-101-en"),
                        "references.yaml"))
  x <- readLines(file.path(get_path_to_protocol("sfp-101-en"),
                           "02_subject.Rmd"))
  file.remove(file.path(get_path_to_protocol("sfp-101-en"),
                        "02_subject.Rmd"))
  expect_error(check_structure("sfp-101-en", fail = TRUE),
               "Some problems")
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "02_subject.Rmd")
  writeLines(x, file.path(get_path_to_protocol("sfp-101-en"),
                          "02_subject.Rmd"))

  # add Rmd file with duplicate chapter number
  file.create(file.path(get_path_to_protocol("sfp-101-en"),
                        "01_afhankelijkheden.Rmd"))
  expect_error(check_structure("sfp-101-en", fail = TRUE),
               "Some problems")
  expect_output(
    check_structure("sfp-101-en", fail = FALSE),
    "01")
  file.remove(file.path(get_path_to_protocol("sfp-101-en"),
                        "01_afhankelijkheden.Rmd"))


  # create a protocol with generic template
  version_number <- "2021.02"
  create_sfp(
    short_title = "water 2",
    theme = "water", language = "en", version_number =  version_number,
    template = "generic"
  )

  expect_output(
    check_structure("sfp-102-en", fail = TRUE),
    "No problems")


  # Cleanup
  unlink(test_repo, recursive = TRUE)

})
