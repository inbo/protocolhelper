test_that("test that adding dependencies to yaml works", {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("please install 'gert' package for these tests to work")
  }
  library(ymlthis)
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
    version_number = version_number,
    theme = "water",
    lang = "en"
  )

  # add a projectspecific parameter to index yaml
  index_yml <- rmarkdown::yaml_front_matter(
    file.path("source", "sfp", "1_water", "sfp_101_en_water_1", "index.Rmd"))
  unlink("css", recursive = TRUE)
  index_yml <- ymlthis::as_yml(index_yml)
  index_yml <- ymlthis::yml_params(index_yml, protocolspecific = "defaultvalue")
  template_rmd <-
    file.path("source", "sfp", "1_water", "sfp_101_en_water_1",
              "template.Rmd")
  file.copy(
    from = file.path("source", "sfp", "1_water", "sfp_101_en_water_1",
                     "index.Rmd"),
    to = template_rmd)
  unlink(file.path("source", "sfp", "1_water", "sfp_101_en_water_1",
                   "index.Rmd"))
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE)
  unlink(template_rmd)


  # add dependencies
  add_dependencies(
    code_mainprotocol = "sfp-101-en",
    protocol_code = c("sfp-123-en", "spp-124-en"),
    version_number = c("2020.01", "2020.02"),
    params = list(NA, list(width = 8, height = 8))
    )

  main <- file.path(protocolhelper:::get_path_to_protocol("sfp-101-en"),
                    "index.Rmd")

  index_yml <- rmarkdown::yaml_front_matter(main)
  unlink("css", recursive = TRUE)
  index_yml <- ymlthis::as_yml(index_yml)
  testthat::expect_equal(
    index_yml$params,
    list(protocolspecific = "defaultvalue",
         dependencies =
           list(value =
                  list(
                    list(protocol_code = "sfp-123-en",
                         version_number = "2020.01",
                         params = NA,
                         appendix = FALSE),
                    list(protocol_code = "spp-124-en",
                         version_number = "2020.02",
                         params = list(width = 8, height = 8),
                         appendix = TRUE)
                    )
                )
         )
    )
})
