# ==============================================================================
# Unit Tests: add_dependencies()
#
# What is tested:
#   - Adding protocol dependencies to a protocol's index.Rmd YAML front matter.
#   - Correct population of dependency metadata: protocol code, version number,
#     custom rendering parameters, and appendix flag.
#   - Preservation of existing project-specific parameters in the YAML front matter.
#
# How it is tested:
#   - Initializes a mock git repository and mock contributors.
#   - Scaffolds a new SFP protocol (sfp-101-en) via create_sfp().
#   - Injects custom YAML parameters into index.Rmd using ymlthis.
#   - Calls add_dependencies() with multiple subprotocol specifications and parameter lists.
#   - Parses the resulting index.Rmd YAML front matter and asserts that the params
#     structure exactly matches expected nested configuration.
# ==============================================================================

test_that("test that adding dependencies to yaml works", {
  library(ymlthis)
  language <- "en"
  setup_mock_contributors(language =language)
  # setup_mock_local_repo()
  setup_mock_bare_origin_repo()
  # create a protocol
  version_number <- "2021.01"
  create_sfp(
    short_title = "water 1",
    version_number = version_number,
    theme = "water",
    language = language
  )

  # add a projectspecific parameter to index yaml
  index_yml <- rmarkdown::yaml_front_matter(
    file.path("source", "sfp", "1_water", "sfp_101_en_water_1", "index.Rmd")
  )
  unlink("css", recursive = TRUE)
  index_yml <- ymlthis::as_yml(index_yml)

  # this is an added parameter to the index yml.
  index_yml <- ymlthis::yml_params(index_yml, protocolspecific = "defaultvalue")
  # this is the index_rmd without params in the yml header
  index_rmd <- file.path(
      "source", "sfp", "1_water", "sfp_101_en_water_1",
      "index.Rmd"
    )

  # instead of creating a template rmd that is a copy of the index rmd
  # we use the overwrite options of the usethis package
  withr::with_options(list(usethis.overwrite = TRUE), {
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    template = index_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE
  )
})
  # add dependencies

  add_dependencies(
    code_mainprotocol = "sfp-101-en",
    protocol_code = c("sfp-123-en", "spp-124-en"),
    version_number = c("2020.01", "2020.02"),
    params = list(NA, list(width = 8, height = 8))
  )

  main <- file.path(
    protocolhelper:::get_path_to_protocol("sfp-101-en"),
    "index.Rmd"
  )

  index_yml <- rmarkdown::yaml_front_matter(main)
  unlink("css", recursive = TRUE)
  index_yml <- ymlthis::as_yml(index_yml)
  # Comparing the params of index with the added dependencies in the header
  testthat::expect_equal(
    index_yml$params,
    list(
      protocolspecific = "defaultvalue",
      dependencies =
        list(
          value = list(
            list(
              protocol_code = "sfp-123-en",
              version_number = "2020.01",
              params = NA,
              appendix = FALSE
            ),
            list(
              protocol_code = "spp-124-en",
              version_number = "2020.02",
              params = list(width = 8, height = 8),
              appendix = TRUE
            )
          )
        )
    )
  )
})
