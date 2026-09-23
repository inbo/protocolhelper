test_that(
  "Update zenodo works",
  {
    language <- "en"
    setup_mock_contributors(language=language)
    setup_mock_repo()
    
    # create a protocol
    version_number <- "2021.02"
    protocolhelper::create_sfp(
      short_title = "water 2",
      version_number = version_number, theme = "water", language = language
    )
    # add, commit and tag it
    sfp_staged <- gert::git_add(files = ".")
    gert::git_commit_all(message = "sfp-102-en_water-1")
    specific_tag <- paste("sfp-102-en", version_number, sep = "-")
    generic_tag <- paste("protocols", version_number, sep = "-")
    gert::git_tag_create(name = specific_tag, message = "bla")
    gert::git_tag_create(name = generic_tag, message = "bla")

    # new authors added

    # Prepare base JSON and expected result with author added to contributors
    input_json <- mock_zenodo_json()
    expected_list <- jsonlite::fromJSON(input_json, simplifyVector = FALSE)
    expected_list$contributors <- list(
      list(
        name = list(given = "Hans", family = "Van Calster"),
        type = "Researcher",
        email = "hans.vancalster@inbo.be",
        orcid = "0000-0001-8595-8426",
        affiliation = "Research Institute for Nature and Forest (INBO)",
        corresponding = TRUE
      )
    )
    expected_json <- jsonlite::toJSON(
      expected_list,
      pretty = TRUE,
      auto_unbox = TRUE
    )

    # Test that update_zenodo() adds the new author to contributors
    actual_json <- protocolhelper:::update_zenodo(input_json, write = FALSE)

    testthat::expect_equal(
      object = actual_json,
      expected = expected_json
    )

  }
)
