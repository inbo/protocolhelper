# Tests for update_zenodo(): synchronizes protocol contributor metadata into Zenodo JSON.
# How it tests:
# - Sets up mock contributors and scaffolds a tagged protocol (sfp-102-en).
# - Loads a base JSON payload using mock_zenodo_json().
# - Invokes update_zenodo(write = FALSE) to obtain updated metadata without file writes.
# - Asserts that the output JSON matches expected contributor attributes (name, ORCID, affiliation).

test_that(
  "Update zenodo works",
  {
    language <- "en"
    setup_mock_contributors(language=language)
    setup_mock_bare_origin_repo()
    
    # create a protocol
    version_number <- "2021.02"
    protocolhelper::create_sfp(
      short_title = "water 2",
      version_number = version_number, theme = "water", language = language
    )

    # add, commit and tag it
    git_commit_and_tag_protocol(
      protocol_code = "sfp-102-en",
      message = "sfp-102-en_water-1",
      version_number = version_number,
      tag_message = "bla"
    )
    
    # Prepare base JSON and 
    input_json <- mock_zenodo_json()
    
    # Test that update_zenodo() adds the new author to contributors
    actual_json <- protocolhelper:::update_zenodo(input_json, write = FALSE)

    # Create a JSON containing the expected output from update_zenodo (with contributors added)
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

    testthat::expect_equal(
      object = actual_json,
      expected = expected_json
    )

  }
)
