test_that(
  "Update zenodo works", {
    jsontxt <- '{
    "title": "",
    "description": "",
    "license": "cc-by",
    "upload_type": "other",
    "access_right": "open",
    "creators": [
        {
            "name": "Van Calster, Hans",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0001-8595-8426"
        },
        {
            "name": "De Bie, Els",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0001-7679-743X"
        },
        {
            "name": "Onkelinx, Thierry",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0001-8804-4216"
        },
        {
            "name": "Vanderhaeghe, Floris",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0002-6378-6229"
        }
    ],
    "keywords": [
        "open protocol",
        "open science",
        "research institute",
        "nature",
        "forest",
        "environment",
        "markdown",
        "Flanders",
        "Belgium"
        ]
}'
    jsonresult <- '{
    "title": "",
    "description": "",
    "license": "cc-by",
    "upload_type": "other",
    "access_right": "open",
    "creators": [
        {
            "name": "Van Calster, Hans",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0001-8595-8426"
        },
        {
            "name": "De Bie, Els",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0001-7679-743X"
        },
        {
            "name": "Someone, Else",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0001-2345-6789"
        },
        {
            "name": "Onkelinx, Thierry",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0001-8804-4216"
        },
        {
            "name": "Vanderhaeghe, Floris",
            "affiliation": "Research Institute for Nature and Forest",
            "orcid": "0000-0002-6378-6229"
        }
    ],
    "keywords": [
        "open protocol",
        "open science",
        "research institute",
        "nature",
        "forest",
        "environment",
        "markdown",
        "Flanders",
        "Belgium"
        ]
}'
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
    gert::git_config_set(name = "init.defaultBranch", value = "main")

    # create a protocol
    version_number <- "2021.01"
    protocolhelper::create_sfp(
      title = "Test 1", subtitle = "subtitle", short_title = "water 1",
      authors = "Someone, Else", orcids = "0000-0001-2345-6789",
      reviewers = "me", file_manager = "who?",
      version_number = version_number, theme = "water", language = "en"
    )
    # add, commit and tag it
    sfp_staged <- gert::git_add(files = ".")
    gert::git_commit_all(message = "sfp-101-en_water-1")
    specific_tag <- paste("sfp-101-en", version_number, sep = "-")
    generic_tag <- paste("protocols", version_number, sep = "-")
    gert::git_tag_create(name = specific_tag, message = "bla")
    gert::git_tag_create(name = generic_tag, message = "bla")

    testobject <- protocolhelper:::update_zenodo(jsontxt, write = FALSE)

    expectedobject <- jsonlite::fromJSON(jsonresult, simplifyVector = FALSE)
    expectedobject <- jsonlite::toJSON(expectedobject,
                                       pretty = TRUE,
                                       auto_unbox = TRUE)

    testthat::expect_equal(
      object = testobject,
      expected = expectedobject
    )
  })
