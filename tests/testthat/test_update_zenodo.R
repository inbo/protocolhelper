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
    jsonresult1 <- '{
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
    ],
    "contributors": [
        {
            "name": {
                "given": "Hans",
                "family": "Van Calster"
            },
            "type": "Researcher",
            "email": "hans.vancalster@inbo.be",
            "orcid": "0000-0001-8595-8426",
            "affiliation": "Research Institute for Nature and Forest (INBO)",
            "corresponding": true
        }
    ]
}'


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


    origin_repo <- gert::git_init(tempfile("protocol_origin"), bare = TRUE)
    on.exit(unlink(origin_repo, recursive = TRUE), add = TRUE)
    repo <- gert::git_clone(
      url = origin_repo,
      path = tempfile("protocol_local"), verbose = FALSE)
    on.exit(unlink(repo, recursive = TRUE), add = TRUE)
    old_wd <- setwd(repo)
    on.exit(setwd(old_wd), add = TRUE)

    gert::git_config_set(name = "user.name", value = "someone", repo = repo)
    gert::git_config_set(
      name = "user.email", value = "someone@example.org",
      repo = repo)
    file.create("NEWS.md")
    gert::git_add("NEWS.md")
    gert::git_commit_all(message = "add empty NEWS repo file")
    branch_info <- gert::git_branch_list(repo = repo)
    refspec <- branch_info$ref[
      branch_info$name == gert::git_branch(repo = repo)]
    gert::git_push(
      remote = "origin",
      refspec =  refspec,
      set_upstream = TRUE,
      repo = repo)

    branch_info <- gert::git_branch_list(repo = repo)
    main_branch <- ifelse(
      any(branch_info$name == "origin/main"),
      "main",
      ifelse(
        any(branch_info$name == "origin/master"),
        "master", "unknown"))


    # create a protocol
    version_number <- "2021.02"
    protocolhelper::create_sfp(
      short_title = "water 2",
      version_number = version_number, theme = "water", language = "en"
    )
    # add, commit and tag it
    sfp_staged <- gert::git_add(files = ".")
    gert::git_commit_all(message = "sfp-102-en_water-1")
    specific_tag <- paste("sfp-102-en", version_number, sep = "-")
    generic_tag <- paste("protocols", version_number, sep = "-")
    gert::git_tag_create(name = specific_tag, message = "bla")
    gert::git_tag_create(name = generic_tag, message = "bla")

    # new authors added
    testobject <- protocolhelper:::update_zenodo(jsontxt, write = FALSE)
    expectedobject <- jsonlite::fromJSON(jsonresult1, simplifyVector = FALSE)
    expectedobject <- jsonlite::toJSON(
      expectedobject,
      pretty = TRUE,
      auto_unbox = TRUE)

    testthat::expect_equal(
      object = testobject,
      expected = expectedobject
    )
})
