test_that("complete workflow works", {
  if (!requireNamespace("gert", quietly = TRUE)) {
    stop("please install 'gert' package for these tests to work")
  }
  skip_if_offline()
  skip_if_not_installed("zen4R")
  skip_if_not_installed("keyring")
  skip_if(
    assertthat::is.error(try(keyring::key_get("ZENODO_SANDBOX"), silent = TRUE))
  )

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
    ask_yes_no = function(...) FALSE,
    use_author = function(...) author_df,
    use_reviewer = function(...) reviewer_df,
    use_file_manager = function(...) file_manager_df,
    readline = function(...) "Een titel"
  )

  update_news <- function(path, version_number) {
    news <- readLines(file.path(path, "NEWS.md"))
    writeLines(
      c(
        head(news, 2),
        sprintf("\n## [%1$s](../%1$s/index.html)\n", version_number),
        rep("- blabla blabla", 1 + rpois(1, lambda = 3)),
        tail(news, -2)
      ),
      file.path(path, "NEWS.md")
    )
  }
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

  origin_repo <- gert::git_init(tempfile("protocol_origin"), bare = TRUE)
  on.exit(unlink(origin_repo, recursive = TRUE), add = TRUE)
  repo <- gert::git_clone(
    url = origin_repo,
    path = tempfile("protocol_local"), verbose = FALSE
  )
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  old_wd <- setwd(repo)
  on.exit(setwd(old_wd), add = TRUE)

  gert::git_config_set(name = "user.name", value = "someone", repo = repo)
  gert::git_config_set(
    name = "user.email", value = "someone@example.org",
    repo = repo
  )
  file.create("NEWS.md")
  file.create(".zenodo.json")
  writeLines(jsontxt, con = ".zenodo.json")

  file.create(".gitignore")
  writeLines(c("docs/", "publish/"), con = ".gitignore")
  gert::git_add("NEWS.md")
  gert::git_commit_all(message = "add empty NEWS repo file")

  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )

  branch_info <- gert::git_branch_list(repo = repo)
  main_branch <- ifelse(any(branch_info$name == "origin/main"),
    "main", ifelse(any(branch_info$name == "origin/master"),
      "master", "unknown"
    )
  )

  # create a protocol
  checklist::new_branch("sfp-101-en", repo = repo)
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = "en"
  )

  # extra reviewer toevoegen
  # read index template
  path_to_protocol <- get_path_to_protocol("sfp-101-en")
  path(path_to_protocol, "index.Rmd") |>
    readLines() -> index
  yaml <- head(index, grep("---", index)[2])
  yaml <- yaml[-1]
  yaml <- yaml[-38]
  yaml <- c(yaml[1:17], yaml[12:17], yaml[18:37])
  yaml[13] <- gsub("Els", "Pieter", yaml[13])
  yaml[14] <- gsub("Lommelen", "Verschelde", yaml[14])
  yaml[15] <- gsub("els.lommelen", "pieter.verschelde", yaml[15])
  yaml[16] <- gsub("0000-0002-3481-5684", "0000-0002-9199-421X", yaml[16])

  # remove existing yaml
  index <- tail(index, -grep("---", index)[2])
  # add new yaml
  index <- c("---", yaml, "---", index)
  writeLines(index, path(path_to_protocol, "index.Rmd"))


  update_news(
    path = file.path("source", "sfp", "1_water", "sfp_101_en_water_1"),
    version_number = version_number
  )

  protocolhelper:::update_news_release("sfp-101-en")
  protocolhelper:::update_zenodo()
  doi <- protocolhelper:::update_doi("sfp-101-en")

  # add, commit and tag it
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water-1")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )

  # merge into main
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_branch_checkout(main_branch)
  gert::git_merge(ref = refspec, repo = repo)
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )
  gert::git_branch_delete("sfp-101-en", repo = origin_repo)
  gert::git_branch_delete("sfp-101-en", repo = repo)

  protocolhelper:::render_release()

  # Cleanup
  unlink(repo, recursive = TRUE)
})
