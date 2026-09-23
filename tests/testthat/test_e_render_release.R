test_that("complete workflow works", {
  skip_if_offline()
  skip_if(Sys.getenv("MY_UNIVERSE") != "") # skip test on r-universe.dev
  skip_if_not_installed("zen4R")
  skip_if_not_installed("keyring")
  skip_if(
    assertthat::is.error(try(keyring::key_get("ZENODO_SANDBOX"), silent = TRUE))
  )
  language <- "en"
  setup_mock_contributors(language=language)

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
  
  mock_repo <- setup_mock_repo(with_origin=TRUE, with_zenodo=TRUE)
  origin_repo <- mock_repo$origin_repo
  main_branch <- mock_repo$main_branch
  repo <- mock_repo$repo

  # create a protocol
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )
  checklist::new_branch("sfp-101-en", repo = repo)

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

  expect_no_error(protocolhelper:::render_release())
})
