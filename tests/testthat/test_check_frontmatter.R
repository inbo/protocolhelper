test_that("Check frontmatter works", {
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
  repo <- gert::git_clone(url = origin_repo,
                          path = tempfile("protocol_local"), verbose = FALSE)
  on.exit(unlink(repo, recursive = TRUE), add = TRUE)
  old_wd <- setwd(repo)
  on.exit(setwd(old_wd), add = TRUE)

  gert::git_config_set(name = "user.name", value = "someone", repo = repo)
  gert::git_config_set(name = "user.email", value = "someone@example.org",
                       repo = repo)
  file.create("NEWS.md")
  gert::git_add("NEWS.md")
  gert::git_commit_all(message = "add empty NEWS repo file")
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(remote = "origin",
                 refspec =  refspec,
                 set_upstream = TRUE,
                 repo = repo)

  branch_info <- gert::git_branch_list(repo = repo)
  main_branch <- ifelse(any(branch_info$name == "origin/main"),
                        "main", ifelse(any(branch_info$name == "origin/master"),
                                       "master", "unknown"))
  # create a protocol
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = "en"
  )

  # add, commit and tag it
  checklist::new_branch("sfp-101-en", repo = repo)
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-101-en_water-1")
  specific_tag <- paste("sfp-101-en", version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(remote = "origin",
                 refspec =  refspec,
                 set_upstream = TRUE,
                 repo = repo)

  expect_output(check_frontmatter(protocol_code = "sfp-101-en",
                                  fail = FALSE),
                "Well done! No problems found")

  # merge into main
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_branch_checkout(main_branch)
  gert::git_merge(ref = refspec, repo = repo)
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(remote = "origin",
                 refspec =  refspec,
                 set_upstream = TRUE,
                 repo = repo)

  # another protocol
  checklist::new_branch("sfp-102-en", repo = repo)
  version_number_2 <- get_version_number(path = repo)
  protocolhelper::create_protocol(
    short_title = "water 2",
    version_number = version_number_2, theme = "water", language = "en"
  )
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-102-en_water-2")
  specific_tag <- paste("sfp-102-en", version_number_2, sep = "-")
  generic_tag <- paste("protocols", version_number_2, sep = "-")
  gert::git_tag_create(name = specific_tag, message = "bla")
  gert::git_tag_create(name = generic_tag, message = "bla")
  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_push(remote = "origin",
                 refspec =  refspec,
                 set_upstream = TRUE,
                 repo = repo)


  expect_output(check_frontmatter(protocol_code = "sfp-102-en",
                    fail = FALSE),
                "Well done! No problems found")

  # create some problems
  path_to_protocol <- get_path_to_protocol("sfp-102-en")
  x <- readLines(file.path(path_to_protocol, "index.Rmd"))
  x[[3]] <- "subtitle:"
  writeLines(x, file.path(path_to_protocol, "index.Rmd"))
  index_yml <- rmarkdown::yaml_front_matter(
    file.path(path_to_protocol, "index.Rmd"))
  index_yml <- ymlthis::as_yml(index_yml)
  index_yml <- ymlthis::yml_replace(
    index_yml,
    title = c("bla", "bla"),
    version_number = "2020.01.dev",
    language = "en")
  index_yml <- ymlthis::yml_author(
    index_yml,
    name = "Doe, John",
    orcid = "0000-1234-4321")
  template_rmd <- file.path(path_to_protocol, "template.rmd")
  parent_rmd <- file.path(path_to_protocol, "index.Rmd")
  file.copy(from = parent_rmd, to = template_rmd)
  unlink(parent_rmd)
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = path_to_protocol,
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE)
  unlink(template_rmd)

  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_commit_all(message = "mess up sfp-102-en_water-2")
  gert::git_push(remote = "origin",
                 refspec =  refspec,
                 set_upstream = TRUE,
                 repo = repo)

  expect_error(check_frontmatter(protocol_code = "sfp-102-en",
                                 fail = TRUE),
               "Some problems occur")

  expect_output(check_frontmatter(protocol_code = "sfp-102-en",
                    fail = FALSE),
                "Errors in protocol sfp-102-en:")

})
