# ==============================================================================
# Unit Tests: check_frontmatter()
#
# What is tested:
#   - Front-matter validation for valid protocols and for edited subtitle, title,
#     version, and author fields.
#   - Error behavior with fail = TRUE and printed diagnostics with fail = FALSE.
#
# How it is tested:
#   - Initializes a mock git repository with a bare origin and contributor metadata.
#   - Creates, tags, and pushes two valid protocols (merging the first into main),
#     then confirms check_frontmatter() reports no problems for both.
#   - Edits the second protocol's front matter, commits and pushes it, then checks
#     for an error with fail = TRUE and diagnostic output with fail = FALSE.
#   - Does not test conflicts against origin branches or tags.
# ==============================================================================

test_that("Check frontmatter works", {
  language <- "en"
  setup_mock_contributors()
  mock_repo <- setup_mock_repo(with_origin = TRUE)
  repo <-mock_repo$repo
  # create a protocol
  # why do this?
  fs::dir_create(file.path(repo, "source"))
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
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
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )

  expect_output(
    check_frontmatter(
      protocol_code = "sfp-101-en",
      fail = FALSE
    ),
    "Well done! No problems found"
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

  # another protocol
  version_number_2 <- get_version_number(path = repo)

  protocolhelper::create_protocol(
    short_title = "water 2",
    version_number = version_number_2, theme = "water", language = "en"
  )
  checklist::new_branch("sfp-102-en", repo = repo)
  sfp_staged <- gert::git_add(files = ".")
  gert::git_commit_all(message = "sfp-102-en_water-2")
  specific_tag <- paste("sfp-102-en", version_number_2, sep = "-")
  generic_tag <- paste("protocols", version_number_2, sep = "-")
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

  expect_output(
    check_frontmatter(
      protocol_code = "sfp-102-en",
      fail = FALSE
    ),
    "Well done! No problems found"
  )

  # create some problems
  path_to_protocol <- get_path_to_protocol("sfp-102-en")
  x <- readLines(file.path(path_to_protocol, "index.Rmd"))
  x[[3]] <- "subtitle:"
  writeLines(x, file.path(path_to_protocol, "index.Rmd"))
  index_yml <- rmarkdown::yaml_front_matter(
    file.path(path_to_protocol, "index.Rmd")
  )
  index_yml <- ymlthis::as_yml(index_yml)
  index_yml <- ymlthis::yml_replace(
    index_yml,
    title = c("bla", "bla"),
    version_number = "2020.01.dev",
    language = "en"
  )
  index_yml <- ymlthis::yml_author(
    index_yml,
    name = "Doe, John",
    orcid = "0000-1234-4321"
  )
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
    open_doc = FALSE
  )
  unlink(template_rmd)

  branch_info <- gert::git_branch_list(repo = repo)
  refspec <- branch_info$ref[branch_info$name == gert::git_branch(repo = repo)]
  gert::git_commit_all(message = "mess up sfp-102-en_water-2")
  gert::git_push(
    remote = "origin",
    refspec = refspec,
    set_upstream = TRUE,
    repo = repo
  )

  expect_error(
    check_frontmatter(
      protocol_code = "sfp-102-en",
      fail = TRUE
    ),
    "Some problems occur"
  )

  expect_output(
    check_frontmatter(
      protocol_code = "sfp-102-en",
      fail = FALSE
    ),
    "Errors in protocol sfp-102-en:"
  )
})
