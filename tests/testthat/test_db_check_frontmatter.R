# Tests for check_frontmatter(): validates YAML frontmatter metadata in
# index.Rmd.
# How it tests:
# - Scaffolds protocols in a mock repository, committing and tagging them
#   against origin.
# - Confirms valid frontmatter outputs "Well done! No problems found".
# - Corrupts frontmatter fields (invalid title vector, malformed version
#   string, incorrect author format).
# - Asserts check_frontmatter() surfaces specific validation errors and raises
#   errors when fail = TRUE.

test_that("Check frontmatter works", {
  language <- "en"
  setup_mock_contributors()
  # Temporary add zenodo files, fixes the citeme issue
  # (but the issue is at citeme itself)
  mock_repo <- setup_mock_bare_origin_repo(include_zenodo_files = TRUE)
  repo <- mock_repo$repo
  main_branch <- mock_repo$main_branch
  # create a protocol
  fs::dir_create(file.path(repo, "source"))
  version_number <- get_version_number()
  create_sfp(
    short_title = "water 1",
    version_number = version_number, theme = "water", language = language
  )

  # add, commit and tag it
  checklist::new_branch("sfp-101-en", repo = repo)

  git_commit_and_tag_protocol(
    protocol_code = "sfp-101-en",
    message = "sfp-101-en_water-1",
    version_number = version_number,
    tag_message = "bla"
  )
  git_push_current_branch()

  expect_output(
    check_frontmatter(
      protocol_code = "sfp-101-en",
      fail = FALSE
    ),
    "Well done! No problems found"
  )

  # merge into main (Replaced repetitive merge/push boilerplate with helper)
  git_merge_to_main_and_delete(
    branch_name = "sfp-101-en",
    main_branch = main_branch,
    repo = repo,
    delete_branch = FALSE
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

test_that("check_frontmatter flags missing required YAML keys", {
  language <- "en"
  setup_mock_contributors()
  mock_repo <- setup_mock_bare_origin_repo(include_zenodo_files = TRUE)
  repo <- mock_repo$repo
  fs::dir_create(file.path(repo, "source"))
  version_number <- get_version_number()

  create_sfp(
    short_title = "water 1",
    version_number = version_number,
    theme = "water",
    language = language,
    template = "generic"
  )
  checklist::new_branch("sfp-101-en", repo = repo)

  path_to_protocol <- get_path_to_protocol("sfp-101-en")
  index_path <- file.path(path_to_protocol, "index.Rmd")

  # Remove default YAML keys that are absent from skeleton.Rmd
  keys_to_drop <- c(
    "lang", "funder", "rightsholder", "license", "link-citations", "theme"
  )
  index_yml <- rmarkdown::yaml_front_matter(index_path) |>
    ymlthis::as_yml() |>
    ymlthis::yml_discard(keys_to_drop)

  template_rmd <- file.path(path_to_protocol, "template.rmd")
  file.copy(from = index_path, to = template_rmd)
  unlink(index_path)
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

  git_commit_and_tag_protocol(
    protocol_code = "sfp-101-en",
    message = "sfp-101-en_water-1",
    version_number = version_number,
    tag_message = "bla"
  )
  git_push_current_branch()

  out <- capture.output(
    check_frontmatter(protocol_code = "sfp-101-en", fail = FALSE)
  )
  out_text <- paste(out, collapse = "\n")

  for (key in keys_to_drop) {
    expect_match(out_text, sprintf("'%s'", key), fixed = TRUE)
  }
})

test_that("protocol_yaml_defaults and get_required_yaml_keys edge cases", {
  en_sfp <- protocol_yaml_defaults(
    language = "en",
    protocol_code = "sfp-101-en",
    version_number = "2026.01",
    template = "sfp",
    theme = "water"
  )
  expect_true("theme" %in% names(en_sfp))
  expect_false("project_name" %in% names(en_sfp))
  expect_identical(en_sfp[["lang"]], "lang: en-GB")
  expect_identical(en_sfp[["bibliography"]], "bibliography: references.yaml")

  nl_spp <- protocol_yaml_defaults(
    language = "nl",
    protocol_code = "spp-001-nl",
    version_number = "2026.01",
    template = "spp",
    project_name = "test_proj"
  )
  expect_true("project_name" %in% names(nl_spp))
  expect_false("theme" %in% names(nl_spp))
  expect_identical(nl_spp[["lang"]], "lang: nl-BE")
  expect_identical(nl_spp[["bibliography"]], "bibliography: referenties.yaml")

  expect_error(
    protocol_yaml_defaults(
      language = "fr",
      protocol_code = "sap-001-en",
      version_number = "2026.01",
      template = "sap"
    )
  )

  sfp_keys <- get_required_yaml_keys("sfp-101-en")
  spp_keys <- get_required_yaml_keys("spp-001-nl")
  sap_keys <- get_required_yaml_keys("sap-001-en")

  expect_true(
    all(c("theme", "lang", "funder", "rightsholder", "license") %in% sfp_keys)
  )
  expect_false("project_name" %in% sfp_keys)
  expect_true("project_name" %in% spp_keys)
  expect_false("theme" %in% spp_keys)
  expect_false(any(c("theme", "project_name") %in% sap_keys))
  expect_error(get_required_yaml_keys("invalid-code"))
})

test_that("yaml_interactive_keys matches the keys yaml_interactive() writes", {
  # Important test to ensure yaml_interactive_keys are in sync
  # if new interactive fields are added to yaml_interactive() in the future.
  setup_mock_contributors()
  written_yaml <- paste(yaml_interactive(language = "en"), collapse = "\n")
  written_keys <- names(yaml::yaml.load(written_yaml))
  expect_setequal(setdiff(written_keys, "subtitle"), yaml_interactive_keys)
})
