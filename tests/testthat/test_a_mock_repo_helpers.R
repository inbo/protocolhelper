# Tests for mock repository helpers: sets up isolated local and bare-origin Git environments.
# How it tests:
# - Calls setup_mock_local_repo() and asserts the mock repo lacks an upstream tracking branch.
# - Calls setup_mock_bare_origin_repo() with Zenodo files enabled.
# - Verifies git fetch succeeds, tracking branches point to origin/main, and .zenodo.json is created.

test_that("local mock repository has no bare origin or tracking branch", {
  mock_repo <- setup_mock_local_repo()

  remotes <- gert::git_remote_list(repo = mock_repo$repo)
  expect_identical(nrow(remotes), 1L)
  expect_identical(remotes$name, "origin")
  expect_identical(remotes$url, "https://github.com/inbo/unittests")

  expect_error(gert::git_fetch(remote = "origin", repo = mock_repo$repo))
  expect_length(grep("^origin/", gert::git_branch_list(repo = mock_repo$repo)$name), 0L)
  expect_null(mock_repo$origin_repo)
  expect_identical(
    normalizePath(getwd(), winslash = "/"),
    normalizePath(mock_repo$repo, winslash = "/")
  )
})

test_that("bare-origin mock repository has a reachable main branch", {
  mock_repo <- setup_mock_bare_origin_repo(include_zenodo_files = TRUE)

  expect_identical(nrow(gert::git_remote_list(repo = mock_repo$repo)), 1L)
  expect_no_error(gert::git_fetch(remote = "origin", repo = mock_repo$repo))
  expect_identical(mock_repo$main_branch, gert::git_branch(repo = mock_repo$repo))
  expect_match(
    paste(gert::git_branch_list(repo = mock_repo$repo)$name, collapse = " "),
    paste0("origin/", mock_repo$main_branch),
    fixed = TRUE
  )
  expect_true(file.exists(file.path(mock_repo$repo, ".zenodo.json")))
})

test_that("git_merge_to_main_and_delete merges feature branch and deletes it", {
  mock_repo <- setup_mock_bare_origin_repo()
  repo <- mock_repo$repo
  origin_repo <- mock_repo$origin_repo
  main_branch <- mock_repo$main_branch

  checklist::new_branch("sfp-101-en", repo = repo)
  writeLines("test content", file.path(repo, "feature.txt"))
  gert::git_add("feature.txt", repo = repo)
  gert::git_commit("add feature.txt", repo = repo)
  git_push_current_branch(repo = repo)

  git_merge_to_main_and_delete(
    branch_name = "sfp-101-en",
    main_branch = main_branch,
    repo = repo,
    origin_repo = origin_repo
  )

  expect_identical(gert::git_branch(repo = repo), main_branch)
  expect_true(file.exists(file.path(repo, "feature.txt")))
  expect_false("sfp-101-en" %in% gert::git_branch_list(repo = repo)$name)
  expect_false("sfp-101-en" %in% gert::git_branch_list(repo = origin_repo)$name)
})

