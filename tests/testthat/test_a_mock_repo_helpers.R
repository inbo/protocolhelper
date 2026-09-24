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
