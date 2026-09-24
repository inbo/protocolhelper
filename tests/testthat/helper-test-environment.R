# Helper functions for protocolhelper test suite
# Automatically loaded by testthat before running tests

#' Mock contributors data frames
#'
#' Returns a list of default mock data frames for author, reviewer,
#' and file manager matching the specified language affiliation.
#' The language affiliation is required for the citeme package, it does not allaw a mismatch
#' in the author and reviewer roles
#' @param language Language for contributor affiliations, either `"en"` (English, default)
#'   or `"nl"` (Dutch).
#' @return A list of data frames with elements `author_df`, `reviewer_df`, and `file_manager_df`.
mock_contributors_df <- function(language = c("en", "nl")) {
  language <- match.arg(language)
  affiliation <- switch(
    language,
    "en" = "Research Institute for Nature and Forest (INBO)",
    "nl" = "Instituut voor Natuur- en Bosonderzoek (INBO)"
  )

  author_df <- data.frame(
    given = c("Hans"),
    family = c("Van Calster"),
    email = c("hans.vancalster@inbo.be"),
    orcid = c("0000-0001-8595-8426"),
    affiliation = affiliation,
    stringsAsFactors = FALSE
  )
  reviewer_df <- data.frame(
    given = c("Els"),
    family = c("Lommelen"),
    email = c("els.lommelen@inbo.be"),
    orcid = c("0000-0002-3481-5684"),
    affiliation = affiliation,
    stringsAsFactors = FALSE
  )
  file_manager_df <- data.frame(
    given = c("Pieter"),
    family = c("Verschelde"),
    email = c("pieter.verschelde@inbo.be"),
    orcid = c("0000-0002-9199-421X"),
    affiliation = affiliation,
    stringsAsFactors = FALSE
  )
  list(
    author_df = author_df,
    reviewer_df = reviewer_df,
    file_manager_df = file_manager_df
  )
}

#' Setup mock interactive prompts and contributor bindings
#'
#' Mocks `ask_yes_no`, `select_individual`, `select_reviewer`,
#' `select_file_manager`, and `readline` for the duration of the calling
#' environment (using [testthat::local_mocked_bindings]).
#' 
#' The local_mocked_bindings function replaces the bindings of the variables with the anonymous functions
#' protocolhelper will now call these functions instead of the interactive functions.
#' 
#' @param language Language for contributor affiliations (`"en"` or `"nl"`).
#' @param env The environment to attach the mocked bindings to (defaults to caller).
#' @param author_df Data frame for author selection (defaults to standard mock).
#' @param reviewer_df Data frame for reviewer selection (defaults to standard mock).
#' @param file_manager_df Data frame for file manager selection (defaults to standard mock).
#' @param readline_value Character value returned by `readline()` (default "Een titel").
#'
#' @return None. Called for its side-effect of setting mocked bindings.
setup_mock_contributors <- function(language = c("en", "nl"),
                                    env = parent.frame(), # need to keep parent environment for mock setup
                                    author_df = NULL,
                                    reviewer_df = NULL,
                                    file_manager_df = NULL,
                                    readline_value = "Een titel") {
  language <- match.arg(language)
  defaults <- mock_contributors_df(language = language)
  if (is.null(author_df)) author_df <- defaults$author_df
  if (is.null(reviewer_df)) reviewer_df <- defaults$reviewer_df
  if (is.null(file_manager_df)) file_manager_df <- defaults$file_manager_df

  # Handle string or function for readline_value
  readline_fn <- if (is.function(readline_value)) {
    readline_value
  } else {
    function(...) readline_value
  }

  testthat::local_mocked_bindings(
    ask_yes_no = function(...) FALSE,
    select_individual = function(...) author_df,
    select_reviewer = function(...) reviewer_df,
    select_file_manager = function(...) file_manager_df,
    readline = readline_fn,
    .env = env
  )
}

#' Default mock Zenodo JSON metadata
#'
#' @return A character string with valid `.zenodo.json` contents.
mock_zenodo_json <- function() {
  '{
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
}

#' Set up a temporary local Git repository without a bare origin
#'
#' Creates an empty repository with an origin URL for citation metadata, but
#' no reachable local bare remote or remote-tracking branches. Tests using this
#' fixture should not fetch or push. Cleanup and working-directory restoration
#' are deferred to `env`.
#'
#' @param env Environment controlling cleanup lifetime (defaults to caller).
#' @return A list with `repo`, `origin_repo` (NULL), and `main_branch`.
setup_mock_local_repo <- function(env = parent.frame()) {
  repo <- tempfile("test_protocol")
  withr::defer(unlink(repo, recursive = TRUE), envir = env)
  
  gert::git_init(path = repo)
  old_wd <- setwd(repo)
  withr::defer(setwd(old_wd), envir = env)

  # keep url origin as citeme uses it for citation metadata
  gert::git_remote_add(url = "https://github.com/inbo/unittests", repo = repo)
  gert::git_config_set(name = "user.name", value = "someone", repo = repo)
  gert::git_config_set(name = "user.email", value = "someone@example.org", repo = repo)

  list(repo = repo, origin_repo = NULL, main_branch = gert::git_branch(repo = repo))
}

#' Set up a temporary Git clone with a local bare origin
#'
#' Creates a bare repository, clones it locally, commits `NEWS.md`, and pushes
#' the initial branch with upstream tracking. Cleanup and working-directory
#' restoration are deferred to `env`.
#'
#' @param include_zenodo_files If TRUE, also commits `.zenodo.json` and
#'   `.gitignore`. This does not mock Zenodo API calls.
#' @param env Environment controlling cleanup lifetime (defaults to caller).
#' @return A list with `repo`, `origin_repo`, and `main_branch`.
setup_mock_bare_origin_repo <- function(include_zenodo_files = FALSE,
                                        env = parent.frame()) {
  protocol_origin_path <- tempfile("protocol_origin")
  protocol_local_path <- tempfile("protocol_local")

  origin_repo <- gert::git_init(protocol_origin_path , bare = TRUE)
  withr::defer(unlink(origin_repo, recursive = TRUE), envir = env)

  repo <- gert::git_clone(
    url = origin_repo,
    path = protocol_local_path,
    verbose = FALSE
  )
  withr::defer(unlink(repo, recursive = TRUE), envir = env)

  old_wd <- setwd(repo)
  withr::defer(setwd(old_wd), envir = env)

  gert::git_config_set(name = "user.name", value = "someone", repo = repo)
  gert::git_config_set(name = "user.email", value = "someone@example.org", repo = repo)

  file.create("NEWS.md")
  if (include_zenodo_files) {
    writeLines(mock_zenodo_json(), con = ".zenodo.json")
    writeLines(c("docs/", "publish/"), con = ".gitignore")
  }

  gert::git_add(".", repo = repo)
  gert::git_commit_all(message = "add empty NEWS repo file", repo = repo)
  git_push_current_branch(repo = repo)

  branch_info <- gert::git_branch_list(repo = repo)
  main_branch <- if ("origin/main" %in% branch_info$name) {
    "main"
  } else if ("origin/master" %in% branch_info$name) {
    "master"
  } else {
    stop("No origin/main or origin/master branch found in mock repository")
  }

  list(repo = repo, origin_repo = origin_repo, main_branch = main_branch)
}

#' Push the currently checked-out branch to remote
#'
#' @param repo Path to the git repository (default ".").
#' @param remote Name of the remote (default "origin").
#' @param set_upstream Logical. Whether to set tracking upstream (default TRUE).
git_push_current_branch <- function(repo = ".", remote = "origin", set_upstream = TRUE) {
  branch_info <- gert::git_branch_list(repo = repo)
  current_branch <- gert::git_branch(repo = repo)
  refspec <- branch_info$ref[branch_info$name == current_branch]
  gert::git_push(
    remote = remote,
    refspec = refspec,
    set_upstream = set_upstream,
    repo = repo
  )
}

#' Stage, commit, and create version tags for a protocol
#' The changes are staged in the temporary repo
#' 
#' @param protocol_code Character. E.g. "sfp-101-en".
#' @param version_number Character. E.g. "2021.01".
#' @param message Commit message. Defaults to `paste(protocol_code, version_number, sep = "_")`.
#' @param tag_message Message attached to both tags (default "test tag").
#' @param repo Path to git repository (default ".").
#' @param remote Remote name to push to (default "origin").
#' @param push Logical. Whether to push after tagging (default TRUE).
git_commit_and_tag_protocol <- function(protocol_code,
                                       version_number,
                                       message = paste(protocol_code, version_number, sep = "_"),
                                       tag_message = "test tag",
                                       repo = ".",
                                       remote = "origin",
                                       push = TRUE) {
  gert::git_add(files = ".", repo = repo) 
  gert::git_commit_all(message = message, repo = repo)
  specific_tag <- paste(protocol_code, version_number, sep = "-")
  generic_tag <- paste("protocols", version_number, sep = "-")
  gert::git_tag_create(name = specific_tag, message = tag_message, repo = repo)
  gert::git_tag_create(name = generic_tag, message = tag_message, repo = repo)
  if (push) {
    git_push_current_branch(repo = repo, remote = remote)
  }
}

#' Merge a feature branch into main, push, and delete the feature branch
#'
#' @param branch_name Branch to merge and delete.
#' @param main_branch Main branch name (default "main").
#' @param repo Path to local git repo (default ".").
#' @param origin_repo Path to origin bare repo (if deleting remote branch as well).
#' @param remote Remote name (default "origin").
git_merge_to_main_and_delete <- function(branch_name,
                                         main_branch = "main",
                                         repo = ".",
                                         origin_repo = NULL,
                                         remote = "origin") {
  gert::git_branch_checkout(main_branch, repo = repo)
  gert::git_merge(ref = branch_name, repo = repo)
  git_push_current_branch(repo = repo, remote = remote)
  if (!is.null(origin_repo)) {
    gert::git_branch_delete(branch_name, repo = origin_repo)
  }
  gert::git_branch_delete(branch_name, repo = repo)
}

#' Update protocol NEWS.md with a new release section
#'
#' @param path Path to the protocol directory containing NEWS.md.
#' @param version_number Version number string (e.g. "2021.01").
#' @param n_bullets Number of bullet points to insert (default 2, used to be rpois).
update_news <- function(path, version_number, n_bullets = 2) {
  news_file <- file.path(path, "NEWS.md")
  news <- readLines(news_file)
  writeLines(
    c(
      head(news, 2),
      sprintf("\n## [%1$s](../%1$s/index.html)\n", version_number),
      rep("- blabla blabla", n_bullets),
      tail(news, -2)
    ),
    news_file
  )
}

#' Inject an invalid news version for testing check_all / check_news
#'
#' @param path Path to the protocol directory containing NEWS.md.
#' @param version_number Invalid version string (default "1900.01").
make_news_error <- function(path, version_number = "1900.01") {
  news_file <- file.path(path, "NEWS.md")
  news <- readLines(news_file)
  writeLines(
    c(
      head(news, 2),
      sprintf("\n## [%1$s](../%1$s/index.html)\n", version_number),
      rep("- blabla blabla", 2),
      tail(news, -2)
    ),
    news_file
  )
}
