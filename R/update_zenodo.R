#' Update the `.zenodo.json` file
#'
#' Updating is done by scanning the `index.Rmd` files for author metadata and
#' adding missing author metadata to the list of creators in the `.zenodo.json`.
#' This internal function should only be used by one of the administrators.
#'
#' @param json a `JSON` string, url or file
#' @param write whether to write a `.zenodo.json` file (TRUE) or to return a
#' `json`
#' string
#' @param path Default current working directory. The root folder of the
#' `protocolsource` repo.
#'
#' @importFrom purrr map flatten map_chr
#' @importFrom jsonlite fromJSON write_json toJSON
#' @importFrom fs dir_ls is_dir
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom gert git_branch git_branch_list git_branch_checkout
#' @importFrom assertthat assert_that is.flag is.string noNA
#'
#' @noRd
#'
#' @return a `.zenodo.json` file (write = TRUE) or a `json` string
#' (write = FALSE)
#'
update_zenodo <- function(json = ".zenodo.json", write = TRUE, path = ".") {
  assert_that(is_dir(path))
  assert_that(is.flag(write), noNA(write))

  # list all index.Rmd files
  indexpaths <- fs::dir_ls(path = path, recurse = TRUE, regexp = "index\\.Rmd")

  # read YAML front matter
  yamllists <- purrr::map(indexpaths, rmarkdown::yaml_front_matter)

  # extract author metadata
  authormeta <- purrr::map(yamllists, "author")

  # read .zenodo.json file from main branch
  current_branch <- git_branch(repo = path)
  branch_info <- git_branch_list(repo = path)
  main_branch <- ifelse(any(branch_info$name == "origin/main"),
                        "main", ifelse(any(branch_info$name == "origin/master"),
                                       "master", "unknown"))
  assert_that(main_branch %in% c("main", "master"),
              msg = "no branch `origin/main` or `origin/master` found.")
  git_branch_checkout(branch = main_branch)

  zenodo <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  # switch back to current branch
  git_branch_checkout(current_branch)

  # extract contributors
  contributors <- zenodo$contributors

  # check for authors not in contributors based on orcid
  orcids <- purrr::flatten(authormeta)
  orcids <- purrr::map_chr(orcids, "orcid")
  contributororcids <- purrr::map_chr(contributors, "orcid")
  to_add <- which(!orcids %in%
                    contributororcids
                  )

  # add missing author metadata to contributors
  authors_to_add <- purrr::flatten(unname(authormeta))[to_add]
  for (i in seq_along(authors_to_add)) {
    authors_to_add[[i]] <- append(
      authors_to_add[[i]],
      list(type = "Researcher"),
      after = 1)
      }
  contributors <- append(contributors, authors_to_add)

  # write updated .zenodo.json file
  zenodo$contributors <- contributors
  if (write) {
    jsonlite::write_json(x = zenodo,
                         ".zenodo.json",
                         pretty = TRUE,
                         auto_unbox = TRUE)
  } else {
    jsonlite::toJSON(x = zenodo,
                     pretty = TRUE,
                     auto_unbox = TRUE)
  }
}
