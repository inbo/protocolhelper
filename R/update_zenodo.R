#' Update the .zenodo.json file
#'
#' Updating is done by scanning the index.Rmd files for author metadata and
#' adding missing author metadata to the list of creators in the .zenodo.json.
#' This internal function should only be used by one of the administrators.
#'
#' @param json a JSON string, url or file
#' @param write whether to write a .zenodo.json file (TRUE) or to return a json
#' string
#'
#' @importFrom purrr map flatten map_chr
#' @importFrom jsonlite fromJSON write_json toJSON
#' @importFrom fs dir_ls
#' @importFrom rmarkdown yaml_front_matter
#'
#' @return a .zenodo.json file (write = TRUE) or a json string (write = FALSE)
#'
update_zenodo <- function(json = ".zenodo.json", write = TRUE) {

  # list all index.Rmd files
  indexpaths <- fs::dir_ls(path = ".", recurse = TRUE, regexp = "index\\.Rmd") # nolint: nonportable_path_linter, line_length_linter.

  # read YAML front matter
  yamllists <- purrr::map(indexpaths, rmarkdown::yaml_front_matter)

  # extract author metadata
  authormeta <- purrr::map(yamllists, "author")

  # read .zenodo.json file
  zenodo <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  # extract creators
  creators <- zenodo$creators

  # check for authors not in creators
  authornames <- purrr::flatten(authormeta)
  authornames <- purrr::map_chr(authornames, "name")
  creatornames <- purrr::map_chr(creators, "name")
  to_add <- which(!authornames %in%
                    creatornames
                  )

  # add missing author metadata to creators
  authors_to_add <- purrr::flatten(unname(authormeta[to_add]))
  for (i in seq_along(authors_to_add)) {
    authors_to_add[[i]] <- append(
      authors_to_add[[i]],
      list(affiliation = "Research Institute for Nature and Forest"),
      after = 1)
      }
  creators <- append(creators, authors_to_add, length(creators) - 2)

  # write updated .zenodo.json file
  zenodo$creators <- creators
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
