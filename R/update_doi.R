#' @title Update the protocol-specific Zenodo doi
#'
#' @description
#' Gets and sets a DOI in the yaml front matter.
#' Not intended for interactive use.
#' Should be used in a GitHub Action on a PR branch.
#'
#' @param protocol_code protocol code of the protocol that is to be published in
#' this release
#' @param path Default current working directory. The root folder of the
#' `protocolsource` repo.
#'
#' @importFrom fs
#' path
#' is_dir
#' @importFrom rmarkdown yaml_front_matter
#'
#' @noRd
update_doi <- function(
    protocol_code,
    path = ".",
    sandbox = TRUE,
    token = keyring::key_get(
      c("ZENODO_SANDBOX", "ZENODO")[c(sandbox, !sandbox)]
    ),
    logger = "INFO"
) {
  check_protocolcode(protocol_code)
  assert_that(requireNamespace("zen4R", quietly = TRUE))
  assert_that(requireNamespace("keyring", quietly = TRUE))
  assert_that(requireNamespace("rlang", quietly = TRUE))
  assert_that(is.string(path), noNA(path), is.flag(sandbox), noNA(sandbox))
  assert_that(is_dir(path), msg = "`path` is not an existing directory")

  ppath <- get_path_to_protocol(protocol_code)

  # read index
  path(ppath, "index.Rmd") |>
    readLines() -> index
  # existing yaml
  index_yml <- head(index, grep("---", index)[2])[-grep("---", index)[2]]
  yml_list <- yaml_front_matter(path(ppath, "index.Rmd"))

  # get reserved doi or new version doi
  zenodo <- zen4R::ZenodoManager$new(
    token = token,
    sandbox = sandbox,
    logger = logger
  )

  if (rlang::is_empty(yml_list$doi)) {
    rec <- zenodo$createEmptyRecord()
    # de gereserveerde doi voor deze versie is dan
    doi <- rec$pids$doi$identifier
  } else {
    lookup_doi <- yml_list$doi
    myrec <- zenodo$getDepositionByDOI(lookup_doi)
    myrec <- zenodo$depositRecordVersion(
      myrec,
      delete_latest_files = TRUE,
      files = list(),
      publish = FALSE)
    doi <- myrec$pids$doi$identifier
  }

  # add or replace doi
  index_yml <- c(
    index_yml[!grepl("^doi:", index_yml)],
    sprintf("doi: %s", doi),
    "---"
  )

  # remove existing yaml
  index <- tail(index, -grep("---", index)[2])
  # add new yaml
  index <- c(index_yml, index)
  writeLines(index, path(ppath, "index.Rmd"))

  # add, commit (optionally? can also be done in GHA docker shell)



  return(invisible(doi))
}
