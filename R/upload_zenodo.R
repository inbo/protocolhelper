#' @title Upload the protocol to protocol-specific Zenodo archive
#'
#' @description
#' Not intended for interactive use.
#' Should be used in a GitHub Action on main branch after merge of a protocol
#' Helper function used in protocolhelper:::render_release()
#'
#' @importFrom zen4R ZenodoManager
#' @importFrom utils zip
#'
#' @noRd
upload_zenodo <- function(
    yaml,
    rendered_folder,
    source_folder,
    sandbox = TRUE,
    token = keyring::key_get(
      c("ZENODO_SANDBOX", "ZENODO")[c(sandbox, !sandbox)]
    ),
    logger = "INFO"
) {
  assert_that(requireNamespace("zen4R", quietly = TRUE))
  assert_that(requireNamespace("keyring", quietly = TRUE))

  # prepare the data to upload
  # keep pdf separate
  pdf <- dir_ls(path = rendered_folder, glob = "*.pdf")
  assert_that(is.string(pdf))
  # zip html
  not_pdf <- dir_ls(
    path = rendered_folder, glob = "*.pdf", invert = TRUE, recurse = TRUE
  )
  not_pdf <- path_rel(not_pdf, start = rendered_folder)
  # flags:
  # -r Recursively compress all files and subdirectories
  # 9: Use maximum compression level
  # X: Exclude extra file attributes
  # q: Run in quiet mode
  # T: Test the archive's integrity after creation
  zip_html <- file.path(
    rendered_folder,
    paste0(yaml$protocol_code, "-", yaml$version, ".zip")
  )
  zip_html_rel <- zip_html |> path_rel(rendered_folder)
  oldwd <- setwd(rendered_folder)
  zip(
    zipfile = zip_html_rel,
    files = not_pdf,
    flags = "-r9XqT"
  )
  setwd(oldwd)
  to_upload <- list(pdf, zip_html)

  # prepare upload to zenodo
  zenodo <- ZenodoManager$new(
    token = token,
    sandbox = sandbox,
    logger = logger
  )

  zenodojson <- jsonlite::read_json(file.path(source_folder, ".zenodo.json"))

  myrec <- zenodo$getDepositionByDOI(yaml$doi)
  myrec$setPublicationDate(Sys.Date())
  myrec$setVersion(yaml$version)
  myrec$setTitle(yaml$title)
  zenodojson$description |>
    myrec$setDescription()
  myrec$setResourceType("publication")

  myrec <- zen_creator(myrec, zenodojson$creator)
  myrec <- zen_contributor(myrec, zenodojson$contributor)
  myrec$setLicense("cc-by-4.0", sandbox = sandbox)
  myrec$setSubjects(yaml$keywords)
  myrec$setPublisher(yaml$publisher)

  myrec <- zenodo$depositRecord(myrec, publish = FALSE)
  for (i in seq_along(to_upload)) {
    zenodo$uploadFile(to_upload[[i]], record = myrec)
  }
  myrec <- zenodo$publishRecord(myrec$id)

  zenodo$submitRecordToCommunities(myrec, yaml$community)
  if (interactive()) {
    browseURL(myrec$links$self_html)
  }
  return(invisible(myrec))
}

#' source code from: inbo/checklist
zen_creator <- function(zen_rec, creators) {
  for (x in creators) {
    zen_rec$addCreator(
      name = x$name, affiliation = x$affiliation, orcid = x$orcid
    )
  }
  return(zen_rec)
}

#' source code from: inbo/checklist
zen_contributor <- function(zen_rec, contributors) {
  for (x in contributors) {
    zen_rec$addContributor(
      firstname = character(0), lastname = x$name, affiliation = x$affiliation,
      orcid = x$orcid, role = x$type
    )
  }
  return(zen_rec)
}
