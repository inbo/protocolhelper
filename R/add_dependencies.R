#' Adds dependencies to the YAML of an `index.Rmd` file
#'
#' @param code_mainprotocol Protocol code of the protocol for which dependencies
#' need to be declared in the YAML of its `index.Rmd` file
#' @param protocol_code Character vector of protocol codes that are dependencies
#' to the main protocol.
#' @param version_number Character vector of version numbers corresponding with
#' protocol_code.
#' @param params List of lists with protocol-specific parameters corresponding
#' with parameters from the protocols in protocol_code. Use `NA` if no
#' parameters should be set for a protocol.
#' @param appendix Logical vector indicating whether or not a dependency needs
#' to be included as a subprotocol (at the end of the main protocol in an
#' appendix). Default is `!is.na(params)`. When `params` is not `NA`, `appendix`
#' will always be set to `TRUE` even if the user passes another value.
#'
#' @importFrom ymlthis as_yml use_index_rmd
#' @importFrom rmarkdown yaml_front_matter
#' @importFrom purrr transpose
#'
#' @export
#' @family creation
#'
#' @examples
#' \dontrun{
#' protocolhelper::add_dependencies(
#'   code_mainprotocol = "spp-999-en",
#'   protocol_code = c("sfp-123-en", "spp-124-en"),
#'   version_number = c("2020.01", "2020.02"),
#'   params = list(NA, list(width = 8, height = 8))
#'   )
#'}
add_dependencies <- function(code_mainprotocol,
                             protocol_code,
                             version_number,
                             params,
                             appendix = !is.na(params)) {

  assert_that(is.string(code_mainprotocol))
  right_format <- grepl("s[fpioa]p-[0-9]{3}-[nl|en]", code_mainprotocol)
  assert_that(
    right_format,
    msg = "protocol code not in s*f-###-nl or s*f-###-en format"
  )

  # force appendix to TRUE when corresponding params are given
  appendix[!is.na(params)] <- TRUE

  path_to_protocol <- get_path_to_protocol(code_mainprotocol)
  main <- file.path(path_to_protocol, "index.Rmd")
  index_yml <- rmarkdown::yaml_front_matter(main)
  unlink("css", recursive = TRUE)
  index_yml <- ymlthis::as_yml(index_yml)


  dependencies_list <-
    list(
      protocol_code = protocol_code,
      version_number = version_number,
      params = params,
      appendix = appendix)

  dependencies_list <- transpose(dependencies_list)


  if (is.null(index_yml$params)) {
    index_yml$params <- list(dependencies = list(value = dependencies_list))
  } else {
    index_yml$params <- c(index_yml$params,
                          list(dependencies = list(value = dependencies_list)))
  }

  # overwrite old yaml sections
  template_rmd <- file.path(path_to_protocol, "template.rmd")
  file.copy(from = main, to = template_rmd)
  unlink(main)
  ymlthis::use_index_rmd(
    .yml = index_yml,
    path = path_to_protocol,
    template = template_rmd,
    include_body = TRUE,
    include_yaml = FALSE,
    quiet = TRUE,
    open_doc = FALSE)
  unlink(template_rmd)

  }
