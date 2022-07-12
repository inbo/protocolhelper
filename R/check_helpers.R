#' @title Check the version number format
#'
#' @description
#' Check if version number is of format YYYY.NN
#'
#' @param version_number Character string with format YYYY.NN#'
#'
#' @importFrom assertthat assert_that is.string
#'
#' @keywords internal
check_versionnumber <- function(version_number) {
  assert_that(is.string(version_number))
  right_format <- grepl("[0-9]{4}\\.[0-9]{2}", version_number) # nolint: nonportable_path_linter, line_length_linter.
  assert_that(
    right_format,
    msg = "version number not in YYYY.XX format"
  )
}



#' @title Check the protocolcode format
#'
#' @description
#' Check if protocolcode is of format `s[f|p|i|o|a]p-###-[nl|en]`
#'
#' @param version_number Character string with format YYYY.NN#'
#'
#' @importFrom assertthat assert_that is.string
#'
#' @keywords internal
check_protocolcode <- function(protocolcode) {
  assert_that(is.string(protocolcode))
  right_format <- grepl("s[fpioa]p-[0-9]{3}-[nl|en]", protocolcode)
  assert_that(
    right_format,
    msg = "protocol code not in s*f-###-nl or s*f-###-en format"
  )
}
