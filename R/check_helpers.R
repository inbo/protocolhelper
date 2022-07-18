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
  right_format <- grepl("^\\d{4}\\.\\d{2}$", version_number)
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


#' @title validate an ORCID string
#'
#' @description Generates check digit as per ISO 7064 11,2.
#' The last character in the ORCID iD is a checksum.
#' In accordance with ISO/IEC 7064:2003, MOD 11-2,
#' this checksum must be "0-9" or "X", a capital letter X which represents
#' the value 10.
#'
#' @param orcid An `orcid` in the `0000-0000-0000-0000` format
#'
#' @return Logical. TRUE if check digit is correct.
#' @export
#'
#' @examples
#' validate_orcid("0000-0002-6378-6229")
validate_orcid <- function(orcid) {
  digits <- strsplit(orcid, "-")[[1]]
  digits <- strsplit(digits, "")
  digits <- unlist(digits)
  check_digit <- digits[length(digits)]
  digits <- digits[seq_along(digits[-1])]
  digits <- as.integer(digits)
  total <- 0
  for (digit in digits) {
    total <- (total + digit) * 2
  }
  remainder <- total %% 11
  result <- (12 - remainder) %% 11
  result <- ifelse(result == 10, "X", as.character(result))
  return(check_digit == result)
}
