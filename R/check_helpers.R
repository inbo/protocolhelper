#' @title Check the version number format
#'
#' @description
#' Check if version number is of format `YYYY.NN`
#'
#' @param version_number Character string.
#'
#' @importFrom assertthat assert_that is.string
#'
#' @noRd
check_versionnumber <- function(version_number) {
  assert_that(is.string(version_number))
  right_format <- grepl("^\\d{4}\\.\\d{2}$", version_number)
  assert_that(
    right_format,
    msg = "version number not in YYYY.XX format"
  )
}



#' @title Check the `protocolcode` format
#'
#' @description
#' Check if `protocolcode` is of format `s[fpioa]p-###-[nl|en]` or is on the
#' list of reserved protocol codes
#'
#' @param version_number Character string with format `YYYY.NN`
#'
#' @importFrom assertthat assert_that is.string is.flag
#'
#' @noRd
check_protocolcode <- function(protocolcode) {
  assert_that(is.string(protocolcode))
  right_format <- grepl("^s[fpioa]p-\\d{3}-(?:nl|en)$", protocolcode)
  is_reserved <- any(protocolcode %in% reserved_codes$protocolcode)

  if (!(is.flag(Sys.getenv("CI")) && isTRUE(Sys.getenv("CI")))) {
    assert_that(
      right_format | is_reserved,
      msg = "protocol code not in s*f-###-nl or s*f-###-en format"
    )
  } else {
    assert_that(
      right_format | is_reserved,
      msg = "branch name and protocol_code are different"
    )
  }
}


#' @title validate an ORCID string
#'
#' @description Generates check digit as per ISO 7064 11,2.
#' The last character in the `ORCID ID` is a checksum.
#' In accordance with ISO/IEC 7064:2003, MOD 11-2,
#' this checksum must be "0-9" or "X", a capital letter X which represents
#' the value 10.
#'
#' @param orcid An `orcid` in the `0000-0000-0000-0000` format
#'
#' @return Logical. TRUE if check digit is correct.
#' @export
#' @family check
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



#' Helper function to check if person information is correct
#'
#' Checks the format of person names and `orcid` ids.
#'
#' @param person_list the yaml front matter part containing person info
#' @param problems_vect character vector of previously encountered problems
#'
#' @return a character vector of previously encountered problems and problems
#' identified for person names and `orcid` ids.
#' @export
#' @family check
check_all_person_info <- function(
    person_list,
    problems_vect
    ) {
  person_given <- map_lgl(person_list, ~is.string(.$name$given))
  person_family <- map_lgl(person_list, ~is.string(.$name$family))
  person_orcid <- map_lgl(person_list, ~is.string(.$orcid))
  problems <-
    c(problems_vect,
      sprintf(
        "Person nr %s had an invalid given name (no string)",
        seq_along(person_given)[!person_given]
      ))
  problems <-
    c(problems_vect,
      sprintf(
        "Person nr %s had an invalid family name (no string)",
        seq_along(person_family)[!person_family]
      ))
  problems <-
    c(problems,
      sprintf(
        "Person nr %s had an invalid orcid (no string)",
        seq_along(person_orcid)[!person_orcid]
      ))

  if (all(person_orcid)) {
    orcids <- map_chr(person_list, "orcid")
    problems <-
      c(problems,
        "Please provide `orcids` in the `0000-0000-0000-0000` format."[
          !all(nchar(orcids) == 19)])
    valid_orcids <- map_lgl(orcids, validate_orcid)
    problems <- c(
      problems,
      sprintf("protocolhelper::validate_orcid() indicates %s is not valid",
              orcids)[!valid_orcids]
    )
  }

  return(problems)
}
