#' Get protocol type from protocol code
#'
#' The protocol type corresponds to the first 3 letters of the protocol code
#'
#' @param protocol_code Character vector giving the protocol code(s)
#' @param auto_identifier Logical.
#' If `TRUE` returns labels following
#' [Pandoc's auto-identifier](https://pandoc.org/MANUAL.html#extension-auto_identifiers)
#' rules.
#'
#' @return A factor with 5 levels corresponding to "sfp", "sip", "sap", "sop"
#' and "spp". The labels depend on `auto_identifier` setting.
#' @export
#'
get_protocol_type <- function(protocol_code, auto_identifier = FALSE) {
  type <- regmatches(protocol_code,
                     regexpr("^s.p", protocol_code))
  if (!auto_identifier) {
    type <- factor(
      type,
      levels = c("sfp", "sip", "sap", "sop", "spp"),
      labels = c("Standard field protocols (sfp)",
                 "Standard instrument protocols (sip)",
                 "Standard analytical protocols (sap)",
                 "Standard operating procedures (sop)",
                 "Project-specific protocols (spp)")
    )
  } else {
    type <- factor(
      type,
      levels = c("sfp", "sip", "sap", "sop", "spp"),
      labels = c("standard-field-protocols-sfp",
                 "standard-instrument-protocols-sip",
                 "standard-analytical-protocols-sap",
                 "standard-operating-procedures-sop",
                 "project-specific-protocols-spp")
    )
  }
  return(type)
}
