#' @title Load protocol to be checked
#'
#' @description Make a `Protocolcheck object` for the protocol that has to be
#' checked if the argument is a protocol code.
#'
#' @param x Either a `Protocolcheck` object or the variable `protocol_code`.
#'
#' @return A `Protocolcheck` object.
#'
#' @importFrom assertthat assert_that is.string
#' @noRd
#'
load_protocolcheck <- function(x) {
  if (!inherits(x, "Protocolcheck")) {
    assert_that(is.string(x))
    x <- protocolcheck$new(protocol_code = x)
  }
  return(x)
}
