#' Check protocol `frontmatter` and `structure`
#'
#' Combines `check_frontmatter()` and `check_structure()` in one function.
#'
#' @inheritParams check_frontmatter
#'
#' @return A report of all failed checks.
#' @export
#' @family check
#'
check_all <- function(protocol_code, fail = !interactive()) {
  cat("Checking protocol YAML block with metadata (frontmatter) ...")
  check_fm <-
    tryCatch(
      check_frontmatter(protocol_code = protocol_code,
                        fail = fail),
      error = function(e) e
    )
  cat("Checking protocol structure...")
  check_str <-
    tryCatch(
      check_structure(protocol_code = protocol_code,
                      fail = fail),
      error = function(e) e
    )
  if (inherits(check_fm, "error") || inherits(check_str, "error")) {
    stop(
      sprintf(
        "Errors in either check_frontmatter or check_structure:\n %s",
        c(check_fm$message, check_str$message)
      )
    )
  }
}
