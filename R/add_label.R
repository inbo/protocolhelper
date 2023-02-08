#' Helper function to create labelled captions for `pander` tables
#'
#' The function adds a reference label to the caption so that `pander` tables
#' can be cross-referenced in `bookdown` using the `\@ref(tab:label)` syntax.
#' The function should only be used in `pander(x, caption = add_label())`.
#'
#' @param caption The caption text as a string
#' @param tag The tag to use as a prefix. Default is `tab`.
#'
#' @return The caption text prefixed with a reference label.
#' @export
#'
#' @family utility
#'
#' @examples
#' add_label("caption text")
add_label <- function(caption = "", tag = "tab") {
  chunk_label <- knitr::opts_current$get("label")
  pretag <- if (knitr::is_latex_output()) {
    paste0("\\label{", tag, ":",chunk_label , "}")
  } else {
    paste0("(\\#", tag, ":", chunk_label, ")")
  }
  paste0(pretag, caption)
}
