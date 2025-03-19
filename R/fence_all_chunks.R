#' Surround chunks with `pandoc` markdown `divs`
#'
#' Adapted from `bookdown::fence_theorems()` but with the following differences:
#' - chunks will be kept, not replaced by `divs`
#' - all chunks will be processed, not only theorem chunks
#' - all chunks will be surrounded by `div` environments
#' - a label will be added to the `div` environment based on the chunk name
#' - if the chunk parameters contain `fig.cap`, the label will be
#'  `#fig:<chunkname>`, else we assume it is a table `#tab:<chunkname>`
#'
#' @details This function is only relevant in case of
#' `bookdown::markdown_document2` output in order to work around a problem with
#' cross-references for tables and figures.
#' It is assumed that the `\@ref()` syntax is used to refer to tables and
#' figures and that chunk names are used as labels to refer to figures and
#' tables.
#' The input file will be overwritten.
#'
#' @param input Path to input `Rmarkdown` file
#' @importFrom xfun read_utf8 write_utf8
#' @importFrom knitr all_patterns
#' @importFrom stringr str_extract
#'
#' @noRd
fence_all_chunks <- function(input) {
  #adapted from bookdown::fence_theorems()
  text <- read_utf8(input)
  md_pattern <- knitr::all_patterns$md
  block_start <- grep(md_pattern$chunk.begin, text)
  params <- gsub(md_pattern$chunk.begin, "\\1", text[block_start])
  reg <- ".*"
  to_convert <- grepl(reg, params)
  params <- params[to_convert]
  block_start <- block_start[to_convert]
  block_end <- grep(md_pattern$chunk.end, text)
  block_end <- vapply(block_start,
                      function(x) block_end[block_end > x][1],
                      integer(1))
  chunk_names <- stringr::str_extract(
    params,
    "(?<=r\\s)[a-zA-Z0-9_]+")
  is_fig <- grepl("fig\\.cap", params)
  labels <- chunk_names
  labels[is_fig] <- paste0("::: {#fig:", labels[is_fig], "}")
  # if not figure chunk, assume its a table (no harm done if something else)
  labels[!is_fig] <- paste0("::: {#tab:", labels[!is_fig], "}")
  for (i in rev(seq_along(labels))) {
    text <- append(text, values = c(":::", ""), after = block_end[i])
    text <- append(text, values = c("", labels[i]), after = block_start[i] - 1)
  }
  xfun::write_utf8(text, input)
}
