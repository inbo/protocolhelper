library(fs)
library(stringr)

chapters <- fs::dir_ls(
  path = file.path("./inst/rmarkdown/templates"),
  all = FALSE,
  recurse = TRUE,
  type = "file",
  regexp = "\\d{2}_"
)

rmd_md_files <- fs::dir_ls(
  path = file.path("./inst/rmarkdown/templates"),
  all = FALSE,
  recurse = TRUE,
  type = "file",
  regexp = "md$"
)

titles <- purrr::map_chr(chapters, function(x) {
  x <- readLines(x)
  x[[1]]
})

compare <- data.frame(
  path = unname(chapters),
  chapter_file = fs::path_file(chapters),
  titles = unname(titles)
)
View(compare)

library(commonmark)
library(xml2)

headings <- purrr::map_dfr(chapters, function(x) {
  xx <- readLines(x)
  xml <- markdown_xml(xx)
  xml <- read_xml(xml)
  all_headings <- xml_find_all(xml,
                               xpath = ".//d1:heading")
  headings_level <- xml_attr(all_headings, "level")
  headings_text <- xml_text(all_headings)
  tibble::tibble(
    file = x,
    level = headings_level,
    text = headings_text
  )
})

library(dplyr)
library(tidyr)
headings <- headings %>%
  separate(col = file,
           into = c("c0", "c1", "c2", "c3", "template", "c4", "filename"),
           sep = "/") %>%
  select(-starts_with("c")) %>%
  mutate(template = str_remove(template, "template_"),
         filename = str_remove(filename, "\\.Rmd")) %>%
  separate(template, c("template_type", "template_lang"))
