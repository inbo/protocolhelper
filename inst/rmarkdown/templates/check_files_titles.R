library(fs)
library(stringr)

chapters <- fs::dir_ls(path = file.path("./inst/rmarkdown/templates"),
           all = FALSE,
           recurse = TRUE,
           type = "file",
           regexp = "\\d{2}_")

titles <- map_chr(chapters, function(x) {
  x <- readLines(x)
  x[[1]]
})

compare <- data.frame(
  path = unname(chapters),
  chapter_file = fs::path_file(chapters),
  titles = unname(titles))
View(compare)
