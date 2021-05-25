test_that("function add_captions works", {
  temp_dir <- tempdir()
  add_captions(
    from = "../testdata/test_captions.Rmd",
    to = file.path(temp_dir, "step1.Rmd")
  )
  add_captions(
    from = paste(temp_dir, "step1.Rmd", sep = "\\"),
    to = file.path(temp_dir, "final_result.Rmd"),
    name_figure = "foto"
  )
  expect_equal(
    readLines(paste(temp_dir, "final_result.Rmd", sep = "\\")),
    readLines("../testdata/result_captions.Rmd")
  )
})
