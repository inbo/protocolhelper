test_that("function add_captions works", {
  temp_dir <- tempdir()
  add_captions(
    from = "../testdata/test_captions.Rmd",
    to = file.path(temp_dir, "step1.Rmd")
  )
  add_captions(
    from = file.path(temp_dir, "step1.Rmd"),
    to = file.path(temp_dir, "final_result.Rmd"),
    name_figure_from = "foto"
  )
  expect_equal(
    readLines(file.path(temp_dir, "final_result.Rmd")),
    readLines("../testdata/result_captions.Rmd")
  )
})
