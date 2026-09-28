# Tests for add_captions(): converts markdown descriptions into numbered figure captions and cross-references.
# How it tests:
# - Reads a test Rmd file containing image links and informal figure descriptions.
# - Runs add_captions() to convert descriptions and custom labels ("foto") into knitr figure captions.
# - Asserts the resulting Rmd file matches the expected reference file (result_captions.Rmd).

test_that("function add_captions works", {
  temp_dir <- tempdir()
  add_captions(
    from = file.path("..", "testdata", "test_captions.Rmd"),
    to = file.path(temp_dir, "step1.Rmd")
  )
  add_captions(
    from = file.path(temp_dir, "step1.Rmd"),
    to = file.path(temp_dir, "final_result.Rmd"),
    name_figure_from = "foto"
  )
  expect_equal(
    readLines(file.path(temp_dir, "final_result.Rmd")),
    readLines(file.path("..", "testdata", "result_captions.Rmd"))
  )
})
