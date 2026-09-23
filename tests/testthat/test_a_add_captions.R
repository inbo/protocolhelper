# ==============================================================================
# Unit Tests: add_captions()
#
# What is tested:
#   - Transformation of inline markdown figure and table descriptions into formal
#     Bookdown captions and numbered cross-references.
#   - Handling of custom figure labels (e.g., Dutch label 'foto').
#   - Correct replacement of raw descriptions without corrupting surrounding text.
#
# How it is tested:
#   - Copies a sample markdown fixture (testdata/test_captions.Rmd) to a temporary
#     directory.
#   - Runs add_captions() in multi-step transformations (default labels and custom
#     'foto' label).
#   - Reads the final generated file lines and asserts strict equivalence against
#     a reference fixture (testdata/result_captions.Rmd).
# ==============================================================================

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
