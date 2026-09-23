# ==============================================================================
# Unit Tests: increment_version_number()
#
# What is tested:
#   - Increment logic for protocol version strings following the 'YYYY.NN' convention.
#   - Rollover behavior across calendar years (resetting sequence number to '01'
#     when existing versions belong to previous years).
#   - Sequential increment within the current calendar year (e.g., 'YYYY.02' -> 'YYYY.03').
#   - Initialization of version string when no prior versions exist (empty input).
#
# How it is tested:
#   - Evaluates increment_version_number() against simulated vector scenarios:
#     previous year versions, current year versions, and empty character vectors.
#   - Compares function output against dynamically calculated expected strings
#     derived from Sys.Date().
# ==============================================================================

test_that("increment version number works", {
  currentyear <- format(Sys.Date(), "%Y")
  previousyear <- as.character(as.numeric(currentyear) - 1)
  versions0 <- paste(previousyear, c("01", "02"), sep = ".")
  versions1 <- paste(currentyear, c("01", "02"), sep = ".")
  versions2 <- character(0)
  expect_equal(
    protocolhelper:::increment_version_number(versions0),
    paste0(currentyear, ".01")
  )
  expect_equal(
    protocolhelper:::increment_version_number(versions1),
    paste0(currentyear, ".03")
  )
  expect_equal(
    protocolhelper:::increment_version_number(versions2),
    paste0(currentyear, ".01")
  )
})
