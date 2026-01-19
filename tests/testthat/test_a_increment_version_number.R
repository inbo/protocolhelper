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
