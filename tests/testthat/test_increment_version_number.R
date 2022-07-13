test_that("increment version number works", {
  versions1 <- c("2021.01", "2021.02")
  versions2 <- character(0)
  currentyear <- format(Sys.Date(), "%Y")
  expect_equal(
    protocolhelper:::increment_version_number(versions1),
    ifelse(currentyear == "2021",
           "2021.03",
           paste0(currentyear, ".01"))
  )
  expect_equal(
    protocolhelper:::increment_version_number(versions2),
    paste0(currentyear, ".01")
  )
})
