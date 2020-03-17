test_that("protocol", {
  expect_error(get_protocolnumbers())
  expect_error(get_short_titles())
  expect_error(create_sfp())
})
