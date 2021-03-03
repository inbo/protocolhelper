test_that("protocol", {
  expect_equal(get_protocolnumbers(), character(0))
  expect_equal(get_short_titles(), character(0))
  expect_error(create_sfp())
})
