test_that("All municipalities are fetched", {
  expect_equal(length(get_municipalities()), 290)
})