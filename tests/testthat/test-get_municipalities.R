test_that("All municipalities are fetched", {
  expect_equal(length(get_municipalities()), 290)
})



test_that("Function has correct output format", {
  expect_true(is.character(get_municipalities()))
})