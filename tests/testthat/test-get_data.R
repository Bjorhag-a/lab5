test_that("-1 is returned for empty data", {
  expect_equal(get_data("1", "ÄÄÄÄ", "2019"), -1)
})

test_that("correct output, if data is available", {
  expect_equal(get_data("N09890", "Helsingborg", "2019"),
               data.frame(gender=c("K", "M", "T"),value=c(10.272117, 6.348855, 8.248716)))
})

test_that("wrong input data types raise error", {
  expect_error(get_data("N09890", "Helsingborg", 2019))
  expect_error(get_data(1234, "Helsingborg", "2019"))
  expect_error(get_data("N09890", 1234, "2019"))
})

test_that("Function has correct output format", {
  expect_true(is.data.frame(get_data("N09890", "Helsingborg", "2019")))
})

# No test for large API request as the function only fetches concrete results