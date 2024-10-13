test_that("all membergs of KPI_group are fetched", {
  expect_equal(nrow(rbind.data.frame(get_kpi_group("GKPI127")$members)), 36)
})

test_that("Error raised for wrong input data type", {
  expect_error(get_kpi_group(123))
})

test_that("Function has correct output format", {
  expect_true(is.data.frame(get_kpi_group("GKPI127")))
})