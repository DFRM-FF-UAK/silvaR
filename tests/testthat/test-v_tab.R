# Test data
age = c(100, 101, 102)
height = c(32, 33, 34)
species = c('SO', 'DB', 'BK')

test_that("the result structure is correct", {
  result <- v_tab(age, height, species)
  expect_true(is.numeric(result))
  expect_equal(length(result), 3)
})

test_that("the result value is correct", {
  expected_values <- c(520.87, 623.78, 595)
  result <- round(v_tab(age, height, species), 2)
  expect_equal(result, expected_values)
})

test_that("the expected errors are raised", {
  expect_error(v_tab(age = c(100, "abc", NA), height, species), "age must be numeric")
})
