# Test data
dbh = c(32, 33, 34)
height = c(27, 28, 29)
species=c('DB', 'JD', 'SO')

test_that("the result structure is correct", {
  result <- v_tree(dbh, height, species)
  expect_true(is.numeric(result))
  expect_equal(length(result), 3)
})

test_that("the result value is correct", {
  expected_values <- c(1.127, 1.233, 1.175)
  result <- round(v_tree(dbh, height, species=c('DB', 'JD', 'SO')), 3)
  expect_equal(result, expected_values)
})

test_that("the expected errors are raised", {
  expect_error(v_tree(dbh=c(32, 33, '34'), height, species), "dbh must be numeric")
  err <- expect_error(v_tree(dbh, height = c(27, 28, '30'), species))
  expect_equal(err$message, "height must be numeric")
})

test_that("the expected messages are raised", {
  expect_warning(v_tree(dbh, height, species=c('DB', 'JD', 'OL SZ')))
})
