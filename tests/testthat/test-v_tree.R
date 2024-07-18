# Dane do testów
dbh = c(32, 33, 34)
height = c(27, 28, 29)
species=c('DB', 'JD', 'SO')

test_that("struktura wyniku jest poprawna", {
  result <- v_tree(dbh, height, species)
  expect_true(is.numeric(result))
  expect_equal(length(result), 3)
})

test_that("wartość wyniku jest poprawna", {
  expected_values <- c(1.127, 1.233, 1.175)
  result <- round(v_tree(dbh, height, species=c('DB', 'JD', 'SO')), 3)
  expect_equal(result, expected_values)
})

test_that("występują odpowiednie błędy", {
  expect_error(v_tree(dbh=c(32, 33, '34'), height, species), "dbh must be numeric")
  err <- expect_error(v_tree(dbh, height = c(27, 28, '30'), species))
  expect_equal(err$message, "height must be numeric")
})

test_that("występują odpowiednie komunikaty", {
  expect_warning(v_tree(dbh, height, species=c('DB', 'JD', 'OL SZ')))
})
