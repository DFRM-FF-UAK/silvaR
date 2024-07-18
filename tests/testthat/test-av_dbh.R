# Przykładowe dane
plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
tree_id = c(1, 2 ,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)
dbh = c(33, 32, 31, NA, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)

test_that("av_dbh oblicza poprawne średnie dbh, gdy only_measured_h jest TRUE", {
  result <- round(av_dbh(plot_id, tree_id, species, age, layer, dbh, height, only_measured_h = TRUE),1)
  expected_result <- c(32.5,32.5,31.0,NA,28.0,NA,12.0,NA,NA,48.0,31.3,31.3,31.3,NA)
  expect_equal(result, expected_result)
})

test_that("av_dbh oblicza poprawne średnie dbh, gdy only_measured_h jest FALSE", {
  new_dbh <- dbh
  new_dbh[4] <- 14
  result <- av_dbh(plot_id, tree_id, species, age, layer, new_dbh, only_measured_h = FALSE)
  expected_result <- c(32.50,32.50,31.00,14.00,28.00,47.00,12.00,37.00,37.00,37.00,31.75,31.75,31.75,31.75)
  expect_equal(result, expected_result)
})

test_that("av_dbh zgłasza błąd, gdy only_measured_h jest TRUE, ale brakuje height", {
  expect_error(av_dbh(plot_id, tree_id, species, age, layer, dbh, only_measured_h = TRUE),
               "Please provide height parameter or change only_measured_h to FALSE")
})

test_that("av_dbh zgłasza ostrzeżenie dla grup bez pomiarów dbh, gdy only_measured_h jest FALSE", {
  expect_warning(result <- av_dbh(plot_id, tree_id, species, age, layer, dbh, only_measured_h = FALSE),
                 "Sorry, but we couldn't calculate average dbh for these groups:")
})
