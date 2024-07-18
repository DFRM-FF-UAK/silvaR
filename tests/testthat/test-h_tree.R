# Przykładowe dane
species = c('SO', 'DB', 'OL SZ')
av_H = c(27, 28, 29)
av_dbh_h = c(34, 35, 36)
dbh = c(44, 36, 28)

test_that("h_tree oblicza poprawne wartości wysokości", {
  result <- h_tree(species=c('SO', 'DB', 'ŚW'), av_H, av_dbh_h, dbh)
  expected_result <- c(28.87500,28.21918,26.06762)  # Przykładowe oczekiwane wartości
  expect_equal(result, expected_result, tolerance = 1e-5)
})

test_that("h_tree zgłasza ostrzeżenie dla gatunków bez parametrów", {
  expect_warning(h_tree(c('SO', 'DB', 'UNKNOWN'), av_H, av_dbh_h, dbh))
})
