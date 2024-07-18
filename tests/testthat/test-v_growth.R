# Przykładowe dane
stand_id = c(1, 2, 3)
years = 10
age = c(100, 101, 102)
height = c(32, 33, 34)
volume = c(150, 160, 170)
species = c('SO', 'DB', 'BK')
region = c('I', 'II', 'GLOB')

test_that("v_growth oblicza poprawne wartości wzrostu", {
  result <- v_growth(stand_id, years, age, height, volume, species, region, output_type = 'df')
  expect_true(all(c("stand_id", "species_cd", "species", "growth_sum") %in% colnames(result)))
})

test_that("v_growth zwraca wynik jako wektor", {
  result <- v_growth(stand_id, years, age, height, volume, species, region, output_type = 'list')
  expect_true(is.vector(result))
})

test_that("v_growth zwraca poprawny wynik", {
  result <- v_growth(stand_id, years, age, height, volume, species, region, output_type = 'list')
  expect_equal(result, c(20.07202,25.02441,32.74718), tolerance = 1e-5)
})

test_that("v_growth obsługuje brakujące parametry", {
  expect_error(v_growth(stand_id, years, age = c(100, 101), height, volume, species, region, output_type = 'df'))
})
