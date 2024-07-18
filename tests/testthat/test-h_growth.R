# Przykładowe dane
T1 = c(100, 85, 110)
T2 = c(102, 100, 100)
H1 = c(32, 27, 36)
species = c('SO', 'DB', 'ŚW')

test_that("h_growth oblicza poprawne wartości wysokości", {
  result <- h_growth(T1, T2, H1, species)
  expected_result <- c(32.24168,28.84662,34.82564)
  expect_equal(result, expected_result, tolerance = 1e-5)
}
)

test_that("h_growth zgłasza błąd dla niepoprawnych gatunków", {
  expect_warning(h_growth(T1, T2, H1, c('XYZ', 'ABC', 'DEF')))
})

test_that("h_growth poprawnie obsługuje brakujące parametry", {
  expect_error(h_growth(T1, T2=c(102, 100), H1, species))
})
