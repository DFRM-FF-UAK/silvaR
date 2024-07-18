test_that("sp_clean zwraca poprawne wyniki", {
  species_list <- c('GLG ', 'Db', 'CZM P', 'Sosna Zwyczajna')
  expected_result <- c('GŁG', 'DB', 'CZM.P', 'SO')
  result <- sp_clean(species_list)
  expect_equal(result, expected_result)
})

test_that("sp_clean radzi sobie z nieznanymi wartościami", {
  species_list <- c('pinia', 'AnotherOne')
  expect_warning(result <- sp_clean(species_list))
  expect_true(all(is.na(result)))
})
