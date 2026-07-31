test_that("sp_clean returns correct results", {
  species_list <- c('GLG ', 'Db', 'CZM P', 'Sosna Zwyczajna')
  expected_result <- c('GŁG', 'DB', 'CZM.P', 'SO')
  result <- sp_clean(species_list)
  expect_equal(result, expected_result)
})

test_that("sp_clean handles unknown values", {
  species_list <- c('pinia', 'AnotherOne')
  expect_warning(result <- sp_clean(species_list))
  expect_true(all(is.na(result)))
})

test_that("sp_clean resolves typos and common names of the main species", {
  expect_equal(sp_clean('Sesna'), 'SO')
  expect_equal(
    sp_clean(c('sosna', 'świerk', 'jodła', 'dąb', 'buk', 'brzoza', 'olsza', 'modrzew')),
    c('SO', 'ŚW', 'JD', 'DB', 'BK', 'BRZ', 'OL', 'MD')
  )
  expect_equal(sp_clean(c('swierk', 'jodla', 'olcha')), c('ŚW', 'JD', 'OL'))
})
