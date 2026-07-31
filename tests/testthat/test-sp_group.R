test_that("sp_group keeps the main species unchanged in height grouping", {
  species_list <- c('SO', 'ŚW', 'DB', 'BK', 'BRZ', 'OL', 'MD', 'JD')
  result <- suppressMessages(sp_group(species_list, 'GRP_TH'))
  expect_equal(result, c('SO', 'ŚW', 'DB', 'BK', 'BRZ', 'OL', 'MD', 'JD'))
})

test_that("sp_group assigns different groups for different grouping types", {
  expect_equal(suppressMessages(sp_group('AK', 'GRP_TH')), 'AK')
  expect_equal(suppressMessages(sp_group('AK', 'GRP_P')),  'DB')
  expect_equal(suppressMessages(sp_group('AK', 'GRP_V')),  'BRZ')

  expect_equal(suppressMessages(sp_group('JW', 'GRP_TH')), 'KL')
  expect_equal(suppressMessages(sp_group('JW', 'GRP_P')),  'BK')
})

test_that("sp_group warns about species outside the dictionary and groups them as others", {
  expect_warning(result <- suppressMessages(sp_group(c('WZ.G', 'SO'), 'GRP_TH')),
                 "are not present in the dictionary")
  expect_equal(result, c('BK', 'SO'))
})

test_that("sp_group respects the others argument", {
  expect_warning(result <- suppressMessages(sp_group(c('WZ.G', 'SO'), 'GRP_TH',
                                                     others = 'DB')))
  expect_equal(result, c('DB', 'SO'))
})

test_that("sp_group rejects an invalid grouping type", {
  expect_error(sp_group('SO', 'NOT_A_TYPE'), "Group type is not valid")
})

test_that("sp_group handles every documented grouping type", {
  species_list <- c('SO', 'DB', 'BK')
  for (ty in c('GRP_P', 'GRP_TH', 'GRP_V', 'GRP_H', 'GRP_P_BDL', 'GRP_V_TAB')) {
    result <- suppressMessages(sp_group(species_list, ty))
    expect_length(result, length(species_list))
    expect_type(result, "character")
    expect_false(any(is.na(result)))
  }
})

test_that("sp_group reports the grouping type used", {
  expect_message(sp_group('SO', 'GRP_TH'), "Grouped by height")
  expect_message(sp_group('SO', 'GRP_V'),  "Grouped by volume")
  expect_message(sp_group('SO', 'GRP_P'),  "Grouped by growth")
})

test_that("sp_group returns a vector as long as the input, NA included", {
  expect_warning(result <- suppressMessages(sp_group(c('SO', NA, 'DB'), 'GRP_TH')))
  expect_length(result, 3)
  expect_equal(result[c(1, 3)], c('SO', 'DB'))
})
