# Example data
plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)

test_that("av_height returns correct mean heights", {
  new_height <- c(21, 13, 24, 10, 12, 18, 18, 22, 21, 31, 32, 24, 25, 40)
  result <- av_height(plot_id, species, age, layer, new_height)
  expected_result <- c(17.00,17.00,24.00,10.00,12.00,18.00,18.00,21.50,21.50,31.00,30.25,30.25,30.25,30.25)
  expect_equal(result, expected_result)
})

test_that("av_height warns for groups with no height measurements", {
  new_height <- height
  new_height[1:2] <- NA  # Missing height values
  expect_warning(result <- av_height(plot_id, species, age, layer, new_height),
                 "Sorry, but we couldn't calculate average height for these groups:")
})
