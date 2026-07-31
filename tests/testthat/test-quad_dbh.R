# Data as in the documentation examples
plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
tree_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)
dbh = c(33, 32, 31, NA, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)

test_that("quad_dbh returns the quadratic mean when only_measured_h is TRUE", {
  result <- quad_dbh(plot_id, tree_id, species, age, layer, dbh, height,
                     only_measured_h = TRUE)
  expected_result <- c(32.50385, 32.50385, 31.00000, NA, 28.00000, NA, 12.00000,
                       NA, NA, 48.00000, 32.23869, 32.23869, 32.23869, 32.23869)
  expect_equal(result, expected_result, tolerance = 1e-5)
})

test_that("quad_dbh matches a hand-computed RMS for a single group", {
  # plot 1, SO, age 40, layer 1: trees with dbh 33 and 32, both with measured height
  result <- quad_dbh(plot_id, tree_id, species, age, layer, dbh, height,
                     only_measured_h = TRUE)
  expect_equal(result[1], sqrt((33^2 + 32^2) / 2))
})

test_that("quad_dbh returns a vector as long as the input", {
  result <- quad_dbh(plot_id, tree_id, species, age, layer, dbh, height,
                     only_measured_h = TRUE)
  expect_length(result, length(plot_id))
})

# Regression: the join with the group table must stay many-to-one. The earlier
# version joined many-to-many and collapsed duplicates with distinct(), which
# shortened the result when input rows were repeated.
test_that("quad_dbh does not drop rows when tree data is repeated", {
  result <- quad_dbh(c(1, 1), c(1, 1), c('SO', 'SO'), c(40, 40), c(1, 1),
                     c(30, 30), c(20, 20), only_measured_h = TRUE)
  expect_length(result, 2)
  expect_equal(result, c(30, 30))
})

test_that("quad_dbh throws an error when height is missing and only_measured_h = TRUE", {
  expect_error(quad_dbh(plot_id, tree_id, species, age, layer, dbh,
                        only_measured_h = TRUE),
               "Please provide height parameter or change only_measured_h to FALSE")
})

test_that("quad_dbh validates the only_measured_h argument", {
  expect_error(quad_dbh(plot_id, tree_id, species, age, layer, dbh, height,
                        only_measured_h = "tak"),
               "must be a single logical value")
  expect_error(quad_dbh(plot_id, tree_id, species, age, layer, dbh, height,
                        only_measured_h = c(TRUE, FALSE)),
               "must be a single logical value")
})

test_that("quad_dbh warns for groups with no dbh measurements", {
  expect_warning(quad_dbh(plot_id, tree_id, species, age, layer, dbh,
                          only_measured_h = FALSE),
                 "Sorry, but we couldn't calculate average dbh for these groups:")
})

test_that("quad_dbh returns the quadratic mean for only_measured_h = FALSE as well", {
  new_dbh <- dbh
  new_dbh[4] <- 14
  result <- quad_dbh(plot_id, tree_id, species, age, layer, new_dbh,
                     only_measured_h = FALSE)
  expected_result <- c(32.50385, 32.50385, 31.00000, 14.00000, 28.00000, 47.00000,
                       12.00000, 32.90137, 32.90137, 48.00000, 32.43070, 32.43070,
                       32.43070, 32.43070)
  expect_equal(result, expected_result, tolerance = 1e-5)
})

# Regression: the FALSE branch used to compute mean(dbh), the same as av_dbh().
# The two functions must now differ wherever a group holds more than one tree
# with differing diameters.
test_that("quad_dbh (RMS) differs from av_dbh (arithmetic mean)", {
  p <- c(1, 1); t <- c(1, 2); s <- c('BK', 'BK')
  a <- c(30, 30); l <- c(1, 1); d <- c(20, 30)

  rms  <- quad_dbh(p, t, s, a, l, d, only_measured_h = FALSE)
  arit <- av_dbh(p, t, s, a, l, d, only_measured_h = FALSE)

  expect_equal(rms[1],  sqrt(mean(c(20, 30)^2)))
  expect_equal(arit[1], mean(c(20, 30)))
  expect_gt(rms[1], arit[1])
})
