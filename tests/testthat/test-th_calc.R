library(terra)

# Przykładowy raster CHM
r <- rast(nrows=10, ncols=10, resolution=1)
set.seed(123)
values(r) <- runif(ncell(r), 10, 30)  # Losowe wartości wysokości

test_that("th_calc zwraca poprawny typ danych", {
  result <- th_calc(r, f="h_23", fact=2)
  expect_s4_class(result, "SpatRaster")
})

test_that("th_calc zwraca poprawne wartości dla funkcji h_23", {
  # Ustawienie wartości dla chm dla lepszej przewidywalności
  values(r) <- rep(10:30, length.out = ncell(r))
  result <- th_calc(r, f="h_23", fact=2)
  expected_values <- terra::values(terra::aggregate(r, fact=2, fun=function(x) mean(na.omit(x)[na.omit(x) > (mean(na.omit(x)) * 2/3)])))
  expect_equal(values(result), expected_values)
})

test_that("th_calc zwraca poprawne wartości dla funkcji h_sd", {
  # Ustawienie wartości dla chm dla lepszej przewidywalności
  values(r) <- rep(10:30, length.out = ncell(r))
  result <- th_calc(r, f="h_sd", fact=2)
  expected_values <- terra::values(terra::aggregate(r, fact=2, fun=function(x) mean(na.omit(x)[na.omit(x) > (quantile(na.omit(x), .97) - 6.42)])))
  expect_equal(values(result), expected_values)
})

test_that("th_calc zgłasza błąd dla niepoprawnego typu chm", {
  expect_error(th_calc(list(), f="h_23", fact=2), "Parameter chm is not a valid datatype")
})

test_that("th_calc zgłasza błąd dla niepoprawnej funkcji", {
  expect_error(th_calc(r, f="invalid_function", fact=2), "Name of the function parameter is not valid")
})

test_that("th_calc zgłasza błąd dla zbyt niskiej rozdzielczości rastra", {
  r_low_res <- rast(nrows=10, ncols=10, resolution=2)
  expect_error(th_calc(r_low_res, f="h_23", fact=2))
})

test_that("th_calc agreguje raster, jeśli rozdzielczość jest wyższa niż 1 m", {
  r_high_res <- rast(nrows=10, ncols=10, resolution=0.5)
  values(r_high_res) <- runif(ncell(r_high_res), 10, 30)
  result <- th_calc(r_high_res, f="h_23", fact=2)
  expect_s4_class(result, "SpatRaster")
  expect_true(all(terra::xres(result) >= 1))
})
