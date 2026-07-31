# Tests for th_calc(chm, polygon) - current API.
# The function returns an sf object (a copy of `polygon`) with an added
# `top_height` column; the aggregation factor to the 10 m grid is derived inside.

chm <- terra::rast(system.file("raster/chm.tif", package = "silvaR"))
stands <- sf::st_read(system.file("vector/stands.gpkg", package = "silvaR"), quiet = TRUE)

test_that("th_calc returns an sf object with a top_height column", {
  result <- th_calc(chm, stands)
  expect_s3_class(result, "sf")
  expect_true("top_height" %in% names(result))
  expect_equal(nrow(result), nrow(stands))
  expect_type(result$top_height, "double")
})

test_that("th_calc keeps the columns of the input polygon", {
  result <- th_calc(chm, stands)
  expect_true(all(names(stands) %in% names(result)))
})

test_that("th_calc returns values within a realistic height range", {
  result <- th_calc(chm, stands)
  th <- result$top_height[!is.na(result$top_height)]

  expect_gt(length(th), 0)
  expect_true(all(is.finite(th)))
  expect_true(all(th > 0))
  expect_true(all(th <= max(terra::values(chm), na.rm = TRUE)))
})

test_that("th_calc reprojects the polygon when its CRS differs from the raster", {
  stands_wgs84 <- sf::st_transform(stands, 4326)
  expect_equal(
    th_calc(chm, stands_wgs84)$top_height,
    th_calc(chm, stands)$top_height,
    tolerance = 0.05
  )
})

test_that("th_calc throws an error for an invalid chm type", {
  expect_error(th_calc(list(), stands), "Parameter chm is not a valid datatype")
})

test_that("th_calc throws an error for an invalid polygon type", {
  expect_error(th_calc(chm, list()), "Parameter polygon is not a valid sf object")
})

test_that("th_calc throws an error for a too coarse raster resolution", {
  fact_coarse <- ceiling(11 / terra::xres(chm))
  chm_coarse <- terra::aggregate(chm, fact = fact_coarse, fun = mean, na.rm = TRUE)
  expect_error(th_calc(chm_coarse, stands), "10 meters or finer")
})

# Regression: rasters finer than 1 m used to crash the function while building
# the resampling template (terra::rast() without xmax/ymax -> "invalid extent"
# in a projected CRS). Aggregation now goes straight to the 10 m grid.
test_that("th_calc handles a raster finer than 1 m", {
  chm_sub <- terra::disagg(chm, fact = 2, method = "bilinear")
  expect_equal(terra::xres(chm_sub), 0.5)

  result_sub <- th_calc(chm_sub, stands)
  expect_s3_class(result_sub, "sf")
  expect_equal(nrow(result_sub), nrow(stands))

  # bilinear disaggregation does not change the CHM content, so the result must
  # agree with the one computed on the 1 m raster
  expect_equal(result_sub$top_height, th_calc(chm, stands)$top_height,
               tolerance = 0.01)
})

test_that("th_calc warns when the resolution does not divide 10 m", {
  chm_3m <- terra::aggregate(chm, fact = 3, fun = mean, na.rm = TRUE)
  expect_equal(terra::xres(chm_3m), 3)

  # round(10/3) = 3 -> a 9 m cell, not 10 m
  expect_warning(result <- th_calc(chm_3m, stands), "does not divide 10 m")
  expect_s3_class(result, "sf")
  expect_true("top_height" %in% names(result))
})

test_that("th_calc stays silent for resolutions that divide 10 m", {
  chm_2m <- terra::aggregate(chm, fact = 2, fun = mean, na.rm = TRUE)
  expect_equal(terra::xres(chm_2m), 2)
  expect_no_warning(th_calc(chm_2m, stands))
})

test_that("th_calc warns for non-square pixels", {
  # aggregating with different factors per axis yields rectangular pixels;
  # written without assuming which axis terra scales first
  chm_ns <- terra::aggregate(chm, fact = c(1, 2), fun = mean, na.rm = TRUE)
  expect_true(abs(terra::xres(chm_ns) - terra::yres(chm_ns)) > 0.05)

  expect_warning(th_calc(chm_ns, stands), "not square")
})
