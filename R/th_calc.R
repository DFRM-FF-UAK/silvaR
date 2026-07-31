#' Top Height (TH) calculation
#'
#' @description Estimates the top height (TH) of a forest stand from a canopy height
#' model (CHM) raster for a given polygon vector file based on the approach proposed
#' in Hawryło et al. (2024). The function first aggregates CHM pixels to a 10x10 m
#' grid; the aggregation factor is computed automatically from the resolution of the
#' input raster. The mean value of the 10 m cells is then calculated, and cells with
#' a height below 2/3 of that mean are removed and treated as empty cells. The top
#' height for each polygon is finally calculated as the mean value of the remaining
#' non-empty 10x10 m cells.
#' @param chm canopy height model (resolution of 10 meters or finer). Ideally the
#' resolution divides 10 m evenly (e.g. 0.25, 0.5, 1, 2, 2.5, 5 m); other
#' resolutions are aggregated to the closest achievable grid and raise a warning,
#' as the resulting cell size then departs from the 10 m grid the method is
#' defined on. \code{SpatRaster} or \code{RasterLayer}.
#' @param polygon polygon vector delineating forest stand boundaries.
#' \code{sf} object with polygon or multipolygon geometry.
#'
#' @return An \code{sf} object equivalent to \code{polygon} with an additional
#' column \code{top_height} containing the estimated top height (m) for each polygon.
#' @export
#'
#' @examples
#' chm = terra::rast(system.file('raster/chm.tif', package = 'silvaR'))
#' stands = sf::st_read(system.file('vector/stands.gpkg', package = 'silvaR'))
#' th = th_calc(chm, stands)
#'
#' @importFrom exactextractr exact_extract
#' @importFrom sf st_transform

th_calc = function(chm, polygon) {

  # input check
  if (!inherits(chm, c("SpatRaster", "RasterLayer"))) stop("Parameter chm is not a valid datatype")
  if (!inherits(polygon, c("sf", "sfc"))) stop("Parameter polygon is not a valid sf object")

  if (inherits(chm, "RasterLayer")) chm <- terra::rast(chm)

  # resolution check
  if (terra::xres(chm) > 10) stop("Input raster resolution must be 10 meters or finer")

  # aggregate() applies a single factor to both axes, so non-square pixels would
  # silently end up on cells that are not square either
  if (abs(terra::xres(chm) - terra::yres(chm)) > 0.05) {
    warning(sprintf(paste("CHM pixels are not square (%s x %s m);",
                          "top height is computed on non-square cells."),
                    signif(terra::xres(chm), 4), signif(terra::yres(chm), 4)))
  }

  # derive aggregation factor to reach the 10 m grid; valid for any input
  # resolution finer than 10 m, including sub-metre rasters
  fact <- round(10 / terra::xres(chm))

  # resolutions that do not divide 10 m land on a different grid. Compare the
  # resulting cell size instead of testing divisibility directly, which would
  # misfire on the floating-point noise typical of raster headers
  cell <- fact * terra::xres(chm)
  if (abs(cell - 10) > 0.05) {
    warning(sprintf(paste("CHM resolution (%s m) does not divide 10 m;",
                          "top height computed on a %s m grid instead of 10 m."),
                    signif(terra::xres(chm), 4), signif(cell, 4)))
  }

  # aggregate CHM to 10 m using mean
  chm_10m <- terra::aggregate(chm, fact = fact, fun = mean, na.rm = TRUE)

  # reproject polygon to CHM CRS if needed
  polygon <- sf::st_transform(polygon, terra::crs(chm_10m))

  # calculate top height per polygon: mean of cells > 2/3 of within-polygon mean
  th_values <- exactextractr::exact_extract(chm_10m, polygon, function(vals, cov) {
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return(NA_real_)
    mean(vals[vals > mean(vals) * 2/3])
  })

  polygon$top_height <- th_values
  return(polygon)
}
