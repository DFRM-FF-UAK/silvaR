#' Top Height (TH) calculation
#'
#' @description Estimates the top height (TH) of a forest stand from a canopy height
#' model (CHM) raster. The function aggregates CHM pixels to a 10x10 m grid by
#' selecting the mean of the tallest cells within each grid cell, which approximates
#' the height of dominant trees. The aggregation factor is computed automatically
#' based on the resolution of the input raster, so any CHM with a resolution of
#' 2 meters or finer is accepted.
#'
#' @param chm canopy height model (resolution equal or higher than 1 meter).
#' SpatRaster or RasterLayer
#'
#' @return SpatRaster of top height values (m) at 10x10 m resolution
#' @export
#'
#' @examples
#' chm = terra::rast(system.file('raster/chm.tif', package = 'silvaR'))
#' terra::plot(chm)
#' th = th_calc(chm)
#' terra::plot(th)

th_calc = function(chm, f="h_23") {

  # input check
  if (!inherits(chm, c("SpatRaster","RasterLayer"))) stop("Parameter chm is not a valid datatype")

  if (inherits(chm, "RasterLayer")) chm <- terra::rast(chm)

  # default functions list and check of f parameter
  f_list <- list(
    "h_23" = "function(x) mean(na.omit(x)[na.omit(x)>(mean(na.omit(x))*2/3)])",
    "h_sd" = "function(x) mean(na.omit(x)[na.omit(x)>(quantile(na.omit(x),.97)-6.42)])"
  )
  if (all(is.character(f) & !f%in%names(f_list))) stop("Name of the function parameter is not valid")

  # resolution check and resampling to 1 m if needed
  if (terra::xres(chm) < 1) {
    r_ref <- terra::rast(xmin=terra::xmin(chm), ymin=terra::ymin(chm), resolution = 1)
    chm <- terra::resample(chm, r_ref)
  } else if (terra::xres(chm) > 2) {
    stop("Input raster resolution must be 10 meters or finer")
  }

  # derive aggregation factor to reach 10 m output
  fact <- round(10 / terra::xres(chm))

  th <- terra::aggregate(chm,
                         fact=fact,
                         fun=eval(parse(text = paste(f_list[f]))))
  return(th)
}
