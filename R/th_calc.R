#' Top Height (TH) calculation
#'
#' @description Calculate TH of the tree stand in 10x10 m raster. Using two optional function
#' to obtain top height.
#'
#' @param chm canopy height model (resolution equal or higher than 1 meter).
#' SpatRaster or RasterLayer
#' @param f function used to calculate top height. Build-in functions: "h_23","h_sd"
#' @param fact positive integer. Aggregation factor expressed as number of cells
#' in each direction (horizontally and vertically). Or two integers (horizontal and vertical aggregation factor).
#'
#' @details
#' - "h_23" - (default) mean of 1/3 highest cells to aggregate
#' - "h_sd" - mean of cells higher than threshold: (percentile of 97 - 6.42)
#'
#' @return Top Height in the 10x10 m (default) raster
#' @export
#'
#' @examples
#' chm = terra::rast(system.file('raster/chm.tif', package = 'growthmodels'))
#' terra::plot(chm)
#' th = th_calc(chm, f = "h_sd")
#' terra::plot(th)

th_calc = function(chm, f="h_23", fact=10) {

  # input check
  if (!inherits(chm, c("SpatRaster","RasterLayer"))) stop("Parameter chm is not a valid datatype")

  if (inherits(chm, "RasterLayer")) chm <- terra::rast(chm)

  # default functions list and check of f parameter
  f_list <- list(
    "h_23" = "function(x) mean(na.omit(x)[na.omit(x)>(mean(na.omit(x))*2/3)])",
    "h_sd" = "function(x) mean(na.omit(x)[na.omit(x)>(quantile(na.omit(x),.97)-6.42)])"
  )
  if (all(is.character(f) & !f%in%names(f_list))) stop("Name of the function parameter is not valid")

  # resolution check
  if (terra::xres(chm) < 1) {
    chm = terra::aggregate(chm, fact=1, fun=mean, na.rm = T)
  }
  else if (terra::xres(chm) > 1) {
    stop("Require raster resolution is [1] meter or higher")
  }

  th <- terra::aggregate(chm,
                         fact=fact,
                         fun=eval(parse(text = paste(f_list[f]))))
  return(th)
}
