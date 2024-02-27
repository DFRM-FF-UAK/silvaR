#' Top Height (TH) calculation
#'
#' @description Calculate TH of the tree stand in 10x10 m raster. Using
#' 1/3 highest cell values
#'
#'
#' @param chm canopy height model (resolution equal or lower than 1 m)

#' @return Top Height in the 10x10 m raster
#' @export
#'
#' @examples
#'r =  terra::rast(ncol=100, nrow=100, nlyr=1, res = 0.5)
#'rv = runif(terra::ncell(r), min = 5, max = 36)
#'terra::values(r) = rv
#'terra::plot(r)
#'th = th_calc(r)
#'terra::plot(th)


th_calc = function(chm){


  chm_1 = terra::aggregate(chm, fact=1/terra::xres(chm), fun=mean, na.rm = T)
  rm(chm)
  chm_10 = terra::aggregate(chm_1, fact=10, fun=mean, na.rm = T)

  chm_10 = terra::resample(chm_10, chm_1)
  chm_23 = chm_1

  chm_23[chm_23<(2/3)*chm_10] = NA
  th = terra::aggregate(chm_23, fact=10, fun=mean, na.rm = T)
  rm(chm_1, chm_10, chm_23)

  return(th)

}


