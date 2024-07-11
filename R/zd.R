#' Stocking index
#'
#' @description Calculation of the stocking index of the stand on the basis of
#'  the calculated volume and the tabular volume (see v_tab() function).
#'
#'
#' @param volume_stand calculated stand volume
#' @param vt_stand calculated tabular volume (see v_tab() function)
#'
#' @return Stock index as a ratio of actual stock to tabular volume
#' @export
#'
#' @examples
#' volume_stand = c(250, 341, 80)
#' vt_stand = c(261, 182, 34)
#' zd(volume_stand, vt_stand)


zd = function(volume_stand, vt_stand) {

  zd = volume_stand/vt_stand

  return(zd)
}

