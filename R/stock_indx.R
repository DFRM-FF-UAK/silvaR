#' Stocking index
#'
#' @description Calculate the stocking index (relative density) of the stand
#' as the ratio of actual stand volume to species-share-weighted tabular volume
#' (see v_tab()). Values above 1.0 indicate overstocking, below 1.0 understocking.
#'
#' @param stand_id stand id
#' @param volume stand volume by age-species group (m3/ha)
#' @param age tree stand age by age-species group (years)
#' @param height tree stand height by age-species group (m)
#' @param species species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#'
#' @return Numeric vector of stocking index values (dimensionless ratio of actual stock to tabular volume)
#' @export
#'
#' @examples
#' stand_id = c(1, 2, 3)
#' age = c(100, 101, 102)
#' height = c(32, 33, 34)
#' volume = c(150, 160, 170)
#' species = c('SO', 'DB', 'BK')

stock_indx = function(stand_id, volume, age, height, species) {

  df = data.frame(stand_id, species, age, height, volume) %>%
    dplyr::group_by(stand_id) %>%
    dplyr::mutate(volume_stand = sum(volume, na.rm = TRUE)) %>%
    dplyr::mutate(share = volume/volume_stand) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vt = v_tab(age, height, species),
           vt_sh = vt * share,
           ) %>%
    dplyr::group_by(stand_id) %>%
    dplyr::mutate(vt_stand = sum(vt_sh, na.rm = T)) %>%
    dplyr::ungroup()

  zd = df$volume_stand/df$vt_stand

  return(zd)
}

