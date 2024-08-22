#' Stocking index
#'
#' @description Calculation of the stocking index of the stand on the basis of
#'  the calculated volume and the tabular volume (see v_tab() function) by age-species group.
#'
#' @param stand_id stand id
#' @param volume volume in the stand by age-species group
#' @param age tree stand age by age-species group
#' @param height tree stand height by age-species group
#' @param species species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#'
#' @return Stock index as a ratio of actual stock to tabular volume
#' @export
#'
#' @examples
#' stand_id = c(1, 2, 3)
#' age = c(100, 101, 102)
#' height = c(32, 33, 34)
#' volume = c(150, 160, 170)
#' species = c('SO', 'DB', 'BK')

zd_share = function(stand_id, volume, age, height, species) {

  df = data.frame(stand_id, species, age, height, volume) %>%
    dplyr::group_by(stand_id) %>%
    dplyr::mutate(volume_stand = sum(volume, na.rm = TRUE)) %>%
    dplyr::mutate(share = volume/volume_stand) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(vt = growthmodels::v_tab(age, height, species),
           vt_sh = vt * share,
           ) %>%
    dplyr::group_by(stand_id) %>%
    dplyr::mutate(vt_stand = sum(vt_sh, na.rm = T)) %>%
    dplyr::ungroup()

  zd = df$volume_stand/df$vt_stand

  return(zd)
}

