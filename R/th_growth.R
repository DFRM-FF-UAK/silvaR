#' Height growth prediction
#'
#' @description Predict tree height at a target age (T2) based on current age (T1),
#' current height (H1) and species. Can also be used to calculate Site Index (SI)
#' by setting T2 as base age (usually 50 or 100).
#'
#' @param H1 Vector of Height at age T1 - top or average (m)
#' @param T1  Vector of Age with known height (years)
#' @param T2 Vector of age when we want predict height (years)
#' @param species Vector of Species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB',
#'  'JD', 'SO', 'ŚW'
#'
#' @return Numeric vector of predicted heights at age T2 (m)
#' @export
#'
#' @examples
#' T1 = 100
#' T2 = 102
#' H1 = 32
#' species = 'SO'
#'
#' th_growth(T1, T2, H1, species)
#'
#' # Calculation of Site Index (SI)
#' T1 = c(85, 110)
#' T2 = c(100, 100)
#' H1 = c(27, 36)
#' species = c('DB', 'ŚW')
#'
#' si <- th_growth(T1, T2, H1, species)
#' print(si)


th_growth = function(T1, T2, H1, species){

  params = readRDS(system.file("params/params_site_index.rds", package = 'silvaR'))
  #params = read.csv2("inst/params/params_site_index.csv") %>%
   # tidyverse::filter(gatunek == species)

  df = data.frame(T1, T2, H1, species) %>%
    dplyr::mutate(species = sp_group(species, 'GRP_TH')) %>%
    dplyr::left_join(params) %>%
    dplyr::mutate(H2 = H1 * (T2 ^ b1 * (T1 ^ b1 * ((H1 - b3) + ((H1 - b3)^ 2 + (2 * b2 * H1) /
                                                             (T1 ^ b1)) ^ 0.5) + b2)) /
                    (T1 ^ b1 * (T2 ^ b1 * ((H1 - b3) + ((H1 - b3)^ 2 + (2 * b2 * H1) /
                                                          (T1 ^ b1)) ^ 0.5) + b2))
                  )

  return(df$H2)
}
