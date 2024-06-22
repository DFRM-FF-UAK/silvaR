#' Height growth prediction
#'
#' @description Predict tree height in T2, based on the T1, H1 and species
#'
#'
#' @param H1 Vector of Height at age T1 (Top or average)
#' @param T1  Vector of Age with known height
#' @param T2 Vector of age when we want predict height
#' @param species Vector of Species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB',
#'  'JD', 'SO'
#'
#' @return H2 - Height at age T2
#' @export
#'
#' @examples
#' T1 = 100
#' T2 = 102
#' H1 = 32
#' species = 'SO'
#'
#'h_growth(T1, T2, H1, species)


h_growth = function(T1, T2, H1, species){

  params = readr::read_rds(system.file("params/params_site_index.rds", package = 'growthmodels'))
  #params = read.csv2("inst/params/params_site_index.csv") %>%
   # tidyverse::filter(gatunek == species)

  df = data.frame(T1, T2, H1, species) %>%
    dplyr::left_join(params) %>%
    dplyr::mutate(H2 = H1 * (T2 ^ b1 * (T1 ^ b1 * ((H1 - b3) + ((H1 - b3)^ 2 + (2 * b2 * H1) /
                                                             (T1 ^ b1)) ^ 0.5) + b2)) /
                    (T1 ^ b1 * (T2 ^ b1 * ((H1 - b3) + ((H1 - b3)^ 2 + (2 * b2 * H1) /
                                                          (T1 ^ b1)) ^ 0.5) + b2))
                  )

  return(df$H2)
}
