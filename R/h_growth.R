#' Height growth prediction
#'
#' @description Predict tree height in T2, based on the T1, H1 and species
#'
#'
#' @param H1 Height at age T1 (Top or average)
#' @param T1 Age with known height
#' @param T2 Age when we want predict height
#' @param species Species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
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

  params = utils::read.csv2(system.file("params/params_site_index.csv", package = 'growthmodels'))%>%
  dplyr::filter(gatunek == species) # parameters for species
  #params = read.csv2("inst/params/params_site_index.csv") %>%
   # tidyverse::filter(gatunek == species)

  b1 = params$b1
  b2 = params$b2
  b3 = params$b3

  H2 = H1 * (T2^b1 * (T1^b1 * ((H1 - b3) + ((H1 - b3)^2 + (2 *
                                                               b2 * H1)/(T1^b1))^0.5) + b2))/(T1^b1 * (T2^b1 * ((H1 - b3) +
                                                                                                                  ((H1 - b3)^2 + (2 * b2 * H1)/(T1^b1))^0.5) + b2))
  rm(params, b1, b2, b3)

  return(H2)
}
