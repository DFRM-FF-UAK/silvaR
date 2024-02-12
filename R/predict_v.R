#' Height growth prediction
#'
#' @description Predict tree volume in T2, based on the T1, H1, region, species
#'
#'
#' @param H1 Height at age T1 (average)
#' @param V Volume at age T1
#' @param T1 Age with known height
#' @param T2 Age when we want predict height
#' @param species Species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#' @param region Natural region - Abbreviation: 'I, 'II', 'III', 'IV', 'V', 'VI',
#'  'VII', 'VIII', 'GLOB'
#' @return Volume at age T2
#' @export
#'
#' @examples
#' T2 = c(100, 101, 102)
#' T1 = c(102, 103, 104)
#' H1 = c(32, 33, 34)
#' V = c(150, 160, 170)
#' species = c('SO', 'DB', 'BK')
#' region = c('I', 'II', 'GLOB')
#'
#' predict_v(T1, T2, H1, V, species, region)




predict_v = function(T1, T2, H1, V, species, region){

  params_si = utils::read.csv2(system.file('params/params_site_index.csv', package = 'growthmodels'))
  params_vt = utils::read.csv2(system.file('params/params_v_tab.csv', package = 'growthmodels'))
  params_spg = utils::read.csv2(system.file('params/params_spg.csv', package = 'growthmodels'))
  params_growth = utils::read.csv2(system.file('params/params_growth.csv', package = 'growthmodels'))


  df = data.frame(T1, T2, H1, V, species, region) %>%
    dplyr::left_join(params_si) %>%
    dplyr::left_join(params_vt) %>%
    dplyr::left_join(params_spg)%>%
    dplyr::left_join(params_growth) %>%
    dplyr::mutate(
      T0 = 100,
      SI = H1 *
        (T0 ^ b1 * (T1 ^ b1 * ((H1 - b3) +
                                 ((H1 - b3)^ 2 + (2 * b2 * H1) / (T1 ^ b1)) ^ 0.5) + b2))/
        (T1 ^ b1 * (T0 ^ b1 * ((H1 - b3) + ((H1 - b3)^ 2 + (2 * b2 * H1) /
                                              (T1 ^ b1)) ^ 0.5) + b2)),
      H2 = H1 * (T2^b1 * (T1^b1 * ((H1 - b3) + ((H1 - b3)^2 + (2 *
                                                                 b2 * H1)/(T1^b1))^0.5) + b2))/(T1^b1 * (T2^b1 * ((H1 - b3) +
                                                                                                                    ((H1 - b3)^2 + (2 * b2 * H1)/(T1^b1))^0.5) + b2)),

      vt = (n1 * SI - n2) * ((1 - exp(b * H1)) / (1 - exp(b * 100))) ^ (c * (n1 * SI - n2)^a),
      zd = V / vt,
      spg_1 =  ((psi4 * SI - psi5) * ((1 - exp(psi1 * T1)) / (1 - exp(psi1 * 100))) ^ (psi2 * (psi4 * SI - psi5)^psi3) + psi6 * (H1)^4),
      spg_2 =  ((psi4 * SI - psi5) * ((1 - exp(psi1 * T2)) / (1 - exp(psi1 * 100))) ^ (psi2 * (psi4 * SI - psi5)^psi3) + psi6 * (H2)^4),
      growth = (((spg_2) - (spg_1)) / (T2 - T1)) * ni1 * (zd)^ni2 * SI^ni3 * T1^ni4,
      V_fin = V + growth

    )

  return(df$V_fin)
}
