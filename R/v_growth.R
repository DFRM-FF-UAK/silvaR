#' Volume prediction
#'
#' @description Predict tree volume growth in specific period of time based on H1, region, species
#'
#'
#' @param years number of years that we want to predict the growth
#' @param age tree stand age
#' @param H1 Height at age T1 (average)
#' @param V Volume at age T1
#' @param species Species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#' @param region Natural region - Abbreviation: 'I, 'II', 'III', 'IV', 'V', 'VI',
#'  'VII', 'VIII', 'GLOB'
#' @return Growth sum in specific timeline
#' @export
#'
#' @examples
#' years = 10
#' age = c(100, 101, 102)
#' H1 = c(32, 33, 34)
#' V = c(150, 160, 170)
#' species = c('SO', 'DB', 'BK')
#' region = c('I', 'II', 'GLOB')
#'
#' v_growth(years, age, H1, V, species, region)




v_growth = function(years, age, H1, V, species, region){


  params_si = readr::read_rds(system.file('params/params_site_index.rds', package = 'growthmodels'))
  params_vt = readr::read_rds(system.file('params/params_v_tab.rds', package = 'growthmodels'))
  params_spg = readr::read_rds(system.file('params/params_spg.rds', package = 'growthmodels'))
  params_growth = readr::read_rds(system.file('params/params_growth.rds', package = 'growthmodels'))

  df = data.frame(age, H1, V, species, region) %>%
    dplyr::left_join(params_si) %>%
    dplyr::left_join(params_vt) %>%
    dplyr::left_join(params_spg)%>%
    dplyr::left_join(params_growth)

  for (y in c(1:years-1)) {

    growth = paste0('growth_', y,'_', y+1)

    df = df %>% dplyr::mutate(
      T1 = age + y,
      T2 = age + y + 1,
      T0 = 100,
      z0 = (H1 - b3),
      r = z0 + (z0 ^ 2 + (2 * b2 * H1) / (T1 ^ b1)) ^ 0.5,
      si = H1 * (T0 ^ b1 * (T1 ^ b1 * r + b2)) / (T1 ^ b1 * (T0 ^ b1 * r + b2)),
      vt = (n1*si -n2)*((1-exp(b*T1))/(1-exp(b*100)))^(c*(n1*si -n2)^a),
      zd = V / vt,
      H2 = H1 * (T2 ^ b1 * (T1 ^ b1 * ((H1 - b3) + ((H1 - b3)^ 2 + (2 * b2 * H1) / (T1 ^ b1)) ^ 0.5) + b2)) / (T1 ^ b1 * (T2 ^ b1 * ((H1 - b3) + ((H1 - b3)^ 2 + (2 * b2 * H1) / (T1 ^ b1)) ^ 0.5) + b2)),
      spg_1 =  ((psi4*si -psi5)*((1-exp(psi1*T1))/(1-exp(psi1*100)))^(psi2*(psi4*si -psi5)^psi3)+psi6*H1^4),
      spg_2 =  ((psi4*si -psi5)*((1-exp(psi1*T2))/(1-exp(psi1*100)))^(psi2*(psi4*si -psi5)^psi3)+psi6*H2^4),
      !!growth := (((spg_2) - (spg_1)) / (T2 - T1)) * ni1 * (zd)^ni2 * si^ni3 * T1^ni4,
      V = V + !!dplyr::sym(growth)
    )

  }

  df = df %>% dplyr::mutate(growth_sum = rowSums(dplyr::select(., starts_with("growth"))))

  return(df$growth_sum)

}



