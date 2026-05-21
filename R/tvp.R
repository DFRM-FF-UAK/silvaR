#' Total coarse wood production
#'
#' @description Predict total coarse wood production in the stand based on
#' the stand age, height and species. You can calculate the production using
#' global or region-specific parameters (Polish natural forest regions I-VIII).
#'
#' @param age tree stand age (years)
#' @param height tree stand top height (m)
#' @param species species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#' @param region Natural region - Abbreviation: 'I, 'II', 'III', 'IV', 'V', 'VI',
#'  'VII', 'VIII', 'GLOB' (default, global parameters)
#' @return Numeric vector of total coarse wood production (m3/ha)
#' @export
#'
#' @examples
#' age = c(100, 101, 102)
#' height = c(32, 33, 34)
#' species = c('SO', 'DB', 'BK')
#' tvp(age, height, species)


tvp = function(age, height, species, region = 'GLOB') {


  params_spg = readRDS(system.file("params/params_spg.rds",
                                   package = "silvaR"))

  df = data.frame(species, age, height, region) %>%
    dplyr::mutate(species = sp_group(species, "GRP_P")) %>%
    dplyr::left_join(params_spg)

  df = df %>%
    dplyr::mutate(si = th_growth(T1 = age, T2 = rep(100, nrow(.)), H1 = height, species = species),
                  spg = (psi4*si -psi5)*((1-exp(psi1*age))/(1-exp(psi1*100)))^(psi2*(psi4*si -psi5)^psi3)+psi6*height^4)

  return(df$spg)
}

