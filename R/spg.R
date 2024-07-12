#' Total coarse wood production
#'
#' @description Predict total coarse wood production in the stand
#' based on the stand age, height and species. You cand calculate the production
#' based on the global and local parameters (forest regions).
#'
#' @param age tree stand age
#' @param height tree stand top height
#' @param species species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#' @param region Natural region - Abbreviation: 'I, 'II', 'III', 'IV', 'V', 'VI',
#'  'VII', 'VIII', 'GLOB' (deafult GLOBAL)
#' @return Total coarse wood production at input age in cubic meters per hectare
#' @export
#'
#' @examples
#' age = c(100, 101, 102)
#' height = c(32, 33, 34)
#' species = c('SO', 'DB', 'BK')
#' spg(age, height, species)


spg = function(age, height, species, region = 'GLOB') {


  params_spg = readRDS(system.file("params/params_spg.rds",
                                   package = "growthmodels"))

  df = data.frame(species, age, height, region) %>%
    dplyr::mutate(species = growthmodels::sp_group(species, "GRP_V")) %>%
    dplyr::left_join(params_spg)

  df = df %>%
    dplyr::mutate(si = growthmodels::h_growth(T1 = age, T2 = rep(100, nrow(.)), H1 = height, species = species),
                  spg = (psi4*si -psi5)*((1-exp(psi1*age))/(1-exp(psi1*100)))^(psi2*(psi4*si -psi5)^psi3)+psi6*height^4)

  return(df$spg)
}

