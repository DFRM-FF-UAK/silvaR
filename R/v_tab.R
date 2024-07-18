#' Tabular volume prediction
#'
#' @description Predict stand volume based on height, age and species.
#' Function is using tabular reference data from
#' Szymkiewicz B. "Tablice zasobności i przyrostu drzewostanów"
#'
#' @param age tree stand age
#' @param height tree stand top height
#' @param species species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#'
#' @return Tabular volume for species at input age in cubic meters per hectare
#' @export
#'
#' @examples
#' age = c(100, 101, 102)
#' height = c(32, 33, 34)
#' species = c('SO', 'DB', 'BK')
#' v_tab(age, height, species)


v_tab = function(age, height, species) {

  stopifnot("age must be numeric" = is.numeric(age))
  stopifnot("height must be numeric" = is.numeric(height))
  params_vt = readRDS(system.file("params/params_v_tab.rds",
                                  package = "growthmodels"))

  df = data.frame(species, age, height) %>%
    dplyr::mutate(species = growthmodels::sp_group(species, "GRP_V")) %>%
    dplyr::left_join(params_vt)

  df = df %>%
    dplyr::mutate(si = growthmodels::h_growth(T1 = age, T2 = rep(100, nrow(.)), H1 = height, species = species),
                  vt = (n1 * si - n2) * ((1 - exp(b * age))/(1 - exp(b * 100)))^(c * (n1 * si - n2)^a))

  return(df$vt)
}
