#' Tabular volume prediction
#'
#' @description Predict stand volume based on height,
#' age and species using tabular reference data from
#' Szymkiewicz B. "Tablice zasobnosci i przyrostu drzewostanow".
#' Internally calculates Site Index (height at age 100) via h_growth() and
#' applies species-specific yield table parameters.
#'
#' @references
#' Szymkiewicz T. (1952).
#' *Tablice zasobności i przyrostu drzewostanów*.
#' Warszawa: Państwowe Wydawnictwo Rolnicze i Leśne.
#'
#' @param age tree stand age (years)
#' @param height tree stand top height (m)
#' @param species species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#'
#' @return Numeric vector of tabular volumes (m3/ha)
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
                                  package = "silvaR"))

  df = data.frame(species, age, height) %>%
    dplyr::mutate(species = sp_group(species, 'GRP_V_TAB')) %>%
    dplyr::left_join(params_vt)

  df = df %>%
    dplyr::mutate(si = h_growth(T1 = age, T2 = rep(100, nrow(.)), H1 = height, species = species),
                  vt = (n1 * si - n2) * ((1 - exp(b * age))/(1 - exp(b * 100)))^(c * (n1 * si - n2)^a))

  return(df$vt)
}
