#' Group species
#'
#' @description Group species in eight groups (BK, OL, MD, BRZ, DB, ŚW, JD, SO),
#' necessary for the height and growth prediction
#'
#'
#' @param species vector of species
#' @return Species group name
#' @export
#'
#' @examples
#' species_list = c('GŁG', 'DB', 'CZM.P')
#' sp_group(species_list)



sp_group = function(species){
  species = data.frame(species)
  sp_g = utils::read.csv2(system.file('sp_dict/sp_groups.csv', package = 'growthmodels'))
  #sp_g = utils::read.csv2('inst/sp_dict/sp_groups.csv')
  species = species %>% dplyr::left_join(sp_g, by = c('species' = 'SPECIES_CD'))

  return(species$species_group)
}
