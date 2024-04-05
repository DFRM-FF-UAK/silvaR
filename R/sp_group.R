#' Group species
#'
#' @description Group species in eight groups (BK, OL, MD, BRZ, DB, ŚW, JD, SO),
#' necessary for the height and growth prediction
#'
#'
#' @param species vector of species
#' @param  type Grouping type (3 options): GRP_H (height group), GRP_V (volume group), GRP_P (growth group). Deafult: GRP_V description
#' @return Species group name
#'
#' @export
#'
#' @examples
#' species_list = c('GŁG', 'DB', 'CZM.P')
#' sp_group(species_list)



sp_group = function(species, type = 'GRP_V'){
  species = data.frame(species)
  sp_g = data.table::fread(system.file('sp_dict/sp_groups.csv', package = 'growthmodels')) %>%
  #sp_g = data.table::fread('inst/sp_dict/sp_groups.csv') %>%
    dplyr::select('GAT', type)

  species = species %>% dplyr::left_join(sp_g, by = c('species' = 'GAT')) %>%
    dplyr::rename('species_group' = type)


  if(type == 'GRP_V'){
    message('Grouped by volume')
  }else if(type == 'GRP_H'){
    message('Grouped by height')
  }else{
    message('Grouped by growth')
  }

  return(species$species_group)
}


