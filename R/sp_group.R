#' Group species
#'
#' @description Group species in eight groups (BK, OL, MD, BRZ, DB, ŚW, JD, SO),
#' necessary for the height and growth prediction
#'
#'
#' @param species vector of species
#' @param  type Grouping type (5 options):
#'  GRP_H (height group - Forest Data Bank based),
#'  GRP_TH (top height group),
#'  GRP_V (volume group),
#'  GRP_P (growth group),
#'  GRP_P_BDL (growth group - Forest Data Bank based).
#'  Deafult: GRP_P
#' @return Species group name
#'
#' @export
#'
#' @examples
#' species_list = c('GŁG', 'DB', 'CZM.P')
#' sp_group(species_list, 'GRP_TH')



sp_group = function(species, type = 'GRP_P'){
  species = data.frame(species)
  sp_g = data.table::fread(system.file('sp_dict/sp_groups.csv', package = 'growthmodels')) %>%
  #sp_g = data.table::fread('inst/sp_dict/sp_groups.csv') %>%
    dplyr::select('GAT', type)

  species = species %>% dplyr::left_join(sp_g, by = c('species' = 'GAT')) %>%
    dplyr::rename('species_group' = type)


  if(type == 'GRP_V'){
    message('Grouped by volume')
  }else if(type %in% c('GRP_H', 'GRP_TH')){
    message('Grouped by height')
  }else{
    message('Grouped by growth')
  }

  return(species$species_group)
}


