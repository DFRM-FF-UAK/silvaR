#' Group species
#'
#' @description Group species in eight groups (BK, OL, MD, BRZ, DB, ŚW, JD, SO)
#' necessary for the height and growth prediction
#'
#'
#' @param species vector of species
#' @param  type grouping type (5 options: GRP_P, GRP_TH, GRP_V, GRP_H, GRP_P_BDL)
#'
#' @details
#' - GRP_P - (default) growth group
#' - GRP_TH - top height group
#' - GRP_V - volume group
#' - GRP_H - height group; Forest Data Bank based
#' - GRP_P_BDL - growth group; Forest Data Bank based
#'
#' @return Species group name
#'
#' @export
#'
#' @examples
#' species_list = c('AK', 'DB', 'CZM.P')
#' sp_group(species_list, 'GRP_TH')


sp_group = function(species, type = 'GRP_P'){

  # check type
  stopifnot("Group type is not valid" = type %in% c('GRP_H','GRP_TH','GRP_V','GRP_P','GRP_P_BDL'))

  species = data.frame(species)

  sp_g = data.table::fread(system.file('sp_dict/sp_groups.csv', package = 'growthmodels'), encoding = "UTF-8") %>%
    dplyr::select('GAT', all_of(type))

  # omit empty group value
  sp_g = sp_g[!sp_g[[type]] %in% c(NA, "", " "),]

  # compare input species with dictionary
  if (!all(species$species %in% unique(sp_g$GAT))) {
    stop(paste0("Some species: ",
                paste(species$species[!species$species %in% unique(sp_g$GAT)], collapse = ";"),
                " are not present in the dictionary")
    )
  }

  species = species %>% dplyr::left_join(sp_g, by = c('species' = 'GAT')) %>%
    dplyr::rename('species_group' = type)


  if (type == 'GRP_V') {
    message('Grouped by volume')
  } else if(type == 'GRP_TH') {
    message('Grouped by height')
  } else if (type == 'GRP_P') {
    message('Grouped by growth')
  } else if (type == 'GRP_H') {
    message('Grouped by heigth - Forest Data Bank based')
  } else if (type == 'GRP_P_BDL') {
    message('Grouped by growth - Forest Data Bank based')
  }

  return(species$species_group)
}
