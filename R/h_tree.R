#' Single tree height calculation
#'
#' @description Calculate single tree height using Bruchwald (2000) equation
#'
#'
#' @param plot_id Unique plot id
#' @param tree_id Unique tree id at the plot
#' @param species Tree species
#' @param age Tree age
#' @param layer Stand structure layer
#' @param dbh Tree dbh
#' @param height Tree heighs
#' @return Tree height (Bruchwald)
#' @export
#'
#' @examples
#' plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
#' tree_id = c(1, 2 ,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
#' species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
#' age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
#' layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
#' height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)
#' dbh = c(33, 32, 31, NA, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)
#' h_tree(plot_id, tree_id, species, age, layer, dbh, height)

h_tree = function(plot_id, tree_id, species, age, layer, dbh, height){

  params_h_tree = readRDS(system.file('/params/height_curves.rds', package = 'growthmodels')) %>%
  #params_h_tree = readr::read_rds('inst/params/height_curves.rds')
    dplyr::mutate(r = as.numeric(r),
           o = as.numeric(o)
           )

  df = data.frame(plot_id, tree_id, species, age, layer, dbh, height) %>%
    dplyr::mutate(av_dbh = rms_dbh(plot_id, tree_id, species, age, layer, dbh, height, only_measured_h = T),
                  av_h = lorey_height(plot_id, species, age, layer, height, dbh)) %>%
    dplyr::mutate(species = dplyr::if_else(species %in% params_h_tree$species, species, growthmodels::sp_group(species, 'GRP_TH'))) %>%
    dplyr::left_join(params_h_tree) %>%
    dplyr::mutate(b = dplyr::if_else(species == 'BRZ',
                                     0.364043 - 0.0375941 * sqrt(av_h),
                                     o * (av_h^r)
                                     ),
                  a = (av_dbh / sqrt(av_h - 1.3)) - b * av_dbh,
                  H = (dbh / (a + b * dbh))^2 + 1.3
                  )

  empty = df[is.na(df$H),]

  if(nrow(empty) > 0) {
    warning(paste("Sorry but we couldn't calculate height for these trees:\n",
                  paste(unique(empty$species), collapse = "\n"),
                  '\n Parameters not found'))
  }

  return(df$H)

}


