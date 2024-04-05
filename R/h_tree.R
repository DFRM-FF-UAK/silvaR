#' Single tree height calculation
#'
#' @description Calculate single tree height using Bruchwald (2000) equation
#'
#'

#' @param species tree species
#' @param av_H average height of the species at the plot in the layer and age class
#' @param av_dbh_h  average dbh of the species at the plot in the layer and age class, with measured heights
#' @param dbh tree dbh
#' @return Tree height (Bruchwald)
#' @export
#'
#' @examples
#' species = c('SO', 'DB', 'OL SZ')
#' av_H = c(27, 28, 29)
#' av_dbh_h = c(34, 35, 36)
#' dbh = c(44, 36, 28)
#' h_tree(species, av_H, av_dbh_h, dbh)

h_tree = function(species, av_H, av_dbh_h, dbh){

  params_h_tree = readr::read_rds(system.file('/params/height_curves.rds', package = 'growthmodels')) %>%
  #params_h_tree = readr::read_rds('inst/params/height_curves.rds')
    dplyr::mutate(r = as.numeric(r),
           o = as.numeric(o)
           )

  df = data.frame(species, av_H, av_dbh_h, dbh) %>%
    dplyr::left_join(params_h_tree) %>%
    dplyr::mutate(b = dplyr::if_else(species == 'BRZ',
                                     0.364043 - 0.0375941 * sqrt(av_H),
                                     o * (av_H^r)
                                     ),
                  a = (av_dbh_h / sqrt(av_H - 1.3)) - b * av_dbh_h,
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


