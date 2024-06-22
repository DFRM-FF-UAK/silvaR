#' Single tree volume calculation
#'
#' @description Calculate single tree volume
#'
#'

#' @param dbh diameter at the brest height
#' @param height tree height
#' @param species tree species
#' @return Tree volume
#' @export
#'
#' @examples
#' dbh = c(32, 33, 34)
#' height = c(27, 28, 29)
#' species = c('SO', 'DB', 'OL SZ')
#' v_tree(dbh, height, species)


v_tree = function(dbh, height, species){

  params_v_tree = readr::read_rds(system.file('/params/v_tree.rds', package = 'growthmodels'))%>%
  #params_v_tree = readr::read_rds('inst/params/v_tree.rds')
    dplyr::mutate(beta0 = as.numeric(beta0),
                  beta1 = as.numeric(beta1),
                  beta2 = as.numeric(beta2)
                  )
  params_bark = readRDS(system.file('/params/bark_param.rds', package = 'growthmodels'))%>%
  #params_bark = readr::read_rds('inst/params/bark_param.rds')
    dplyr::mutate(t = as.numeric(t)
                  )

  df = data.frame(dbh, height, species) %>%
    dplyr::left_join(params_v_tree) %>%
    dplyr::left_join(params_bark) %>%
    dplyr::mutate(V = beta0 * dbh ^ beta1 * height ^ beta2)

  empty = df[is.na(df$V),]

  if(nrow(empty) > 0) {
    warning(paste("Sorry but we couldn't calculate parameters for these species:\n",
                  paste(unique(empty$species), collapse = "\n"),
                  '\n Parameters not found'))
  }

  return(df$V)

}

