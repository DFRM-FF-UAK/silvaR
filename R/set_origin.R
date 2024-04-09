#' Set lowland or mountainous origin
#'
#' @description Set lowland or mountainous origin
#' Based on the forest habitat or forest region
#'
#'
#' @param value forest habitat or region
#' @return Species group name
#'
#' @export
#'
#' @examples
#' list = c('1', 'II', 'Karpacka', 'Bór górski wilgotny', '14', 'BMB', 'Bór')
#' set_origin(list)



set_origin = function(value){
  value = data.frame(value) %>%
    dplyr::mutate(value = as.character(value))

  origin_dict = read.csv2(system.file('sp_dict/origin_dict.csv', package = 'growthmodels'))
  #origin_dict = read.csv2('inst/sp_dict/origin_dict.csv')

  value = value %>% dplyr::left_join(origin_dict)


  empty = value[is.na(value$origin),]

  if(nrow(empty) > 0) {
    warning(paste("Sorry but we couldn't find these values in our dictionary:\n",
                  paste(unique(empty$value), collapse = "\n")))
  }

  return(value$origin)
}

