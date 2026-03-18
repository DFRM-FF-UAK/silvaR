#' Set lowland or mountainous origin
#'
#' @description Classify plot or stand origin as lowland ("L") or mountainous ("M")
#' based on a forest habitat type code, habitat name, or forest region identifier.
#' Accepts numeric codes (e.g. "1", "14"), Roman numeral region codes (e.g. "II", "VIII"),
#' region names (e.g. "Karpacka"), and habitat type abbreviations or names (e.g. "BMB", "Bor gorski wilgotny").
#'
#'
#' @param value forest habitat or region
#' @return Character vector of origin classification: "L" (lowland) or "M" (mountainous).
#' Returns NA for values not found in the dictionary, with a warning.
#'
#' @export
#'
#' @examples
#' list = c('1', 'II', 'Karpacka', 'Bór górski wilgotny', '14', 'BMB', 'Bór')
#' set_origin(list)



set_origin = function(value){
  value = data.frame(value) %>%
    dplyr::mutate(value = as.character(value))

  origin_dict = read.csv2(system.file('sp_dict/origin_dict.csv', package = 'silvaR'))
  #origin_dict = read.csv2('inst/sp_dict/origin_dict.csv')

  value = value %>% dplyr::left_join(origin_dict)


  empty = value[is.na(value$origin),]

  if(nrow(empty) > 0) {
    warning(paste("Sorry but we couldn't find these values in our dictionary:\n",
                  paste(unique(empty$value), collapse = "\n")))
  }

  return(value$origin)
}

