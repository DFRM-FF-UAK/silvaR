#' Clean species
#'
#' @description Clean species names
#'
#'
#' @param species vector of species
#' @return Clean species vector
#' @export
#'
#' @examples
#' species_list = c('GLG ', 'Db', 'CZM P', 'Sosna Zwyczajna', 'Sesna')
#' sp_clean(species_list)


sp_clean = function(species){

  SPECIES_CD = dplyr::tibble(species) %>%
    dplyr::rename(value = species)

  sp_dict = utils::read.csv2(system.file('sp_dict/dict.csv', package = 'growthmodels'), encoding = "UTF-8") %>%
    tidyr::separate_rows(typos, sep = ',') %>%
    tidyr::pivot_longer(cols = c(species_name, latin_name, english_name, typos)) %>%
    dplyr::select(SPECIES_CD, value) %>%
    dplyr::distinct(value, .keep_all = T)

  SPECIES_CD_low = SPECIES_CD %>%
    dplyr::mutate(value_n = tolower(value)) %>%
    dplyr::left_join(sp_dict, by = c('value_n' = 'value')) %>%
    tidyr::drop_na()

  SPECIES_CD_upp = SPECIES_CD %>%
    dplyr::mutate(value_n = toupper(value),
                  value_n = trimws(value_n),
                  value_n = gsub(' ', '.', value_n)
    )%>%
    dplyr::left_join(sp_dict, by = c('value_n' = 'value'))%>%
    tidyr::drop_na()

  fin = rbind(SPECIES_CD_low, SPECIES_CD_upp) %>%
    dplyr::select(-value_n)

  SPECIES_CD = SPECIES_CD %>%
    dplyr::left_join(fin)

  empty = SPECIES_CD[is.na(SPECIES_CD$SPECIES_CD),]

  if (nrow(empty) > 0) warning(paste0("Not found: ", empty$value, " "))

  return(SPECIES_CD$SPECIES_CD)

}



