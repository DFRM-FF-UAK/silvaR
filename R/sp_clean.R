#' Clean species names
#'
#' @description Clean species names and convert to short names used in Poland
#'
#' @param species vector of species
#' @return Cleaned species vector
#' @export
#'
#' @examples
#' species_list = c('GLG ', 'Db', 'CZM P', 'Sosna Zwyczajna', 'Sesna', 'Abies alba', 'rowan')
#' sp_clean(species_list)


sp_clean = function(species) {

  sp_dict = utils::read.csv2(system.file('sp_dict/dict.csv', package = 'growthmodels'), encoding = "UTF-8") %>%
    tidyr::separate_rows(typos, sep = ',') %>%
    tidyr::pivot_longer(cols = c(species_name, latin_name, english_name, typos)) %>%
    dplyr::select(SPECIES_CD, value) %>%
    dplyr::distinct(value, .keep_all = T)

  compare_strings <- function(input_string, dictionary, method = "lv") {
    # Konwersja wszystkich stringów do małych liter
    input_string <- tolower(input_string)
    dictionary <- tolower(dictionary)

    # Obliczanie odległości na podstawie wybranej metody
    distances <- stringdist::stringdist(input_string, dictionary, method = method)

    # Tworzenie data frame z wynikami
    results <- data.frame(
      word = dictionary,
      distance = distances
    )

    return(results)
  }

  # Dopasowanie do słownika dla listy wejściowej. Dopuszczalna różnica w znakach - 10%
  SPECIES_CD <- sapply(species, function(i) {
    dd <- compare_strings(i, sp_dict$value)
    if (min(dd$distance, na.rm = T)<=(ceiling(nchar(i)*0.1))) {
      return(sp_dict$SPECIES_CD[which.min(dd$distance)])
    } else {
      return(NA)
    }
  }, USE.NAMES = T)

  empty = names(SPECIES_CD[is.na(SPECIES_CD)])

  if (length(empty) > 0) warning(paste0("Not found: ", empty, " "))

  return(unname(SPECIES_CD))
}
