#' Clean species names
#'
#' @description Standardize tree species names from various input formats
#' (Polish abbreviations, Polish common names, Latin names, English names, and common typos)
#' to official Polish forestry abbreviations (e.g. 'SO', 'DB', 'BK').
#' Uses fuzzy string matching (Levenshtein distance) with 10% tolerance to handle misspellings and formatting inconsistencies.
#'
#' @param species vector of species
#' @return Character vector of standardized Polish forestry species abbreviations.
#' Returns NA for species that could not be matched, with a warning.
#' @export
#'
#' @examples
#' species_list = c('GLG ', 'Db', 'CZM P', 'Sosna Zwyczajna', 'Sesna', 'Abies alba', 'rowan')
#' sp_clean(species_list)


sp_clean = function(species) {

  # sp_dict = utils::read.csv2(system.file('sp_dict/dict.csv', package = 'silvaR'), encoding = "UTF-8") %>%
  #   tidyr::separate_rows(typos, sep = ',') %>%
  #   tidyr::pivot_longer(cols = c(species_name, latin_name, english_name, typos)) %>%
  #   dplyr::select(SPECIES_CD, value) %>%
  #   dplyr::distinct(value, .keep_all = T)

  data("sp_dict")
  sp_dict = sp_dict %>%
    tidyr::separate_rows(typos, sep = ',') %>%
    tidyr::pivot_longer(cols = c(species_name, latin_name, english_name, typos)) %>%
    dplyr::select(SPECIES_CD, value) %>%
    dplyr::distinct(value, .keep_all = T)

  # Bare Polish common names of the main forest-forming species, added so that
  # misspellings of the common name (e.g. 'Sesna' -> 'SO') are resolved instead
  # of returning NA. Full multi-word names in the dictionary keep priority.
  common_names <- data.frame(
    SPECIES_CD = c('SO','\u015aW','JD','DB','BK','BRZ','OL','OL','MD','GB','LP','KL',
                   'JS','JW','WZ','TP','OS','WB','DG','AK','AK','CZM'),
    value      = c('sosna','\u015bwierk','jod\u0142a','d\u0105b','buk','brzoza','olsza','olcha',
                   'modrzew','grab','lipa','klon','jesion','jawor','wi\u0105z','topola',
                   'osika','wierzba','daglezja','robinia','akacja','czeremcha'),
    stringsAsFactors = FALSE
  )
  sp_dict <- dplyr::bind_rows(sp_dict, common_names) %>%
    dplyr::distinct(value, .keep_all = TRUE)

  compare_strings <- function(input_string, dictionary, method = "lv") {
    # Convert all strings to lower case
    input_string <- tolower(input_string)
    dictionary <- tolower(dictionary)

    # Compute distances with the selected method
    distances <- stringdist::stringdist(input_string, dictionary, method = method)

    # Build a data frame with the results
    results <- data.frame(
      word = dictionary,
      distance = distances
    )

    return(results)
  }

  ## Pre-clean the names by stripping whitespace
  species <- gsub(" ", "", species)

  # Match the input list against the dictionary; tolerated character difference - 10%
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
