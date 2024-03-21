library(dplyr)
library(stringi)
library(tidyverse)


dict_wisl = read.csv('D:/slowniki_gat/SL_GAT.csv')
dict_ph = read.csv2('D:/slowniki_gat/sp_dict.csv')
dict_pj = read.csv2('D:/slowniki_gat/gat_dic.csv')

names(dict_wisl)
names(dict_ph)
names(dict_pj)

z = dict_wisl %>% filter(!GAT %in% dict_ph$gatunek)

dict_ph = dict_ph %>% add_row(gatunek = z$GAT)

dict = dict_ph %>%
  left_join(dict_wisl %>%
              rename(gatunek = GAT)
            ) %>%
  left_join(dict_pj %>%
              rename(gatunek = SPECIES_CD)
            ) %>%
  select(-c(X, Nr_gat))




create_typos = function(str){

  str_1 = stri_trans_general(str = str,
                             id = "Latin-ASCII")
  str = c(str, str_1)

  str_2 = gsub("\\.", "", str)

  str = c(str, str_2)

  str_3 = gsub("\\.", " ", str)

  str = c(str, str_3)

  str_4 = c(paste0(str, "."))

  str = c(str, str_4)

  str = unique(str)

  return(paste0(str))
}


dict_fin = dict %>%
  mutate(typos = sapply(gatunek, create_typos)) %>%
  rename(SPECIES_CD = gatunek,
         SPECIES_GROUP = Grupa_gat,
         SPCIES_GROUP_NUM = Nr_grupy_gat,
         AGE_GROUP = grupa_wieku
         ) %>%
  rowwise() %>%
  mutate(typos = paste(typos, collapse=',')) %>%
  ungroup()


write_csv2(dict_fin, 'inst/sp_dict/dict.csv')
