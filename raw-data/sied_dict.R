library(dplyr)
library(tidyverse)
library(data.table)
library(sf)

dict_sied = fread('D:/zdjecia/Milicz_2023/raw-data/BDL_13_18_MILICZ_2023/BDL_13_18_MILICZ_2023/f_site_type_dic.txt') %>%
  mutate(origin = ifelse(site_type_nr == 18 | (site_type_nr >= 55 & site_type_nr <= 96), 'M', 'L')) %>%
  select(-c(date_from, date_to, site_type_grp, site_type_grp_act)) %>%
  mutate(site_type_nr = as.character(site_type_nr)) %>%
  pivot_longer(cols = c(site_type_cd, site_type_name, site_type_nr))

dict_kr = st_read('D:/krainy_PL/Krainy_pol_Polska.shp')%>%
 st_drop_geometry() %>%
  mutate(KR_NAZ = c('Bałtycka', 'Mazursko-Podlaska', 'Wielkopolsko-Pomorska', 'Mazowiecko-Podlaska', 'Śląska', 'Małopolska', 'Sudecka', 'Karpacka'),
         origin = if_else(KR > 6, 'M', 'L'),
         KR = as.character(KR)) %>%
  pivot_longer(cols = c(KR, KR_NAZ, KR_ROM))

dict = dict_sied %>%
  bind_rows(dict_kr)


write.csv2(dict, 'inst/sp_dict/origin_dict.csv')
