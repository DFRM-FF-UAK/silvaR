library(dplyr)


df = read.csv2('D:/TaksPlot/Portable/gat_dic.csv') %>%
  distinct(SPECIES_CD, .keep_all = T) %>%
  select(SPECIES_CD, Grupa_gat) %>%
  mutate(Grupa_gat = ifelse(SPECIES_CD == 'MD', 'MD', Grupa_gat),
         Grupa_gat = ifelse(Grupa_gat == 'TP', 'BRZ', Grupa_gat),
         Grupa_gat = ifelse(Grupa_gat == 'JS', 'BK', Grupa_gat)
         ) %>%
  rename(species_group = Grupa_gat)

write.csv2(df, 'inst/sp_dict/sp_groups.csv')
