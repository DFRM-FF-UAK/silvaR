library(dplyr)
library(readr)


SI = readRDS('inst/params/params_v_tab.rds')

dict = read.csv2('inst/sp_dict/sp_groups.csv')

dict_fin = dict %>%
  mutate(GRP_TH = case_when(
    startsWith(GAT, 'JS') ~ 'JS',
    GAT == 'OS' ~ 'OS',
    GAT == 'AK' ~ 'AK',
    startsWith(GAT, 'DG') ~ 'DG',
    startsWith(GAT, 'GB') ~ 'GB',
    startsWith(GAT, 'DB.C') ~ 'DB.CZ',
    startsWith(GAT, 'LP') ~ 'LP',
    startsWith(GAT, 'KL.') ~ 'KL',
    TRUE ~ GRP_TH
  )) %>%
  mutate(GRP_V_TAB = case_when(
    startsWith(GAT, 'JS') ~ 'JS',
    startsWith(GAT, 'OS') ~ 'OS',
    TRUE ~ GRP_P
  ))


rm(dict)
write_csv2(dict_fin, 'inst/sp_dict/sp_groups.csv')
