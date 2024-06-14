library(dplyr)
library(data.table)
library(stringr)


df = fread('D:/funkcja_bruchwald/SL_GAT.csv')
df = df %>% mutate(
  GRP_TH = case_when(
    GRP_H == 'DG' ~ 'JD',
    GRP_H == 'GB' ~ 'BK',
    GRP_H == 'LP' ~ 'BK',
    TRUE ~ GRP_H
  ),
  GRP_TH = ifelse(str_detect(GAT_NAZWA, 'modrzew'), 'MD', GRP_TH)
)%>% mutate(
  GRP_P_new = case_when(
    GRP_P == 'DG' ~ 'JD',
    GRP_P == 'GB' ~ 'BK',
    GRP_P == 'OS' ~ 'BRZ',
    GRP_P == 'JW' ~ 'BK',
    GRP_P == 'AK' ~ 'DB',
    GRP_P == 'WZ' ~ 'BK',
    GRP_P == 'DB.C' ~ 'DB',
    GRP_P == 'JS' ~ 'DB',
    GRP_P == 'LP' ~ 'BK',
    GRP_P == 'TP' ~ 'BRZ',
    TRUE ~ GRP_P
  )
) %>%
  rename(GRP_P_BDL = GRP_P,
        GRP_P  = GRP_P_new)

write.csv2(df, 'inst/sp_dict/sp_groups.csv')

