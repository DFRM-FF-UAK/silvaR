library(dplyr)
library(data.table)


df = fread('D:/funkcja_bruchwald/SL_GAT.csv')
write.csv2(df, 'inst/sp_dict/sp_groups.csv')
