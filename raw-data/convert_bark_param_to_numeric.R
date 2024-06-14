library(readr)

params_bark = readr::read_rds(system.file('/params/bark_param.rds', package = 'growthmodels'))%>%
  #params_bark = readr::read_rds('inst/params/bark_param.rds') %>%
  mutate(t = as.numeric(t)
  )

write_rds(params_bark, 'inst/params/bark_param.rds')
