library(tools)
library(readr)

f = list.files('inst/params/', full.names = TRUE, pattern = '.csv$', recursive = TRUE)

convert = function(file){
  file_rds = read.csv2(file,  stringsAsFactors = FALSE, encoding = "Latin-1")
  write_rds(file_rds, paste0(file_path_sans_ext(file),
                             '.rds'
                             )
            )
  file.remove(file)
}

lapply(f, convert)


