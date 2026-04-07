#' Calculate Lorey's height at the sample plot
#'
#' @description Calculate Lorey's height (basal-area-weighted mean height) per species,
#' age and layer group within each plot. Lorey's height gives more weight to larger trees
#' by using cross-sectional area (g = pi * (dbh/2)^2) as weights,
#' making it more representative of stand volume than the arithmetic mean.
#'
#'
#' @param plot_id Unique plot id
#' @param species Tree species
#' @param age Tree age (years)
#' @param layer Stand structure layer
#' @param height Tree height (m)
#' @param dbh Tree DBH (cm)
#' @return Numeric vector of Lorey's height values (m)
#' @export
#'
#' @examples
#' plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
#' species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
#' age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
#' layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
#' height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)
#' dbh = c(33, 32, 31, NA, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)
#' lorey_height(plot_id, species, age, layer, height, dbh)



lorey_height = function(plot_id, species, age, layer, height, dbh){

  df = data.frame(plot_id, species, age, layer, height, dbh) %>%
    dplyr::mutate(g =3.14*(dbh/2)^2) %>%
    dplyr::group_by(plot_id, species, age, layer) %>%
    dplyr::mutate(H = stats::weighted.mean(height, g, na.rm=T)
                ) %>%
    dplyr::ungroup()

  empty_h = df[is.na(df$H),]

  if(nrow(empty_h) > 0) {
    warning(paste("Sorry, but we couldn't calculate average height for these groups:\n",
                  paste(paste0('Plot ', unique(empty_h$plot_id),': ', unique(empty_h$species), ' ', unique(empty_h$layer)), collapse = "\n"),
                  '\n No height measurements'))
  }

  return(df$H)
}

