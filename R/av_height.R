#' Calculate average height at the sample plot
#'
#' @description Calculate average height of the species at the plot in the layer
#'
#'
#' @param plot_id Unique plot id
#' @param species Tree species
#' @param layer Stand structure layer
#' @param height Tree height
#' @return Average tree height
#' @export
#'
#' @examples
#' plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
#' species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
#' layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
#' height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)




av_height = function(plot_id, species, layer, height){

  df = data.frame(plot_id, species, layer, height) %>%
    dplyr::group_by(plot_id, species, layer) %>%
    dplyr::mutate(H = mean(height, na.rm=T)
                ) %>%
    dplyr::ungroup()

  empty_h = df[is.na(df$H),]

  if(nrow(empty_h) > 0) {
    warning(paste("Sorry, but we couldn't calculate average height for these trees:\n",
                  paste(paste0('Plot ', unique(empty_h$plot_id),': ', unique(empty_h$species), ' ', unique(empty_h$layer)), collapse = "\n"),
                  '\n No height measurements'))
  }

  return(df$H)
}


