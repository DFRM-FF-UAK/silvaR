#' Calculate average dbh at the sample plot
#'
#' @description Calculate average dbh of the species at the plot in the layer
#'
#'
#' @param plot_id Unique plot id
#' @param species Tree species
#' @param layer Stand structure layer
#' @param dbh Tree dbh
#' @return Average tree height
#' @export
#'
#' @examples
#' plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
#' species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
#' layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
#' dbh = c(33, 32, 31, NA, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)
#' av_dbh(plot_id, species, layer, dbh)



av_dbh = function(plot_id, species, layer, dbh){

  df = data.frame(plot_id, species, layer, dbh) %>%
    dplyr::group_by(plot_id, species, layer) %>%
    dplyr::mutate(DBH = mean(dbh)) %>%
    dplyr::ungroup()

  empty_d = df[is.na(df$DBH),]

  if(nrow(empty_d) > 0) {
    warning(paste("Sorry, but we couldn't calculate average dbh for these trees:\n",
                  paste(paste0('Plot ', unique(empty_d$plot_id),': ', unique(empty_d$species), ' ', unique(empty_d$layer)), collapse = "\n"),
                  '\n No height measurements'))
  }

  return(df$DBH)
}

