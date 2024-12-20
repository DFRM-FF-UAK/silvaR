#' Calculate RMS dbh at the sample plot
#'
#' @description Calculate RMS dbh of the species at the plot in the layer and age group
#'
#'
#' @param plot_id Unique plot id
#' @param tree_id Unique tree id at the plot
#' @param species Tree species
#' @param age Tree age
#' @param layer Stand structure layer
#' @param dbh Tree dbh
#' @param only_measured_h if TRUE only trees, with measured dbh will be used (logical, defult TRUE)
#' @param height Tree heighs (you have to provide tree heights if only_measured_h = T)
#' @return Average tree dbh
#' @export
#'
#' @examples
#' plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
#' tree_id = c(1, 2 ,3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
#' species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB', 'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
#' age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
#' layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
#' height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)
#' dbh = c(33, 32, 31, NA, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)
#' av_dbh(plot_id, tree_id, species, age, layer, dbh, height)



rms_dbh = function(plot_id, tree_id, species, age, layer, dbh, height, only_measured_h = T){

  if (only_measured_h == T) {

    if (missing(height)) {
      stop("Please provide height parameter or change only_measured_h to FALSE")
    }

    df = data.frame(plot_id, tree_id, species, age, layer, dbh, height)

    df_fil = df %>% dplyr::group_by(plot_id, species, age, layer) %>%
      tidyr::drop_na(height) %>%
      dplyr::mutate(DBH = sqrt(mean(dbh^2))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c("tree_id",  "dbh", "height"))

    df = df %>%
      dplyr::left_join(df_fil) %>%
      dplyr::distinct()

    empty_d = df_fil[is.na(df_fil$DBH), ]
    if(nrow(empty_d) > 0) {
      warning(paste("Sorry, but we couldn't calculate average dbh for these groups:\n",
                    paste(paste0('Plot ', unique(empty_d$plot_id),': ', unique(empty_d$species), ' ', unique(empty_d$layer)), collapse = "\n"),
                    '\n No dbh or height measurements'))
    }


  }else{
    df = data.frame(plot_id, species, age, layer, dbh) %>%
      dplyr::group_by(plot_id, species, age, layer) %>%
      dplyr::mutate(DBH = mean(dbh)) %>%
      dplyr::ungroup()

    empty_d = df[is.na(df$DBH),]

    if(nrow(empty_d) > 0) {
      warning(paste("Sorry, but we couldn't calculate average dbh for these groups:\n",
                    paste(paste0('Plot ', unique(empty_d$plot_id),': ', unique(empty_d$species), ' ', unique(empty_d$layer)), collapse = "\n"),
                    '\n No dbh measurements'))
    }
  }



  return(df$DBH)
}
