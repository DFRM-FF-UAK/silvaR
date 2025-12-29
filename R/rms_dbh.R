#' Calculate RMS dbh at the sample plot
#'
#' @description Calculate root mean square (RMS) diameter at breast height (DBH)
#'  of the species at the plot in the layer and age group.
#'
#' @param plot_id Integer or factor. Unique plot identifier.
#' @param tree_id Integer or factor. Unique tree identifier within plot.
#' @param species Character. Tree species code.
#' @param age Integer. Tree age.
#' @param layer Integer. Stand structure layer.
#' @param dbh Numeric. Diameter at breast height (cm).
#' @param only_measured_h Logical. If TRUE, only trees with measured height
#'   are used. Default is TRUE.
#' @param height Numeric. Tree height (m). Required if `only_measured_h = TRUE`.
#'
#' @return
#' Numeric vector of RMS DBH values corresponding to input rows.
#'
#' @export
#'
#' @examples
#' plot_id = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2)
#' tree_id = c(1, 2 ,3, 4, 5, 6, 1, 2, 3, 4, 5, 6, 7, 8)
#' species = c('SO', 'SO', 'DB', 'JW', 'BK', 'DB',
#'             'DB', 'ŚW', 'ŚW', 'ŚW', 'SO', 'SO', 'SO', 'SO')
#' age = c(40, 40, 40, 40, 60, 45, 50, 50, 50, 60, 60, 60, 60, 60)
#' layer = c(1, 1, 2, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1)
#' height = c(21, 13, 24, NA, 12, NA, 18, NA, NA, 31, 32, 24, 25, NA)
#' dbh = c(33, 32, 31, NA, 28, 47, 12, 22, 41, 48, 27, 42, 25, 33)
#'
#' rms_dbh(plot_id, tree_id, species, age, layer, dbh, height)

rms_dbh = function(plot_id,
                   tree_id,
                   species,
                   age,
                   layer,
                   dbh,
                   height,
                   only_measured_h = T) {

  if (!is.logical(only_measured_h) || length(only_measured_h) != 1) {
    stop("`only_measured_h` must be a single logical value.")
  }

  if (only_measured_h) {

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

  } else {

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
