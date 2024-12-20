#' Single tree volume calculation
#'
#' @description Calculate single tree volume
#'
#'
#' @param plot_id Unique plot id
#' @param tree_id Unique tree id at the plot
#' @param species Tree species
#' @param age Tree age
#' @param layer Stand structure layer
#' @param dbh Tree dbh
#' @param height Tree heighs
#' @param origin L (lowland) or M (mountain). This parameter needs to be provided if the species is BK
#' @return Tree volume
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
#' origin = c(NA, NA, NA, NA, 'M', NA, NA, NA, NA, NA, NA, NA, NA, NA)
#' v_tree_bruchwald(plot_id, tree_id, species, age, layer, dbh, height, origin)

v_tree_bruchwald = function(plot_id, tree_id, species, age, layer, dbh, height, origin = NA){

    # funkcja do obliczania V (ŚW, SO, JD, BK, BRZ, OL, DG) -------------------------

    V1 = function(species, dbh, height, av_H, av_DBH, origin){

      if (species == 'SO') {
        f1 = 1 / (1 + (dbh / (1.2895 + 0.90645 * dbh))^4)
        s = ((dbh - 6) / (0.2834 + 0.988 * (dbh - 6)))^4
      } else if (species == 'ŚW') {
        if (dbh != 0) {
          f1 = 0.34 + 0.684 / sqrt(dbh)
          s = 1 - 225.73 * ((dbh - 1)^-3.2542)
        }
      } else if (species == 'JD') {
        f1 = 0.4132 + (0.4779 / sqrt(av_H)) + 0.4426 * (av_H^-1.6259) * (av_DBH - dbh)
        s = 1 - 559.4519 * dbh^-3.5946
      } else if (species == 'DB') {
        f1 = 0.5441 * dbh^-0.0415
        s = ((dbh - 3) / (0.9549 + 0.9439 * (dbh - 3)))^4
      } else if (species == 'BK') {
        if (origin == 'M') {
          f1 = 0.5526 * av_DBH^-0.0566 - (0.0001 - 1.6317 / (av_DBH^2)) * (av_DBH - dbh)
          s = 1.2224 - 3.9316 / dbh
        } else if (origin == 'L') {
          f1 = 0.46 * av_DBH^-0.008 + (0.0059 - 0.0001 * av_DBH) * (av_DBH - dbh)
          s = 1.1168 - (48.115 / (dbh^2))
        } else {
          stop("Wrong origin for BK")
        }
      } else if (species == 'BRZ') {
        F_1 = 0.669802 - 0.07496122 * log(dbh)
        f1 = F_1 - ((1.17477 + 0.0008625 * (av_DBH^2))^2 - 1.3) * (1 / av_DBH - 1 / dbh)
        s = 1.03242 - 50.051 / (av_DBH^2.3)
      } else if (species == 'OL') {
        f1 = 0.5755 * av_DBH^-0.0609 - (0.0001 - 0.4561 / (av_DBH^2)) * (av_DBH - dbh)
        s = 1.0207 - 16.613 / (dbh^2)
      } else if (species == 'DG') {
        f1 = ((0.358 + 0.06 * (height / dbh - 0.9)) * (av_H / (av_H - 1.3))^3.8) - 160 / (av_DBH * (av_H - 1.3)^2)
        s = 1
      } else {
        F_1 = 0.669802 - 0.07496122 * log(dbh)
        f1 = F_1 - ((1.17477 + 0.0008625 * (av_DBH^2))^2 - 1.3) * (1 / av_DBH - 1 / dbh)
        s = 1.03242 - 50.051 / (av_DBH^2.3)
      }

      Fq = f1 * s
      Vq = (pi / 40000) * dbh^2 * height * Fq

      return(Vq)
    }

    # funkcja do obliczania V (MD, OS, GB, TP, LP, CZR) -------------------------

    V2 = function(species, dbh, h){

      if (species == 'MD') {
        a = -0.00202663 + 0.000351243 * dbh
        b = 1.10305 + 0.0938913 * log(dbh)
        Vq = a * (height^b)
      } else if (species == 'OS') {
        b = 0.0000529644 * dbh^1.88236
        if (dbh <= 40) {
          a = 0
        } else {
          a = -0.729897 + 0.115514 * sqrt(dbh)
        }
        Vq = a + b * height
      } else if (species == 'GB') {
        if (dbh <= 45) {
          a = 0
          b = (-0.016008 + 0.006824 * dbh)^2
        } else if (dbh <= 51) {
          a = -1.47263 + 0.030648 * dbh
          b = -0.033182 + 0.002765 * dbh
        } else {
          a = -4.10794 + 0.082111 * dbh
          b = 0.068102 + 0.000784 * dbh
        }
        Vq = a + b * height
      } else if (species == 'TP') {
        a = (-0.123182 + 0.00917243 * dbh)^2
        b = 0.0000439076 * dbh^1.8962
        Vq = a + b * height
      } else if (species == 'LP') {
        b = 0.0000545466 * dbh^1.85815
        if (dbh <= 35) {
          a = 0
        } else {
          a = (-0.532622 + 0.0163865 * dbh)^2
        }
        Vq = a + b * height
      } else if (species == 'CZR') {
        Vq = 0.000128 * dbh^2 - 0.00145 * height + 0.000034175 * dbh^2 * height
      } else {
        Vq = NA
      }

      return(Vq)
    }





    df = data.frame(plot_id, tree_id, species, age, layer, dbh, height, origin) %>%
      dplyr::mutate(av_DBH = rms_dbh(plot_id, tree_id, species, age, layer, dbh, height, only_measured_h = T),
                    av_H = lorey_height(plot_id, species, age, layer, height, dbh)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(V = ifelse(species %in% c('ŚW', 'SO', 'JD', 'DB', 'BK', 'BRZ', 'OL', 'DG'),
                        mapply(V1, species, dbh, height, av_H, av_DBH, origin),
                        mapply(V2, species, dbh, height)
      )) %>%
      dplyr::ungroup()


  empty = df[is.na(df$V),]

  if(nrow(empty) > 0) {
    warning(paste("Sorry but we couldn't calculate V for these species:\n",
                  paste(unique(empty$species), collapse = "\n"),
                  '\n Found missing values'))
  }

  return(df$V)

}
