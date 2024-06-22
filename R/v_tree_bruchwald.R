#' Single tree volume calculation
#'
#' @description Calculate single tree volume
#'
#'
#' @param dbh diameter at the brest height
#' @param height tree height
#' @param av_H average height
#' @param av_DBH average DBH
#' @param species tree species
#' @param origin L (lowland) or M (mountain). This parameter needs to be provided if the species is BK
#' @return Tree volume
#' @export
#'
#' @examples
#' dbh = c(31, 31, 31, NA, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31)
#' height = c(27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27)
#' av_H = c(26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26)
#' av_DBH = c(31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5, 31.5)
#' species = c('SO', 'JD', 'DB', 'BK', 'BRZ', 'OL', 'DG', 'MD', 'OS', 'GB', 'TP', 'LP', 'CZR', 'OL CZ')
#' origin = c(NA, NA, NA, 'M', NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)


v_tree_bruchwald = function(dbh, height, av_H, av_DBH, species, origin = NA){

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





    df = data.frame(dbh, height, av_H, av_DBH, species, origin) %>%
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
