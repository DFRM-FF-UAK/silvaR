#' Volume prediction
#'
#' @description Predict tree volume growth in specific period of time based on H1,
#'  and species with regard to climatic conditions
#'
#' @param stand_id stand id
#' @param years number of years that we want to predict the growth
#' @param age tree stand age
#' @param height Height at age T1 (average)
#' @param volume Volume at age T1
#' @param species Species - Abbreviation: 'BK', 'OL', 'MD', 'BRZ', 'DB', 'ŚW',
#'  'JD', 'SO'
#' @param rsum precipitation sum #chodzi o sumę opadów z całego roku?
#' @param maxtavg maxium temperature #chodzi o maksymalną średnią temperaturę miesięczną?
#' @param output_type by default results will be returned as a vector, you can type 'df' to return data frame
#' @return Growth sum in specific timeline
#' @export
#'
#' @examples
#' years = 10
#' stand_id = c(1, 2, 3)
#' age = c(100, 101, 102)
#' height = c(32, 33, 34)
#' volume = c(150, 160, 170)
#' species = c('SO', 'DB', 'BK')
#' rsum = c(800, 700, 600)
#' maxtavg = c(23, 17, 21.8)
#' v_growth_climate(stand_id, years, age, height, volume, species, rsum, maxtavg, output_type = 'df')
#' @importFrom rlang :=

v_growth_climate = function (stand_id,
                     years,
                     age,
                     height,
                     volume,
                     species,
                     rsum,
                     maxtavg,
                     output_type = NULL) {

  #params_si = readRDS(system.file("params/params_site_index.rds",
  # package = "growthmodels"))
  #params_vt = readRDS(system.file("params/params_v_tab.rds",
  #  package = "growthmodels"))
  #params_spg = readRDS(system.file("params/params_spg.rds",
  #  package = "growthmodels"))
  params_growth = readRDS(system.file("params/params_growth_clim.rds",
                                      package = "growthmodels"))

  df = data.frame(stand_id, species, age, height, volume) %>%
    dplyr::mutate(species_cd = species, species = growthmodels::sp_group(species))  %>%
    #dplyr::left_join(params_si) %>%
    #dplyr::left_join(params_vt) %>% dplyr::left_join(params_spg) %>%
    dplyr::left_join(params_growth)

  #y = 1

  for (y in c(1:years - 1)) {
    a = paste0("age_", y, "_", y + 1)
    growth = paste0("growth_", y, "_", y + 1)
    v = paste0("v_", y, "_", y + 1)
    v_stand = paste0("v_stand_", y, "_", y + 1)
    sh = paste0("sh_", y, "_", y + 1)
    h_start = paste0("h_start_", y, "_", y + 1)
    h_end = paste0("h_end_", y, "_", y + 1)
    si = paste0("si_", y, "_", y + 1)
    vt = paste0("vt_", y, "_", y + 1)
    vtsh = paste0("vtsh_", y, "_", y + 1)
    vt_stand = paste0("vt_stand_", y, "_", y + 1)
    z = paste0("z_", y, "_", y + 1)
    spg_start = paste0("spg_start_", y, "_", y + 1)
    spg_end = paste0("spg_end_", y, "_", y + 1)
    # Calcultate share of species
    df = df %>%
      dplyr::mutate(`:=` (!!a, age),
                    `:=` (!!v, volume),
                    `:=` (!!h_start, height))
    df = df %>%
      dplyr::select(-age, -volume, -height)
    df = df %>%
      dplyr::mutate(age = !!rlang::sym(a),
                    volume = !!rlang::sym(v),
                    height = !!rlang::sym(h_start))
    df = df %>%
      dplyr::group_by(stand_id) %>%
      dplyr::mutate(`:=` (!!v_stand, sum(volume, na.rm = TRUE))) %>%
      dplyr::mutate(`:=` (!!sh, volume / !!rlang::sym(v_stand))) %>%
      dplyr::mutate(share = !!rlang::sym(sh)) %>%
      #   dplyr::mutate(volume_stand = !!rlang::sym(v_stand)) %>%
      dplyr::ungroup()

    df = df %>%
      dplyr::mutate(T1 = age,
                    T2 = age + 1,
                    H1 = height,
                    T0 = 100,
                    #z0 = (H1 - b3),
                    #r = z0 + (z0^2 + (2 * b2 * H1)/(T1^b1))^0.5,
                    si = growthmodels::h_growth(T1, T0, H1, species)#,
                    #vt = growthmodels::v_tab(T1, H1, species),
                    #vt_sh = vt * share
      ) %>%
      # dplyr::group_by(stand_id) %>%
      #dplyr::mutate(vt_stand = sum(vt_sh, na.rm = T)) %>%
      #dplyr::ungroup() %>%
      dplyr::mutate(zd = growthmodels::zd_share(stand_id, volume, T1, H1, species)) %>%
      dplyr::mutate(H2 = growthmodels::h_growth(T1, T2, H1, species),
                    `:=` (!!spg_start, growthmodels::spg(T1, H1, species, region)),
                    `:=` (!!spg_end, growthmodels::spg(T2, H2, species, region)),
                    `:=`(!!growth, (((!!rlang::sym(spg_end)) - (!!rlang::sym(spg_start)))/(T2 - T1)) *
                           ni1 * zd ^ (ni2 + ni2_opad * rsum + ni2_temp * maxtavg) * si ^ ni3 * age ^ ni4),
                    `:=`(!!growth, !!rlang::sym(growth) * share),
                    `:=` (!!v, volume + !!rlang::sym(growth))
      )


    df = df %>%
      dplyr::mutate(`:=` (!!h_end, H2),
                    `:=` (!!si, si),
                    #`:=` (!!vt, vt),
                    #`:=` (!!vt_stand, vt_stand),
                    #`:=` (!!vtsh, vt_sh),
                    `:=` (!!z, zd),
                    `:=` (!!a, T1),
                    age = T2,
                    height = H2,
                    volume = !!rlang::sym(v))
    df = df %>%
      dplyr::select(-si,
                    #-vt,
                    #-vt_sh,
                    #-vt_stand,
                    -zd,
                    -H2)
  }

  df = df %>%
    dplyr::select(-age) %>%
    dplyr::mutate(growth_sum = rowSums(dplyr::select(., dplyr::contains("growth_"))))

  if (is.null(output_type)) {
    return(df$growth_sum)
  }
  else if (output_type == 'df') {
    return(df)
  }
}
