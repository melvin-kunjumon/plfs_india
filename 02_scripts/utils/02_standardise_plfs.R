
# dplyr - version ----

standardise_plfs <- function(df, name) {
  
  df <- as_tibble(df)

  # capturing wave and levels ----

  wave <- sub(".*?(\\d{4}(?:_\\d{2})?).*", "\\1", name)
  level <- sub(".*_(hhv1|perv1)$", "\\1", name)

  # fixing column names ----

  ## common column names ----

  # from calendar year 2024

  hhv1_cols <- c("panel", "file_id", "sch", "qtr", "visit", "sec", "st", "dc", "nss_reg", "strm", "sstrm", "ss", "sro",
                 "mfsu", "seg", "sss", "ssu", "smonth", "resp_code", "svc", "rea_sub", "hh_size", "hhtype", "relg", "sg",
                 "hce1", "hce2", "hce3", "hce4", "hce5", "hce_tot", "inf_srl", "sur_date", "sur_time", "nss", "nsc", "mult", "no_qtr")

  perv1_cols <- c("panel", "file_id", "sch", "qtr", "visit", "sec", "st", "dc", "nss_reg", "strm", "sstrm", "ss", "sro",
                  "mfsu", "seg", "sss", "ssu", "srl", "rel", "sex", "age", "marst", "gedu_lvl", "tedu_lvl", "form_edu",
                  "curr_att", "voc", "voc_compl", "voc_fld", "voc_dur", "voc_typ", "voc_fund", "pas", "ind_pas", "ocu_pas",
                  "has_sas", "loc_pas", "etyp_pas", "wrkr_pas", "job_pas", "leave_pas", "ssec_pas", "ecoprd_pas", "sas",
                  "ind_sas", "ocu_sas", "loc_sas", "etyp_sas", "wrkr_sas", "job_sas", "leave_sas", "ssec_sas", "ecoprd_sas",
                  "wrk_365", "dur_pas", "dur_sas", "eff_pas", "dur_unp", "evr_wrk", "rea_nw", "rea", "das17", "ind17", "hr17",
                  "ern17", "das27", "ind27", "hr27", "ern27", "hr7", "ahr7", "das16", "ind16", "hr16", "ern16", "das26", "ind26",
                  "hr26", "ern26", "hr6", "ahr6", "das15", "ind15", "hr15", "ern15", "das25", "ind25", "hr25", "ern25", "hr5",
                  "ahr5", "das14", "ind14", "hr14", "ern14", "das24", "ind24", "hr24", "ern24", "hr4", "ahr4", "das13", "ind13",
                  "hr13", "ern13", "das23", "ind23", "hr23", "ern23", "hr3", "ahr3", "das12", "ind12", "hr12", "ern12", "das22",
                  "ind22", "hr22", "ern22", "hr2", "ahr2", "das11", "ind11", "hr11", "ern11", "das21", "ind21", "hr21", "ern21",
                  "hr1", "ahr1", "acws", "aind_cws", "ocu_cws", "ern_reg", "ern_self", "nss", "nsc", "mult", "no_qtr")

  ## cleaning names ----

  if (level == "hhv1") {

    household_cols <- hhv1_cols

    if (wave %in% c("2022", "2023")) {

      names(df) <- household_cols

    } else if (wave %in% c("2023_24", "2022_23", "2021_22", "2020_21")) {

      household_cols <- household_cols[-1]

      names(df) <- household_cols

    } else if (wave %in% c("2019_20", "2018_19", "2017_18")) {

      household_cols <- household_cols[-c(1, 26:30)]

      names(df) <- household_cols

    }

  }

  if (level == "perv1") {

    person_cols <- perv1_cols

    if (wave %in% c("2022", "2023")) {

      names(df) <- person_cols

    } else if (wave %in% c("2023_24", "2022_23")) {

      person_cols <- person_cols[-1]

      names(df) <- person_cols

    } else if (wave == "2021_22") {

      person_cols <- person_cols[-1]
      person_cols <- append(person_cols, c("pas_spec_based_work", "pas_spec_type"), after = 43)
      person_cols <- append(person_cols, c("sas_spec_based_work", "sas_spec_type"), after = 54)

      names(df) <- person_cols

    } else if (wave == "2020_21") {

      mig_cols <- c("mig_enum_diff_last_residence", "mig_moved_post_mar2020", "mig_enum_prev_usual_residence",
                    "mig_last_residence_location", "mig_last_residence_state", "mig_reason_left_residence", "mig_intend_move_out",
                    "mig_last_pas_status", "mig_last_nic2d", "mig_last_nco3d", "mig_last_education_level", "mig_residence_diff_usual",
                    "mig_residence_prev_usual", "mig_last_residence_location2", "mig_last_residence_state2", "mig_reason_left_residence2",
                    "mig_return_intention", "mig_return_pas_status", "mig_return_nic2d", "mig_return_nco3d")

      person_cols <- person_cols[-1]
      person_cols <- append(person_cols, mig_cols, after = 135)

      names(df) <- person_cols

    } else if (wave %in% c("2019_20", "2018_19", "2017_18")) {

      person_cols <- person_cols[-c(1, 43, 53:61)]

      names(df) <- person_cols

    }

  }

  if (level == "perv1") {

    df <- df %>%
      rename(cws = acws, ind_cws = aind_cws)

  }

  # mutating -----

  ## standardising column types -----

  df <- df %>%
    mutate(across(everything(.), ~ remove_val_labels(.)),
           across(everything(.), ~ remove_var_label(.)),
           across(where(is.character), ~ na_if(., "")),
           across(where(~all(is.na(.) | grepl("^\\d+$", .))), ~ as.numeric(.)),
           wave = wave)

  ## common household and person id construction -----

  if (level == "hhv1") {

    if (wave == "2025") {

      df <- df %>%
        mutate(chk_id = paste0(month, visit, sec, mfsu, sss, ssu))

    } else {

      df <- df %>%
        mutate(chk_id = paste0(qtr, visit, sec, mfsu, seg, sss, ssu))

    }

  }

  if (level == "perv1") {

    if (wave == "2025") {

      df <- df %>%
        mutate(chk_id = paste0(month, visit, sec, mfsu, sss, ssu),
               cpk_id = paste0(month, visit, sec, mfsu, sss, ssu, srl))

    } else {

      df <- df %>%
        mutate(chk_id = paste0(qtr, visit, sec, mfsu, seg, sss, ssu),
               cpk_id = paste0(qtr, visit, sec, mfsu, seg, sss, ssu, srl))

    }

  }

  ## constructing final weights ----

  if (wave != "2025") {

    df <- df %>%
      mutate(mult_final = case_when(nss == nsc ~ mult/100, T ~ mult/200),
             mult_final = mult_final/4)

  } else {

   df <- df %>%
     mutate(mult_final = mult/100)

  }

  # selecting relevant household file columns ----

  if (level == "hhv1") {

    if (wave == "2025") {

      col_select <- c("chk_id", "smonth", "resp_code", "svc", "rea_sub", "hh_size", "hhtype", "relg", "sg", "hce1", "hce2",
                      "hce3", "hce4", "hce5", "hce_tot", "lposs", "llease", "rent", "interest", "pension", "remit", "inc_tot",
                      "inf_srl", "sur_date", "sur_time")

      df <- df %>%
        select(any_of(col_select))

    } else {

      col_select <- c("chk_id", "smonth", "resp_code", "svc", "rea_sub", "hh_size", "hhtype", "relg", "sg", "hce1", "hce2",
                      "hce3", "hce4", "hce5", "hce_tot", "inf_srl", "sur_date", "sur_time")

      df <- df %>%
        select(any_of(col_select))

    }

  }

  return(df)

}
