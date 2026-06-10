
# dplyr - version ----

recode_plfs <- function(df) {
  
  # labels ----
  
  ## geographical characteristics ----
  
  sec_labels <- c("Rural" = 1, "Urban" = 2)
  
  state_labels <- c("Jammu & Kashmir" = 1, "Himachal Pradesh" = 2, "Punjab" = 3, 
                    "Chandigarh" = 4, "Uttarakhand" = 5, "Haryana" = 6, "Delhi" = 7,
                    "Rajasthan" = 8, "Uttar Pradesh" = 9, "Bihar" = 10, "Sikkim" = 11,
                    "Arunachal Pradesh" = 12, "Nagaland" = 13, "Manipur" = 14, 
                    "Mizoram" = 15, "Tripura" = 16, "Meghalaya" = 17, "Assam" = 18, 
                    "West Bengal" = 19, "Jharkhand" = 20, "Odisha" = 21, "Chhattisgarh" = 22, 
                    "Madhya Pradesh" = 23, "Gujarat" = 24, "Dadra & Nagar Haveli & Daman & Diu" = 25, 
                    "Maharashtra" = 27, "Andhra Pradesh" = 28, "Karnataka" = 29, 
                    "Goa" = 30, "Lakshadweep" = 31, "Kerala" = 32, "Tamil Nadu" = 33, 
                    "Puduchery" = 34, "Andaman & Nicobar Island" = 35, "Telegana" = 36, "Ladakh" = 37)
  
  state_abv_labels <- c("JK" = 1, "HP" = 2, "PB" = 3, "CH" = 4, "UK" = 5, "HR" = 6, "DL" = 7,
                        "RJ" = 8, "UP" = 9, "BR" = 10, "SK" = 11, "AR" = 12, "NL" = 13, "MN" = 14,
                        "MZ" = 15, "TR" = 16, "ML" = 17, "AS" = 18, "WB" = 19, "JH" = 20, "OD" = 21,
                        "CG" = 22, "MP" = 23, "GJ" = 24, "DNDD" = 25, "MH" = 27, "AP" = 28,
                        "KA" = 29, "GA" = 30, "LD" = 31, "KL" = 32, "TN" = 33, "PY" = 34,
                        "AN" = 35, "TS" = 36, "LA" = 37)
  
  ## individual characteristics ----
  
  sex_labels <- c("Male" = 1, "Female" = 2, "Transgender" = 3)
  
  sg_labels <- c("Scheduled Tribe" = 1, "Scheduled Caste" = 2, "Other Backward Classes" = 3, "Others" = 9)
  
  sg_abv_labels <- c("ST" = 1, "SC" = 2, "OBC" = 3, "GC" = 4)
  
  relg_labels <- c("Hinduism" = 1, "Islam" = 2, "Christianity" = 3, "Sikhism" = 4, "Jainism" = 5,
                   "Buddhism" = 6, "Zoroastrianism" = 7, "Others" = 9)
  
  marst_labels <- c("Never Married" = 1, "Currently Married" = 2, "Widowed" = 3, "Divorced/Separated" = 4)
  
  ## educational characteristics ----
  
  gedu_labels <- c("Not literate" = 1, "Literate without formal schooling: EGS/NFEC/AEC" = 2,
                   "Literate without formal schooling: TLC" = 3, "Literate without formal schooling: Others" = 4,
                   "Below primary" = 5, "Primary" = 6, "Middle" = 7, "Secondary" = 8, "Higher secondary" = 10,
                   "Diploma/certificate course" = 11, "Graduate" = 12, "Postgraduate and above" = 13)
  
  gedu_recoded_labels <- c("Not literate" = 1, "Literate upto primary" = 2, "Middle" = 3,
                           "Secondary & higher secondary" = 4, "Graduate & above" = 5)
  
  voc_labels <- c("Formal Training" = 1, "Informal Training" = 2, "No Training" = 3)
  
  ## labour market characteristics ----
  
  lf_labels <- c("Labour force" = 1, "Not in labour force" = 0)
  
  wf_labels <- c("Workforce" = 1, "Not in workforce" = 0)
  
  emp_labels <- c("Employed" = 1, "Unemployed" = 0)
  
  emptyp_labels <- c("Self-Employed" = 1, "Regular Salaried/Wage Worker" = 2, "Casual Labour" = 3)
  
  emptypd_labels <- c("Own Account Worker" = 1, "Employer" = 2, "Unpaid Helper" = 3, 
                      "Regular Salary Worker" = 4, "Casual Labour - Public Works" = 5, 
                      "Casual Labour - Other Works" = 6)
  
  emptypd_cws_labels <- c("Own Account Worker" = 1, "Employer" = 2, "Unpaid Helper" = 3,
                          "Self-employed but didn't work" = 4,
                          "Regular Salary Worker" = 5, "Casual Labour - Public Works" = 6, 
                          "Casual Labour - Other Works" = 7)
  
  nic2008_section_labels <- c("Agriculture" = 1,  "Mining & Quarrying" = 2, "Manufacturing" = 3,
                              "Electricity, gas, steam and air conditioning supply" = 4,
                              "Water supply; sewerage, waste management and remediation activities" = 5,
                              "Construction" = 6, "Wholesale and retail trade; repair of motor vehicles and motorcycles" = 7,
                              "Transportation and storage" = 8, "Accommodation and Food service activities" = 9,
                              "Information and communication" = 10, "Financial and insurance activities" = 11,
                              "Real estate activities" = 12, "Professional, scientific and technical activities" = 13,
                              "Administrative and support service activities" = 14,
                              "Public administration and defence; compulsory social security" = 15,
                              "Education" = 16, "Human health and social work activities" = 17,
                              "Arts, entertainment and recreation" = 18, "Other service activities" = 19,
                              "Activities of households as employers; undifferentiated goods- and services producing activities of households for own use" = 20,
                              "Activities of extraterritorial organizations and bodies" = 21)
  
  nco2015_division_labels <- c("Managers" = 1, "Professionals" = 2, "Technicians & Associate Professionals" = 3,
                              "Clerks & Clerical Support Workers" = 4, "Service & Sales Workers" = 5,
                              "Skilled Agricultural Workers" = 6, "Craft & Related Trades Workers" = 7,
                              "Plant & Machine Operators" = 8, "Elementary Workers" = 9)
  
  etyp_labels <- c("Proprietary: Male" = 1, "Proprietary: Female" = 2, 
                   "Partnership: with members from same household" = 3,
                   "Partnership: with members from different household" = 4,
                   "Government/Local Body" = 5, "Public Sector Enterprises" = 6,
                   "Autonomous Bodies" = 7, "Public/Private Limited Companies" = 8,
                   "Co-operative Societies" = 10, "Trust/Other Non-profit Institutions" = 11,
                   "Employer's Household" = 12, "Others" = 19)
  
  wrkr_labels <- c("Less than 6" = 1, "6 and above but less than 10" = 2,
                   "10 and above but less than 20" = 3, "20 and above" = 4,
                   "Not Known" = 9)
  
  sectyp_labels = c("Organised Sector" = 1, "Unorganised Sector" = 2)
  
  form_emp_labels = c("Formal Employment" = 1, "Informal Employment" = 2)
  
  # helper functions ----
  
  nic_map <- function(x) {
    case_when(x %in% 1:3 ~ 1, x %in% 5:9 ~ 2, x %in% 10:33 ~ 3, x == 35 ~ 4, 
              x %in% 36:39 ~ 5, x %in% 41:43 ~ 6, x %in% 45:47 ~ 7, x %in% 49:53 ~ 8, 
              x %in% 55:56 ~ 9, x %in% 58:63 ~ 10, x %in% 64:66 ~ 11, x == 68 ~ 12, 
              x %in% 69:75 ~ 13, x %in% 77:82 ~ 14, x == 84 ~ 15, x == 85 ~ 16, 
              x %in% 86:88 ~ 17, x %in% 90:93 ~ 18, x %in% 94:96 ~ 19, x %in% 97:98 ~ 20, 
              x == 99 ~ 21, T ~ NA)
  }
  
  
  ndays_das <- function(das17, das27, das16, das26, das15, das25, das14, 
                        das24, das13, das23, das12, das22, das11, das21,
                        activity_codes) {
    
    cbind(das17 %in% activity_codes | das27 %in% activity_codes,
          das16 %in% activity_codes | das26 %in% activity_codes,
          das15 %in% activity_codes | das25 %in% activity_codes,
          das14 %in% activity_codes | das24 %in% activity_codes,
          das13 %in% activity_codes | das23 %in% activity_codes,
          das12 %in% activity_codes | das22 %in% activity_codes,
          das11 %in% activity_codes | das21 %in% activity_codes)
    
  }
  
  ndays_cl <- function(das17, das27, hr17, hr27, das16, das26, hr16, hr26,
                       das15, das25, hr15, hr25, das14, das24, hr14, hr24,
                       das13, das23, hr13, hr23, das12, das22, hr12, hr22,
                       das11, das21, hr11, hr21, activity_codes) {
    
    calc_days <- function(das, hr) {
      
      case_when(das %in% activity_codes & hr > 4 ~ 1,
                das %in% activity_codes & hr > 0 & hr <= 4 ~ 0.5,
                T ~ 0)
      
    }
    
    cbind(pmax(calc_days(das17, hr17), calc_days(das27, hr27)),
          pmax(calc_days(das16, hr16), calc_days(das26, hr26)),
          pmax(calc_days(das15, hr15), calc_days(das25, hr25)),
          pmax(calc_days(das14, hr14), calc_days(das24, hr24)),
          pmax(calc_days(das13, hr13), calc_days(das23, hr23)),
          pmax(calc_days(das12, hr12), calc_days(das22, hr22)),
          pmax(calc_days(das11, hr11), calc_days(das21, hr21)))
    
  }
  
  earn_das = function(das17, das27, ern17, ern27, das16, das26, ern16, ern26,
                      das15, das25, ern15, ern25, das14, das24, ern14, ern24,
                      das13, das23, ern13, ern23, das12, das22, ern12, ern22,
                      das11, das21, ern11, ern21, activity_codes) {
    
    calc_wage <- function(das, ern) {
      
      case_when(das %in% activity_codes ~ ern,
                T ~ 0)
      
    }
    
    cbind(calc_wage(das17, ern17), calc_wage(das27, ern27),
          calc_wage(das16, ern16), calc_wage(das26, ern26),
          calc_wage(das15, ern15), calc_wage(das25, ern25),
          calc_wage(das14, ern14), calc_wage(das24, ern24),
          calc_wage(das13, ern13), calc_wage(das23, ern23),
          calc_wage(das12, ern12), calc_wage(das22, ern22),
          calc_wage(das11, ern11), calc_wage(das21, ern21))
    
  }
  
  # columns to skip
  
  skip_vars <- c("wave", "chk_id", "cpk_id", "file_id", "qtr", "visit", "panel", "bstrm", "sro") # these columns are characters
  
  # mutating ----
  
  df %>% 
    mutate(
      
      ## tidying ----
      
      # coercing variables to numeric
      across(where(~ is.character(.) | is.labelled(.)) & !any_of(skip_vars), ~ as.numeric(as.character(.))),
      
      ## geographical characteristics ----
      
      # sector
      sec = labelled(sec, labels = sec_labels),
      
      # state
      st_recoded = case_when(st == 26 ~ 25, T ~ st),
      st_recoded = labelled(st_recoded, labels = state_labels),
      st_abv_recoded = labelled(st_recoded, labels = state_abv_labels),
      
      ## individual characteristics ----
      
      # sex
      sex = labelled(sex, labels = sex_labels),
      
      # social group
      sg = labelled(sg, labels = sg_labels),
      sg_abv = case_when(sg == 1 ~ 1, sg == 2 ~ 2, sg == 3 ~ 3, sg == 9 ~ 4),
      sg_abv = labelled(sg_abv, labels = sg_abv_labels),
      
      # religion
      relg = labelled(relg, labels = relg_labels),
      
      # martial status
      marst = labelled(marst, labels = marst_labels),
      
      ## household characteristics ----
      
      # monthly per capita expenditure
      mpce = hce_tot/hh_size,
      
      ## educational characteristics ----
      
      # general education
      gedu_lvl = labelled(gedu_lvl, labels = gedu_labels),
      
      gedu_recoded = case_when(gedu_lvl == 1 ~ 1, gedu_lvl %in% c(2, 3, 4, 5, 6) ~ 2,
                               gedu_lvl == 7 ~ 3, gedu_lvl %in% c(8, 10) ~ 4,
                               gedu_lvl %in% c(11, 12, 13) ~ 5, T ~ 1),
      gedu_recoded = labelled(gedu_recoded, labels = gedu_recoded_labels),
      
      # vocational training
      voc_lvl = case_when(voc == 1 ~ 1, voc %in% c(2:5) ~ 2, voc == 6 ~ 3, T ~ NA),
      voc_lvl = labelled(voc_lvl, labels = voc_labels),
      
      ## labour market characteristics ----
      
      # usual status - activity
      idx_pas = case_when(pas %in% c(11:51) ~ 1, T ~ 0),
      idx_sas = case_when(!pas %in% c(11:51) & has_sas == 1 ~ 1, T ~ 0),
      uas = case_when(idx_pas == 1 ~ pas, idx_sas == 1 ~ sas, T ~ pas),
      
      # labour force participation
      lf_uas = case_when(uas %in% c(11:81) ~ 1, T ~ 0),
      lf_uas = labelled(lf_uas, labels = lf_labels),
      
      lf_pas = case_when(pas %in% c(11:81) ~ 1, T ~ 0),
      lf_pas = labelled(lf_pas, labels = lf_labels),
      
      lf_cws = case_when(cws %in% c(11:82) ~ 1, T ~ 0),
      lf_cws = labelled(lf_cws, labels = lf_labels),
      
      # workforce participation
      wf_uas = case_when(uas %in% c(11:51) ~ 1, T ~ 0),
      wf_uas = labelled(wf_uas, labels = wf_labels),
      
      wf_pas = case_when(pas %in% c(11:51) ~ 1, T ~ 0),
      wf_pas = labelled(wf_pas, labels = wf_labels),
      
      wf_cws = case_when(cws %in% c(11:72) ~ 1, T ~ 0),
      wf_cws = labelled(wf_cws, labels = wf_labels),
      
      # employment status
      emp_uas = case_when(uas %in% c(11:51) ~ 1, uas == 81 ~ 0, T ~ NA),
      emp_uas = labelled(emp_uas, labels = emp_labels),
      
      emp_pas = case_when(pas %in% c(11:51) ~ 1, pas == 81 ~ 0, T ~ NA),
      emp_pas = labelled(emp_pas, labels = emp_labels),
      
      emp_cws = case_when(cws %in% c(11:72) ~ 1, cws %in% c(81, 82) ~ 0, T ~ NA),
      emp_cws = labelled(emp_cws, labels = emp_labels),
      
      # employment type
      emptyp_uas = case_when(uas %in% c(11, 12, 21) ~ 1, uas == 31 ~ 2,
                             uas %in% c(41, 51) ~ 3, T ~ NA),
      emptyp_uas = labelled(emptyp_uas, labels = emptyp_labels),
      
      emptyp_pas = case_when(pas %in% c(11, 12, 21) ~ 1, pas == 31 ~ 2,
                             pas %in% c(41, 51) ~ 3, T ~ NA),
      emptyp_pas = labelled(emptyp_pas, labels = emptyp_labels),
      
      emptyp_sas = case_when(sas %in% c(11, 12, 21) ~ 1, sas == 31 ~ 2,
                             sas %in% c(41, 51) ~ 3, T ~ NA),
      emptyp_sas = labelled(emptyp_sas, labels = emptyp_labels),
      
      emptyp_cws = case_when(cws %in% c(11, 12, 21, 61, 62) ~ 1, 
                             cws %in% c(31, 71, 72) ~ 2,
                             cws %in% c(41, 42, 51) ~ 3, T ~ NA),
      emptyp_cws = labelled(emptyp_cws, labels = emptyp_labels),
      
      # employment type disaggregated
      emptypd_uas = case_when(uas == 11 ~ 1, uas == 12 ~ 2, uas == 21 ~ 3,
                              uas == 31 ~ 4, uas == 41 ~ 5, uas == 51 ~ 6,
                              T ~ NA),
      emptypd_uas = labelled(emptypd_uas, labels = emptypd_labels),
      
      emptypd_pas = case_when(pas == 11 ~ 1, pas == 12 ~ 2, pas == 21 ~ 3,
                              pas == 31 ~ 4, pas == 41 ~ 5, pas == 51 ~ 6,
                              T ~ NA),
      emptypd_pas = labelled(emptypd_pas, labels = emptypd_labels),
      
      emptypd_sas = case_when(sas == 11 ~ 1, sas == 12 ~ 2, sas == 21 ~ 3,
                              sas == 31 ~ 4, sas == 41 ~ 5, sas == 51 ~ 6,
                              T ~ NA),
      emptypd_sas = labelled(emptypd_sas, labels = emptypd_labels),
      
      emptypd_cws = case_when(cws == 11 ~ 1, cws == 12 ~ 2, cws == 21 ~ 3,
                              cws %in% c(61, 62) ~ 4, cws %in% c(31, 71, 72) ~ 5,
                              cws %in% c(41, 42) ~ 6, cws == 51 ~ 7, T ~ NA),
      emptypd_cws = labelled(emptypd_cws, labels = emptypd_cws_labels),
      
      # industry
      ind_uas = case_when(idx_pas == 1 ~ ind_pas, idx_sas == 1 ~ ind_sas, T ~ NA),
      nic2008_section_uas = labelled(nic_map(as.numeric(str_sub(ind_uas, start = 1, end = -4))), labels = nic2008_section_labels),
      nic2008_section_pas = labelled(nic_map(as.numeric(str_sub(ind_pas, start = 1, end = -4))), labels = nic2008_section_labels),
      nic2008_section_sas = labelled(nic_map(as.numeric(str_sub(ind_sas, start = 1, end = -4))), labels = nic2008_section_labels),
      nic2008_section_cws = labelled(nic_map(ind_cws), labels = nic2008_section_labels),
      
      # occupation
      ocu_uas = case_when(idx_pas == 1 ~ ocu_pas, idx_sas == 1 ~ ocu_sas, T ~ NA),
      # nco 2004 was used till 2020-21 - need harmonisation 
      nco2015_division_uas = labelled(as.numeric(str_sub(ocu_uas, end = 1)), labels = nco2015_division_labels), 
      nco2015_division_pas = labelled(as.numeric(str_sub(ocu_pas, end = 1)), labels = nco2015_division_labels),
      nco2015_division_sas = labelled(as.numeric(str_sub(ocu_sas, end = 1)), labels = nco2015_division_labels),
      nco2015_division_cws = labelled(as.numeric(str_sub(ocu_cws, end = 1)), labels = nco2015_division_labels),
      
      # enterprise type
      etyp_uas = case_when(idx_pas == 1 ~ etyp_pas, idx_sas == 1 ~ etyp_sas, T ~ NA),
      etyp_uas = labelled(etyp_uas, labels = etyp_labels),
      etyp_pas = labelled(etyp_pas, labels = etyp_labels),
      etyp_sas = labelled(etyp_sas, labels = etyp_labels),
      
      # worker count
      wrkr_uas = case_when(idx_pas == 1 ~ wrkr_pas, idx_sas == 1 ~ wrkr_sas, T ~ NA),
      wrkr_uas = labelled(wrkr_uas, labels = wrkr_labels),
      wrkr_pas = labelled(wrkr_pas, labels = wrkr_labels),
      wrkr_sas = labelled(wrkr_sas, labels = wrkr_labels),
      
      # leave
      leave_pas = case_when(leave_pas == 1 ~ 1, leave_pas == 2 ~ 0, T ~ NA),
      leave_sas = case_when(leave_sas == 1 ~ 1, leave_sas == 2 ~ 0, T ~ NA),
      leave_uas = case_when(idx_pas == 1 ~ leave_pas, idx_sas == 1 ~ leave_sas, T ~ NA),
      
      # job contract
      jcon_pas = case_when(job_pas %in% c(2:4) ~ 1, job_pas == 1 ~ 0, T ~ NA),
      jcon_sas = case_when(job_sas %in% c(2:4) ~ 1, job_sas == 1 ~ 0, T ~ NA),
      jcon_uas = case_when(idx_pas == 1 ~ jcon_pas, idx_sas == 1 ~ jcon_sas, T ~ NA),
      
      # social security
      ssb_pas = case_when(ssec_pas %in% c(1:7) ~ 1, ssec_pas == 8 ~ 0, T ~ NA),
      ssb_sas = case_when(ssec_sas %in% c(1:7) ~ 1, ssec_sas == 8 ~ 0, T ~ NA),
      ssb_uas = case_when(idx_pas == 1 ~ ssb_pas, idx_sas == 1 ~ ssb_sas, T ~ NA),
      
      # informality (proprietary and partnership enterprises in non-agriculture sector) - PLFS definition
      infpp_uas = case_when(etyp_uas %in% c(1:4) & nic2008_section_uas == 1 ~ 1,
                            etyp_uas %in% c(1:4) & nic2008_section_uas != 1 ~ 2,
                            !is.na(etyp_uas) & nic2008_section_uas == 1 ~ 3,
                            !is.na(etyp_uas) & nic2008_section_uas != 1 ~ 4,
                            T ~ NA),
      
      infpp_pas = case_when(etyp_pas %in% c(1:4) & nic2008_section_pas == 1 ~ 1,
                            etyp_pas %in% c(1:4) & nic2008_section_pas != 1 ~ 2,
                            !is.na(etyp_pas) & nic2008_section_pas == 1 ~ 3,
                            !is.na(etyp_pas) & nic2008_section_pas != 1 ~ 4,
                            T ~ NA),
      
      # organised sector - ILO definitions
      sectyp_uas = case_when(wf_uas == 1 & (etyp_uas %in% c(1:4, 19) | is.na(etyp_uas)) ~ 
                            case_when(wrkr_uas %in% c(1, 2) ~ 2,
                                      wrkr_uas %in% c(3, 4) ~ 1,
                                      wrkr_uas == 9  & ssb_uas == 1 ~ 1,
                                      wrkr_uas == 9  & (ssb_uas == 0 | is.na(ssb_uas)) ~ 2,
                                      is.na(wrkr_uas) & ssb_uas == 1 ~ 1,
                                      is.na(wrkr_uas) & (ssb_uas == 0 | is.na(ssb_uas)) ~ 2,
                                      T ~ NA),
                          wf_uas == 1 & etyp_uas %in% 5:11 ~ 1,
                          wf_uas == 1 & etyp_uas == 12 ~ 2,
                          T ~ NA),
      sectyp_uas = labelled(sectyp_uas, labels = sectyp_labels),
      
      sectyp_pas = case_when(wf_pas == 1 & (etyp_pas %in% c(1:4, 19) | is.na(etyp_pas)) ~ 
                            case_when(wrkr_pas %in% c(1, 2) ~ 2,
                                      wrkr_pas %in% c(3, 4) ~ 1,
                                      wrkr_pas == 9  & ssb_pas == 1 ~ 1,
                                      wrkr_pas == 9  & (ssb_pas == 0 | is.na(ssb_pas)) ~ 2,
                                      is.na(wrkr_pas) & ssb_pas == 1 ~ 1,
                                      is.na(wrkr_pas) & (ssb_pas == 0 | is.na(ssb_pas)) ~ 2,
                                      T ~ NA),
                          wf_pas == 1 & etyp_pas %in% 5:11 ~ 1,
                          wf_pas == 1 & etyp_pas == 12 ~ 2,
                          T ~ NA),
      sectyp_pas = labelled(sectyp_pas, labels = sectyp_labels),
      
      # formal employment - ILO definitions
      form_emp_uas = case_when(uas %in% c(11, 21) ~ 2,
                             uas == 12 ~ case_when(sectyp_uas == 1 ~ 1,
                                                   sectyp_uas == 2 ~ 2,
                                                   T ~ NA),
                             uas %in% c(31, 41, 51) ~ case_when(ssb_uas == 1 ~ 1,
                                                                ssb_uas == 0 | is.na(ssb_uas) ~ 2,
                                                                T ~ NA),
                             T ~ NA),
      form_emp_uas = labelled(form_emp_uas, labels = form_emp_labels),
      
      form_emp_pas = case_when(pas %in% c(11, 21) ~ 2,
                             pas == 12 ~ case_when(sectyp_pas == 1 ~ 1,
                                                   sectyp_pas == 2 ~ 2,
                                                   T ~ NA),
                             pas %in% c(31, 41, 51) ~ case_when(ssb_pas == 1 ~ 1,
                                                                ssb_pas == 0 | is.na(ssb_pas) ~ 2,
                                                                T ~ NA),
                             T ~ NA),
      form_emp_pas = labelled(form_emp_pas, labels = form_emp_labels),
      
      # total days worked in a week based on activity statuses
      totdays_wrk_das = case_when(wf_cws == 1 ~ rowSums(ndays_das(das17, das27, das16, das26, das15, das25, das14, 
                                                                  das24, das13, das23, das12, das22, das11, das21,
                                                                  activity_codes = c(11, 12, 21, 31, 41, 42, 51)),
                                                        na.rm = T),
                                  T ~ NA),
      
      # total days worked in a week based on total hours engaged
      totdays_wrk_hrs = case_when(wf_cws == 1 ~ rowSums(across(c(hr7, hr6, hr5, hr4, hr3, hr2, hr1), ~ case_when(.x > 4 ~ 1,
                                                                                                                 .x > 0 & .x <= 4 ~ 0.5,
                                                                                                                 T ~ 0)),
                                                        na.rm = T),
                              T ~ NA),
      
      # total days worked by casual labourers in a week based on hours engaged in casual labour
      totdays_wrk_cl = case_when(wf_cws == 1 ~ rowSums(ndays_cl(das17, das27, hr17, hr27, das16, das26, hr16, hr26,
                                                                das15, das25, hr15, hr25, das14, das24, hr14, hr24,
                                                                das13, das23, hr13, hr23, das12, das22, hr12, hr22,
                                                                das11, das21, hr11, hr21, activity_codes = c(41, 42, 51)),
                                                       na.rm = T),
                                 T ~ NA),
      
      # earnings
      ern_self = case_when(emptyp_cws == 1 ~ ern_self,
                           T ~ NA),
      
      ern_reg = case_when(emptyp_cws == 2 ~ ern_reg,
                          T ~ NA),
      
      ern_cl = case_when(emptyp_cws == 3 ~ rowSums(cbind(ern17, ern27, ern16, ern26, ern15, ern25, ern14, ern24, 
                                                         ern13, ern23, ern12, ern22, ern11, ern21), 
                                                   na.rm = T)/totdays_wrk_cl,
                         T ~ NA),
      
      # total hours worked in a week
      tothrs_wrk = case_when(wf_cws == 1 ~ rowSums(cbind(hr7, hr6, hr5, hr4, hr3, hr2, hr1), na.rm = T),
                             T ~ NA)
      
      )
  
}
