
#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, site_certified_days, 
#' facilitycode, late_ineligible
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' enrollment_status_by_site()
#' }
enrollment_status_by_site <- function(analytic){
  df <- analytic %>% 
    select(screened, eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, site_certified_days, 
           facilitycode, late_ineligible) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certified_days))) %>% 
    rename(Facility = facilitycode) %>% 
    rename(not_enrolled = not_consented) %>% 
    filter(!is.na(Facility))
  
  
  df_1st <- df %>% 
    group_by(Facility) %>% 
    summarize('Days Certified' = site_certified_days[1], Screened = sum(screened), Eligible = sum(eligible))
  
  df_2nd <- df %>% 
    filter(eligible == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(Refused = sum(refused), 'Not Enrolled for Other Reasons' = sum(not_enrolled), Consented = sum(consented))
  
  df_3rd <- df %>% 
    filter(eligible == TRUE & consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize("Discontinued Pre-Randomization" = sum(discontinued_pre_randomization),
              "Late Ineligible" = sum(late_ineligible), 
              "Enrolled" = sum(enrolled)) 
  
  table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
    left_join(df_3rd, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,NA,`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(`Discontinued Pre-Randomization` = format_count_percent(`Discontinued Pre-Randomization`, Consented)) %>% 
    mutate(`Late Ineligible` = format_count_percent(`Late Ineligible`, Consented)) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Consented)) %>% 
    mutate(Consented = format_count_percent(Consented, Eligible)) %>% 
    mutate(Refused = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Enrolled for Other Reasons` = format_count_percent(`Not Enrolled for Other Reasons`, Eligible)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Screened))
  
  table<- kable(table_raw, align='l', padding='2l') %>% 
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 3)) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}



#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled (Variable Discontinued)
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, site_certified_days, facilitycode
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' enrollment_status_by_site_var_discontinued()
#' }
enrollment_status_by_site_var_discontinued <- function(analytic, discontinued="discontinued", discontinued_colname="Discontinued"){
  df <- analytic %>% 
    select(screened, eligible, refused, not_consented, consented, not_randomized, randomized, enrolled, site_certified_days, 
           facilitycode, all_of(discontinued))
  
  colnames(df)[11] <- "discontinued"
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certified_days))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  
  df_1st <- df %>% 
    group_by(Facility) %>% 
    summarize('Days Certified' = site_certified_days[1], Screened = sum(screened), Eligible = sum(eligible))
  
  df_2nd <- df %>% 
    filter(eligible == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(Refused = sum(refused), 'Not Consented' = sum(not_consented), Consented = sum(consented))
  
  df_3rd <- df %>% 
    filter(eligible == TRUE & consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize("Randomized" = sum(randomized),
              !!discontinued_colname := sum(discontinued),
              "Enrolled" = sum(enrolled)) 
  
  table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
    left_join(df_3rd, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,NA,`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(!!discontinued_colname := format_count_percent(!!sym(discontinued_colname), Consented)) %>% 
    mutate(Randomized = format_count_percent(Randomized, Consented)) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Consented)) %>% 
    mutate(Consented = format_count_percent(Consented, Eligible)) %>% 
    mutate(Refused = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Consented` = format_count_percent(`Not Consented`, Eligible)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Screened))
  
  table<- kable(table_raw, align='l', padding='2l') %>% 
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 3)) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}





#' Ankle and Plateau X-Ray and Measurement Status
#'
#' @description This function visualizes Ankle and Plateau X-Ray and Measurement Status
#'
#' @param analytic This is the analytic data set that must include followup_expected_6wk, 
#' followup_expected_3mo, followup_expected_6mo, followup_expected_12mo, injury_type,
#' radiographs_taken_6wk, radiographs_taken_3mo, radiographs_taken_6mo,
#' plat_tib_fib_overlap_6mo, plat_sagittal_pl_alignment_6mo, plat_patella_centered_6mo, 
#' plat_medial_prox_tibia_deg_6mo, plat_medial_lateral_diff_6mo, plat_condylar_width_6mo, plat_art_step_off_medial_6mo, 
#' plat_art_step_off_lateral_6mo, plat_femur_tibia_deg_6mo,
#' plat_tib_fib_overlap_3mo, plat_sagittal_pl_alignment_3mo, plat_patella_centered_3mo, 
#' plat_medial_prox_tibia_deg_3mo, plat_medial_lateral_diff_3mo, plat_condylar_width_3mo, plat_art_step_off_medial_3mo, 
#' plat_art_step_off_lateral_3mo, plat_femur_tibia_deg_3mo,
#' plat_tib_fib_overlap_6wk, plat_sagittal_pl_alignment_6wk, plat_patella_centered_6wk, 
#' plat_medial_prox_tibia_deg_6wk, plat_medial_lateral_diff_6wk, plat_condylar_width_6wk, plat_art_step_off_medial_6wk, 
#' plat_art_step_off_lateral_6wk, plat_femur_tibia_deg_6wk,
#' plat_tib_fib_overlap_12mo, plat_sagittal_pl_alignment_12mo, plat_patella_centered_12mo, 
#' plat_medial_prox_tibia_deg_12mo, plat_medial_lateral_diff_12mo, plat_condylar_width_12mo, plat_art_step_off_medial_12mo, 
#' plat_art_step_off_lateral_12mo, plat_femur_tibia_deg_12mo
#' ankle_talar_tilt_degrees_6wk, ankle_sagital_disp_6wk, ankle_coronal_plane_disp_6wk, 
#' ankle_talar_tilt_degrees_3mo, ankle_sagital_disp_3mo, ankle_coronal_plane_disp_3mo, 
#' ankle_talar_tilt_degrees_6mo, ankle_sagital_disp_6mo, ankle_coronal_plane_disp_6mo, 
#' ankle_talar_tilt_degrees_12mo, ankle_sagital_disp_12mo, ankle_coronal_plane_disp_12mo
#' 
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' ankle_and_plateau_x_ray_and_measurement_status()
#' }
ankle_and_plateau_x_ray_and_measurement_status <- function(analytic){
  
  df1_ankle <- analytic %>% 
    filter(injury_type=="ankle") %>% 
    select(cfu_status_6wk, cfu_status_3mo, cfu_status_6mo) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = ifelse(str_detect(value,"Complete"),"Complete",value)) %>% 
    group_by(name, value) %>%
    count() %>%
    filter(!is.na(value)) %>%
    ungroup() %>% 
    mutate(name = str_replace(str_replace(str_remove(name, "cfu_status_"),"mo", " Months"),"wk", " Weeks"))
  
  
  df1_expected_ankle <- analytic %>% 
    filter(injury_type=="ankle") %>% 
    select(followup_expected_6wk, followup_expected_3mo, followup_expected_6mo) %>% 
    summarise("3 Months"= sum(followup_expected_3mo, na.rm = TRUE),
              "6 Months"= sum(followup_expected_6mo, na.rm = TRUE),  "6 Weeks"= sum(followup_expected_6wk, na.rm = TRUE)) %>% 
    pivot_longer(everything())  %>% 
    rename(n=value) %>% 
    mutate(value="Expected")
  
  df1_shell_ankle <- tibble(name=c("6 Weeks", "3 Months", "6 Months")) %>% 
    group_by(name) %>% 
    reframe(value=c("Complete", "Incomplete", "Missing", "Expected")) %>% 
    ungroup()
  
  df_one_ankle <- left_join(df1_shell_ankle, bind_rows(df1_expected_ankle, df1_ankle)) %>% 
    mutate(value = factor(value, c("Expected", "Complete", "Incomplete", "Missing"))) %>% 
    mutate(name = factor(name, c("6 Weeks", "3 Months", "6 Months"))) %>% 
    arrange(name, value) %>% 
    mutate(n = replace_na(n, 0)) %>% 
    rename("Complete Visits"=value) %>% 
    left_join(df1_expected_ankle %>% mutate(expected=n) %>% select(name,expected)) %>% 
    mutate(n = ifelse(`Complete Visits`!="Expected",format_count_percent(n,expected),n)) %>% 
    select(-expected)
  
  df2_ankle_expected <- analytic %>% 
    filter(injury_type=="ankle") %>% 
    select(followup_expected_6wk, followup_expected_3mo, followup_expected_6mo, 
           radiographs_taken_6wk, radiographs_taken_3mo, radiographs_taken_6mo) %>% 
    mutate(radiographs_taken_6wk = ifelse(followup_expected_6wk,
                                          ifelse(is.na(radiographs_taken_6wk),"Missing",radiographs_taken_6wk),NA)) %>% 
    mutate(radiographs_taken_3mo = ifelse(followup_expected_3mo,
                                          ifelse(is.na(radiographs_taken_3mo),"Missing",radiographs_taken_3mo),NA)) %>% 
    mutate(radiographs_taken_6mo = ifelse(followup_expected_6mo,
                                          ifelse(is.na(radiographs_taken_6mo),"Missing",radiographs_taken_6mo),NA)) %>% 
    select(-followup_expected_6wk, -followup_expected_3mo, -followup_expected_6mo) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = ifelse(str_detect(value,"Yes|YES"),"Yes",value)) %>% 
    group_by(name, value) %>%
    count() %>%
    filter(!is.na(value)) %>%
    ungroup() %>% 
    mutate(name = str_replace(str_replace(str_remove(name, "radiographs_taken_"),"mo", " Months"),"wk", " Weeks"))
  
  df2_ankle <- df2_ankle_expected %>% 
    left_join(df1_expected_ankle %>% mutate(expected=n) %>% select(name,expected)) %>% 
    mutate(n = format_count_percent(n,expected)) %>% 
    select(-expected)
  
  df2_ankle_expected <- df2_ankle_expected %>% 
    filter(value=="Yes") %>% 
    mutate(expected=n) %>% 
    select(name, expected)
  
  df2_shell_ankle <- tibble(name=c("6 Weeks", "3 Months", "6 Months")) %>% 
    group_by(name) %>% 
    reframe(value=c("Yes", "No", "Missing", " ")) %>% 
    ungroup()
  
  df_two_ankle <- left_join(df2_shell_ankle, df2_ankle) %>% 
    mutate(value = factor(value, c("Yes", "No", "Missing", " "))) %>% 
    mutate(name = factor(name, c("6 Weeks", "3 Months", "6 Months"))) %>% 
    arrange(name, value) %>% 
    mutate(n = ifelse(value!=" " & is.na(n)," 0 ( 0%)",n)) %>% 
    mutate(n = ifelse(value==" "," ",n)) %>% 
    rename("Radiographs"=value, n2=n)
  
  df3_ankle <- analytic %>% 
    select(injury_type, followup_expected_6wk, followup_expected_3mo, followup_expected_6mo, 
           ankle_talar_tilt_degrees_6wk, ankle_sagital_disp_6wk, ankle_coronal_plane_disp_6wk, 
           ankle_talar_tilt_degrees_3mo, ankle_sagital_disp_3mo, ankle_coronal_plane_disp_3mo, 
           ankle_talar_tilt_degrees_6mo, ankle_sagital_disp_6mo, ankle_coronal_plane_disp_6mo) %>% 
    filter(injury_type=="ankle") %>% 
    select(-injury_type) %>% 
    mutate(
      ankle_6_weeks = rowSums(is.na(select(., ends_with("6wk"))))<3,
      ankle_3_months = rowSums(is.na(select(., ends_with("3mo"))))<3,
      ankle_6_months = rowSums(is.na(select(., ends_with("6mo"))))<3
    ) %>% 
    mutate(ankle_6_weeks = ifelse(followup_expected_6wk, ankle_6_weeks,NA)) %>% 
    mutate(ankle_3_months = ifelse(followup_expected_3mo, ankle_3_months,NA)) %>% 
    mutate(ankle_6_months = ifelse(followup_expected_6mo, ankle_6_months,NA)) %>% 
    select(ankle_6_weeks,ankle_3_months,ankle_6_months) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = ifelse(value,"Completed","Not Completed")) %>% 
    group_by(name, value) %>%
    count() %>% 
    filter(!is.na(value)) %>%
    ungroup() %>% 
    mutate(name = str_to_title(str_replace(str_remove(name, "ankle_"),"_", " "))) %>% 
    left_join(df2_ankle_expected) %>% 
    mutate(n = format_count_percent(n, expected)) %>% 
    select(-expected)
  
  df3_shell_ankle <- tibble(name=c("6 Weeks", "3 Months", "6 Months")) %>% 
    group_by(name) %>% 
    reframe(value=c("Completed","Not Completed", "  "," ")) %>% 
    ungroup()
  
  df_three_ankle <- left_join(df3_shell_ankle, df3_ankle) %>% 
    mutate(value = factor(value, c("Completed","Not Completed", "  "," "))) %>% 
    mutate(name = factor(name, c("6 Weeks", "3 Months", "6 Months"))) %>% 
    arrange(name, value) %>% 
    mutate(n = ifelse(value==" "|value=="  ","",n)) %>% 
    rename("X-ray Measurements"=value, n3=n)
  
  df_ankle <- cbind(df_one_ankle %>% select(-name), df_two_ankle %>% select(-name), df_three_ankle %>% select(-name))
  
  index_vec <- c("6 Weeks"=4,"3 Months"=4, "6 Months"=4)
  
  table_raw_ankle<- kable(df_ankle, align='l', padding='2l', col.names = str_replace(colnames(df),"^n.|^n"," ")) %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left')
  
  
  
  df1_plateau <- analytic %>% 
    filter(injury_type=="plateau") %>% 
    select(cfu_status_6wk, cfu_status_3mo, cfu_status_6mo) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = ifelse(str_detect(value,"Complete"),"Complete",value)) %>% 
    group_by(name, value) %>%
    count() %>%
    filter(!is.na(value)) %>%
    ungroup() %>% 
    mutate(name = str_replace(str_replace(str_remove(name, "cfu_status_"),"mo", " Months"),"wk", " Weeks"))
  
  
  df1_expected_plateau <- analytic %>% 
    filter(injury_type=="plateau") %>% 
    select(followup_expected_6wk, followup_expected_3mo, followup_expected_6mo) %>% 
    summarise("3 Months"= sum(followup_expected_3mo, na.rm = TRUE),
              "6 Months"= sum(followup_expected_6mo, na.rm = TRUE),  "6 Weeks"= sum(followup_expected_6wk, na.rm = TRUE)) %>% 
    pivot_longer(everything())  %>% 
    rename(n=value) %>% 
    mutate(value="Expected")
  
  df1_shell_plateau <- tibble(name=c("6 Weeks", "3 Months", "6 Months")) %>% 
    group_by(name) %>% 
    reframe(value=c("Complete", "Incomplete", "Missing", "Expected")) %>% 
    ungroup()
  
  df_one_plateau <- left_join(df1_shell_plateau, bind_rows(df1_expected_plateau, df1_plateau)) %>% 
    mutate(value = factor(value, c("Expected", "Complete", "Incomplete", "Missing"))) %>% 
    mutate(name = factor(name, c("6 Weeks", "3 Months", "6 Months"))) %>% 
    arrange(name, value) %>% 
    mutate(n = replace_na(n, 0)) %>% 
    rename("Complete Visits"=value) %>% 
    left_join(df1_expected_plateau %>% mutate(expected=n) %>% select(name,expected)) %>% 
    mutate(n = ifelse(`Complete Visits`!="Expected",format_count_percent(n,expected),n)) %>% 
    select(-expected)
  
  df2_plateau_expected <- analytic %>% 
    filter(injury_type=="plateau") %>% 
    select(followup_expected_6wk, followup_expected_3mo, followup_expected_6mo, 
           radiographs_taken_6wk, radiographs_taken_3mo, radiographs_taken_6mo) %>% 
    mutate(radiographs_taken_6wk = ifelse(followup_expected_6wk,
                                          ifelse(is.na(radiographs_taken_6wk),"Missing",radiographs_taken_6wk),NA)) %>% 
    mutate(radiographs_taken_3mo = ifelse(followup_expected_3mo,
                                          ifelse(is.na(radiographs_taken_3mo),"Missing",radiographs_taken_3mo),NA)) %>% 
    mutate(radiographs_taken_6mo = ifelse(followup_expected_6mo,
                                          ifelse(is.na(radiographs_taken_6mo),"Missing",radiographs_taken_6mo),NA)) %>% 
    select(-followup_expected_6wk, -followup_expected_3mo, -followup_expected_6mo) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = ifelse(str_detect(value,"Yes|YES"),"Yes",value)) %>% 
    group_by(name, value) %>%
    count() %>%
    filter(!is.na(value)) %>%
    ungroup() %>% 
    mutate(name = str_replace(str_replace(str_remove(name, "radiographs_taken_"),"mo", " Months"),"wk", " Weeks"))
  
  df2_plateau <- df2_plateau_expected %>% 
    left_join(df1_expected_plateau %>% mutate(expected=n) %>% select(name,expected)) %>% 
    mutate(n = format_count_percent(n,expected)) %>% 
    select(-expected)
  
  df2_plateau_expected <- df2_plateau_expected %>% 
    filter(value=="Yes") %>% 
    mutate(expected=n) %>% 
    select(name, expected)
  
  df2_shell_plateau <- tibble(name=c("6 Weeks", "3 Months", "6 Months")) %>% 
    group_by(name) %>% 
    reframe(value=c("Yes", "No", "Missing", " ")) %>% 
    ungroup()
  
  df_two_plateau <- left_join(df2_shell_plateau, df2_plateau) %>% 
    mutate(value = factor(value, c("Yes", "No", "Missing", " "))) %>% 
    mutate(name = factor(name, c("6 Weeks", "3 Months", "6 Months"))) %>% 
    arrange(name, value) %>% 
    mutate(n = ifelse(value!=" " & is.na(n)," 0 ( 0%)",n)) %>% 
    mutate(n = ifelse(value==" "," ",n)) %>% 
    rename("Radiographs"=value, n2=n)
  
  df3_plateau <- analytic %>% 
    select(injury_type, followup_expected_6wk, followup_expected_3mo, followup_expected_6mo, followup_expected_12mo, 
           plat_tib_fib_overlap_6mo, plat_sagittal_pl_alignment_6mo, plat_patella_centered_6mo, 
           plat_medial_prox_tibia_deg_6mo, plat_medial_lateral_diff_6mo, plat_condylar_width_6mo, plat_art_step_off_medial_6mo, 
           plat_art_step_off_lateral_6mo, plat_femur_tibia_deg_6mo,
           plat_tib_fib_overlap_3mo, plat_sagittal_pl_alignment_3mo, plat_patella_centered_3mo, 
           plat_medial_prox_tibia_deg_3mo, plat_medial_lateral_diff_3mo, plat_condylar_width_3mo, plat_art_step_off_medial_3mo, 
           plat_art_step_off_lateral_3mo, plat_femur_tibia_deg_3mo,
           plat_tib_fib_overlap_6wk, plat_sagittal_pl_alignment_6wk, plat_patella_centered_6wk, 
           plat_medial_prox_tibia_deg_6wk, plat_medial_lateral_diff_6wk, plat_condylar_width_6wk, plat_art_step_off_medial_6wk, 
           plat_art_step_off_lateral_6wk, plat_femur_tibia_deg_6wk) %>% 
    filter(injury_type=="plateau") %>% 
    select(-injury_type) %>% 
    mutate(
      plateau_6_weeks = rowSums(is.na(select(., ends_with("6wk"))))<3,
      plateau_3_months = rowSums(is.na(select(., ends_with("3mo"))))<3,
      plateau_6_months = rowSums(is.na(select(., ends_with("6mo"))))<3,
    ) %>% 
    mutate(plateau_6_weeks = ifelse(followup_expected_6wk, plateau_6_weeks,NA)) %>% 
    mutate(plateau_3_months = ifelse(followup_expected_3mo, plateau_3_months,NA)) %>% 
    mutate(plateau_6_months = ifelse(followup_expected_6mo, plateau_6_months,NA)) %>% 
    select(plateau_6_weeks,plateau_3_months,plateau_6_months) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = ifelse(value,"Completed","Not Completed")) %>% 
    group_by(name, value) %>%
    count() %>% 
    filter(!is.na(value)) %>%
    ungroup() %>% 
    mutate(name = str_to_title(str_replace(str_remove(name, "plateau_"),"_", " "))) %>% 
    left_join(df2_plateau_expected) %>% 
    mutate(n = format_count_percent(n, expected)) %>% 
    select(-expected)
  
  df3_shell_plateau <- tibble(name=c("6 Weeks", "3 Months", "6 Months")) %>% 
    group_by(name) %>% 
    reframe(value=c("Completed","Not Completed", "  "," ")) %>% 
    ungroup()
  
  df_three_plateau <- left_join(df3_shell_plateau, df3_plateau) %>% 
    mutate(value = factor(value, c("Completed","Not Completed", "  "," "))) %>% 
    mutate(name = factor(name, c("6 Weeks", "3 Months", "6 Months"))) %>% 
    arrange(name, value) %>% 
    mutate(n = ifelse(value==" "|value=="  ","",n)) %>% 
    rename("X-ray Measurements"=value, n3=n)
  
  df_plateau <- cbind(df_one_plateau %>% select(-name), df_two_plateau %>% select(-name), df_three_plateau %>% select(-name))
  
  index_vec <- c("6 Weeks"=4,"3 Months"=4, "6 Months"=4)
  
  table_raw_plateau<- kable(df_plateau, align='l', padding='2l', col.names = str_replace(colnames(df),"^n.|^n"," ")) %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left')
  
  output <- paste0("<h3>Ankle</h3><br />",table_raw_ankle, "<h3>Plateau</h3><br />",table_raw_plateau)
  
  return(output)
}


#' Expected, completed, missing, out of window visits by each form
#'
#' @description This function visualizes the expected visits for each timepoint for MRR, CFU, PFU, BPI, AOS, KOOS forms
#'
#' @param analytic This is the analytic data set that must include mrr_status_6wk, mrr_status_3mo, mrr_status_6mo, 
#' mrr_status_12mo, cfu_status_6wk, cfu_status_3mo, cfu_status_6mo, cfu_status_12mo, pfu_status_6wk, pfu_status_3mo, 
#' pfu_status_6mo, pfu_status_12mo, bpi_status_6wk, bpi_status_3mo, bpi_status_6mo,bpi_status_12mo, aos_status_6wk, 
#' aos_status_3mo, aos_status_6mo, aos_status_12mo, koos_status_6wk, koos_status_3mo, koos_status_6mo, koos_status_12mo, 
#' followup_expected_6wk, followup_expected_3mo, followup_expected_6mo, followup_expected_12mo, injury_type
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' visit_status_for_followup_by_form()
#' }
visit_status_for_followup_by_form <- function(analytic){
  
  df <- analytic %>% 
    select(mrr_status_6wk, mrr_status_3mo, mrr_status_6mo, mrr_status_12mo, cfu_status_6wk, cfu_status_3mo, cfu_status_6mo, 
           pfu_status_6wk, pfu_status_3mo, pfu_status_6mo, pfu_status_12mo, bpi_status_6wk, bpi_status_3mo, bpi_status_6mo, 
           bpi_status_12mo, aos_status_6wk, aos_status_3mo, aos_status_6mo, aos_status_12mo, koos_status_6wk, koos_status_3mo, koos_status_6mo, 
           koos_status_12mo)
  
  df_expected <- analytic %>% 
    select(followup_expected_6wk, followup_expected_3mo, followup_expected_6mo, followup_expected_12mo) %>% 
    summarise("12 Months"= sum(followup_expected_12mo, na.rm = TRUE), "3 Months"= sum(followup_expected_3mo, na.rm = TRUE),
              "6 Months"= sum(followup_expected_6mo, na.rm = TRUE),  "6 Weeks"= sum(followup_expected_6wk, na.rm = TRUE)) %>% 
    mutate(Form = "Enrolled") %>% 
    mutate(Status = "Expected") 
  
  df_injury_expected <- analytic %>% 
    select(followup_expected_6wk, followup_expected_3mo, followup_expected_6mo, followup_expected_12mo, injury_type) %>%
    group_by(injury_type) %>% 
    summarise("12 Months"= sum(followup_expected_12mo, na.rm = TRUE), "3 Months"= sum(followup_expected_3mo, na.rm = TRUE),
              "6 Months"= sum(followup_expected_6mo, na.rm = TRUE),  "6 Weeks"= sum(followup_expected_6wk, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(Form = str_to_title(injury_type))%>% 
    select(-injury_type) %>% 
    mutate(Status = "Expected") %>% 
    filter(!is.na(Form))
  
  
  output <- count_split_cols_long_and_wide(df, '_status_') %>%
    rename(Form=prefix, Status=level)
  
  summed_completes <- summate_levels(output, 'Form', 'Status', c('Complete: Out of Window', 'Complete: In window')) %>%
    mutate(Status='Complete')
  output <- output %>%
    filter(Status!='Complete: In window') %>%
    bind_rows(summed_completes) %>% 
    mutate(Status = ifelse(Status == 'Complete: Out of Window', "Out of Window", Status)) %>% 
    mutate(Form = toupper(Form)) %>%
    rename("6 Weeks"=`6wk`, "3 Months"=`3mo`, "6 Months"=`6mo`, "12 Months"=`12mo`) %>%
    select(Form, Status, `6 Weeks`, `3 Months`, `6 Months`, `12 Months`)
  
  bound_df <- bind_rows(output, df_expected, df_injury_expected) %>% 
    ungroup()
  
  df_table_raw <- reorder_rows(bound_df, list('Form'=c("Enrolled", 'MRR', 'CFU', 'PFU', 'BPI',"Ankle", 'AOS',"Plateau", 'KOOS'), 
                                              'Status'=c('Expected', 'Complete', 'Out of Window', 'Incomplete', 'Missing'))) %>%
    mutate_if(is.numeric, replace_na, 0) 
  
  
  index_vec <- c(" " = 1, "MRR"=4,"CFU"=4, "PFU"=4, "BPI"=4,  "AOS"= 5, "KOOS"=5)
  
  df_for_table <- df_table_raw %>% 
    ungroup() %>% 
    select(-Form) %>% 
    rename("Form Completion"=Status)
  
  table_raw<- kable(df_for_table, align='l', padding='2l') %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    add_indent(c(3,7,11,15,20,25)) %>% 
    kable_styling("striped", full_width = F, position='left') %>% 
    row_spec(c(0,1,18,23), extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}


#' Injury characteristics for OTA classification and Schatzker Type injuries
#'
#' @description This function visualizes the Injury characteristics for OTA classification and Schatzker Types for Ankle and Plateau
#' injuries
#'
#' @param analytic This is the analytic data set that must include injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' injury_ankle_plateau_characteristics()
#' }
injury_ankle_plateau_characteristics <- function(analytic){
  
  df <- analytic %>% 
    select(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled) %>%  filter(enrolled == TRUE)
  
  summary_totals <- df %>%
    filter(injury_type == "plateau" & is.na(injury_classification_ankle_ota) | injury_type == "ankle" & is.na(injury_classification_plat_schatzker)) %>%
    group_by(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    mutate(injury_classification_ankle_ota = ifelse(injury_type == "ankle" & is.na(injury_classification_ankle_ota) & is.na(injury_classification_plat_schatzker), "Missing", injury_classification_ankle_ota)) %>% 
    mutate(injury_classification_plat_schatzker = ifelse(injury_type == "plateau" & is.na(injury_classification_ankle_ota) & is.na(injury_classification_plat_schatzker), "Missing", injury_classification_plat_schatzker)) %>% 
    select(-injury_type) %>% 
    mutate(Name = ifelse(!is.na(injury_classification_ankle_ota), injury_classification_ankle_ota, injury_classification_plat_schatzker)) %>% 
    mutate(Category = ifelse(!is.na(injury_classification_ankle_ota), "O", "T")) %>% 
    select(-injury_classification_ankle_ota, -injury_classification_plat_schatzker)
  
  injury_type_total <- df %>% 
    group_by(injury_type) %>% 
    summarise(Total = n()) %>%
    ungroup() %>% 
    rename(Name = injury_type) %>% 
    mutate(Category = ifelse(Name == "ankle", "A", "P")) 
  
  total_sum <- sum(injury_type_total$Total)
  total_ank <- injury_type_total$Total[injury_type_total$Name=="ankle"]
  total_plat <- injury_type_total$Total[injury_type_total$Name!="ankle"]
  
  summary_table <- bind_rows(injury_type_total, summary_totals) %>% 
    arrange(Category) %>% 
    mutate(Name = ifelse(Name == "ankle", "Number of Ankles", 
                         ifelse(Name == "plateau", "Number of Plateaus", Name))) %>% 
    mutate(Total = format_count_percent(Total, ifelse(Category=="O", 
                                                      total_ank,
                                                      ifelse(Category=="T", 
                                                             total_plat,total_sum)), decimals = 2))
  
  ota_number <- summary_table %>% 
    filter(Category == "O") %>% 
    nrow()
  
  schatzer_number <- summary_table %>% 
    filter(Category == "T") %>% 
    nrow()
  
  
  df_table <- summary_table %>% 
    select(-Category)
  
  index_vec <- c("OTA Classification"= ota_number+1, "Tibial Plateau"=schatzer_number+1) 
  
  table_raw<- kable(df_table, align='l', padding='2l', col.names = NULL) %>%
    add_indent(c(seq(ota_number)+1, seq(schatzer_number)+1+ota_number+1)) %>% 
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left")
  
  return(table_raw)
}

#' Baseline Characteristics Percent 
#'
#' @description This function visualizes the categorical percentages of baseline characteristics sex, age, race, education, and military
#'
#' @param analytic This is the analytic data set that must include enrolled, age, age_group
#' @param sex is a meta construct that is required that defaults to "sex"
#' @param race is a meta construct that is required that defaults to "ethnicity_race"
#' @param education is a meta construct that is required that defaults to "education_level"
#' @param military is a meta construct that is required that defaults to "military_status"
#' @param sex_levels sets default values and orders for sex meta construct
#' @param race_levels sets default values and orders for race meta construct
#' @param education_levels sets default values and orders for education meta construct
#' @param military_levels sets default values and orders for military meta construct
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' baseline_characteristics_percent()
#' }
baseline_characteristics_percent <- function(analytic, sex="sex", race="ethnicity_race", education="education_level", military="military_status",
                                             sex_levels=c("Female","Male", "Missing"), 
                                             race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                             education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing"), 
                                             military_levels=c("Active Military", "Active Reserves", "Not Active Duty","Missing")){
  
  constructs <- c(sex, race, education, military)
  
  sex_default <- tibble(type=sex_levels)
  race_default <- tibble(type=race_levels)
  education_default <- tibble(type=education_levels)
  military_default <- tibble(type=military_levels)
  
  
  df <- analytic %>% 
    select(enrolled, age_group, age, all_of(constructs)) %>% 
    filter(enrolled) %>% 
    rename(sex = !!sym(sex)) %>% 
    rename(race = !!sym(race)) %>% 
    rename(education = !!sym(education)) %>% 
    rename(military = !!sym(military)) %>% 
    mutate(age = as.numeric(age))
  
  total <- sum(df$enrolled)
  
  sex_df <- df %>% 
    mutate(sex = replace_na(sex, "Missing")) %>% 
    group_by(sex) %>% 
    count(sex) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, total)) %>% 
    select(-number) %>% 
    rename(type = sex) %>% 
    full_join(sex_default) %>% 
    mutate(order = factor(type, sex_levels)) %>% 
    arrange(order) %>% 
    select(-order)
  
  age_df <- df %>% 
    summarize( type = 'Mean (SD)', percentage = format_mean_sd(age))
  
  
  age_group_df <- df %>% 
    mutate(age_group = replace_na(age_group, "Missing")) %>% 
    group_by(age_group) %>% 
    count(age_group) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, total)) %>% 
    select(-number) %>% 
    rename(type = age_group)
  
  education_df <- df %>% 
    mutate(education = replace_na(education, "Missing")) %>% 
    group_by(education) %>% 
    count(education) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, total)) %>% 
    select(-number) %>% 
    rename(type = education) %>% 
    full_join(education_default) %>% 
    mutate(order = factor(type, education_levels)) %>% 
    arrange(order) %>% 
    select(-order)
  
  race_df <- df %>% 
    mutate(race = replace_na(race, "Missing")) %>% 
    group_by(race) %>% 
    count(race) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, total)) %>% 
    select(-number) %>% 
    rename(type = race) %>% 
    full_join(race_default) %>% 
    mutate(order = factor(type, race_levels)) %>% 
    arrange(order) %>% 
    select(-order)
  
  military_df <- df %>% 
    mutate(military = ifelse(is.na(military), "Missing", military)) %>% 
    group_by(military) %>% 
    count(military) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, total)) %>% 
    select(-number) %>% 
    rename(type = military) %>% 
    full_join(military_default) %>% 
    mutate(order = factor(type, military_levels)) %>% 
    arrange(order) %>% 
    select(-order)
  
  df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df, military_df) %>% 
    mutate_all(replace_na, "0 (0%)") 
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  
  vis <- kable(df_final, align='l', padding='2l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Sex' = nrow(sex_df), 'Age' = (nrow(age_df) + nrow(age_group_df)), 'Race' = nrow(race_df), 
                        'Education' = nrow(education_df), 'Military' = nrow(military_df)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis) 
} 

#' Number of Discontinued Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type
#' This was originally made for Union
#'
#' @param analytic This is the analytic data set that must include enrolled, censored_reason, 
#' protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative, sae_count
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' discontinuation_sae_deviation_by_type()
#' }
discontinuation_sae_deviation_by_type <- function(analytic){
  total <- sum(analytic$enrolled, na.rm=T)
  discontinuation_df <- analytic %>% 
    select(enrolled, censored_reason) %>% 
    filter(enrolled == TRUE) %>% 
    count(censored_reason) %>%
    rename(type=censored_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  discontinuation_df_tot <- tibble(type="Discontinuations", n=sum(discontinuation_df$n))
  
  sae_df <- analytic %>% 
    select(study_id, enrolled, sae_count) %>% 
    filter(enrolled & sae_count>0) %>% 
    mutate(sae_count = "SAE") %>% 
    count(sae_count) %>%
    rename(type=sae_count) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_sc_df <- analytic %>% 
    select(study_id, enrolled, protocol_deviation_screen_consent) %>% 
    filter(enrolled == TRUE) %>% 
    count(protocol_deviation_screen_consent) %>%
    rename(type=protocol_deviation_screen_consent) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_p_df <- analytic %>% 
    select(study_id, enrolled, protocol_deviation_procedural) %>% 
    filter(enrolled == TRUE) %>% 
    count(protocol_deviation_procedural) %>%
    rename(type=protocol_deviation_procedural) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_a_df <- analytic %>% 
    select(study_id, enrolled, protocol_deviation_administrative) %>% 
    filter(enrolled == TRUE) %>% 
    count(protocol_deviation_administrative) %>%
    rename(type=protocol_deviation_administrative) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = str_replace(type,"Other: .+","Other")) %>% 
    mutate(type = as.character(type))
  
  deviation_sc_tot <- tibble(type="Screen and Consent",n=sum(deviation_sc_df$n))
  deviation_p_tot <- tibble(type="Procedural",n=sum(deviation_p_df$n))
  deviation_a_tot <- tibble(type="Administrative/Other",n=sum(deviation_a_df$n))
  deviation_df_tot <- tibble(type="Protocol Deviations",n=sum(deviation_sc_df$n)+sum(deviation_p_df$n)+sum(deviation_a_df$n))
  
  
  df_final <- bind_rows(discontinuation_df_tot, discontinuation_df, sae_df, deviation_df_tot, 
                        deviation_sc_tot, deviation_sc_df, deviation_p_tot, deviation_p_df, deviation_a_tot, deviation_a_df) %>% 
    mutate(n = format_count_percent(n, total, decimals=2))
  
  
  n_disc <- nrow(discontinuation_df)
  n_dsc <- nrow(deviation_sc_df)
  n_dp <- nrow(deviation_p_df)
  n_da <- nrow(deviation_a_df)
  
  indents_vec <- vector()
  if(n_dsc > 0){
    indents_vec <- c(indents_vec, 1 + n_disc + 1 + 1 + 1 + seq(n_dsc))
  }
  if(n_dp > 0){
    indents_vec <- c(indents_vec, 1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + seq(n_dp))
  }
  if(n_da > 0){
    indents_vec <- c(indents_vec, 1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + seq(n_da))
  }
  
  vis <- kable(df_final, align='l', padding='2l', col.names = c(" ", paste0("n=",total))) %>%
    add_indent(c(seq(n_disc) + 1, 1 + n_disc + 1 + 1 + seq(1+n_dsc+1+n_dp+1+n_da))) %>% 
    add_indent(indents_vec) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1+ n_disc, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 1px solid") %>%
    kable_styling("striped", full_width = F, position="left") 
  
  
  return(vis)
}


#' Number of Adjudications and Discontinuations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type
#' This was originally made for NSAID
#'
#' @param analytic This is the analytic data set that must include screened; inappropriate_enrollment; 
#' late_ineligible; late_refusal; withdrawn_patient; withdrawn_physician; adjudication_pending; 
#' dead; sae_count; protocol_deviation_screen_consent; protocol_deviation_procedural; protocol_deviation_administrative
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' adjudications_and_discontinuations_by_type()
#' }
adjudications_and_discontinuations_by_type <- function(analytic){
  df <- analytic %>% 
    filter(screened == TRUE) %>% 
    select(inappropriate_enrollment, late_ineligible, late_refusal, withdrawn_patient, withdrawn_physician, #adjudication_pending, 
           dead, sae_count, protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative) %>% 
    
    mutate(na_count = rowSums(is.na(select(., 
                                           study_discontinuation,
                                           protocol_deviation_screen_consent,
                                           protocol_deviation_procedural,
                                           protocol_deviation_administrative,
                                           sae_count)))) %>%
    filter(na_count != 5) %>%
    select(-na_count) %>% 
    mutate(sae_count = ifelse(sae_count == TRUE, 'SAE', sae_count))
  
  total <- sum(df$enrolled)
  
  totals_df <- df %>%
    mutate(total_disc = ifelse(!is.na(study_discontinuation), TRUE, FALSE)) %>% 
    mutate(total_dsc = ifelse(!is.na(protocol_deviation_screen_consent), TRUE, FALSE)) %>% 
    mutate(total_dp = ifelse(!is.na(protocol_deviation_procedural), TRUE, FALSE)) %>% 
    mutate(total_da = ifelse(!is.na(protocol_deviation_administrative), TRUE, FALSE)) %>% 
    mutate(total_sae = ifelse(!is.na(sae_count), TRUE, FALSE)) %>% 
    select(total_disc, total_dsc, total_dp, total_da, total_sae)
  
  total_disc <- sum(totals_df$total_disc)
  total_dsc <- sum(totals_df$total_dsc)
  total_dp <- sum(totals_df$total_dp)
  total_da <- sum(totals_df$total_da)
  total_sae <- sum(totals_df$total_sae)
  
  vec_disc <- c(format_count_percent(total_disc, total))
  vec_protocol_deviations <- c(format_count_percent(total_dsc + total_dp + total_da, total))
  vec_dsc <- c(format_count_percent(total_dsc, total))
  vec_dp <- c(format_count_percent(total_dp, total))
  vec_da <- c(format_count_percent(total_da, total))
  
  
  disc <- tibble(type = "Discontinuous", percentage = vec_disc)
  protocol_deviations <- tibble(type = 'Protocol Deviations', percentage = vec_protocol_deviations)
  sc <- tibble(type = 'Screen and Consent', percentage = vec_dsc)
  dp <- tibble(type = 'Procedural', percentage = vec_dp)
  da <- tibble(type = 'Administrative/Other', percentage = vec_da)
  
  
  study_discontinuation_df <- df %>% 
    select(study_discontinuation) %>% 
    filter(!is.na(study_discontinuation)) %>% 
    count(study_discontinuation) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = study_discontinuation)
  
  protocol_deviation_screen_consent_df <- df %>% 
    select(protocol_deviation_screen_consent) %>% 
    filter(!is.na(protocol_deviation_screen_consent)) %>% 
    count(protocol_deviation_screen_consent) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = protocol_deviation_screen_consent)
  
  protocol_deviation_procedural_df <- df %>% 
    select(protocol_deviation_procedural) %>% 
    filter(!is.na(protocol_deviation_procedural)) %>% 
    count(protocol_deviation_procedural) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = protocol_deviation_procedural)
  
  protocol_deviation_administrative_df <- df %>% 
    select(protocol_deviation_administrative) %>% 
    filter(!is.na(protocol_deviation_administrative)) %>% 
    count(protocol_deviation_administrative) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = protocol_deviation_administrative)
  
  sae_count_df <- df %>% 
    select(sae_count) %>% 
    filter(!is.na(sae_count)) %>% 
    count(sae_count) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = sae_count)
  
  df_final <- rbind(disc, study_discontinuation_df, sae_count_df, protocol_deviations, sc, protocol_deviation_screen_consent_df, 
                    dp, protocol_deviation_procedural_df, da, protocol_deviation_administrative_df) 
  
  n_disc <- nrow(study_discontinuation_df)
  n_dsc <- nrow(protocol_deviation_screen_consent_df)
  n_dp <- nrow(protocol_deviation_procedural_df)
  n_da <- nrow(protocol_deviation_administrative_df)
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  if(n_dsc>0){
    dsc_indents <- seq(n_dsc) + 1 + n_disc + 1 + 1 + 1
  } else{
    dsc_indents <- NA
  }
  
  if(n_dp>0){
    dp_indents <- seq(n_dp) + 1 + n_disc + 1 + 1 + 1 + n_dsc + 1
  } else{
    dp_indents <- NA
  }
  
  if(n_da>0){
    da_indents <- seq(n_da) + 1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1
  } else{
    da_indents <- NA
  }
  
  
  vis <- kable(df_final, align='l', padding='2l', col.names = NULL) %>%
    add_header_above(header) %>%  
    add_indent(c(seq(n_disc) + 1, seq(1 + n_dsc + 1 + n_dp + 1 + n_da) + 1 + n_disc + 2, na.omit(c(dsc_indents, dp_indents, da_indents)))) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1+ n_disc, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 1px solid") %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis)
}

#' Number of patients Ineligible by Top 5 reasons of Exclusion
#'
#' @description This function visualizes the number of patients Ineligible by Top 5 reasons of Exclusion criteria
#'
#' @param analytic This is the analytic data set that must include facilitycode,  screened, ineligible, ineligibility_reasons
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' ineligibility_by_reasons()
#' }
ineligibility_by_reasons <- function(analytic, n_top_reasons = 5){
  
  
  df <- analytic %>% 
    select(study_id, facilitycode,  screened, ineligible, ineligibility_reasons) %>% 
    filter(screened == TRUE) 
  
  reasons <- df %>%  select(study_id, facilitycode, ineligibility_reasons) %>% 
    column_unzipper('ineligibility_reasons', sep = '; ') %>% 
    boolean_column_counter() %>% 
    pivot_longer(everything()) %>% 
    arrange(desc(value)) %>% 
    slice(1:n_top_reasons) %>% 
    pull(name) 
  
  total <- df %>% 
    mutate(ineligibility_reasons = ifelse(ineligibility_reasons %in% reasons, ineligibility_reasons,'Other Reasons')) %>% 
    column_unzipper('ineligibility_reasons', sep = '; ') %>% 
    boolean_column_counter() %>% 
    mutate(Site = 'Total') %>% 
    select(Site, screened, ineligible, all_of(reasons), `Other Reasons`)
  
  
  sites <- df %>% 
    mutate(ineligibility_reasons = ifelse(ineligibility_reasons %in% reasons, ineligibility_reasons,'Other Reasons')) %>% 
    column_unzipper('ineligibility_reasons', sep = '; ') %>% 
    boolean_column_counter(groups = 'facilitycode') %>% 
    rename(Site = facilitycode) %>% 
    select(Site, screened, ineligible, all_of(reasons), `Other Reasons`)
  
  output <- bind_rows(total, sites) %>% 
    rename(Screened = screened,
           Ineligible = ineligible) %>% 
    arrange(desc(Screened)) %>% 
    mutate(Ineligible = format_count_percent(Ineligible, Screened))
  
  top_n_header <- paste0("Top ", n_top_reasons, " Ineligibility Reasons =")
  
  vis <- kable(output, align='l', padding='2l') %>%
    add_header_above(c(" " = 3, top_n_header = n_top_reasons, " " = 1)) %>%  
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis)
}


#' AO Gustillo Tscherne Injury Characteristics
#'
#' @description This function visualizes the injury characteristics
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' injury_gustilo, injury_classification_tscherne, injury_classification_ankle_ao
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' ao_gustillo_tscherne_characteristics()
#' }
ao_gustillo_tscherne_injury_characteristics <- function(analytic){
  pull <- analytic %>% 
    filter(enrolled) %>%
    select(injury_gustilo, injury_classification_tscherne, injury_classification_ankle_ao)
  
  inj_gust <- pull %>% 
    count(injury_gustilo) %>%
    pivot_longer(-n) %>%
    mutate(value=ifelse(!is.na(value), paste('Gustilo Type', value), value)) %>%
    mutate(value=ifelse(is.na(value), 'Unknown', value)) %>%
    select(-name)
  
  total <- inj_gust %>%
    mutate(n=as.numeric(n)) %>%
    pull(n) %>%
    sum() 
  
  inj_ao <- pull %>% 
    count(injury_classification_ankle_ao) %>%
    pivot_longer(-n) %>%
    mutate(value=ifelse(is.na(value), 'Unknown', value)) %>%
    select(-name)
  
  inj_tsch <- pull %>% 
    count(injury_classification_tscherne) %>%
    pivot_longer(-n) %>%
    mutate(value=ifelse(!is.na(value), paste('Tscherne Gotzen Grade', value), value)) %>%
    mutate(value=ifelse(is.na(value), 'Tscherne Unknown', value)) %>%
    select(-name)
  
  combined <- bind_rows(inj_gust, inj_tsch, inj_ao) %>%
    relocate(n, .after=value) %>%
    rename('Fracture Type'=value)
  
  total_closed <- combined %>%
    filter(`Fracture Type`=='Gustilo Type Closed') %>%
    pull(n)
  known_closed <- inj_tsch %>%
    filter(value!='Tscherne Unknown') %>%
    mutate(n=as.numeric(n)) %>%
    pull(n) %>%
    sum()
  
  out <- combined %>%
    mutate(n=ifelse(`Fracture Type`=='Tscherne Unknown', 
                    total_closed-known_closed, n)) %>%
    mutate(n=ifelse(str_detect(`Fracture Type`, 'Tscherne'), 
                    format_count_percent(n, total_closed),
                    format_count_percent(n, total))) %>%
    mutate(`Fracture Type`=ifelse(`Fracture Type`=='Tscherne Unknown', 
                                  'Unknown', 
                                  `Fracture Type`)) 
  
  output<- kable(out, align='l', padding='2l', col.names = c("Fracture Type", paste0("n=",total))) %>% 
    kable_styling("condensed", position="left") %>%
    pack_rows("Open Fracture", 1, nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("Closed Fracture", nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("AO Class", nrow(inj_gust)+nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch)+nrow(inj_ao), label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}


#' Status of IRB Approvals and Certification by Site
#'
#' @description This function returns a list of sites and their dates of 
#' local, DOD, and METRC certifications
#'
#' @param analytic This is the analytic data set that must include site_certified_date
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' ineligibility_by_reasons()
#' }
certification_date_data <- function(analytic){
  df <- analytic %>% 
    select(site_certified_date) %>%
    unique()
  
  date_today <- Sys.Date()
  
  cols <- c('Facility', 'Local (or sIRB)  Approval Date', 'DoD Approval Date',
            'Certified by MCC to Start Screening', 
            paste0('Days Number of Days Certified (as of ', as.character(date_today), ')'))
  
  site_data <- df %>%
    separate(site_certified_date, cols, sep = ';') %>%
    filter(!is.na(Facility))
  
  vis <- kable(site_data, align='l', padding='2l') %>% 
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}



#' Complications by severity and relatedness
#'
#' @description This function visualizes the complications by severity and relatedness for dsmb report
#'
#' @param analytic This is the analytic data set that must include study_id, complication_data
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' complications_by_severity_relatedness()
#' }
complications_by_severity_relatedness <- function(analytic){
  
  comp <- analytic %>%  select(study_id, complication_data) %>% 
    filter(!is.na(complication_data))
  
  
  unzipped_comp <- comp %>%
    separate_rows(complication_data, sep = ";new_row: ") %>%
    separate(complication_data, into = c("redcap_event_name", "visit_date", "complications", 
                                         "first_complication_note", "another_complication_note", "diagnosis_date", 
                                         "relatedness", "severity", "treatment_related", "new_or_previous_diagnosis",
                                         "form_notes"), sep = '\\|')  %>% 
    select(study_id, severity, relatedness, complications) %>% 
    mutate(severity = case_when(
      severity %in% c('Mild', 'Moderate') ~ "Grade 2,1",
      severity == "Severe and Undesirable" ~ "Grade 3",
      severity == "Life-threatening or disabling" ~ "Grade 4",
      severity == "Fatal" ~ "Unknown",
      TRUE ~ NA_character_
    )) %>% 
    group_by(study_id, relatedness, severity,complications) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    pivot_wider(names_from = relatedness, values_from = Total) %>% 
    bind_rows(tibble(
      "Definitely related"= vector(mode="integer"),
      "Probably related" = vector(mode="integer"),
      "Possibly related" = vector(mode="integer"),
      "Unlikely related" = vector(mode="integer"),
      "Unrelated" = vector(mode="integer"),
      "Don't know" = vector(mode="integer"))) %>% 
    rename(Definitely= "Definitely related",
           Probably = "Probably related",
           Possibly = "Possibly related",
           Unlikely = "Unlikely related",
           Unrelated = "Unrelated",
           Unknown = "Don't know") %>% 
    mutate(complications = recode(complications,
                                  "Superficial-infection" = "Superficial",
                                  "Deep-Infection" = "Deep - Involving Bone", 
                                  "Deep-Infection, Not Involving Bone" = "Deep - Not Involving Bone"))
  
  total_complications <- unzipped_comp %>% 
    group_by(severity, complications) %>% 
    summarise(Definitely_c = sum(Definitely, na.rm = T), Possibly_c = sum(Possibly, na.rm = T), 
              Probably_c = sum(Probably, na.rm = T), Unlikely_c = sum(Unlikely, na.rm = T), 
              Unrelated_c = sum(Unrelated, na.rm = T), Unknown_c = sum(Unknown, na.rm = T), 
              Total_c = n()) %>% 
    ungroup() 
  
  comp_sums <- sapply(total_complications[, c("Definitely_c", "Possibly_c", "Probably_c", "Unlikely_c", "Unrelated_c", "Unknown_c")], sum)
  
  summary_comp_sums <- data.frame(t(comp_sums)) 
  
  total_ids <- unzipped_comp %>% 
    mutate_all(replace_na, 0) %>% 
    group_by(severity, complications) %>% 
    summarise(Definitely_id = length(unique(study_id[Definitely > 0])) , Possibly_id = length(unique(study_id[Possibly > 0])) , 
              Probably_id = length(unique(study_id[Probably > 0])) , Unlikely_id = length(unique(study_id[Unlikely > 0])) , 
              Unrelated_id = length(unique(study_id[Unrelated > 0])) , Unknown_id = length(unique(study_id[Unknown > 0])) , 
              Total_id = length(unique(study_id))) %>% 
    ungroup()
  
  
  id_sums <- sapply(total_ids[, c("Definitely_id", "Possibly_id", "Probably_id", "Unlikely_id", "Unrelated_id", "Unknown_id")], sum)
  
  summary_id_sums <- data.frame(t(id_sums)) 
  
  
  output_complication <- full_join(total_complications, total_ids) %>% 
    mutate_all(replace_na, 0) %>% 
    mutate(Definitely = paste0(Definitely_c, "[", Definitely_id, "]"),
           Probably = paste0(Probably_c, "[", Probably_id, "]"),
           Possibly = paste0(Possibly_c, "[", Possibly_id, "]"),
           Unlikely = paste0(Unlikely_c, "[", Unlikely_id, "]"),
           Unrelated = paste0(Unrelated_c, "[", Unrelated_id, "]"),
           Unknown = paste0(Unknown_c, "[", Unknown_id, "]"), 
           Total = paste0(Total_c, "[", Total_id, "]")) %>% 
    select(-ends_with("_id"), -ends_with("_c")) %>% 
    mutate_all(str_replace_all, "0\\[0\\]", "-")
  
  
  
  output_overall <- cross_join(summary_comp_sums, summary_id_sums) %>% 
    mutate(Definitely = paste0(Definitely_c, "[", Definitely_id, "]"),
           Probably = paste0(Probably_c, "[", Probably_id, "]"),
           Possibly = paste0(Possibly_c, "[", Possibly_id, "]"),
           Unlikely = paste0(Unlikely_c, "[", Unlikely_id, "]"),
           Unrelated = paste0(Unrelated_c, "[", Unrelated_id, "]"),
           Unknown = paste0(Unknown_c, "[", Unknown_id, "]")) %>% 
    mutate(Total = paste0(Definitely_c+Probably_c+Possibly_c+Unlikely_c+Unrelated_c+Unknown_c,
                          "[",Definitely_id+Probably_id+Possibly_id+Unlikely_id+Unrelated_id+Unknown_id,"]")) %>% 
    select(-ends_with("_id"), -ends_with("_c")) %>% 
    mutate(complications = "Overall")
  
  severity_categories <- c('Grade 4', 'Grade 3', 'Grade 2,1', 'Grade Unknown')
  level_order <- c("Superficial", "Deep - Involving Bone", "Deep - Not Involving Bone",
                   "Wound Dehiscence", "Wound Seroma/Hematoma", "Fixation failure", "Malunion", "Peri-implant Fracture",
                   "Other")
  
  df_template <- tibble(
    severity = c(severity_categories),
  ) %>% group_by(severity) %>% 
    reframe(complications = level_order)
  
  output_complication <- left_join(df_template, output_complication)%>% 
    mutate_all(replace_na, "-")
  
  output <- bind_rows(output_overall, output_complication) %>% 
    mutate(across(everything(), ~replace(., is.na(.), "-"))) %>% 
    select(complications, everything()) %>% 
    mutate(severity = factor(severity, c("-",severity_categories))) %>%
    mutate(complications = factor(complications, c("Overall",level_order))) %>%
    arrange(severity, complications) %>%
    select(-severity)
  
  colnames(output)[1] <- " "
  
  index_vec <- c(" " = 1, "Grade 4" = 9, "Grade 3"= 9,"Grade 2,1"= 9, "Grade Unknown"= 9)
  subindex_vec <- c(" " = 1, "Infection" = 3, " " = 6, "Infection" = 3, " " = 6, "Infection" = 3, " " = 6,
                    "Infection" = 3, " " = 6)
  table_raw<- kable(output, align='l', padding='2l') %>%  
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    pack_rows(index = subindex_vec,label_row_css = "text-align:left;padding-left: 2em;", bold = FALSE) %>% 
    row_spec(1, extra_css = "border-bottom: 1px solid") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(table_raw)
}



#' Nonunion surgery outcome
#'
#' @description This function visualizes the Nonunion surgery outcome
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' followup_due_3mo, followup_due_12mo, nonunion_90days,  nonunion_1yr
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' nonunion_surgery_outcome()
#' }
nonunion_surgery_outcome <- function(analytic){
  df <- analytic %>% 
    select(enrolled, followup_due_3mo, nonunion_90days, followup_due_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_due_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_due_12mo))
  
  colname <- c("Enrolled", "Expected Three Month", "90 Day Non-Union", "Expected Twelve Month", "1 Year Non-Union")
  
  table<- kable(df, align='l', padding='2l', col.names = colname) %>% 
    kable_styling("striped", full_width = F, position="left")
  return(table)
}


#' Crossover Monitoring by Site
#'
#' @description This function visualizes the crossovers by site in hospital and at discharge
#'
#' @param analytic This is the analytic data set that must include enrolled, df_surg_completed, 
#' ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero, and facilitycode
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' ih_and_dc_crossover_monitoring_by_site()
#' }
ih_and_dc_crossover_monitoring_by_site <- function(analytic){
  df <- analytic %>% 
    select(facilitycode, enrolled, df_surg_completed, ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    rename(Facility = facilitycode) %>% 
    filter(enrolled) %>% 
    mutate(ih_discharge_date = !is.na(ih_discharge_date)) %>% 
    group_by(Facility) %>% 
    summarize('Enrolled' = sum(enrolled),
              "Definitive Fixation Complete" = sum(df_surg_completed), 
              "Discharged from Index Hospitalization" = sum(ih_discharge_date),
              "Discharged on Radomization Date" = sum(ih_discharge_date_on_time_zero),
              "Inpatient Crossover" = sum(crossover_inpatient),
              "Discharge Crossover" = sum(crossover_discharge))
  
  
  table_raw <- df %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total)
  
  table<- kable(table_raw, align='l', padding='2l') %>% 
    kable_styling("striped", full_width = F, position="left")
  return(table)
}


#' Expected visit status for 3 Months, 6 Months, and 12 Months followup
#'
#' @description This function uses status constructs but treats early, late and complete as mutually exclusive.
#' Therefore, complete is renamed to "On Time" and all three of them combined to Complete.
#'
#' @param analytic This is the analytic data set that must include followup_due_3mo, followup_due_6mo, 
#' followup_due_12mo, followup_status_3mo, followup_status_6mo, followup_status_12mo
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' expected_and_followup_visit()
#' }
expected_and_followup_visit <- function(analytic){
  df_expected <- analytic %>% 
    select(followup_due_3mo, followup_due_6mo, followup_due_12mo) %>% 
    summarize("Status" = "Expected", "3 Month" = sum(followup_due_3mo, na.rm = TRUE), "6 Month" = sum(followup_due_6mo, na.rm = TRUE),
              "12 Month" = sum(followup_due_12mo, na.rm = TRUE))
  
  df_complete <- analytic %>% 
    select(followup_status_3mo, followup_status_6mo, followup_status_12mo) %>% 
    mutate(followup_status_3mo = followup_status_3mo == "Complete" | followup_status_3mo == "Early" | followup_status_3mo == "Late") %>% 
    mutate(followup_status_6mo = followup_status_6mo == "Complete" | followup_status_6mo == "Early" | followup_status_6mo == "Late") %>% 
    mutate(followup_status_12mo = followup_status_12mo == "Complete" | followup_status_12mo == "Early" | followup_status_12mo == "Late") %>% 
    summarize("Status" = "Complete", "3 Month" = sum(followup_status_3mo, na.rm = TRUE), "6 Month" = sum(followup_status_6mo, na.rm = TRUE),
              "12 Month" = sum(followup_status_12mo, na.rm = TRUE))
  
  df_3mo <- analytic %>% 
    filter(!is.na(followup_status_3mo)) %>% 
    select(followup_status_3mo) %>% 
    mutate(followup_status_3mo = ifelse(followup_status_3mo == "Complete", "On Time", followup_status_3mo)) %>% 
    group_by(followup_status_3mo) %>%
    count() %>% 
    rename("3 Month" = n, 
           "Status" = followup_status_3mo) 
  
  
  df_6mo <- analytic %>% 
    filter(!is.na(followup_status_6mo)) %>% 
    select(followup_status_6mo) %>% 
    mutate(followup_status_6mo = ifelse(followup_status_6mo == "Complete", "On Time", followup_status_6mo)) %>% 
    group_by(followup_status_6mo) %>%
    count() %>% 
    rename("6 Month" = n, 
           "Status" = followup_status_6mo) 
  
  
  df_12mo <- analytic %>% 
    filter(!is.na(followup_status_12mo)) %>% 
    select(followup_status_12mo) %>% 
    mutate(followup_status_12mo = ifelse(followup_status_12mo == "Complete", "On Time", followup_status_12mo)) %>% 
    group_by(followup_status_12mo) %>%
    count() %>% 
    rename("12 Month" = n, 
           "Status" = followup_status_12mo) 
  
  
  level_order <- c('Early', 'On Time', 'Late', 'Missing', 'Not Started')
  
  bound_df <- full_join(df_3mo, df_6mo) %>% full_join(df_12mo) %>% 
    mutate(Status = factor(Status, level_order)) %>% 
    arrange(Status) %>% 
    mutate_if(is.numeric, replace_na, 0) 
  
  df_new <- bind_rows(df_expected, df_complete, bound_df) %>% 
    filter(Status != "Due")
  
  table_raw<- kable(df_new, align='l', padding='2l') %>%
    add_indent(c(3,4,5)) %>% 
    kable_styling("striped", full_width = F, position='left')
  
  return(table_raw)
}

#' Injury Characteristics
#'
#' @description This function visualizes the certain injury characteristics for study participants study injuries
#'
#' @param analytic This is the analytic data set that must include enrolled, injury_classification_ankle_ao, injury_at_work, injury_in_battle, 
#' injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type
#
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{

#' injury_characteristics_by_alternate_constructs()
#' }
injury_characteristics_by_alternate_constructs <- function(analytic){
  df <- analytic %>% 
    select(enrolled, injury_classification_ankle_ao, injury_at_work, injury_in_battle, 
           injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type) %>% 
    filter(enrolled)
  
  total <- sum(df$enrolled)
  
  type_df <- df %>% 
    mutate(injury_type = replace_na(injury_type, "Missing")) %>% 
    group_by(injury_type) %>% 
    count(injury_type) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = injury_type) %>% 
    select(-n) %>% 
    arrange(factor(type, levels = c('Blunt', 'Penetrating', 'Missing')))
  
  work_df <- df %>% 
    mutate(injury_at_work = as.character(injury_at_work)) %>% 
    mutate(injury_at_work = replace_na(injury_at_work, "Missing")) %>% 
    count(injury_at_work) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = injury_at_work) %>% 
    select(-n) %>% 
    mutate(type = case_when(
      type == TRUE  ~ "Yes",
      type == FALSE ~ "No",
      type == 'Missing' ~ 'Missing')) %>%  
    arrange(factor(type, levels = c('Yes', 'No', 'Missing')))
  
  battle_df <- df %>% 
    mutate(injury_in_battle = as.character(injury_in_battle)) %>% 
    mutate(injury_in_battle = replace_na(injury_in_battle, "Missing")) %>% 
    group_by(injury_in_battle) %>% 
    count(injury_in_battle) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = injury_in_battle) %>% 
    select(-n) %>% 
    mutate(type = case_when(
      type == TRUE  ~ "Yes",
      type == FALSE ~ "No",
      type == 'Missing' ~ 'Missing')) %>% 
    arrange(factor(type, levels = c('Yes', 'No', 'Missing')))
  
  blast_df <- df %>% 
    mutate(injury_in_blast = as.character(injury_in_blast)) %>% 
    mutate(injury_in_blast = replace_na(injury_in_blast, "Missing")) %>% 
    group_by(injury_in_blast) %>% 
    count(injury_in_blast) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = injury_in_blast) %>% 
    select(-n) %>% 
    arrange(factor(type, levels = c('Yes', 'No', 'Missing')))
  
  side_df <- df %>% 
    mutate(injury_side = as.character(injury_side)) %>% 
    mutate(injury_side = replace_na(injury_side, "Missing")) %>% 
    group_by(injury_side) %>% 
    count(injury_side) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = injury_side) %>% 
    select(-n) %>% 
    arrange(factor(type, levels = c('Left', 'Right', 'Missing')))
  
  tscherne_df <- df %>% 
    mutate(injury_classification_tscherne = as.character(injury_classification_tscherne)) %>% 
    mutate(injury_classification_tscherne = replace_na(injury_classification_tscherne, "Missing")) %>% 
    group_by(injury_classification_tscherne) %>% 
    count(injury_classification_tscherne) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = injury_classification_tscherne) %>% 
    select(-n)
  
  ao_df <- df %>% 
    mutate(injury_classification_ankle_ao = as.character(injury_classification_ankle_ao)) %>% 
    mutate(injury_classification_ankle_ao = replace_na(injury_classification_ankle_ao, "Missing")) %>% 
    group_by(injury_classification_ankle_ao) %>% 
    count(injury_classification_ankle_ao) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = injury_classification_ankle_ao) %>% 
    select(-n)
  
  df_final <- rbind(type_df, work_df, battle_df, blast_df, side_df, tscherne_df, ao_df) %>% 
    mutate_all(replace_na, "0 (0%)") 
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  
  vis <- kable(df_final, align='l', padding='2l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Type of Injury' = nrow(type_df), 'Work Related Injury' = nrow(work_df), 'Battlefield Injury' = nrow(battle_df), 
                        'Blast Injury' = nrow(blast_df), 'Side of Study Injury' = nrow(side_df), 
                        'Tscherne Classification' = nrow(tscherne_df), 'AO Classification' = nrow(ao_df)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}
#######################################################################################
#' Number of Visits Expected, Completed, Missed, and out of Window
#'
#' @description This function visualizes the expected visits, and which of those are completed in what relative window, and which of those are missed
#'
#' @param analytic This is the analytic data set that must include enrolled, followup_complete_4wk_6wk, followup_due_4wk_6wk, followup_early_4wk_6wk, followup_incomplete_4wk_6wk, 
#' followup_late_4wk_6wk, followup_missing_4wk_6wk, followup_not_started_4wk_6wk, followup_ontime_4wk_6wk, 
#' followup_complete_7wk_9wk, followup_due_7wk_9wk, followup_early_7wk_9wk, followup_incomplete_7wk_9wk, 
#' followup_late_7wk_9wk, followup_missing_7wk_9wk, followup_not_started_7wk_9wk, followup_complete_12wk_16wk, 
#' followup_due_12wk_16wk, followup_early_12wk_16wk, followup_incomplete_12wk_16wk, followup_late_12wk_16wk, 
#' followup_missing_12wk_16wk, followup_not_started_12wk_16wk, followup_ontime_12wk_16wk, followup_complete_22wk_32wk, 
#' followup_due_22wk_32wk, followup_early_22wk_32wk, followup_incomplete_22wk_32wk, followup_late_22wk_32wk, 
#' followup_missing_22wk_32wk, followup_not_started_22wk_32wk, followup_ontime_22wk_32wk
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' expected_visits_by_followup_period()
#' }
expected_visits_by_followup_period <- function(analytic){
  df <- analytic %>% 
    select(study_id, enrolled, followup_complete_4wk_6wk, followup_due_4wk_6wk, followup_early_4wk_6wk, followup_incomplete_4wk_6wk, 
           followup_late_4wk_6wk, followup_missing_4wk_6wk, followup_not_started_4wk_6wk, followup_ontime_4wk_6wk, 
           followup_complete_7wk_9wk, followup_due_7wk_9wk, followup_early_7wk_9wk, followup_incomplete_7wk_9wk, 
           followup_late_7wk_9wk, followup_missing_7wk_9wk, followup_not_started_7wk_9wk, followup_complete_12wk_16wk, 
           followup_due_12wk_16wk, followup_early_12wk_16wk, followup_incomplete_12wk_16wk, followup_late_12wk_16wk, 
           followup_missing_12wk_16wk, followup_not_started_12wk_16wk, followup_ontime_12wk_16wk, followup_complete_22wk_32wk, 
           followup_due_22wk_32wk, followup_early_22wk_32wk, followup_incomplete_22wk_32wk, followup_late_22wk_32wk, 
           followup_missing_22wk_32wk, followup_not_started_22wk_32wk, followup_ontime_22wk_32wk) %>% 
    filter(enrolled) %>% 
    mutate_all(~ifelse(is.na(.), FALSE, .))
  
  
  expected <- df %>% 
    select(followup_due_4wk_6wk, followup_due_7wk_9wk, followup_due_12wk_16wk, followup_due_22wk_32wk) %>% 
    summarize('4-6 Weeks' = sum(followup_due_4wk_6wk), '7-9 Weeks' = sum(followup_due_7wk_9wk), 
              '12-16 Weeks' = sum(followup_due_12wk_16wk), '22-32 Weeks' = sum(followup_due_22wk_32wk))
  
  complete <- df %>% 
    select(followup_complete_4wk_6wk, followup_complete_7wk_9wk, followup_complete_12wk_16wk, followup_complete_22wk_32wk) %>% 
    summarize('4-6 Weeks' = sum(followup_complete_4wk_6wk), '7-9 Weeks' = sum(followup_complete_7wk_9wk), 
              '12-16 Weeks' = sum(followup_complete_12wk_16wk), '22-32 Weeks' = sum(followup_complete_22wk_32wk))
  
  early <- df %>% 
    select(followup_early_4wk_6wk, followup_early_7wk_9wk, followup_early_12wk_16wk, followup_early_22wk_32wk) %>% 
    summarize('4-6 Weeks' = sum(followup_early_4wk_6wk), '7-9 Weeks' = sum(followup_early_7wk_9wk), 
              '12-16 Weeks' = sum(followup_early_12wk_16wk), '22-32 Weeks' = sum(followup_early_22wk_32wk))
  
  late <- df %>% 
    select(followup_late_4wk_6wk, followup_late_7wk_9wk, followup_late_12wk_16wk, followup_late_22wk_32wk) %>% 
    summarize('4-6 Weeks' = sum(followup_late_4wk_6wk), '7-9 Weeks' = sum(followup_late_7wk_9wk), 
              '12-16 Weeks' = sum(followup_late_12wk_16wk), '22-32 Weeks' = sum(followup_late_22wk_32wk))
  
  not_started <- df %>% 
    select(followup_not_started_4wk_6wk, followup_not_started_7wk_9wk, followup_not_started_12wk_16wk, followup_not_started_22wk_32wk) %>% 
    summarize('4-6 Weeks' = sum(followup_not_started_4wk_6wk), '7-9 Weeks' = sum(followup_not_started_7wk_9wk), 
              '12-16 Weeks' = sum(followup_not_started_12wk_16wk), '22-32 Weeks' = sum(followup_not_started_22wk_32wk))
  
  missing <- df %>% 
    select(followup_missing_4wk_6wk, followup_missing_7wk_9wk, followup_missing_12wk_16wk, followup_missing_22wk_32wk) %>% 
    summarize('4-6 Weeks' = sum(followup_missing_4wk_6wk), '7-9 Weeks' = sum(followup_missing_7wk_9wk), 
              '12-16 Weeks' = sum(followup_missing_12wk_16wk), '22-32 Weeks' = sum(followup_missing_22wk_32wk))
  
  all <- rbind(expected, complete, early, late, not_started, missing)
  rownames(all) <- c('Expected (Due)', 'Complete', 'Early', 'Late', 'Not Started', 'Missing')
  
  four <- all[[1, "4-6 Weeks"]]
  seven <- all[[1, "7-9 Weeks"]]
  twelve <- all[[1, "12-16 Weeks"]]
  twentytwo <- all[[1, "22-32 Weeks"]]
  fourcomp <- all[[2, "4-6 Weeks"]]
  sevencomp <- all[[2, "7-9 Weeks"]]
  twelvecomp <- all[[2, "12-16 Weeks"]]
  twentytwocomp <- all[[2, "22-32 Weeks"]]
  
  
  final <- cbind(Status = rownames(all), all) %>% 
    mutate(`4-6 Weeks` = ifelse(Status %in% c('Complete', 'Not Started', 'Missing'), format_count_percent(`4-6 Weeks`, four), ifelse(Status %in% c('Early', 'Late'), format_count_percent(`4-6 Weeks`, fourcomp), `4-6 Weeks`))) %>% 
    mutate(`7-9 Weeks` = ifelse(Status %in% c('Complete', 'Not Started', 'Missing'), format_count_percent(`7-9 Weeks`, seven), ifelse(Status %in% c('Early', 'Late'), format_count_percent(`7-9 Weeks`, sevencomp), `7-9 Weeks`))) %>% 
    mutate(`12-16 Weeks` = ifelse(Status %in% c('Complete', 'Not Started', 'Missing'), format_count_percent(`12-16 Weeks`, twelve), ifelse(Status %in% c('Early', 'Late'), format_count_percent(`12-16 Weeks`, twelvecomp), `12-16 Weeks`))) %>% 
    mutate(`22-32 Weeks` = ifelse(Status %in% c('Complete', 'Not Started', 'Missing'), format_count_percent(`22-32 Weeks`, twentytwo), ifelse(Status %in% c('Early', 'Late'), format_count_percent(`22-32 Weeks`, twentytwocomp), `22-32 Weeks`)))
  
  vis <- kable(final, align = 'l', padding = '2l') %>%
    kable_styling("striped", full_width = F, position = "left") %>% 
    add_indent(c(3, 4)) %>% 
    row_spec(1, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(4, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(5, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(6, extra_css = "border-bottom: 1px solid")
  
  return(vis)
}

#' Amputations and Gustilo Injury Characteristics
#'
#' @description This function visualizes the injury characteristics for amputations and Gustilo Injury types for 
#' Sextant study
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' injury_gustilo_type, injury_amputation_status
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' amputations_and_gustilo_injury_characteristics()
#' }
amputations_and_gustilo_injury_characteristics <- function(analytic){
  
  pull <- analytic %>% 
    filter(enrolled) %>%
    select(injury_gustilo_type, injury_amputation_status)
  
  inj_gust <- pull %>%  
    select(injury_gustilo_type) %>% 
    mutate(injury_gustilo_type = strsplit(as.character(injury_gustilo_type), ";\\s*")) %>%
    unnest(injury_gustilo_type) %>%
    group_by(injury_gustilo_type) %>%
    summarise(count = n()) %>%
    mutate(injury_gustilo_type = coalesce(injury_gustilo_type, 'Unknown'))
  
  
  total <- inj_gust %>%
    mutate(count=as.numeric(count)) %>%
    pull(count) %>%
    sum() 
  
  out_gustilo <- inj_gust %>%
    mutate(count= format_count_percent(count, total))
  
  amputation_status <- pull %>% 
    select(injury_amputation_status) %>% 
    count(injury_amputation_status) %>%
    pivot_longer(-n) %>%
    mutate(value=ifelse(is.na(value), 'Unknown', value)) %>%
    select(-name) %>%
    rename(count = n, injury_gustilo_type = value)
  
  total_amputations <- amputation_status %>%
    pull(count) %>% 
    sum()
  
  out_amputations <- amputation_status %>%
    mutate(count= format_count_percent(count, total_amputations))
  
  n_amputations <- data.frame(
    count = as.character(total_amputations),
    injury_gustilo_type = "Amputation Status")
  
  n_gustilo <- data.frame(
    count = as.character(total),
    injury_gustilo_type = "Fracture Type")
  
  
  combined <- bind_rows(n_amputations, out_amputations,n_gustilo, out_gustilo) %>%
    relocate(count, .after=injury_gustilo_type) %>%
    rename('Fracture Type'=injury_gustilo_type)
  
  output<- kable(combined, align='l', padding='2l', col.names = NULL) %>%
    kable_styling("condensed", position = "left") %>%
    add_indent(positions = c(2,3,4,6,7,8,9,10,11,12)) %>% 
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(c(1,5), bold=T,hline_after = T)
  
  return(output)
}


#' Refusal reasons by each site
#'
#' @description This function visualizes the reasons of refusal, total refused and total screened by each site.
#'
#' @param analytic This is the analytic data set that must include facilitycode, screened, refused, refused_reason
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' refusal_reasons_by_site()
#' }
refusal_reasons_by_site <- function(analytic){
  
  df <- analytic %>% 
    select(facilitycode, screened, refused, refused_reason) %>% 
    filter(screened == TRUE) 
  
  screened_df <- df %>% select(facilitycode, screened) %>% 
    filter(screened) %>% 
    count(facilitycode, screened) %>% 
    rename(screen_n = n) %>% 
    select(facilitycode, screen_n)
  
  refused_df <- df %>% select(facilitycode, refused) %>% 
    filter(refused) %>% 
    count(facilitycode, refused) %>% 
    rename(refuse_n = n) %>% 
    select(facilitycode, refuse_n)
  
  reasons <- df %>%  select(facilitycode, refused_reason) %>% 
    count(facilitycode, refused_reason) %>% 
    filter(!is.na(refused_reason)) %>% 
    pivot_wider(names_from = refused_reason,
                values_from = n) 
  
  
  exclude_columns <- c("facilitycode", "screen_n", "refuse_n")
  
  
  df_final <- left_join(reasons, screened_df) %>% left_join(refused_df) %>% 
    mutate_all(~ ifelse(is.na(.), 0, .)) %>% 
    mutate(across(-one_of(exclude_columns),
                  ~ format_count_percent(.x, refuse_n),
                  .names = "{col}_percentage")) %>% 
    select(ends_with("_percentage"), one_of(exclude_columns)) %>% 
    rename_with(~ sub("_percentage$", "", .), ends_with("_percentage")) %>% 
    select(one_of(exclude_columns), everything()) %>%
    select(-contains("Other"), -contains("Unknown"), contains("Other"), contains("Unknown")) %>% 
    rename(`Screened, to date` = screen_n,
           `Refused, to date` = refuse_n, 
           `Clinical Site` = facilitycode)
  
  output <- kable(df_final, align='l', padding='2l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}

#' Other reason of refusal by each site
#'
#' @description This function visualizes list of each "Other" reason of refusal, total screened by each site.
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, screened_date, 
#' refused_reason_other
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' other_reason_refusal_by_site()
#' }
other_reason_refusal_by_site <- function(analytic){
  
  df1 <- analytic %>%  select(study_id, facilitycode, screened_date, refused_reason_other) %>% 
    filter(!is.na(refused_reason_other)) %>% 
    rename(`Clinical Site` = facilitycode,
           `Screened Date` = screened_date,
           `"Other" reason of refusal` = refused_reason_other,
           `Study_ID` = study_id)
  
  output <- kable(df1, align='l', padding='2l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}

#' Not enrolled for other reasons
#'
#' @description This function visualizes list of study_ids who were screened howevere were not enrolled for "Other"
#' reasons
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, able_to_participate, 
#' nonparticipation_text_given, constraint_noconsent, constraint_admin, constraint_othr, constraint_othr_txt, screened
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' not_enrolled_for_other_reasons()
#' }
not_enrolled_for_other_reasons <- function(analytic){
  
  df1 <- analytic %>%  select(study_id, facilitycode, able_to_participate, nonparticipation_text_given, 
                              constraint_noconsent, constraint_admin, constraint_othr, constraint_othr_txt, screened) %>% 
    filter(screened) %>% 
    filter(constraint_admin == TRUE | constraint_noconsent == TRUE | constraint_othr == TRUE | !is.na(nonparticipation_text_given)) %>% 
    select(-screened) %>% 
    mutate(constraint_noconsent = ifelse(constraint_noconsent, "Yes", "No")) %>% 
    mutate(constraint_admin = ifelse(constraint_admin, "Yes", "No")) %>% 
    mutate(constraint_othr = ifelse(constraint_othr, "Yes", "No")) %>% 
    rename(`Clinical Site` = facilitycode,
           `Able to participate` = able_to_participate,
           `Reason for nonparticpation` = nonparticipation_text_given,
           `Constraint: No consent given` = constraint_noconsent,
           `Constraint: Administrative reason` = constraint_admin,
           `Constraint: Other` = constraint_othr,
           `Other constraint reason` = constraint_othr_txt,
           `Study_ID` = study_id)
  
  
  output <- kable(df1, align='l', padding='2l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}


#' Treatment crossover and nonadherance
#'
#' @description This function visualizes the treatment crossover or any nonadherance occured during the Sextant 
#' study.
#'
#' @param analytic This is the analytic data set that must include facilitycode, eligible, enrolled, dwc_date, 
#' dwc_tobra, dwc_vanco
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' treatment_crossover_and_nonadherance()
#' }
treatment_crossover_and_nonadherance <- function(analytic){
  
  df <- analytic %>% select(facilitycode, eligible, enrolled, dwc_date, dwc_tobra, dwc_vanco) %>% 
    filter(eligible == TRUE & enrolled == TRUE) %>% 
    mutate(eligible_enrolled = ifelse(eligible == TRUE & enrolled == TRUE, TRUE, FALSE)) %>% 
    mutate(dwc_complete = ifelse(!is.na(dwc_date), TRUE, FALSE)) %>% 
    mutate(dwc_vanco_tobra = ifelse(dwc_vanco & dwc_tobra, TRUE, FALSE)) %>% 
    select(-eligible, -enrolled, -dwc_date, -dwc_tobra, -dwc_vanco) 
  
  
  df_2nd <- df %>% 
    group_by(facilitycode) %>% 
    summarize(elig_enr = sum(eligible_enrolled), total_dwc = sum(dwc_complete), treatment_completed = sum(dwc_vanco_tobra)) %>% 
    replace(., is.na(.), 0)
  
  numerical_columns <- df_2nd %>% select_if(is.numeric) %>% colnames()
  
  df_3rd <- df_2nd %>%
    summarise(across(all_of(numerical_columns), sum, na.rm = TRUE),
              facilitycode = "TOTAL")
  
  
  result <- bind_rows(df_2nd, df_3rd[nrow(df_3rd), ]) %>% 
    mutate(treatment_completed= format_count_percent(treatment_completed, total_dwc)) %>% 
    rename(`Clinical Site` = facilitycode,
           `Eligible and Enrolled` = elig_enr,
           `Definitive Wound Closure completed` = total_dwc,
           `Received treatment per protocol and assignment(% DWC complete)` = treatment_completed)
  
  output <- kable(result, align='l', padding='2l') %>%
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(nrow(result), bold = TRUE)
  
  return(output)
}

#' Characteristics Treatment
#'
#' @description This function visualizes definitive fixation by total df complete out of total enrolled, stages breakdown,
#' incisions broken down by plateau fractures and pilon fractures, and whether or not the study treatment adhered to protocol.
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, df_date, plat_df_surgical_incision, pil_df_surgical_incision, df_number_procedures, df_randomized_treatment
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' charcateristics_treatment()
#' }
characteristics_treatment <- function(analytic){
  
  df <- analytic %>% 
    select(study_id, enrolled, df_date, plat_df_surgical_incision, pil_df_surgical_incision, df_number_procedures, df_randomized_treatment) %>% 
    filter(enrolled)
  
  total <- sum(df$enrolled, na.rm=T)
  df_total <- sum(!is.na(df$df_date))
  
  df_complete <- data.frame(type = 'Patients with Definitive Fixation Data Complete', percentage = format_count_percent(df_total, total))
  
  plat <- sum(!is.na(df$plat_df_surgical_incision))
  pil <- sum(!is.na(df$pil_df_surgical_incision))
  
  avg_stages <- df %>% 
    filter(!is.na(df_date)) %>% 
    summarize(type = 'Mean Stages (SD)', percentage = format_mean_sd(df_number_procedures))
  
  stages <- df %>% 
    filter(!is.na(df_date)) %>% 
    count(df_number_procedures) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, df_total)) %>% 
    select(-number) %>% 
    rename(type = df_number_procedures) 
  
  plat_incisions <- df %>%
    filter(!is.na(df_date)) %>% 
    mutate(plat_incisions = str_count(plat_df_surgical_incision, ";") + 1) %>% 
    summarize(type = paste0('Plateau Fractures (n = ', plat, ")"), percentage = format_mean_sd(plat_incisions))
  
  pil_incisions <- df %>%
    filter(!is.na(df_date)) %>% 
    mutate(pil_incisions = str_count(pil_df_surgical_incision, ";") + 1) %>% 
    summarize(type = paste0('Pilon Fractures (n = ', pil, ")"), percentage = format_mean_sd(pil_incisions))
  
  adherance <- df %>% 
    filter(!is.na(df_date)) %>% 
    mutate(df_randomized_treatment = as.character(df_randomized_treatment)) %>%
    mutate(df_randomized_treatment = replace_na(df_randomized_treatment, 'Missing')) %>% 
    mutate(type = recode(df_randomized_treatment, 'TRUE' = "Yes", 
                         'FALSE' = 'No')) %>% 
    count(type) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, df_total)) %>% 
    select(-number) %>% 
    arrange(factor(type, levels = c('Yes', 'No', 'Missing')))
  
  df_final <- rbind(df_complete, avg_stages, stages, plat_incisions, pil_incisions, adherance)
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  n_df <- nrow(df_complete)
  n_avg <- nrow(avg_stages)
  n_stages <- nrow(stages)
  
  vis <- kable(df_final, align='l', padding='2l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c(" " = nrow(df_complete), 'Definitive Fixation' = (nrow(avg_stages) + nrow(stages)), 'Number of Incisions [Mean (SD)]' = (nrow(pil_incisions) + nrow(plat_incisions)),
                        'Study Treatment Adhering to Protocol' = nrow(adherance)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1, extra_css = 'border-bottom: 1px solid') %>% 
    add_indent(seq(n_stages) + n_avg + n_df)
  
  return(vis)
}


#' Fracture Characteristics
#'
#' @description This function visualizes fracture characteristics, broken down by tibial plateau or pilon, 
#' and then closed or open fracture with tscherne grades and gustilo types respectively
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, fracture_type, injury_gustilo,
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' fracture_characteristics()
#' }
fracture_characteristics <- function(analytic){
  
  df <- analytic %>% select(study_id, enrolled, fracture_type, injury_gustilo, injury_classification_tscherne) %>% 
    filter(enrolled) %>% 
    mutate(closed = ifelse(!is.na(injury_classification_tscherne), TRUE, FALSE)) %>% 
    mutate(open = ifelse(!is.na(injury_gustilo), TRUE, FALSE))
  
  total <- sum(df$enrolled)
  closed_total <- sum(df$closed)
  open_total <- sum(df$open)
  
  closed <- data.frame(type = 'Closed Fracture', percentage = format_count_percent(closed_total, total))
  open <- data.frame(type = 'Open Fracture', percentage = format_count_percent(open_total, total))
  
  fracture_type <- df %>% 
    mutate(fracture_type = replace_na(fracture_type, "Unknown")) %>% 
    group_by(fracture_type) %>% 
    count(fracture_type) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    rename(type = fracture_type) %>% 
    select(-n) %>% 
    arrange(factor(type, levels = c('Tibial Plateau', 'Tibial Pilon', 'Unknown')))
  
  tscherne <- df %>% 
    filter(closed) %>% 
    group_by(injury_classification_tscherne) %>% 
    mutate(injury_classification_tscherne = recode(injury_classification_tscherne, 'C0' ='Tscherne Grade 0',
                                                   'CI' = 'Tscherne Grade 1',
                                                   'CII' = 'Tscherne Grade 2',
                                                   'CIII' = 'Tscherne Grade 3')) %>% 
    count(injury_classification_tscherne) %>% 
    mutate(percentage = format_count_percent(n, closed_total)) %>% 
    rename(type = injury_classification_tscherne) %>% 
    select(-n) %>% 
    arrange(factor(type, levels = c("Tscherne Grade 0","Tscherne Grade 1","Tscherne Grade 2","Tscherne Grade 3","N/A (low velocity GSW)")))
  
  gustilo <- df %>% 
    filter(open) %>% 
    group_by(injury_gustilo) %>% 
    mutate(injury_gustilo = recode(injury_gustilo, 'I' = 'Gustilo Type I',
                                   'II' = 'Gustilo Type II',
                                   'IIIA' = 'Gustilo Type IIIa')) %>% 
    count(injury_gustilo) %>% 
    mutate(percentage = format_count_percent(n, open_total)) %>% 
    rename(type = injury_gustilo) %>% 
    select(-n) %>% 
    arrange(factor(type, levels = c('I' = 'Gustilo Type I','II' = 'Gustilo Type II','III' = 'Gustilo Type IIIa')))
  
  df_final <- rbind(fracture_type, closed, tscherne, open, gustilo) 
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  n_closed <- nrow(closed)
  n_open <- nrow(open)
  n_frac <- nrow(fracture_type)
  n_tscherne <- nrow(tscherne)
  n_gustilo <- nrow(gustilo)
  
  
  vis <- kable(df_final, align='l', padding='2l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Fractured Bone' = nrow(fracture_type), 'Fracture Type' = (nrow(closed) + nrow(tscherne) + nrow(open) + nrow(gustilo))), label_row_css = "text-align:left") %>%
    add_indent(c(seq(n_tscherne) + n_frac + n_closed, seq(n_gustilo) + n_frac + n_closed + n_open + n_tscherne)) %>% 
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}


#' Treatment characteristics_sextant
#'
#' @description This function visualizes the treatment characteristics per protocol and assignmnet for Sextant. 
#'
#' @param analytic This is the analytic data set that must include local_antibiotic_at_dwc,
#' systemic_antibiotic_post_dwc, no_other_antibiotic_at_dwc
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' treatment_characteristics_sextant()
#' }
treatment_characteristics_sextant <- function(analytic){
  
  df <- analytic %>% 
    select(local_antibiotic_at_dwc,
           systemic_antibiotic_post_dwc,
           no_other_antibiotic_at_dwc)
  result <- df %>%
    group_by(local_antibiotic_at_dwc, systemic_antibiotic_post_dwc, no_other_antibiotic_at_dwc) %>%
    summarize(count = n()) %>%
    filter(!is.na(local_antibiotic_at_dwc) & !is.na(systemic_antibiotic_post_dwc) & !is.na(no_other_antibiotic_at_dwc)) %>% 
    arrange(desc(count)) 
  
  total <- result %>%
    pull(count) %>% 
    sum()
  
  df <- result %>% 
    mutate(count = format_count_percent(count, total)) %>% 
    mutate_at(vars(-count), funs(ifelse(. == TRUE, "Adherent", "Not Adherent"))) %>% 
    rename(`Local antibiotic treatment at DWC` = local_antibiotic_at_dwc,
           `Systemic antibiotic treatment post DWC`= systemic_antibiotic_post_dwc,
           `No other local antibiotic use at DWC` = no_other_antibiotic_at_dwc,
           `Overall` = count) 
  
  colnames(df)[which(names(df) == "Overall")] <- paste("Overall (n =", total, ")", sep = " ")
  
  output <- kable(df, align='l', padding='2l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}

