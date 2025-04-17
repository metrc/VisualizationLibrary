#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description 
#' Visualizes the count of the study statuses for each site, as well as displaying the days the site 
#' has been certified.
#' 
#' For other enrollment by site visualizations that may better fit your study, please look at: enrollment_status_by_site_var_discontinued, 
#' enrollment_by_site_last_days_var_disc, enrollment_status_by_site
#'
#' @param analytic analytic data set that must include screened, eligible, refused, consented, enrolled, 
#' not_consented, discontinued_pre_randomization, site_certified_days, facilitycode, late_ineligible
#'
#' @return html table
#' @export
#'
#' @examples
#' enrollment_status_by_site("Replace with Analytic Tibble")
#' 
enrollment_status_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('screened', 'eligible', 'refused', 'consented', 'enrolled', 'not_consented', 
                           'discontinued_pre_randomization', 'site_certified_days', 
                           'facilitycode', 'late_ineligible'), 
    example_types = c('Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Number',
                      'FacilityCode', 'Boolean'))

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
    rename("Discontinued Post-Randomization (late ineligible)" = `Late Ineligible`) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Consented)) %>% 
    rename("Enrolled & Eligible" = `Enrolled`) %>% 
    mutate(Consented = format_count_percent(Consented, Eligible)) %>% 
    mutate(Refused = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Enrolled for Other Reasons` = format_count_percent(`Not Enrolled for Other Reasons`, Eligible)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Screened))
  
  table<- kable(table_raw, format="html", align='l') %>%
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 3)) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}



#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled (Variable Discontinued)
#'
#' @description 
#' Visualizes the totals of each include construct by site, split into among eligible and among consented.
#' 
#' For other enrollment by site visualizations that may better fit your study, refer to: enrollment_by_site, 
#' enrollment_by_site_last_days_var_disc, enrollment_status_by_site, enrollment_status_by_site_var_discontinued
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, site_certified_days, facilitycode,
#' consent_date, not_randomized
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_status_by_site_var_discontinued("Replace with Analytic Tibble")
#' 
enrollment_status_by_site_var_discontinued <- function(analytic, discontinued="discontinued", 
                                                       discontinued_colname="Discontinued", only_total=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "refused", "consented", "enrolled", "randomized",
                           "not_consented", "site_certified_days", "facilitycode", "consent_date",
                           "not_randomized", "discontinued"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Date", "FacilityCode", "Date", "Boolean", "Boolean"))
  
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
    mutate(`Days Certified`=ifelse(is_total,"-",`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(!!discontinued_colname := format_count_percent(!!sym(discontinued_colname), Consented)) %>% 
    mutate(Randomized = format_count_percent(Randomized, Consented)) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Consented)) %>% 
    mutate(Consented = format_count_percent(Consented, Eligible)) %>% 
    mutate(Refused = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Consented` = format_count_percent(`Not Consented`, Eligible)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Screened))
  
  if(only_total){
    table_raw <- table_raw %>% filter(Facility=="Total")
  }
  
  table<- kable(table_raw, format="html", align='l') %>%
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 3)) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}



#' Ankle and Plateau X-Ray and Measurement Status
#'
#' @description 
#' This function visualizes Ankle and Plateau X-Ray and Measurement Status.
#'
#' @param analytic This is the analytic data set that must include followup_expected_6wk,followup_expected_3mo,followup_expected_6mo, 
#' followup_expected_12mo,injury_type,followup_data,radiographs_taken_6wk, 
#' radiographs_taken_3mo,radiographs_taken_6mo,plat_tib_fib_overlap_6mo, 
#' plat_sagittal_pl_alignment_6mo, plat_patella_centered_6mo, plat_medial_prox_tibia_deg_6mo, 
#' plat_medial_lateral_diff_6mo,plat_condylar_width_6mo,plat_art_step_off_medial_6mo, 
#' plat_art_step_off_lateral_6mo,plat_femur_tibia_deg_6mo,plat_tib_fib_overlap_3mo, 
#' plat_sagittal_pl_alignment_3mo,plat_patella_centered_3mo,plat_medial_prox_tibia_deg_3mo, 
#' plat_medial_lateral_diff_3mo,plat_condylar_width_3mo,plat_art_step_off_medial_3mo, 
#' plat_art_step_off_lateral_3mo,plat_femur_tibia_deg_3mo,plat_tib_fib_overlap_6wk, 
#' plat_sagittal_pl_alignment_6wk,plat_patella_centered_6wk,plat_medial_prox_tibia_deg_6wk, 
#' plat_medial_lateral_diff_6wk,plat_condylar_width_6wk,plat_art_step_off_medial_6wk, 
#' plat_art_step_off_lateral_6wk,plat_femur_tibia_deg_6wk,ankle_talar_tilt_degrees_6wk, 
#' ankle_sagital_disp_6wk,ankle_coronal_plane_disp_6wk,ankle_talar_tilt_degrees_3mo, 
#' ankle_sagital_disp_3mo,ankle_coronal_plane_disp_3mo,ankle_talar_tilt_degrees_6mo, 
#' ankle_sagital_disp_6mo,ankle_coronal_plane_disp_6mo
#' 
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' ankle_and_plateau_x_ray_and_measurement_status()
#' }
ankle_and_plateau_x_ray_and_measurement_status <- function(analytic){
  
  df1_ankle <- analytic %>% 
    filter(injury_type=="ankle") %>% 
    select(followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA') %>% 
    filter(form == 'Clinical Follow-up') %>% 
    mutate(status = ifelse(str_detect(status,"Complete"),"Complete",status)) %>% 
    group_by(form, followup_period, status) %>%
    count() %>%
    filter(!is.na(status)) %>%
    ungroup() %>% 
    mutate(name = recode(followup_period, 
                         "6 Week" = "6 Weeks", 
                         "3 Month" = "3 Months",
                         "6 Month" = "6 Months",
                         "12 Month" = "12 Months")) %>% 
    rename(value = status) %>% 
    select(name, n, value)
  
  
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
    reframe(value=c("Complete", "Incomplete", "Missed", "Expected")) %>% 
    ungroup()
  
  df_one_ankle <- left_join(df1_shell_ankle, bind_rows(df1_expected_ankle, df1_ankle)) %>% 
    mutate(value = factor(value, c("Expected", "Complete", "Incomplete", "Missed"))) %>% 
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
                                          ifelse(is.na(radiographs_taken_6wk),"Missed",radiographs_taken_6wk),NA)) %>% 
    mutate(radiographs_taken_3mo = ifelse(followup_expected_3mo,
                                          ifelse(is.na(radiographs_taken_3mo),"Missed",radiographs_taken_3mo),NA)) %>% 
    mutate(radiographs_taken_6mo = ifelse(followup_expected_6mo,
                                          ifelse(is.na(radiographs_taken_6mo),"Missed",radiographs_taken_6mo),NA)) %>% 
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
    mutate(value = factor(value, c("Yes", "No", "Missed", " "))) %>% 
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
  
  table_raw_ankle<- kable(df_ankle, format="html", align='l', col.names = str_replace(colnames(df_ankle),"^n.|^n"," ")) %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left')
  
  
  
  df1_plateau <- analytic %>% 
    filter(injury_type=="plateau") %>% 
    select(followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA') %>% 
    filter(form == 'Clinical Follow-up') %>% 
    mutate(status = ifelse(str_detect(status,"Complete"),"Complete",status)) %>% 
    group_by(form, followup_period, status) %>%
    count() %>%
    filter(!is.na(status)) %>%
    ungroup() %>% 
    mutate(name = recode(followup_period, 
                         "6 Week" = "6 Weeks", 
                         "3 Month" = "3 Months",
                         "6 Month" = "6 Months",
                         "12 Month" = "12 Months")) %>% 
    rename(value = status) %>% 
    select(name, n, value)
  
  
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
    reframe(value=c("Complete", "Incomplete", "Missed", "Expected")) %>% 
    ungroup()
  
  df_one_plateau <- left_join(df1_shell_plateau, bind_rows(df1_expected_plateau, df1_plateau)) %>% 
    mutate(value = factor(value, c("Expected", "Complete", "Incomplete", "Missed"))) %>% 
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
                                          ifelse(is.na(radiographs_taken_6wk),"Missed",radiographs_taken_6wk),NA)) %>% 
    mutate(radiographs_taken_3mo = ifelse(followup_expected_3mo,
                                          ifelse(is.na(radiographs_taken_3mo),"Missed",radiographs_taken_3mo),NA)) %>% 
    mutate(radiographs_taken_6mo = ifelse(followup_expected_6mo,
                                          ifelse(is.na(radiographs_taken_6mo),"Missed",radiographs_taken_6mo),NA)) %>% 
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
    reframe(value=c("Yes", "No", "Missed", " ")) %>% 
    ungroup()
  
  df_two_plateau <- left_join(df2_shell_plateau, df2_plateau) %>% 
    mutate(value = factor(value, c("Yes", "No", "Missed", " "))) %>% 
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
  
  table_raw_plateau<- kable(df_plateau, format="html", align='l', col.names = str_replace(colnames(df_plateau),"^n.|^n"," ")) %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left')
  
  output <- paste0("<h3>Ankle</h3><br />",table_raw_ankle, "<h3>Plateau</h3><br />",table_raw_plateau)
  
  return(output)
}


#' Injury characteristics for OTA classification and Schatzker Type injuries
#'
#' @description 
#' Counts the number of enrolled participants enrolled for each class of injury, seperated by Ankle 
#' and Plateau injury types. If injury is not classified, it is marked as "Missed." 
#'
#' @param analytic analytic data set that must include injury_type, injury_classification_ankle_ota, 
#' injury_classification_plat_schatzker, enrolled
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' injury_ankle_plateau_characteristics("Replace with Analytic Tibble")
#' 
injury_ankle_plateau_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c('injury_type', 'injury_classification_ankle_ota', 'injury_classification_plat_schatzker',
                           'enrolled'),
    example_types = c("NamedCategory['ankle' 'plateau']", 
                      "NamedCategory['44B2' '44B3' '44C1' '44C2' '44A2' '44C3' '44B1']",
                      "NamedCategory['Type IV' 'Type I']", "Boolean")) 
  
  df <- analytic %>% 
    select(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled) %>%  
    filter(enrolled == TRUE)
  
  summary_totals <- df %>%
    filter(injury_type == "plateau" & is.na(injury_classification_ankle_ota) | injury_type == "ankle" & 
             is.na(injury_classification_plat_schatzker)) %>%
    group_by(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    mutate(injury_classification_ankle_ota = ifelse(injury_type == "ankle" & is.na(injury_classification_ankle_ota) & 
                                                      is.na(injury_classification_plat_schatzker), 
                                                    "Missed", 
                                                    injury_classification_ankle_ota)) %>% 
    mutate(injury_classification_plat_schatzker = ifelse(injury_type == "plateau" & is.na(injury_classification_ankle_ota) & 
                                                           is.na(injury_classification_plat_schatzker), 
                                                         "Missed", 
                                                         injury_classification_plat_schatzker)) %>% 
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
  
  table_raw<- kable(df_table, format="html", align='l', col.names = NULL) %>%
    add_indent(c(seq(ota_number)+1, seq(schatzer_number)+1+ota_number+1)) %>% 
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left")
  
  return(table_raw)
}

#' Baseline characteristics percent 
#'
#' @description 
#' Visualizes the categorical distribution of baseline characteristics 
#' sex, age, race, education, military, enrolled. See below as this is a generic visualization and includes meta construct for each of the 
#' analysis outputs. You may also specify the levels that these outputs have in the function call. 
#' Outputs two columns: type (sex, age, race, education, military), and their respective counts and percentages.
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
#' @return html table 
#' @export
#'
#' @examples
#' baseline_characteristics_percent("Replace with Analytic Tibble")
#' 
baseline_characteristics_percent <- function(analytic, sex="sex", race="ethnicity_race", education="education_level", military="military_status",
                                             sex_levels=c("Female","Male", "Missing"), 
                                             race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                             education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing"), 
                                             military_levels=c("Active Military", "Active Reserves", "Not Active Duty","Missing")){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("sex", "ethnicity_race", "education_level", 'military_status', "age", "age_group", 
                           "enrolled"),
    example_types = c("NamedCategory['Female' 'Male' 'Missing']", "NamedCategory['Non-Hispanic White' 'Non-Hispanic Black' 'Hispanic' 'Other' 'Missing']",
                      "NamedCategory['Less than High School' 'GED or High School Diploma' 'More than High School' 'Refused / Don't know' 'Missing']",
                      "NamedCategory['Active Military' 'Active Reserves' 'Not Active Duty' 'Missing']", "Number", "Category", "Boolean")) 
  
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
  
  vis <- kable(df_final, format="html", align='l',  col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Sex' = nrow(sex_df), 'Age' = (nrow(age_df) + nrow(age_group_df)), 'Race' = nrow(race_df), 
                        'Education' = nrow(education_df), 'Military' = nrow(military_df)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis) 
} 

#' Baseline characteristics percent (no military status)
#'
#' @description 
#' Visualizes the categorical distribution of values for the baseline characteristics sex, age, race, 
#' and education. This function, as opposed to baseline_characteristics_percent, does not return data
#' for any military construct. Also returns the distribution of the age_group construct. Only participants
#' who are enrolled will be counted.
#' 
#' For each characteristic, this function takes two parameters: the construct name parameter and the  
#' construct levels parameter. The construct name parameter specifies the name of the construct to use for  
#' the corresponding characteristic, while the construct levels parameter specifies the expected values for  
#' each characteristic. The construct levels parameters create the rows of an empty table that analytic  
#' data is full joined to, so if, for example, you expect the sex column to contain the values male,  
#' female, and missing and the analytic dataset you provide only has male and missing, the function will  
#' have a row of female with a count of 0. The construct levels parameter also creates the order of the
#' rows. Notably, the function will return values found in the analytic dataset that are not specified 
#' in the levels parameter.
#'
#' @param analytic analytic dataset that must include enrolled, age, age_group, and all the constructs
#' specified in the following construct name parameters
#' @param sex name of the construct of the sex characteristic, defaults to "sex"
#' @param race name of the construct of the race characteristic, defaults to "ethnicity_race"
#' @param education name of the construct of the education characteristic, defaults to "education_level"
#' @param sex_levels default values and orders for sex characteristic
#' @param race_levels default values and orders for race characteristic
#' @param education_levels default values and orders for education characteristic
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble")
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble", race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian / Pacific Islander", "Other", "Missing"))
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble", sex_levels=c("Male","Female", "Missing"))
#' 
baseline_characteristics_percent_nm <- function(analytic, sex="sex", race="ethnicity_race", education="education_level",
                                             sex_levels=c("Female","Male", "Missing"), 
                                             race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                             education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing")
                                             ){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("sex", "ethnicity_race", "education_level", "age", "age_group",
                           "enrolled"),
    example_types = c("NamedCategory['Female' 'Male' 'Missing']", "NamedCategory['Non-Hispanic White' 'Non-Hispanic Black' 'Hispanic' 'Other' 'Missing']",
                      "NamedCategory['Less than High School' 'GED or High School Diploma' 'More than High School' 'Refused / Don't know' 'Missing']",
                      "Number-U100", "Category-U5", "Boolean")) 
  
  constructs <- c(sex, race, education)
  
  sex_default <- tibble(type=sex_levels)
  race_default <- tibble(type=race_levels)
  education_default <- tibble(type=education_levels)
  
  
  df <- analytic %>% 
    select(enrolled, age_group, age, all_of(constructs)) %>% 
    filter(enrolled) %>% 
    rename(sex = !!sym(sex)) %>% 
    rename(race = !!sym(race)) %>% 
    rename(education = !!sym(education)) %>% 
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
  
  
  df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df) %>% 
    mutate_all(replace_na, "0 (0%)") 
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  
  vis <- kable(df_final, format="html", align='l',  col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Sex' = nrow(sex_df), 
                        'Age' = (nrow(age_df) + nrow(age_group_df)), 
                        'Race' = nrow(race_df), 
                        'Education' = nrow(education_df)), 
              label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left")  
  
  return(vis) 
} 





#' Number of Non-Completing Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of non-completions, not expected, and SAEs for only 
#' "enrolled" participants and Protocol Deviations by type for all the "consented" participants. 
#' 
#' Refer to not_complete_sae_deviation_by_type_auto_categories.
#'
#' @param analytic This is the analytic data set that must include enrolled, not_expected_reason, not_completed_reason,
#' protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative, sae_count, not_completed, sae_count
#'
#' @return html table
#' @export
#'
#' @examples
#' not_complete_sae_deviation_by_type("Replace with Analytic Tibble")
#' 
not_complete_sae_deviation_by_type <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "not_expected_reason", "not_completed_reason",
                           "protocol_deviation_screen_consent", "protocol_deviation_procedural",
                           "protocol_deviation_administrative", "sae_count", 
                           "not_completed", "consented"), 
    example_types = c("Boolean", "Category", "Category", "Category", "Category",
                      "Category", "Number", "Boolean", "Boolean"))
  
  total <- sum(analytic$enrolled, na.rm=T)
  not_completed_df <- analytic %>% 
    select(enrolled, not_completed_reason, not_completed) %>% 
    mutate(not_completed_reason = ifelse(not_completed, not_completed_reason, NA)) %>% 
    select(-not_completed) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_completed_reason) %>%
    rename(type=not_completed_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  not_completed_df_tot <- tibble(type="Not Completed", n=sum(not_completed_df$n))
  
  not_expected_df <- analytic %>% 
    select(enrolled, not_expected_reason) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_expected_reason) %>%
    rename(type=not_expected_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  not_expected_df_tot <- tibble(type="Not Expected", n=sum(not_expected_df$n))
  
  sae_df <- analytic %>% 
    select(study_id, enrolled, sae_count) %>% 
    filter(enrolled & sae_count>0) %>% 
    mutate(sae_count = "SAE") %>% 
    count(sae_count) %>%
    rename(type=sae_count) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  if (nrow(sae_df) == 0) {
    sae_df <- tibble(type = "SAE", n = 0)
  }
  
  consented <- sum(analytic$consented, na.rm = TRUE)
  consented_df <- tibble(type = " ", n = paste0("n=", consented, ' <sub>(Consented)</sub>'))
  
  deviation_sc_df <- analytic %>% 
    select(study_id, consented, protocol_deviation_screen_consent) %>% 
    separate_rows(protocol_deviation_screen_consent, sep=";") %>% 
    filter(consented == TRUE) %>% 
    count(protocol_deviation_screen_consent) %>%
    rename(type=protocol_deviation_screen_consent) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_p_df <- analytic %>% 
    select(study_id, consented, protocol_deviation_procedural) %>% 
    separate_rows(protocol_deviation_procedural, sep=";") %>% 
    filter(consented == TRUE) %>% 
    count(protocol_deviation_procedural) %>%
    rename(type=protocol_deviation_procedural) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_a_df <- analytic %>% 
    select(study_id, consented, protocol_deviation_administrative) %>% 
    separate_rows(protocol_deviation_administrative, sep=";") %>%
    filter(consented == TRUE) %>% 
    mutate(protocol_deviation_administrative = ifelse(grepl("^Other:", protocol_deviation_administrative), "Other", protocol_deviation_administrative)) %>% 
    count(protocol_deviation_administrative) %>%
    rename(type=protocol_deviation_administrative) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = str_replace(type,"Other: .+","Other")) %>% 
    mutate(type = as.character(type))
  
  deviation_sc_tot <- tibble(type="Screen and Consent",n=sum(deviation_sc_df$n))
  deviation_p_tot <- tibble(type="Procedural",n=sum(deviation_p_df$n))
  deviation_a_tot <- tibble(type="Administrative/Other",n=sum(deviation_a_df$n))
  deviation_df_tot <- tibble(type="Protocol Deviations",n=sum(deviation_sc_df$n)+sum(deviation_p_df$n)+sum(deviation_a_df$n))
  
  
  df_final_top <- rbind(not_completed_df_tot, not_completed_df, not_expected_df_tot, not_expected_df, sae_df, consented_df) %>% 
    mutate(n = ifelse(type == " ", n,
                      format_count_percent(n, total, decimals=2)))
  
  df_final_bottom <- rbind(deviation_df_tot, deviation_sc_tot, deviation_sc_df, deviation_p_tot, deviation_p_df, deviation_a_tot, deviation_a_df) %>% 
    mutate(n = ifelse(type == " ", n,
                      format_count_percent(n, consented, decimals=2)))
  
  df_final <- rbind(df_final_top, df_final_bottom)
  
  n_act <- if (exists("not_completed_df")) nrow(not_completed_df) else 0
  n_disc <- if (exists("not_expected_df")) nrow(not_expected_df) else 0
  n_dsc <- if (exists("deviation_sc_df")) nrow(deviation_sc_df) else 0
  n_dp <- if (exists("deviation_p_df")) nrow(deviation_p_df) else 0
  n_da <- if (exists("deviation_a_df")) nrow(deviation_a_df) else 0
  
  
  indents_vec <- vector()
  
  if (n_dsc > 0) {
    indents_vec <- c(indents_vec, 1 + n_act + 1 + n_disc + 1 + 1 + 1 + seq(n_dsc) + 1)
  }
  if (n_dp > 0) {
    indents_vec <- c(indents_vec, 1 + n_act + 1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + seq(n_dp) + 1)
  }
  if (n_da > 0) {
    indents_vec <- c(indents_vec, 1 + n_act + 1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + seq(n_da) + 1)
  }
  
  
  indents_vec <- indents_vec[indents_vec <= nrow(df_final)]
  
  first_indents_vec <- seq(1+n_dsc+1+n_dp+1+n_da) + 1 + n_act + 1 + n_disc + 1 + 1 + 1
  
  if(n_disc>0){
    first_indents_vec <- c(seq(n_disc) + 1 + n_act + 1, first_indents_vec)
  }

  if(n_act>0){
    first_indents_vec <- c(seq(n_act) + 1, first_indents_vec)
  }
  
  first_indents_vec <- first_indents_vec[!is.na(first_indents_vec)]
  
  
  vis <- kable(df_final, format = "html", align = 'l', col.names = c(" ", paste0("n=", total, ' <sub>(Enrolled)</sub>')), escape = FALSE) %>%
    add_indent(first_indents_vec) %>% 
    add_indent(indents_vec) %>%
    row_spec(0, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_act, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_act + 1 + n_disc, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_act + 1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_act + 1 + n_disc + 1 + 1, extra_css = "border-bottom: 1px solid; font-weight: bold") %>% 
    row_spec(1 + n_act + 1 + n_disc + 1 + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 1px solid") %>%
    kable_styling("striped", full_width = F, position = "left")
  
  return(vis)
}


#' Number of Non-Completing Participants, SAEs, and Protocol Deviations by type with AUTO Protocol Deviation 
#' Categorization
#'
#' @description 
#' Visualizes the number of non-completions, not expected, and SAE presences (multiple SAES add only 
#' one) for enrolled participants and Protocol Deviations by type for consented participants. Amongst 
#' enrolled, counts instances of presence of the construct not_completed_reason for Not Completed count, 
#' not_expected_reason for Not expected count, and sae_count not being 0 for SAE count. Protocol deviation 
#' counts are extracted from the protocol_deviation_full_data long file, where "Other. . ." values are 
#' truncated to "Other."
#' 
#' Categories of protocol deviations are separated by indentation.
#'
#' @param analytic analytic data set that must include enrolled, not_expected_reason, not_completed_reason, 
#' not_completed, protocol_deviation_full_data, sae_count, consented
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' not_complete_sae_deviation_by_type_auto_categories("Replace with Analytic Tibble")
#' 
not_complete_sae_deviation_by_type_auto_categories <- function(analytic, 
                                                               category_defaults=c("Safety","Informed Consent",
                                                                                   "Eligibility","Protocol Implementation",
                                                                                   "Other")){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('enrolled', "protocol_deviation_full_data", "not_expected_reason", 'not_completed', 
                           'not_completed_reason', 'sae_count', 'consented'), 
    example_types = c('Boolean', "(';new_row: ', '|')FacilityCode|Date|Category|Date|Category|Character", 'Category', 'Boolean',
                      'Category', 'Number', 'Boolean'))

  total <- sum(analytic$enrolled, na.rm=T)
  not_completed_df <- analytic %>% 
    select(enrolled, not_completed_reason, not_completed) %>% 
    mutate(not_completed_reason = ifelse(not_completed, not_completed_reason, NA)) %>% 
    select(-not_completed) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_completed_reason) %>%
    rename(type=not_completed_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  not_completed_df_tot <- tibble(type="Not Completed", n=sum(not_completed_df$n))
  
  not_expected_df <- analytic %>% 
    select(enrolled, not_expected_reason) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_expected_reason) %>%
    rename(type=not_expected_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  not_expected_df_tot <- tibble(type="Not Expected", n=sum(not_expected_df$n))
  
  sae_df <- analytic %>% 
    select(study_id, enrolled, sae_count) %>% 
    filter(enrolled & sae_count>0) %>% 
    mutate(sae_count = "SAE") %>% 
    count(sae_count) %>%
    rename(type=sae_count) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  if (nrow(sae_df) == 0) {
    sae_df <- tibble(type = "SAE", n = 0)
  }
  
  analytic <- analytic %>% 
    mutate(protocol_deviation_data = protocol_deviation_full_data)
  
  
  deviations_df <- analytic %>% 
    select(study_id, consented, protocol_deviation_data) %>% 
    separate_rows(protocol_deviation_data, sep = ";new_row: ") %>% 
    separate(protocol_deviation_data, into = c("facilitycode", "consent_date", "category", "deviation_date", "protocol_deviation", 
                                               "deviation_description"), sep='\\|') %>% 
    filter(consented & !is.na(protocol_deviation)) %>% 
    mutate(protocol_deviation = ifelse(str_detect(protocol_deviation,"^Other:"), "Other", protocol_deviation)) %>% 
    count(category, protocol_deviation) %>%
    rename(type=protocol_deviation) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = str_replace(type,"Other: .+","Other")) %>% 
    mutate(type = as.character(type))
  
  if(is.null(category_defaults)){
    category_defaults <- sort(unique(deviations_df$type))
  }
  if(is_empty(category_defaults)){
    category_defaults <- sort(unique(deviations_df$type))
  }
  
  deviation_df_tot <- tibble(type="Protocol Deviations",n=sum(deviations_df$n))
  
  consented <- sum(analytic$consented, na.rm = TRUE)
  consented_df <- tibble(type = " ", n = paste0("n=", consented, ' <sub>(Consented)</sub>'))
  
  df_final_top <- rbind(not_completed_df_tot, not_completed_df, not_expected_df_tot, not_expected_df, sae_df, consented_df) 
  df_final_bottom <- rbind(deviation_df_tot) 
  
  n_act <- if (exists("not_completed_df")) nrow(not_completed_df) else 0
  n_disc <- if (exists("not_expected_df")) nrow(not_expected_df) else 0
  
  indents_vec <- vector(mode="integer")
  
  indents_offset <- 2
  
  if(n_act>0){
    indents_vec <- c(indents_offset+seq(n_act))
    indents_offset <- indents_offset+n_act
  }
  
  indents_offset <-indents_offset+1
  
  if(n_disc>0){
    indents_vec <- c(indents_offset+seq(n_act))
    indents_offset <- indents_offset+n_disc
  }
  
  indents_offset <-indents_offset+1+1
  second_vec <- vector(mode="integer")
  second_offset <- indents_offset
  
  for(category_i in category_defaults) {
    category_df <- deviations_df %>% 
      filter(category==category_i) %>% 
      select(-category)
    
    tot_df <- tibble(type=category_i,n=sum(category_df$n))
    
    df_final_bottom <- rbind(df_final_bottom, tot_df, category_df)
    indents_vec <- c(indents_vec,indents_offset+1)
    indents_offset <-indents_offset+1
    second_offset <-second_offset+1
    
    if(nrow(category_df)>0){
      indents_vec <- c(indents_vec,indents_offset+seq(nrow(category_df)))
      indents_offset <-indents_offset+1
      second_vec <- c(second_vec,second_offset+seq(nrow(category_df)))
      second_offset <-second_offset+1
    }
  }
  
  df_final_top <- df_final_top %>% 
    mutate(n = ifelse(type == " ", n,
                      format_count_percent(n, total, decimals=2)))
  
  df_final_bottom <- df_final_bottom %>% 
    mutate(n = ifelse(type == " ", n,
                      format_count_percent(n, consented, decimals=2)))
  
  df_final <- rbind(df_final_top, df_final_bottom)
  
  if(is_empty(second_vec)){
    vis <- kable(df_final, format = "html", align = 'l', col.names = c(" ", paste0("n=", total, ' <sub>(Enrolled)</sub>')), escape = FALSE) %>%
      add_indent(indents_vec) %>% 
      row_spec(0, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1 + 1, extra_css = "border-bottom: 1px solid; font-weight: bold") %>% 
      row_spec(nrow(df_final), extra_css = "border-bottom: 1px solid") %>% 
      kable_styling("striped", full_width = F, position = "left")
  } else{
    vis <- kable(df_final, format = "html", align = 'l', col.names = c(" ", paste0("n=", total, ' <sub>(Enrolled)</sub>')), escape = FALSE) %>%
      add_indent(indents_vec) %>% 
      add_indent(second_vec) %>%
      row_spec(0, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1 + 1, extra_css = "border-bottom: 1px solid; font-weight: bold") %>% 
      row_spec(nrow(df_final), extra_css = "border-bottom: 1px solid") %>% 
      kable_styling("striped", full_width = F, position = "left")
  }

  return(vis)
}


#' Number of Adjudications and Discontinuations by type
#'
#' @description 
#' This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type.
#' This was originally made for NSAID.
#'
#' @param analytic This is the analytic data set that must include screened, inappropriate_enrollment, 
#' late_ineligible, late_refusal, withdrawn_patient, withdrawn_physician, adjudication_pending, 
#' dead, sae_count, protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative,
#' study_discontinuation
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' adjudications_and_discontinuations_by_type("Replace with Analytic Tibble")
#' 
adjudications_and_discontinuations_by_type <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('screened', 'inappropriate_enrollment', 'late_ineligible', 'late_refusal', 
                           'withdrawn_patient', 'withdrawn_physician', 'dead', 'sae_count', 'protocol_deviation_screen_consent', 
                           'protocol_deviation_procedural', 'protocol_deviation_administrative'), 
    example_types = c('Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Number-U5', 
                      'Category', 'Category', 'Category'))
  
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
  
  
  vis <- kable(df_final, format="html", align='l', col.names = NULL) %>%
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
#' @description 
#' Visualizes the counts of ineligibility reasons (NOT NUMBER OF INELIGIBLE PARTICIPANTS) by site. The 
#' function will display a user-specified number of reasons broken down, and then collate the rest into
#' an "Other Reasons" column. Included are two columns depicting numbers of screened and ineligible
#' study participants.
#' 
#' Compare with ineligibility_reasons_info.
#'
#' @param analytic analytic data set that must include facilitycode, screened, ineligible, ineligibility_reasons
#' @param pre_screened when pre_screened is TRUE then uses pre-screening constructs
#' @param n_top_reasons is by default set to 5 but in case there are less than 5 reasons then as many 
#' columns would be reflected in the ineligibility table as reasons exist.
#' @param only_total hides site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' ineligibility_by_reasons("Replace with Analytic Tibble")
#' ineligibility_by_reasons("Replace with Analytic Tibble", n_top_reasons = 3, only_total = TRUE)
#' 
ineligibility_by_reasons <- function(analytic, pre_screened = FALSE, n_top_reasons = 5, only_total=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "screened", "ineligible", 'ineligibility_reasons'), 
    example_types = c('FacilityCode', 'Boolean', 'Boolean', 'Category-NS'))
  
  if (pre_screened) { 
    analytic <- analytic %>% 
      mutate(ineligibility_reasons = pre_ineligibility_reasons) %>% 
      mutate(screened = pre_screened) %>% 
      mutate(ineligible = pre_ineligible)
  }
  
  data <- analytic %>%
    select(study_id, facilitycode, screened, ineligible, ineligibility_reasons) %>%
    filter(screened == TRUE) 
  
  df <- data %>%
    select(study_id, facilitycode, ineligibility_reasons) %>%
    mutate(ineligibility_reasons = as.character(ineligibility_reasons)) %>%  
    column_unzipper('ineligibility_reasons', sep = '; ')
  
  if (nrow(df) == 0) {
    output <- tibble(
      `Sites` = unique(analytic$facilitycode),  
      `No participant ineligible` = NA_character_
    )
    
    vis <- kable(output, format = "html", align = 'l') %>%
      kable_styling("striped", full_width = FALSE, position = "left")
  }
  
  if (nrow(df) > 0 ) {
    
    reason_data <- df %>%  
      boolean_column_counter() %>% 
      pivot_longer(everything()) %>% 
      arrange(desc(value)) %>% 
      filter(name != 'Other')
    
    n_reasons <- nrow(reason_data)
    n_top_reasons <- if (n_reasons >= n_top_reasons) n_top_reasons else max(1, n_reasons)
    
    reasons <- reason_data %>%
      slice(1:n_top_reasons) %>%
      pull(name)
    
    screened_total <- data %>% 
      select(study_id, screened, ineligible) %>% 
      boolean_column_counter() %>%
      mutate(Site = 'Total')
    
    total <- data %>%
      column_unzipper('ineligibility_reasons', sep = '; ') %>%
      boolean_column_counter() %>%
      mutate(otherreasons = rowSums(across(-c(all_of(reasons), screened, ineligible)))) %>% 
      select(-screened, -ineligible) %>%
      mutate(Site = 'Total') %>%
      left_join(screened_total) %>%
      select(Site, screened, ineligible, all_of(reasons), otherreasons)
    
    screened_total_sites <- data %>%
      select(facilitycode, screened, ineligible) %>%
      boolean_column_counter(groups = 'facilitycode') %>%
      rename(Site = facilitycode)
    
    sites <- data %>%
      column_unzipper('ineligibility_reasons', sep = '; ') %>%
      boolean_column_counter(groups = 'facilitycode') %>%
      mutate(otherreasons = rowSums(across(-c(all_of(reasons), screened, ineligible, facilitycode)))) %>%
      rename(Site = facilitycode) %>%
      select(-screened, -ineligible) %>%
      left_join(screened_total_sites) %>%
      select(Site, screened, ineligible, all_of(reasons), otherreasons)
    
    output <- bind_rows(total, sites) %>% 
      rename(Screened = screened,
             Ineligible = ineligible,
             `Other Reasons` = otherreasons) %>% 
      arrange(desc(Screened)) %>% 
      mutate(Ineligible = format_count_percent(Ineligible, Screened)) %>%
      mutate(across(4:(n_top_reasons+3), ~ format_count_percent(.x, Screened)))
    
    if(pre_screened){
      output <- output %>% 
        rename("Pre-Screened" = Screened,
               "Pre-Ineligible" = Ineligible)
    }
    
    top_n_header_text <- paste0("Top ", n_top_reasons, " Ineligibility Reasons")
    
    header_names <- c(" " = 3, top_n_header_text = n_top_reasons, " " = 1)
    
    names(header_names)[2] <- top_n_header_text
    
    if(only_total){
      output <- output %>% filter(Site=="Total")
    }
    
    vis <- kable(output, format = "html", align = 'l') %>%
      add_header_above(header_names) %>%
      kable_styling("striped", full_width = FALSE, position = "left")
  }
  
  return(vis)
}


#' Status of IRB approvals and certification by site
#'
#' @description 
#' Visualizes the sites for a given study and their dates of 
#' local, DOD, and METRC certifications. This function outputs 5 columns, Facility, Local or sIRB approval date, DoD approval date, 
#' certified by MCC to start screening, Number of days certified. To run this visualization a study needs qa site_certified_date long file.
#'
#' @param analytic This is the analytic data set that must include site_certified_date
#' @param exclude_local_irb whether Local (or iSRB) column is in output, defaults to false
#'
#' @return html table
#' @export
#'
#' @examples
#' certification_date_data("Replace with Analytic Tibble")
#' certification_date_data("Replace with Analytic Tibble", exclude_local_irb = TRUE)
#' 
certification_date_data <- function(analytic, exclude_local_irb=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = "site_certified_date",
    example_types = "FacilityCode;Date;Date;Date;NamedCategory['2 days' '3 days' '4 days' '40 days']") 
  
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
  
  site_data <- rbind(site_data %>% filter(.[[5]]!="NA days"),site_data %>% filter(.[[5]]=="NA days"))
  
  if(exclude_local_irb){
    cols <- cols[-2] 
    site_data <- site_data %>% 
      select(all_of(cols))
  }
  
  site_data <- site_data %>% 
    arrange(`Certified by MCC to Start Screening`)
  
  vis <- kable(site_data, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}



#' Complications by severity and relatedness
#'
#' @description 
#' Visualizes the complication_data long file. Data is shown for each grade and type of complication,
#' as well as the number of study participants who experienced this complication (in brackets). If a
#' study is documenting unique or obscure complications that are not in the example table, then an update 
#' to this function is necessary for the study. The complications shown in the example table will always
#' be present, even if the proposed study is not tracking them.
#' 
#' Grade is determined by the severity column, with 2,1 being Mild or Moderate and 3, 4 being Severe
#' and Life-threatening, respectively. Notably, a fatal complication results in a grade Unknown.
#'
#' @param analytic analytic data set that must include complication_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' complications_by_severity_relatedness("Replace with Analytic Tibble")
#' 
complications_by_severity_relatedness <- function(analytic){
  analytic <- if_needed_generate_example_data(
    "Replace with Analytic Tibble",
    example_constructs = "complication_data",
    example_types = "(';new_row: ', '|')FollowupPeriod|Date|NamedCategory['Superficial-infection' 'Deep-Infection' 'Deep-Infection, Not Involving Bone' 'Deep-Infection, Septic Joint' 'Non-Union' 'Malunion' 'Loss of limb/amputation' 'Fixation failure' 'Peri-implant Fracture' 'Reaction to Hardware' 'Wound Dehiscence' 'Wound Seroma/Hematoma' 'Flap failure' 'Tendon Injury' 'Delayed Wound Healing' 'Cellulitis' 'DVT/PE' 'Joint Arthritis' 'Other' 'Other' 'Other' 'Other' 'Moderate' 'Mild' 'Life-threatening or disabling' 'Severe and Undesirable' 'Fatal']|Character|Character|Date|NamedCategory['Definitely related' 'Probably related' 'Possibly related' 'Unlikely related' 'Unrelated' 'Don't know']|NamedCategory['Moderate' 'Mild' 'Life-threatening or disabling' 'Severe and Undesirable' 'Fatal']|NamedCategory['Operative' 'Non-operative' 'No treatment']|NamedCategory['New' 'Previous']|Character") 
  
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
  table_raw<- kable(output, format="html", align='l') %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    pack_rows(index = subindex_vec,label_row_css = "text-align:left;padding-left: 2em;", bold = FALSE) %>% 
    row_spec(1, extra_css = "border-bottom: 1px solid") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(table_raw)
}



#' Nonunion surgery outcome
#'
#' @description 
#' Visualizes the checks at 3 Months and 12 Months across the count of Non-Union at those timepoints.
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' followup_expected_3mo, followup_expected_12mo, nonunion_90days,  nonunion_1yr
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' nonunion_surgery_outcome("Replace with Analytic Tibble")
#' 
nonunion_surgery_outcome <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "followup_expected_3mo", "followup_expected_12mo",
                           "nonunion_90days", "nonunion_1yr"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean"))
  
  df <- analytic %>% 
    select(enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_expected_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_expected_12mo))
  
  colname <- c("Enrolled", "Expected Three Month", "90 Day Non-Union", "Expected Twelve Month", "1 Year Non-Union")
  
  table<- kable(df, format="html", align='l', col.names = colname) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}




#' Injury Characteristics
#'
#' @description This function visualizes the certain injury characteristics for study participants study injuries, 
#' ross reference the potential usage of this visualization with injury_characteristics
#'
#' @param analytic This is the analytic data set that must include enrolled, injury_classification_ankle_ao, injury_at_work, injury_in_battle, 
#' injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type
#
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' injury_characteristics_by_alternate_constructs("Replace with Analytic Tibble")
#' 
injury_characteristics_by_alternate_constructs <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('injury_classification_ankle_ao', 'injury_at_work', 'injury_in_battle', 
                           'injury_in_blast', 'injury_date', 'injury_mechanism', 
                           'injury_side', 'injury_classification_tscherne', 'injury_type',
                           'enrolled'), 
    example_types = c('Category', 'Boolean', 'Boolean', "Boolean", 'Date', 'Category', 
                      'Category', 'Category', 'Category', 'Boolean'))
  
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
  
  
  vis <- kable(df_final, format="html", align='l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Type of Injury' = nrow(type_df), 'Work Related Injury' = nrow(work_df), 'Battlefield Injury' = nrow(battle_df), 
                        'Blast Injury' = nrow(blast_df), 'Side of Study Injury' = nrow(side_df), 
                        'Tscherne Classification' = nrow(tscherne_df), 'AO Classification' = nrow(ao_df)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}


#' Generic Characteristics
#'
#' @description 
#' Visualize basic count statistics for a number of constructs. Missing values are 
#' given the value "Missing."
#' 
#' For other relevant characteristics counting visualizations, please see: baseline_characteristics_percent, baseline_characteristics_percent_nm
#'
#' @param analytic This is the analytic data set 
#' @param constructs The constructs to run statistics from
#' @param names_vec The names of the constructs in the final visualization
#' @param filter_cols The columns to filter the the data by (for totals and missing counts)
#' @param titlecase Changes construct values to Title Case
#' @param splits Splits the constructs if they are lists like "test_one,test_two" into two rows then counts them
#' @param subcategory_constructs This allows a characteristic to have a construct as a sub category, 
#' must be empty or specify a subcategory construct (or NA) for each construct (length of constructs == length of subcategory_constructs)
#'
#' @return html table
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' generic_characteristics("Replace with Analytic Tibble", constructs="stages", names_vec="Stages")
#' }
generic_characteristics <- function(analytic, constructs = c(), names_vec = c(), 
                                    filter_cols = c("enrolled"), titlecase = FALSE, splits=NULL,
                                    subcategory_constructs = c()){

  out <- NULL
  index_vec <- c()
  sub_index_vec <- c()
  sub_bold_index_vec <- c()
  
  if(is.null(splits)){
    splits <- rep(NA, length(constructs))
  } else{
    if(length(splits) == 1) {
      splits <- rep(splits, length(constructs))
    }
  }
  
  if(is_empty(subcategory_constructs)){
    subcategory_constructs <- rep(NA, length(constructs))
  } else{
    if(length(subcategory_constructs) == 1) {
      subcategory_constructs <- rep(subcategory_constructs, length(constructs))
    }
  }
  
  for (i in seq(length(constructs))) {
    name_str <- names_vec[i]
    construct <- constructs[i]
    sub_construct <- subcategory_constructs[i]
    
    if (!is.null(filter_cols)){
      if(length(filter_cols) == 1) {
        inner_analytic <- analytic %>%
          filter(!!sym(filter_cols)) %>%
          select(study_id, all_of(c(constructs,subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]))
      } else {
        inner_analytic <- analytic %>%
          filter(!!sym(filter_cols[i])) %>%
          select(study_id, all_of(c(constructs,subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]))
      }
    }
    total <- nrow(inner_analytic)
    
    inner <- inner_analytic %>%
      mutate(temp = !!sym(construct)) %>% 
      mutate(temp =  replace_na(as.character(temp), "Missing"))
    
    if(!is.na(sub_construct)){
      inner <- inner %>% 
        mutate(sub_temp = !!sym(sub_construct)) %>% 
        mutate(sub_temp =  replace_na(as.character(sub_temp), "Missing"))
    }
    
    inner_split <- splits[i]

    if(!is.na(inner_split)){
      inner <- inner %>% 
        separate_rows(temp,sep = inner_split)
    }
    
    if(!is.na(sub_construct)){
      sub_cats <- sort(unique(inner$sub_temp))
      if("Missing" %in% sub_cats){
        sub_cats <- c(sub_cats[sub_cats != "Missing"],"Missing")
      }
      row_count <- ifelse(is.null(out),0,nrow(out))
      new_row_count <- 0
      for(sub_cat in sub_cats){
        category_df <- inner %>% 
          filter(sub_temp==sub_cat) %>% 
          select(-sub_temp) %>% 
          group_by(temp) %>% 
          count(temp)
        
        category_tot <- sum(category_df$n)
        category_df <- category_df %>% 
          mutate(percentage = format_count_percent(n,category_tot))
        tot_df <- tibble(temp=sub_cat,percentage=format_count_percent(category_tot, total),header=name_str)
        
        category_df <- category_df  %>% 
          select(-n) %>%
          mutate(header = name_str) %>%
          arrange(temp == "Missing")
        
        if (titlecase) {
          category_df <- category_df %>%
            mutate(temp = str_to_title(temp))
        }
        
        if (is.null(out)) {
          out <- bind_rows(tot_df, category_df)
        } else {
          out <- rbind(out, tot_df, category_df)
        }
        sub_bold_index_vec <- c(sub_bold_index_vec, row_count+1)
        sub_index_vec <- c(sub_index_vec, seq(nrow(category_df))+ row_count+1)
        row_count <- row_count + nrow(category_df) + 1
        new_row_count <- new_row_count + nrow(category_df) + 1
      }
      new <- new_row_count
      names(new) <- paste0(name_str, ' (n=', total, ')')
      index_vec <- c(index_vec, new)
    } else{
      inner <- inner %>% 
        group_by(temp) %>% 
        count(temp) %>% 
        mutate(percentage = format_count_percent(n, total)) %>% 
        select(-n) %>%
        mutate(header = name_str) %>%
        arrange(temp == "Missing")
      
      if (titlecase) {
        inner <- inner %>%
          mutate(temp = str_to_title(temp))
      }
      
      new <- nrow(inner)
      names(new) <- paste0(name_str, ' (n=', total, ')')
      index_vec <- c(index_vec, new)
      
      if (is.null(out)) {
        out <- inner
      } else {
        out <- rbind(out, inner)
      }
    }
  }
  out <- out %>%
    select(-header)
  
  if(is_empty(sub_bold_index_vec)){
    vis <- kable(out, format="html", align='l', col.names = c('', '')) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      row_spec(c(1, cumsum(index_vec[1: length(index_vec)-1])+1), extra_css = "border-top: 1px solid") %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
      kable_styling("striped", full_width = F, position="left")
  } else{
    vis <- kable(out, format="html", align='l', col.names = c('', '')) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      add_indent(sub_index_vec) %>% 
      row_spec(sub_bold_index_vec, bold = TRUE) %>% 
      row_spec(c(1, cumsum(index_vec[1: length(index_vec)-1])+1), extra_css = "border-top: 1px solid") %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
      kable_styling("striped", full_width = F, position="left")
  }
  return(vis)
}



#' Amputations and gustilo injury characteristics
#'
#' @description 
#' Visualizes the injury characteristics for the amputation status (by category), and the fracture type 
#' (by gustilo). In order for the indents to work properly, the injury_gustilo_type 
#' construct must contain 7 unique values and the injury_amputation_status construct must contain 3 
#' unique values. 
#'
#' @param analytic analytic data set that must include constructs enrolled, injury_gustilo_type, injury_amputation_status
#'
#' @return html table
#' @export
#'
#' @examples
#' amputations_and_gustilo_injury_characteristics("Replace with Analytic Tibble")
#' 
amputations_and_gustilo_injury_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c('enrolled', 'injury_gustilo_type', 'injury_amputation_status'),
    example_types = c('Boolean', 'Category-U7', 'Category-U3')) 
  
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
  
  output<- kable(combined, format="html", align='l', col.names = c(" ", " ")) %>%
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
#' @return An HTML table.
#' @export
#'
#' @examples
#' refusal_reasons_by_site("Replace with Analytic Tibble")
#' 
refusal_reasons_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "screened", "refused", 'refused_reason'), 
    example_types = c('FacilityCode', 'Boolean', 'Boolean', 'Category'))

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
  
  totals_reasons <- reasons %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    mutate(facilitycode = "Total")
  
  totals_screened <- screened_df %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    mutate(facilitycode = "Total")
  
  totals_refused <- refused_df %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    mutate(facilitycode = "Total")
  
  totals <- left_join(totals_reasons, totals_screened) %>% left_join(totals_refused)
  
  exclude_columns <- c("facilitycode", "screen_n", "refuse_n")
  
  
  df_final <- left_join(reasons, screened_df) %>% left_join(refused_df) %>% 
    mutate_all(~ ifelse(is.na(.), 0, .)) %>% 
    rbind(totals) %>% 
    arrange(ifelse(facilitycode == "Total", 0, 1)) %>% 
    mutate(across(-one_of(exclude_columns),
                  ~ format_count_percent(.x, refuse_n),
                  .names = "{col}_percentage")) %>% 
    select(ends_with("_percentage"), one_of(exclude_columns)) %>% 
    rename_with(~ sub("_percentage$", "", .), ends_with("_percentage")) %>% 
    select(one_of(exclude_columns), everything()) %>%
    arrange(desc(refuse_n)) %>% 
    select(-contains("Other"), -contains("Unknown"), contains("Other"), contains("Unknown")) %>% 
    rename(`Screened, to date` = screen_n,
           `Refused, to date` = refuse_n, 
           `Clinical Site` = facilitycode) 
  
  output <- kable(df_final, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(row = 1, bold = TRUE)
  
  return(output)
}

#' Other reason of refusal by each site
#'
#' @description 
#' Returns a table of descriptions of all of the "Other" reasons given for refusal to participate.
#' 
#' See also the complementary table: refusal_reasons_by_site
#'
#' @param analytic analytic data set that must include study_id, facilitycode, screened_date, 
#' refused_reason_other
#'
#' @return html table
#' @export
#'
#' @examples
#' other_reason_refusal_by_site("Replace with Analytic Tibble")
#' 
other_reason_refusal_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened_date", "refused_reason_other", "facilitycode"),
    example_types = c("Date", "Character", "FacilityCode")) 
  
  df1 <- analytic %>%  select(study_id, facilitycode, screened_date, refused_reason_other) %>% 
    filter(!is.na(refused_reason_other)) %>% 
    rename(`Clinical Site` = facilitycode,
           `Screened Date` = screened_date,
           `"Other" reason of refusal` = refused_reason_other,
           `Study_ID` = study_id)
  
  output <- kable(df1, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}

#' Not enrolled for other reasons
#'
#' @description Visualizes the list of study_ids who were screened however were not enrolled for reasons labeled 'other'.
#' 
#' See also the complementary table: not_enrolled_reason
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, able_to_participate, 
#' nonparticipation_text_given, constraint_noconsent, constraint_admin, constraint_other, constraint_other_txt, screened
#'
#' @return html table
#' @export
#'
#' @examples
#' not_enrolled_for_other_reasons("Replace with Analytic Tibble")
#' 
not_enrolled_for_other_reasons <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "able_to_participate", "nonparticipation_text_given", 
                           "constraint_noconsent", "constraint_admin", "constraint_other", 
                           "constraint_other_txt", "screened"), 
    example_types = c("FacilityCode", "Boolean", "Character", 
                      "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean"))
  
  df1 <- analytic %>%  select(study_id, facilitycode, able_to_participate, nonparticipation_text_given, 
                              constraint_noconsent, constraint_admin, constraint_other, constraint_other_txt, screened) %>% 
    filter(screened) %>% 
    filter(constraint_admin == TRUE | constraint_noconsent == TRUE | constraint_other == TRUE | !is.na(nonparticipation_text_given)) %>% 
    select(-screened) %>% 
    mutate(constraint_noconsent = ifelse(constraint_noconsent, "Yes", "No")) %>% 
    mutate(constraint_admin = ifelse(constraint_admin, "Yes", "No")) %>% 
    mutate(constraint_other = ifelse(constraint_other, "Yes", "No")) %>% 
    rename(`Clinical Site` = facilitycode,
           `Able to participate` = able_to_participate,
           `Reason for nonparticpation` = nonparticipation_text_given,
           `Constraint: No consent given` = constraint_noconsent,
           `Constraint: Administrative reason` = constraint_admin,
           `Constraint: Other` = constraint_other,
           `Other constraint reason` = constraint_other_txt,
           `Study_ID` = study_id)
  
  
  output <- kable(df1, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}


#' Fracture characteristics
#'
#' @description 
#' This function visualizes fracture characteristics, broken down by tibial plateau or pilon, 
#' and then closed or open fracture with tscherne grades and gustilo types respectively. Percentages
#' are frome within each type of fracture.
#'
#' @param analytic analytic data set that must include study_id, enrolled, fracture_type, injury_gustilo, 
#' injury_classification_tscherne
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' fracture_characteristics("Replace with Analytic Tibble")
#' 
fracture_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "fracture_type","injury_gustilo", "injury_classification_tscherne"),
    example_types = c("Boolean", "Category", "Category", "Category")) 
  
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
    separate_rows(fracture_type, sep = ";") %>% 
    group_by(fracture_type) %>%
    summarize(n = n()) %>%
    mutate(percentage = format_count_percent(n, sum(n))) %>%
    rename(type = fracture_type) %>%
    select(-n) %>%
    arrange(factor(type, levels = c('Tibial Plateau', 'Tibial Pilon', 'Tibial Shaft', 'Fibula', 'Unknown')))
  
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
  
  vis <- kable(df_final, format="html", align='l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Fractured Bone' = nrow(fracture_type), 'Fracture Type' = (nrow(closed) + nrow(tscherne) + nrow(open) + nrow(gustilo))), label_row_css = "text-align:left") %>%
    add_indent(c(seq(n_tscherne) + n_frac + n_closed, seq(n_gustilo) + n_frac + n_closed + n_open + n_tscherne)) %>% 
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}

#' enrollment_by_site tobra and sextant (var discontinued)
#'
#' @description 
#' Visualizes the number of subjects enrolled, not enrolled etc, with parameter specifications to include 
#' more columns. 
#' 
#' For other enrollment by site visualizations that may better fit your study, please look at: enrollment_by_site, 
#' enrollment_by_site_last_days_var_disc, enrollment_status_by_site, enrollment_status_by_site_var_discontinued
#'
#' @param analytic This is the analytic data set that must include screened, eligible, refused, not_consented, 
#' not_randomized, consented_and_randomized, enrolled, site_certified_days, facilitycode, screened_date, 
#' consented, randomized, consent_date, discontinued
#' @param days the number of last days to include in the last days summary section of the table
#' @param discontinued this is a meta construct where you can specify your discontinued construct like 
#' 'discontinued' or 'adjudicated_discontinued' (defaults to 'discontinued')
#' @param discontinued_colname this determines the label applied to the discontinued column of your choosing 
#' (defaults to 'Discontinued')
#' @param include_exclusive_safety_set this is a toggle that will include a exclusive_safety_set construct 
#' if you want it included (defaults to FALSE)
#' @param average if days argument is set to something other than 0, will return the average over the
#' time period specified for the length of the study
#' @param cumulative_data whether to include the final counts of the study statuses
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble")
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", days = 20, average = TRUE)
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", days = 20, average = FALSE)
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", discontinued_colname = 'HERE!')
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", average = TRUE)
#' print("Note this call does not work, as average is set to true and no days are specified")
#' 
enrollment_by_site_last_days_var_disc <- function(analytic, days = 0, 
                                                  discontinued="discontinued", 
                                                  discontinued_colname="Discontinued", 
                                                  include_exclusive_safety_set=FALSE, 
                                                  average = FALSE, 
                                                  cumulative_data = TRUE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "refused", "consented", "enrolled", "randomized",
                           "not_consented", "site_certified_days", "facilitycode", "consent_date",
                           "not_randomized", "discontinued", "consented_and_randomized", "screened_date", "exclusive_safety_set"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Date", "FacilityCode", "Date", "Boolean", "Boolean",
                      "Boolean", "Date", "Boolean"))
  
  if(include_exclusive_safety_set){
    df <- analytic %>% 
      select(screened, eligible, refused, not_consented, not_randomized, consented_and_randomized, enrolled, site_certified_days, 
             facilitycode, all_of(discontinued), screened_date, exclusive_safety_set)
  } else{
    df <- analytic %>% 
      select(screened, eligible, refused, not_consented, not_randomized, consented_and_randomized, enrolled, site_certified_days, 
             facilitycode, all_of(discontinued), screened_date)
  }
  
  colnames(df)[10] <- "discontinued"
  
  last_days <- Sys.Date() - days
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certified_days))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility)) %>% 
    mutate(weeks_site_certified = site_certified_days/7)
  
  if(include_exclusive_safety_set){
    df_1st <- df %>% 
      group_by(Facility) %>% 
      summarize('Days Certified' = site_certified_days[1], 
                Screened = sum(screened), Eligible = sum(eligible), 
                Refused = sum(refused[eligible == TRUE]), 
                'Not Consented' = sum(not_consented[eligible == TRUE]), 
                cnr = sum(consented_and_randomized[eligible == TRUE])) 
    
    df_2nd <- df %>% 
      group_by(Facility) %>% 
      summarize('Discontinued' = sum(discontinued[eligible == TRUE & consented_and_randomized == TRUE]), 
                "Enrolled" = sum(enrolled[eligible == TRUE & consented_and_randomized == TRUE]), 
                'Safety Set' = sum(exclusive_safety_set[eligible == TRUE & consented_and_randomized == TRUE])) %>% 
      select(Facility, Discontinued, Enrolled, `Safety Set`)
    
  } else{
    df_1st <- df %>% 
      group_by(Facility) %>% 
      summarize('Days Certified' = site_certified_days[1], 
                Screened = sum(screened), Eligible = sum(eligible), 
                Refused = sum(refused[eligible == TRUE]), 
                'Not Consented' = sum(not_consented[eligible == TRUE]), 
                cnr = sum(consented_and_randomized[eligible == TRUE])) 
    
    df_2nd <- df %>% 
      group_by(Facility) %>% 
      summarize('Discontinued' = sum(discontinued[eligible == TRUE & consented_and_randomized == TRUE]), 
                "Enrolled" = sum(enrolled[eligible == TRUE & consented_and_randomized == TRUE])) %>% 
      select(Facility, Discontinued, Enrolled)
  }
  
  table_raw <- left_join(df_1st, df_2nd, by = 'Facility')
  
  facilities <- df %>% 
    select(Facility) %>% 
    unique()
  
  last_day_df <- facilities
  
  for(last_day in last_days){
    new_last_day_df <- df %>% 
      mutate(screened_date = as.Date(screened_date)) %>% 
      mutate(screened_last = ifelse(screened_date > last_day, TRUE, FALSE)) %>% 
      mutate(eligible_last = ifelse(screened_last, eligible, FALSE)) %>% 
      mutate(enrolled_last = ifelse(screened_last, enrolled, FALSE)) %>% 
      select(Facility, screened_last, eligible_last, enrolled_last) %>% 
      group_by(Facility) %>% 
      summarize('last_days_Screened' = sum(screened_last, na.rm = T),
                'last_days_Eligible' = sum(eligible_last, na.rm = T),
                'last_days_Enrolled' = sum(enrolled_last, na.rm = T))
    
    last_day_df <- left_join(last_day_df, new_last_day_df, by = 'Facility')
  }
  
  by_week <- df %>%
    filter(!is.na(weeks_site_certified)) %>% 
    select(Facility, screened, enrolled, weeks_site_certified) %>% 
    group_by(Facility) %>% 
    summarize(
      Screened2 = round(sum(screened, na.rm = TRUE) / first(weeks_site_certified), 2),
      Enrolled2 = round(sum(enrolled, na.rm = TRUE) / first(weeks_site_certified), 2))
  
  weekly <- left_join(facilities, by_week, by = 'Facility')
  
  almost <- left_join(last_day_df, weekly, by = 'Facility')
  
  sum_days_certified <- sum(table_raw$`Days Certified`, na.rm=T)
  
  final <- left_join(almost, table_raw, by = 'Facility') %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,sum_days_certified,`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(across(starts_with(c("last_days_Eligible", "last_days_Enrolled")), 
      ~ format_count_percent(.x, 
                             get(str_replace(cur_column(), 
                                             "^(last_days_Eligible|last_days_Enrolled)(.*)$", 
                                             "last_days_Screened\\2"))))) %>% 
    mutate(`Discontinued (% randomized)` = format_count_percent(Discontinued, cnr)) %>% 
    mutate(`Eligible & Enrolled (% randomized)` = format_count_percent(Enrolled, cnr)) %>% 
    mutate(`Consented & Randomized (% eligible)` = format_count_percent(cnr, Eligible)) %>% 
    mutate(`Refused (% eligible)` = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Enrolled for 'Other' Reasons (% eligible)` = format_count_percent(`Not Consented`, Eligible)) %>% 
    mutate(`Eligible (% screened)` = format_count_percent(Eligible, Screened)) 
  
  if (include_exclusive_safety_set) {
    final <- final %>%
      mutate(`Safety Set` = format_count_percent(`Safety Set`, cnr))
  }
  
  total_row <- final %>% 
    slice_head(n=1)
  
  if(include_exclusive_safety_set){
    last <- bind_rows(final, total_row) %>% 
      slice_tail(n=-1) %>% 
      select(-Eligible, -Enrolled, -Refused, -`Not Consented`, -cnr, -Discontinued) %>% 
      select(Facility, starts_with('last_days'), Screened2, Enrolled2, Screened, `Eligible (% screened)`, `Refused (% eligible)`, `Not Enrolled for 'Other' Reasons (% eligible)`, 
             `Consented & Randomized (% eligible)`, `Discontinued (% randomized)`, `Safety Set`, `Eligible & Enrolled (% randomized)`)
    
    colnames(last) <- c('Facility', rep(c('Screened', 'Eligible (% screened)', 'Enrolled (% screened)'), length(days)), "Screened", 'Enrolled', 'Screened', 'Eligible (% screened)', 'Refused (% eligible)', 'Not Enrolled for `Other` Reasons (% eligible)', 
                        'Consented & Randomized (% eligible)', paste(discontinued_colname, '(% randomized)'), 'Not Enrolled Safety Set (% randomized)', 'Eligible & Enrolled (% randomized)')
    
    header_num <- c(1,rep(3, length(days)),2,8)
    header_names <- c(" ", paste("Last", days, " Days"), paste("Average per week"), paste("Cumulative", "to date"))
    names(header_num) <- header_names
  } else{
    last <- bind_rows(final, total_row) %>% 
      slice_tail(n=-1) %>% 
      select(-Eligible, -Enrolled, -Refused, -`Not Consented`, -cnr, -Discontinued) %>% 
      select(Facility, starts_with('last_days'), Screened2, Enrolled2, Screened, `Eligible (% screened)`, `Refused (% eligible)`, `Not Enrolled for 'Other' Reasons (% eligible)`, 
             `Consented & Randomized (% eligible)`, `Discontinued (% randomized)`, `Eligible & Enrolled (% randomized)`)
    
    colnames(last) <- c('Facility', rep(c('Screened', 'Eligible (% screened)', 'Enrolled (% screened)'), length(days)), "Screened", 'Enrolled', 'Screened', 'Eligible (% screened)', 'Refused (% eligible)', 'Not Enrolled for `Other` Reasons (% eligible)', 
                        'Consented & Randomized (% eligible)', paste(discontinued_colname, '(% randomized)'), 'Eligible & Enrolled (% randomized)')
    
    header_num <- c(1,rep(3, length(days)),2,7)
    header_names <- c(" ", paste("Last", days, " Days"), paste("Average per week"), paste("Cumulative", "to date"))
    names(header_num) <- header_names
  }
  
  if(length(days) == 1){
    
    if(days == 0){
      last <- last[, c(1, seq(from=5, to=ncol(last)))]
      
      if(average == FALSE){
        last <- last[, c(1, seq(from=4, to=ncol(last)))]
        header_num <- header_num[c(1, 4)]
      }
    } else {
      if(average == FALSE){
        last <- last[, c(1, 2, 3, 4, seq(from=7, to=ncol(last)))]
        header_num <- header_num[c(1, 2, 4)]
      }
    }
  } else {
    if(average == FALSE){
      last <- last[, c(seq(from = 1, to = 3*length(days)+1), seq(3*length(days)+4, to=ncol(last)))]
      header_num <- header_num[c(seq(from=1, to=length(days)+1), length(header_num))]
    }
  }
  
  if(cumulative_data == FALSE){
    if(average == TRUE){
      last <- last[, c(seq(from = 1, to = (ncol(last) - 7)))]
      header_num <- header_num[c(seq(from = 1, to = (length(days) + 2)))]
    }else{
      last <- last[, c(seq(from = 1, to = (ncol(last) - 7)))]
      header_num <- header_num[c(seq(from = 1, to = (length(days) + 1)))]
    }
  }
  
  table <- kable(last, format="html", align='l') %>%
    add_header_above(header_num) %>%
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(nrow(last), bold = TRUE)
  
  return(table)
}

#' Weight Bearing Injury characteristics for Main paper
#'
#' @description This function outputs a table with various injury characteristics for enrolled patients with "Ankle"
#' injuries. This table is produced for Weight bearing main paper. 
#'
#' @param analytic injury_classification_weber, injury_classification_lauge_hansen, injury_gustilo, 
#' injury_type, injury_classification_ankle_ota, definitive_fixation_construct, 
#' definitive_fixation_type, soft_tissue_closure, enrolled
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' wbs_main_paper_injury_characteristics("Replace with Analytic Tibble")
#' 
wbs_main_paper_injury_characteristics <- function(analytic){
  df <- analytic %>% 
    select(injury_classification_weber, injury_classification_lauge_hansen, injury_gustilo, injury_type, 
           injury_classification_ankle_ota, definitive_fixation_construct, definitive_fixation_type, 
           soft_tissue_closure, enrolled) %>% 
    filter(enrolled & injury_type == 'ankle')
  
  total <- df %>% nrow()
  
  df_injury_ota <- df %>% 
    select(injury_classification_ankle_ota) %>% 
    mutate(ota_classification = ifelse(injury_classification_ankle_ota %in% c('44A2', '44A3'), "44 A2/A3", 
                                       ifelse(injury_classification_ankle_ota %in% c('44B2', '44B3'), '44 B2/B3',
                                              ifelse(injury_classification_ankle_ota %in% c('44C1', '44C2', '44C3'), '44 C1/C2/C3', injury_classification_ankle_ota)))) %>% 
    select(-injury_classification_ankle_ota) %>% 
    group_by(ota_classification) %>% 
    count() %>% 
    rename(heading = ota_classification) %>% 
    mutate(Category = "OTA") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_weber <- df %>% 
    select(injury_classification_weber) %>% 
    group_by(injury_classification_weber) %>% 
    count() %>% 
    rename(heading = injury_classification_weber) %>% 
    mutate(Category = "Weber") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_lauge_hansen <- df %>% 
    select(injury_classification_lauge_hansen) %>% 
    group_by(injury_classification_lauge_hansen) %>% 
    count() %>% 
    rename(heading = injury_classification_lauge_hansen) %>% 
    mutate(Category = "Lauge Hansen") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_gustilo <- df %>% 
    select(injury_gustilo) %>% 
    group_by(injury_gustilo) %>% 
    count() %>% 
    rename(heading = injury_gustilo) %>% 
    mutate(Category = "Gustilo")
  
  
  df_fixation_construct <- df %>% 
    select(definitive_fixation_construct) %>% 
    separate_rows(definitive_fixation_construct, sep = ";") %>% 
    group_by(definitive_fixation_construct) %>% 
    count() %>% 
    rename(heading = definitive_fixation_construct) %>% 
    mutate(Category = "Construct") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_fixation_medial <- df %>% 
    select(definitive_fixation_type) %>% 
    separate(definitive_fixation_type, into = c("medial", "lateral", "posterior"), sep='\\|') %>% 
    select(medial) %>% 
    mutate(medial = str_replace(medial, "^Medial-", "")) %>% 
    group_by(medial) %>% 
    count() %>% 
    rename(heading = medial) %>% 
    mutate(Category = "Medial")
  
  df_fixation_lateral <- df %>% 
    select(definitive_fixation_type) %>% 
    separate(definitive_fixation_type, into = c("medial", "lateral", "posterior"), sep='\\|') %>% 
    select(lateral) %>% 
    separate_rows(lateral, sep = ";") %>% 
    mutate(lateral = str_replace(lateral, "^Lateral-", "")) %>% 
    group_by(lateral) %>% 
    count() %>% 
    rename(heading = lateral) %>% 
    mutate(Category = "Lateral") 
  
  df_fixation_posterior <- df %>% 
    select(definitive_fixation_type) %>% 
    separate(definitive_fixation_type, into = c("medial", "lateral", "posterior"), sep='\\|') %>% 
    select(posterior) %>% 
    separate_rows(posterior, sep = ";") %>% 
    mutate(posterior = str_replace(posterior, "^Posterior-", "")) %>% 
    group_by(posterior) %>% 
    count() %>% 
    rename(heading = posterior) %>% 
    mutate(Category = "Posterior") 
  
  empty_df <- tibble(
    soft_tissue_closure = c("Primary closure", "Delayed primary closure", "STSG", "Flap(rotational or free)"),
    n = NA_real_
  )
  
  df_soft_tissue <- df %>% 
    select(soft_tissue_closure) %>% 
    separate_rows(soft_tissue_closure, sep = ";") %>% 
    mutate(soft_tissue_closure = recode(soft_tissue_closure, 
                                        "Primary" = "Primary closure")) %>% 
    full_join(empty_df, by = "soft_tissue_closure") %>% 
    group_by(soft_tissue_closure) %>% 
    count() %>% 
    rename(heading = soft_tissue_closure) %>% 
    mutate(Category = "Tissue") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_heading <- tibble(
    Category = "Fixation Types",
    n = NA_real_
  )
  
  bound_df <- bind_rows(df_injury_ota, df_weber, df_lauge_hansen, df_gustilo, df_fixation_construct, df_heading, df_fixation_medial, df_fixation_lateral, df_fixation_posterior,
                        df_soft_tissue) %>% 
    mutate(n = format_count_percent(n, total))
  
  
  df_table_raw <- reorder_rows(bound_df, list('Category'=c("OTA", "Weber", "Lauge Hansen", "Gustilo", "Construct", 
                                                           'Fixation Types', 'Medial', 'Lateral', 'Posterior', 
                                                           'Tissue'), 
                                              'heading'=c('44 A2/A3', '44 B2/B3', '44 C1/C2/C3', '44B1',
                                                          'Type B', 'Type C', 'Pronation-abduction (PA)',
                                                          'Pronation-external rotation (PER)', 'Supination-adduction (SA)',
                                                          'Supination-external rotation (SER)', 'Closed', 'Type I',
                                                          'Type II', 'Type IIIA', 'Medial only', 'Lateral and Posterior', 'Medial and Lateral',
                                                          'Medial, Lateral, and Posterior', 'No fixation', 'Ligament repair', 'Intramedullary Device',
                                                          'Screws', 'Screws Only', 'Screws and Plates', 'Delayed primary closure', 'Flap(rotational or free)',
                                                          'Primary closure', 'STSG', 'Missing'))) 
  
  index_vec_a <- c(
    "OTA Injury Classification" = df_table_raw %>% filter(Category=='OTA') %>% nrow(), 
    "Weber Classification" = df_table_raw %>% filter(Category=='Weber') %>% nrow(), 
    "Lauge-Hansen Classification" = df_table_raw %>% filter(Category=='Lauge Hansen') %>% nrow(),
    "Gustilo Type" = df_table_raw %>% filter(Category=='Gustilo') %>% nrow(),  
    "Fixation Constructs"= df_table_raw %>% filter(Category=='Construct') %>% nrow(), 
    "Fixation Types" = df_table_raw %>% filter(Category=='Medial'|Category=='Lateral'|Category=='Posterior') %>% nrow(), 
    "Soft Tissue Closure"= df_table_raw %>% filter(Category=='Tissue') %>% nrow()
    )
  index_vec_b <- c(
    " " = 21, 
    "Medial"= df_table_raw %>% filter(Category=='Medial') %>% nrow(), 
    "Lateral"= df_table_raw %>% filter(Category=='Lateral') %>% nrow(), 
    "Posterior"= df_table_raw %>% filter(Category=='Posterior') %>% nrow(),
    " "= 5
    )
  
  
  title <- paste("Total = ", total)
  
  df_for_table <- df_table_raw %>% 
    filter(!is.na(n)) %>% 
    select(-Category) %>% 
    filter(!is.na(heading)) %>% 
    rename(" " = heading) %>% 
    rename(!!title := n) 
  
  table_raw<- kable(df_for_table, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>% 
    pack_rows(index = index_vec_b, label_row_css = "text-align:left", bold = FALSE) %>% 
    kable_styling("striped", full_width = F, position='left') %>% 
    row_spec(c(0,4,7,12,16,21,34), extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}


#' Weight Bearing Patient Characteristics for Main paper
#'
#' @description Visualizes various patient characteristics/demographics for enrolled 
#' patients with "Ankle" injuries. 
#' 
#' NOTE: This table was originally produced for Weight bearing main paper, but may apply to your study, 
#' see the used constructs for more. 
#'
#' @param analytic enrolled, injury_type, sex, age, ethnicity_race, education_level, patient_reported_self_efficacy_6mo, 
#' patient_reported_self_efficacy_12mo, preinjury_productive_activity, preinjury_work_demand, 
#' preinjury_work_hours, tobacco_use, bmi, preinjury_health, insurance_type
#'
#' @return html table
#' @export
#'
#' @examples
#' wbs_main_paper_patient_characteristics(analytic)
#' 
wbs_main_paper_patient_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "injury_type", "sex", "age", "ethnicity_race", "education_level",
                           "patient_reported_self_efficacy_6mo", "patient_reported_self_efficacy_12mo",
                           "preinjury_productive_activity", "preinjury_work_demand", "preinjury_work_hours",
                           "tobacco_use", "bmi", "preinjury_health", "insurance_type"), 
    example_types = c("Boolean", "NamedCategory['ankle' 'other']", "Category-U3", 
                      "Number-U100", "Category-U5", "Number-U10", "Number-U10", "Category-U4",
                      "Category-U4", "Category-U4", "Number-U60", "Boolean", "Number-U60", 
                      "Category-U4", "Category-U4"))
  
  df <- analytic %>% select(enrolled, injury_type, sex, age, ethnicity_race, education_level,
                            patient_reported_self_efficacy_6mo, patient_reported_self_efficacy_12mo,
                            preinjury_productive_activity, preinjury_work_demand, preinjury_work_hours,
                            tobacco_use, bmi, preinjury_health, insurance_type) %>% 
    filter(enrolled, injury_type == 'ankle')
  
  total <- df %>% nrow()
  
  df_age_missing <- df %>%  select(age) %>% mutate(age = ifelse(is.na(age), "Missing", age)) %>% 
    filter(age == "Missing") %>% count(age) %>% 
    rename(heading = age) %>% 
    mutate(Category = "Age") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_age <- df %>% 
    select(age) %>% 
    mutate(age = as.numeric(age)) %>%
    filter(!is.na(age)) %>% 
    summarise(age_mean = format_mean_sd(age)) %>% 
    mutate(heading = 'Mean age, (SD)') %>% 
    mutate(Category = "Age") %>% 
    rename(n = age_mean)
  
  df_age_final <- rbind(df_age, df_age_missing)
  
  df_sex <- df %>% select(sex) %>% 
    group_by(sex) %>% 
    count() %>% 
    mutate(Category = 'Sex') %>% 
    rename(heading = sex) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_race_ethnicity <- df %>% 
    select(ethnicity_race) %>% 
    group_by(ethnicity_race) %>% 
    count() %>% 
    mutate(Category = 'Race Ethnicity') %>% 
    rename(heading = ethnicity_race) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_education <- df %>% 
    select(education_level) %>% 
    group_by(education_level) %>% 
    count() %>% 
    mutate(Category = 'Education Level') %>% 
    rename(heading = education_level) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_self_efficacy_missing_6mo <- df %>%  
    select(patient_reported_self_efficacy_6mo) %>% 
    mutate(patient_reported_self_efficacy_6mo = ifelse(is.na(patient_reported_self_efficacy_6mo), "Missing 6 months Self Efficacy", patient_reported_self_efficacy_6mo)) %>% 
    filter(patient_reported_self_efficacy_6mo == "Missing 6 months Self Efficacy") %>% 
    count(patient_reported_self_efficacy_6mo) %>% 
    rename(heading = patient_reported_self_efficacy_6mo) %>% 
    mutate(Category = "Self Efficacy") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_self_efficacy_missing_12mo <- df %>%  
    select(patient_reported_self_efficacy_12mo) %>% 
    mutate(patient_reported_self_efficacy_12mo = ifelse(is.na(patient_reported_self_efficacy_12mo), "Missing 12 months Self Efficacy", patient_reported_self_efficacy_12mo)) %>% 
    filter(patient_reported_self_efficacy_12mo == "Missing 12 months Self Efficacy") %>% 
    count(patient_reported_self_efficacy_12mo) %>% 
    rename(heading = patient_reported_self_efficacy_12mo) %>% 
    mutate(Category = "Self Efficacy") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_self_efficacy_6mo <- df %>% 
    select(patient_reported_self_efficacy_6mo) %>% 
    filter(!is.na(patient_reported_self_efficacy_6mo)) %>% 
    mutate(patient_reported_self_efficacy_6mo = as.numeric(patient_reported_self_efficacy_6mo)) %>% 
    summarise(n = format_mean_sd(patient_reported_self_efficacy_6mo)) %>% 
    mutate(heading = 'Within 6 Months') %>% 
    mutate(Category = 'Self Efficacy')  
  
  df_self_efficacy_12mo <- df %>% 
    select(patient_reported_self_efficacy_12mo) %>% 
    filter(!is.na(patient_reported_self_efficacy_12mo)) %>% 
    mutate(patient_reported_self_efficacy_12mo = as.numeric(patient_reported_self_efficacy_12mo)) %>% 
    summarise(n = format_mean_sd(patient_reported_self_efficacy_12mo)) %>% 
    mutate(heading = 'Within 1 year') %>% 
    mutate(Category = 'Self Efficacy')  
  
  df_self_efficacy_final <- rbind(df_self_efficacy_6mo, df_self_efficacy_missing_6mo, df_self_efficacy_12mo, df_self_efficacy_missing_12mo)
  
  
  df_usual_major_activity <- df %>% 
    select(preinjury_productive_activity) %>% 
    group_by(preinjury_productive_activity) %>% 
    count() %>% 
    mutate(Category = 'Major Activity') %>% 
    rename(heading = preinjury_productive_activity) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_physical_demand <- df %>% 
    select(preinjury_work_demand) %>% 
    group_by(preinjury_work_demand) %>% 
    count() %>% 
    mutate(Category = 'Work Demand') %>% 
    rename(heading = preinjury_work_demand) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_work_hours_missing <- df %>%  select(preinjury_work_hours) %>% 
    mutate(preinjury_work_hours = ifelse(is.na(preinjury_work_hours), "Missing", preinjury_work_hours)) %>% 
    filter(preinjury_work_hours == "Missing") %>% count(preinjury_work_hours) %>% 
    rename(heading = preinjury_work_hours) %>% 
    mutate(Category = "Work hours") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_work_hours <- df %>% 
    select(preinjury_work_hours) %>% 
    mutate(preinjury_work_hours = as.numeric(preinjury_work_hours)) %>% 
    filter(!is.na(preinjury_work_hours)) %>% 
    summarise(work_hours = format_mean_sd(preinjury_work_hours)) %>% 
    mutate(heading = 'Mean hours, (SD)') %>% 
    mutate(Category = "Work hours")  %>% 
    rename(n = work_hours)
  
  df_work_hours_final <- rbind(df_work_hours, df_work_hours_missing)
  
  
  df_tobacco <- df %>% 
    select(tobacco_use) %>% 
    group_by(tobacco_use) %>% 
    count() %>% 
    rename(heading = tobacco_use) %>% 
    mutate(Category = 'Tobacco') %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_bmi_missing <- df %>%  select(bmi) %>% mutate(bmi = ifelse(is.na(bmi), "Missing", bmi)) %>% 
    filter(bmi == "Missing") %>% count(bmi) %>% 
    rename(heading = bmi) %>% 
    mutate(Category = "BMI") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_bmi <- df %>% 
    select(bmi) %>% 
    mutate(bmi = as.numeric(bmi)) %>% 
    summarise(bmi_mean = format_mean_sd(bmi)) %>% 
    mutate(heading = 'Mean, (SD)') %>% 
    mutate(Category = "BMI")  %>% 
    rename(n = bmi_mean)
  
  df_bmi_final <- rbind(df_bmi, df_bmi_missing)
  
  df_preinjury_health <- df %>% 
    select(preinjury_health) %>% 
    group_by(preinjury_health) %>%
    count() %>% 
    rename(heading = preinjury_health) %>% 
    mutate(Category = 'Health') %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_insurance <- df %>% 
    select(insurance_type) %>% 
    mutate(insurance_type = ifelse(str_detect(insurance_type, "Medicaid"), "Medicaid", 
                                   ifelse(!is.na(insurance_type), "Other Insurance", NA))) %>% 
    mutate(insurance_type = ifelse(!is.na(insurance_type), insurance_type, "Missing")) %>% 
    group_by(insurance_type) %>%
    count() %>% 
    rename(heading = insurance_type) %>% 
    mutate(Category = 'Insurance')  %>% 
    mutate(n = format_count_percent(n, total))
  
  df_final <- rbind(df_age_final, df_sex, df_race_ethnicity, df_education, df_self_efficacy_final, df_usual_major_activity,
                    df_physical_demand, df_work_hours_final, df_tobacco, df_bmi_final, df_preinjury_health, df_insurance) 
  
  
  index_vec_a <- c(
    "Age" = nrow(df_age_final),
    "Sex" = nrow(df_sex),
    "Race Ethnicity" = nrow(df_race_ethnicity),
    "Education" = nrow(df_education),
    "Self Efficacy for return to Usual Activities" = nrow(df_self_efficacy_final),
    "Preinjury Usual Major Activity" = nrow(df_usual_major_activity),
    "Physical Demand of Job" = nrow(df_physical_demand),
    "Hours worked per week" = nrow(df_work_hours_final),
    "Tobacco Use" = nrow(df_tobacco),
    "BMI" = nrow(df_bmi_final),
    "Preinjury Health" = nrow(df_preinjury_health),
    "Insurance Type" = nrow(df_insurance)
  )
  
  # Compute the cumulative row indices for adding bottom borders.
  # The first element (0) is used for the header row.
  border_rows <- c(0, cumsum(index_vec_a))
  
  title <- paste("Total = ", total)
  
  
  df_for_table <- df_final %>% 
    select(heading, n) %>% 
    rename("Enrolled" = heading) %>% 
    rename(!!title := n) 
  
  
  table_raw<- kable(df_for_table, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left') %>% 
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")
  
  
  return(table_raw)
} 

#' Expected visit status for Overall Followup
#'
#' @description 
#' Returns the counts of all the statuses of the Overall follow-up form at every follow-up periods. Notably,
#' this function does not separate the counts by site.
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' expected_and_followup_visit_overall("Replace with Analytic Tibble")
#' 
expected_and_followup_visit_overall <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("followup_data"),
    example_types = c("(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date")) 

  df <- analytic %>% 
    select(study_id, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate(status = as.character(status)) %>% 
    mutate_all(na_if, 'NA')
  
  fu_levels <- df$followup_period %>% unique()
  fu_levels <- fu_levels[!is.na(fu_levels)]
  
  result_list <- list()
  
  for (i in fu_levels) {
    result <- df %>% 
      filter(followup_period == i,
             form == 'Overall') %>% 
      select(study_id, status) %>%
      filter(!is.na(status)) %>% 
      separate_rows(status, sep = ': ') %>% 
      count(status) %>% 
      rename(!!i := n)
    
    result_list[[i]] <- result
  }
  
  combined <- Reduce(function(x, y) full_join(x, y, by = "status"), result_list) %>%
    mutate(status = tools::toTitleCase(as.character(status))) %>%
    mutate(status = ifelse(status == 'Not_started', 'Not Started', status)) %>% 
    mutate(status = as.character(status))
  
  df_empty <- data.frame('status' = c("Not Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete')) %>% 
    mutate_all(as.character)
  
  final_raw <- left_join(df_empty, combined, by = 'status') %>% 
    mutate(across(everything(), ~replace_na(., 0)))
  
  summed_statuses <- c("Complete", "Incomplete", "Missed", "Not Started")
  
  expected_row <- final_raw %>%
    filter(status %in% summed_statuses) %>%
    summarize(across(-status, sum, na.rm = TRUE)) %>%
    mutate(status = "Expected") %>%
    select(status, everything())
  
  final_pre_pct <- rbind(expected_row, final_raw)
  
  divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
  names(divisor_expected) <- names(final_pre_pct)[-1]
  divisor_complete <- final_pre_pct[3, -1] %>% as.numeric()
  names(divisor_complete) <- names(final_pre_pct)[-1]
  
  top <- final_pre_pct %>% 
    slice_head(n=3) %>%
    slice_tail(n=1) %>% 
    mutate(across(-status, 
                  ~ format_count_percent(., divisor_expected[cur_column()]),
                  .names = "{.col}"))
  
  bottom <- final_pre_pct %>% 
    slice_tail(n=3) %>% 
    mutate(across(-status, 
                  ~ format_count_percent(., divisor_expected[cur_column()]),
                  .names = "{.col}"))
  
  middle <- final_pre_pct %>% 
    slice_head(n=5) %>% 
    slice_tail(n=2) %>% 
    mutate(across(-status, 
                  ~ format_count_percent(., divisor_complete[cur_column()]),
                  .names = "{.col}"))
  
  not_expected <- final_pre_pct %>%
    slice_head(n=2) %>%
    slice_tail(n=1)
  
  final_last <- rbind(not_expected, expected_row, top, middle, bottom) %>% 
    rename(Status = status)
  
  vis <- kable(final_last, format="html", align='l') %>%
    add_indent(c(4,5)) %>% 
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}



#' Followup Data Single Form and Timepoint By Site
#'
#' @description Returns the designated followup form status across all sites, 
#' for a single timepoint.
#' 
#' #' For other manipulations of the followup_data long file that may better fit your study, please see: followup_completion_time_stats, 
#' followup_form_all_timepoints_by_site, followup_form_at_timepoint_by_site, followup_forms_all_timepoints, followup_forms_at_timepoint_by_site
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data
#' @param timepoint the point in time to be considered in the visualization
#' @param form_selection the form to be considered in the visualization
#' @param name optional argument for changing the name of the followup form, for aesthetic use
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_form_at_timepoint_by_site("Replace with Analytic Tibble", "3 Month", "Form 3")
#' 
followup_form_at_timepoint_by_site <- function(analytic, timepoint, form_selection, name = NULL){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "followup_data"), 
    example_types = c("FacilityCode", "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  form_collected <- function(form_selection, facility = 'TOTAL'){
    if (facility!='TOTAL') {
      df <- df %>%
        filter(facilitycode == facility)
    }
    
    result <- df %>% 
      filter(followup_period == timepoint,
             form == form_selection) %>% 
      select(study_id, status) %>%
      filter(!is.na(status)) %>% 
      separate_rows(status, sep = ': ') %>% 
      count(status) %>% 
      rename(!!form_selection := n)
    
    df_empty <- data.frame('status' = c("Not Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'))
    
    final_raw <- left_join(df_empty, result, by = 'status') %>% 
      mutate(across(everything(), ~replace_na(., 0)))
    
    summed_statuses <- c("Complete", "Incomplete", "Missed", "Not Started")
    
    expected_row <- final_raw %>%
      filter(status %in% summed_statuses) %>%
      summarize(across(-status, sum, na.rm = TRUE)) %>%
      mutate(status = "Expected") %>%
      select(status, everything())
    
    final_pre_pct <- rbind(expected_row, final_raw)
    
    divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
    names(divisor_expected) <- names(final_pre_pct)[-1]
    divisor_complete <- final_pre_pct[3, -1] %>% as.numeric()
    names(divisor_complete) <- names(final_pre_pct)[-1]
    
    top <- final_pre_pct %>% 
      slice_head(n=3) %>%
      slice_tail(n=1) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_expected[cur_column()]),
                    .names = "{.col}"))
    
    bottom <- final_pre_pct %>% 
      slice_tail(n=3) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_expected[cur_column()]),
                    .names = "{.col}"))
    
    middle <- final_pre_pct %>% 
      slice_head(n=5) %>% 
      slice_tail(n=2) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_complete[cur_column()]),
                    .names = "{.col}"))
    
    not_expected_row <- final_pre_pct %>%
      slice_head(n=2) %>%
      slice_tail(n=1)
      
    out <- rbind(not_expected_row, expected_row, top, middle, bottom) %>% 
      rename(Status = status) %>%
      pivot_wider(values_from = -Status, names_from = Status) %>%
      mutate(Facility = facility) %>%
      select(Facility, everything())
    out
  }
  
  facilities <- df %>%
    pull(facilitycode) %>%
    unique()
  facilities <- c('TOTAL', facilities)
  facilities <- facilities[!is.na(facilities)]
  
  form_df <- tibble()
  for (code in facilities) {
    form_df <- bind_rows(form_df, form_collected(form_selection, code))
  }
  
  form_df <- form_df %>%
    filter(!is.na(Facility)&Facility!='NA')
  
  header <- c(1,8)
  names(header) <- c(' ', ifelse(is.null(name),
                                 paste0(form_selection, ' Status at ', timepoint, ' Period'),
                                 paste0(name, ' Status at ', timepoint, ' Period')))
  
  vis <- kable(form_df, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Followup Data Single Form All Timepoints By Site
#'
#' @description 
#' Returns the counts of the specified form statuses of a given form for all follow-up periods where that
#' form is present, by site. Specifying "Overall" in the form_selection results in a slightly more streamlined
#' look, without a header above the table.
#'
#' @param analytic analytic data set that must include study_id, followup_data
#' @param form_selection form whose statuses are to be investigated
#' @param included_colmns statuses to include in the vis
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_form_all_timepoints_by_site("Replace with Analytic Tibble", form_selection = "Form 3")
#' followup_form_all_timepoints_by_site("Replace with Analytic Tibble", form_selection = "Form 3", included_columns = c("Expected", "Complete", "Incomplete"))
#' 
followup_form_all_timepoints_by_site <- function(
    analytic, form_selection = 'Overall', 
    included_columns=c("Not Expected", "Expected", "Complete", 
                       "Early", "Late", 'Missed', 'Not Started', 
                       'Incomplete')){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "followup_data"), 
    example_types = c("FacilityCode", "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  form_collected <- function(form_selection, timepoint, facility = 'TOTAL'){
    if (facility!='TOTAL') {
      df <- df %>%
        filter(facilitycode == facility)
    }
    
    result <- df %>% 
      filter(followup_period == timepoint,
             form == form_selection) %>% 
      select(study_id, status) %>%
      filter(!is.na(status)) %>% 
      separate_rows(status, sep = ': ') %>% 
      count(status) %>% 
      rename(!!form_selection := n)
    
    df_empty <- data.frame('status' = c("Not Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'))
    
    final_raw <- left_join(df_empty, result, by = 'status') %>% 
      mutate(across(everything(), ~replace_na(., 0)))
    
    summed_statuses <- c("Complete", "Incomplete", "Missed", "Not Started")
    
    expected_row <- final_raw %>%
      filter(status %in% summed_statuses) %>%
      summarize(across(-status, sum, na.rm = TRUE)) %>%
      mutate(status = "Expected") %>%
      select(status, everything())
    
    final_pre_pct <- rbind(expected_row, final_raw)
    
    divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
    names(divisor_expected) <- names(final_pre_pct)[-1]
    divisor_complete <- final_pre_pct[3, -1] %>% as.numeric()
    names(divisor_complete) <- names(final_pre_pct)[-1]
    
    top <- final_pre_pct %>% 
      slice_head(n=3) %>%
      slice_tail(n=1) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_expected[cur_column()]),
                    .names = "{.col}"))
    
    bottom <- final_pre_pct %>% 
      slice_tail(n=3) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_expected[cur_column()]),
                    .names = "{.col}"))
    
    middle <- final_pre_pct %>% 
      slice_head(n=5) %>% 
      slice_tail(n=2) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_complete[cur_column()]),
                    .names = "{.col}"))
    
    not_expected_row <- final_pre_pct %>%
      slice_head(n=2) %>%
      slice_tail(n=1)
    
    out <- rbind(not_expected_row, expected_row, top, middle, bottom) %>% 
      rename(Status = status) %>%
      pivot_wider(values_from = -Status, names_from = Status) %>%
      mutate(Facility = facility) %>%
      select(Facility, everything())
    out
  }
  
  facilities <- df %>%
    pull(facilitycode) %>%
    unique()
  facilities <- c('TOTAL', facilities)
  facilities <- facilities[!is.na(facilities)]
  
  timepoints <- df %>%
    filter(form == form_selection) %>%
    pull(followup_period) %>%
    unique()
  timepoints <- timepoints[!is.na(timepoints)]
  
  form_df <- tibble(
    Facility = facilities
  )
  for (timepoint in timepoints) {
    period_df <- tibble()
    for (code in facilities) {
      period_df <- bind_rows(period_df, form_collected(form_selection, timepoint, code))
    }
    form_df <- full_join(form_df, period_df, by = 'Facility')
  }
  
  form_df <- form_df %>%
    filter(!is.na(Facility)&Facility!='NA') %>% 
    select(matches(paste0("^",paste(c("Facility",included_columns),collapse="|^"))))
    
  colnames(form_df) <- c('Facility', rep(included_columns, times = length(timepoints)))
  
  header <- c(1,rep(length(included_columns), length(timepoints)))
  names(header) <- c(' ', timepoints)
  
  over_header <- c(1, length(included_columns)*length(timepoints))
  names(over_header) <- c(' ', paste(form_selection, 'Form Status'))
  
  if(form_selection=="Overall"){
    vis <- kable(form_df, format="html", align='l') %>%
      add_header_above(header) %>%
      kable_styling("striped", full_width = F, position='left')
  } else{
    vis <- kable(form_df, format="html", align='l') %>%
      add_header_above(header) %>%
      add_header_above(over_header) %>%
      kable_styling("striped", full_width = F, position='left')
  }
  return(vis)
}




#' Followup Data Multiple Forms and Single Timepoints By Site
#'
#' @description Visualizes the specified follow-up forms at a specific timeppoint, listed by site.
#' 
#' #' For other manipulations of this file that may better fit your study, please see: followup_completion_time_stats, 
#' followup_form_all_timepoints_by_site, followup_form_at_timepoint_by_site, followup_forms_all_timepoints, followup_forms_at_timepoint_by_site
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_forms_at_timepoint_by_site("Replace with Analytic Tibble", '3 Month', c('Form 3', 'Form 2'))
#' 
followup_forms_at_timepoint_by_site <- function(analytic, timepoint, forms, names = NULL){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "followup_data"), 
    example_types = c("FacilityCode", "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  output <- tibble(
    Facility = c('TOTAL', unique(df$facilitycode))
  )
  
  for (form_selection in forms) {
    form_collected <- function(form_selection, facility = 'TOTAL'){
      if (facility!='TOTAL') {
        df <- df %>%
          filter(facilitycode == facility)
      }
      
      result <- df %>% 
        filter(followup_period == timepoint,
               form == form_selection) %>% 
        select(study_id, status) %>%
        filter(!is.na(status)) %>% 
        separate_rows(status, sep = ': ') %>% 
        count(status) %>% 
        rename(!!form_selection := n)
      
      df_empty <- data.frame('status' = c("Not Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'))
      
      final_raw <- left_join(df_empty, result, by = 'status') %>% 
        mutate(across(everything(), ~replace_na(., 0)))
      
      summed_statuses <- c("Complete", "Incomplete", "Missed", "Not Started")
      
      expected_row <- final_raw %>%
        filter(status %in% summed_statuses) %>%
        summarize(across(-status, sum, na.rm = TRUE)) %>%
        mutate(status = "Expected") %>%
        select(status, everything())
      
      final_pre_pct <- rbind(expected_row, final_raw)
      
      divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
      names(divisor_expected) <- names(final_pre_pct)[-1]
      divisor_complete <- final_pre_pct[3, -1] %>% as.numeric()
      names(divisor_complete) <- names(final_pre_pct)[-1]
      
      top <- final_pre_pct %>% 
        slice_head(n=3) %>%
        slice_tail(n=1) %>% 
        mutate(across(-status, 
                      ~ format_count_percent(., divisor_expected[cur_column()]),
                      .names = "{.col}"))
      
      bottom <- final_pre_pct %>% 
        slice_tail(n=3) %>% 
        mutate(across(-status, 
                      ~ format_count_percent(., divisor_expected[cur_column()]),
                      .names = "{.col}"))
      
      middle <- final_pre_pct %>% 
        slice_head(n=5) %>% 
        slice_tail(n=2) %>% 
        mutate(across(-status, 
                      ~ format_count_percent(., divisor_complete[cur_column()]),
                      .names = "{.col}"))
      
      not_expected_row <- final_pre_pct %>%
        slice_head(n=2) %>%
        slice_tail(n=1)
      
      out <- rbind(not_expected_row, expected_row, top, middle, bottom) %>% 
        rename(Status = status) %>%
        pivot_wider(values_from = -Status, names_from = Status) %>%
        mutate(Facility = facility) %>%
        select(Facility, everything())
      out
    }
    
    facilities <- df %>%
      pull(facilitycode) %>%
      unique()
    facilities <- c('TOTAL', facilities)
    facilities <- facilities[!is.na(facilities)]
    
    form_df <- tibble()
    for (code in facilities) {
      form_df <- bind_rows(form_df, form_collected(form_selection, code))
    }
    output <- full_join(output, form_df, by = 'Facility') %>%
      filter(!is.na(Facility)&Facility!='NA')
  }
  
  cols <- c('Facility', rep(c("Not Expected", "Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 
                              'Incomplete'), times = length(forms)))
  colnames(output) <- cols
  
  header <- c(1,rep(8, length(forms)))
  if (is.null(names)) {
    header_names <- c(' ', paste0(forms, ' Status at ', timepoint, ' Period'))
  } else {
    header_names <- c(' ', paste0(names, ' Status at ', timepoint, ' Period'))
  }
  
  names(header) <- header_names
  
  vis <- kable(output, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Followup Data Multiple Forms and All Timepoints
#'
#' @description 
#' Returns all of the statuses of the given follow-up forms at all timepoints by site. Not specifying forms results
#' in all follow-up forms being in the visualization.
#' 
#' NOTE: THIS VISUALIZATION CAN BE VERY LARGE!
#'
#' @param analytic analytic data set that must include study_id, followup_data, facilitycode
#' @param forms followup forms to output, as found in the followup_data construct
#' @param timepoints timepoints to output, as found in the followup_data construct
#' @param vertical whether to arrange the output vertically by form or horizontally.
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_forms_all_timepoints("Replace with Analytic Tibble", forms = c('Form 3', 'Form 2'), timepoints = c('3 Month', '6 Month'))
#' followup_forms_all_timepoints("Replace with Analytic Tibble", vertical = FALSE)
#' 
followup_forms_all_timepoints <- function(analytic, forms = NULL, timepoints = NULL, vertical = TRUE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "followup_data"), 
    example_types = c('FacilityCode', "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  if (is.null(forms)) {
    forms <- df %>%
      pull(form) %>%
      unique()
    forms <- forms[!is.na(forms)]
  }
  if (is.null(timepoints)) {
    timepoints <- df %>%
      pull(followup_period) %>%
      unique()
    timepoints <- timepoints[!is.na(timepoints)]
  }
  
  per_form <- function(form_name) {
    form_df <- df %>%
      filter(form==form_name)
    
    if (nrow(form_df)==0) {
      stop('function call asks for form not in followup_data construct!')
    }
  
    fu_levels <- timepoints
    
    result_list <- list()
    
    for (i in fu_levels) {
      result <- form_df %>% 
        filter(followup_period == i) %>% 
        select(study_id, status) %>%
        filter(!is.na(status)) %>% 
        separate_rows(status, sep = ': ') %>% 
        count(status) %>% 
        rename(!!i := n)
      
      if (nrow(result) != 0) {
        result_list[[i]] <- result      }
    }
    
    combined <- Reduce(function(x, y) full_join(x, y, by = "status"), result_list) %>%
      mutate(status = tools::toTitleCase(status)) %>%
      mutate(status = ifelse(status == 'Not_started', 'Not Started', status))
    
    form_df_empty <- data.frame('status' = c("Not Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'))
    
    final_raw <- left_join(form_df_empty, combined, by = 'status') %>% 
      mutate(across(everything(), ~replace_na(., 0)))
    
    summed_statuses <- c("Complete", "Incomplete", "Missed", "Not Started")
    
    expected_row <- final_raw %>%
      filter(status %in% summed_statuses) %>%
      summarize(across(-status, sum, na.rm = TRUE)) %>%
      mutate(status = "Expected") %>%
      select(status, everything())
    
    final_pre_pct <- rbind(expected_row, final_raw)
    
    divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
    names(divisor_expected) <- names(final_pre_pct)[-1]
    divisor_complete <- final_pre_pct[3, -1] %>% as.numeric()
    names(divisor_complete) <- names(final_pre_pct)[-1]
    
    top <- final_pre_pct %>% 
      slice_head(n=3) %>%
      slice_tail(n=1) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_expected[cur_column()]),
                    .names = "{.col}"))
    
    bottom <- final_pre_pct %>% 
      slice_tail(n=3) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_expected[cur_column()]),
                    .names = "{.col}"))
    
    middle <- final_pre_pct %>% 
      slice_head(n=5) %>% 
      slice_tail(n=2) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_complete[cur_column()]),
                    .names = "{.col}"))
    
    not_expected_row <- final_pre_pct %>%
      slice(2)
    
    final_last <- rbind(not_expected_row, expected_row, top, middle, bottom) %>% 
      rename(Status = status)
    
    final_last
  }
  
  found_timepoints <- c()
  header <- c()
  out <- NULL
  for (form_name in forms) {
    res <- per_form(form_name)
    if (is.null(out)) {
      out <- res
      found_timepoints <- colnames(res)
      header <- rep(form_name, times = length(colnames(res))-1)
    } else {
      out <- full_join(out, per_form(form_name), by = 'Status')
      found_timepoints <- c(found_timepoints, colnames(res))
      header <- c(header, rep(form_name, times = length(colnames(res))-1))
    }
  }
  found_timepoints <- found_timepoints[found_timepoints!='Status']
  
  colnames(out) <- c('Status', found_timepoints)
  header <- c(' ', base::table(header))
  
  if (vertical) {
    out_long <- NULL
    i <- 2
    for (package in names(header[-1])) {
      colcount <- as.numeric(header[package]) - 1
      colindex <- i + colcount
      temp_df <- out[i:colindex]
      if (is.null(out_long)) {
        out_long <- temp_df
      } else {
        out_long <- bind_rows(out_long, temp_df)
      }
      i <- colindex + 1
    }
    
    out_long <- out_long %>%
      mutate(across(everything(), ~replace(., is.na(.), "."))) %>%
      mutate(Status = rep(c("Not Expected", "Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'), 
                          length(forms))) %>%
      select(Status, everything())
    
      
    vis <- kable(out_long, format="html", align='l')  %>%
      kable_styling("striped", full_width = F, position='left')
    
    i <- 1
    for (package in names(header[-1])) {
      vis <- vis %>%
        pack_rows(package, i, i + 7)
      i <- i + 8
    }
  } else if (!vertical) {
    vis <- kable(out, format="html", align='l') %>%
      add_indent(c(3,4)) %>% 
      add_header_above(header) %>%
      kable_styling("striped", full_width = F, position='left')
  }
  
  return(vis)
}


#' Overview of enrollment and follow-up activities
#'
#' @description 
#' Returns the screened, overall, and follow-up data, separated by sites
#'
#' @param analytic analytic data set that must include study_id, followup_data,
#' facilitycode, screened, enrolled, eligible, screened_date
#' @param form_name The exact name (specified in followup_data) that you want
#' to find the data for
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_and_followup_activities_overview("Replace with Analytic Tibble")
#' 
enrollment_and_followup_activities_overview <- function(analytic, form_name = 'Overall'){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "followup_data", "screened", "enrolled", "eligible", "screened_date"), 
    example_types = c('FacilityCode', "(';new_row: ', '|')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date",
                      "Boolean", "Boolean", "Boolean", "Date"))
  
   followups <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 
                              'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA') %>%
    filter(form == form_name)
  
  followups <- followups %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  study_status <- analytic %>%
    select(study_id, facilitycode, screened, enrolled, eligible, screened_date)
  
  last30 <- Sys.Date() - 30
  
  per_site <- function(site = 'TOTAL') {
    if (site != 'TOTAL') {
      site_followups <-  followups %>%
        filter(facilitycode == site)
      site_study_status <- study_status %>%
        filter(facilitycode == site)
    } else {
      site_followups <- followups
      site_study_status <- study_status
    }
    
    last_month <- site_study_status %>%
      filter(screened_date > last30) %>%
      mutate(elig_not_enr = eligible & !enrolled) %>%
      reframe(Screened = sum(screened, na.rm = TRUE),
              Enrolled = sum(enrolled, na.rm = TRUE),
              `Eligible, Not Enrolled` = sum(elig_not_enr, na.rm = TRUE)) %>%
      mutate(Enrolled = format_count_percent(Enrolled, Screened),
             `Eligible, Not Enrolled` = format_count_percent(`Eligible, Not Enrolled`, Screened))
    
    historical <- site_study_status %>%
      mutate(elig_not_enr = eligible & !enrolled) %>%
      reframe(Screened = sum(screened, na.rm = TRUE),
              Enrolled = sum(enrolled, na.rm = TRUE),
              `Eligible, Not Enrolled` = sum(elig_not_enr, na.rm = TRUE)) %>%
      mutate(Enrolled = format_count_percent(Enrolled, Screened),
             `Eligible, Not Enrolled` = format_count_percent(`Eligible, Not Enrolled`, Screened))
    
    periods <- site_followups %>%
      pull(followup_period) %>%
      unique()
    
    followup_counts <- tibble()
    
    count_list <- list()
    
    for (period in periods) {
      complete_count <- site_followups %>%
        filter(followup_period == period, status == 'Complete') %>%
        nrow()
      
      count_list[[paste(period, "Follow-up")]] <- complete_count
    }
    
    followup_counts <- as_tibble(count_list)  
    
    site_combined <- cbind(last_month, historical, followup_counts)
    site_counts <- tibble(
      Site=site, site_combined, .name_repair = 'minimal'
    )
    
    site_counts
  }
  
  all_sites <- study_status$facilitycode %>% unique() %>% sort()
  all_sites_followup <- followups$facilitycode %>% unique() %>% sort()
  
  if (length(all_sites) != length(all_sites_followup)){
    sites_wo_followups <- all_sites[!all_sites %in% all_sites_followup]
    warning(paste("Site(s)", sites_wo_followups, "not found in followup_data, will not be included in final visualization"))
    all_sites <- all_sites[all_sites %in% all_sites_followup]
  }
  
  sites_combined <- tibble()
  for (site in c('TOTAL', all_sites)) {
    sites_combined <- rbind(sites_combined, per_site(site))
  }
  
  header <- c(1, 3, 3, ncol(sites_combined)-7)
  names(header) <- c(' ', 'Last 30 Days', 'Study Length', 'Follow-up Completion Status')
  
  vis <- kable(sites_combined, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Follow-up Forms Time to Complete
#'
#' @description 
#' Returns summary statistics on the number of days to complete various follow-up forms. Each study will have a 'unique' followup data long file, 
#' this visualization manipulates a part of that file to return that info for the desired forms.
#' 
#' For other manipulations of the followup_data long file that may better fit your study, please see: followup_completion_time_stats, 
#' followup_form_all_timepoints_by_site, followup_form_at_timepoint_by_site, followup_forms_all_timepoints, followup_forms_at_timepoint_by_site
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data, event_time_zero,
#' and enrolled
#' @param timepoints the point in time to be considered in the visualization
#' @param form_selection the form to be considered in the visualization
#'
#' @return html table
#' @export
#'
#' @examples
#' followup_completion_time_stats("Replace with Analytic Tibble")
#' 
followup_completion_time_stats <- function(analytic, timepoints = c('6mo', '12mo'), ortho_timepoints = NULL, form_selection = 'Overall'){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('time_zero', "followup_data", "enrolled", 'followup_expected_12mo', 'followup_expected_6mo'), 
    example_types = c('Date', "(';new_row: ', '|')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date", 'Boolean', 'Boolean', 'Boolean'))
  
  if (is.null(ortho_timepoints)) {
    ortho_timepoints <- timepoints
  }
  
  df <- analytic %>%
    select(study_id, time_zero, followup_data,
           matches(paste0('^orthopaedic_last_date_(', paste(ortho_timepoints, collapse = '|'), ')$')),
           matches(paste0('^followup_expected_(', paste(timepoints, collapse = '|'), ')$')),
           enrolled) %>%
    mutate(across(matches("_date$|_date_"), ~ as.Date(., format = "%Y-%m-%d"))) %>%
    mutate(time_zero = as.Date(sapply(str_split(time_zero, ";"), `[`, 1))) %>%
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>%
    separate(status, c('status', 'timing'), sep = ':') %>%
    filter(form == form_selection) %>%
    select(-form, -redcap_event_name, -timing)
  
  df <- df %>%
    mutate(status = na_if(status, 'NA')) %>%
    mutate(form_dates = na_if(form_dates, 'NA')) 
  
  beautify_timepoint <- function(string) {
    newstr <- str_replace(string, 'wk', ' Week') %>%
      str_replace('mo', ' Month')
    return(newstr)
  }
  
  expected_counts <- df %>%
    select(study_id, starts_with('followup_expected')) %>%
    unique() %>%
    summarise(across(starts_with('followup_expected'), ~ sum(. == TRUE))) %>%
    pivot_longer(starts_with('followup_expected'))  %>%
    mutate(name = beautify_timepoint(str_remove(name, "followup_expected_"))) %>%
    rename(`Follow-up Period` = name)
  
  converted_timepoints <- beautify_timepoint(timepoints)
  converted_ortho_timepoints <- beautify_timepoint(ortho_timepoints)
  
  filtered_and_pivoted <- df %>%
    select(-starts_with('followup_expected')) %>%
    filter(followup_period %in% converted_timepoints) %>%
    mutate(form_dates = as.Date(form_dates)) %>%
    pivot_wider(values_from = form_dates, names_from = followup_period,
                names_prefix = 'Form; ')
  
  long_format <- filtered_and_pivoted %>%
    pivot_longer(
      cols = starts_with("orthopaedic_last_date") | starts_with("Form"),
      names_to = "timepoint",
      values_to = "date"
    ) %>%
    mutate(timepoint = beautify_timepoint(str_replace(timepoint, 'orthopaedic_last_date_', 'Ortho; ')))
  
  date_calc <- long_format %>%
    mutate(days = as.numeric(date - time_zero))
  
  inner_function <- function(inner_data) {
    inner_out <- tibble(
      `N (Number of Complete)` = nrow(inner_data),
      `Mean (Days)` = mean(inner_data$days, na.rm = TRUE) %>% round(2),
      `Standard Deviation` = sd(inner_data$days, na.rm = TRUE) %>% round(2),
      `Minimum (Days)` = min(inner_data$days, na.rm = TRUE),
      `24th Percentile (Days)` = quantile(inner_data$days, 0.24, na.rm = TRUE, type = 1),
      `Median (Days)` = quantile(inner_data$days, 0.5, na.rm = TRUE, type = 1),
      `75th Percentile (Days)` = quantile(inner_data$days, 0.75, na.rm = TRUE, type = 1),
      `Maximum (Days)` = max(inner_data$days, na.rm = TRUE)
    )
    if (inner_out$`N (Number of Complete)` == 0) {
      inner_out <- inner_out %>%
        mutate(across(-`N (Number of Complete)`, ~ "."))
    }
    inner_out
  }
  
  separated <- date_calc %>%
    separate(timepoint, into = c('kind', 'period'), sep = '; ')
  
  out <- NULL
  for(time in converted_timepoints) {
    temp <- separated %>%
      filter(period == time)
    
    if (time %in% converted_ortho_timepoints) {
      temp2 <- rbind(
        inner_function(temp %>% filter(kind=='Form'&enrolled&!is.na(days))) %>%
          mutate(kind = 'form completion', group = 'enrolled', 
                 Description = 'Days to Form Completion'),
        inner_function(temp %>% filter(kind=='Ortho'&enrolled&!is.na(days))) %>%
          mutate(kind = 'ortho visit', group = 'enrolled', 
                 Description = 'Days to Last Orthopaedic Visit')) %>%
        mutate(period = time)
    } else {
      temp2 <- rbind(
        inner_function(temp %>% filter(kind=='Form'&enrolled&!is.na(days))) %>%
          mutate(kind = 'form completion', group = 'enrolled', 
                 Description = 'Days to Form Completion')) %>%
        mutate(period = time)
    }
    
    if (is.null(out)) {
      out <- temp2
    } else {
      out <- rbind(out, temp2)
    }
  }
  
  output <- full_join(expected_counts, out %>% rename(`Follow-up Period` = period)) %>%
    select("Follow-up Period", "value", "N (Number of Complete)", 
           "Description", "Mean (Days)", "Standard Deviation", 
           "Minimum (Days)", "24th Percentile (Days)", "Median (Days)", 
           "75th Percentile (Days)", "Maximum (Days)") %>%
    arrange(factor(`Follow-up Period`, levels = converted_timepoints)) %>%
    rename(`N (Number of Expected)` = value)
  
  vis <- kable(output, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Not enrolled reason
#'
#' @description 
#' Visualizes list of study_ids who were are not enrolled, the reasons, and the screening notes reasons.
#' 
#' See also: not_enrolled_for_other_reasons, which examines the reasons categorized as 'other'
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, study_id, not_enrolled_reason, pre_screened_notes
#'
#' @return An HTML table.
#' @export
#'
#' @examples                     
#' not_enrolled_reason("Replace with Analytic Tibble")
#' 
not_enrolled_reason <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "study_id", "not_enrolled_reason", 
                           "pre_screened_notes"), 
    example_types = c("FacilityCode", "Number", "Character", 
                      "Character"))
  
  df <- analytic %>% select(facilitycode, study_id, not_enrolled_reason, pre_screened_notes) %>% 
    filter(!is.na(not_enrolled_reason)) %>% 
    rename(`Site` = facilitycode,
           `ID` = study_id,
           `Reason for Not Enrolling` = not_enrolled_reason,
           `Screening Notes` = pre_screened_notes)
  
  
  output <- kable(df, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}
                       

#' Outcome by Site
#'
#' @description 
#' Returns summary statistics on the number of the time to event data of each site for a specified outcome.
#' Output column "Percent of Target" comes from dividing the average outcome_days with the average target_days,
#' and "Percent of Expected" refers to dividing the average outcome_days with the average expected_days.
#'
#' @param analytic analytic data set that must include study_id, outcome_data, facilitycode, and enrolled
#' @param outcome_name name of the outcome to be considered in the visualization
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' outcome_by_site("Replace with Analytic Tibble", 'test_outcome')
#' 
outcome_by_site <- function(analytic, outcome_name) {
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('outcome_data', 'facilitycode', 'enrolled'), 
    example_types = c("(';', ',')NamedCategory['test_outcome']|Number|Number|Date|Date|NamedCategory['check' 'event']|Number|Number|Date", 'FacilityCode', 'Boolean'))
  
  # Extract the relevant outcome data
  outcome_data <- analytic %>%
    select(study_id, outcome_data, facilitycode, enrolled) %>%
    filter(enrolled) %>%
    # Split the outcome_data string
    separate_rows(outcome_data, sep=";") %>% 
    # Split each record into columns
    separate(
      outcome_data,
      c("outcome_name", "target_days", "expected_days", "time_zero", 
        "outcome_date_extended", "outcome_type", "outcome_days_extended", 
        "outcome_days", "outcome_date"),
      sep = ","
    ) %>%
    # Filter for the specific outcome
    filter(outcome_name == !!outcome_name) %>%
    filter((as.Date(time_zero)+365)<Sys.Date()) %>% 
    # Convert numeric columns
    mutate(
      target_days = as.numeric(target_days),
      expected_days = as.numeric(expected_days),
      outcome_days_extended = as.numeric(outcome_days_extended),
      outcome_days = as.numeric(outcome_days)
    )
  
  # Calculate overall statistics
  overall_stats <- outcome_data %>%
    summarise(
      n_total = n(),
      n_missing = sum(is.na(outcome_days)),
      min_days = min(outcome_days, na.rm = TRUE),
      max_days = max(outcome_days, na.rm = TRUE),
      avg_days = format_mean_sd(outcome_days, decimals = 0),
      pct_target = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(target_days, na.rm = TRUE) *100, 0), "%"),
      pct_expected = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%")
    ) %>%
    mutate(facilitycode = "Overall")
  
  # Calculate site-specific statistics
  site_stats <- outcome_data %>%
    group_by(facilitycode) %>%
    summarise(
      n_total = n(),
      n_missing = sum(is.na(outcome_days)),
      min_days = min(outcome_days, na.rm = TRUE),
      max_days = max(outcome_days, na.rm = TRUE),
      avg_days = format_mean_sd(outcome_days, decimals = 0),
      pct_target = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(target_days, na.rm = TRUE) *100, 0), "%"),
      pct_expected = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%")
    ) %>%
    ungroup() %>%
    mutate(order_col = as.numeric(str_remove(pct_expected,"%"))) %>% 
    arrange(desc(order_col)) %>% 
    select(-order_col)
  
  # Combine overall and site-specific statistics
  results <- bind_rows(overall_stats, site_stats)
  
  results <- results %>%
    rename(`N (Participants)` = n_total,
           `Missing Time to Event (Participants)` = n_missing,
           `Minimum (Days)` = min_days,
           `Maximum (Days)` = max_days,
           `Mean (Standard Deviation)` = avg_days,
           `Percent of Target` = pct_target,
           `Percent of Expected` = pct_expected,
           `Site` = facilitycode) %>%
    select(`Site`, `N (Participants)`, `Missing Time to Event (Participants)`, `Minimum (Days)`, `Maximum (Days)`, `Mean (Standard Deviation)`, `Percent of Target`, `Percent of Expected`)
  
  vis <- kable(results, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}

#' Outcome by Name Overall
#'
#' @description 
#' Returns summary statistics on the number of days to complete various follow-up forms.
#'
#' @param analytic This is the analytic data set that must include study_id, outcome_data, and enrolled
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' outcome_by_name_overall("Replace with Analytic Tibble")
#' 
outcome_by_name_overall <- function(analytic) {
  analytic <- if_needed_generate_example_data(analytic, 
                                              example_constructs = c('outcome_data', 'enrolled'), 
                                              example_types = c("(';', ',')NamedCategory['test_outcome']|Number|Number|Date|Date|NamedCategory['check' 'event']|Number|Number|Date", 'Boolean'))
  
  outcome_data <- analytic %>%
    select(study_id, outcome_data, enrolled) %>%
    filter(enrolled) %>%
    separate_rows(outcome_data, sep=";") %>%
    separate(outcome_data, c('outcome_name', 'target_days', 'expected_days', 'time_zero', 'outcome_date_extended', 'outcome_type', 'outcome_days_extended', 'outcome_days', 'outcome_date'), sep=",") %>% 
    filter((as.Date(time_zero)+365)<Sys.Date())

  stats <- outcome_data %>%
    mutate(outcome_days = as.numeric(outcome_days)) %>%
    mutate(target_days = as.numeric(target_days)) %>%
    mutate(expected_days = as.numeric(expected_days)) %>%
    group_by(outcome_name) %>%
    summarise(
      n_total = n(),
      n_missing = sum(is.na(outcome_days)),
      min_days = min(outcome_days, na.rm = TRUE),
      max_days = max(outcome_days, na.rm = TRUE),
      avg_days = format_mean_sd(outcome_days, decimals = 0),
      pct_target = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(target_days, na.rm = TRUE) *100, 0), "%"),
      pct_expected = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%")
    )

  results <- stats %>%
    rename(`N (Participants)` = n_total,
           `Missing Time to Event (Participants)` = n_missing,
           `Minimum (Days)` = min_days,
           `Maximum (Days)` = max_days,
           `Mean (Standard Deviation)` = avg_days,
           `Percent of Target` = pct_target,
           `Percent of Expected` = pct_expected,
           `Outcome` = outcome_name) %>%
    select(`Outcome`, `N (Participants)`, `Missing Time to Event (Participants)`, `Minimum (Days)`, `Maximum (Days)`, `Mean (Standard Deviation)`, `Percent of Target`, `Percent of Expected`)

  # cleanup the names of the outcomes by replacing the underscores with spaces and capitalizing the first letter
  results <- results %>%
    mutate(Outcome = str_replace_all(Outcome, "_", " "))
  
  vis <- kable(results, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}
