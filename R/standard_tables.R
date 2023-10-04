


#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, early_withdraw, days_site_certified, 
#' facilitycode, late_ineligible, inappropriate_enrollment, late_refusal
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
    select(screened, eligible, refused, consented, enrolled, not_consented, early_withdraw, days_site_certified, 
           facilitycode, late_ineligible, inappropriate_enrollment, late_refusal) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(disc_post = ifelse(late_ineligible == TRUE | inappropriate_enrollment == TRUE, TRUE, FALSE)) %>% 
    select(-late_ineligible, -inappropriate_enrollment) %>% 
    rename(disc_pre = early_withdraw) %>% 
    mutate(days_site_certified = as.numeric(Sys.Date() - as.Date(days_site_certified))) %>% 
    rename(Facility = facilitycode) %>% 
    rename(not_enrolled = not_consented) %>% 
    filter(!is.na(Facility))
  
  
  df_1st <- df %>% 
    group_by(Facility) %>% 
    summarize('Days Certified' = days_site_certified[1], Screened = sum(screened), Eligible = sum(eligible))
  
  df_2nd <- df %>% 
    filter(eligible == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(Refused = sum(refused), 'Not Enrolled for Other Reasons' = sum(not_enrolled), Consented = sum(consented))
  
  df_3rd <- df %>% 
    filter(eligible == TRUE & consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize("Discontinued Pre-Randomization" = sum(disc_pre),"Discontinued Post-Randomization" = sum(disc_post), 
              "Late Refused" = sum(late_refusal), "Eligible and Enrolled" = sum(enrolled)) 
  
  table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
    left_join(df_3rd, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    arrange(desc(`Days Certified`), desc(Screened))
  
  table<- kable(table_raw, align='l', padding='2l') %>% 
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 4)) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}





#' Expected, completed, missing, out of window visits by each form
#'
#' @description This function visualizes the expected visits for each timepoint for MRR, CFU, PFU, BPI, AOS, KOOS forms
#'
#' @param analytic This is the analytic data set that must include mrr_status_6wk, mrr_status_3mo, mrr_status_6mo, 
#' mrr_status_12mo, cfu_status_6wk, cfu_status_3mo, cfu_status_6mo, cfu_status_12mo, pfu_status_6wk, pfu_status_3mo, 
#' pfu_status_6mo, pfu_status_12mo, bpi_status_6wk, bpi_status_3mo, bpi_status_6mo,bpi_status_12mo, aos_status_6wk, 
#' aos_status_3mo, aos_status_6mo, aos_status_12mo, koos_status_6wk, koos_status_3mo, koos_status_6mo, koos_status_12mo, 
#' sixweek_visit_expected, threemonth_visit_expected, sixmonth_visit_expected, twelvemonth_visit_expected, injury_type
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
           cfu_status_12mo, pfu_status_6wk, pfu_status_3mo, pfu_status_6mo, pfu_status_12mo, bpi_status_6wk, bpi_status_3mo, bpi_status_6mo, 
           bpi_status_12mo, aos_status_6wk, aos_status_3mo, aos_status_6mo, aos_status_12mo, koos_status_6wk, koos_status_3mo, koos_status_6mo, 
           koos_status_12mo)
  
  df_expected <- analytic %>% 
    select(sixweek_visit_expected, threemonth_visit_expected, sixmonth_visit_expected, twelvemonth_visit_expected) %>% 
    summarise("12 Months"= sum(twelvemonth_visit_expected, na.rm = TRUE), "3 Months"= sum(threemonth_visit_expected, na.rm = TRUE),
              "6 Months"= sum(sixmonth_visit_expected, na.rm = TRUE),  "6 Weeks"= sum(sixweek_visit_expected, na.rm = TRUE)) %>% 
    mutate(Form = "Enrolled") %>% 
    mutate(Status = "Expected") 
  
  df_injury_expected <- analytic %>% 
    select(sixweek_visit_expected, threemonth_visit_expected, sixmonth_visit_expected, twelvemonth_visit_expected, injury_type) %>%
    group_by(injury_type) %>% 
    summarise("12 Months"= sum(twelvemonth_visit_expected, na.rm = TRUE), "3 Months"= sum(threemonth_visit_expected, na.rm = TRUE),
              "6 Months"= sum(sixmonth_visit_expected, na.rm = TRUE),  "6 Weeks"= sum(sixweek_visit_expected, na.rm = TRUE)) %>% 
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
    pack_rows(index = index_vec) %>% 
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
#' @param analytic This is the analytic data set that must include injury_type, ankle_ota_class, schatzker_type, enrolled
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
    select(injury_type, ankle_ota_class, schatzker_type, enrolled) %>%  filter(enrolled == TRUE)
  
  summary_totals <- df %>%
    filter(injury_type == "plateau" & is.na(ankle_ota_class) | injury_type == "ankle" & is.na(schatzker_type)) %>%
    group_by(injury_type, ankle_ota_class, schatzker_type) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    mutate(ankle_ota_class = ifelse(injury_type == "ankle" & is.na(ankle_ota_class) & is.na(schatzker_type), "Missing", ankle_ota_class)) %>% 
    mutate(schatzker_type = ifelse(injury_type == "plateau" & is.na(ankle_ota_class) & is.na(schatzker_type), "Missing", schatzker_type)) %>% 
    select(-injury_type) %>% 
    mutate(Name = ifelse(!is.na(ankle_ota_class), ankle_ota_class, schatzker_type)) %>% 
    mutate(Category = ifelse(!is.na(ankle_ota_class), "O", "T")) %>% 
    select(-ankle_ota_class, -schatzker_type)
  
  injury_type_total <- df %>% 
    group_by(injury_type) %>% 
    summarise(Total = n()) %>%
    ungroup() %>% 
    rename(Name = injury_type) %>% 
    mutate(Category = ifelse(Name == "ankle", "A", "P")) 
  
  total_sum <- sum(injury_type_total$Total)
  
  summary_table <- bind_rows(injury_type_total, summary_totals) %>% 
    arrange(Category) %>% 
    mutate(Name = ifelse(Name == "ankle", "Number of Ankles", 
                         ifelse(Name == "plateau", "Number of Plateaus", Name))) %>% 
    mutate(Total = format_count_percent(Total, total_sum, decimals = 2))
  
  ota_number <- summary_table %>% 
    filter(Category == "O") %>% 
    nrow()
  
  schatzer_number <- summary_table %>% 
    filter(Category == "T") %>% 
    nrow()
  
  
  df_table <- summary_table %>% 
    select(-Category)
  
  index_vec <- c(" "= 1,"OTA Classification"= ota_number, " "= 1, "Tibial Plateau"=schatzer_number) 
  
  table_raw<- kable(df_table, align='l', padding='2l', col.names = NULL) %>%
    pack_rows(index = index_vec) %>% 
    kable_styling("striped", full_width = F, position="left")
  
  return(table_raw)
}

#' Baseline Characteristics Percent 
#'
#' @description This function visualizes the categorical percentages of baseline characteristics sex, age, race, education, and military
#'
#' @param analytic This is the analytic data set that must include enrolled, age, age_group
#' @param sex is a meta construct that is required that defaults to "sex"
#' @param race is a meta construct that is required that defaults to "race_ethnicity"
#' @param education is a meta construct that is required that defaults to "education_level"
#' @param military is a meta construct that is required that defaults to "military_status"
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' baseline_characteristics_percent()
#' }
baseline_characteristics_percent <- function(analytic, sex="sex", race="race_ethnicity", education="education_level", military="military_status"){
  constructs <- c(sex, race, education, military)
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
    rename(type = sex) 
  
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
    rename(type = education)
  
  race_df <- df %>% 
    mutate(race = replace_na(race, "Missing")) %>% 
    group_by(race) %>% 
    count(race) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, total)) %>% 
    select(-number) %>% 
    rename(type = race)
  
  military_df <- df %>% 
    mutate(military = ifelse(is.na(military), "Missing", military)) %>% 
    group_by(military) %>% 
    count(military) %>% 
    rename(number = n) %>% 
    mutate(percentage = format_count_percent(number, total)) %>% 
    select(-number) %>% 
    rename(type = military)
  
  df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df, military_df) 
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  
  vis <- kable(df_final, align='l', padding='2l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Sex' = nrow(sex_df), 'Age' = (nrow(age_df) + nrow(age_group_df)), 'Race' = nrow(race_df), 
                        'Education' = nrow(education_df), 'Military' = nrow(military_df))) %>% 
    kable_styling("striped", full_width = F, position="left") 
  
 return(vis) 
} 

#' Number of Discontinued Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type
#'
#' @param analytic This is the analytic data set that must include enrolled, study_discontinuation, 
#' deviation_screen_consent, deviation_procedural, deviation_administrative, sae_reported
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' discontinuation_sae_deviation_by_type()
#' }
discontinuation_sae_deviation_by_type <- function(analytic){
  df <- analytic %>% 
    select(enrolled, study_discontinuation, deviation_screen_consent, deviation_procedural, deviation_administrative, sae_reported) %>% 
    filter(enrolled == TRUE) %>% 
    mutate(na_count = rowSums(is.na(select(., 
                                           study_discontinuation,
                                           deviation_screen_consent,
                                           deviation_procedural,
                                           deviation_administrative,
                                           sae_reported)))) %>%
    filter(na_count != 5) %>%
    select(-na_count) %>% 
    mutate(sae_reported = ifelse(sae_reported == TRUE, 'SAE', sae_reported))
  
  total <- sum(df$enrolled)
  
  totals_df <- df %>%
    mutate(total_disc = ifelse(!is.na(study_discontinuation), TRUE, FALSE)) %>% 
    mutate(total_dsc = ifelse(!is.na(deviation_screen_consent), TRUE, FALSE)) %>% 
    mutate(total_dp = ifelse(!is.na(deviation_procedural), TRUE, FALSE)) %>% 
    mutate(total_da = ifelse(!is.na(deviation_administrative), TRUE, FALSE)) %>% 
    mutate(total_sae = ifelse(!is.na(sae_reported), TRUE, FALSE)) %>% 
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
  
  deviation_screen_consent_df <- df %>% 
    select(deviation_screen_consent) %>% 
    filter(!is.na(deviation_screen_consent)) %>% 
    count(deviation_screen_consent) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = deviation_screen_consent)
  
  deviation_procedural_df <- df %>% 
    select(deviation_procedural) %>% 
    filter(!is.na(deviation_procedural)) %>% 
    count(deviation_procedural) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = deviation_procedural)
  
  deviation_administrative_df <- df %>% 
    select(deviation_administrative) %>% 
    filter(!is.na(deviation_administrative)) %>% 
    count(deviation_administrative) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = deviation_administrative)
  
  sae_reported_df <- df %>% 
    select(sae_reported) %>% 
    filter(!is.na(sae_reported)) %>% 
    count(sae_reported) %>% 
    mutate(percentage = format_count_percent(n, total)) %>% 
    select(-n) %>% 
    rename(type = sae_reported)
  
  df_final <- rbind(disc, study_discontinuation_df, sae_reported_df, protocol_deviations, sc, deviation_screen_consent_df, 
                    dp, deviation_procedural_df, da, deviation_administrative_df) 
  
  n_disc <- nrow(study_discontinuation_df)
  n_dsc <- nrow(deviation_screen_consent_df)
  n_dp <- nrow(deviation_procedural_df)
  n_da <- nrow(deviation_administrative_df)
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  vis <- kable(df_final, align='l', padding='2l', col.names = NULL) %>%
    add_header_above(header) %>%  
    add_indent(c(seq(n_disc) + 1, seq(1 + n_dsc + 1 + n_dp + 1 + n_da) + 1 + n_disc + 2)) %>% 
    add_indent(na.omit(c(ifelse(rep(n_dsc,n_dsc)>0,seq(n_dsc) + 1 + n_disc + 1 + 1 + 1,NA),
                 ifelse(rep(n_dp,n_dp)>0, seq(n_dp) + 1 + n_disc + 1 + 1 + 1 + n_dsc + 1,NA),
                 ifelse(rep(n_da,n_da)>0, seq(n_da) + 1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1,NA)))) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1+ n_disc, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 1px solid") %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis)
}

