


#' Closed Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include treatment_arm screened, 
#' eligible, refused, consented, enrolled, not_consented, early_withdraw, days_site_certified, 
#' facilitycode, late_ineligible, inappropriate_enrollment, late_refusal
#'
#' @return nothing
#' @export
#'
#' @examples
#' dontrun{
#' closed_enrollment_status_by_site()
#' }
closed_enrollment_status_by_site <- function(analytic){
  df_a <- analytic %>% 
    select(treatment_arm, screened, eligible, refused, consented, enrolled, not_consented, early_withdraw, days_site_certified, 
           facilitycode, late_ineligible, inappropriate_enrollment, late_refusal) %>% 
    filter(treatment_arm=="Group A") %>% 
    select(-treatment_arm)
  
  df_b <- analytic %>% 
    select(treatment_arm, screened, eligible, refused, consented, enrolled, not_consented, early_withdraw, days_site_certified, 
           facilitycode, late_ineligible, inappropriate_enrollment, late_refusal) %>% 
    filter(treatment_arm=="Group B") %>% 
    select(-treatment_arm)
  
  df_full <- analytic %>% 
    select(screened, eligible, refused, consented, enrolled, not_consented, early_withdraw, days_site_certified, 
           facilitycode, late_ineligible, inappropriate_enrollment, late_refusal)
  
  inner_closed_enrollment_status_by_site <- function(input_df){
    df <- input_df %>% 
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
    
    table_out <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
      left_join(df_3rd, by = 'Facility') %>% 
      mutate_all(~ifelse(is.na(.), 0, .)) %>% 
      arrange(desc(`Days Certified`), desc(Screened))
    return(table_out)
  }
    
  table_a <- inner_closed_enrollment_status_by_site(df_a)
  table_b <- inner_closed_enrollment_status_by_site(df_b)
  table_full <- inner_closed_enrollment_status_by_site(df_full)
  
  table_raw <- full_join(full_join(table_a, table_b, by=c("Facility", "Days Certified")), 
                         table_full, by=c("Facility", "Days Certified")) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total)
  
  col_ns <- c(colnames(table_full),
              colnames(table_full)[3:ncol(table_full)],
              colnames(table_full)[3:ncol(table_full)])
  
  table<- kable(table_raw, align='l', padding='2l', col.names = col_ns) %>% 
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 4,
                       " " = 2, "Among Eligible" = 3, "Among Consented" = 4,
                       " " = 2, "Among Eligible" = 3, "Among Consented" = 4)) %>%
    add_header_above(c(" " = 2, "Group A" = 9, "Group B" = 9, "All" = 9)) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}





#' Closed Expected, completed, missing, out of window visits by each form
#'
#' @description This function visualizes the expected visits for each timepoint for MRR, CFU, PFU, BPI, AOS, KOOS forms
#'
#' @param analytic This is the analytic data set that must include treatment_arm mrr_status_6wk, mrr_status_3mo, mrr_status_6mo, 
#' mrr_status_12mo, cfu_status_6wk, cfu_status_3mo, cfu_status_6mo, cfu_status_12mo, pfu_status_6wk, pfu_status_3mo, 
#' pfu_status_6mo, pfu_status_12mo, bpi_status_6wk, bpi_status_3mo, bpi_status_6mo,bpi_status_12mo, aos_status_6wk, 
#' aos_status_3mo, aos_status_6mo, aos_status_12mo, koos_status_6wk, koos_status_3mo, koos_status_6mo, koos_status_12mo, 
#' sixweek_visit_expected, threemonth_visit_expected, sixmonth_visit_expected, twelvemonth_visit_expected, injury_type
#'
#' @return nothing
#' @export
#'
#' @examples
#' dontrun{
#' closed_visit_status_for_followup_by_form()
#' }
closed_visit_status_for_followup_by_form <- function(analytic){
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  out <- paste0("<h4>Group A</h4><br />",
                visit_status_for_followup_by_form(df_a),
                "<h4>Group B</h4><br />",
                visit_status_for_followup_by_form(df_b))
                
  return(out)
}


#' Closed Injury characteristics for OTA classification and Schatzker Type injuries
#'
#' @description This function visualizes the Injury characteristics for OTA classification and Schatzker Types for Ankle and Plateau
#' injuries
#'
#' @param analytic This is the analytic data set that must include treatment_arm injury_type, ankle_ota_class, schatzker_type, enrolled
#'
#' @return nothing
#' @export
#'
#' @examples
#' dontrun{
#' closed_injury_ankle_plateau_characteristics()
#' }
closed_injury_ankle_plateau_characteristics <- function(analytic){
  
  df <- analytic %>% 
    select(injury_type, ankle_ota_class, schatzker_type, enrolled) %>%  
    filter(enrolled == TRUE)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A") %>% 
    select(injury_type, ankle_ota_class, schatzker_type, enrolled) %>%  
    filter(enrolled == TRUE)
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B") %>% 
    select(injury_type, ankle_ota_class, schatzker_type, enrolled) %>%  
    filter(enrolled == TRUE)
  
  inner_closed_injury_ankle_plateau_characteristics <- function(df){
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
    
    return(df_table)
  }
  
  table_a <- inner_closed_injury_ankle_plateau_characteristics(df_a)
  table_b <- inner_closed_injury_ankle_plateau_characteristics(df_b)
  table_full <- inner_closed_injury_ankle_plateau_characteristics(df)
  
  df_table <- full_join(full_join(table_a, table_b, by="Name"), 
                        table_full, by="Name")
  
  colnames(df_table) <- c("Name", "Group A", "Group B", "Total")
  
  
  index_vec <- c(" "= 1,"OTA Classification"= ota_number, " "= 1, "Tibial Plateau"=schatzer_number) 
  
  table_raw<- kable(df_table, align='l', padding='2l', col.names = NULL) %>%
    pack_rows(index = index_vec) %>% 
    kable_styling("striped", full_width = F, position="left")
  
  return(table_raw)
}

#' Closed Baseline Characteristics Percent 
#'
#' @description This function visualizes the categorical percentages of baseline characteristics sex, age, race, education, and military
#'
#' @param analytic This is the analytic data set that must include treatment_arm enrolled, age, age_group
#' @param sex is a meta construct that is required that defaults to "sex"
#' @param race is a meta construct that is required that defaults to "race_ethnicity"
#' @param education is a meta construct that is required that defaults to "education_level"
#' @param military is a meta construct that is required that defaults to "military_status"
#'
#' @return nothing
#' @export
#'
#' @examples
#' dontrun{
#' closed_baseline_characteristics_percent()
#' }
closed_baseline_characteristics_percent <- function(analytic, sex="sex", race="race_ethnicity", education="education_level", military="military_status",
                                                    sex_levels=c("Female","Male", "Missing"), 
                                                    race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                                    education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don’t Know", "Missing"), 
                                                    military_levels=c("Active Military", "Active Reserves", "Not Active Duty","Missing")){
  
  constructs <- c(sex, race, education, military)
  
  sex_default <- tibble(type=sex_levels)
  race_default <- tibble(type=race_levels)
  education_default <- tibble(type=education_levels)
  military_default <- tibble(type=military_levels)
  
  sex_n <- -1
  age_n <- -1
  race_n <- -1
  education_n <- -1
  military_n <- -1
  
  df <- analytic %>% 
    filter(enrolled == TRUE)
  
  total <- sum(df$enrolled, na.rm=T)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A") %>% 
    filter(enrolled == TRUE)
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B") %>% 
    filter(enrolled == TRUE)
  
  inner_closed_baseline_characteristics_percent <- function(analytic){
    
    df <- analytic %>% 
      select(enrolled, age_group, age, all_of(constructs)) %>% 
      filter(enrolled) %>% 
      rename(sex = !!sym(sex)) %>% 
      rename(race = !!sym(race)) %>% 
      rename(education = !!sym(education)) %>% 
      rename(military = !!sym(military)) %>% 
      mutate(age = as.numeric(age)) %>% 
      mutate(df = "age")
    
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
      select(-order) %>% 
      mutate(df = "sex")
    
    age_df <- df %>% 
      summarize( type = 'Mean (SD)', percentage = format_mean_sd(age)) %>% 
      mutate(df = "age_n")
    
    
    age_group_df <- df %>% 
      mutate(age_group = replace_na(age_group, "Missing")) %>% 
      group_by(age_group) %>% 
      count(age_group) %>% 
      rename(number = n) %>% 
      mutate(percentage = format_count_percent(number, total)) %>% 
      select(-number) %>% 
      rename(type = age_group) %>% 
      mutate(df = "age")
    
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
      select(-order) %>% 
      mutate(df = "education")
    
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
      select(-order) %>% 
      mutate(df = "race")
    
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
      select(-order) %>% 
      mutate(df = "military")
    
    df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df, military_df) %>% 
      mutate() 
    
    sex_n <<- nrow(sex_df)
    age_n <<- nrow(age_group_df)
    race_n <<- nrow(race_df)
    education_n <<- nrow(education_df)
    military_n <<- nrow(military_df)
    
    return(df_final)
  }
  
  table_a <- inner_closed_baseline_characteristics_percent(df_a)
  table_b <- inner_closed_baseline_characteristics_percent(df_b)
  table_full <- inner_closed_baseline_characteristics_percent(df)
  
  table_final <- left_join(table_full, full_join(table_a, table_b, by=c("df","type")), 
                                       by=c("df","type")) %>% 
    select(-df) %>% 
    mutate_all(replace_na, "0 (0%)") %>% 
    select(type, percentage.x,  percentage.y, percentage)
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  vis <- kable(table_final, align='l', padding='2l', 
               col.names = c(" ","Group A", "Group B", "Total")) %>% 
    pack_rows(index = c('Sex' = sex_n, 'Age' = 1 + age_n, 'Race' = race_n, 
                        'Education' = education_n, 'Military' = military_n)) %>% 
    kable_styling("striped", full_width = F, position="left") 
  
 return(vis) 
} 


#' Closed Number of Discontinued Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type
#'
#' @param analytic This is the analytic data set that must include treatment_arm screened, study_discontinuation, 
#' deviation_screen_consent, deviation_procedural, deviation_administrative, sae_reported
#'
#' @return nothing
#' @export
#'
#' @examples
#' dontrun{
#' closed_discontinuation_sae_deviation_by_type()
#' }
closed_discontinuation_sae_deviation_by_type <- function(analytic){
  
  df_full <- analytic
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  n_disc <- -1
  n_dsc <- -1
  n_dp <- -1
  n_da <- -1
  total <- sum(analytic$screened, na.rm=T)
  
  inner_closed_discontinuation_sae_deviation_by_type <- function(analytic){
  
    df <- analytic %>% 
      select(screened, study_discontinuation, deviation_screen_consent, deviation_procedural, deviation_administrative, sae_reported) %>% 
      filter(screened == TRUE) %>% 
      mutate(na_count = rowSums(is.na(select(., 
                                             study_discontinuation,
                                             deviation_screen_consent,
                                             deviation_procedural,
                                             deviation_administrative,
                                             sae_reported)))) %>%
      filter(na_count != 5) %>%
      select(-na_count) %>% 
      mutate(sae_reported = ifelse(sae_reported == TRUE, 'SAE', sae_reported))
    
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
    
    vec_disc <- c(format_count_percent(total_disc, total, decimals = 2))
    vec_protocol_deviations <- c(format_count_percent(total_dsc + total_dp + total_da, total, decimals = 2))
    vec_dsc <- c(format_count_percent(total_dsc, total, decimals = 2))
    vec_dp <- c(format_count_percent(total_dp, total, decimals = 2))
    vec_da <- c(format_count_percent(total_da, total, decimals = 2))
    
    
    disc <- tibble(type = "Discontinuous", percentage = vec_disc)
    protocol_deviations <- tibble(type = 'Protocol Deviations', percentage = vec_protocol_deviations)
    sc <- tibble(type = 'Screen and Consent', percentage = vec_dsc)
    dp <- tibble(type = 'Procedural', percentage = vec_dp)
    da <- tibble(type = 'Administrative/Other', percentage = vec_da)
    
    
    study_discontinuation_df <- df %>% 
      select(study_discontinuation) %>% 
      filter(!is.na(study_discontinuation)) %>% 
      count(study_discontinuation) %>% 
      mutate(percentage = format_count_percent(n, total, decimals = 2)) %>% 
      select(-n) %>% 
      rename(type = study_discontinuation)
    
    deviation_screen_consent_df <- df %>% 
      select(deviation_screen_consent) %>% 
      filter(!is.na(deviation_screen_consent)) %>% 
      count(deviation_screen_consent) %>% 
      mutate(percentage = format_count_percent(n, total, decimals = 2)) %>% 
      select(-n) %>% 
      rename(type = deviation_screen_consent)
    
    deviation_procedural_df <- df %>% 
      select(deviation_procedural) %>% 
      filter(!is.na(deviation_procedural)) %>% 
      count(deviation_procedural) %>% 
      mutate(percentage = format_count_percent(n, total, decimals = 2)) %>% 
      select(-n) %>% 
      rename(type = deviation_procedural)
    
    deviation_administrative_df <- df %>% 
      select(deviation_administrative) %>% 
      filter(!is.na(deviation_administrative)) %>% 
      count(deviation_administrative) %>% 
      mutate(percentage = format_count_percent(n, total, decimals = 2)) %>% 
      select(-n) %>% 
      rename(type = deviation_administrative)
    
    sae_reported_df <- df %>% 
      select(sae_reported) %>% 
      filter(!is.na(sae_reported)) %>% 
      count(sae_reported) %>% 
      mutate(percentage = format_count_percent(n, total, decimals = 2)) %>% 
      select(-n) %>% 
      rename(type = sae_reported)
    
    df_final <- rbind(disc, study_discontinuation_df, sae_reported_df, protocol_deviations, sc, deviation_screen_consent_df, 
                      dp, deviation_procedural_df, da, deviation_administrative_df) 
    
    n_disc <<- nrow(study_discontinuation_df)
    n_dsc <<- nrow(deviation_screen_consent_df)
    n_dp <<- nrow(deviation_procedural_df)
    n_da <<- nrow(deviation_administrative_df)
    
    return(df_final)
  }
  
  table_a <- inner_closed_discontinuation_sae_deviation_by_type(df_a)
  table_b <- inner_closed_discontinuation_sae_deviation_by_type(df_b)
  table_full <- inner_closed_discontinuation_sae_deviation_by_type(df_full)
  
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
  
  df_table <- full_join(full_join(table_a, table_b, by="type"), 
                        table_full, by="type") %>% 
    mutate_all(replace_na, "0 (0%)")
  
  
  vis <- kable(df_final, align='l', padding='2l', col.names = c(" ", "Group A", "Group B", "Total")) %>%
    add_header_above(header) %>%  
    add_indent(c(seq(n_disc) + 1, seq(1 + n_dsc + 1 + n_dp + 1 + n_da) + 1 + n_disc + 2, na.omit(c(dsc_indents, dp_indents, da_indents)))) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1+ n_disc, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 1px solid") %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis)
}

