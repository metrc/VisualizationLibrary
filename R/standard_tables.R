


#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, days_site_certified, 
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
    select(screened, eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, days_site_certified, 
           facilitycode, late_ineligible) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
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
    summarize("Discontinued Pre-Randomization" = sum(discontinued_pre_randomization),
              "Late Ineligible" = sum(late_ineligible), 
              "Enrolled" = sum(enrolled)) 
  
  table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
    left_join(df_3rd, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total)
  
  table<- kable(table_raw, align='l', padding='2l') %>% 
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 3)) %>%
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
#' @param race is a meta construct that is required that defaults to "race_ethnicity"
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
baseline_characteristics_percent <- function(analytic, sex="sex", race="race_ethnicity", education="education_level", military="military_status",
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
#'
#' @param analytic This is the analytic data set that must include enrolled, enrolled_discontinuation_reason, 
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
  total <- sum(analytic$enrolled, na.rm=T)
  discontinuation_df <- analytic %>% 
    select(enrolled, enrolled_discontinuation_reason) %>% 
    filter(enrolled == TRUE) %>% 
    count(enrolled_discontinuation_reason) %>%
    rename(type=enrolled_discontinuation_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  discontinuation_df_tot <- tibble(type="Discontinuations", n=sum(discontinuation_df$n))
  
  sae_df <- analytic %>% 
    select(study_id, enrolled, sae_reported) %>% 
    filter(enrolled & sae_reported>0) %>% 
    mutate(sae_reported = "SAE") %>% 
    count(sae_reported) %>%
    rename(type=sae_reported) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_sc_df <- analytic %>% 
    select(study_id, enrolled, deviation_screen_consent) %>% 
    filter(enrolled == TRUE) %>% 
    count(deviation_screen_consent) %>%
    rename(type=deviation_screen_consent) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_p_df <- analytic %>% 
    select(study_id, enrolled, deviation_procedural) %>% 
    filter(enrolled == TRUE) %>% 
    count(deviation_procedural) %>%
    rename(type=deviation_procedural) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  
  deviation_a_df <- analytic %>% 
    select(study_id, enrolled, deviation_administrative) %>% 
    filter(enrolled == TRUE) %>% 
    count(deviation_administrative) %>%
    rename(type=deviation_administrative) %>% 
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
ineligibility_by_reasons <- function(analytic){
  
  
  df <- analytic %>% 
    select(study_id, facilitycode,  screened, ineligible, ineligibility_reasons) %>% 
    filter(screened == TRUE) 
  
  reasons <- df %>%  select(study_id, facilitycode, ineligibility_reasons) %>% 
    column_unzipper('ineligibility_reasons', sep = '; ') %>% 
    boolean_column_counter() %>% 
    pivot_longer(everything()) %>% 
    arrange(desc(value)) %>% 
    slice(1:5) %>% 
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
    arrange(desc(Screened))

  vis <- kable(output, align='l', padding='2l') %>%
    add_header_above(c(" " = 3, "Top 5 Ineligibility Reasons" = 5, " " = 1)) %>%  
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis)
}


#' AO Gustillo Tscherne Injury Characteristics
#'
#' @description This function visualizes the injury characteristics
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' injury_gustilo, injury_tscherne, injury_ao_class
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
    select(injury_gustilo, injury_tscherne, injury_ao_class)
  
  inj_gust <- pull %>% 
    count(injury_gustilo) %>%
    pivot_longer(-n) %>%
    mutate(value=ifelse(!is.na(value), paste('Gustilo Type', value), value)) %>%
    mutate(value=ifelse(is.na(value), 'Unknown', value)) %>%
    select(-name)
  
  inj_ao <- pull %>% 
    count(injury_ao_class) %>%
    pivot_longer(-n) %>%
    mutate(value=ifelse(is.na(value), 'Unknown', value)) %>%
    select(-name)
  
  inj_tsch <- pull %>% 
    count(injury_tscherne) %>%
    pivot_longer(-n) %>%
    mutate(value=ifelse(!is.na(value), paste('Tscherne Gotzen Grade', value), value)) %>%
    mutate(value=ifelse(is.na(value), 'Unknown', value)) %>%
    select(-name)
  
  combined <- bind_rows(inj_tsch, inj_gust, inj_ao) %>%
    relocate(n, .after=value) %>%
    rename('Fracture Type'=value, 'n='=n)
  
  output<- kable(combined, align='l', padding='2l') %>% 
    kable_styling("condensed", position="left") %>%
    pack_rows("Closed Fracture", 1, nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("Open Fracture", nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("AO Class", nrow(inj_gust)+nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch)+nrow(inj_ao), label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}


#' Status of IRB Approvals and Certification by Site
#'
#' @description This function returns a list of sites and their dates of 
#' local, DOD, and METRC certifications
#'
#' @param analytic This is the analytic data set that must include sites_certification_dates
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
    select(sites_certification_dates) %>%
    unique()
  
  date_today <- Sys.Date()
  
  cols <- c('Facility', 'Local (or sIRB)  Approval Date', 'DoD Approval Date',
            'Certified by MCC to Start Screening', 
            paste0('Days Number of Days Certified (as of ', date_today, ')'))
  
  site_data <- df %>%
    separate(sites_certification_dates, cols, sep = ';')
  
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
  
  severity_categories <- c('Grade 2,1', 'Grade 3', 'Grade 4', 'Grade Unknown')
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
