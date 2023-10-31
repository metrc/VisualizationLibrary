


#' Closed Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include treatment_arm screened, 
#' eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, days_site_certified, 
#' facilitycode, late_ineligible
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_enrollment_status_by_site()
#' }
closed_enrollment_status_by_site <- function(analytic){
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  out <- paste0("<h4>Group A</h4><br />",
                enrollment_status_by_site(df_a),
                "<h4>Group B</h4><br />",
                enrollment_status_by_site(df_b))
  return(out)
}



#' Closed Number of Subjects Screened, Eligible, Enrolled and Not Enrolled (Variable Discontinued)
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, days_site_certified, facilitycode
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_enrollment_status_by_site_var_discontinued ()
#' }
closed_enrollment_status_by_site_var_discontinued <- function(analytic, discontinued="discontinued", discontinued_colname="Discontinued"){
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  out <- paste0("<h4>Group A</h4><br />",
                enrollment_status_by_site_var_discontinued(df_a, discontinued=discontinued, discontinued_colname=discontinued_colname),
                "<h4>Group B</h4><br />",
                enrollment_status_by_site_var_discontinued(df_b, discontinued=discontinued, discontinued_colname=discontinued_colname))
  return(out)
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
#' \dontrun{
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
#' \dontrun{
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
  
  ota_number <- -1
  schatzer_numbe <- -1
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
    
    ota_number <<- summary_table %>% 
      filter(Category == "O") %>% 
      nrow()
    
    schatzer_number <<- summary_table %>% 
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
  
  
  index_vec <- c("OTA Classification"= ota_number+1, "Tibial Plateau"=schatzer_number+1) 
  
  table_raw<- kable(df_table, align='l', padding='2l', col.names = NULL) %>%
    add_indent(c(seq(ota_number)+1, seq(schatzer_number)+1+ota_number+1)) %>% 
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
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
#' closed_baseline_characteristics_percent()
#' }
closed_baseline_characteristics_percent <- function(analytic, sex="sex", race="race_ethnicity", education="education_level", military="military_status",
                                                    sex_levels=c("Female","Male", "Missing"), 
                                                    race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                                    education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing"), 
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
                        'Education' = education_n, 'Military' = military_n),
              label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
 return(vis) 
} 


#' Closed Number of Discontinued Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type
#'
#' @param analytic This is the analytic data set that must include treatment_arm enrolled, enrolled_discontinuation, 
#' deviation_screen_consent, deviation_procedural, deviation_administrative, sae_reported
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
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
  total <- sum(analytic$enrolled, na.rm=T)
  
  inner_closed_discontinuation_sae_deviation_by_type <- function(analytic){
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
    
    
    df_final <- bind_rows(discontinuation_df_tot %>% mutate(group=as.numeric(rep(1,length(type)))), 
                          discontinuation_df %>% mutate(group=as.numeric(rep(2,length(type)))),
                          sae_df %>% mutate(group=as.numeric(rep(3,length(type)))),
                          deviation_df_tot %>% mutate(group=as.numeric(rep(4,length(type)))), 
                          deviation_sc_tot %>% mutate(group=as.numeric(rep(5,length(type)))),
                          deviation_sc_df %>% mutate(group=as.numeric(rep(6,length(type)))),
                          deviation_p_tot %>% mutate(group=as.numeric(rep(7,length(type)))),
                          deviation_p_df %>% mutate(group=as.numeric(rep(8,length(type)))), 
                          deviation_a_tot %>% mutate(group=as.numeric(rep(9,length(type)))), 
                          deviation_a_df %>% mutate(group=as.numeric(rep(10,length(type))))) %>% 
      mutate(n = format_count_percent(n, total, decimals=2))
    
    
    n_disc <<- nrow(discontinuation_df)
    n_dsc <<- nrow(deviation_sc_df)
    n_dp <<- nrow(deviation_p_df)
    n_da <<- nrow(deviation_a_df)
    
    return(df_final)
  }
  
  table_a <- inner_closed_discontinuation_sae_deviation_by_type(df_a)
  table_b <- inner_closed_discontinuation_sae_deviation_by_type(df_b)
  table_full <- inner_closed_discontinuation_sae_deviation_by_type(df_full) 
  table_full <- table_full %>% 
    mutate(o = seq(nrow(table_full)))
  
  df_table <- full_join(full_join(table_a, table_b, by=c("type","group")), 
                        table_full, by=c("type","group")) %>% 
    arrange(o) %>% 
    select(-o,-group) %>% 
    mutate_all(replace_na, "0 (0%)")
  
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

  vis <- kable(df_table, align='l', padding='2l', col.names = c(paste0("n=",total), "Group A", "Group B", "Total")) %>%
    add_indent(c(seq(n_disc) + 1, 1 + n_disc + 1 + 1 + seq(1+n_dsc+1+n_dp+1+n_da))) %>% 
    add_indent(indents_vec) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1+ n_disc, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 1px solid") %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis)
}


#' Closed Complications by severity and relatedness
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
#' closed_complications_by_severity_relatedness()
#' }
closed_complications_by_severity_relatedness <- function(analytic){
  
  inner_closed_complications_by_severity_relatedness <- function(analytic){
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
      select(complications, everything())
    
    if(sum(df_template$severity!=output_complication$severity) > 0 | 
       sum(df_template$complications!=output_complication$complications) >0 ){
      stop("bad complications alignment")
    }
    
    return(output)
  }
  
  df_full <- analytic
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  table_a <- inner_closed_complications_by_severity_relatedness(df_a)
  table_b <- inner_closed_complications_by_severity_relatedness(df_b)
  table_full <- inner_closed_complications_by_severity_relatedness(df_full)
  
  output <- cbind(table_a %>% select(-severity), 
                  table_b %>% select(-severity, -complications), 
                  table_full %>% select(-severity, -complications))
    
  
  colnames(output)[1] <- " "
  
  index_vec <- c(" " = 1, "Grade 4" = 9, "Grade 3"= 9,"Grade 2,1"= 9, "Grade Unknown"= 9)
  subindex_vec <- c(" " = 1, "Infection" = 3, " " = 6, "Infection" = 3, " " = 6, "Infection" = 3, " " = 6,
                    "Infection" = 3, " " = 6)
  table_raw<- kable(output, align='l', padding='2l') %>%  
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    pack_rows(index = subindex_vec,label_row_css = "text-align:left;padding-left: 2em;", bold = FALSE) %>% 
    add_header_above(c(" " = 1, "Group A" = 7, "Group B" = 7, "All" = 7)) %>%
    row_spec(1, extra_css = "border-bottom: 1px solid") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(table_raw)
}




#' Appendix A: Listing of Serious Adverse Events(SAEs) for closed report
#'
#' @description This function visualizes appendix for all the serious adverse events noted for the patients
#'
#' @param analytic This is the analytic data set that must include study_id, sae_data
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_appendix_A_SAEs()
#' }
closed_appendix_A_SAEs <- function(analytic){
  
  df <- analytic %>% 
    select(study_id, sae_data) %>% 
    filter(!is.na(sae_data))
  
  unzipped_sae <- df %>%
    separate_rows(sae_data, sep = ";new_row: ") %>% 
    separate(sae_data, into = c("facilitycode", "consent_date", "sae_dt_event", "age", "sae_relatedness_injury", 
                                "sae_relatedness_treatment", "sae_outcome", "sae_describe"), sep='\\|') 
  
  
  output_df <- unzipped_sae %>% 
    mutate(text = paste0(
      "<b>Participant ID</b>: ", study_id, "-", facilitycode, "<br /> ",
      "<b>Date Enrolled</b>: ", consent_date, "<br /> ",
      "<b>Date of SAE</b>: ", sae_dt_event, "<br /> ",
      "<b>Age</b>: ", age, "<br /> ",
      "<b>Related to Injury(per Site)</b>: ", sae_relatedness_injury, "<br /> ",
      "<b>Related to Treatment (per Medical Monitor)</b>: ", sae_relatedness_treatment, "<br /> ",
      "<b>Outcome</b>: ", sae_outcome, "<br /> ",
      "<b>Description</b>: ", sae_describe, "<br /> ",
      "<br />")) 
  
  if (nrow(unzipped_sae) == 0) {
    return(paste0("<br />\nNone at this time.<br />\n"))
  }
  
  output_text <- output_df %>% pull(text) %>% 
    paste(collapse = "<br />\n")
  
  return(output_text)
}


#' Appendix B: Listing of any Death for closed report
#'
#' @description This function visualizes any death occurred during the study time period.
#'
#' @param analytic This is the analytic data set that must include study_id, sae_data, death_date
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_appendix_B_deaths()
#' }
closed_appendix_B_deaths <- function(analytic){
  
  df <- analytic %>% 
    select(study_id, sae_data, death_date) 
  
  unzipped_death <- df %>%
    separate_rows(sae_data, sep = ";new_row: ") %>% 
    separate(sae_data, into = c("facilitycode", "consent_date", "sae_dt_event", "age", "sae_relatedness_injury", 
                                "sae_relatedness_treatment", "sae_outcome", "sae_describe"), sep='\\|') %>% 
    filter(sae_outcome == "Death" | !is.na(death_date))
  
  if (nrow(unzipped_death) == 0) {
    return(paste0("<br />\nNone at this time.<br />\n"))
  }
  
  output_text <- output_df %>% 
    paste(collapse = "<br />\n")
  
  return(output_text)
}

#' Appendix C: Listing of any Discontinuations for closed report
#'
#' @description This function visualizes any discontinuations occurred during the study time period.
#'
#' @param analytic This is the analytic data set that must include study_id, discontinuation_data
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_appendix_C_discontinuations()
#' }
closed_appendix_C_discontinuations <- function(analytic){
  
  df <- analytic %>% 
    select(study_id, discontinuation_data) %>% 
    filter(!is.na(discontinuation_data))
  
  unzipped_discontinuation <- df %>%
    separate(discontinuation_data, into = c("facilitycode", "consent_date", "discontinue_date", "age", 
                                            "discontinuation_reason"), sep='\\|') 
  
  output_df <- unzipped_discontinuation %>% 
    mutate(text = paste0(
      "<b>Participant ID</b>: ", study_id, "-", facilitycode, "<br /> ",
      "<b>Date Enrolled</b>: ", consent_date, "<br /> ",
      "<b>Date discontinued</b>: ", discontinue_date, "<br /> ",
      "<b>Age</b>: ", age, "<br /> ",
      "<b>Reason for discontinuation</b>: ", discontinuation_reason, "<br /> ",
      "<br />")) 
  
  if (nrow(unzipped_discontinuation) == 0) {
    return(paste0("<br />\nNone at this time.<br />\n"))
  }
  
  output_text <- output_df %>% pull(text) %>% 
    paste(collapse = "<br />\n")
  
  return(output_text)
}

#' Appendix D: Listing of any protocol deviations for closed report
#'
#' @description This function visualizes any protocol deviations occurred during the study time period.
#'
#' @param analytic This is the analytic data set that must include study_id, protocol_deviation_data
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_appendix_D_protocol_deviation()
#' }
closed_appendix_D_protocol_deviation <- function(analytic){
  
  df <- analytic %>% 
    select(study_id, protocol_deviation_data) %>% 
    filter(!is.na(protocol_deviation_data))
  
  unzipped_protocol_deviation <- df %>% 
    separate_rows(protocol_deviation_data, sep = ";new_row: ") %>% 
    separate(protocol_deviation_data, into = c("facilitycode", "consent_date", "deviation_date", "protocol_deviation", 
                                               "deviation_description"), sep='\\|') 
  
  output_df <- unzipped_protocol_deviation %>% 
    mutate(text = paste0(
      "<b>Participant ID</b>: ", study_id, "-", facilitycode, "<br /> ",
      "<b>Date Enrolled</b>: ", consent_date, "<br /> ",
      "<b>Date of deviation</b>: ", deviation_date, "<br /> ",
      "<b>Deviation type</b>: ", protocol_deviation, "<br /> ",
      "<b>Description</b>: ", deviation_description, "<br /> ",
      "<br />")) 
  
  if (nrow(unzipped_protocol_deviation) == 0) {
    return(paste0("<br />\nNone at this time.<br />\n"))
  }
  
  output_text <- output_df %>% pull(text) %>% 
    paste(collapse = "<br />\n")
  
  return(output_text)
}



#' Crossover Monitoring by Site Closed
#'
#' @description This function visualizes the crossovers by site in hospital and at discharge
#'
#' @param analytic This is the analytic data set that must include enrolled, dfsurg_completed, 
#' ih_dischrg_date, ih_crossover, dc_crossover, ih_dischrg_date_on_time_zero, facilitycode, and treatment_arm
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_ih_and_dc_crossover_monitoring_by_site()
#' }
closed_ih_and_dc_crossover_monitoring_by_site <- function(analytic){
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  out <- paste0("<h4>Group A</h4><br />",
                ih_and_dc_crossover_monitoring_by_site(df_a),
                "<h4>Group B</h4><br />",
                ih_and_dc_crossover_monitoring_by_site(df_b))
  return(out)
}
# closed_ih_and_dc_crossover_monitoring_by_site <- function(analytic){
#   
#   df <- analytic %>% 
#     filter(enrolled == TRUE)
#   
#   df_a <- analytic %>% 
#     filter(treatment_arm=="Group A") %>% 
#     filter(enrolled == TRUE)
#   
#   df_b <- analytic %>% 
#     filter(treatment_arm=="Group B") %>% 
#     filter(enrolled == TRUE)
#   
#   inner_closed_ih_and_dc_crossover_monitoring_by_site <- function(analytic_df){
#     
#     df <- analytic_df %>% 
#       select(facilitycode, enrolled, dfsurg_completed, ih_dischrg_date, ih_crossover, dc_crossover, ih_dischrg_date_on_time_zero) %>% 
#       mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
#       rename(Facility = facilitycode) %>% 
#       filter(enrolled) %>% 
#       mutate(ih_dischrg_date = !is.na(ih_dischrg_date)) %>% 
#       group_by(Facility) %>% 
#       summarize('Enrolled' = sum(enrolled),
#                 "Definitive Fixation Complete" = sum(dfsurg_completed), 
#                 "Discharged from Index Hospitalization" = sum(ih_dischrg_date),
#                 "Discharged on Radomization Date" = sum(ih_dischrg_date_on_time_zero),
#                 "Inpatient Crossover" = sum(ih_crossover),
#                 "Discharge Crossover" = sum(dc_crossover))
#     
#     table_raw <- df %>% 
#       adorn_totals("row") %>% 
#       mutate(is_total=Facility=="Total") %>% 
#       arrange(desc(is_total), Facility) %>% 
#       select(-is_total)
#     
#     return(table_raw)
#   }
#   
#   table_a <- inner_closed_ih_and_dc_crossover_monitoring_by_site(df_a)
#   table_b <- inner_closed_ih_and_dc_crossover_monitoring_by_site(df_b)
#   table_full <- inner_closed_ih_and_dc_crossover_monitoring_by_site(df)
#   
#   df_table <- full_join(full_join(table_a, table_b, by="Facility"), 
#                         table_full, by="Facility")
#   
#   
#   table_raw<- kable(df_table, align='l', padding='2l', col.names = str_remove(colnames(df_table),"\\.x|\\.y")) %>%
#     add_header_above(c(" " = 1, "Group A" = 6, "Group B" = 6, "All" = 6)) %>%
#     kable_styling("striped", full_width = F, position="left")
#   
#   return(table_raw)
# }




#' Closed Nonunion surgery outcome
#'
#' @description This function visualizes the Nonunion surgery outcome
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' followup_due_3mo, followup_due_12mo, nonunion_90day,  nonunion_1yr, treatment_arm
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_nonunion_surgery_outcome()
#' }
closed_nonunion_surgery_outcome <- function(analytic){
  df <- analytic %>% 
    select(enrolled, followup_due_3mo, nonunion_90day, followup_due_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90day = format_count_percent(nonunion_90day, followup_due_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_due_12mo)) %>% 
    mutate(Treatment = "Total") %>% 
    select(Treatment, enrolled, followup_due_3mo, nonunion_90day, followup_due_12mo, nonunion_1yr)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A") %>% 
    select(enrolled, followup_due_3mo, nonunion_90day, followup_due_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90day = format_count_percent(nonunion_90day, followup_due_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_due_12mo)) %>% 
    mutate(Treatment = "Group A") %>% 
    select(Treatment, enrolled, followup_due_3mo, nonunion_90day, followup_due_12mo, nonunion_1yr)
    
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B") %>% 
    select(enrolled, followup_due_3mo, nonunion_90day, followup_due_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90day = format_count_percent(nonunion_90day, followup_due_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_due_12mo)) %>% 
    mutate(Treatment = "Group B") %>% 
    select(Treatment, enrolled, followup_due_3mo, nonunion_90day, followup_due_12mo, nonunion_1yr)
  
  df <- rbind(df, df_a, df_b)
  
  colname <- c("Treatment", "Enrolled", "Expected Three Month", "90 Day Non-Union", "Expected Twelve Month", "1 Year Non-Union")
  
  table<- kable(df, align='l', padding='2l', col.names = colname) %>% 
    kable_styling("striped", full_width = F, position="left")
  return(table)
}




#' Closed AO Gustillo Tscherne Injury Characteristics
#'
#' @description This function visualizes the injury characteristics
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' injury_gustilo, injury_tscherne, injury_ao_class, treatment_arm
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_ao_gustillo_tscherne_characteristics()
#' }
closed_ao_gustillo_tscherne_injury_characteristics <- function(analytic){
  
  
  df <- analytic %>% 
    filter(enrolled == TRUE)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A") %>% 
    filter(enrolled == TRUE)
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B") %>% 
    filter(enrolled == TRUE)
  
  inj_tsch <- NULL
  inj_gust <- NULL
  inj_ao <- NULL
  
  closed_ao_gustillo_tscherne_injury_characteristics <- function(analytic_df){
  
    pull <- analytic_df %>% 
      filter(enrolled) %>%
      select(injury_gustilo, injury_tscherne, injury_ao_class)
    
    inj_gust <<- pull %>% 
      count(injury_gustilo) %>%
      pivot_longer(-n) %>%
      mutate(value=ifelse(!is.na(value), paste('Gustilo Type', value), value)) %>%
      mutate(value=ifelse(is.na(value), 'Unknown 1', value)) %>%
      select(-name)
    
    total <- inj_gust %>%
      mutate(n=as.numeric(n)) %>%
      pull(n) %>%
      sum()
    
    inj_ao <<- pull %>% 
      count(injury_ao_class) %>%
      pivot_longer(-n) %>%
      mutate(value=ifelse(is.na(value), 'Unknown 2', value)) %>%
      select(-name)
    
    inj_tsch <<- pull %>% 
      count(injury_tscherne) %>%
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
    
  }
  
  table_a <- closed_ao_gustillo_tscherne_injury_characteristics(df_a)
  table_b <- closed_ao_gustillo_tscherne_injury_characteristics(df_b)
  table_full <- closed_ao_gustillo_tscherne_injury_characteristics(df)
  
  df_table <- full_join(full_join(table_a, table_b, by="Fracture Type"), 
                        table_full, by="Fracture Type") %>% 
    mutate("Fracture Type" = str_replace(`Fracture Type`,"Unknown [0-9]", "Unknown"))
  
  output<- kable(df_table, align='l', padding='2l', col.names = c("Fracture Type", "Group A", "Group B", "Total")) %>% 
    kable_styling("condensed", position="left") %>%
    pack_rows("Open Fracture", 1, nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("Closed Fracture", nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("AO Class", nrow(inj_gust)+nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch)+nrow(inj_ao), label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}


#' Closed Expected visit status for 3 Months, 6 Months, and 12 Months followup
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
#' closed_expected_and_followup_visit()
#' }
closed_expected_and_followup_visit <- function(analytic){

  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  out <- paste0("<h4>Group A</h4><br />",
                expected_and_followup_visit(df_a),
                "<h4>Group B</h4><br />",
                expected_and_followup_visit(df_b))
    
  return(out)
}
