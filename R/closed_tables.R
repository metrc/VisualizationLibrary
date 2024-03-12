


#' Closed Expected, completed, missing, out of window visits by each form
#'
#' @description This function visualizes the expected visits for each timepoint for MRR, CFU, PFU, BPI, AOS, KOOS forms
#'
#' @param analytic This is the analytic data set that must include treatment_arm mrr_status_6wk, mrr_status_3mo, mrr_status_6mo, 
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
#' @param analytic This is the analytic data set that must include treatment_arm injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled
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
    select(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled) %>%  
    filter(enrolled == TRUE)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A") %>% 
    select(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled) %>%  
    filter(enrolled == TRUE)
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B") %>% 
    select(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled) %>%  
    filter(enrolled == TRUE)
  
  ota_number <- -1
  schatzer_numbe <- -1
  inner_closed_injury_ankle_plateau_characteristics <- function(df){
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
closed_baseline_characteristics_percent <- function(analytic, sex="sex", race="ethnicity_race", education="education_level", military="military_status",
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
#' @param analytic This is the analytic data set that must include treatment_arm enrolled, censored, 
#' protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative, sae_count
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
  
  inner_closed_discontinuation_sae_deviation_by_type <- function(df){
    discontinuation_df <- df %>% 
      select(enrolled, censored_reason) %>% 
      filter(enrolled == TRUE) %>% 
      count(censored_reason) %>%
      rename(type=censored_reason) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    
    discontinuation_df_tot <- tibble(type="Discontinuations", n=sum(discontinuation_df$n))
    
    sae_df <- df %>% 
      select(study_id, enrolled, sae_count) %>% 
      filter(enrolled & sae_count>0) %>% 
      mutate(sae_count = "SAE") %>% 
      count(sae_count) %>%
      rename(type=sae_count) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    
    
    deviation_sc_df <- df %>% 
      select(study_id, enrolled, protocol_deviation_screen_consent) %>% 
      filter(enrolled == TRUE) %>% 
      count(protocol_deviation_screen_consent) %>%
      rename(type=protocol_deviation_screen_consent) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    
    
    deviation_p_df <- df %>% 
      select(study_id, enrolled, protocol_deviation_procedural) %>% 
      filter(enrolled == TRUE) %>% 
      count(protocol_deviation_procedural) %>%
      rename(type=protocol_deviation_procedural) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    
    
    deviation_a_df <- df %>% 
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
      arrange(severity, complications)
    
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
    separate(sae_data, into = c("facilitycode", "treatment_arm", "treatment_received", "consent_date", "sae_dt_event", "age", "sae_relatedness_injury",
                                "sae_relatedness_treatment", "sae_outcome", "sae_describe"), sep='\\|') 
  
  
  output_df <- unzipped_sae %>% 
    mutate(text = paste0(
      "<b>Participant ID</b>: ", study_id, "-", facilitycode, "<br /> ",
      "<b>Date Enrolled</b>: ", consent_date, "<br /> ",
      "<b>Tx Group</b>: ", treatment_arm, "<br /> ",
      "<b>Date of SAE</b>: ", sae_dt_event, "<br /> ",
      "<b>Age</b>: ", age, "<br /> ",
      "<b>Treatment Received</b>: ", treatment_received, "<br /> ",
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
    separate(discontinuation_data, into = c("facilitycode","treatment_arm", "consent_date", "discontinue_date", "age", 
                                            "discontinuation_reason"), sep='\\|') 
  
  output_df <- unzipped_discontinuation %>% 
    mutate(text = paste0(
      "<b>Participant ID</b>: ", study_id, "-", facilitycode, "<br /> ",
      "<b>Date Enrolled</b>: ", consent_date, "<br /> ",
      "<b>Tx Group</b>: ", treatment_arm, "<br /> ",
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
#' @param analytic This is the analytic data set that must include enrolled, df_surg_completed, 
#' ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero, facilitycode, and treatment_arm
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
#       select(facilitycode, enrolled, df_surg_completed, ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero) %>% 
#       mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
#       rename(Facility = facilitycode) %>% 
#       filter(enrolled) %>% 
#       mutate(ih_discharge_date = !is.na(ih_discharge_date)) %>% 
#       group_by(Facility) %>% 
#       summarize('Enrolled' = sum(enrolled),
#                 "Definitive Fixation Complete" = sum(df_surg_completed), 
#                 "Discharged from Index Hospitalization" = sum(ih_discharge_date),
#                 "Discharged on Radomization Date" = sum(ih_discharge_date_on_time_zero),
#                 "Inpatient Crossover" = sum(crossover_inpatient),
#                 "Discharge Crossover" = sum(crossover_discharge))
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
#' followup_due_3mo, followup_due_12mo, nonunion_90days,  nonunion_1yr, treatment_arm
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
    select(enrolled, followup_due_3mo, nonunion_90days, followup_due_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_due_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_due_12mo)) %>% 
    mutate(Treatment = "Total") %>% 
    select(Treatment, enrolled, followup_due_3mo, nonunion_90days, followup_due_12mo, nonunion_1yr)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A") %>% 
    select(enrolled, followup_due_3mo, nonunion_90days, followup_due_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_due_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_due_12mo)) %>% 
    mutate(Treatment = "Group A") %>% 
    select(Treatment, enrolled, followup_due_3mo, nonunion_90days, followup_due_12mo, nonunion_1yr)
    
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B") %>% 
    select(enrolled, followup_due_3mo, nonunion_90days, followup_due_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_due_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_due_12mo)) %>% 
    mutate(Treatment = "Group B") %>% 
    select(Treatment, enrolled, followup_due_3mo, nonunion_90days, followup_due_12mo, nonunion_1yr)
  
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
#' injury_gustilo, injury_classification_tscherne, injury_classification_ankle_ao, treatment_arm
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
      select(injury_gustilo, injury_classification_tscherne, injury_classification_ankle_ao)
    
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
      count(injury_classification_ankle_ao) %>%
      pivot_longer(-n) %>%
      mutate(value=ifelse(is.na(value), 'Unknown 2', value)) %>%
      select(-name)
    
    inj_tsch <<- pull %>% 
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




#' Closed Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description This function visualizes the enrollment totals for each site by treatment arm
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, site_certified_days, 
#' facilitycode, late_ineligible, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_enrollment_status_by_site(analytic)
#' }
closed_enrollment_status_by_site <- function(analytic){
  inner_enrollment_status_by_site <- function(df) {
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
    
    return(table_raw)
  }
  
  df <- analytic %>% 
    select(screened, eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, site_certified_days, 
           facilitycode, late_ineligible, treatment_arm) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certified_days))) %>% 
    rename(Facility = facilitycode) %>% 
    rename(not_enrolled = not_consented) %>% 
    filter(!is.na(Facility))
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  table_raw_a <- inner_enrollment_status_by_site(df_a)
  table_raw_b <- inner_enrollment_status_by_site(df_b)
  table_raw_full <- inner_enrollment_status_by_site(df)
  
  df_table <- full_join(table_raw_a, table_raw_b, by = "Facility", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(table_raw_full, by = "Facility") %>%
    select(Facility, `Days Certified`, starts_with("Screened"), starts_with("Eligible"), starts_with("Refused"), 
           starts_with("Not Enrolled for Other Reasons"), starts_with("Consented"), starts_with("Discontinued Pre-Randomization"),
           starts_with("Late Ineligible"), starts_with("Enrolled"))
  
  table <- kable(df_table, align='l', padding='2l') %>% 
    add_header_above(c(" " = 2, "Group A" = 8, "Group B" = 8, "Overall" = 8)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(table)
}

#' Closed Number of Subjects Screened, Eligible, Enrolled and Not Enrolled (Variable Discontinued)
#'
#' @description This function visualizes the enrollment totals for each site by treatment arm
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, site_certified_days, facilitycode, treatment_arm
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_enrollment_status_by_site_var_discontinued(analytic, discontinued = "adjudicated_discontinued", 
#'                                                   discontinued_colname = "Adjudicated Discontinued")
#' }
closed_enrollment_status_by_site_var_discontinued <- function(analytic, discontinued="discontinued", discontinued_colname="Discontinued"){
  
  inner_enrollment_status_by_site_var_discontinued <- function(df) {
    colnames(df)[11] <- "discontinued"
    
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
    
    return(table_raw)
  }
  
  df <- analytic %>%
    select(screened, eligible, refused, not_consented, consented, not_randomized, randomized, enrolled, site_certified_days, 
           facilitycode, all_of(discontinued), treatment_arm) %>%
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certified_days))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B") 
  
  table_raw_a <- inner_enrollment_status_by_site_var_discontinued(df_a)
  table_raw_b <- inner_enrollment_status_by_site_var_discontinued(df_b)
  table_raw_full <- inner_enrollment_status_by_site_var_discontinued(df)
  
  df_table <- full_join(table_raw_a, table_raw_b, by = "Facility", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(table_raw_full, by = "Facility") %>%
    select(Facility, `Days Certified`, starts_with("Screened"), starts_with("Eligible"), starts_with("Refused"), 
           starts_with("Not Consented"), starts_with("Consented"), starts_with("Randomized"),
           starts_with(discontinued_colname), starts_with("Enrolled"))
  
  table <- kable(df_table, align='l', padding='2l') %>% 
    add_header_above(c(" " = 2, "Group A" = 7, "Group B" = 7, "Overall" = 7)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(table)
}

#' Closed Ankle and Plateau X-Ray and Measurement Status
#'
#' @description This function visualizes Ankle and Plateau X-Ray and Measurement Status by treatment arm
#'
#' @param analytic This is the analytic data set that must include treatment_arm, followup_expected_6wk, 
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
#' @return Two kable tables separated by headers for Ankle and Plateau
#' @export
#'
#' @examples
#' \dontrun{
#' closed_ankle_and_plateau_x_ray_and_measurement_status(analytic)
#' }
closed_ankle_and_plateau_x_ray_and_measurement_status <- function(analytic){
  
  inner_ankle_and_plateau_x_ray_and_measurement_status <- function(analytic) {
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
      reframe(value=c("Complete", "Incomplete","Missing", "Expected")) %>%
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
    
    table_raw_ankle<- kable(df_ankle, align='l', padding='2l', col.names = str_replace(colnames(df_ankle),"^n.|^n"," ")) %>%
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
      reframe(value=c("Complete", "Incomplete",
                      "Missing", "Expected")) %>% 
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
    
    table_raw_plateau<- kable(df_plateau, align='l', padding='2l', col.names = str_replace(colnames(df_plateau),"^n.|^n"," ")) %>%
      pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
      kable_styling("striped", full_width = F, position='left')
    
    return(list(table_raw_ankle, table_raw_plateau))
  }
  
  df_a <- analytic %>% filter(treatment_arm == "Group A")
  df_b <- analytic %>% filter(treatment_arm == "Group B")
  
  table_raw_a <- inner_ankle_and_plateau_x_ray_and_measurement_status(df_a)
  table_raw_b <- inner_ankle_and_plateau_x_ray_and_measurement_status(df_b)
  
  output_a <- paste0("<h3>Group A - Ankle</h3><br />", table_raw_a[[1]], "<h3>Group A - Plateau</h3><br />", table_raw_a[[2]])
  output_b <- paste0("<h3>Group B - Ankle</h3><br />", table_raw_b[[1]], "<h3>Group B - Plateau</h3><br />", table_raw_b[[2]])
  
  output <- paste0(output_a, output_b)
  
  return(output)
}

#' Closed Number of Adjudications and Discontinuations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type and treatment arm
#' This was originally made for NSAID
#'
#' @param analytic This is the analytic data set that must include screened, treatment_arm, inappropriate_enrollment;
#' late_ineligible; late_refusal; withdrawn_patient; withdrawn_physician; adjudication_pending;
#' dead; sae_count; protocol_deviation_screen_consent; protocol_deviation_procedural; protocol_deviation_administrative
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_adjudications_and_discontinuations_by_type(analytic)
#' }
closed_adjudications_and_discontinuations_by_type <- function(analytic){
  
  inner_adjudications_and_discontinuations_by_type <- function(df) {
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
    
    n_disc <- nrow(study_discontinuation_df)
    n_dsc <- nrow(protocol_deviation_screen_consent_df)
    n_dp <- nrow(protocol_deviation_procedural_df)
    n_da <- nrow(protocol_deviation_administrative_df)
    
    df_final <- rbind(disc, study_discontinuation_df, sae_count_df, protocol_deviations, sc, protocol_deviation_screen_consent_df, 
                      dp, protocol_deviation_procedural_df, da, protocol_deviation_administrative_df)
    
    return(list(n_disc, n_dsc, n_dp, n_da, df_final))
  }
  
  df <- analytic %>%
    filter(screened == TRUE) %>%
    select(screened, enrolled, treatment_arm, inappropriate_enrollment, late_ineligible, late_refusal, withdrawn_patient, withdrawn_physician, #adjudication_pending,
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
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  results_a <- inner_adjudications_and_discontinuations_by_type(df_a)
  results_b <- inner_adjudications_and_discontinuations_by_type(df_b)
  results_full <- inner_adjudications_and_discontinuations_by_type(df)
  
  n_disc_a <- results_a[[1]]
  n_dsc_a <- results_a[[2]]
  n_dp_a <- results_a[[3]]
  n_da_a <- results_a[[4]]
  df_final_a <- results_a[[5]]
  
  n_disc_b <- results_b[[1]]
  n_dsc_b <- results_b[[2]]
  n_dp_b <- results_b[[3]]
  n_da_b <- results_b[[4]]
  df_final_b <- results_b[[5]]
  
  n_disc_full <- results_full[[1]]
  n_dsc_full <- results_full[[2]]
  n_dp_full <- results_full[[3]]
  n_da_full <- results_full[[4]]
  df_final_full <- results_full[[5]]
  
  df_table <- full_join(df_final_a, df_final_b, by = "type", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(df_final_full, by = "type") %>%
    select(type, ends_with(" (Group A)"), ends_with(" (Group B)"), ends_with("percentage"))
  
  indents_vec_a <- vector()
  if (n_dsc_a > 0) {
    indents_vec_a <- c(indents_vec_a, 1 + n_disc_a + 1 + 1 + 1 + seq(n_dsc_a))
  }
  if (n_dp_a > 0) {
    indents_vec_a <- c(indents_vec_a, 1 + n_disc_a + 1 + 1 + 1 + n_dsc_a + 1 + seq(n_dp_a))
  }
  if (n_da_a > 0) {
    indents_vec_a <- c(indents_vec_a, 1 + n_disc_a + 1 + 1 + 1 + n_dsc_a + 1 + n_dp_a + 1 + seq(n_da_a))
  }
  
  indents_vec_b <- vector()
  if (n_dsc_b > 0) {
    indents_vec_b <- c(indents_vec_b, 1 + n_disc_b + 1 + 1 + 1 + seq(n_dsc_b))
  }
  if (n_dp_b > 0) {
    
    indents_vec_b <- c(indents_vec_b, 1 + n_disc_b + 1 + 1 + 1 + n_dsc_b + 1 + seq(n_dp_b))
  }
  if (n_da_b > 0) {
    indents_vec_b <- c(indents_vec_b, 1 + n_disc_b + 1 + 1 + 1 + n_dsc_b + 1 + n_dp_b + 1 + seq(n_da_b))
    
  }
  
  indents_vec_full <- vector()
  if (n_dsc_full > 0) {
    indents_vec_full <- c(indents_vec_full, 1 + n_disc_full + 1 + 1 + 1 + seq(n_dsc_full))
  }
  if (n_dp_full > 0) {
    indents_vec_full <- c(indents_vec_full, 1 + n_disc_full + 1 + 1 + 1 + n_dsc_full + 1 + seq(n_dp_full))
  }
  
  if (n_da_full > 0) {
    indents_vec_full <- c(indents_vec_full, 1 + n_disc_full + 1 + 1 + 1 + n_dsc_full + 1 + n_dp_full + 1 + seq(n_da_full))
  }
  
  cnames <- c(' ', paste('Group A (n=', sum(df_a$enrolled), ')'),
              paste('Group B (n=', sum(df_b$enrolled), ')'),
              paste('Overall (n=', total, ')'))
  header_names <- c("Discontinuous", rep("", n_disc_full), "SAE",
                    "Protocol Deviations", rep("", 4 + n_dsc_full + n_dp_full + n_da_full))
  
  vis <- kable(df_table, align='l', padding='2l', col.names = cnames) %>%
    row_spec(header_names, bold=T) %>%
    add_indent(c(seq(n_disc_full) + 1, 1 + n_disc_full + 1 + 1 + seq(1 + n_dsc_full + 1 + n_dp_full + 1 + n_da_full)),
               indents = c(1,0), level_of_indent = "  ") %>%
    add_indent(indents_vec_a, indents = 1, level_of_indent = "    ", columns = 2) %>% 
    add_indent(indents_vec_b, indents = 1, level_of_indent = " ", columns = 3) %>% 
    add_indent(indents_vec_full, indents = 1, level_of_indent = " ", columns = 4) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc_full, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc_full + 1, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc_full + 1 + 1 + 1 + n_dsc_full + 1 + n_dp_full + 1 + n_da_full, extra_css = "border-bottom: 1px solid") %>% 
    kable_styling("striped", full_width = F, position = "left")
  return(vis)
}

#' Closed Number of patients Ineligible by Top 5 reasons of Exclusion
#'
#' @description This function visualizes the number of patients Ineligible by Top 5 reasons of Exclusion criteria by treatment arm
#'
#' @param analytic This is the analytic data set that must include facilitycode,  screened, ineligible, ineligibility_reasons, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_ineligibility_by_reasons(analytic)
#' }
closed_ineligibility_by_reasons <- function(analytic, n_top_reasons = 5){
  inner_ineligibility_by_reasons <- function(df) {
    reasons <- df %>%  select(study_id, facilitycode, ineligibility_reasons) %>%
      column_unzipper('ineligibility_reasons', sep = '; ') %>%
      boolean_column_counter() %>%
      pivot_longer(everything()) %>%
      arrange(desc(value)) %>%
      slice(1:n_top_reasons) %>%
      pull(name)
    
    total <- df %>% 
        column_unzipper('ineligibility_reasons', sep = '; ') %>% 
        boolean_column_counter() %>% 
        mutate(otherreasons = rowSums(across(-c(all_of(reasons), screened, ineligible)))) %>% 
        mutate(Site = 'Total') %>% 
        select(Site, screened, ineligible, all_of(reasons), otherreasons)
      
    sites <- df %>% 
      column_unzipper('ineligibility_reasons', sep = '; ') %>% 
      boolean_column_counter(groups = 'facilitycode') %>% 
      mutate(otherreasons = rowSums(across(-c(all_of(reasons), screened, ineligible, facilitycode)))) %>% 
      rename(Site = facilitycode) %>% 
      select(Site, screened, ineligible, all_of(reasons), otherreasons)
    
    output <- bind_rows(total, sites) %>% 
      rename(Screened = screened,
             Ineligible = ineligible,
             `Other Reasons` = otherreasons) %>% 
      arrange(desc(Screened)) %>% 
      mutate(Ineligible = format_count_percent(Ineligible, Screened))
    
    return(output)
    
  }
  
  df <- analytic %>%
    select(study_id, facilitycode,  screened, ineligible, ineligibility_reasons, treatment_arm) %>%
    filter(screened == TRUE)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  output_a <- inner_ineligibility_by_reasons(df_a)
  output_b <- inner_ineligibility_by_reasons(df_b)
  output_full <- inner_ineligibility_by_reasons(df)
  
  top_n_header_text <- paste0("Top ", n_top_reasons, " Ineligibility Reasons =")
  
  header_names <- c(" " = 3,
                    "Group A" = n_top_reasons, " " = 1,
                    "Group B" = n_top_reasons, " " = 1,
                    top_n_header_text = n_top_reasons, " " = 1)
  
  names(header_names)[4+2*n_top_reasons] <- top_n_header_text
  
  df_table <- full_join(output_a, output_b, by = "Site", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(output_full, by = "Site") %>%
    select(Site, starts_with("Screened"), starts_with("Ineligible"), starts_with("Other Reasons"), everything())
  
  vis <- kable(df_table, align = 'l', padding = '2l') %>%
    add_header_above(header_names) %>%
    
    kable_styling("striped", full_width = FALSE, position = "left")
  
  return(vis)
}

#' Closed Status of IRB Approvals and Certification by Site
#'
#' @description This function returns a list of sites and their dates of
#' local, DOD, and METRC certifications by treatment arm
#'
#' @param analytic This is the analytic data set that must include site_certified_date
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_certification_date_data()
#' }
closed_certification_date_data <- function(analytic){
  
  date_today <- Sys.Date()
  
  cols <- c('Facility', 'Local (or sIRB) Approval Date', 'DoD Approval Date',
            'Certified by MCC to Start Screening',
            paste0('Days Number of Days Certified (as of ', as.character(date_today), ')'))
  
  df <- analytic %>%
    filter(!is.na(treatment_arm)) %>%
    group_by(treatment_arm) %>%
    mutate(site_cert_date = na_if(site_certified_date, "NA")) %>%
    separate(site_cert_date, cols, sep = ';') %>%
    select(treatment_arm, Facility, `Local (or sIRB) Approval Date`, `DoD Approval Date`,
           `Certified by MCC to Start Screening`, `Days Number of Days Certified (as of Thursday, March 07, 2024)`) %>%
    filter(!is.na(Facility))
  
  df_a <- df %>% filter(treatment_arm == "Group A") %>% select(-treatment_arm)
  df_b <- df %>% filter(treatment_arm == "Group B") %>% select(-treatment_arm)
  
  vis <- bind_rows(df_a, df_b) %>%
    kable(align='l', padding='2l') %>%
    add_header_above(c(" " = 1, "Group A" = 4, "Group B" = 4)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(vis)
}

#' Closed Injury Characteristics
#'
#' @description This function visualizes the certain injury characteristics for study participants study injuries by treatment arm
#'
#' @param analytic This is the analytic data set that must include enrolled, treatment_arm, injury_classification_ankle_ao, injury_at_work, injury_in_battle,
#' injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type

#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_injury_characteristics_by_alternate_constructs(analytic)
#' }
closed_injury_characteristics_by_alternate_constructs <- function(analytic){
  
  inner_injury_characteristics_by_alternate_constructs <- function(df) {
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
    
    return(df_final)
  }
  
  df <- analytic %>%
    filter(enrolled) %>%
    select(enrolled, treatment_arm, injury_classification_ankle_ao, injury_at_work, injury_in_battle,
           injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type)
  
  total <- sum(df$enrolled)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  df_final_a <- inner_injury_characteristics_by_alternate_constructs(df_a)
  df_final_b <- inner_injury_characteristics_by_alternate_constructs(df_b)
  df_final_full <- inner_injury_characteristics_by_alternate_constructs(df)
  
  df_table <- full_join(df_final_a, df_final_b, by = "type", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(df_final_full, by = "type") %>%
    select(type, ends_with(" (Group A)"), ends_with(" (Group B)"), ends_with("percentage"))
  
  cnames <- c(' ', paste('Group A (n=', sum(df_a$enrolled), ')'),
              paste('Group B (n=', sum(df_b$enrolled), ')'),
              paste('Overall (n=', total, ')'))
  header <- c(1,1)
  names(header) <- cnames
  
  vis <- kable(df_table, align='l', padding='2l', col.names = cnames) %>%
    pack_rows(index = c('Type of Injury' = nrow(df_table %>% filter(str_detect(type, "Blunt|Penetrating|Missing"))),
                        'Work Related Injury' = nrow(df_table %>% filter(str_detect(type, "Yes|No"))),
                        'Battlefield Injury' = nrow(df_table %>% filter(str_detect(type, "Yes|No"))),
                        'Blast Injury' = nrow(df_table %>% filter(str_detect(type, "Yes|No"))),
                        
                        'Side of Study Injury' = nrow(df_table %>% filter(str_detect(type, "Left|Right|Missing"))),
                        'Tscherne Classification' = nrow(df_table %>% filter(str_detect(type, "Tscherne|N/A"))),
                        'AO Classification' = nrow(df_table %>% filter(str_detect(type, "44|43")))),
              label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(vis)
}

#' Closed Number of Visits Expected, Completed, Missed, and out of Window
#'
#' @description This function visualizes the expected visits, and which of those are completed in what relative window,
#' and which of those are missed by treatment arm
#'
#' @param analytic This is the analytic data set that must include enrolled, treatment_arm, followup_complete_4wk_6wk, followup_due_4wk_6wk, followup_early_4wk_6wk, followup_incomplete_4wk_6wk,
#' followup_late_4wk_6wk, followup_missing_4wk_6wk, followup_not_started_4wk_6wk, followup_ontime_4wk_6wk,
#' followup_complete_7wk_9wk, followup_due_7wk_9wk, followup_early_7wk_9wk, followup_incomplete_7wk_9wk,
#' followup_late_7wk_9wk, followup_missing_7wk_9wk, followup_not_started_7wk_9wk, followup_complete_12wk_16wk,
#' followup_due_12wk_16wk, followup_early_12wk_16wk, followup_incomplete_12wk_16wk, followup_late_12wk_16wk,
#' followup_missing_12wk_16wk, followup_not_started_12wk_16wk, followup_ontime_12wk_16wk, followup_complete_22wk_32wk,
#' followup_due_22wk_32wk, followup_early_22wk_32wk, followup_incomplete_22wk_32wk, followup_late_22wk_32wk,
#' followup_missing_22wk_32wk, followup_not_started_22wk_32wk, followup_ontime_22wk_32wk
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_expected_visits_by_followup_period(analytic)
#' }
closed_expected_visits_by_followup_period <- function(analytic){
  inner_expected_visits_by_followup_period <- function(df) {
    df <- df %>%
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
    
    return(final)
  }
  
  df <- analytic %>%
    select(study_id, enrolled, treatment_arm, followup_complete_4wk_6wk, followup_due_4wk_6wk, followup_early_4wk_6wk, followup_incomplete_4wk_6wk,
           followup_late_4wk_6wk, followup_missing_4wk_6wk, followup_not_started_4wk_6wk, followup_ontime_4wk_6wk,
           followup_complete_7wk_9wk, followup_due_7wk_9wk, followup_early_7wk_9wk, followup_incomplete_7wk_9wk,
           followup_late_7wk_9wk, followup_missing_7wk_9wk, followup_not_started_7wk_9wk, followup_complete_12wk_16wk,
           followup_due_12wk_16wk, followup_early_12wk_16wk, followup_incomplete_12wk_16wk, followup_late_12wk_16wk,
           followup_missing_12wk_16wk, followup_not_started_12wk_16wk, followup_ontime_12wk_16wk, followup_complete_22wk_32wk,
           followup_due_22wk_32wk, followup_early_22wk_32wk, followup_incomplete_22wk_32wk, followup_late_22wk_32wk,
           followup_missing_22wk_32wk, followup_not_started_22wk_32wk, followup_ontime_22wk_32wk)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  final_a <- inner_expected_visits_by_followup_period(df_a)
  final_b <- inner_expected_visits_by_followup_period(df_b)
  final_full <- inner_expected_visits_by_followup_period(df)
  
  df_table <- full_join(final_a, final_b, by = "Status", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(final_full, by = "Status") %>%
    select(Status, ends_with(" (Group A)"), ends_with(" (Group B)"), ends_with("Weeks"))
  
  vis <- kable(df_table, align = 'l', padding = '2l') %>%
    add_header_above(c(" " = 1, "Group A" = 4, "Group B" = 4, "Overall" = 4)) %>%
    kable_styling("striped", full_width = F, position = "left") %>%
    add_indent(c(3, 4)) %>%
    row_spec(1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(4, extra_css = "border-bottom: 1px solid") %>%
    row_spec(5, extra_css = "border-bottom: 1px solid") %>%
    row_spec(6, extra_css = "border-bottom: 1px solid")
  
  return(vis)
}

#' Closed Amputations and Gustilo Injury Characteristics
#'
#' @description This function visualizes the injury characteristics for amputations and Gustilo Injury types for
#' Sextant study by treatment arm
#'
#' @param analytic This is the analytic data set that must include enrolled, treatment_arm,
#' injury_gustilo_type, injury_amputation_status
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_amputations_and_gustilo_injury_characteristics(analytic)
#' }
closed_amputations_and_gustilo_injury_characteristics <- function(analytic){
  inner_amputations_and_gustilo_injury_characteristics <- function(pull) {
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
      mutate(count= format_count_percent(count, total)) %>%
      rename(`Fracture Type` = injury_gustilo_type)
    
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
      mutate(count= format_count_percent(count, total_amputations)) %>%
      rename(`Fracture Type` = injury_gustilo_type)
    
    n_amputations <- tibble(
      count = as.character(total_amputations),
      `Fracture Type` = "Amputation Status")
    
    n_gustilo <- tibble(
      count = as.character(total),
      `Fracture Type` = "Fracture Type")
    
    combined <- bind_rows(n_amputations, out_amputations,n_gustilo, out_gustilo) %>%
      relocate(count, .after = `Fracture Type`)
    
    return(combined)
  }
  
  pull <- analytic %>%
    filter(enrolled) %>%
    select(enrolled, treatment_arm, injury_gustilo_type, injury_amputation_status)
  
  pull_a <- pull %>% filter(treatment_arm == "Group A")
  pull_b <- pull %>% filter(treatment_arm == "Group B")
  
  combined_a <- inner_amputations_and_gustilo_injury_characteristics(pull_a)
  combined_b <- inner_amputations_and_gustilo_injury_characteristics(pull_b)
  
  combined_full <- inner_amputations_and_gustilo_injury_characteristics(pull)
  
  df_table <- full_join(combined_a, combined_b, by = "Fracture Type", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(combined_full, by = "Fracture Type") %>%
    select(`Fracture Type`, ends_with(" (Group A)"), ends_with(" (Group B)"), count)
  
  output <- kable(df_table, align='l', padding='2l', col.names = c(" ", "Group A", "Group B", "Overall")) %>%
    kable_styling("condensed", position = "left", full_width = FALSE) %>%
    add_indent(positions = c(2,3,4,6,7,8,9,10,11,12)) %>%
    row_spec(c(1,5), bold=T,hline_after = T)
  
  return(output)
}

#' Closed Refusal reasons by each site
#'
#' @description This function visualizes the reasons of refusal, total refused and total screened by each site and treatment arm.
#'
#' @param analytic This is the analytic data set that must include facilitycode, screened, refused, refused_reason, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_refusal_reasons_by_site(analytic)
#' }
closed_refusal_reasons_by_site <- function(analytic){
  
  inner_refusal_reasons_by_site <- function(df) {
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
    
    return(df_final)
  }
  
  df <- analytic %>%
    select(facilitycode, screened, refused, refused_reason, treatment_arm) %>%
    filter(screened == TRUE)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  df_final_a <- inner_refusal_reasons_by_site(df_a)
  df_final_b <- inner_refusal_reasons_by_site(df_b)
  
  df_table <- full_join(df_final_a, df_final_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  output <- kable(df_table, align='l', padding='2l') %>%
    add_header_above(c(" " = 1, "Group A" = (ncol(df_table)-1)/2, "Group B" = (ncol(df_table)-1)/2)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed Other reason of refusal by each site
#'
#' @description This function visualizes list of each "Other" reason of refusal, total screened by each site and treatment arm.
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, screened_date,
#' refused_reason_other, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_other_reason_refusal_by_site(analytic)
#' }
closed_other_reason_refusal_by_site <- function(analytic){
  df1 <- analytic %>%
    select(study_id, facilitycode, screened_date, refused_reason_other, treatment_arm) %>%
    filter(!is.na(refused_reason_other))
  
  df1_a <- df1 %>% filter(treatment_arm == "Group A") %>% select(-treatment_arm)
  df1_b <- df1 %>% filter(treatment_arm == "Group B") %>% select(-treatment_arm)
  
  output_a <- kable(df1_a, align='l', padding='2l', col.names = gsub("_", " ", names(df1_a))) %>%
    kable_styling("striped", full_width = F, position="left")
  
  output_b <- kable(df1_b, align='l', padding='2l', col.names = gsub("_", " ", names(df1_b))) %>%
    kable_styling("striped", full_width = F, position="left")
  
  output <- paste0("<h3>Group A</h3>", output_a, "<h3>Group B</h3>", output_b)
  
  return(output)
}

#' Not enrolled for other reasons
#'
#' @description This function visualizes list of study_ids who were screened howevere were not enrolled for "Other"
#' reasons by treatment arm
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, able_to_participate,
#' nonparticipation_text_given, constraint_noconsent, constraint_admin, constraint_othr, constraint_othr_txt, screened, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_not_enrolled_for_other_reasons(analytic)
#' }
closed_not_enrolled_for_other_reasons <- function(analytic){
  
  inner_not_enrolled_for_other_reasons <- function(df1) {
    df1 %>%
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
  }
  
  df <- analytic %>%
    select(study_id, facilitycode, able_to_participate, nonparticipation_text_given,
           constraint_noconsent, constraint_admin, constraint_othr, constraint_othr_txt, screened, treatment_arm)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  df1_a <- inner_not_enrolled_for_other_reasons(df_a)
  df1_b <- inner_not_enrolled_for_other_reasons(df_b)
  
  output_a <- kable(df1_a, align='l', padding='2l') %>%
    kable_styling("striped", full_width = F, position="left")
  
  output_b <- kable(df1_b, align='l', padding='2l') %>%
    kable_styling("striped", full_width = F, position="left")
  
  output <- paste0("<h3>Group A</h3>", output_a, "<h3>Group B</h3>", output_b)
  
  return(output)
}

#' Closed Treatment crossover and nonadherence
#'
#' @description This function visualizes the treatment crossover or any nonadherence occured during the Sextant
#' study by treatment arm.
#'
#' @param analytic This is the analytic data set that must include facilitycode, eligible, enrolled, time_zero,
#' local_antibiotic_at_dwc
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_treatment_crossover_and_nonadherence(analytic)
#' }
closed_treatment_crossover_and_nonadherence <- function(analytic){
  
  inner_treatment_crossover_and_nonadherence <- function(df) {
    df_2nd <- df %>%
      group_by(facilitycode) %>%
      summarize(elig_enr = sum(eligible_enrolled), total_dwc = sum(dwc_complete), treatment_completed = sum(dwc_treatment)) %>%
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
    
    return(result)
  }
  
  df <- analytic %>%
    select(facilitycode, eligible, enrolled, time_zero, local_antibiotic_at_dwc, treatment_arm) %>%
    filter(eligible == TRUE & enrolled == TRUE) %>%
    mutate(eligible_enrolled = ifelse(eligible == TRUE & enrolled == TRUE, TRUE, FALSE)) %>%
    mutate(dwc_complete = ifelse(!is.na(time_zero), TRUE, FALSE)) %>%
    mutate(dwc_treatment = ifelse(local_antibiotic_at_dwc, TRUE, FALSE)) %>%
    select(-eligible, -enrolled, -time_zero)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  result_a <- inner_treatment_crossover_and_nonadherence(df_a)
  result_b <- inner_treatment_crossover_and_nonadherence(df_b)
  
  df_table <- full_join(result_a, result_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  output <- kable(df_table, align='l', padding='2l') %>%
    add_header_above(c(" " = 1, "Group A" = (ncol(df_table)-1)/2, "Group B" = (ncol(df_table)-1)/2)) %>%
    kable_styling("striped", full_width = F, position="left") %>%
    row_spec(nrow(df_table), bold = TRUE)
  
  return(output)
}

#' Closed Characteristics Treatment
#'
#' @description This function visualizes definitive fixation by total df complete out of total enrolled, stages breakdown,
#' incisions broken down by plateau fractures and pilon fractures, and whether or not the study treatment adhered to protocol
#' by treatment arm.
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, df_date, plat_df_surgical_incision, pil_df_surgical_incision, df_number_procedures, df_randomized_treatment, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_charcateristics_treatment(analytic)
#' }
closed_characteristics_treatment <- function(analytic){
  
  inner_characteristics_treatment <- function(df) {
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
    
    adherence <- df %>%
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
    
    df_final <- rbind(df_complete, avg_stages, stages, plat_incisions, pil_incisions, adherence)
    
    n_df <- nrow(df_complete)
    n_avg <- nrow(avg_stages)
    n_stages <- nrow(stages)
    
    return(list(df_final, n_df, n_avg, n_stages))
  }
  
  df <- analytic %>%
    select(study_id, enrolled, df_date, plat_df_surgical_incision, pil_df_surgical_incision, df_number_procedures, df_randomized_treatment, treatment_arm) %>%
    filter(enrolled)
  
  total <- sum(df$enrolled, na.rm=T)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  results_a <- inner_characteristics_treatment(df_a)
  results_b <- inner_characteristics_treatment(df_b)
  results_full <- inner_characteristics_treatment(df)
  
  df_final_a <- results_a[[1]]
  n_df_a <- results_a[[2]]
  n_avg_a <- results_a[[3]]
  n_stages_a <- results_a[[4]]
  
  df_final_b <- results_b[[1]]
  n_df_b <- results_b[[2]]
  n_avg_b <- results_b[[3]]
  n_stages_b <- results_b[[4]]
  
  df_final_full <- results_full[[1]]
  n_df_full <- results_full[[2]]
  n_avg_full <- results_full[[3]]
  n_stages_full <- results_full[[4]]
  
  df_table <- full_join(df_final_a, df_final_b, by = "type", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(df_final_full, by = "type") %>%
    select(type, ends_with(" (Group A)"), ends_with(" (Group B)"), percentage)
  
  cnames <- c(' ', paste('Group A (n=', sum(df_a$enrolled), ')'),
              paste('Group B (n=', sum(df_b$enrolled), ')'),
              paste('Overall (n=', total, ')'))
  header <- c(1,1,1,1)
  names(header) <- cnames
  
  vis <- kable(df_table, align='l', padding='2l', col.names = cnames) %>%
    pack_rows(index = c(" " = n_df_full, 'Definitive Fixation' = (n_avg_full + n_stages_full),
                        
                        'Number of Incisions [Mean (SD)]' = 2, 'Study Treatment Adhering to Protocol' = 3),
              label_row_css = "text-align:left") %>%
    row_spec(0, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1, extra_css = 'border-bottom: 1px solid') %>%
    add_indent(seq(n_stages_full) + n_avg_full + n_df_full)
  
  return(vis)
}

#' Closed Fracture Characteristics
#'
#' @description This function visualizes fracture characteristics, broken down by tibial plateau or pilon,
#' and then closed or open fracture with tscherne grades and gustilo types respectively by treatment arm
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, fracture_type, injury_gustilo,
#' treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_fracture_characteristics(analytic)
#' }
closed_fracture_characteristics <- function(analytic){
  
  inner_fracture_characteristics <- function(df) {
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
    
    return(df_final)
  }
  
  df <- analytic %>%
    select(study_id, enrolled, fracture_type, injury_gustilo, injury_classification_tscherne, treatment_arm) %>%
    filter(enrolled) %>%
    mutate(closed = ifelse(!is.na(injury_classification_tscherne), TRUE, FALSE)) %>%
    mutate(open = ifelse(!is.na(injury_gustilo), TRUE, FALSE))
  
  total <- sum(df$enrolled)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  df_final_a <- inner_fracture_characteristics(df_a)
  df_final_b <- inner_fracture_characteristics(df_b)
  df_final_full <- inner_fracture_characteristics(df)
  
  df_table <- full_join(df_final_a, df_final_b, by = "type", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(df_final_full, by = "type") %>%
    select(type, ends_with(" (Group A)"), ends_with(" (Group B)"), percentage)
  
  cnames <- c(' ', paste('Group A (n=', sum(df_a$enrolled), ')'),
              paste('Group B (n=', sum(df_b$enrolled), ')'),
              paste('Overall (n=', total, ')'))
  header <- c(1,1,1,1)
  names(header) <- cnames
  
  n_closed <- nrow(df_table %>% filter(str_detect(type, "Closed Fracture")))
  n_open <- nrow(df_table %>% filter(str_detect(type, "Open Fracture")))
  n_frac <- nrow(df_table %>% filter(str_detect(type, "Tibial")))
  n_tscherne <- nrow(df_table %>% filter(str_detect(type, "Tscherne")))
  n_gustilo <- nrow(df_table %>% filter(str_detect(type, "Gustilo")))
  
  vis <- kable(df_table, align='l', padding='2l', col.names = cnames) %>%
    pack_rows(index = c('Fractured Bone' = n_frac,
                        
                        'Fracture Type' = (n_closed + n_tscherne + n_open + n_gustilo)),
              label_row_css = "text-align:left") %>%
    add_indent(c(seq(n_tscherne) + n_frac + n_closed, seq(n_gustilo) + n_frac + n_closed + n_open + n_tscherne)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(vis)
}

#' Closed Treatment characteristics Sextant
#'
#' @description This function visualizes the treatment characteristics per protocol and assignment for Sextant by treatment arm.
#'
#' @param analytic This is the analytic data set that must include local_antibiotic_at_dwc, treatment_arm,
#' systemic_antibiotic_post_dwc, no_other_antibiotic_at_dwc
#'
#' @return A kable table

#' @export
#'
#' @examples
#' \dontrun{
#' closed_treatment_characteristics_sextant(analytic)
#' }
closed_treatment_characteristics_sextant <- function(analytic){
  
  inner_treatment_characteristics_sextant <- function(df) {
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
    
    return(df)
  }
  
  df <- analytic %>%
    select(local_antibiotic_at_dwc, treatment_arm, systemic_antibiotic_post_dwc, no_other_antibiotic_at_dwc)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  df_a_final <- inner_treatment_characteristics_sextant(df_a)
  df_b_final <- inner_treatment_characteristics_sextant(df_b)
  df_full_final <- inner_treatment_characteristics_sextant(df)
  
  df_table <- full_join(df_a_final, df_b_final, by = c("Local antibiotic treatment at DWC", "Systemic antibiotic treatment post DWC", "No other local antibiotic use at DWC"), suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(df_full_final, by = c("Local antibiotic treatment at DWC", "Systemic antibiotic treatment post DWC", "No other local antibiotic use at DWC"))
  
  output <- kable(df_table, align='l', padding='2l') %>%
    add_header_above(c(" " = 3, "Group A" = 1, "Group B" = 1, "Overall" = 1)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed Followup 2 week status by site for Sextant
#'
#' @description This function visualizes 2 weeks followup status by site for Clinical followup form(crf12) and patient
#' reported outcome forms(CRF 14 & 15) for Sextant weekly report  by treatment arm

#'
#' @param analytic This is the analytic data set that must include study_id, eligible, enrolled, time_zero, facilitycode,
#' followup_expected_2wk, followup_complete_crf12_2wk, followup_incomplete_crf12_2wk, followup_early_crf12_2wk,
#' followup_late_crf12_2wk, followup_missing_crf12_2wk, followup_not_started_crf12_2wk,
#' followup_status_crf14_crf15_2wk,
#' treatment_arm

#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_followup_2wk_status_by_site_sextant(analytic)
#' }
closed_followup_2wk_status_by_site_sextant <- function(analytic){
  
  inner_followup_2wk_status_by_site_sextant <- function(df) {
    df_crf12 <- df %>%
      select(study_id, facilitycode, followup_complete_crf12_2wk, followup_incomplete_crf12_2wk,
             followup_early_crf12_2wk, followup_late_crf12_2wk, followup_missing_crf12_2wk, followup_not_started_crf12_2wk) %>%
      group_by(facilitycode) %>%
      summarise("Complete" = sum(followup_complete_crf12_2wk, na.rm = TRUE), "Incomplete" = sum(followup_incomplete_crf12_2wk, na.rm = TRUE),
                "Early" = sum(followup_early_crf12_2wk, na.rm = TRUE), "Late" = sum(followup_late_crf12_2wk, na.rm = TRUE),
                "Missing" = sum(followup_missing_crf12_2wk, na.rm = TRUE), "Not Started" = sum(followup_not_started_crf12_2wk, na.rm = TRUE))
    df_crf14_crf15 <- df %>%
      select(study_id, facilitycode, followup_status_crf14_crf15_2wk) %>%
      pivot_wider(names_from = followup_status_crf14_crf15_2wk, values_from = followup_status_crf14_crf15_2wk) %>%
      mutate(across(-c(study_id, facilitycode), ~ !is.na(.)), not_started = FALSE) %>%
      group_by(facilitycode) %>%
      summarise("Complete_crf14_15" = sum(complete, na.rm = TRUE),
                "Incomplete_crf14_15" = sum(incomplete, na.rm = TRUE),
                "Early_crf14_15" = sum(early, na.rm = TRUE),
                "Late_crf14_15" = sum(late, na.rm = TRUE),
                "Missing_crf14_15" = sum(missing, na.rm = TRUE),
                "Not Started_crf14_15" = sum(not_started, na.rm = TRUE))
    
    exclude_columns <- c("facilitycode", "eligible_and_enrolled", "dwc_completed", "expected")
    
    df_expected_2wk <- df %>% 
      select(study_id, facilitycode, eligible, enrolled, time_zero, followup_expected_2wk) %>% 
      mutate(dwc_complete = ifelse(!is.na(time_zero), TRUE, FALSE)) %>% 
      mutate(eligible_enrolled = ifelse(eligible & enrolled, TRUE, FALSE)) %>% 
      select(-time_zero, -enrolled, -eligible) %>% 
      group_by(facilitycode) %>% 
      summarise("eligible_and_enrolled"= sum(eligible_enrolled, na.rm = TRUE), "dwc_completed"= sum(dwc_complete, na.rm = TRUE),
                "expected"= sum(followup_expected_2wk, na.rm = TRUE)) %>% 
      left_join(df_crf12) %>% 
      left_join(df_crf14_crf15) %>% 
      mutate(across(-one_of(exclude_columns),
                    ~ format_count_percent(., expected))) %>% 
      rename(`Clinical Site` = facilitycode,
             `Eligible & Enrolled` = eligible_and_enrolled,
             `DF Complete` = dwc_completed,
             `Expected` = expected) 
    
    colnames(df_expected_2wk) <- gsub("Complete_crf14_15", "Complete", gsub("Incomplete_crf14_15", "Incomplete", 
                                                                            gsub("Missing_crf14_15", "Missing", gsub("Early_crf14_15", "Early", 
                                                                                                                     gsub("Late_crf14_15", "Late", gsub("Not Started_crf14_15", "Not started", 
                                                                                                                                                        colnames(df_expected_2wk)))))))
    
    return(df_expected_2wk)
  }
  
  df <- analytic %>%
    select(study_id, eligible, enrolled, facilitycode, time_zero, followup_expected_2wk, followup_complete_crf12_2wk,
           followup_incomplete_crf12_2wk, followup_early_crf12_2wk, followup_late_crf12_2wk, followup_missing_crf12_2wk,
           followup_not_started_crf12_2wk, followup_status_crf14_crf15_2wk, treatment_arm)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  table_raw_a <- inner_followup_2wk_status_by_site_sextant(df_a)
  table_raw_b <- inner_followup_2wk_status_by_site_sextant(df_b)
  
  df_table <- full_join(table_raw_a, table_raw_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  output <- kable(df_table, align='l') %>%
    add_header_above(c(" " = 1, "Group A" = (ncol(df_table)-1)/2, "Group B" = (ncol(df_table)-1)/2)) %>%
    add_header_above(c(" " = 1, "Group A" = 4, " " = 6, "Group B" = 4, " " = 6), align = "c") %>%
    add_header_above(c(" " = 1, "2 Weeks CRF12 (Clinical followup form)" = 10, "2 Weeks CRF14 & CRF15 (Patient reported outcomes)" = 10)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed Followup 3 month status by site for Sextant
#'
#' @description This function visualizes 3 month followup status by site for Clinical followup form(crf12) and patient
#' reported outcome forms(CRF 14 & 15) for Sextant weekly report by treatment arm
#'
#' @param analytic This is the analytic data set that must include study_id, eligible, enrolled, facilitycode, followup_expected_3mo, time_zero,
#' followup_complete_crf12_3mo, followup_incomplete_crf12_3mo, followup_early_crf12_3mo, followup_late_crf12_3mo,
#' followup_missing_crf12_3mo, followup_not_started_crf12_3mo, followup_status_crf14_crf15_3mo, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_followup_3mo_status_by_site_sextant(analytic)
#' }
closed_followup_3mo_status_by_site_sextant <- function(analytic){
  
  inner_followup_3mo_status_by_site_sextant <- function(df) {
    df_crf12 <- df %>% 
      select(study_id, facilitycode, followup_complete_crf12_3mo, followup_incomplete_crf12_3mo, 
             followup_early_crf12_3mo, followup_late_crf12_3mo, followup_missing_crf12_3mo, followup_not_started_crf12_3mo) %>% 
      group_by(facilitycode) %>% 
      summarise("Complete" = sum(followup_complete_crf12_3mo, na.rm = TRUE), "Incomplete" = sum(followup_incomplete_crf12_3mo, na.rm = TRUE),
                "Early" = sum(followup_early_crf12_3mo, na.rm = TRUE), "Late" = sum(followup_late_crf12_3mo, na.rm = TRUE),
                "Missing" = sum(followup_missing_crf12_3mo, na.rm = TRUE), "Not Started" = sum(followup_not_started_crf12_3mo, na.rm = TRUE))
    
    df_crf14_crf15 <- df %>%
      select(study_id, facilitycode, followup_status_crf14_crf15_3mo) %>%
      pivot_wider(names_from = followup_status_crf14_crf15_3mo, values_from = followup_status_crf14_crf15_3mo) %>%
      mutate(across(-c(study_id, facilitycode), ~ !is.na(.)), not_started = FALSE) %>%
      group_by(facilitycode) %>%
      summarise("Complete_crf14_15" = sum(complete, na.rm = TRUE),
                "Incomplete_crf14_15" = sum(incomplete, na.rm = TRUE),
                "Early_crf14_15" = sum(early, na.rm = TRUE),
                "Late_crf14_15" = sum(late, na.rm = TRUE),
                "Missing_crf14_15" = sum(missing, na.rm = TRUE),
                "Not Started_crf14_15" = sum(not_started, na.rm = TRUE))
    
    exclude_columns <- c("facilitycode", "eligible_and_enrolled", "dwc_completed", "expected")
    
    df_expected_3mo <- df %>% 
      select(study_id, facilitycode, eligible, enrolled, time_zero, followup_expected_3mo) %>% 
      mutate(dwc_complete = ifelse(!is.na(time_zero), TRUE, FALSE)) %>% 
      mutate(eligible_enrolled = ifelse(eligible & enrolled, TRUE, FALSE)) %>% 
      select(-time_zero, -enrolled, -eligible) %>% 
      group_by(facilitycode) %>% 
      summarise("eligible_and_enrolled"= sum(eligible_enrolled, na.rm = TRUE), "dwc_completed"= sum(dwc_complete, na.rm = TRUE),
                "expected"= sum(followup_expected_3mo, na.rm = TRUE)) %>% 
      left_join(df_crf12) %>% 
      left_join(df_crf14_crf15) %>% 
      mutate(across(-one_of(exclude_columns),
                    ~ format_count_percent(., expected))) %>% 
      rename(`Clinical Site` = facilitycode,
             `Eligible & Enrolled` = eligible_and_enrolled,
             `DF Complete` = dwc_completed,
             `Expected` = expected) 
    
    colnames(df_expected_3mo) <- gsub("Complete_crf14_15", "Complete", gsub("Incomplete_crf14_15", "Incomplete", 
                                                                            gsub("Missing_crf14_15", "Missing", gsub("Early_crf14_15", "Early", 
                                                                                                                     gsub("Late_crf14_15", "Late", gsub("Not Started_crf14_15", "Not started", 
                                                                                                                                                        colnames(df_expected_3mo)))))))
    
    return(df_expected_3mo)
  }
  
  df <- analytic %>%
    select(study_id, eligible, enrolled, facilitycode, followup_expected_3mo, time_zero,
           followup_complete_crf12_3mo, followup_incomplete_crf12_3mo, followup_early_crf12_3mo,
           followup_late_crf12_3mo, followup_missing_crf12_3mo, followup_not_started_crf12_3mo,
           followup_status_crf14_crf15_3mo, treatment_arm)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  table_raw_a <- inner_followup_3mo_status_by_site_sextant(df_a)
  table_raw_b <- inner_followup_3mo_status_by_site_sextant(df_b)
  
  df_table <- full_join(table_raw_a, table_raw_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  output <- kable(df_table, align='l') %>%
    add_header_above(c(" " = 1, "Group A" = (ncol(df_table)-1)/2, "Group B" = (ncol(df_table)-1)/2)) %>%
    add_header_above(c(" " = 1, "Group A" = 4, " " = 6, "Group B" = 4, " " = 6), align = "c") %>%
    add_header_above(c(" " = 1, "3 Months CRF12 (Clinical followup form)" = 10, "3 Months CRF14 & CRF15 (Patient reported outcomes)" = 10)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed Followup 6 month status by site for Sextant
#'
#' @description This function visualizes 6 month followup status by site for Clinical followup form(crf12) and patient
#' reported outcome forms(CRF 14 & 15) for Sextant weekly report by treatment arm
#'
#' @param analytic This is the analytic data set that must include study_id, eligible, enrolled, facilitycode,
#' followup_expected_6mo, time_zero, followup_complete_crf12_6mo, followup_incomplete_crf12_6mo,
#' followup_early_crf12_6mo, followup_late_crf12_6mo, followup_missing_crf12_6mo, followup_not_started_crf12_6mo,
#' followup_status_crf14_crf15_6mo, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_followup_6mo_status_by_site_sextant(analytic)
#' }
closed_followup_6mo_status_by_site_sextant <- function(analytic){
  
  inner_followup_6mo_status_by_site_sextant <- function(df) {
    df_crf12 <- df %>% 
      select(study_id, facilitycode, followup_complete_crf12_6mo, followup_incomplete_crf12_6mo, 
             followup_early_crf12_6mo, followup_late_crf12_6mo, followup_missing_crf12_6mo, followup_not_started_crf12_6mo) %>% 
      group_by(facilitycode) %>% 
      summarise("Complete" = sum(followup_complete_crf12_6mo, na.rm = TRUE), "Incomplete" = sum(followup_incomplete_crf12_6mo, na.rm = TRUE),
                "Early" = sum(followup_early_crf12_6mo, na.rm = TRUE), "Late" = sum(followup_late_crf12_6mo, na.rm = TRUE),
                "Missing" = sum(followup_missing_crf12_6mo, na.rm = TRUE), "Not Started" = sum(followup_not_started_crf12_6mo, na.rm = TRUE))
    
    df_crf14_crf15 <- df %>%
      select(study_id, facilitycode, followup_status_crf14_crf15_6mo) %>%
      pivot_wider(names_from = followup_status_crf14_crf15_6mo, values_from = followup_status_crf14_crf15_6mo) %>%
      mutate(across(-c(study_id, facilitycode), ~ !is.na(.)), not_started = FALSE) %>%
      group_by(facilitycode) %>%
      summarise("Complete_crf14_15" = sum(complete, na.rm = TRUE),
                "Incomplete_crf14_15" = sum(incomplete, na.rm = TRUE),
                "Early_crf14_15" = sum(early, na.rm = TRUE),
                "Late_crf14_15" = sum(late, na.rm = TRUE),
                "Missing_crf14_15" = sum(missing, na.rm = TRUE),
                "Not Started_crf14_15" = sum(not_started, na.rm = TRUE))
    exclude_columns <- c("facilitycode", "eligible_and_enrolled", "dwc_completed", "expected")
    
    df_expected_6mo <- df %>% 
      select(study_id, facilitycode, eligible, enrolled, time_zero, followup_expected_6mo) %>% 
      mutate(dwc_complete = ifelse(!is.na(time_zero), TRUE, FALSE)) %>% 
      mutate(eligible_enrolled = ifelse(eligible & enrolled, TRUE, FALSE)) %>% 
      select(-time_zero, -enrolled, -eligible) %>% 
      group_by(facilitycode) %>% 
      summarise("eligible_and_enrolled"= sum(eligible_enrolled, na.rm = TRUE), "dwc_completed"= sum(dwc_complete, na.rm = TRUE),
                "expected"= sum(followup_expected_6mo, na.rm = TRUE)) %>% 
      left_join(df_crf12) %>% 
      left_join(df_crf14_crf15) %>% 
      mutate(across(-one_of(exclude_columns),
                    ~ format_count_percent(., expected))) %>% 
      rename(`Clinical Site` = facilitycode,
             `Eligible & Enrolled` = eligible_and_enrolled,
             `DF Complete` = dwc_completed,
             `Expected` = expected) 
    
    colnames(df_expected_6mo) <- gsub("Complete_crf14_15", "Complete", gsub("Incomplete_crf14_15", "Incomplete", 
                                                                            gsub("Missing_crf14_15", "Missing", gsub("Early_crf14_15", "Early", 
                                                                                                                     gsub("Late_crf14_15", "Late", gsub("Not Started_crf14_15", "Not started", 
                                                                                                                                                        colnames(df_expected_6mo)))))))
    
    return(df_expected_6mo)
  }
  
  df <- analytic %>%
    select(study_id, eligible, enrolled, facilitycode, followup_expected_6mo, time_zero,
           followup_complete_crf12_6mo, followup_incomplete_crf12_6mo,
           followup_early_crf12_6mo, followup_late_crf12_6mo, followup_missing_crf12_6mo, followup_not_started_crf12_6mo,
           followup_status_crf14_crf15_6mo, treatment_arm)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  table_raw_a <- inner_followup_6mo_status_by_site_sextant(df_a)
  table_raw_b <- inner_followup_6mo_status_by_site_sextant(df_b)
  
  df_table <- full_join(table_raw_a, table_raw_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  output <- kable(df_table, align='l') %>%
    add_header_above(c(" " = 1, "Group A" = (ncol(df_table)-1)/2, "Group B" = (ncol(df_table)-1)/2)) %>%
    add_header_above(c(" " = 1, "Group A" = 4, " " = 6, "Group B" = 4, " " = 6), align = "c") %>%
    add_header_above(c(" " = 1, "6 Months CRF12 (Clinical followup form)" = 10, "6 Months CRF14 & CRF15 (Patient reported outcomes)" = 10)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed Followup 12 month status by site for Sextant
#'
#' @description This function visualizes 12 month followup status by site using CRF12(Clinical followup form),
#' PROMIS pain interference/intensity + CRF15(Survey version) or CRF14 + CRF15(Redcap version), and CRF08(Medical
#' Record Review) form by treatment arm
#'
#' @param analytic This is the analytic data set that must include study_id, eligible, enrolled, facilitycode,
#' followup_expected_12mo, time_zero, followup_complete_crf12_12mo, followup_incomplete_crf12_12mo,
#' followup_early_crf12_12mo, followup_late_crf12_12mo, followup_missing_crf12_12mo,
#' followup_not_started_crf12_12mo, followup_status_crf14_crf15_12mo, followup_status_crf09_12mo, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_followup_12mo_status_by_site_sextant(analytic)
#' }
closed_followup_12mo_status_by_site_sextant <- function(analytic){
  
  inner_followup_12mo_status_by_site_sextant <- function(df) {
    df_crf12 <- df %>% 
      select(study_id, facilitycode, followup_complete_crf12_12mo, followup_incomplete_crf12_12mo, 
             followup_early_crf12_12mo, followup_late_crf12_12mo, followup_missing_crf12_12mo, followup_not_started_crf12_12mo) %>% 
      group_by(facilitycode) %>% 
      summarise("Complete" = sum(followup_complete_crf12_12mo, na.rm = TRUE), "Incomplete" = sum(followup_incomplete_crf12_12mo, na.rm = TRUE),
                "Early" = sum(followup_early_crf12_12mo, na.rm = TRUE), "Late" = sum(followup_late_crf12_12mo, na.rm = TRUE),
                "Missing" = sum(followup_missing_crf12_12mo, na.rm = TRUE), "Not Started" = sum(followup_not_started_crf12_12mo, na.rm = TRUE)) %>% 
      mutate(facilitycode = as.character(facilitycode)) 
    
    all_categories <- c("complete", "incomplete", "missing", "early", "late", "not_started")
    
    empty_df <- tibble(
      study_id = as.character(integer()),  
      !!!setNames(rep(list(""), length(all_categories)), all_categories))
    
    df_crf14_crf15 <- df %>%
      select(study_id, facilitycode, followup_status_crf14_crf15_12mo) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_wider(names_from = followup_status_crf14_crf15_12mo, 
                  values_from = followup_status_crf14_crf15_12mo, 
                  values_fill = list(followup_status_crf14_crf15_12mo = ""))
    
    df_pivot <- left_join(df_crf14_crf15, empty_df) %>% 
      select(study_id, facilitycode, complete, incomplete, missing, early, late, not_started) %>% 
      mutate(across(-c(study_id, facilitycode), ~if_else(is.na(.) | . == "", FALSE, TRUE)))
    
    df_crf1415 <- df_pivot %>% 
      group_by(facilitycode) %>%
      summarise("Complete_crf14_15" = sum(complete, na.rm = TRUE),
                "Incomplete_crf14_15" = sum(incomplete, na.rm = TRUE),
                "Early_crf14_15" = sum(early, na.rm = TRUE),
                "Late_crf14_15" = sum(late, na.rm = TRUE),
                "Missing_crf14_15" = sum(missing, na.rm = TRUE),
                "Not Started_crf14_15" = sum(not_started, na.rm = TRUE)) %>% 
      mutate(facilitycode = as.character(facilitycode)) 
    
    df_crf09 <- df %>%
      select(study_id, facilitycode, followup_status_crf09_12mo) %>%
      mutate(across(everything(), as.character)) %>%
      pivot_wider(names_from = followup_status_crf09_12mo, 
                  values_from = followup_status_crf09_12mo, 
                  values_fill = list(followup_status_crf09_12mo = ""))
    
    merged_crf09 <- left_join(df_crf09, empty_df) %>% 
      select(study_id, facilitycode, complete, incomplete, missing, early, late, not_started) %>% 
      mutate(across(-c(study_id, facilitycode), ~if_else(is.na(.) | . == "", FALSE, TRUE))) %>% 
      group_by(facilitycode) %>%
      summarise("Complete_crf09" = sum(complete, na.rm = TRUE),
                "Incomplete_crf09" = sum(incomplete, na.rm = TRUE),
                "Early_crf09" = sum(early, na.rm = TRUE),
                "Late_crf09" = sum(late, na.rm = TRUE),
                "Missing_crf09" = sum(missing, na.rm = TRUE),
                "Not Started_crf09" = sum(not_started, na.rm = TRUE)) %>% 
      mutate(facilitycode = as.character(facilitycode)) 
    
    exclude_columns <- c("facilitycode", "eligible_and_enrolled", "dwc_completed", "expected")
    
    df_expected_12mo <- df %>% 
      select(study_id, facilitycode, eligible, enrolled, time_zero, followup_expected_12mo) %>% 
      mutate(dwc_complete = ifelse(!is.na(time_zero), TRUE, FALSE)) %>% 
      mutate(eligible_enrolled = ifelse(eligible & enrolled, TRUE, FALSE)) %>% 
      select(-time_zero, -enrolled, -eligible) %>% 
      group_by(facilitycode) %>% 
      summarise("eligible_and_enrolled"= sum(eligible_enrolled, na.rm = TRUE), "dwc_completed"= sum(dwc_complete, na.rm = TRUE),
                "expected"= sum(followup_expected_12mo, na.rm = TRUE)) %>% 
      left_join(df_crf12) %>% 
      left_join(df_crf1415) %>% 
      left_join(merged_crf09) %>% 
      mutate(across(-one_of(exclude_columns),
                    ~ format_count_percent(., expected))) %>% 
      rename(`Clinical Site` = facilitycode,
             `Eligible & Enrolled` = eligible_and_enrolled,
             `DF Complete` = dwc_completed,
             `Expected` = expected) 
    
    colnames(df_expected_12mo) <- gsub("Complete_crf14_15", "Complete", gsub("Incomplete_crf14_15", "Incomplete", 
                                                                             gsub("Missing_crf14_15", "Missing", gsub("Early_crf14_15", "Early", 
                                                                                                                      gsub("Late_crf14_15", "Late", gsub("Not Started_crf14_15", "Not started",
                                                                                                                                                         gsub("Complete_crf09", "Complete", gsub("Incomplete_crf09", "Incomplete", 
                                                                                                                                                                                                 gsub("Missing_crf09", "Missing", gsub("Early_crf09", "Early", 
                                                                                                                                                                                                                                       gsub("Late_crf09", "Late", gsub("Not Started_crf09", "Not started",
                                                                                                                                                                                                                                                                       colnames(df_expected_12mo)))))))))))))
    
    return(df_expected_12mo)
  }
  
  df <- analytic %>%
    select(study_id, eligible, enrolled, facilitycode, followup_expected_12mo, time_zero,
           followup_complete_crf12_12mo, followup_incomplete_crf12_12mo,
           followup_early_crf12_12mo, followup_late_crf12_12mo, followup_missing_crf12_12mo,
           followup_not_started_crf12_12mo, followup_status_crf14_crf15_12mo, followup_status_crf09_12mo, treatment_arm)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  table_raw_a <- inner_followup_12mo_status_by_site_sextant(df_a)
  table_raw_b <- inner_followup_12mo_status_by_site_sextant(df_b)
  
  df_table <- full_join(table_raw_a, table_raw_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  output <- kable(df_table, align='l') %>%
    add_header_above(c(" " = 1, "Group A" = (ncol(df_table)-1)/2, "Group B" = (ncol(df_table)-1)/2)) %>%
    add_header_above(c(" " = 1, "Group A" = 4, " " = 6, " " = 6, "Group B" = 4, " " = 6, " " = 6), align = "c") %>%
    add_header_above(c(" " = 1,
                       "12 Months CRF12 (Clinical followup form)" = 16,
                       "12 Months CRF14 & CRF15 (Patient reported outcomes)" = 16,
                       
                       "12 Months CRF09 (Medical record review)" = 16)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed Adherence by site
#'
#' @description This function visualizes the treatment crossover or any nonadherence occured during the Tobra
#' study by treatment arm.
#'
#' @param analytic This is the analytic data set that must include facilitycode, df_date, df_randomized_treatment, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_adherence_by_site(analytic)
#' }
closed_adherence_by_site <- function(analytic){
  
  inner_adherence_by_site <- function(df) {
    treatment_total <- sum(df$df_randomized_treatment, na.rm = TRUE)
    
    df2 <- df %>% 
      group_by(facilitycode) %>% 
      summarize(elig_enr = sum(enrolled, na.rm = TRUE), df_total = sum(df_complete, na.rm = TRUE), treatment_completed = sum(df_randomized_treatment, na.rm = TRUE)) %>% 
      mutate(treatment_completed = format_count_percent(treatment_completed, df_total)) 
    
    enrolled_total <- sum(df2$elig_enr, na.rm = TRUE)
    df_total <- sum(df2$df_total, na.rm = TRUE)
    
    df3 <- data.frame(facilitycode = 'TOTAL', elig_enr = enrolled_total, df_total = df_total, treatment_completed = format_count_percent(treatment_total, df_total))
    
    df4 <- rbind(df2, df3) %>% 
      rename(`Clinical Site` = facilitycode,
             `Eligible and Enrolled` = elig_enr,
             `Definitive Wound Closure completed` = df_total,
             `Received treatment per protocol and assignment(% DWC complete)` = treatment_completed)
    
    return(df4)
  }
  
  df <- analytic %>%
    select(facilitycode, enrolled, df_date, df_randomized_treatment, treatment_arm) %>%
    filter(enrolled) %>%
    mutate(df_date = na_if(df_date, "NA")) %>%
    mutate(df_complete = ifelse(!is.na(df_date), TRUE, FALSE))
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  df4_a <- inner_adherence_by_site(df_a)
  df4_b <- inner_adherence_by_site(df_b)
  
  df_table <- full_join(df4_a, df4_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  output <- kable(df_table, align='l', padding='2l') %>%
    add_header_above(c(" " = 1, "Group A" = (ncol(df_table)-1)/2, "Group B" = (ncol(df_table)-1)/2)) %>%
    row_spec(nrow(df_table), bold = TRUE) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed Enrollment by site tobra and sextant (var discontinued)
#'
#' @description This function visualizes the number of subjects enrolled, not enrolled etc, with specs for last 14 days and average by week
#' study by treatment arm.
#'
#' @param analytic This is the analytic data set that must include screened, eligible, refused, not_consented, not_randomized, consented_and_randomized, enrolled, site_certified_days,
#' facilitycode, adjudicated_discontinued, screened_date, treatment_arm
#' @param days Number of days to look back for enrollment activity (default is 14)
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_enrollment_by_site_var_disc_tobra_sextant(analytic)
#' }
closed_enrollment_by_site_var_disc_tobra_sextant <- function(analytic, days = 14){
  
  inner_enrollment_by_site_var_disc_tobra_sextant <- function(df) {
    last14 <- Sys.Date() - days
    df <- df %>%
      mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
      mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certified_days))) %>% 
      rename(Facility = facilitycode) %>% 
      filter(!is.na(Facility)) %>% 
      mutate(weeks_site_certified = site_certified_days/7)
    
    df_1st <- df %>% 
      group_by(Facility) %>% 
      summarize('Days Certified' = site_certified_days[1], Screened = sum(screened), Eligible = sum(eligible))
    
    df_2nd <- df %>% 
      filter(eligible == TRUE) %>% 
      group_by(Facility) %>% 
      summarize(Refused = sum(refused), 'Not Consented' = sum(not_consented), cnr = sum(consented_and_randomized))
    
    df_3rd <- df %>% 
      filter(eligible == TRUE & consented_and_randomized == TRUE) %>% 
      group_by(Facility) %>% 
      summarize('Discontinued' = sum(adjudicated_discontinued),
                "Enrolled" = sum(enrolled)) 
    
    table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
      left_join(df_3rd, by = 'Facility') %>% 
      mutate_all(~ifelse(is.na(.), 0, .))
    
    facilities <- df %>% 
      select(Facility) %>% 
      unique()
    
    last_fourteen <- df %>% 
      mutate(screened_date = ymd(screened_date)) %>% 
      mutate(Screened = ifelse(screened_date > last14, TRUE, FALSE)) %>% 
      filter(Screened) %>% 
      select(Facility, Screened, eligible, enrolled) %>% 
      group_by(Facility) %>% 
      summarize('Screened1' = sum(Screened, na.rm = TRUE),
                'Eligible1' = sum(eligible, na.rm = TRUE),
                'Enrolled1' = sum(enrolled, na.rm = TRUE))
    
    l14 <- left_join(facilities, last_fourteen) 
    
    by_week <- df %>%
      filter(!is.na(weeks_site_certified)) %>% 
      group_by(Facility) %>% 
      summarize(
        Screened2 = round(sum(screened, na.rm = TRUE) / first(weeks_site_certified), 2),
        Enrolled2 = round(sum(enrolled, na.rm = TRUE) / first(weeks_site_certified), 2))
    
    weekly <- left_join(facilities, by_week, by = 'Facility')
    
    almost <- left_join(l14, weekly, by = 'Facility')
    
    final <- left_join(almost, table_raw, by = 'Facility') %>% 
      adorn_totals("row") %>% 
      mutate(is_total=Facility=="Total") %>% 
      mutate(`Days Certified`=ifelse(is_total,NA,`Days Certified`)) %>% 
      arrange(desc(is_total), Facility) %>% 
      select(-is_total) %>% 
      mutate(`Discontinued (% randomized)` = format_count_percent(Discontinued, cnr)) %>% 
      mutate(`Eligible & Enrolled (% randomized)` = format_count_percent(Enrolled, cnr)) %>% 
      mutate(`Consented & Randomized (% eligible)` = format_count_percent(cnr, Eligible)) %>% 
      mutate(`Refused (% eligible)` = format_count_percent(Refused, Eligible)) %>% 
      mutate(`Not Enrolled for 'Other' Reasons (% eligible)` = format_count_percent(`Not Consented`, Eligible)) %>% 
      mutate(`Eligible (% screened)` = format_count_percent(Eligible, Screened))
    
    total_row <- final %>% 
      slice_head(n=1)
    
    last <- bind_rows(final, total_row) %>% 
      slice_tail(n=-1) %>% 
      select(-`Days Certified`, -Eligible, -Enrolled, -Refused, -`Not Consented`, -cnr, -Discontinued) %>% 
      select(Facility, Screened1, Eligible1, Enrolled1, Screened2, Enrolled2, Screened, `Eligible (% screened)`, `Refused (% eligible)`, `Not Enrolled for 'Other' Reasons (% eligible)`, 
             `Consented & Randomized (% eligible)`, `Discontinued (% randomized)`, `Eligible & Enrolled (% randomized)`) %>% 
      mutate(`Eligible1` = format_count_percent(`Eligible1`, `Screened1`),
             `Enrolled1` = format_count_percent(`Enrolled1`, `Screened1`))
    
    colnames(last) <- c('Facility', 'Screened', 'Eligible', 'Enrolled', "Screened per week", 'Enrolled per week', 'Screened total', 'Eligible (% screened)', 'Refused (% eligible)', 'Not Enrolled for `Other` Reasons (% eligible)', 
                        'Consented & Randomized (% eligible)', 'Discontinued (% randomized)', 'Eligible & Enrolled (% randomized)' )
    
    return(last)
  }
  
  df <- analytic %>%
    select(screened, eligible, refused, not_consented, not_randomized, consented_and_randomized, enrolled, site_certified_days,
           facilitycode, adjudicated_discontinued, screened_date, treatment_arm)
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  last_a <- inner_enrollment_by_site_var_disc_tobra_sextant(df_a)
  
  last_b <- inner_enrollment_by_site_var_disc_tobra_sextant(df_b)
  
  df_table <- full_join(last_a, last_b, by = "Facility", suffix = c(" (Group A)", " (Group B)")) %>%
    select(Facility, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  header_num <- c(1,3,2,7,3,2,7)
  header_names <- c(" ", paste("Last", days, " Days"), paste("Average per week"), paste("Cumulative", "to date"), 
                    paste("Last", days, " Days"), paste("Average per week"), paste("Cumulative", "to date"))
  names(header_num) <- header_names
  table <- kable(df_table, align='l', padding='2l') %>%
    add_header_above(header_num) %>%
    kable_styling("striped", full_width = F, position="left") %>%
    row_spec(nrow(df_table), bold = TRUE)
  
  return(table)
}