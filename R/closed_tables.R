

#' Closed Injury characteristics for OTA classification and Schatzker Type injuries
#'
#' @description This function visualizes the Injury characteristics for OTA classification and Schatzker Types for Ankle and Plateau
#' injuries 
#'
#' @param analytic This is the analytic data set that must include treatment_arm injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' closed_injury_ankle_plateau_characteristics()
#' }
closed_injury_ankle_plateau_characteristics <- function(analytic){
   
  confirm_stability_of_related_visual('injury_ankle_plateau_characteristics', 'f8abb9ea7ed258855162e2a98d3218a3')
  
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
      mutate(injury_classification_ankle_ota = ifelse(injury_type == "ankle" & is.na(injury_classification_ankle_ota) & is.na(injury_classification_plat_schatzker), "Missed", injury_classification_ankle_ota)) %>% 
      mutate(injury_classification_plat_schatzker = ifelse(injury_type == "plateau" & is.na(injury_classification_ankle_ota) & is.na(injury_classification_plat_schatzker), "Missed", injury_classification_plat_schatzker)) %>% 
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
    
    df_table
  }
  
  table_a <- inner_closed_injury_ankle_plateau_characteristics(df_a)
  table_b <- inner_closed_injury_ankle_plateau_characteristics(df_b)
  table_full <- inner_closed_injury_ankle_plateau_characteristics(df)
  
  df_table <- full_join(full_join(table_a, table_b, by="Name"), 
                        table_full, by="Name")
  
  colnames(df_table) <- c("Name", "Group A", "Group B", "Total")
  
  
  df_table <- df_table %>%
    mutate_all(~ ifelse(is.na(.), "-", .))
  
  
  index_vec <- c("OTA Classification"= ota_number + 1, "Tibial Plateau" = schatzer_number + 1)
  
 
  table_raw <- kable(df_table, format = "html", align = 'l', col.names = colnames(df_table)) %>%
    add_indent(c(seq(ota_number) + 1, seq(schatzer_number) + 1 + ota_number + 1)) %>% 
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = FALSE, position = "left")

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
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_baseline_characteristics_percent("Replace with Analytic Tibble")
#' 
closed_baseline_characteristics_percent <- function(analytic, sex="sex", race="ethnicity_race", education="education_level", military="military_status",
                                                       sex_levels=c("Female","Male", "Missing"), 
                                                       race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                                       education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing"), 
                                                       military_levels=c("Active Military", "Active Reserves", "Not Active Duty","Missing")){
  
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("sex", "ethnicity_race", "education_level", "age", "age_group",
                           "enrolled", "military_status", "treatment_arm"),
    example_types = c("NamedCategory['Female' 'Male' 'Missing']", "NamedCategory['Non-Hispanic White' 'Non-Hispanic Black' 'Hispanic' 'Other' 'Missing']",
                      "NamedCategory['Less than High School' 'GED or High School Diploma' 'More than High School' 'Refused / Don't know' 'Missing']",
                      "Number", "Category", "Boolean", "NamedCategory['Active Military' 'Active Reserves' 'Not Active Duty' 'Missing']",
                      "TreatmentArm")) 
  
  confirm_stability_of_related_visual("baseline_characteristics_percent", "a94af36b5ce50165af1612005be223db")
  
  sex_df <- tibble()
  age_df <- tibble()
  age_group_df <- tibble()
  race_df <- tibble()
  education_df <- tibble()
  military_df <- tibble()
  
  
  inner_baseline_characteristics_percent <- function(inner_analytic){
    constructs <- c(sex, race, education, military)
    
    sex_default <- tibble(type=sex_levels)
    race_default <- tibble(type=race_levels)
    education_default <- tibble(type=education_levels)
    military_default <- tibble(type=military_levels)
    
    
    df <- inner_analytic %>% 
      select(enrolled, age_group, age, all_of(constructs)) %>% 
      filter(enrolled) %>% 
      rename(sex = !!sym(sex)) %>% 
      rename(race = !!sym(race)) %>% 
      rename(education = !!sym(education)) %>% 
      rename(military = !!sym(military)) %>% 
      mutate(age = as.numeric(age))
    
    total <- sum(df$enrolled)
    
    sex_df <<- df %>% 
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
      mutate(Category = 'Sex')
    
    age_df <<- df %>% 
      summarize( type = 'Mean (SD)', percentage = format_mean_sd(age))
    
    
    age_group_df <<- df %>% 
      mutate(age_group = replace_na(age_group, "Missing")) %>% 
      group_by(age_group) %>% 
      count(age_group) %>% 
      rename(number = n) %>% 
      mutate(percentage = format_count_percent(number, total)) %>% 
      select(-number) %>% 
      rename(type = age_group)%>% 
      mutate(Category = 'Age')
    
    education_df <<- df %>% 
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
      select(-order)%>% 
      mutate(Category = 'Education')
    
    race_df <<- df %>% 
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
      select(-order)%>% 
      mutate(Category = 'Race')
    
    military_df <<- df %>% 
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
      select(-order)%>% 
      mutate(Category = 'Military')
    
    df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df, military_df) %>% 
      mutate_all(replace_na, "0 (0%)") %>% 
      ungroup()
    df_final
  }
  
  
  df_a <- analytic %>% filter(treatment_arm=="Group A")
  df_b <- analytic %>% filter(treatment_arm=="Group B")
  
  output_a <- inner_baseline_characteristics_percent(df_a) %>% mutate(percentage = replace_na(percentage, "NA")) 
  output_b <- inner_baseline_characteristics_percent(df_b) %>% mutate(percentage = replace_na(percentage, "NA"))
  output_total <- inner_baseline_characteristics_percent(analytic) %>% mutate(percentage = replace_na(percentage, "NA"))
  
  
  full_output <- full_join(output_a, output_b, by = c('Category', 'type'))
  
  
  full_output <- full_join(full_output, output_total, by = c('Category', 'type')) %>% 
    reorder_rows(list(Category = c('Sex', '0 (0%)', 'Age', 'Race', 'Education', 'Military'))) %>% 
    mutate_all(replace_na, "0 (0%)") %>% 
    select(-Category)
  
  colnames(full_output) <- c(" ", paste0("Group A (n=",nrow(df_a %>% filter(enrolled)),")"), paste0("Group B (n=",nrow(df_b %>% filter(enrolled)),")"), paste0("Total (n=",nrow(df_a %>% filter(enrolled))+nrow(df_b %>% filter(enrolled)),")"))
  
  vis <- kable(full_output, format="html", align='l') %>%
    pack_rows(index = c('Sex' = nrow(sex_df), 'Age' = (nrow(age_df) + nrow(age_group_df)), 'Race' = nrow(race_df), 
                        'Education' = nrow(education_df), 'Military' = nrow(military_df)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis) 
} 


#' Closed Baseline Characteristics Percent (No Military Status)
#'
#' @description This function visualizes the categorical percentages of baseline characteristics sex, age, race, and education
#'
#' @param analytic This is the analytic data set that must include treatment_arm enrolled, age, age_group
#' @param sex is a meta construct that is required that defaults to "sex"
#' @param race is a meta construct that is required that defaults to "race_ethnicity"
#' @param education is a meta construct that is required that defaults to "education_level"
#' @param sex_levels sets default values and orders for sex meta construct
#' @param race_levels sets default values and orders for race meta construct
#' @param education_levels sets default values and orders for education meta construct
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' closed_baseline_characteristics_percent_nm()
#' }
closed_baseline_characteristics_percent_nm <- function(analytic, sex="sex", race="ethnicity_race", education="education_level",
                                                       sex_levels=c("Female","Male", "Missing"), 
                                                       race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                                       education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing")){
  
  
  confirm_stability_of_related_visual("baseline_characteristics_percent_nm", "fe7cb8b490a86eccd44baa0eaab32b6d")
  
  sex_df <- tibble()
  age_df <- tibble()
  age_group_df <- tibble()
  race_df <- tibble()
  education_df <- tibble()
  
  inner_baseline_characteristics_percent_nm <- function(inner_analytic){
    constructs <- c(sex, race, education)
    
    sex_default <- tibble(type=sex_levels)
    race_default <- tibble(type=race_levels)
    education_default <- tibble(type=education_levels)
    
    
    df <- inner_analytic %>% 
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
      select(-order) %>% 
      mutate(Category = 'sex')
    
    age_df <<- df %>% 
      summarize( type = 'Mean (SD)', percentage = format_mean_sd(age))%>% 
      mutate(Category = 'age')
    
    
    age_group_df <<- df %>% 
      mutate(age_group = replace_na(age_group, "Missing")) %>% 
      group_by(age_group) %>% 
      count(age_group) %>% 
      rename(number = n) %>% 
      mutate(percentage = format_count_percent(number, total)) %>% 
      select(-number) %>% 
      rename(type = age_group)%>% 
      mutate(Category = 'age')
    
    education_df <<- df %>% 
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
      select(-order)%>% 
      mutate(Category = 'education')
    
    race_df <<- df %>% 
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
      select(-order)%>% 
      mutate(Category = 'race')
    
    
    df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df) %>% 
      mutate_all(replace_na, "0 (0%)") %>% 
      ungroup()
    df_final
  }
  
  
  df_a <- analytic %>% filter(treatment_arm=="Group A")
  df_b <- analytic %>% filter(treatment_arm=="Group B")
  
  output_a <- inner_baseline_characteristics_percent_nm(df_a) %>% mutate(percentage = replace_na(percentage, "NA")) 
  output_b <- inner_baseline_characteristics_percent_nm(df_b) %>% mutate(percentage = replace_na(percentage, "NA"))
  output_total <- inner_baseline_characteristics_percent_nm(analytic) %>% mutate(percentage = replace_na(percentage, "NA"))
  
  
  full_output <- full_join(output_a, output_b, by = c('Category', 'type'))
  
  
  full_output <- full_join(full_output, output_total, by = c('Category', 'type')) %>% 
    reorder_rows(list(Category = c('sex', 'age', 'race', 'education'))) %>% 
    mutate_all(replace_na, "0 (0%)") %>% 
    select(-Category)
  
  colnames(full_output) <- c(" ", paste0("Group A (n=",nrow(df_a %>% filter(enrolled)),")"), paste0("Group B (n=",nrow(df_b %>% filter(enrolled)),")"), paste0("Total (n=",nrow(df_a %>% filter(enrolled))+nrow(df_b %>% filter(enrolled)),")"))
  
  category_counts <- output_total %>%
    count(`Category`) %>%
    pull(n, name = `Category`)
  
  vis <- kable(full_output, format = "html", align = 'l') %>%
    pack_rows(
      index = c(
        'Sex' = as.numeric(category_counts['sex']), 
        'Age' = as.numeric(category_counts['age']), 
        'Race' = as.numeric(category_counts['race']), 
        'Education' = as.numeric(category_counts['education'])
      ),
      label_row_css = "text-align:left"
    ) %>%
    kable_styling("striped", full_width = F, position = "left")
  
  return(vis) 
} 


#' Closed Number of Discontinued Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type
#'
#' @param analytic This is the analytic data set that must include enrolled, not_expected_reason, not_completed_reason,
#' protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative, sae_count, treatment_arm
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_not_complete_sae_deviation_by_type("Replace with Analytic Tibble")
#' 
closed_not_complete_sae_deviation_by_type <- function(analytic){
  confirm_stability_of_related_visual('not_complete_sae_deviation_by_type', 'f2fda2495bb5637a2181cb670e62622e')
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "not_expected_reason", "not_completed_reason",
                           "protocol_deviation_screen_consent", "protocol_deviation_procedural",
                           "protocol_deviation_administrative", "sae_count", "treatment_arm",
                           "not_completed", "consented"), 
    example_types = c("Boolean", "Category", "Category", "Category", "Category",
                      "Category", "Number", "TreatmentArm", "Boolean", "Boolean"))
  
  n_act <- NA
  n_disc <- NA
  n_dsc <- NA
  n_dp <- NA
  n_da <- NA
  
  df_full <- analytic
  
  df_a <- analytic  %>%
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  

  inner_closed_not_complete_sae_deviation_by_type <- function(analytic, group){
    total <- sum(analytic$enrolled, na.rm=T)
    not_completed_df <- analytic %>% 
      select(enrolled, not_completed_reason, not_completed) %>% 
      mutate(not_completed_reason = ifelse(not_completed, not_completed_reason, NA)) %>% 
      select(-not_completed) %>% 
      filter(enrolled) %>% 
      count(not_completed_reason) %>%
      rename(type=not_completed_reason) %>% 
      filter(!is.na(type)) %>%
      mutate(type = ifelse(type == 'Other', 'Other1', type)) %>% 
      mutate(type = as.character(type)) 
    
    not_completed_df_tot <- tibble(type="Not Active", n=sum(not_completed_df$n))
    
    not_expected_df <- analytic %>% 
      select(enrolled, not_expected_reason) %>% 
      filter(enrolled) %>% 
      count(not_expected_reason) %>%
      rename(type=not_expected_reason) %>% 
      filter(!is.na(type)) %>%
      mutate(type = ifelse(type == 'Other', 'Other2', type)) %>% 
      mutate(type = as.character(type)) 
    
    not_expected_df_tot <- tibble(type="Not Expected", n=sum(not_expected_df$n))
    
    sae_df <- analytic %>% 
      select(study_id, enrolled, sae_count) %>% 
      filter(enrolled & sae_count>0) %>% 
      mutate(sae_count = "SAE") %>% 
      count(sae_count) %>%
      rename(type=sae_count) %>% 
      filter(!is.na(type)) %>%
      mutate(type = ifelse(type == 'Other', 'Other3', type)) %>% 
      mutate(type = as.character(type)) 
    
    if (nrow(sae_df) == 0) {
      sae_df <- tibble(type = "SAE", n = 0)
    }
    
    deviation_sc_df <- analytic %>% 
      select(study_id, consented, protocol_deviation_screen_consent) %>% 
      separate_rows(protocol_deviation_screen_consent, sep=";") %>% 
      filter(consented) %>% 
      count(protocol_deviation_screen_consent) %>%
      rename(type=protocol_deviation_screen_consent) %>% 
      filter(!is.na(type)) %>%
      mutate(type = ifelse(type == 'Other', 'Other4', type)) %>% 
      mutate(type = as.character(type)) 
    
    
    deviation_p_df <- analytic %>% 
      select(study_id, consented, protocol_deviation_procedural) %>% 
      separate_rows(protocol_deviation_procedural, sep=";") %>% 
      filter(consented) %>% 
      count(protocol_deviation_procedural) %>%
      rename(type=protocol_deviation_procedural) %>% 
      filter(!is.na(type)) %>%
      mutate(type = ifelse(type == 'Other', 'Other5', type)) %>% 
      mutate(type = as.character(type)) 
    
    
    deviation_a_df <- analytic %>% 
      select(study_id, consented, protocol_deviation_administrative) %>% 
      separate_rows(protocol_deviation_administrative, sep=";") %>%
      filter(consented) %>% 
      mutate(protocol_deviation_administrative = ifelse(grepl("^Other:", protocol_deviation_administrative), "Other", protocol_deviation_administrative)) %>% 
      count(protocol_deviation_administrative) %>%
      rename(type=protocol_deviation_administrative) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = str_replace(type,"Other: .+","Other")) %>%
      mutate(type = ifelse(type == 'Other', 'Other6', type)) %>% 
      mutate(type = as.character(type))
    
    deviation_sc_tot <- tibble(type="Screen and Consent",n=sum(deviation_sc_df$n))
    deviation_p_tot <- tibble(type="Procedural",n=sum(deviation_p_df$n))
    deviation_a_tot <- tibble(type="Administrative/Other",n=sum(deviation_a_df$n))
    deviation_df_tot <- tibble(type="Protocol Deviations",n=sum(deviation_sc_df$n)+sum(deviation_p_df$n)+sum(deviation_a_df$n))
    
    consented <- sum(analytic$consented, na.rm = TRUE)
    consented_df <- tibble(type = " ", n = paste0(group, " n=", consented, ' <sub>(Consented)</sub>'))
    
    df_final_top <- rbind(not_completed_df_tot, not_completed_df, not_expected_df_tot, not_expected_df, sae_df, consented_df) %>% 
      mutate(n = ifelse(type == " ", n,
                        format_count_percent(n, total, decimals=2)))
    
    df_final_bottom <- rbind(deviation_df_tot, deviation_sc_tot, deviation_sc_df, deviation_p_tot, deviation_p_df, deviation_a_tot, deviation_a_df) %>% 
      mutate(n = ifelse(type == " ", n,
                        format_count_percent(n, consented, decimals=2)))
    
    df_final <- rbind(df_final_top, df_final_bottom)
    
    n_act <<- nrow(not_completed_df)
    n_disc <<- nrow(not_expected_df)
    n_dsc <<- nrow(deviation_sc_df)
    n_dp <<- nrow(deviation_p_df)
    n_da <<- nrow(deviation_a_df)
    
    df_final
  }
  
  table_a <- inner_closed_not_complete_sae_deviation_by_type(df_a, 'Group A')
  table_b <- inner_closed_not_complete_sae_deviation_by_type(df_b, 'Group B')
  table_full <- inner_closed_not_complete_sae_deviation_by_type(df_full, 'Total')
  table_full <- table_full %>% 
    mutate(o = seq(nrow(table_full)))
  
  df_table <- full_join(full_join(table_a, table_b, by='type'), 
                        table_full, by='type') %>% 
    arrange(o) %>% 
    select(-o) %>% 
    mutate_all(replace_na, "0 (0%)") %>%
    mutate(type = if_else(str_detect(type, "^Other"), "Other", type))
  
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
  
  indents_vec <- indents_vec[indents_vec <= nrow(df_table)]
  
  first_indents_vec <- seq(1+n_dsc+1+n_dp+1+n_da) + 1 + n_act + 1 + n_disc + 1 + 1 + 1
  
  if(n_disc>0){
    first_indents_vec <- c(seq(n_disc) + 1 + n_act + 1, first_indents_vec)
  }
  
  if(n_act>0){
    first_indents_vec <- c(seq(n_act) + 1, first_indents_vec)
  }
  
  vis <- kable(df_table, format="html", align='l',  col.names = c(' ', 
                                                                   paste0("Group A n=", nrow(df_a %>% filter(enrolled)), ' <sub>(Enrolled)</sub>'), 
                                                                   paste0("Group B n=", nrow(df_b %>% filter(enrolled)), ' <sub>(Enrolled)</sub>'), 
                                                                   paste0("Total n=", nrow(df_full %>% filter(enrolled)), ' <sub>(Enrolled)</sub>')), escape = FALSE) %>%
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


#' Closed Number of Non-Completing Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type
#' Now with AUTO Protocol Deviation Categorization!
#'
#' @param analytic This is the analytic data set that must include enrolled, not_expected_reason, 
#' not_completed_reason, protocol_deviation_full_data, sae_count
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' closed_not_complete_sae_deviation_by_type_auto_categories()
#' }
closed_not_complete_sae_deviation_by_type_auto_categories <- function(analytic, category_defaults=c("Safety","Informed Consent","Eligibility","Protocol Implementation","Other")){
  
  confirm_stability_of_related_visual('not_complete_sae_deviation_by_type_auto_categories', 'faa854f7497503c9165267d9ea4ca653')
  
  n_act <- NA
  n_disc <- NA
  indents_vec <- NA
  second_vec <- NA
  
  df_full <- analytic

  df_a <- analytic %>%
    filter(treatment_arm =="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm =="Group B")
  
  inner_closed_not_complete_sae_deviation_by_type <- function(inner_analytic, group){
    total <- sum(inner_analytic$enrolled, na.rm=T)
    not_completed_df <- analytic %>% 
      select(enrolled, not_completed_reason, not_completed) %>% 
      mutate(not_completed_reason = ifelse(not_completed, not_completed_reason, NA)) %>% 
      select(-not_completed) %>% 
      filter(enrolled) %>% 
      count(not_completed_reason) %>%
      rename(type=not_completed_reason) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    
    not_completed_df_tot <- tibble(type="Not Completed", n=sum(not_completed_df$n))
    
    not_expected_df <- inner_analytic %>% 
      select(enrolled, not_expected_reason) %>% 
      filter(enrolled) %>% 
      count(not_expected_reason) %>%
      rename(type=not_expected_reason) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    
    not_expected_df_tot <- tibble(type="Not Expected", n=sum(not_expected_df$n))
    
    sae_df <- inner_analytic %>% 
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
    
    inner_analytic <- inner_analytic %>% 
      mutate(protocol_deviation_data = protocol_deviation_full_data)
    
    deviations_df <- inner_analytic %>% 
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
    
    consented <- sum(inner_analytic$consented, na.rm = TRUE)
    consented_df <- tibble(type = " ", n = paste0(group, " n=", consented, ' <sub>(Consented)</sub>'))
    
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
    
    n_act <<- nrow(not_completed_df)
    n_disc <<- nrow(not_expected_df)
    indents_vec <<- indents_vec
    second_vec <<- second_vec
    
    df_final
  }
  
  table_a <- inner_closed_not_complete_sae_deviation_by_type(df_a, 'Group A')
  table_b <- inner_closed_not_complete_sae_deviation_by_type(df_b, 'Group B')
  table_full <- inner_closed_not_complete_sae_deviation_by_type(df_full, 'Total')
  table_full <- table_full %>% 
    mutate(o = seq(nrow(table_full)))
  
  df_table <- full_join(full_join(table_a, table_b, by='type'), 
                        table_full, by='type') %>% 
    arrange(o) %>% 
    select(-o) %>% 
    mutate_all(replace_na, "0 (0%)") %>%
    mutate(type = if_else(str_detect(type, "^Other"), "Other", type))
  
  if(is_empty(second_vec)){
    vis <- kable(df_table, format="html", align='l',  col.names = c(' ', 
                                                                    paste0("Group A n=", nrow(df_a %>% filter(enrolled)), ' <sub>(Enrolled)</sub>'), 
                                                                    paste0("Group B n=", nrow(df_b %>% filter(enrolled)), ' <sub>(Enrolled)</sub>'), 
                                                                    paste0("Total n=", nrow(df_full %>% filter(enrolled)), ' <sub>(Enrolled)</sub>')), escape = FALSE) %>%
      add_indent(indents_vec) %>% 
      row_spec(0, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1 + 1, extra_css = "border-bottom: 1px solid; font-weight: bold") %>% 
      row_spec(nrow(df_final), extra_css = "border-bottom: 1px solid") %>%
      kable_styling("striped", full_width = F, position = "left")
  } else{
    vis <- kable(df_table, format="html", align='l',  col.names = c(' ', 
                                                                    paste0("Group A n=", nrow(df_a %>% filter(enrolled)), ' <sub>(Enrolled)</sub>'), 
                                                                    paste0("Group B n=", nrow(df_b %>% filter(enrolled)), ' <sub>(Enrolled)</sub>'), 
                                                                    paste0("Total n=", nrow(df_full %>% filter(enrolled)), ' <sub>(Enrolled)</sub>')), escape = FALSE) %>%
      add_indent(indents_vec) %>% 
      add_indent(second_vec) %>%
      row_spec(0, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1 + 1, extra_css = "border-bottom: 1px solid; font-weight: bold") %>% 
      row_spec(nrow(df_table), extra_css = "border-bottom: 1px solid") %>%
      kable_styling("striped", full_width = F, position = "left")
  }
  return(vis)
}


#' Closed Complications by severity and relatedness
#'
#' @description This function visualizes the complications by severity and relatedness for dsmb report
#'
#' @param analytic This is the analytic data set that must include study_id, complication_data, treatment_arm
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_complications_by_severity_relatedness("Replace with Analytic Tibble")
#' 
closed_complications_by_severity_relatedness <- function(analytic){
  analytic <- if_needed_generate_example_data(analytic,
                                              example_constructs = c("complication_data", "treatment_arm"),
                                              example_types = c("(';new_row: ', '|')FollowupPeriod|Form|Category|Category|Character|Date|Category|Category|Character", "TreatmentArm"))
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-23)
  
  
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
      mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
      mutate(across(where(is.character), ~ replace_na(., ""))) %>% 
      group_by(severity, complications) %>% 
      summarise(Definitely_id = length(unique(study_id[Definitely > 0])) , Possibly_id = length(unique(study_id[Possibly > 0])) , 
                Probably_id = length(unique(study_id[Probably > 0])) , Unlikely_id = length(unique(study_id[Unlikely > 0])) , 
                Unrelated_id = length(unique(study_id[Unrelated > 0])) , Unknown_id = length(unique(study_id[Unknown > 0])) , 
                Total_id = length(unique(study_id))) %>% 
      ungroup()
    
    
    id_sums <- sapply(total_ids[, c("Definitely_id", "Possibly_id", "Probably_id", "Unlikely_id", "Unrelated_id", "Unknown_id")], sum)
    
    summary_id_sums <- data.frame(t(id_sums)) 
    
    
    output_complication <- full_join(total_complications, total_ids) %>% 
      mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
      mutate(across(where(is.character), ~ replace_na(., ""))) %>% 
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
    level_order <- c("Superficial", "Deep - Involving Bone", "Deep - Not Involving Bone", "Deep- Septic Joint",
                     "Nonunion", "Malunion", "Flap Failure", "Loss of Limb/Amputation", "Fixation failure", "Peri-implant Fracture",
                     "Reaction to Hardware", "Wound Dehiscence", "Wound Seroma/Hematoma", "Tendon Injury", 
                     "Delayed Wound Healing", "Cellulitis", "DVT/PE", "Joint Arthritis", "Other")
    
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
    
    output
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
  
  index_vec <- c(" " = 1, "Grade 4" = 19, "Grade 3"= 19,"Grade 2,1"= 19, "Grade Unknown"= 19)
  subindex_vec <- c(" " = 1, "Infection" = 4, " " = 15, "Infection" = 4, " " = 15, "Infection" = 4, " " = 15,
                    "Infection" = 4, " " = 15)
  table_raw<- kable(output, format="html", align='l') %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    pack_rows(index = subindex_vec,label_row_css = "text-align:left;padding-left: 2em;", bold = FALSE) %>% 
    add_header_above(c(" " = 1, "Group A" = 7, "Group B" = 7, "All" = 7)) %>%
    row_spec(1, extra_css = "border-bottom: 2px solid") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(table_raw)
}




#' Appendix A: Listing of Serious Adverse Events(SAEs) for closed report
#'
#' @description This function visualizes appendix for all the serious adverse events noted for the patients
#'
#' @param analytic This is the analytic data set that must include study_id, sae_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_appendix_A_SAEs("Replace with Analytic Tibble")
#' 
closed_appendix_A_SAEs <- function(analytic){
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
  analytic <- if_needed_generate_example_data(
    analytic,
     example_constructs = "sae_data",
     example_types = "(';new_row: ', '|')FacilityCode|TreatmentArm|Boolean|Date|Date|Number|Category|Category|Category|Category") 
  
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
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_appendix_B_deaths("Replace with Analytic Tibble")
#' 
closed_appendix_B_deaths <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("sae_data", "death_date"),
    example_types = c("(';new_row: ', '|')FacilityCode|TreatmentArm|Boolean|Date|Date|Number|Category|Category|Category|Character",
                      "Date")) 
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
  df <- analytic %>% 
    select(study_id, sae_data, death_date) 
  
  unzipped_death <- df %>%
    separate_rows(sae_data, sep = ";new_row: ") %>% 
    separate(sae_data, into = c("facilitycode","treatment_arm", "treatment_received", "consent_date", "sae_dt_event", "age", "sae_relatedness_injury", 
                                "sae_relatedness_treatment", "sae_outcome", "sae_describe"), sep='\\|') %>% 
    filter(sae_outcome == "Death" | !is.na(death_date))
  
  if (nrow(unzipped_death) == 0) {
    return(paste0("<br />\nNone at this time.<br />\n"))
  }
  
  output_df <- unzipped_death %>% 
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
  
  output_text <- output_df$text %>% 
    paste(collapse = "<br />\n")
  
  return(output_text)
}

#' Appendix C: Listing of any Not Expected and Not completed cases for closed report
#'
#' @description This function visualizes any not completedness and not expectedness occurred during the study time period.
#'
#' @param analytic This is the analytic data set that must include study_id, not_expected_data, not_completed_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_appendix_C_not_expected_not_completed("Replace with Analytic Tibble")
#' 
closed_appendix_C_not_expected_not_completed <- function(analytic){
  analytic <- if_needed_generate_example_data(analytic,
                                              example_constructs = c("not_expected_data", "not_completed_data"),
                                              example_types = c("FacilityCode;TreatmentArm;Date;Date;Number;Category",
                                                                "FacilityCode;TreatmentArm;Date;Date;Number;Category"))
 
    #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
  df <- analytic %>% 
    select(study_id, not_expected_data, not_completed_data) 
  
  unzipped_not_expected_data <- df %>%
    select(study_id, not_expected_data) %>% 
    filter(!is.na(not_expected_data)) %>% 
    separate(not_expected_data, into = c("facilitycode","treatment_arm", "consent_date", "not_expected_date", "age", 
                                         "not_expected_reason"), sep='\\|') %>% 
    rename(not_expected_completed_date = not_expected_date,
           not_expected_completed_reason = not_expected_reason)
  
  unzipped_not_completed_data <- df %>%
    select(study_id, not_completed_data) %>% 
    filter(!is.na(not_completed_data)) %>% 
    separate(not_completed_data, into = c("facilitycode","treatment_arm", "consent_date", "not_completed_date", "age", 
                                          "not_completed_reason"), sep='\\|') %>% 
    rename(not_expected_completed_date = not_completed_date,
           not_expected_completed_reason = not_completed_reason)
  
  unzipped_not_expected_not_completed <- rbind(unzipped_not_expected_data, unzipped_not_completed_data)
  
  output_df <- unzipped_not_expected_not_completed %>% 
    mutate(text = paste0(
      "<b>Participant ID</b>: ", study_id, "-", facilitycode, "<br /> ",
      "<b>Date Enrolled</b>: ", consent_date, "<br /> ",
      "<b>Tx Group</b>: ", treatment_arm, "<br /> ",
      "<b>Date discontinued</b>: ", not_expected_completed_date, "<br /> ",
      "<b>Age</b>: ", age, "<br /> ",
      "<b>Reason for discontinuation</b>: ", not_expected_completed_reason, "<br /> ",
      "<br />")) 
  
  if (nrow(unzipped_not_expected_not_completed) == 0) {
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
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_appendix_D_protocol_deviation("Replace with Analytic Tibble")
#' 
closed_appendix_D_protocol_deviation <- function(analytic){
  analytic <- if_needed_generate_example_data(analytic,
                                              example_constructs = "protocol_deviation_data",
                                              example_types = "(';new_row: ', '|')FacilityCode|Date|Date|Category|Character") 
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
  if("protocol_deviation_data" %in% colnames(analytic) & !"protocol_deviation_full_data" %in% colnames(analytic)){
    df <- analytic %>% 
      select(study_id, protocol_deviation_data) %>% 
      filter(!is.na(protocol_deviation_data))
    
    unzipped_protocol_deviation <- df %>% 
      separate_rows(protocol_deviation_data, sep = ";new_row: ") %>% 
      separate(protocol_deviation_data, into = c("facilitycode", "consent_date", "deviation_date", "protocol_deviation", 
                                                 "deviation_description"), sep='\\|')
  } else{
    df <- analytic %>% 
      mutate(protocol_deviation_data = protocol_deviation_full_data) %>%
      select(study_id, protocol_deviation_data) %>% 
      filter(!is.na(protocol_deviation_data))
    
    unzipped_protocol_deviation <- df %>% 
      separate_rows(protocol_deviation_data, sep = ";new_row: ") %>% 
      separate(protocol_deviation_data, into = c("facilitycode", "consent_date", "category", "deviation_date", "protocol_deviation", 
                                                 "deviation_description"), sep='\\|') %>% 
      select(-category)
  }

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
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_ih_and_dc_crossover_monitoring_by_site("Replace with Analytic Tibble")
#' 
closed_ih_and_dc_crossover_monitoring_by_site <- function(analytic, footnotes = NULL){
  confirm_stability_of_related_visual('ih_and_dc_crossover_monitoring_by_site', 'c028f0d077842c22c8c6b3bf61baecd2')
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', 'enrolled', 'df_surg_completed', 'ih_discharge_date', 
                           'crossover_inpatient', 'crossover_discharge', 'ih_discharge_date_on_time_zero', 
                           'treatment_arm'), 
    example_types = c('FacilityCode', 'Boolean', 'Boolean', 'Date', 'Boolean', "Boolean", 'Boolean', 
                      'TreatmentArm'))
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4>Group A</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site(df_a),
                  "<h4>Group B</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site(df_b))
  } else{
    out <- paste0("<h4>Group A</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site(df_a) %>% add_footnote(footnotes, notation="number", escape = FALSE),
                  "<h4>Group B</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site(df_b) %>% add_footnote(footnotes, notation="number", escape = FALSE))
  }
  
  return(out)
}




#' Closed Status of IRB Approvals and Certification by Site
#'
#' @description This function returns a list of sites and their dates of
#' local, DOD, and METRC certifications by treatment arm
#'
#' @param analytic This is the analytic data set that must include site_certified_date
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_certification_date_data("Replace with Analytic Tibble")
#' 
closed_certification_date_data <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('treatment_arm', "site_certified_date"), 
    example_types = c('TreatmentArm', 'FacilityCode;Date;Date;Boolean;Number'))
  
  date_today <- Sys.Date()
  
  cols <- c('Facility', 'Local (or sIRB) Approval Date', 'DoD Approval Date',
            'Certified by MCC to Start Screening',
            paste0('Number of Days Certified (as of ', as.character(date_today), ')'))
  
  df <- analytic %>%
    filter(!is.na(treatment_arm)) %>%
    group_by(treatment_arm) %>%
    mutate(site_certified_date = na_if(site_certified_date, "NA")) %>%
    separate(site_certified_date, cols, sep = ';') %>%
    select(treatment_arm, Facility, `Local (or sIRB) Approval Date`, `DoD Approval Date`,
           `Certified by MCC to Start Screening`, `Days Number of Days Certified (as of Thursday, March 07, 2024)`) %>%
    filter(!is.na(Facility))
  
  df_a <- df %>% filter(treatment_arm == "Group A") %>% select(-treatment_arm)
  df_b <- df %>% filter(treatment_arm == "Group B") %>% select(-treatment_arm)
  
  vis <- bind_rows(df_a, df_b) %>%
    kable(align='l') %>%
    add_header_above(c(" " = 1, format="html", "Group A" = 4, "Group B" = 4)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(vis)
}

#' Closed Injury Characteristics
#'
#' @description This function visualizes the certain injury characteristics for study participants study injuries by treatment arm
#'
#' @param analytic This is the analytic data set that must include enrolled, treatment_arm, injury_classification_ankle_ao, injury_at_work, injury_in_battle,
#' injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type

#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_injury_characteristics_by_alternate_constructs("Replace with Analytic Tibble")
#' 
closed_injury_characteristics_by_alternate_constructs <- function(analytic){
  analytic <- if_needed_generate_example_data(analytic, 
                                              example_constructs = c('enrolled', 'treatment_arm', 'injury_classification_ankle_ao', 'injury_at_work', 'injury_in_battle', 
                                                                     'injury_in_blast', 'injury_date', 'injury_mechanism', 'injury_side', 'injury_classification_tscherne', 'injury_type'), 
                                              example_types = c('Boolean', 'TreatmentArm', 'Category', 'Boolean', 'Boolean', 
                                                                "NamedCategory['Yes' 'No' 'Missing']", 'Date', 'Category', "NamedCategory['Left' 'Right' 'Missing']", 'Category', "NamedCategory['Blunt' 'Penetrating' 'Missing']"))
  
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
    
    df_final
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
  
  df_table <- cbind(df_final_a, df_final_b, df_final_full) %>%
    select(type...1, percentage...2, percentage...4, percentage...6) %>% 
    rename(type = type...1,
           "percentage (Group A)" = percentage...2, 
           "percentage (Group B)" = percentage...4,
           "percentage" = percentage...6)
  
  cnames <- c(' ', paste('Group A (n=', sum(df_a$enrolled), ')'),
              paste('Group B (n=', sum(df_b$enrolled), ')'),
              paste('Overall (n=', total, ')'))
  header <- c(1,1)
  names(header) <- cnames
  
  vis <- kable(df_table, format="html", align='l',  col.names = cnames) %>%
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


#' Closed Amputations and Gustilo Injury Characteristics
#'
#' @description This function visualizes the injury characteristics for amputations and Gustilo Injury types for
#' Sextant study by treatment arm
#'
#' @param analytic This is the analytic data set that must include enrolled, treatment_arm,
#' injury_gustilo_type, injury_amputation_status
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' closed_amputations_and_gustilo_injury_characteristics("Replace with Analytic Tibble")
#' 
closed_amputations_and_gustilo_injury_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "treatment_arm", "injury_gustilo_type", "injury_amputation_status"),
    example_types = c("Boolean", "TreatmentArm", "Category-U7", "Category-U3")) 
  
  confirm_stability_of_related_visual("amputations_and_gustilo_injury_characteristics", "2af7b3cdbbcc705b3f642265d3222776")
  
  inner_amputations_and_gustilo_injury_characteristics <- function(pull) {
    inj_gust <- pull %>%
      select(injury_gustilo_type) %>%  
      mutate(injury_gustilo_type = gsub('"|“|”', '', injury_gustilo_type)) %>%
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
    
    combined
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
  
  output <- kable(df_table, format="html", align='l',  col.names = c(" ", "Group A", "Group B", "Overall")) %>%
    kable_styling("striped", position = "left", full_width = F) %>%
    add_indent(positions = c(2,3,4,6,7,8,9,10,11,12)) %>%
    row_spec(c(1,5), bold=T,hline_after = T)
  
  return(output)
}



#' Closed enrollment_by_site tobra and sextant (var discontinued)
#'
#' @description This function visualizes the number of subjects enrolled, not enrolled etc, with specs for last 14 days and average by week by treatment arm
#'
#' @param analytic This is the analytic data set that must include screened, eligible, refused, consented, not_consented, not_randomized, consented_and_randomized, enrolled, site_certified_days, 
#' facilitycode, screened_date, randomized, consent_date, discontinued, treatment_arm, 
#' @param days the number of last days to include in the last days summary section of the table
#' @param discontinued this is a meta construct where you can specify your discontinued construct like 'discontinued' or 'adjudicated_discontinued' (defaults to 'discontinued')
#' @param discontinued_colname this determines the label applied to the discontinued column of your choosing (defaults to 'Discontinued')
#' @param include_exclusive_safety_set this is a toggle that will include a exclusive_safety_set construct if you want it included (defaults to FALSE)
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble")
#' closed_enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", include_exclusive_safety_set = FALSE, footnotes = TRUE)
#' closed_enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", include_exclusive_safety_set = TRUE)
#' 
closed_enrollment_by_site_last_days_var_disc <- function(analytic, days=0, discontinued="discontinued", discontinued_colname="Discontinued", include_exclusive_safety_set=FALSE, footnotes=NULL){
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "refused", "consented", "enrolled", "randomized",
                           "not_consented", "site_certified_days", "facilitycode", "consent_date",
                           "not_randomized", "discontinued", "treatment_arm", "consented_and_randomized", "screened_date"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Date", "FacilityCode", "Date", "Boolean", "Boolean", "TreatmentArm", 
                      "Boolean", "Date"))
  
  #NOTE: USES OPEN VERSION IN A STACKED FORMAT, AUTOMATICALLY SYNCED (2024-11-14)
  
  df_a <- analytic %>% 
    filter(is.na(treatment_arm)|treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(is.na(treatment_arm)|treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4> </h4><br /><h4>Group A</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_a, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_exclusive_safety_set=include_exclusive_safety_set),
                  "<h4>Group B</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_b, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_exclusive_safety_set=include_exclusive_safety_set))
  } else{
    out <- paste0("<h4> </h4><br /><h4>Group A</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_a, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_exclusive_safety_set=include_exclusive_safety_set) %>% add_footnote(footnotes, notation="number", escape = FALSE),
                  "<h4>Group B</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_b, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_exclusive_safety_set=include_exclusive_safety_set) %>% add_footnote(footnotes, notation="number", escape = FALSE))
  }

  return(out)
}



#' Closed Deep Surgical Site Infection reported & adjudicated
#'
#' @description 
#' This function visualizes the treatment crossover or any nonadherence occured during the Tobra
#' study by treatment arm.
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, followup_expected_6mo, 
#' dssi_reported_6mo, dssi_adjudicated_6mo, dssi_adjudication_pending_6mo, dssi_count
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_dssi_reported_adjudicated("Replace with Analytic Tibble")
#' 
closed_dssi_reported_adjudicated <- function(analytic, footnotes = NULL){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "followup_expected_6mo", "dssi_reported_6mo",
                           "dssi_adjudicated_6mo", "dssi_adjudication_pending_6mo",
                           "treatment_arm", "dssi_count"),
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "TreatmentArm", "Number-U5")) 
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
  inner_dssi_reported_adjudicated <- function(pull) {
    
  df <- pull %>% 
    select(study_id, enrolled, followup_expected_6mo, dssi_reported_6mo, dssi_adjudicated_6mo, 
           dssi_adjudication_pending_6mo, dssi_count)
  
  total_ids_dssi <- df %>% 
    filter(dssi_reported_6mo & enrolled) %>% 
    distinct(study_id) %>% 
    nrow() 
  
  df_1 <- df %>%
    filter(enrolled) %>%
    select(-enrolled, -study_id) %>%
    summarize(
      `Number of participants with visits expected at 6 months` = paste(sum(followup_expected_6mo, na.rm = TRUE)), 
      `Deep surgical site infections reported within 6 months (# of patients with infections)` = paste0(sum(dssi_count, na.rm = TRUE), " (", total_ids_dssi, ")"),
      `Deep surgical site infection adjudicated at 6 months` = paste(sum(dssi_adjudicated_6mo, na.rm = TRUE)),
      `Participant with infections yet to be adjudicated within 6 months from time zero` = paste(sum(dssi_adjudication_pending_6mo, na.rm = TRUE))) %>% 
    pivot_longer(everything()) %>% 
    rename(" " = name,
           "Total" = value)
  
  df_1
  }
  
  pull <- analytic %>%
    filter(enrolled) %>%
    select(study_id, enrolled, followup_expected_6mo, dssi_reported_6mo, dssi_adjudicated_6mo, 
           dssi_adjudication_pending_6mo, treatment_arm, dssi_count)
  
  pull_a <- pull %>% filter(treatment_arm == "Group A")
  pull_b <- pull %>% filter(treatment_arm == "Group B")
  
  combined_a <- inner_dssi_reported_adjudicated(pull_a)
  combined_b <- inner_dssi_reported_adjudicated(pull_b)
  
  combined_full <- inner_dssi_reported_adjudicated(pull)
  
  df_table <- full_join(combined_a, combined_b, by = " ", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(combined_full, by = " ") %>%
    select(" ", ends_with(" (Group A)"), ends_with(" (Group B)"), `Total`)
  
  output <- kable(df_table, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}



#' Closed complications reported overall from time_zero
#'
#' @description This function visualizes the number of complications(number of subjects) reported at all time points(Overall) 
#' from time_zero overall, and by each treatment arm. 
#'
#' @param analytic This is the analytic data set that must include study_id, complication_data, treatment_arm
#' @param days it is a keyword argument to pass in the number of days to get cut off date for complications
#' 
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_complications_overall("Replace with Analytic Tibble")
#' 
closed_complications_overall <- function(analytic, min_days=NULL, cutoff_days = NULL){
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
 inner_complications_overall <- function(df){ 
   analytic <- if_needed_generate_example_data(analytic,
                                               example_constructs = c("complication_data", 'time_zero', 'treatment_arm'),
                                               example_types = c("(';new_row: ', '|')FollowupPeriod|Form|Category|Category|Character|Date|Category|Category|Character", 'Date', 'TreatmentArm'))  
   
  df <- df %>%  
    select(study_id, complication_data, time_zero) %>% 
    filter(!is.na(complication_data))
 
  unzipped_complication <- df %>%
    separate_rows(complication_data, sep = ";new_row: ") %>% 
    separate(complication_data, into = c('redcap_event_name', 'form_name', 'event_type', 'complication',
                                         'notes', 'diagnosis_date', 'severity', 'treatment', 'other_info'), 
             sep='\\|') 
  
 
  if(is.null(min_days)){
    df_pre_filter <- unzipped_complication 
  } else {
    df_pre_filter <- unzipped_complication %>% 
      mutate(cut_off = as.Date(time_zero) + min_days) %>% 
      filter(diagnosis_date >= cut_off)
  }
  
  
  if(is.null(cutoff_days)){
    df_pre_filter <- df_pre_filter 
  } else {
    df_pre_filter <- df_pre_filter %>% 
      mutate(cut_off = as.Date(time_zero) + cutoff_days) %>% 
      filter(diagnosis_date < cut_off)
  }
  
  df_1 <-  df_pre_filter %>% 
    mutate(severity = case_when(
      severity %in% c('Mild', 'Moderate') ~ "Grade 2,1",
      severity == "Severe and Undesirable" ~ "Grade 3",
      severity == "Life-threatening or disabling" ~ "Grade 4",
      severity == "Fatal" ~ "Unknown",
      TRUE ~ NA_character_
    )) %>% 
    select(study_id, complication, severity) %>% 
    group_by(study_id, severity,complication) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    mutate(complication = recode(complication, 
                                 "other1" = "Other",  
                                 "other2" = "Other",
                                 "other3" = "Other",
                                 "other4" = "Other")) 
  
  
  unique_study_df <- df_1%>% 
    mutate(severity = ifelse(is.na(severity), "Unknown", severity)) %>% 
    group_by(complication, severity) %>%
    summarize(
      unique_ids = n_distinct(study_id)  
    ) %>%
    ungroup()
  
  new_df <- df_1 %>% select(-study_id) %>% 
    mutate(severity = ifelse(is.na(severity), "Unknown", severity)) %>% 
    group_by(complication, severity) %>% 
    summarize(Total = sum(Total, na.rm = TRUE))
  
  
  combined_df <- left_join(new_df, unique_study_df) %>% 
    mutate(Total = paste0(Total, "[", unique_ids, "]")) %>% 
    select(-unique_ids) %>% 
    mutate(severity = ifelse(is.na(severity), "Unknown", severity))
  
  
  severity_categories <- c('Grade 4', 'Grade 3', 'Grade 2,1', 'Unknown')
  level_order <- c("Deep Surgical Site Infection", "Cellulitis/Skin Infection", "Pin tract infection - treated with antibiotics and/or pin removal", 
                   "Nonunion", "Malunion", "Flap Failure", "Loss of limb / amputation", "Fixation failure", 
                   "Peri-implant Fracture", "Wound dehiscence", "Wound Seroma/Hematoma", "Tendon Rupture", 
                   "Symptomatic Hardware", "Reaction to Vancomycin", "Reaction to Tobramycin", "Renal Insufficiency", "Other")
  
  
  df_template <- tibble(
    severity = c(severity_categories),
  ) %>% group_by(severity) %>% 
    reframe(complication = level_order)
  
  output_complication <- left_join(df_template, combined_df) 
  
  df_table_raw <- reorder_rows(output_complication, list('severity'=c("Grade 4", "Grade 3", "Grade 2,1", "Unknown")))
  df_table_raw
  
 }
 
 df_a <- analytic %>% 
   filter(treatment_arm == 'Group A')
 
 df_b <- analytic %>% 
   filter(treatment_arm == 'Group B')
 
 analytic_final <- inner_complications_overall(analytic)
 a_final <- inner_complications_overall(df_a)
 b_final <- inner_complications_overall(df_b)
 
 colnames(b_final) <- c('sevb', 'compb', 'countb')
 colnames(analytic_final) <- c('seva', 'compa', 'counta')
 
 df_table_raw <- cbind(a_final, b_final, analytic_final) %>%
   select(severity, complication, Total, countb, counta)
 
    grade_4_all_na <- all(is.na(df_table_raw$Total[df_table_raw$severity == "Grade 4"]))
    
    if (grade_4_all_na) {
      df_table_raw <- df_table_raw %>%
        filter(severity != "Grade 4") %>%
        bind_rows(data.frame(complication = "None", severity = "Grade 4", Total = NA))
      
      df_table_raw <- reorder_rows(df_table_raw, list('severity'=c("Grade 4", "Grade 3", "Grade 2,1", "Unknown")))
      
      df_final <- df_table_raw %>% select(-severity) %>%  mutate_all(replace_na, "-")
      
      colnames(df_final) <- c('Complication', 'Group A', 'Group B', 'Overall')
      
      index_vec <- c("Grade 4" = 1, "Grade 3"= 17,"Grade 2,1"= 17, "Unknown"= 17)
      subindex_vec <- c(" "= 1, "Infections" = 2, "Other Complications" = 15, "Infections" = 2, 
                        "Other Complications" = 15,"Infections" = 2, "Other Complications" = 15)
      
      table_raw <- kable(df_final, format="html", align='l') %>%
        pack_rows(index = index_vec, label_row_css = "text-align:left") %>%
        pack_rows(index = subindex_vec, label_row_css = "text-align:left", bold = FALSE) %>%
        kable_styling("striped", full_width = F, position='left') 
      
    } else {
      
      df_table_raw <- reorder_rows(df_table_raw, list('severity'=c("Grade 4", "Grade 3", "Grade 2,1", "Unknown")))
      
      df_final <- df_table_raw %>% select(-severity) %>%  mutate_all(replace_na, "-")
      
      colnames(df_final) <- c('Complication', 'Group A', 'Group B', 'Overall')
      
      index_vec <- c("Grade 4" = 17, "Grade 3"= 17,"Grade 2,1"= 17, "Unknown"= 17)
      subindex_vec <- c("Infections" = 2, "Other Complications " = 15, "Infections" = 2, "Other Complications" = 15, "Infections" = 2, 
                        "Other Complications" = 15,"Infections" = 2, "Other Complications" = 15)
      
      table_raw <- kable(df_final, format="html", align='l') %>%
        pack_rows(index = index_vec, label_row_css = "text-align:left") %>%
        pack_rows(index = subindex_vec, label_row_css = "text-align:left", bold = FALSE) %>%
        kable_styling("striped", full_width = F, position='left') %>%
        row_spec(c(0,5,8,13,18,23,36,41), extra_css = "border-bottom: 1px solid;")
    }
  
  return(table_raw)
}



#' Closed adherence_sextant
#'
#' @description This function visualizes the treatment characteristics per protocol and assignmnet for Sextant for
#' each treatment group
#'
#' @param analytic This is the analytic data set that must include adherence_to_intervention_dwc,
#' adherence_to_intervention_post_dwc, adherence_to_no_other_antibiotic_dwc, treatment_arm
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_adherence_sextant("Replace with Analytic Tibble")
#' 
closed_adherence_sextant <- function(analytic, footnotes=NULL){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("treatment_arm", "adherence_to_intervention_dwc",
                           "adherence_to_intervention_post_dwc", "adherence_to_no_other_antibiotic_dwc", 
                           "dwc_date", "enrolled"),
    example_types = c("TreatmentArm", "Boolean", "Boolean", "Boolean", 
                      "Date", "Boolean")) 
  
  confirm_stability_of_related_visual('adherence_sextant', 'dffdf49f31e7c00274374f2103141409')
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4>Group A</h4><br />",
                  adherence_sextant(df_a),
                  "<h4>Group B</h4><br />",
                  adherence_sextant(df_b))
  } else{
    out <- paste0("<h4>Group A</h4><br />",
                  adherence_sextant(df_a) %>% add_footnote(footnotes, notation="number", escape = FALSE),
                  "<h4>Group B</h4><br />",
                  adherence_sextant(df_b) %>% add_footnote(footnotes, notation="number", escape = FALSE))
  }
  
  return(out)
}


#' closed characteristics_treatment
#'
#' @description This function visualizes the treatment characteristics per protocol and assignment for tobra. 
#'
#' @param analytic This is the analytic data set that must study_id, enrolled, df_date, plat_df_surgical_incision, 
#' pil_df_surgical_incision, df_number_procedures, adherence_to_intervention, treatment_arm
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_characteristics_treatment("Replace with Analytic Tibble")
#' 
closed_characteristics_treatment <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("treatment_arm", "enrolled", "df_date", "plat_df_surgical_incision",
                           "pil_df_surgical_incision", "df_number_procedures", "adherence_to_intervention"),
    example_types = c("TreatmentArm", "Boolean", "Date", "Category",
                      "Category", "Number", "Boolean")) 
  
  confirm_stability_of_related_visual('characteristics_treatment', '560adce14e9d8085cd5c991e701bd780')
  
  inner_characteristics_treatment <- function(df){
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
      mutate(adherence_to_intervention = as.character(adherence_to_intervention)) %>%
      mutate(adherence_to_intervention = replace_na(adherence_to_intervention, 'Missing')) %>% 
      mutate(type = recode(adherence_to_intervention, 'TRUE' = "Yes", 
                           'FALSE' = 'No')) %>% 
      count(type) %>% 
      rename(number = n) %>% 
      mutate(percentage = format_count_percent(number, df_total)) %>% 
      select(-number) %>% 
      arrange(factor(type, levels = c('Yes', 'No', 'Missing')))
    
    df_final <- rbind(df_complete, avg_stages, stages, plat_incisions, pil_incisions, adherence)
    
    df_final
  }
  
  df_all <- analytic %>% 
    select(study_id, enrolled, df_date, plat_df_surgical_incision, pil_df_surgical_incision, df_number_procedures, adherence_to_intervention, treatment_arm) %>% 
    filter(enrolled)
  
  df_a <- df_all %>% filter(treatment_arm == 'Group A')
  df_b <- df_all %>% filter(treatment_arm == 'Group B')
  
  all <- inner_characteristics_treatment(df_all) %>% 
    mutate(pall = percentage)
  a <- inner_characteristics_treatment(df_a) %>% 
    mutate(pa = percentage)
  b <- inner_characteristics_treatment(df_b) %>% 
    mutate(pb = percentage)
  
  total <- sum(df_all$enrolled, na.rm = TRUE)
  atot <- sum(df_a$enrolled)
  btot <- sum(df_b$enrolled)
  
  df_table <- cbind(a, b, all) %>% 
    select('type', 'pa', 'pb', 'pall')
  
  n <- nrow(df_table)
  
  cnames <- c(' ', paste0('Group A (n = ', atot, ')'), paste0('Group A (n = ', btot, ')'), paste0('Overall (n = ', total, ')'))
  header <- c(1,1,1,1)
  names(header)<-cnames
  
  vis <- kable(df_table, format="html", align='l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c(" " = 1, 'Definitive Fixation' = (n-6), 'Number of Incisions [Mean (SD)]' = 2,
                        'Study Treatment Adhering to Protocol' = (3)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1, extra_css = 'border-bottom: 1px solid') %>% 
    add_indent(seq(2) + 2)
  
  return(vis)
}

#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled (Variable Discontinued)
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, site_certified_days, facilitycode, treatment_arm
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_enrollment_status_by_site_var_discontinued("Replace with Analytic Tibble")
#' 
closed_enrollment_status_by_site_var_discontinued <- function(analytic, discontinued="discontinued", discontinued_colname="Discontinued",only_total=FALSE, footnotes = NULL){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened", "eligible", "refused", "not_consented", "consented", 
                           "not_randomized", "randomized", "enrolled", "site_certified_days", 
                           "facilitycode", "discontinued", 'treatment_arm'),
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Date", 
                      "FacilityCode", "Boolean", "TreatmentArm")) 
  
  #NOTE: USES OPEN VERSION IN A STACKED FORMAT, AUTOMATICALLY SYNCED (2024-05-23)
  
  df_a <- analytic %>% 
    filter(is.na(treatment_arm)|treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(is.na(treatment_arm)|treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4> </h4><br /><h4>Group A</h4><br />",
                  enrollment_status_by_site_var_discontinued(df_a, discontinued=discontinued, discontinued_colname=discontinued_colname, only_total=only_total),
                  "<h4>Group B</h4><br />",
                  enrollment_status_by_site_var_discontinued(df_b, discontinued=discontinued, discontinued_colname=discontinued_colname, only_total=only_total))
  } else{
    out <- paste0("<h4> </h4><br /><h4>Group A</h4><br />",
                  enrollment_status_by_site_var_discontinued(df_a, discontinued=discontinued, discontinued_colname=discontinued_colname) %>% add_footnote(footnotes, notation="number", escape = FALSE),
                  "<h4>Group B</h4><br />",
                  enrollment_status_by_site_var_discontinued(df_b, discontinued=discontinued, discontinued_colname=discontinued_colname) %>% add_footnote(footnotes, notation="number", escape = FALSE))
  }
  
  return(out)
}


#' Crossover Monitoring by Site since 01/01/2024
#'
#' @description This function visualizes the crossovers by site in hospital and at discharge 
#' after 01/01/2021
#'
#' @param analytic This is the analytic data set that must include enrolled, df_surg_completed, 
#' ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero, facilitycode, treatment_arm
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_ih_and_dc_crossover_monitoring_by_site_cutoff_date("Replace with Analytic Tibble")
#' 
closed_ih_and_dc_crossover_monitoring_by_site_cutoff_date <- function(analytic, footnotes = NULL){
  #NOTE: USES OPEN VERSION IN A STACKED FORMAT, AUTOMATICALLY SYNCED (2024-05-23)
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "df_surg_completed", "ih_discharge_date", 
                           "crossover_inpatient", "crossover_discharge", "ih_discharge_date_on_time_zero",
                           "facilitycode", "treatment_arm"), 
    example_types = c("Boolean", "Boolean", "Date", 
                      "Boolean", "Boolean", "Boolean",
                      "FacilityCode", "TreatmentArm"))
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4> </h4><br /><h4>Group A</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site_cutoff_date(df_a),
                  "<h4>Group B</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site_cutoff_date(df_b))
  } else{
    out <- paste0("<h4> </h4><br /><h4>Group A</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site_cutoff_date(df_a) %>% add_footnote(footnotes, notation="number", escape = FALSE),
                  "<h4>Group B</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site_cutoff_date(df_b) %>% add_footnote(footnotes, notation="number", escape = FALSE))
  }
  
  return(out)
}


#' Expected visit status for Overall Followup
#'
#' @description This function only looks at the designated overall form(s) for a 
#' given study, as designated in the respective followup_data long file 
#' and organizes them by its detected levels of followup periods. 
#' 
#' The function separates the data for the two study groups by treatment arm
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data, treatment_arm
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_expected_and_followup_visit_overall("Replace with Analytic Tibble")
#' 
closed_expected_and_followup_visit_overall <- function(analytic, footnotes = NULL){
  
  analytic <- if_needed_generate_example_data(analytic,
                                              example_constructs = c("followup_data","treatment_arm"),
                                              example_types = c("(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date","Boolean")) 
  
  confirm_stability_of_related_visual('expected_and_followup_visit_overall', '819920a6d38957636bee47fb326150f0')
  
  pull <- analytic %>% 
    select(study_id, followup_data, treatment_arm) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate(status = as.character(status)) %>% 
    mutate(across(where(is.character), ~ na_if(.x, "NA")))
  
  df_a <- pull %>%
    filter(treatment_arm=='Group A')
  df_b <- pull %>%
    filter(treatment_arm=='Group B')
  
  fu_levels <- pull$followup_period %>% unique()
  fu_levels <- fu_levels[!is.na(fu_levels)]
  

  split_arm <- list('Group A' = c(), 'Group B' = c())
  
  arm_statuses <- function(df) {
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
    
    df_empty <- data.frame('status' = c("Not Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'))
    
    final_raw <- left_join(df_empty, combined, by = 'status') %>% 
      mutate(across(everything(), ~replace_na(., 0)))
    
    summed_statuses <- c("Complete", "Incomplete", "Missed", "Not Started")
    
    expected_row <- final_raw %>%
      filter(status %in% summed_statuses) %>%
      summarize(across(-status, \(x) sum(x, na.rm = TRUE))) %>%
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
    
    final_last
  }
  
  a_statuses <- arm_statuses(df_a)
  b_statuses <- arm_statuses(df_b)
  
  combined_statuses <- a_statuses %>%
    rename_with(~paste0("", .x), -Status) %>%
    inner_join(b_statuses %>%
                 rename_with(~paste0("", .x), -Status),
               by = "Status")
  
  a_cols <- colnames(a_statuses)
  b_cols <- colnames(b_statuses)
  
  colnames(combined_statuses) <- c('Status', a_cols[2:length(a_cols)],
                                   b_cols[2:length(b_cols)])
  
  
  vis <- kable(combined_statuses, format = "html", align = 'l') %>%
    add_indent(c(4, 5)) %>%
    add_header_above(c(' ', 'Group A' = length(a_cols)-1, 'Group B' = length(b_cols)-1)) %>%
    kable_styling("striped", full_width = F, position = 'left')
  
  if (!is.null(footnote)) {
    vis <- add_footnote(vis, footnotes)
  }
  
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
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_fracture_characteristics("Replace with Analytic Tibble")
#' 
closed_fracture_characteristics <- function(analytic){
  confirm_stability_of_related_visual('fracture_characteristics', 'fe024116685f39115df0add9c2ad061b')
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "fracture_type", "injury_gustilo", "injury_classification_tscherne",
                           "treatment_arm"), 
    example_types = c("Boolean", "NamedCategory['Tibial Plateau' 'Tibial Pilon' 'Tibial Shaft' 'Fibula' 'Unknown']", 
                      "NamedCategory['I' 'II' 'IIIA']", "NamedCategory['C0' 'CI' 'CII' 'CIII']",
                      "TreatmentArm"))
  
  inner_fracture_characteristics <- function(df) {
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
    
    df_final
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
  
  df_table <- full_join(df_final_full, df_final_a, by = "type") %>%
    left_join(df_final_b, by = "type") %>%
    mutate(percentage.y = ifelse(is.na(percentage.y), '0 (0%)', percentage.y)) %>% 
    mutate(percentage = ifelse(is.na(percentage), '0 (0%)', percentage.y)) %>% 
    select(type, percentage.y, percentage, percentage.x)
  
  cnames <- c(' ', paste0('Group A (n=', sum(df_a$enrolled), ')'),
              paste0('Group B (n=', sum(df_b$enrolled), ')'),
              paste0('Overall (n=', total, ')'))
  header <- c(1,1,1,1)
  names(header) <- cnames
  
  n_closed <- nrow(df_table %>% filter(str_detect(type, "Closed Fracture")))
  n_open <- nrow(df_table %>% filter(str_detect(type, "Open Fracture")))
  n_frac <- nrow(df_table %>% filter(str_detect(type, "Tibial")|str_detect(type, 'Unknown') | str_detect(type, 'Fibula')))
  n_tscherne <- nrow(df_table %>% filter(str_detect(type, "Tscherne")))
  n_gustilo <- nrow(df_table %>% filter(str_detect(type, "Type")))
  
  vis <- kable(df_table, format="html", align='l',  col.names = cnames) %>%
    pack_rows(index = c('Fractured Bone' = n_frac,
                        'Fracture Type' = (n_closed + n_tscherne + n_open + n_gustilo)),
              label_row_css = "text-align:left") %>%
    add_indent(c(seq(n_tscherne) + n_frac + n_closed, seq(n_gustilo) + n_frac + n_closed + n_open + n_tscherne)) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(vis)
}



#' Closed Followup Data Single Form and Timepoint By Site
#'
#' @description Returns the designated followup form status across all sites, 
#' for a single timepoint and separated by the treatment_arm variable
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data, treatment_arm
#' @param timepoint the point in time to be considered in the visualization
#' @param form_selection the form to be considered in the visualization
#' @param name optional argument for changing the name of the followup form, for aesthetic use
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_followup_form_at_timepoint_by_site("Replace with Analytic Tibble")
#' 
closed_followup_form_at_timepoint_by_site <- function(analytic, timepoint, form_selection, name = NULL){
  confirm_stability_of_related_visual('followup_form_at_timepoint_by_site', '878c411fde80fea78add23f5defe4c14')
  analytic <- if_needed_generate_example_data(analytic, 
                                              example_constructs = c('facilitycode', "followup_data", "treatment_arm"), 
                                              example_types = c('FacilityCode', "(';new_row: ', '|')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date", 'TreatmentArm'))
  
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "followup_data", "treatment_arm"), 
    example_types = c("FacilityCode", "(';', ',')Period|Period|Form|Status|Date",
                      "TreatmentArm"))
  
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data, treatment_arm) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  df_a <- df %>% filter(treatment_arm == 'Group A') %>% select(-treatment_arm)
  df_b <- df %>% filter(treatment_arm == 'Group B') %>% select(-treatment_arm)
  df <- df %>% select(-treatment_arm)
  
  inner_per_treatment_arm <- function(df) {
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
    
    form_df
  }
  
  df_a_collected <- inner_per_treatment_arm(df_a)
  df_b_collected <- inner_per_treatment_arm(df_b)
  total_collected <- inner_per_treatment_arm(df)
  
  full_collected <- rbind(df_a_collected, df_b_collected, total_collected)
  
  header <- c(1,8)
  names(header) <- c(' ', ifelse(is.null(name),
                                 paste0(form_selection, ' Status at ', timepoint, ' Period'),
                                 paste0(name, ' Status at ', timepoint, ' Period')))
  
  
  vis <- kable(full_collected, format="html", align='l') %>%
    add_header_above(header) %>%
    pack_rows(index = c('Group A' = nrow(df_a_collected),
                        'Group B' = nrow(df_b_collected),
                        'Total' = nrow(total_collected)),
              label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Closed Followup Data Single Form All Timepoints By Site
#'
#' @description Returns the designated followup form status by site, for all timepoints,
#' separated by treatment_arm
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data, treatment_arm
#' @param form_selection the form to be considered in the visualization
#' @param included_columns the statuses you want data for. Defaults to all statuses.
#' @param footnotes optional argument for changing the names of the followup forms, for aesthetic use
#'
#' @export
#'
#' @examples
#' closed_followup_form_all_timepoints_by_site("Replace with Analytic Tibble")
#' 
closed_followup_form_all_timepoints_by_site <- function(analytic, form_selection = 'Overall', 
                                                        included_columns=c("Not Expected", "Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'),
                                                        footnotes = NULL){
  confirm_stability_of_related_visual('followup_form_all_timepoints_by_site', '76462363d142eebdbcf60b62e806b082')
  analytic <- if_needed_generate_example_data(analytic, 
                                          example_constructs = c('facilitycode', "followup_data", "treatment_arm"), 
                                          example_types = c('FacilityCode', "(';new_row: ', '|')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date", 'TreatmentArm'))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data, treatment_arm) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  df_a <- df %>% filter(treatment_arm == 'Group A') %>% select(-treatment_arm)
  df_b <- df %>% filter(treatment_arm == 'Group B') %>% select(-treatment_arm)
  df <- df %>% select(-treatment_arm)
  
  timepoints <- df %>% filter(form==form_selection) %>% pull(followup_period) %>% unique()
  timepoints <- timepoints[!is.na(timepoints)]
  
  inner_per_treatment_arm <- function(df) {
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
      
      out <- rbind(not_expected_row, expected_row, top, middle, bottom) %>%         rename(Status = status) %>%
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
      filter(!is.na(Facility)&Facility!='NA')
    colnames(form_df) <- c('Facility', rep(c("Not Expected", "Expected", "Complete", "Early", "Late", 'Missed', 
                                             'Not Started', 'Incomplete'), times = length(timepoints)))
    form_df
  }
  
  df_a_collected <- inner_per_treatment_arm(df_a)
  df_b_collected <- inner_per_treatment_arm(df_b)
  total_collected <- inner_per_treatment_arm(df)
  
  full_collected <- rbind(df_a_collected, df_b_collected, total_collected)
  
  header <- c(1,rep(8, length(timepoints)))
  names(header) <- c(' ', timepoints)
  
  over_header <- c(1, 8*length(timepoints))
  names(over_header) <- c(' ', paste(form_selection, 'Form Status'))
  
  
  vis <- kable(full_collected, format="html", align='l') %>%
    add_header_above(header) %>%
    add_header_above(over_header) %>%
    pack_rows(index = c('Group A' = nrow(df_a_collected),
                        'Group B' = nrow(df_b_collected),
                        'Total' = nrow(total_collected)),
              label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position='left')
  
  if (!is.null(footnotes)) {
    vis <- add_footnote(vis, footnotes)
  }
  
  return(vis)
}


#' Closed Followup Data Multiple Forms and Single Timepoint By Site
#'
#' @description Returns the designated followup form status by site
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data
#' @param timepoint the point in time to be considered in the visualization
#' @param forms the form to be considered in the visualization
#' @param names This is the analytic data set that must include study_id, followup_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' closex_followup_forms_at_timepoint_by_site()
#' }
closed_followup_forms_at_timepoint_by_site <- function(analytic, timepoint, forms, pretty_names = NULL){
  confirm_stability_of_related_visual('followup_forms_at_timepoint_by_site', '98f0d266c706fb6be2a647cffe144564')
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data, treatment_arm) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  df_a <- df %>% filter(treatment_arm == 'Group A') %>% select(-treatment_arm)
  df_b <- df %>% filter(treatment_arm == 'Group B') %>% select(-treatment_arm)
  df <- df %>% select(-treatment_arm)
  
  inner_per_treatment_arm <- function(df){  
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
    output
  }
  
  df_a_collected <- inner_per_treatment_arm(df_a)
  df_b_collected <- inner_per_treatment_arm(df_b)
  total_collected <- inner_per_treatment_arm(df)
  
  full_collected <- rbind(df_a_collected, df_b_collected, total_collected)
  
  cols <- c('Facility', rep(c("Not Expected", "Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 
                              'Incomplete'), times = length(forms)))
  colnames(full_collected) <- cols
  
  header <- c(1,rep(8, length(forms)))
  if (is.null(pretty_names)) {
    header_names <- c(' ', paste0(forms, ' Status at ', timepoint, ' Period'))
  } else {
    header_names <- c(' ', paste0(pretty_names, ' Status at ', timepoint, ' Period'))
  }
  
  names(header) <- header_names
  
  vis <- kable(full_collected, format="html", align='l') %>%
    add_header_above(header) %>%
    pack_rows(index = c('Group A' = nrow(df_a_collected),
                        'Group B' = nrow(df_b_collected),
                        'Total' = nrow(total_collected))) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Closed Followup Data Multiple Forms and All Timepoints
#'
#' @description Returns the designated followup forms status at a specified range
#' of timepoints, not specified by site, separated vertically by treatment_arm
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data, treatment_arm
#' @param forms followup forms to output, as found in the followup_data construct
#' @param timepoints timepoints to output, as found in the followup_data construct
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_followup_forms_all_timepoints("Replace with Analytic Tibble")
#' 
closed_followup_forms_all_timepoints <- function(analytic, forms = NULL, timepoints = NULL, vertical = TRUE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "followup_data", "treatment_arm"), 
    example_types = c('FacilityCode', "(';new_row: ', '|')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date", "TreatmentArm"))
  
  confirm_stability_of_related_visual('followup_forms_all_timepoints', '1418b0b014673d1bd0dcc99f80bedee1')
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data, treatment_arm) %>% 
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
  
  df_a <- df %>% filter(treatment_arm == 'Group A') %>% select(-treatment_arm)
  df_b <- df %>% filter(treatment_arm == 'Group B') %>% select(-treatment_arm)
  df_all <- df %>% select(-treatment_arm)
  
  per_form <- function(form_name, in_df) {
    
    form_df <- in_df %>%
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
      
      result_list[[i]] <- result
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
      slice_tail(n=2) %>% 
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
    
    final_last <- rbind(expected_row, top, middle, bottom) %>% 
      rename(Status = status)
    
    final_last
  }
  
  per_group <- function(df) {
    out <- NULL
    for (form_name in forms) {
      if (is.null(out)) {
        out <- per_form(form_name, df)
      } else {
        out <- full_join(out, per_form(form_name, df), by = 'Status')
      }
    }
    out
  }
  
  df_a_collected <- per_group(df_a)
  df_b_collected <- per_group(df_b)
  total_collected <- per_group(df_all)
  
  full_collected <- rbind(df_a_collected, df_b_collected, total_collected)
  
  colnames(full_collected) <- c('Status', rep(timepoints, times = length(forms)))
  header <- c(1, rep(length(timepoints), times = length(forms)))
  names(header) <- c(' ', paste(forms, 'Form Status'))
  
  if (vertical) {
    out_long <- NULL
    i <- 2
    for (package in names(header[-1])) {
      colcount <- as.numeric(header[package]) - 1
      colindex <- i + colcount
      temp_df <- full_collected[i:colindex]
      if (is.null(out_long)) {
        out_long <- temp_df
      } else {
        out_long <- bind_rows(out_long, temp_df)
      }
      i <- colindex + 1
    }
    
    out_long <- out_long %>%
      mutate(across(everything(), ~replace(., is.na(.), "."))) %>%
      mutate(Status = rep(c("Expected", "Not Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'), 
                          length(forms)*3)) %>%
      select(Status, everything())
    
    
    unpacked_vis <- kable(out_long, format="html", align='l')  %>%
      kable_styling("striped", full_width = F, position='left')
    
    i <- 1
    for (package in names(header[-1])) {
      vis <- unpacked_vis %>%
        pack_rows(package, i, i + 23) %>%
        pack_rows('Group A', i, i + 7) %>%
        pack_rows('Group B', i + 8, i + 15) %>%
        pack_rows('Total', i + 16, i + 23) %>%
        add_indent(c(i+3, i+4, i+12, i+13, i+20, i+21))
      i <- i + 24
    }
  } else if (!vertical) {
    vis <- kable(full_collected, format="html", align='l') %>%
      add_indent(c(4,5,12,13,20,21)) %>%
      add_header_above(header) %>%
      pack_rows(index = c('Group A' = nrow(df_a_collected),
                          'Group B' = nrow(df_b_collected),
                          'Total' = nrow(total_collected))) %>%
      kable_styling("striped", full_width = F, position='left')
  }
  return(vis)
}


#' Closed Generic Characteristics
#'
#' @description 
#' Runs basic count statistics for a number of constructs. Missing values are 
#' given the value "Missing."
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
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' generic_characteristics("Replace with Analytic Tibble", constructs="stages", names_vec="Stages")
#' }
closed_generic_characteristics <- function(analytic, constructs = c(), names_vec = c(), 
                                    filter_cols = c("enrolled"), titlecase = FALSE, splits=NULL,
                                    subcategory_constructs = c()){
  
  confirm_stability_of_related_visual('generic_characteristics', 'eaf3c24dd6f7b5b48c12c2348f56c9d2')
  
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
          select(study_id, all_of(c(constructs, subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]), treatment_arm)
      } else {
        inner_analytic <- analytic %>%
          filter(!!sym(filter_cols[i])) %>%
          select(study_id, all_of(c(constructs, subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]), treatment_arm)
      }
    }
    total <- nrow(inner_analytic)
    a_total <- nrow(inner_analytic %>% filter(treatment_arm=="Group A"))
    b_total <- nrow(inner_analytic %>% filter(treatment_arm=="Group B"))
    
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
          group_by(temp, treatment_arm) %>% 
          count(temp)
        
        category_df_all <- inner %>% 
          filter(sub_temp==sub_cat) %>% 
          select(-sub_temp) %>% 
          group_by(temp) %>% 
          count(temp) %>% 
          mutate(Total = format_count_percent(n, sum(category_df$n))) %>% 
          mutate(header = name_str) %>%
          arrange(temp == "Missing")
        
        category_tot <- sum(category_df_all$n)
        category_tot_a <- sum(category_df %>% filter(treatment_arm=="Group A") %>% pull(n))
        category_tot_b <- sum(category_df %>% filter(treatment_arm=="Group B") %>% pull(n))
        tot_df <- tibble(temp=sub_cat, header=name_str,
                         "Group A"=format_count_percent(category_tot_a, total),
                         "Group B"=format_count_percent(category_tot_b, total),
                         Total=format_count_percent(category_tot, total))
        
        category_df <- category_df   %>% 
          mutate(percentage = 
                   ifelse(treatment_arm == 'Group A', 
                          format_count_percent(n,  category_tot_a),
                          ifelse(treatment_arm == 'Group B', format_count_percent(n,  category_tot_b), NA)
                          )
                   ) %>% 
          select(-n) %>%
          mutate(header = name_str) %>%
          arrange(temp == "Missing") %>%
          pivot_wider(
            names_from = treatment_arm,
            values_from = percentage,
            values_fill = "0 (0%)"
          )
        
        category_df <- full_join(category_df_all %>% select(-n), category_df)
        
        if(!"Group A" %in% colnames(category_df)){
          category_df <- category_df %>% 
            mutate("Group A" = "0 (0%)")
        }
        if(!"Group B" %in% colnames(category_df)){
          category_df <- category_df %>% 
            mutate("Group B" = "0 (0%)")
        }
        
        category_df <- category_df %>%
          select(temp, header, `Group A`, `Group B`, Total)
        
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
      names(new) <- paste0(name_str, ' (Group A=',a_total,', Group B=',b_total,', n=', total, ')')
      index_vec <- c(index_vec, new)
    } else{
      inner_some <- inner %>% 
        group_by(temp, treatment_arm) %>% 
        count(temp) %>% 
        mutate(percentage = format_count_percent(n, total)) %>% 
        select(-n) %>%
        mutate(header = name_str) %>%
        arrange(temp == "Missing") %>%
        pivot_wider(
          names_from = treatment_arm,
          values_from = percentage,
          values_fill = "0 (0%)"
        )%>%
        select(temp, header, `Group A`, `Group B`)
      
      
      inner_all <- inner %>% 
        group_by(temp) %>% 
        count(temp) %>% 
        mutate(Total = format_count_percent(n, total)) %>% 
        select(-n) %>%
        mutate(header = name_str) %>%
        arrange(temp == "Missing")
      
      inner <- full_join(inner_all, inner_some)%>%
        select(temp, header, `Group A`, `Group B`, Total)
      
      
      if (titlecase) {
        inner <- inner %>%
          mutate(temp = str_to_title(temp))
      }
      
      new <- nrow(inner)
      names(new) <- paste0(name_str, ' (Group A=',a_total,', Group B=',b_total,', n=', total, ')')
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
    vis <- kable(out, format="html", align='l', col.names = c(" ", "Group A", "Group B", "Total")) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      row_spec(c(1, cumsum(index_vec[1: length(index_vec)-1])+1), extra_css = "border-top: 1px solid") %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
      kable_styling("striped", full_width = F, position="left")
  }else{
    vis <- kable(out, format="html", align='l', col.names = c(" ", "Group A", "Group B", "Total")) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      add_indent(sub_index_vec) %>% 
      row_spec(sub_bold_index_vec, bold = TRUE) %>% 
      row_spec(c(1, cumsum(index_vec[1: length(index_vec)-1])+1), extra_css = "border-top: 1px solid") %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
      kable_styling("striped", full_width = F, position="left")
  }
  
  return(vis)
}

#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description This function visualizes the enrollment totals for each site
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, site_certified_days, 
#' facilitycode, late_ineligible
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' closed_enrollment_status_by_site("Replace with Analytic Tibble")
#' 
closed_enrollment_status_by_site <- function(analytic){
  #NOTE: USES OPEN VERSION IN A STACKED FORMAT, AUTOMATICALLY SYNCED (2024-05-23)
  analytic <- if_needed_generate_example_data(analytic,
                                              example_constructs = c('screened', 'eligible', 'refused', 'consented', 'enrolled', 'not_consented', 'discontinued_pre_randomization', 'site_certified_days', 
                                                                     'facilitycode', 'late_ineligible', 'treatment_arm'),
                                              example_types = c('Boolean', 'Boolean', "Boolean", "Boolean", 'Boolean', 'Boolean', 'Boolean', 'Number', 'FacilityCode', 'Boolean', 'TreatmentArm'))
  
  df_a <- analytic %>% 
    filter(is.na(treatment_arm)|treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(is.na(treatment_arm)|treatment_arm=="Group B")
  
    out <- paste0("<h4> </h4><br /><h4>Group A</h4><br />",
                  enrollment_status_by_site(df_a),
                  "<h4>Group B</h4><br />",
                  enrollment_status_by_site(df_b))
  
  return(out)
}

