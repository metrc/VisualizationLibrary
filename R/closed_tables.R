

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
   
  confirm_stability_of_related_visual('injury_ankle_plateau_characteristics', 'f58c138ba4639aad9d8cb71bd6bc9d3d')
  
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
  
  confirm_stability_of_related_visual("baseline_characteristics_percent", "7aadae1abf473348fab0c8b12f86228e")
  
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
  
  colnames(full_output) <- c(" ", paste0("Group A (n=",nrow(df_a),")"), paste0("Group B (n=",nrow(df_b),")"), paste0("Total (n=",nrow(df_a)+nrow(df_b),")"))
  
  vis <- kable(full_output, format="html", align='l') %>%
    pack_rows(index = c('Sex' = nrow(sex_df), 'Age' = (nrow(age_df) + nrow(age_group_df)), 'Race' = nrow(race_df), 
                        'Education' = nrow(education_df), 'Military' = nrow(military_df)), label_row_css = "text-align:left") %>% 
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
  confirm_stability_of_related_visual('discontinuation_sae_deviation_by_type', 'd1533c6c962841734b6dd39995a01297')
  
  n_disc <- NA
  n_dsc <- NA
  n_dp <- NA
  n_da <- NA
  
  df_full <- analytic %>%
    filter(enrolled)
  
  df_a <- analytic  %>%
    filter(enrolled) %>%  
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(enrolled) %>%  
    filter(treatment_arm=="Group B")
  

  inner_closed_discontinuation_sae_deviation_by_type <- function(analytic){
    total <- sum(analytic$enrolled, na.rm=T)
    discontinuation_df <- analytic %>% 
      select(enrolled, censored_reason) %>% 
      filter(enrolled == TRUE) %>% 
      count(censored_reason) %>%
      rename(type=censored_reason) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type)) %>% 
      mutate(group="A")
    
    discontinuation_df_tot <- tibble(type="Discontinuations", n=sum(discontinuation_df$n))
    
    sae_df <- analytic %>% 
      select(study_id, enrolled, sae_count) %>% 
      filter(enrolled & sae_count>0) %>% 
      mutate(sae_count = "SAE") %>% 
      count(sae_count) %>%
      rename(type=sae_count) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type)) %>% 
      mutate(group="B")
    
    
    deviation_sc_df <- analytic %>% 
      select(study_id, enrolled, protocol_deviation_screen_consent) %>% 
      separate_rows(protocol_deviation_screen_consent, sep=";") %>% 
      filter(enrolled == TRUE) %>% 
      count(protocol_deviation_screen_consent) %>%
      rename(type=protocol_deviation_screen_consent) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type)) %>% 
      mutate(group="C")
    
    
    deviation_p_df <- analytic %>% 
      select(study_id, enrolled, protocol_deviation_procedural) %>% 
      separate_rows(protocol_deviation_procedural, sep=";") %>% 
      filter(enrolled == TRUE) %>% 
      count(protocol_deviation_procedural) %>%
      rename(type=protocol_deviation_procedural) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type)) %>% 
      mutate(group="D")
    
    
    deviation_a_df <- analytic %>% 
      select(study_id, enrolled, protocol_deviation_administrative) %>% 
      separate_rows(protocol_deviation_administrative, sep=";") %>%
      filter(enrolled == TRUE) %>% 
      mutate(protocol_deviation_administrative = ifelse(grepl("^Other:", protocol_deviation_administrative), "Other", protocol_deviation_administrative)) %>% 
      count(protocol_deviation_administrative) %>%
      rename(type=protocol_deviation_administrative) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = str_replace(type,"Other: .+","Other")) %>% 
      mutate(type = as.character(type)) %>% 
      mutate(group="E")
    
    deviation_sc_tot <- tibble(type="Screen and Consent",n=sum(deviation_sc_df$n))
    deviation_p_tot <- tibble(type="Procedural",n=sum(deviation_p_df$n))
    deviation_a_tot <- tibble(type="Administrative/Other",n=sum(deviation_a_df$n))
    deviation_df_tot <- tibble(type="Protocol Deviations",n=sum(deviation_sc_df$n)+sum(deviation_p_df$n)+sum(deviation_a_df$n))
    
    
    df_final <- bind_rows(discontinuation_df_tot, discontinuation_df, sae_df, deviation_df_tot, 
                          deviation_sc_tot, deviation_sc_df, deviation_p_tot, deviation_p_df, deviation_a_tot, deviation_a_df) %>% 
      mutate(n = format_count_percent(n, total, decimals=2))
    
    n_disc <<- nrow(discontinuation_df)
    n_dsc <<- nrow(deviation_sc_df)
    n_dp <<- nrow(deviation_p_df)
    n_da <<- nrow(deviation_a_df)
    
    df_final
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

  vis <- kable(df_table, format="html", align='l',  col.names = c(' ', 
                                                                   paste0("Group A n=(", nrow(df_a), ")"), 
                                                                   paste0("Group B n=(", nrow(df_b), ")"), 
                                                                   paste0("Total n=(", nrow(df_full), ")"))) %>%
    add_indent(c(seq(n_disc) + 1, 1 + n_disc + 1 + 1 + seq(1+n_dsc+1+n_dp+1+n_da))) %>% 
    add_indent(indents_vec) %>% 
    row_spec(0, extra_css = "border-bottom: 2px solid") %>% 
    row_spec(1+ n_disc, extra_css = "border-bottom: 2px solid") %>% 
    row_spec(1 + n_disc + 1, extra_css = "border-bottom: 2px solid") %>%
    row_spec(1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 2px solid") %>%
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
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_appendix_A_SAEs()
#' }
closed_appendix_A_SAEs <- function(analytic){
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
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
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
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
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
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
closed_ih_and_dc_crossover_monitoring_by_site <- function(analytic, footnotes = NULL){
  confirm_stability_of_related_visual('ih_and_dc_crossover_monitoring_by_site', '1a7b5d1f609833623e6a3a48a3bcc5c2')
  
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



#' Closed Nonunion surgery outcome
#'
#' @description This function visualizes the Nonunion surgery outcome
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' followup_expected_3mo, followup_expected_12mo, nonunion_90days,  nonunion_1yr, treatment_arm
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
    select(enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_expected_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_expected_12mo)) %>% 
    mutate(Treatment = "Total") %>% 
    select(Treatment, enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A") %>% 
    select(enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_expected_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_expected_12mo)) %>% 
    mutate(Treatment = "Group A") %>% 
    select(Treatment, enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr)
    
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B") %>% 
    select(enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_expected_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_expected_12mo)) %>% 
    mutate(Treatment = "Group B") %>% 
    select(Treatment, enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr)
  
  df <- rbind(df, df_a, df_b)
  
  colname <- c("Treatment", "Enrolled", "Expected Three Month", "90 Day Non-Union", "Expected Twelve Month", "1 Year Non-Union")
  
  table<- kable(df, format="html", align='l',  col.names = colname) %>%
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
    out
    
  }
  
  table_a <- closed_ao_gustillo_tscherne_injury_characteristics(df_a)
  table_b <- closed_ao_gustillo_tscherne_injury_characteristics(df_b)
  table_full <- closed_ao_gustillo_tscherne_injury_characteristics(df)
  
  df_table <- full_join(full_join(table_a, table_b, by="Fracture Type"), 
                        table_full, by="Fracture Type") %>% 
    mutate("Fracture Type" = str_replace(`Fracture Type`,"Unknown [0-9]", "Unknown"))
  
  output<- kable(df_table, format="html", align='l',  col.names = c("Fracture Type", "Group A", "Group B", "Total")) %>%
    kable_styling("condensed", position="left") %>%
    pack_rows("Open Fracture", 1, nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("Closed Fracture", nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch), label_row_css = "text-align:left") %>%
    pack_rows("AO Class", nrow(inj_gust)+nrow(inj_tsch)+1, nrow(inj_gust)+nrow(inj_tsch)+nrow(inj_ao), label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
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
    
    table_raw
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
  
  table <- kable(df_table, format="html", align='l') %>%
    add_header_above(c(" " = 2, "Group A" = 8, "Group B" = 8, "Overall" = 8)) %>%
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
    
    table_raw_ankle<- kable(df_ankle, format="html", align='l',  col.names = str_replace(colnames(df_ankle),"^n.|^n"," ")) %>%
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
    
    table_raw_plateau<- kable(df_plateau, format="html", align='l',  col.names = str_replace(colnames(df_plateau),"^n.|^n"," ")) %>%
      pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
      kable_styling("striped", full_width = F, position='left')
    
    list(table_raw_ankle, table_raw_plateau)
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
    
    list(n_disc, n_dsc, n_dp, n_da, df_final)
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
  
  vis <- kable(df_table, format="html", align='l',  col.names = cnames) %>%
    row_spec(header_names, bold=T) %>%
    add_indent(c(seq(n_disc_full) + 1, 1 + n_disc_full + 1 + 1 + seq(1 + n_dsc_full + 1 + n_dp_full + 1 + n_da_full)),
               indents = c(1,0), level_of_indent = "  ") %>%
    add_indent(indents_vec_a, indents = 1, level_of_indent = "    ", columns = 2) %>% 
    add_indent(indents_vec_b, indents = 1, level_of_indent = " ", columns = 3) %>% 
    add_indent(indents_vec_full, indents = 1, level_of_indent = " ", columns = 4) %>% 
    row_spec(0, extra_css = "border-bottom: 2px solid") %>% 
    row_spec(1 + n_disc_full, extra_css = "border-bottom: 2px solid") %>% 
    row_spec(1 + n_disc_full + 1, extra_css = "border-bottom: 2px solid") %>% 
    row_spec(1 + n_disc_full + 1 + 1 + 1 + n_dsc_full + 1 + n_dp_full + 1 + n_da_full, extra_css = "border-bottom: 2px solid") %>% 
    kable_styling("striped", full_width = F, position = "left")
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
            paste0('Number of Days Certified (as of ', as.character(date_today), ')'))
  
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
  
  df_table <- full_join(df_final_a, df_final_b, by = "type", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(df_final_full, by = "type") %>%
    select(type, ends_with(" (Group A)"), ends_with(" (Group B)"), ends_with("percentage"))
  
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
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_amputations_and_gustilo_injury_characteristics(analytic)
#' }
closed_amputations_and_gustilo_injury_characteristics <- function(analytic){
  
  confirm_stability_of_related_visual("amputations_and_gustilo_injury_characteristics", "11da8d8c3b7ef63d4bca5ce8c8351c42")
  
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




#' Closed Adherence by site
#'
#' @description This function visualizes the treatment crossover or any nonadherence occured during the Tobra
#' study by treatment arm.
#'
#' @param analytic This is the analytic data set that must include facilitycode, df_date, treatment_arm, treatment_arm
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_adherence_by_site(analytic)
#' }
closed_adherence_by_site <- function(analytic){
  #confirm_stability_of_related_visual('fracture_characteristics', '39fae01eb7c76ab2d779c0400b13472c')
  
  inner_adherence_by_site <- function(inner_df) {
    treatment_total <- inner_df %>%
      filter(!is.na(treatment_arm)) %>%
      nrow()
    
    df2 <- inner_df %>% 
      group_by(facilitycode) %>% 
      summarize(elig_enr = sum(enrolled, na.rm = TRUE), 
                df_total = sum(df_complete, na.rm = TRUE), 
                # TODO ask about this math, does treatment_arm
                treatment_completed = sum(!is.na(treatment_arm))) %>% 
      mutate(treatment_completed = format_count_percent(df_total, treatment_completed)) 
    
    enrolled_total <- sum(df2$elig_enr, na.rm = TRUE)
    df_total <- sum(df2$df_total, na.rm = TRUE)
    
    df3 <- data.frame(facilitycode = 'TOTAL', 
                      elig_enr = enrolled_total, 
                      df_total = df_total, 
                      treatment_completed = format_count_percent(df_total, sum(!is.na(inner_df$treatment_arm))))
    
    df4 <- rbind(df2, df3) %>% 
      rename(`Clinical Site` = facilitycode,
             `Eligible and Enrolled` = elig_enr,
             `Definitive Wound Closure completed` = df_total,
             `Received treatment per protocol and assignment(% DWC complete)` = treatment_completed)
    
    df4
  }
  
  df <- analytic %>%
    select(facilitycode, enrolled, df_date, treatment_arm) %>%
    filter(enrolled) %>%
    mutate(df_date = na_if(df_date, "NA")) %>%
    mutate(df_complete = ifelse(!is.na(df_date), TRUE, FALSE))
  
  df_a <- df %>% filter(treatment_arm == "Group A")
  df_b <- df %>% filter(treatment_arm == "Group B")
  
  df4_a <- inner_adherence_by_site(df_a)
  df4_b <- inner_adherence_by_site(df_b)
  
  df_table <- full_join(df4_a, df4_b, by = "Clinical Site", suffix = c(" (Group A)", " (Group B)")) %>%
    select(`Clinical Site`, ends_with(" (Group A)"), ends_with(" (Group B)"))
  
  total_row <- df_table %>%
    filter(`Clinical Site` == "TOTAL")
  df_table <- df_table %>%
    filter(`Clinical Site` != "TOTAL")
  df_table <- bind_rows(df_table, total_row)
  
  output <- kable(df_table, format="html", align='l') %>%
    row_spec(nrow(df_table), bold = TRUE) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(output)
}

#' Closed enrollment_by_site tobra and sextant (var discontinued)
#'
#' @description This function visualizes the number of subjects enrolled, not enrolled etc, with specs for last 14 days and average by week by treatment arm
#'
#' @param analytic This is the analytic data set that must include screened, eligible, refused, not_consented, not_randomized, consented_and_randomized, enrolled, site_certified_days, 
#' facilitycode, screened_date
#' @param days the number of last days to include in the last days summary section of the table
#' @param discontinued this is a meta construct where you can specify your discontinued construct like 'discontinued' or 'adjudicated_discontinued' (defaults to 'discontinued')
#' @param discontinued_colname this determines the label applied to the discontinued column of your choosing (defaults to 'Discontinued')
#' @param include_safety_set this is a toggle that will include a safety_set construct if you want it included (defaults to FALSE)
#'
#' @return html table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_enrollment_by_site_last_days_var_disc()
#' }
closed_enrollment_by_site_last_days_var_disc <- function(analytic, days, discontinued="discontinued", discontinued_colname="Discontinued", include_safety_set=FALSE, footnotes=NULL){
  confirm_stability_of_related_visual('enrollment_by_site_last_days_var_disc', 'f3bb72ca1a3ca21695900292fc6d0e2b')
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4>Group A</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_a, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_safety_set=include_safety_set),
                  "<h4>Group B</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_b, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_safety_set=include_safety_set))
  } else{
    out <- paste0("<h4>Group A</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_a, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_safety_set=include_safety_set) %>% add_footnote(footnotes, notation="number", escape = FALSE),
                  "<h4>Group B</h4><br />",
                  enrollment_by_site_last_days_var_disc(df_b, days, discontinued=discontinued, discontinued_colname=discontinued_colname, include_safety_set=include_safety_set) %>% add_footnote(footnotes, notation="number", escape = FALSE))
  }

  return(out)
}



#' Closed Deep Surgical Site Infection reported & adjudicated
#'
#' @description This function visualizes the treatment crossover or any nonadherence occured during the Tobra
#' study by treatment arm.
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, followup_expected_6mo, dssi_reported_6mo, dssi_adjudicated_6mo, 
#' dssi_adjudication_pending_6mo
#'
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_dssi_reported_adjudicated(analytic)
#' }
closed_dssi_reported_adjudicated <- function(analytic, footnotes = NULL){
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
  
  inner_dssi_reported_adjudicated <- function(pull) {
    
  df <- pull %>% 
    select(study_id, enrolled, followup_expected_6mo, dssi_reported_6mo, dssi_adjudicated_6mo, 
           dssi_adjudication_pending_6mo)
  
  total_ids_dssi <- df %>% 
    filter(dssi_reported_6mo & enrolled) %>% 
    distinct(study_id) %>% 
    nrow() 
  
  df_1 <- df %>%
    filter(enrolled) %>%
    select(-enrolled, -study_id) %>%
    summarize(
      `Number of participants with visits expected at 6 months` = paste(sum(followup_expected_6mo, na.rm = TRUE)), 
      `Deep surgical site infections reported within 6 months` = paste0(sum(dssi_reported_6mo, na.rm = TRUE), " (", total_ids_dssi, ")"),
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
           dssi_adjudication_pending_6mo, treatment_arm)
  
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
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_complications_overall(analytic)
#' }
closed_complications_overall <- function(analytic, min_days=NULL, cutoff_days = NULL){
  
  #NOTE: NO OPEN VERSION STABILITY CONFIRMATION NOT APPLICABLE (2024-05-22)
 inner_complications_overall <- function(df){ 
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
    group_by(complication, severity) %>%
    summarize(
      unique_ids = n_distinct(study_id)  
    ) %>%
    ungroup()
  
  new_df <- df_1 %>% select(-study_id) %>% 
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


#' Closed treatment_characteristics (var discontinued)
#'
#' @description This function visualizes the characteristics of the definitive 
#' fixation process. Data included is completeness, number of df stages, 
#' statistics on distribution, incision location counts and distribution,
#' and adherence by treatment arm
#'
#' @param analytic This is the analytic data set that must include study_id, 
#' enrolled, df_date, plat_df_surgical_incision, pil_df_surgical_incision,
#' df_number_procedures, df_randomized_treatment,
#' injury_classification_plat_ao, df_surg_staged, df_surg_n_forms,
#' randomized, fracture_type, treatment_arm
#'
#' @return html table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_treatment_characteristics()
#' }
closed_treatment_characteristics <- function(analytic){
  
  df <- analytic %>%
    select(study_id, enrolled, df_date, 
           plat_df_surgical_incision, pil_df_surgical_incision,
           df_number_procedures, df_randomized_treatment,
           injury_classification_plat_ao, df_surg_staged, df_surg_n_forms,
           randomized, fracture_type, treatment_arm) %>% 
    filter(enrolled) %>%
    select(-enrolled)
  
  df_a <- df %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- df %>% 
    filter(treatment_arm=="Group B")
  
  inner_treatment_characteristics <- function(inner_df) {
    total <- nrow(inner_df)
    
    one_form_complete <- inner_df %>%
      count(df_surg_staged) %>%
      filter(!is.na(df_surg_staged)) 
    
    one_form_complete <- tibble(
      type = 'Patients with Definitive Fixation Data Complete',
      n = format_count_percent(sum(one_form_complete$n), total)
    )
    
    stages <- inner_df %>%
      count(df_number_procedures) %>%
      mutate(type=ifelse(!is.na(df_number_procedures), df_number_procedures, 'Missing Stages')) %>%
      mutate(n=format_count_percent(n, total))
    
    stages_stats <- inner_df %>%
      pull(df_number_procedures) %>%
      format_mean_sd()
    
    stages <- tibble(
      type = c('Number of Stages [Mean (SD)]', stages$type),
      n = c(stages_stats, stages$n)
    )
    
    incisions <- inner_df %>%
      group_by(study_id) %>%
      separate_rows(plat_df_surgical_incision, pil_df_surgical_incision) %>%
      count(plat_df_surgical_incision, pil_df_surgical_incision)
    
    incisions_count <- tibble(
      type = c('Plateau Count', 'Pilon Count'),
      n = c(incisions %>% filter(!is.na(plat_df_surgical_incision)) %>% nrow() %>% as.character(),
            incisions %>% filter(!is.na(pil_df_surgical_incision)) %>% nrow() %>% as.character())
    )
    
    plat_num_incisions <- incisions %>%
      filter(!is.na(plat_df_surgical_incision)) %>%
      summarize(n=n())
    
    pil_num_incisions <- incisions %>%
      filter(!is.na(pil_df_surgical_incision)) %>%
      summarize(n=n())
    
    incisions <- full_join(incisions_count, 
                           tibble(
        #can't add number to row here
        type = c('Plateau [Mean(SD)]','Pilon [Mean(SD)]'),
        n = c(format_mean_sd(plat_num_incisions$n), format_mean_sd(pil_num_incisions$n))
      )
    )
    
    protocol_followed <- inner_df %>%
      count(df_randomized_treatment) %>%
      mutate(type=ifelse(!is.na(df_randomized_treatment), 
                         ifelse(df_randomized_treatment, 'Yes', 'No'), 
                         'Missing Protocol Adherence')) %>%
      mutate(n=format_count_percent(n, total))
    protocol_followed <- protocol_followed[order(-protocol_followed$df_randomized_treatment, na.last = TRUE), ]
    
    out <- full_join(one_form_complete, stages) %>%
      full_join(incisions) %>%
      full_join(protocol_followed) %>%
      select(type, n) %>%
      rename_with(~paste0('(N=', as.character(total),')'), n)
    
    list('table'=out, 
                'com_rows'=nrow(one_form_complete), 
                'stage_rows'=nrow(stages), 
                'inc_rows'=nrow(incisions), 
                'pro_rows'=nrow(protocol_followed), 
                'total_n'=total)
  }
  
  full_table <- inner_treatment_characteristics(df)
  a_table <- inner_treatment_characteristics(df_a)$table
  b_table <- inner_treatment_characteristics(df_b)$table
  
  df_table <- full_join(a_table, b_table) %>%
    full_join(full_table$table) %>%
    rename(' ' = 'type') %>%
    rename_with(~ paste0('Group A ', .), 2) %>%
    rename_with(~ paste0('Group B ', .), 3) %>%
    rename_with(~ paste0('Overall ', .), 4)
  
  #TODO find better numbers for packed rows
  vis <- kable(df_table, format="html", align='l') %>%
    pack_rows(index = c(' ' = full_table$com_rows, 
                        'Definitive Fixation' = full_table$stage_rows, 
                        'Incision Locations' = full_table$inc_rows, 
                        'Study Treatment Adhering to Protocol' = full_table$pro_rows),
              label_row_css = "text-align:left") %>% 
    add_indent((1+full_table$com_rows+full_table$stage_rows):
               (full_table$com_rows+full_table$stage_rows+full_table$inc_rows)) %>%
    row_spec(0, extra_css = "border-bottom: 2px solid") %>%
    row_spec(full_table$com_rows, extra_css = "border-bottom: 2px solid") %>%
    row_spec(full_table$com_rows+full_table$inc_rows+full_table$stage_rows+full_table$pro_rows, 
             extra_css = "border-bottom: 2px solid") %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(vis)
}



#' Closed adherence_sextant
#'
#' @description This function visualizes the treatment characteristics per protocol and assignmnet for Sextant for
#' each treatment group
#'
#' @param analytic This is the analytic data set that must include adherence_to_intervention_dwc,
#' adherence_to_intervention_post_dwc, adherence_to_no_other_antibiotic_dwc, treatment_arm
#'
#' @return nothing
#' 
#' Closed followup followup_2wk_status_by_site_tobra
#'
#' @description This function visualizes 2 weeks followup status by site for Clinical followup form(crf09) and patient
#' medical record review(crf08) for tobra weekly report 
#'
#' @param analytic study_id, df_date, enrolled, facilitycode, followup_status_crf08_2wk, followup_status_crf09_2wk
#' 
#' @return html table
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' closed_adherence_sextant()
#' }
closed_adherence_sextant <- function(analytic, footnotes=NULL){
  
  confirm_stability_of_related_visual('adherence_sextant', 'a7970bc0e7a68c4a3210c478baa0fdbc')
  
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
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_characteristics_treatment()
#' }
closed_characteristics_treatment <- function(analytic){
  
  confirm_stability_of_related_visual('characteristics_treatment', 'da790cf8c715e5b060788e7865bfdbcd')
  
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
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_enrollment_status_by_site_var_discontinued()
#' }
closed_enrollment_status_by_site_var_discontinued <- function(analytic, discontinued="discontinued", discontinued_colname="Discontinued", footnotes = NULL){
   #NOTE: USES OPEN VERSION IN A STACKED FORMAT, AUTOMATICALLY SYNCED (2024-05-23)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4>Group A</h4><br />",
                  enrollment_status_by_site_var_discontinued(df_a, discontinued=discontinued, discontinued_colname=discontinued_colname),
                  "<h4>Group B</h4><br />",
                  enrollment_status_by_site_var_discontinued(df_b, discontinued=discontinued, discontinued_colname=discontinued_colname))
  } else{
    out <- paste0("<h4>Group A</h4><br />",
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
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_ih_and_dc_crossover_monitoring_by_site_cutoff_date()
#' }
closed_ih_and_dc_crossover_monitoring_by_site_cutoff_date <- function(analytic, footnotes = NULL){
  #NOTE: USES OPEN VERSION IN A STACKED FORMAT, AUTOMATICALLY SYNCED (2024-05-23)
  
  df_a <- analytic %>% 
    filter(treatment_arm=="Group A")
  
  df_b <- analytic %>% 
    filter(treatment_arm=="Group B")
  
  if(is.null(footnotes)){
    out <- paste0("<h4>Group A</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site_cutoff_date(df_a),
                  "<h4>Group B</h4><br />",
                  ih_and_dc_crossover_monitoring_by_site_cutoff_date(df_b))
  } else{
    out <- paste0("<h4>Group A</h4><br />",
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
#' @param analytic This is the analytic data set that must include study_id, followup_data
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_expected_and_followup_visit_overall()
#' }
closed_expected_and_followup_visit_overall <- function(analytic, footnotes = NULL){
  confirm_stability_of_related_visual('expected_and_followup_visit_overall', 'c775704ce8301618d56419205a167f87')
  
  pull <- analytic %>% 
    select(study_id, followup_data, treatment_arm) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
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
      mutate(status = tools::toTitleCase(status)) %>%
      mutate(status = ifelse(status == 'Not_started', 'Not Started', status))
    
    df_empty <- data.frame('status' = c("Complete", "Early", "Late", 'Missing', 'Not Started', 'Incomplete'))
    
    final_raw <- left_join(df_empty, combined, by = 'status') %>% 
      mutate(across(everything(), ~replace_na(., 0)))
    
    summed_statuses <- c("Complete", "Incomplete", "Missing", "Not Started")
    
    expected_row <- final_raw %>%
      filter(status %in% summed_statuses) %>%
      summarize(across(-status, \(x) sum(x, na.rm = TRUE))) %>%
      mutate(status = "Expected") %>%
      select(status, everything())
    
    final_pre_pct <- rbind(expected_row, final_raw)
    
    divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
    names(divisor_expected) <- names(final_pre_pct)[-1]
    divisor_complete <- final_pre_pct[2, -1] %>% as.numeric()
    names(divisor_complete) <- names(final_pre_pct)[-1]
    
    top <- final_pre_pct %>% 
      slice_head(n=2) %>%
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
      slice_head(n=4) %>% 
      slice_tail(n=2) %>% 
      mutate(across(-status, 
                    ~ format_count_percent(., divisor_complete[cur_column()]),
                    .names = "{.col}"))
    
    final_last <- rbind(expected_row, top, middle, bottom) %>% 
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
  
  colnames(combined_statuses) <- c('Status', '3 Month', '6 Month', '12 Month',
                                   '3 Month', '6 Month', '12 Month')
  
  
  vis <- kable(combined_statuses, format = "html", align = 'l') %>%
    add_indent(c(3, 4)) %>%
    add_header_above(c(' ', 'Group A' = 3, 'Group B' = 3)) %>%
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
#' @return A kable table
#' @export
#'
#' @examples
#' \dontrun{
#' closed_fracture_characteristics(analytic)
#' }
closed_fracture_characteristics <- function(analytic){
  confirm_stability_of_related_visual('fracture_characteristics', '18791cc2b2fb3c9f95e46cb421ddc67e')
  
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
  
  df_table <- full_join(df_final_a, df_final_b, by = "type", suffix = c(" (Group A)", " (Group B)")) %>%
    left_join(df_final_full, by = "type") %>%
    select(type, ends_with(" (Group A)"), ends_with(" (Group B)"), percentage)
  
  cnames <- c(' ', paste0('Group A (n=', sum(df_a$enrolled), ')'),
              paste0('Group B (n=', sum(df_b$enrolled), ')'),
              paste0('Overall (n=', total, ')'))
  header <- c(1,1,1,1)
  names(header) <- cnames
  
  n_closed <- nrow(df_table %>% filter(str_detect(type, "Closed Fracture")))
  n_open <- nrow(df_table %>% filter(str_detect(type, "Open Fracture")))
  n_frac <- nrow(df_table %>% filter(str_detect(type, "Tibial")|str_detect(type, 'Unknown')))
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
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_followup_form_at_timepoint_by_site()
#' }
closed_followup_form_at_timepoint_by_site <- function(analytic, timepoint, form_selection, name = NULL){
  confirm_stability_of_related_visual('followup_form_at_timepoint_by_site', '9c5925aff1ea0d4735404debdf061957')
  
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
      
      df_empty <- data.frame('status' = c("Complete", "Early", "Late", 'Missing', 'Not Started', 'Incomplete'))
      
      final_raw <- left_join(df_empty, result, by = 'status') %>% 
        mutate(across(everything(), ~replace_na(., 0)))
      
      summed_statuses <- c("Complete", "Incomplete", "Missing", "Not Started")
      
      expected_row <- final_raw %>%
        filter(status %in% summed_statuses) %>%
        summarize(across(-status, sum, na.rm = TRUE)) %>%
        mutate(status = "Expected") %>%
        select(status, everything())
      
      final_pre_pct <- rbind(expected_row, final_raw)
      
      divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
      names(divisor_expected) <- names(final_pre_pct)[-1]
      divisor_complete <- final_pre_pct[2, -1] %>% as.numeric()
      names(divisor_complete) <- names(final_pre_pct)[-1]
      
      top <- final_pre_pct %>% 
        slice_head(n=2) %>%
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
        slice_head(n=4) %>% 
        slice_tail(n=2) %>% 
        mutate(across(-status, 
                      ~ format_count_percent(., divisor_complete[cur_column()]),
                      .names = "{.col}"))
      
      out <- rbind(expected_row, top, middle, bottom) %>% 
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
  
  header <- c(1,7)
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
#' @param footnotes optional argument for changing the names of the followup forms, for aesthetic use
#'
#' @export
#'
#' @examples
#' \dontrun{
#' closed-followup_form_all_timepoints_by_site()
#' }
closed_followup_form_all_timepoints_by_site <- function(analytic, form_selection = 'Overall', footnotes = NULL){
  confirm_stability_of_related_visual('followup_form_all_timepoints_by_site', 'a1cc824358c28898f006f88dca983f08')
  
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
      
      df_empty <- data.frame('status' = c("Complete", "Early", "Late", 'Missing', 'Not Started', 'Incomplete'))
      
      final_raw <- left_join(df_empty, result, by = 'status') %>% 
        mutate(across(everything(), ~replace_na(., 0)))
      
      summed_statuses <- c("Complete", "Incomplete", "Missing", "Not Started")
      
      expected_row <- final_raw %>%
        filter(status %in% summed_statuses) %>%
        summarize(across(-status, sum, na.rm = TRUE)) %>%
        mutate(status = "Expected") %>%
        select(status, everything())
      
      final_pre_pct <- rbind(expected_row, final_raw)
      
      divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
      names(divisor_expected) <- names(final_pre_pct)[-1]
      divisor_complete <- final_pre_pct[2, -1] %>% as.numeric()
      names(divisor_complete) <- names(final_pre_pct)[-1]
      
      top <- final_pre_pct %>% 
        slice_head(n=2) %>%
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
        slice_head(n=4) %>% 
        slice_tail(n=2) %>% 
        mutate(across(-status, 
                      ~ format_count_percent(., divisor_complete[cur_column()]),
                      .names = "{.col}"))
      
      out <- rbind(expected_row, top, middle, bottom) %>% 
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
    colnames(form_df) <- c('Facility', rep(c("Expected", "Complete", "Early", "Late", 'Missing', 
                                             'Not Started', 'Incomplete'), times = length(timepoints)))
    form_df
  }
  
  df_a_collected <- inner_per_treatment_arm(df_a)
  df_b_collected <- inner_per_treatment_arm(df_b)
  total_collected <- inner_per_treatment_arm(df)
  
  full_collected <- rbind(df_a_collected, df_b_collected, total_collected)
  
  header <- c(1,rep(7, length(timepoints)))
  names(header) <- c(' ', timepoints)
  
  over_header <- c(1, 7*length(timepoints))
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
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closex_followup_forms_at_timepoint_by_site()
#' }
closed_followup_forms_at_timepoint_by_site <- function(analytic, timepoint, forms, names = NULL){
  confirm_stability_of_related_visual('followup_forms_at_timepoint_by_site', '5bfa962197af703ad041b77d484abf64')
  
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
        
        df_empty <- data.frame('status' = c("Complete", "Early", "Late", 'Missing', 'Not Started', 'Incomplete'))
        
        final_raw <- left_join(df_empty, result, by = 'status') %>% 
          mutate(across(everything(), ~replace_na(., 0)))
        
        summed_statuses <- c("Complete", "Incomplete", "Missing", "Not Started")
        
        expected_row <- final_raw %>%
          filter(status %in% summed_statuses) %>%
          summarize(across(-status, sum, na.rm = TRUE)) %>%
          mutate(status = "Expected") %>%
          select(status, everything())
        
        final_pre_pct <- rbind(expected_row, final_raw)
        
        divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
        names(divisor_expected) <- names(final_pre_pct)[-1]
        divisor_complete <- final_pre_pct[2, -1] %>% as.numeric()
        names(divisor_complete) <- names(final_pre_pct)[-1]
        
        top <- final_pre_pct %>% 
          slice_head(n=2) %>%
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
          slice_head(n=4) %>% 
          slice_tail(n=2) %>% 
          mutate(across(-status, 
                        ~ format_count_percent(., divisor_complete[cur_column()]),
                        .names = "{.col}"))
        
        out <- rbind(expected_row, top, middle, bottom) %>% 
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
  
  cols <- c('Facility', rep(c("Expected", "Complete", "Early", "Late", 'Missing', 'Not Started', 
                              'Incomplete'), times = length(forms)))
  colnames(full_collected) <- cols
  
  header <- c(1,rep(7, length(forms)))
  if (is.null(names)) {
    header_names <- c(' ', paste0(forms, ' Status at ', timepoint, ' Period'))
  } else {
    header_names <- c(' ', paste0(names, ' Status at ', timepoint, ' Period'))
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
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' closed_followup_forms_all_timepoints()
#' }
closed_followup_forms_all_timepoints <- function(analytic, forms, timepoints){
  confirm_stability_of_related_visual('followup_forms_all_timepoints', '7872a487edccf907a203d1042c3cf5bb')
  
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
    
    form_df_empty <- data.frame('status' = c("Complete", "Early", "Late", 'Missing', 'Not Started', 'Incomplete'))
    
    final_raw <- left_join(form_df_empty, combined, by = 'status') %>% 
      mutate(across(everything(), ~replace_na(., 0)))
    
    summed_statuses <- c("Complete", "Incomplete", "Missing", "Not Started")
    
    expected_row <- final_raw %>%
      filter(status %in% summed_statuses) %>%
      summarize(across(-status, sum, na.rm = TRUE)) %>%
      mutate(status = "Expected") %>%
      select(status, everything())
    
    final_pre_pct <- rbind(expected_row, final_raw)
    
    divisor_expected <- final_pre_pct[1, -1] %>% as.numeric()
    names(divisor_expected) <- names(final_pre_pct)[-1]
    divisor_complete <- final_pre_pct[2, -1] %>% as.numeric()
    names(divisor_complete) <- names(final_pre_pct)[-1]
    
    top <- final_pre_pct %>% 
      slice_head(n=2) %>%
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
      slice_head(n=4) %>% 
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
  
  vis <- kable(full_collected, format="html", align='l') %>%
    add_indent(c(3,4)) %>%
    add_indent(c(10,11)) %>%
    add_indent(c(17,18)) %>%
    add_header_above(header) %>%
    pack_rows(index = c('Group A' = nrow(df_a_collected),
                        'Group B' = nrow(df_b_collected),
                        'Total' = nrow(total_collected))) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}



