#' Crossover Monitoring by Site
#'
#' @description 
#' Visualizes hospital discharges and patient crossovers out of the number of patients enrolled. Data
#' is organized per site.
#'
#' @param analytic analytic data set that must include enrolled, df_surg_completed, ih_discharge_date, 
#' crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero, and facilitycode
#'
#' @return HTML table
#' @export
#'
#' @examples
#' ih_and_dc_crossover_monitoring_by_site("Replace with Analytic Tibble")
#' 
ih_and_dc_crossover_monitoring_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "enrolled", "df_surg_completed", "ih_discharge_date", "crossover_inpatient",
                           "crossover_discharge", "ih_discharge_date_on_time_zero"),
    example_types = c("FacilityCode", "Boolean", "Boolean", "Date", "Boolean", "Boolean", "Boolean"))
  
  df <- analytic %>% 
    select(facilitycode, enrolled, df_surg_completed, ih_discharge_date, crossover_inpatient, 
           crossover_discharge, ih_discharge_date_on_time_zero) %>% 
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
    select(-is_total) %>% 
    mutate(`Definitive Fixation Complete` = format_count_percent(`Definitive Fixation Complete`, `Enrolled`)) %>% 
    mutate(`Discharged from Index Hospitalization` = format_count_percent(`Discharged from Index Hospitalization`, `Enrolled`)) %>%
    mutate(`Discharged on Radomization Date` = format_count_percent(`Discharged on Radomization Date`, `Enrolled`)) %>%
    mutate(`Inpatient Crossover` = format_count_percent(`Inpatient Crossover`, `Enrolled`)) %>% 
    mutate(`Discharge Crossover` = format_count_percent(`Discharge Crossover`, `Enrolled`))
  
  
  table<- kable(table_raw, format="html",, align='l') %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(table)
}



#' Crossover Monitoring by Site since 01/01/2024
#'
#' @description Visualizes the crossovers by site in hospital and at discharge 
#' after 01/01/2021, by site.
#' 
#' For a visualization of the same data not by site, see: ih_and_dc_crossover_monitoring_cutoff_date
#'
#' @param analytic This is the analytic data set that must include enrolled, df_surg_completed, 
#' ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero, and facilitycode
#'
#' @return html table
#' @export
#'
#' @examples
#' ih_and_dc_crossover_monitoring_by_site_cutoff_date("Replace with Analytic Tibble")
#' 
ih_and_dc_crossover_monitoring_by_site_cutoff_date <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "df_surg_completed", "ih_discharge_date", "crossover_inpatient", 
                           "crossover_discharge",
                           "ih_discharge_date_on_time_zero", "facilitycode"),
    example_types = c("Boolean", "Boolean", "Date", "Boolean", "Boolean", "Boolean", "FacilityCode")) 
  
  df <- analytic %>% 
    select(facilitycode, enrolled, df_surg_completed, ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero)  %>% 
    filter(as.Date(ih_discharge_date) > as.Date("2024-01-01")) %>% 
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
    select(-is_total) %>% 
    mutate(`Definitive Fixation Complete` = format_count_percent(`Definitive Fixation Complete`, `Enrolled`)) %>% 
    mutate(`Discharged from Index Hospitalization` = format_count_percent(`Discharged from Index Hospitalization`, `Enrolled`)) %>%
    mutate(`Discharged on Radomization Date` = format_count_percent(`Discharged on Radomization Date`, `Enrolled`)) %>%
    mutate(`Inpatient Crossover` = format_count_percent(`Inpatient Crossover`, `Enrolled`)) %>% 
    mutate(`Discharge Crossover` = format_count_percent(`Discharge Crossover`, `Enrolled`))
  
  
  table<- kable(table_raw, format="html",, align='l') %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(table)
}


#' Adherence by site
#'
#' @description 
#' Visualizes the status of patient study completion, organized by site. The returned 
#' table has 4 columns: Site, Enrolled, DWC complete, and adherence. The DWC complete
#' column is determined by the existence of a value for the df_date construct. Notably,
#' this visualization determines adherence based on the existence of a value for 
#' the treatment_arm construct.
#' 
#' A similar visualization is adherence_sextant, which uses multiple constructs to determine 
#' all types of adherence.
#'
#' @param analytic analytic data set that must include constructs facilitycode, df_date, enrolled, 
#' treatment_arm
#'
#' @return html table
#' @export
#'
#' @examples
#' adherence_by_site("Replace with Analytic Tibble")
adherence_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', 'df_date', 'enrolled', 'treatment_arm'),
    example_types = c('FacilityCode', 'Date', 'Boolean', "TreatmentArm"))
  
  df <- analytic %>%
    select(facilitycode, enrolled, df_date, treatment_arm) %>% 
    filter(enrolled) %>%
    mutate(df_date = as.character(df_date)) %>%
    mutate(df_date = na_if(df_date, "NA")) %>% 
    mutate(df_complete = ifelse(!is.na(df_date), TRUE, FALSE)) 
  
  treatment_total <- sum(!is.na(df$treatment_arm))
  
  df2 <- df %>% 
    group_by(facilitycode) %>% 
    summarize(elig_enr = sum(enrolled, na.rm = TRUE), 
              df_total = sum(df_complete, na.rm = TRUE), 
              treatment_completed = sum(!is.na(treatment_arm))) %>% 
    mutate(treatment_completed = format_count_percent(treatment_completed, df_total)) 
  
  enrolled_total <- (sum(df2$elig_enr, na.rm = TRUE))
  df_total <- (sum(df2$df_total, na.rm = TRUE))
  
  df3 <- data.frame(facilitycode = 'TOTAL', elig_enr = enrolled_total, df_total = df_total, treatment_completed = format_count_percent(treatment_total, df_total))
  
  df4 <- rbind(df2, df3) %>% 
    rename(`Clinical Site` = facilitycode,
           `Eligible and Enrolled` = elig_enr,
           `Definitive Wound Closure completed` = df_total,
           `Received treatment per protocol and assignment(% DWC complete)` = treatment_completed) 
  
  
  output <- kable(df4, format="html",, align='l') %>%
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(nrow(df4), bold = TRUE)
  
  return(output)
}




#' adherence_sextant
#'
#' @description This function visualizes the treatment characteristics per protocol and assignment for Sextant. 
#'
#' @param analytic This is the analytic data set that must include adherence_to_intervention_dwc,
#' adherence_to_intervention_post_dwc, adherence_to_no_other_antibiotic_dwc, enrolled, dwc_date
#'
#' @return nothing
#' @export
#'
#' @examples
#' adherence_sextant("Replace with Analytic Tibble")
#' 
adherence_sextant <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("adherence_to_intervention_dwc", "adherence_to_intervention_post_dwc",
                           "adherence_to_no_other_antibiotic_dwc", "enrolled", "dwc_date"),
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Date")) 
  
  df <- analytic %>% 
    select(adherence_to_intervention_dwc,
           adherence_to_intervention_post_dwc,
           adherence_to_no_other_antibiotic_dwc, dwc_date, enrolled) %>% 
    filter(!is.na(dwc_date) & enrolled) %>% 
    select(-dwc_date, -enrolled)
  
  result <- df %>%
    group_by(adherence_to_intervention_dwc, adherence_to_intervention_post_dwc, adherence_to_no_other_antibiotic_dwc) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) 
  
  total <- result %>%
    pull(count) %>% 
    sum()
  
  df <- result %>% 
    mutate(count = format_count_percent(count, total)) %>% 
    mutate_at(vars(-count), funs(ifelse(is.na(.), 'Missing Data', ifelse(. == TRUE, "Adherent", "Not Adherent")))) %>% 
    rename(`Local antibiotic treatment at DWC` = adherence_to_intervention_dwc,
           `Systemic antibiotic treatment post DWC`= adherence_to_intervention_post_dwc,
           `No other local antibiotic use at DWC` = adherence_to_no_other_antibiotic_dwc,
           `Overall` = count)  
  
  colnames(df)[which(names(df) == "Overall")] <- paste("Overall (n =", total, ")", sep = " ")
  
  output <- kable(df, format="html",, align='l') %>%
    kable_styling("striped", full_width = F, position="left")  
  
  return(output)
}



#' Characteristics of treatment
#'
#' @description 
#' Visualizes definitive fixation data and study protocol adherence. The function will be formatted 
#' properly when analytic data includes at most two stages of definitive fixation.
#' 
#' Construct df_number_procedures determines the number of stages of definitive fixation. Constructs
#' df_plat_surgical_incision and df_pil_surgical_incision use semicolon seperators to count the number 
#' of incisions at the plateau and pilon sites.
#'
#' @param analytic analytic data set that must study_id, enrolled, df_date, df_plat_surgical_incision, 
#' df_pil_surgical_incision, df_number_procedures, adherence_to_intervention
#'
#' @return nothing
#' @export
#'
#' @examples
#' characteristics_treatment("Replace with Analytic Tibble")
#' 
characteristics_treatment <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('enrolled', 'df_date', 'df_plat_surgical_incision', 'df_pil_surgical_incision', 
                           'df_number_procedures', 'adherence_to_intervention'), 
    example_types = c('Boolean', 'Date', 'Category-NS', 'Category-NS', 'Number-U2', 'Boolean'))
  
  df <- analytic %>% 
    select(study_id, enrolled, df_date, df_plat_surgical_incision, df_pil_surgical_incision, df_number_procedures, adherence_to_intervention) %>% 
    filter(enrolled)
  
  total <- sum(df$enrolled, na.rm=T)
  df_total <- sum(!is.na(df$df_date))
  
  df_complete <- data.frame(type = 'Patients with Definitive Fixation Data Complete', percentage = format_count_percent(df_total, total))
  
  plat <- sum(!is.na(df$df_plat_surgical_incision))
  pil <- sum(!is.na(df$df_pil_surgical_incision))
  
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
    mutate(plat_incisions = str_count(df_plat_surgical_incision, ";") + 1) %>% 
    summarize(type = paste0('Plateau Fractures (n = ', plat, ")"), percentage = format_mean_sd(plat_incisions))
  
  pil_incisions <- df %>%
    filter(!is.na(df_date)) %>% 
    mutate(pil_incisions = str_count(df_pil_surgical_incision, ";") + 1) %>% 
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
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  n_df <- nrow(df_complete)
  n_avg <- nrow(avg_stages)
  n_stages <- nrow(stages)
  
  vis <- kable(df_final, format="html",, align='l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c(" " = nrow(df_complete), 'Definitive Fixation' = (nrow(avg_stages) + nrow(stages)), 'Number of Incisions [Mean (SD)]' = (nrow(pil_incisions) + nrow(plat_incisions)),
                        'Study Treatment Adhering to Protocol' = nrow(adherence)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1, extra_css = 'border-bottom: 1px solid') %>% 
    add_indent(seq(n_stages) + n_avg + n_df)
  
  return(vis)
}

