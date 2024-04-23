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
#' @description This function visualizes the crossovers by site in hospital and at discharge 
#' after 01/01/2021
#'
#' @param analytic This is the analytic data set that must include enrolled, df_surg_completed, 
#' ih_discharge_date, crossover_inpatient, crossover_discharge, ih_discharge_date_on_time_zero, and facilitycode
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' ih_and_dc_crossover_monitoring_by_site_cutoff_date()
#' }
ih_and_dc_crossover_monitoring_by_site_cutoff_date <- function(analytic){
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


#' adherence by site
#'
#' @description This function visualizes the treatment crossover or any nonadherence occured during the study.
#'
#' @param analytic This is the analytic data set that must include facilitycode, df_date, df_randomized_treatment
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' adherence_by_site()
#' }
adherence_by_site <- function(analytic){
  
  df <- analytic %>%
    select(facilitycode, enrolled, df_date, treatment_arm) %>% 
    filter(enrolled) %>%
    mutate(df_date = na_if(df_date, "NA")) %>% 
    mutate(df_complete = ifelse(!is.na(df_date), TRUE, FALSE)) 
  
  treatment_total <- (sum(df$treatment_arm, na.rm = TRUE))
  
  df2 <- df %>% 
    group_by(facilitycode) %>% 
    summarize(elig_enr = sum(enrolled, na.rm = TRUE), df_total = sum(df_complete, na.rm = TRUE), treatment_completed = sum(treatment_arm, na.rm = TRUE)) %>% 
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