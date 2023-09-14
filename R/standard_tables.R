


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
    mutate(Form = toupper(Form)) 
  
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

