


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
  
  table_raw<- kable(table_raw, align='l', padding='2l') %>% 
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 4)) %>%
    kable_styling("striped", full_width = F, position="left")
  table_raw <- str_replace_all(table_raw, 'table table-striped"', 'table-basic" border=1 frame=hsides rules=rows')
  table_raw <- str_replace_all(table_raw, '#ddd', 'black')
  table_raw<- gsub('([a-z])\\.','\\1 ',table_raw)
  
  return(table_raw)
}

