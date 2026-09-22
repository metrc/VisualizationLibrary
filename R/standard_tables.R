#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled
#'
#' @description 
#' Visualizes the count of the study statuses for each site, as well as displaying the days the site 
#' has been certified.
#' 
#' For other enrollment by site visualizations that may better fit your study, please look at: enrollment_status_by_site_var_discontinued, 
#' enrollment_by_site_last_days_var_disc, enrollment_status_by_site
#'
#' @param analytic analytic data set that must include screened, eligible, refused, consented, enrolled, 
#' not_consented, discontinued_pre_randomization, site_certification_date, facilitycode, late_ineligible
#'
#' @return html table
#' @export
#'
#' @examples
#' enrollment_status_by_site("Replace with Analytic Tibble")
#' 
enrollment_status_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('screened', 'eligible', 'refused', 'consented', 'enrolled', 'not_consented', 
                           'discontinued_pre_randomization', 'site_certification_date', 
                           'facilitycode', 'late_ineligible'), 
    example_types = c('Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Number',
                      'FacilityCode', 'Boolean'))

  df <- analytic %>% 
    select(screened, eligible, refused, consented, enrolled, not_consented, discontinued_pre_randomization, site_certification_date, 
           facilitycode, late_ineligible) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    rename(not_enrolled = not_consented) %>% 
    filter(!is.na(Facility))
  
  
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
    rename("Discontinued Post-Randomization (late ineligible)" = `Late Ineligible`) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Consented)) %>% 
    rename("Enrolled & Eligible" = `Enrolled`) %>% 
    mutate(Consented = format_count_percent(Consented, Eligible)) %>% 
    mutate(Refused = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Enrolled for Other Reasons` = format_count_percent(`Not Enrolled for Other Reasons`, Eligible)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Screened))
  
  table<- kable(table_raw, format="html", align='l') %>%
    add_header_above(c(" " = 4, "Among Eligible" = 3, "Among Consented" = 3)) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}



#' Number of Subjects Screened, Eligible, Enrolled and Not Enrolled (Variable Discontinued)
#'
#' @description 
#' Visualizes the totals of each include construct by site, split into among eligible and among consented.
#' 
#' For other enrollment by site visualizations that may better fit your study, refer to: enrollment_by_site, 
#' enrollment_by_site_last_days_var_disc, enrollment_status_by_site, enrollment_status_by_site_var_discontinued
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, site_certification_date, facilitycode,
#' consent_date, not_randomized
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_status_by_site_var_discontinued("Replace with Analytic Tibble")
#' 
enrollment_status_by_site_var_discontinued <- function(analytic, discontinued="discontinued", 
                                                       discontinued_colname="Discontinued", pre_screened = NULL,
                                                       pre_screened_eligible = NULL, only_total=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "refused", "consented", "enrolled", "randomized",
                           "not_consented", "site_certification_date", "facilitycode", "consent_date",
                           "not_randomized", "discontinued"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Date", "FacilityCode", "Date", "Boolean", "Boolean"))
  
  df <- analytic %>%
    select(screened, eligible, refused, not_consented, consented, not_randomized, randomized, enrolled,
      site_certification_date, facilitycode, any_of(c(discontinued, pre_screened, pre_screened_eligible)))
  
  colnames(df)[which(names(df) == discontinued)] <- "discontinued"

  if (!is.null(pre_screened)) {
    colnames(df)[which(names(df) == pre_screened)] <- "pre_screened"
  }
  if (!is.null(pre_screened_eligible)) {
    colnames(df)[which(names(df) == pre_screened_eligible)] <- "pre_screened_eligible"
  }
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  if (!is.null(pre_screened) && !is.null(pre_screened_eligible)) {
    df_1st <- df %>%
      group_by(Facility) %>%
      summarize(
        `Days Certified` = site_certified_days[1],
        `Pre-screened` = sum(pre_screened),
        `Pre-screened Eligible` = sum(pre_screened_eligible),
        Screened = sum(screened),
        Eligible = sum(eligible))
  } else if (!is.null(pre_screened)) {
    df_1st <- df %>%
      group_by(Facility) %>%
      summarize(
        `Days Certified` = site_certified_days[1],
        `Pre-screened` = sum(pre_screened),
        Screened = sum(screened),
        Eligible = sum(eligible))
  } else if (!is.null(pre_screened_eligible)) {
    df_1st <- df %>%
      group_by(Facility) %>%
      summarize(
        `Days Certified` = site_certified_days[1],
        `Pre-screened Eligible` = sum(pre_screened_eligible),
        Screened = sum(screened),
        Eligible = sum(eligible))
  } else {
    df_1st <- df %>%
      group_by(Facility) %>%
      summarize(
        `Days Certified` = site_certified_days[1],
        Screened = sum(screened),
        Eligible = sum(eligible))
  }
  
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
    mutate(`Days Certified`=ifelse(is_total,"-",`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(!!discontinued_colname := format_count_percent(!!sym(discontinued_colname), Consented)) %>% 
    mutate(Randomized = format_count_percent(Randomized, Consented)) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Consented)) %>% 
    mutate(Consented = format_count_percent(Consented, Eligible)) %>% 
    mutate(Refused = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Consented` = format_count_percent(`Not Consented`, Eligible)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Screened))
  
  if(only_total){
    table_raw <- table_raw %>% filter(Facility=="Total")
  }
  
  n_pre   <- !is.null(pre_screened)
  n_pre_el<- !is.null(pre_screened_eligible)
  no_header <- sum(c(n_pre, n_pre_el)) + 4
  
  header <- c(" " = no_header, "Among Eligible" = 3, "Among Consented" = 3)
  
  table <- kable(table_raw, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}

#' enrollment_status_by_site_var_discontinued_i
#'
#' @description 
#' Visualizes the totals of screening, eligibility, and consent by site.
#'
#' @param analytic Analytic data set. Must include: screened, eligible, refused, 
#' consented, not_consented, site_certification_date, facilitycode.
#' @param pre_screened Optional column name for pre-screened counts.
#' @param pre_screened_eligible Optional column name for pre-screened eligible counts.
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_status_by_site_var_discontinued_i("Replace with Analytic Tibble")
enrollment_status_by_site_var_discontinued_i <- function(analytic, pre_screened = NULL,
                                                pre_screened_eligible = NULL, only_total=FALSE){
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "refused", "consented", "not_consented", 
                           "site_certification_date", "facilitycode"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Date", "FacilityCode"))
  
  df <- analytic %>%
    select(screened, eligible, refused, not_consented, consented,
           site_certification_date, facilitycode, any_of(c(pre_screened, pre_screened_eligible))) %>%
    arrange(facilitycode)
  
  if (!is.null(pre_screened)) colnames(df)[which(names(df) == pre_screened)] <- "pre_screened"
  if (!is.null(pre_screened_eligible)) colnames(df)[which(names(df) == pre_screened_eligible)] <- "pre_screened_eligible"
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  if (!is.null(pre_screened) && !is.null(pre_screened_eligible)) {
    df_1st <- df %>% group_by(Facility) %>%
      summarize(`Days Certified` = site_certified_days[1],
                `Pre-screened` = sum(pre_screened),
                `Pre-screened Eligible` = sum(pre_screened_eligible),
                Screened = sum(screened), Eligible = sum(eligible))
  } else if (!is.null(pre_screened)) {
    df_1st <- df %>% group_by(Facility) %>%
      summarize(`Days Certified` = site_certified_days[1],
                `Pre-screened` = sum(pre_screened),
                Screened = sum(screened), Eligible = sum(eligible))
  } else if (!is.null(pre_screened_eligible)) {
    df_1st <- df %>% group_by(Facility) %>%
      summarize(`Days Certified` = site_certified_days[1],
                `Pre-screened Eligible` = sum(pre_screened_eligible),
                Screened = sum(screened), Eligible = sum(eligible))
  } else {
    df_1st <- df %>% group_by(Facility) %>%
      summarize(`Days Certified` = site_certified_days[1],
                Screened = sum(screened), Eligible = sum(eligible))
  }
  
  df_2nd <- df %>% 
    filter(eligible == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(Refused = sum(refused), 'Not Consented' = sum(not_consented), Consented = sum(consented))
  
  table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,"-",`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(Consented = format_count_percent(Consented, Eligible)) %>% 
    mutate(Refused = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Consented` = format_count_percent(`Not Consented`, Eligible)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Screened))
  
  if(only_total) table_raw <- table_raw %>% filter(Facility=="Total")
  
  n_pre    <- !is.null(pre_screened)
  n_pre_el <- !is.null(pre_screened_eligible)
  no_header_left <- sum(c(n_pre, n_pre_el)) + 4 
  
  header_left <- c(" " = no_header_left, "Among Eligible" = 3)
  
  table <- kable(table_raw, format="html", align='l') %>%
    add_header_above(header_left) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(table)
}

#' enrollment_status_by_site_var_discontinued_ii
#'
#' @description 
#' Visualizes the totals of randomization, discontinuation, and enrollment by site.
#' Values are calculated as percentages of the "Consented" population.
#'
#' @param analytic Analytic data set. Must include: eligible, consented, enrolled, 
#' randomized, facilitycode, discontinued (or custom name).
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_status_by_site_var_discontinued_ii("Replace with Analytic Tibble")
enrollment_status_by_site_var_discontinued_ii <- function(analytic, discontinued="discontinued",
                                                 discontinued_colname="Discontinued", only_total=FALSE){
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("eligible", "consented", "enrolled", "randomized",
                           "facilitycode", "discontinued"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean",
                      "FacilityCode", "Boolean"))
  
  df <- analytic %>%
    select(eligible, consented, randomized, enrolled, facilitycode, any_of(discontinued)) %>%
    arrange(facilitycode)
  
  colnames(df)[which(names(df) == discontinued)] <- "discontinued"
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  df_consented_totals <- df %>%
    filter(eligible == TRUE) %>%
    group_by(Facility) %>%
    summarize(Consented_Count = sum(consented))
  
  df_main <- df %>% 
    filter(eligible == TRUE & consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize("Randomized" = sum(randomized),
              !!discontinued_colname := sum(discontinued),
              "Enrolled" = sum(enrolled)) 
  
  table_raw <- full_join(df_consented_totals, df_main, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(!!discontinued_colname := format_count_percent(!!sym(discontinued_colname), Consented_Count)) %>% 
    mutate(Randomized = format_count_percent(Randomized, Consented_Count)) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Consented_Count)) %>%
    select(-Consented_Count)
  
  if(only_total) table_raw <- table_raw %>% filter(Facility=="Total")
  
  header_right <- c(" " = 1, "Among Consented" = 3)
  
  table <- kable(table_raw, format="html", align='l') %>%
    add_header_above(header_right) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(table)
}

#' Monitoring required
#'
#' @description 
#' Simple function that returns whether a site has reached the number of enrolled required for monitoring
#' to be required.
#'
#' @param analytic analytic data set that must include enrolled, facilitycode, consent_date
#' @param standard_threshold number to use as baseline threshold for monitoring requirement
#' @param spec_threshold optional kwarg that allows you to specify sites that have different monitoring
#' thresholds
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' monitoring_required("Replace with Analytic Tibble", standard_threshold = 25, spec_threshold = list('AAA' = 50))
#' 
monitoring_required <- function(analytic, standard_threshold = 10, spec_threshold = list()) {
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', 'enrolled', 'consent_date'), 
    example_types = c("FacilityCode", 'Boolean', 'Date'))
  
  sum_enrolled <- analytic %>%
    filter(enrolled) %>%
    group_by(facilitycode) %>%
    summarize(enrolled_count = n(), .groups = 'drop')
  
  spec_sites <- names(spec_threshold)
  
  mon_req <- sum_enrolled %>%
    rowwise() %>%
    mutate(`Monitoring Required` = ifelse(
      facilitycode %in% spec_sites,
      enrolled_count >= spec_threshold[[facilitycode]],
      enrolled_count >= standard_threshold
    )) %>%
    ungroup()
  
  dates <- analytic %>%
    filter(enrolled) %>%
    group_by(facilitycode) %>%
    summarize(consent_dates = list(sort(consent_date)), .groups = 'drop')
  
  combined <- mon_req %>% 
    left_join(dates, by = "facilitycode") %>%
    rowwise() %>%
    mutate(`Date Monitoring Required` = 
             ifelse(`Monitoring Required`, 
                    ifelse(facilitycode %in% spec_sites,
                           as.character(consent_dates[[spec_threshold[[facilitycode]]]]),
                           as.character(consent_dates[[standard_threshold]])),
                    'Monitoring Not Required')) %>%
    select(-consent_dates, -`Monitoring Required`) %>%
    rename(`Enrolled Count` = enrolled_count) %>%
    ungroup()
  
  anti_enrolled <- analytic %>%
    filter(!enrolled | is.na(enrolled)) %>%
    filter(!facilitycode %in% combined$facilitycode) %>%
    filter(!is.na(facilitycode)) %>%
    select(facilitycode) %>%
    unique() %>%
    mutate(`Enrolled Count` = 0,
           `Date Monitoring Required` = "Monitoring Not Required")
  
  combined <- combined %>%
    rbind(anti_enrolled) %>%
    arrange(desc(`Enrolled Count`))
  
  vis <- kable(combined, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}




#' Ankle and Plateau X-Ray and Measurement Status
#'
#' @description 
#' This function visualizes Ankle and Plateau X-Ray and Measurement Status.
#' 
#' NOTE: this function is very unlikely to work with your study, check the construct requirements in
#' the analytic parameter.
#'
#' @param analytic analytic data set that must include followup_expected_6wk,followup_expected_3mo,followup_expected_6mo, 
#' followup_expected_12mo,injury_type,followup_data,radiographs_taken_6wk, 
#' radiographs_taken_3mo,radiographs_taken_6mo,plat_tib_fib_overlap_6mo, 
#' plat_sagittal_pl_alignment_6mo, plat_patella_centered_6mo, plat_medial_prox_tibia_deg_6mo, 
#' plat_medial_lateral_diff_6mo,plat_condylar_width_6mo,plat_art_step_off_medial_6mo, 
#' plat_art_step_off_lateral_6mo,plat_femur_tibia_deg_6mo,plat_tib_fib_overlap_3mo, 
#' plat_sagittal_pl_alignment_3mo,plat_patella_centered_3mo,plat_medial_prox_tibia_deg_3mo, 
#' plat_medial_lateral_diff_3mo,plat_condylar_width_3mo,plat_art_step_off_medial_3mo, 
#' plat_art_step_off_lateral_3mo,plat_femur_tibia_deg_3mo,plat_tib_fib_overlap_6wk, 
#' plat_sagittal_pl_alignment_6wk,plat_patella_centered_6wk,plat_medial_prox_tibia_deg_6wk, 
#' plat_medial_lateral_diff_6wk,plat_condylar_width_6wk,plat_art_step_off_medial_6wk, 
#' plat_art_step_off_lateral_6wk,plat_femur_tibia_deg_6wk,ankle_talar_tilt_degrees_6wk, 
#' ankle_sagital_disp_6wk,ankle_coronal_plane_disp_6wk,ankle_talar_tilt_degrees_3mo, 
#' ankle_sagital_disp_3mo,ankle_coronal_plane_disp_3mo,ankle_talar_tilt_degrees_6mo, 
#' ankle_sagital_disp_6mo,ankle_coronal_plane_disp_6mo
#' 
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' ankle_and_plateau_x_ray_and_measurement_status()
#' }
ankle_and_plateau_x_ray_and_measurement_status <- function(analytic){
  
  df1_ankle <- analytic %>% 
    filter(injury_type=="ankle") %>% 
    select(followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA') %>% 
    filter(form == 'Clinical Follow-up') %>% 
    mutate(status = ifelse(str_detect(status,"Complete"),"Complete",status)) %>% 
    group_by(form, followup_period, status) %>%
    count() %>%
    filter(!is.na(status)) %>%
    ungroup() %>% 
    mutate(name = recode(followup_period, 
                         "6 Week" = "6 Weeks", 
                         "3 Month" = "3 Months",
                         "6 Month" = "6 Months",
                         "12 Month" = "12 Months")) %>% 
    rename(value = status) %>% 
    select(name, n, value)
  
  
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
    reframe(value=c("Complete", "Incomplete", "Missed", "Expected")) %>% 
    ungroup()
  
  df_one_ankle <- left_join(df1_shell_ankle, bind_rows(df1_expected_ankle, df1_ankle)) %>% 
    mutate(value = factor(value, c("Expected", "Complete", "Incomplete", "Missed"))) %>% 
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
                                          ifelse(is.na(radiographs_taken_6wk),"Missed",radiographs_taken_6wk),NA)) %>% 
    mutate(radiographs_taken_3mo = ifelse(followup_expected_3mo,
                                          ifelse(is.na(radiographs_taken_3mo),"Missed",radiographs_taken_3mo),NA)) %>% 
    mutate(radiographs_taken_6mo = ifelse(followup_expected_6mo,
                                          ifelse(is.na(radiographs_taken_6mo),"Missed",radiographs_taken_6mo),NA)) %>% 
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
    mutate(value = factor(value, c("Yes", "No", "Missed", " "))) %>% 
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
  
  table_raw_ankle<- kable(df_ankle, format="html", align='l', col.names = str_replace(colnames(df_ankle),"^n.|^n"," ")) %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left')
  
  
  
  df1_plateau <- analytic %>% 
    filter(injury_type=="plateau") %>% 
    select(followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA') %>% 
    filter(form == 'Clinical Follow-up') %>% 
    mutate(status = ifelse(str_detect(status,"Complete"),"Complete",status)) %>% 
    group_by(form, followup_period, status) %>%
    count() %>%
    filter(!is.na(status)) %>%
    ungroup() %>% 
    mutate(name = recode(followup_period, 
                         "6 Week" = "6 Weeks", 
                         "3 Month" = "3 Months",
                         "6 Month" = "6 Months",
                         "12 Month" = "12 Months")) %>% 
    rename(value = status) %>% 
    select(name, n, value)
  
  
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
    reframe(value=c("Complete", "Incomplete", "Missed", "Expected")) %>% 
    ungroup()
  
  df_one_plateau <- left_join(df1_shell_plateau, bind_rows(df1_expected_plateau, df1_plateau)) %>% 
    mutate(value = factor(value, c("Expected", "Complete", "Incomplete", "Missed"))) %>% 
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
                                          ifelse(is.na(radiographs_taken_6wk),"Missed",radiographs_taken_6wk),NA)) %>% 
    mutate(radiographs_taken_3mo = ifelse(followup_expected_3mo,
                                          ifelse(is.na(radiographs_taken_3mo),"Missed",radiographs_taken_3mo),NA)) %>% 
    mutate(radiographs_taken_6mo = ifelse(followup_expected_6mo,
                                          ifelse(is.na(radiographs_taken_6mo),"Missed",radiographs_taken_6mo),NA)) %>% 
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
    reframe(value=c("Yes", "No", "Missed", " ")) %>% 
    ungroup()
  
  df_two_plateau <- left_join(df2_shell_plateau, df2_plateau) %>% 
    mutate(value = factor(value, c("Yes", "No", "Missed", " "))) %>% 
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
  
  table_raw_plateau<- kable(df_plateau, format="html", align='l', col.names = str_replace(colnames(df_plateau),"^n.|^n"," ")) %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left')
  
  output <- paste0("<h3>Ankle</h3><br />",table_raw_ankle, "<h3>Plateau</h3><br />",table_raw_plateau)
  
  return(output)
}


#' Injury characteristics for OTA classification and Schatzker Type injuries
#'
#' @description 
#' Counts the number of enrolled participants enrolled for each class of injury, seperated by Ankle 
#' and Plateau injury types. If injury is not classified, it is marked as "Missed." 
#'
#' @param analytic analytic data set that must include injury_type, injury_classification_ankle_ota, 
#' injury_classification_plat_schatzker, enrolled
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' injury_ankle_plateau_characteristics("Replace with Analytic Tibble")
#' 
injury_ankle_plateau_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c('injury_type', 'injury_classification_ankle_ota', 'injury_classification_plat_schatzker',
                           'enrolled'),
    example_types = c("NamedCategory['ankle' 'plateau']", 
                      "NamedCategory['44B2' '44B3' '44C1' '44C2' '44A2' '44C3' '44B1']",
                      "NamedCategory['Type IV' 'Type I']", "Boolean")) 
  
  df <- analytic %>% 
    select(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker, enrolled) %>%  
    filter(enrolled == TRUE)
  
  summary_totals <- df %>%
    filter(injury_type == "plateau" & is.na(injury_classification_ankle_ota) | injury_type == "ankle" & 
             is.na(injury_classification_plat_schatzker)) %>%
    group_by(injury_type, injury_classification_ankle_ota, injury_classification_plat_schatzker) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    mutate(injury_classification_ankle_ota = ifelse(injury_type == "ankle" & is.na(injury_classification_ankle_ota) & 
                                                      is.na(injury_classification_plat_schatzker), 
                                                    "Missed", 
                                                    injury_classification_ankle_ota)) %>% 
    mutate(injury_classification_plat_schatzker = ifelse(injury_type == "plateau" & is.na(injury_classification_ankle_ota) & 
                                                           is.na(injury_classification_plat_schatzker), 
                                                         "Missed", 
                                                         injury_classification_plat_schatzker)) %>% 
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
  
  ota_number <- summary_table %>% 
    filter(Category == "O") %>% 
    nrow()
  
  schatzer_number <- summary_table %>% 
    filter(Category == "T") %>% 
    nrow()
  
  df_table <- summary_table %>% 
    select(-Category)
  
  index_vec <- c("OTA Classification"= ota_number+1, "Tibial Plateau"=schatzer_number+1) 
  
  table_raw<- kable(df_table, format="html", align='l', col.names = NULL) %>%
    add_indent(c(seq(ota_number)+1, seq(schatzer_number)+1+ota_number+1)) %>% 
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left")
  
  return(table_raw)
}

#' Baseline characteristics percent 
#'
#' @description 
#' Visualizes the categorical distribution of baseline characteristics 
#' sex, age, race, education, military, enrolled. See below as this is a generic visualization and includes meta construct for each of the 
#' analysis outputs. You may also specify the levels that these outputs have in the function call. 
#' Outputs two columns: type (sex, age, race, education, military), and their respective counts and percentages.
#'
#' @param analytic analytic data set that must include enrolled, age, age_group, and the constructs specified
#' in the following parameters.
#' @param sex is a meta construct that is required that defaults to "sex"
#' @param race is a meta construct that is required that defaults to "ethnicity_race"
#' @param education is a meta construct that is required that defaults to "education_level"
#' @param military is a meta construct that is required that defaults to "military_status"
#' @param sex_levels sets default values and orders for sex meta construct
#' @param race_levels sets default values and orders for race meta construct
#' @param education_levels sets default values and orders for education meta construct
#' @param military_levels sets default values and orders for military meta construct
#'
#' @return html table 
#' @export
#'
#' @examples
#' baseline_characteristics_percent("Replace with Analytic Tibble")
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble", race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian / Pacific Islander", "Other", "Missing"))
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble", sex_levels=c("Male", "Female", "Missing"))
#' 
baseline_characteristics_percent <- function(
    analytic, sex="sex", race="ethnicity_race", education="education_level", military="military_status",
    sex_levels=c("Female","Male", "Missing"), 
    race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
    education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing"), 
    military_levels=c("Active Military", "Active Reserves", "Not Active Duty","Missing")){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("sex", "ethnicity_race", "education_level", 'military_status', "age", "age_group", 
                           "enrolled"),
    example_types = c("NamedCategory['Female' 'Male' 'Missing']", "NamedCategory['Non-Hispanic White' 'Non-Hispanic Black' 'Hispanic' 'Other' 'Missing']",
                      "NamedCategory['Less than High School' 'GED or High School Diploma' 'More than High School' 'Refused / Don't know' 'Missing']",
                      "NamedCategory['Active Military' 'Active Reserves' 'Not Active Duty' 'Missing']", "Number", "Category", "Boolean")) 
  
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
  
  vis <- kable(df_final, format="html", align='l',  col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Sex' = nrow(sex_df), 'Age' = (nrow(age_df) + nrow(age_group_df)), 'Race' = nrow(race_df), 
                        'Education' = nrow(education_df), 'Military' = nrow(military_df)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis) 
} 

#' Baseline characteristics percent (no military status)
#'
#' @description 
#' Visualizes the categorical distribution of values for the baseline characteristics sex, age, race, 
#' and education. This function, as opposed to baseline_characteristics_percent, does not return data
#' for any military construct. Also returns the distribution of the age_group construct. Only participants
#' who are enrolled will be counted.
#' 
#' For each characteristic, this function takes two parameters: the construct name parameter and the  
#' construct levels parameter. The construct name parameter specifies the name of the construct to use for  
#' the corresponding characteristic, while the construct levels parameter specifies the expected values for  
#' each characteristic. The construct levels parameters create the rows of an empty table that analytic  
#' data is full joined to, so if, for example, you expect the sex column to contain the values male,  
#' female, and missing and the analytic dataset you provide only has male and missing, the function will  
#' have a row of female with a count of 0. The construct levels parameter also creates the order of the
#' rows. Notably, the function will return values found in the analytic dataset that are not specified 
#' in the levels parameter.
#'
#' @param analytic analytic dataset that must include enrolled, age, age_group, and all the constructs
#' specified in the following construct name parameters
#' @param sex name of the construct of the sex characteristic, defaults to "sex"
#' @param race name of the construct of the race characteristic, defaults to "ethnicity_race"
#' @param education name of the construct of the education characteristic, defaults to "education_level"
#' @param sex_levels default values and orders for sex characteristic
#' @param race_levels default values and orders for race characteristic
#' @param education_levels default values and orders for education characteristic
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble")
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble", race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Asian / Pacific Islander", "Other", "Missing"))
#' baseline_characteristics_percent_nm("Replace with Analytic Tibble", sex_levels=c("Male","Female", "Missing"))
#' 
baseline_characteristics_percent_nm <- function(analytic, sex="sex", race="ethnicity_race", education="education_level",
                                             sex_levels=c("Female","Male", "Missing"), 
                                             race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"), 
                                             education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing")
                                             ){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("sex", "ethnicity_race", "education_level", "age", "age_group",
                           "enrolled"),
    example_types = c("NamedCategory['Female' 'Male' 'Missing']", "NamedCategory['Non-Hispanic White' 'Non-Hispanic Black' 'Hispanic' 'Other' 'Missing']",
                      "NamedCategory['Less than High School' 'GED or High School Diploma' 'More than High School' 'Refused / Don't know' 'Missing']",
                      "Number-U100", "Category-U5", "Boolean")) 
  
  constructs <- c(sex, race, education)
  
  sex_default <- tibble(type=sex_levels)
  race_default <- tibble(type=race_levels)
  education_default <- tibble(type=education_levels)
  
  
  df <- analytic %>% 
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
  
  
  df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df) %>% 
    mutate_all(replace_na, "0 (0%)") 
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  
  vis <- kable(df_final, format="html", align='l',  col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Sex' = nrow(sex_df), 
                        'Age' = (nrow(age_df) + nrow(age_group_df)), 
                        'Race' = nrow(race_df), 
                        'Education' = nrow(education_df)), 
              label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left")  
  
  return(vis) 
} 


#' Baseline characteristics percent plus insurance
#'
#' @description
#' Visualizes the categorical distribution of baseline characteristics
#' sex, age, race, education, military, insurance, enrolled. See below as this is a generic visualization and includes meta construct for each of the
#' analysis outputs. You may also specify the levels that these outputs have in the function call.
#' Every categorical characteristic always shows a "Missing" row, including the age_group characteristic,
#' whose "Missing" row is always ordered last.
#' Outputs two columns: type (sex, age, race, education, military, insurance), and their respective counts and percentages.
#'
#' @param analytic analytic data set that must include enrolled, age, age_group, and the constructs specified
#' in the following parameters.
#' @param sex is a meta construct that is required that defaults to "sex"
#' @param race is a meta construct that is required that defaults to "ethnicity_race"
#' @param education is a meta construct that is required that defaults to "education_level"
#' @param military is a meta construct that is required that defaults to "military_status"
#' @param insurance is a meta construct that is required that defaults to "insurance"
#' @param sex_levels sets default values and orders for sex meta construct
#' @param race_levels sets default values and orders for race meta construct
#' @param education_levels sets default values and orders for education meta construct
#' @param military_levels sets default values and orders for military meta construct
#' @param insurance_levels sets default values and orders for insurance meta construct
#'
#' @return html table
#' @export
#'
#' @examples
#' baseline_characteristics_percent_plus("Replace with Analytic Tibble")
#' baseline_characteristics_percent_plus("Replace with Analytic Tibble", insurance_levels=c("Yes", "No", "Missing"))
#' baseline_characteristics_percent_plus("Replace with Analytic Tibble", sex_levels=c("Male", "Female", "Missing"))
#'
baseline_characteristics_percent_plus <- function(
    analytic, sex="sex", race="ethnicity_race", education="education_level", military="military_status", insurance="insurance",
    sex_levels=c("Female","Male", "Missing"),
    race_levels=c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other", "Missing"),
    education_levels=c("Less than High School", "GED or High School Diploma", "More than High School", "Refused / Don't know", "Missing"),
    military_levels=c("Active Military", "Active Reserves", "Not Active Duty","Missing"),
    insurance_levels=c("Yes", "No", "Missing")){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("sex", "ethnicity_race", "education_level", 'military_status', "insurance", "age", "age_group",
                           "enrolled"),
    example_types = c("NamedCategory['Female' 'Male' 'Missing']", "NamedCategory['Non-Hispanic White' 'Non-Hispanic Black' 'Hispanic' 'Other' 'Missing']",
                      "NamedCategory['Less than High School' 'GED or High School Diploma' 'More than High School' 'Refused / Don't know' 'Missing']",
                      "NamedCategory['Active Military' 'Active Reserves' 'Not Active Duty' 'Missing']", "NamedCategory['Yes' 'No' 'Missing']",
                      "Number", "Category", "Boolean"))

  # A logical insurance construct renders as TRUE/FALSE, which is not a
  # publication label; map it to the Yes/No levels the table expects.
  if (is.logical(analytic[[insurance]])) {
    analytic[[insurance]] <- ifelse(is.na(analytic[[insurance]]), NA_character_,
                                    ifelse(analytic[[insurance]], "Yes", "No"))
  }

  constructs <- c(sex, race, education, military, insurance)

  sex_default <- tibble(type=sex_levels)
  race_default <- tibble(type=race_levels)
  education_default <- tibble(type=education_levels)
  military_default <- tibble(type=military_levels)
  insurance_default <- tibble(type=insurance_levels)
  age_group_default <- tibble(type="Missing")


  df <- analytic %>%
    select(enrolled, age_group, age, all_of(constructs)) %>%
    filter(enrolled) %>%
    rename(sex = !!sym(sex)) %>%
    rename(race = !!sym(race)) %>%
    rename(education = !!sym(education)) %>%
    rename(military = !!sym(military)) %>%
    rename(insurance = !!sym(insurance)) %>%
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
    rename(type = age_group) %>%
    full_join(age_group_default) %>%
    mutate(order = type == "Missing") %>%
    arrange(order) %>%
    select(-order)

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

  insurance_df <- df %>%
    mutate(insurance = replace_na(insurance, "Missing")) %>%
    group_by(insurance) %>%
    count(insurance) %>%
    rename(number = n) %>%
    mutate(percentage = format_count_percent(number, total)) %>%
    select(-number) %>%
    rename(type = insurance) %>%
    full_join(insurance_default) %>%
    mutate(order = factor(type, insurance_levels)) %>%
    arrange(order) %>%
    select(-order)

  df_final <- rbind(sex_df, age_df, age_group_df, race_df, education_df, military_df, insurance_df) %>%
    mutate_all(replace_na, "0 (0%)")

  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames

  vis <- kable(df_final, format="html", align='l',  col.names = NULL) %>%
    add_header_above(header) %>%
    pack_rows(index = c('Sex' = nrow(sex_df), 'Age' = (nrow(age_df) + nrow(age_group_df)), 'Race/Ethnicity' = nrow(race_df),
                        'Education' = nrow(education_df), 'Military' = nrow(military_df), 'Insurance' = nrow(insurance_df)), label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = F, position="left")

  return(vis)
}





#' Number of Non-Completing Participants, SAEs, and Protocol Deviations by type
#'
#' @description This function visualizes the number of non-completions, not expected, and SAEs for only 
#' "enrolled" participants and Protocol Deviations by type for all the "consented" participants. 
#' 
#' Refer to not_complete_sae_deviation_by_type_auto_categories.
#'
#' @param analytic This is the analytic data set that must include enrolled, not_expected_reason, not_completed_reason,
#' protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative, sae_count, not_completed
#' @param include_ae whether to include adverse events in the visualization ae_count, defaults to FALSE
#' @param factors dynamic argument to set a custom order in the final visualization, accepted names are 
#' not_completed, not_espected, deviation_a, deviation_p, deviation_sc and the values are character vectors
#' of final strings in desired order.
#' 
#' @return html table
#' @export
#'
#' @examples
#' not_complete_sae_deviation_by_type("Replace with Analytic Tibble")
#' 
not_complete_sae_deviation_by_type <- function(analytic, include_ae=FALSE, factors=list()){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "not_expected_reason", "not_completed_reason",
                           "protocol_deviation_screen_consent", "protocol_deviation_procedural",
                           "protocol_deviation_administrative", "sae_count", 
                           "not_completed", "consented"), 
    example_types = c("Boolean", "Category", "Category", "Category", "Category",
                      "Category", "Number", "Boolean", "Boolean"))
  
  total <- sum(analytic$enrolled, na.rm = TRUE)
  
  # --- Not Completed
  not_completed_df <- analytic %>% 
    select(enrolled, not_completed_reason, not_completed) %>% 
    mutate(not_completed_reason = ifelse(not_completed, not_completed_reason, NA)) %>% 
    select(-not_completed) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_completed_reason) %>%
    rename(type = not_completed_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  not_completed_df_tot <- tibble(type = "Not Completed", n = sum(not_completed_df$n))
  
  # --- Not Expected
  not_expected_df <- analytic %>% 
    select(enrolled, not_expected_reason) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_expected_reason) %>%
    rename(type = not_expected_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  not_expected_df_tot <- tibble(type = "Not Expected", n = sum(not_expected_df$n))
  
  # --- SAE
  total_saes <- sum(as.numeric(analytic$sae_count[analytic$enrolled == TRUE]), na.rm = TRUE)
  sae_label <- paste("Unique participants with SAEs (of", total_saes, "total SAEs):")
  
  sae_df <- analytic %>% 
    select(study_id, enrolled, sae_count) %>% 
    filter(enrolled & as.numeric(sae_count) > 0) %>% 
    mutate(sae_count = sae_label) %>% 
    count(sae_count) %>%
    rename(type = sae_count) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  if (nrow(sae_df) == 0) sae_df <- tibble(type = sae_label, n = 0)
  
  # --- AE (optional)
  if (include_ae) {
    ae_df <- analytic %>% 
      select(study_id, enrolled, ae_count) %>% 
      filter(enrolled & as.numeric(ae_count) > 0) %>% 
      mutate(ae_count = "AE") %>% 
      count(ae_count) %>% 
      rename(type = ae_count) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    if (nrow(ae_df) == 0) ae_df <- tibble(type = "AE", n = 0)
  }
  
  # --- Consented count “separator” row
  consented <- sum(analytic$consented, na.rm = TRUE)
  consented_df <- tibble(type = " ", n = paste0("n=", consented, ' <sub>(Consented)</sub>'))
  
  # --- Deviations (Screen & Consent)
  deviation_sc_df <- analytic %>% 
    select(study_id, consented, protocol_deviation_screen_consent) %>% 
    separate_rows(protocol_deviation_screen_consent, sep = ";") %>% 
    filter(consented == TRUE) %>% 
    count(protocol_deviation_screen_consent) %>%
    rename(type = protocol_deviation_screen_consent) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  # --- Deviations (Procedural)
  deviation_p_df <- analytic %>% 
    select(study_id, consented, protocol_deviation_procedural) %>% 
    separate_rows(protocol_deviation_procedural, sep = ";") %>% 
    filter(consented == TRUE) %>% 
    count(protocol_deviation_procedural) %>%
    rename(type = protocol_deviation_procedural) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  # --- Deviations (Administrative/Other)
  deviation_a_df <- analytic %>% 
    select(study_id, consented, protocol_deviation_administrative) %>% 
    separate_rows(protocol_deviation_administrative, sep = ";") %>%
    filter(consented == TRUE) %>% 
    mutate(protocol_deviation_administrative = ifelse(grepl("^Other:", protocol_deviation_administrative), 
                                                      "Other", protocol_deviation_administrative)) %>% 
    count(protocol_deviation_administrative) %>%
    rename(type = protocol_deviation_administrative) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = stringr::str_replace(type, "Other: .+", "Other")) %>% 
    mutate(type = as.character(type))
  
  # --- Section totals for deviations
  deviation_sc_tot <- tibble::tibble(type = "Screen and Consent",     n = sum(deviation_sc_df$n))
  deviation_p_tot  <- tibble::tibble(type = "Procedural",             n = sum(deviation_p_df$n))
  deviation_a_tot  <- tibble::tibble(type = "Administrative/Other",   n = sum(deviation_a_df$n))
  deviation_df_tot <- tibble::tibble(type = "Protocol Deviations",    n = sum(deviation_sc_df$n) + sum(deviation_p_df$n) + sum(deviation_a_df$n))
  
  if (!is_empty(factors)) {
    names <- names(factors)

    if ('not_completed' %in% names) {
      not_completed_df <- not_completed_df %>%
        arrange(factor(type, levels = factors[['not_completed']]))
    }
    if ('not_expected' %in% names) {
      not_expected_df <- not_expected_df %>%
        arrange(factor(type, levels = factors[['not_expected']]))
    }
    if ('deviation_sc' %in% names) {
      deviation_sc_df <- deviation_sc_df %>%
        arrange(factor(type, levels = factors[['deviation_sc']]))
    }
    if ('deviation_p' %in% names) {
      deviation_p_df <- deviation_p_df %>%
        arrange(factor(type, levels = factors[['deviation_p']]))
    }
    if ('deviation_a' %in% names) {
      deviation_a_df <- deviation_a_df %>%
        arrange(factor(type, levels = factors[['deviation_a']]))
    }
  }
  
  # --- Assemble final table pieces
  if (include_ae) {
    df_final_top <- rbind(
      not_completed_df_tot, not_completed_df,
      not_expected_df_tot,  not_expected_df,
      sae_df, ae_df,
      consented_df
    ) %>% mutate(n = ifelse(type == " ", n, format_count_percent(n, total, decimals = 0)))
  } else {
    df_final_top <- rbind(
      not_completed_df_tot, not_completed_df,
      not_expected_df_tot,  not_expected_df,
      sae_df,
      consented_df
    ) %>% mutate(n = ifelse(type == " ", n, format_count_percent(n, total, decimals = 0)))
  }
  
  df_final_bottom <- rbind(
    deviation_df_tot,
    deviation_sc_tot, deviation_sc_df,
    deviation_p_tot,  deviation_p_df,
    deviation_a_tot,  deviation_a_df
  ) %>% 
  mutate(n = case_when(
      type == " " ~ as.character(n), 
      type == "Protocol Deviations" ~ as.character(n),
      TRUE ~ format_count_percent(as.integer(n), as.integer(deviation_df_tot$n), decimals = 0)
    ))
  
  df_final <- rbind(df_final_top, df_final_bottom)
  
  # --- Child counts (already robust if any section is empty)
  n_act <- if (exists("not_completed_df")) nrow(not_completed_df) else 0
  n_disc <- if (exists("not_expected_df")) nrow(not_expected_df) else 0
  n_dsc <- if (exists("deviation_sc_df")) nrow(deviation_sc_df) else 0
  n_dp  <- if (exists("deviation_p_df"))  nrow(deviation_p_df) else 0
  n_da  <- if (exists("deviation_a_df"))  nrow(deviation_a_df) else 0
  
  # =========================
  # Robust styling by labels:
  # =========================
  parents <- c(
    "Not Completed",
    "Not Expected",
    sae_label,
    if (include_ae) "AE",
    " ",                       # "n=... (Consented)" separator
    "Protocol Deviations",
    "Screen and Consent",
    "Procedural",
    "Administrative/Other"
  )
  
  # indent every non-parent row (i.e., children under each section)
  indent_idx <- which(!(df_final$type %in% parents))
  
  # end-of-block rows computed from actual row positions + child counts
  r_not_completed_parent <- which(df_final$type == "Not Completed")
  r_not_expected_parent  <- which(df_final$type == "Not Expected")
  r_sae                  <- which(df_final$type == sae_label)
  r_ae                   <- if (include_ae) which(df_final$type == "AE") else integer(0)
  r_consent_sep          <- which(df_final$type == " ")
  r_admin_parent         <- which(df_final$type == "Administrative/Other")
  
  r_not_completed_end <- if (length(r_not_completed_parent)) r_not_completed_parent + n_act else integer(0)
  r_not_expected_end  <- if (length(r_not_expected_parent))  r_not_expected_parent  + n_disc else integer(0)
  r_admin_end         <- if (length(r_admin_parent))         r_admin_parent         + n_da  else integer(0)
  
  # Build kable
  vis <- kableExtra::kable(
    df_final,
    format = "html",
    align  = "l",
    col.names = c(" ", paste0("n=", total, ' <sub>(Enrolled)</sub>')),
    escape = FALSE
  ) %>%
    kableExtra::add_indent(indent_idx) %>%
    kableExtra::row_spec(0, extra_css = "border-bottom: 1px solid") %>%
    { if (length(r_not_completed_end)) kableExtra::row_spec(., r_not_completed_end, extra_css = "border-bottom: 1px solid") else . } %>%
    { if (length(r_not_expected_end))  kableExtra::row_spec(., r_not_expected_end,  extra_css = "border-bottom: 1px solid") else . } %>%
    { if (length(r_sae))               kableExtra::row_spec(., r_sae,               extra_css = "border-bottom: 1px solid") else . } %>%
    kableExtra::kable_styling("striped", full_width = FALSE, position = "left")
  
  if (include_ae && length(r_ae)) {
    vis <- vis %>% kableExtra::row_spec(r_ae, extra_css = "border-bottom: 1px solid")
  }
  
  if (length(r_consent_sep)) {
    vis <- vis %>% kableExtra::row_spec(r_consent_sep, extra_css = "border-bottom: 1px solid; font-weight: bold")
  }
  if (length(r_admin_end)) {
    vis <- vis %>% kableExtra::row_spec(r_admin_end, extra_css = "border-bottom: 1px solid")
  }
  
  return(vis)
}


#' Number of Non-Completing Participants, SAEs, and Protocol Deviations by type with AUTO Protocol Deviation 
#' Categorization
#'
#' @description 
#' Visualizes the number of non-completions, not expected, and SAE presences (multiple SAES add only 
#' one) for enrolled participants and Protocol Deviations by type for consented participants. Amongst 
#' enrolled, counts instances of presence of the construct not_completed_reason for Not Completed count, 
#' not_expected_reason for Not expected count, and sae_count not being 0 for SAE count. Protocol deviation 
#' counts are extracted from the protocol_deviation_full_data long file, where "Other. . ." values are 
#' truncated to "Other."
#' 
#' Categories of protocol deviations are separated by indentation.
#'
#' @param analytic analytic data set that must include enrolled, not_expected_reason, not_completed_reason, 
#' not_completed, protocol_deviation_full_data, sae_count, consented
#' @param category_defaults a vector of category defaults to use for the protocol deviation categories, defaults to c("Safety","Informed Consent","Eligibility","Protocol Implementation","Other")
#' @param include_ae whether to include adverse events in the visualization ae_count, defaults to FALSE
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' not_complete_sae_deviation_by_type_auto_categories("Replace with Analytic Tibble")
#' 
not_complete_sae_deviation_by_type_auto_categories <- function(analytic, 
                                                               category_defaults=c("Safety","Informed Consent",
                                                                                   "Eligibility","Protocol Implementation",
                                                                                   "Other"), include_ae=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('enrolled', "protocol_deviation_full_data", "not_expected_reason", 'not_completed', 
                           'not_completed_reason', 'sae_count', 'consented', "ae_count"), 
    example_types = c('Boolean', "(';new_row: ', '|')FacilityCode|Date|Category|Date|Category|Character", 'Category', 'Boolean',
                      'Category', 'Number', 'Boolean', "Number"))

  total <- sum(analytic$enrolled, na.rm=T)
  not_completed_df <- analytic %>% 
    select(enrolled, not_completed_reason, not_completed) %>% 
    mutate(not_completed_reason = ifelse(not_completed, not_completed_reason, NA)) %>% 
    select(-not_completed) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_completed_reason) %>%
    rename(type=not_completed_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  not_completed_df_tot <- tibble(type="Not Completed", n=sum(not_completed_df$n))
  
  not_expected_df <- analytic %>% 
    select(enrolled, not_expected_reason) %>% 
    filter(enrolled == TRUE) %>% 
    count(not_expected_reason) %>%
    rename(type=not_expected_reason) %>% 
    filter(!is.na(type)) %>% 
    mutate(type = as.character(type))
  
  not_expected_df_tot <- tibble(type="Not Expected", n=sum(not_expected_df$n))
  
  sae_df <- analytic %>% 
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

  if (include_ae) { 
    ae_df <- analytic %>% 
      select(study_id, enrolled, ae_count) %>% 
      filter(enrolled & ae_count > 0) %>% 
      mutate(ae_count = "AE") %>% 
      count(ae_count) %>% 
      rename(type = ae_count) %>% 
      filter(!is.na(type)) %>% 
      mutate(type = as.character(type))
    if (nrow(ae_df) == 0) {
      ae_df <- tibble(type = "AE", n = 0)
    }
  }
  analytic <- analytic %>% 
    mutate(protocol_deviation_data = protocol_deviation_full_data)
  
  
  deviations_df <- analytic %>% 
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

  category_defaults <- c(unique(deviations_df$category)[!unique(deviations_df$category) %in% category_defaults], category_defaults)
  
  deviation_df_tot <- tibble(type="Protocol Deviations",n=sum(deviations_df$n))
  
  consented <- sum(analytic$consented, na.rm = TRUE)
  consented_df <- tibble(type = " ", n = paste0("n=", consented, ' <sub>(Consented)</sub>'))
  
  
  # Build top section with an explicit "level" column for indentation (0 = header/total, 1 = child/type)
  if (include_ae) {
    df_final_top <- bind_rows(
      not_completed_df_tot %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      not_completed_df     %>% mutate(level = 1L) %>% mutate(n=as.character(n)),
      not_expected_df_tot  %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      not_expected_df_tot  %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      sae_df               %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      ae_df                %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      consented_df         %>% mutate(level = 0L)
    )
  } else {
    df_final_top <- bind_rows(
      not_completed_df_tot %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      not_completed_df     %>% mutate(level = 1L) %>% mutate(n=as.character(n)),
      not_expected_df_tot  %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      not_expected_df      %>% mutate(level = 1L) %>% mutate(n=as.character(n)),
      sae_df               %>% mutate(level = 0L) %>% mutate(n=as.character(n)),
      consented_df         %>% mutate(level = 0L)
    )
  }

  # Bottom section (deviations): category headers level 0, their types level 1
  df_final_bottom <- deviation_df_tot %>% mutate(level = 0L)

  for (category_i in category_defaults) {
    category_df <- deviations_df %>%
      filter(category == category_i) %>%
      select(-category)

    tot_df <- tibble(type = category_i, n = sum(category_df$n)) %>% mutate(level = 0L)

    df_final_bottom <- bind_rows(
      df_final_bottom,
      tot_df,
      category_df %>% mutate(level = 1L)
    )
  }

  # Format counts -> percents
  df_final_top <- df_final_top %>%
    mutate(n = ifelse(type == " ", n, format_count_percent(n, total, decimals = 0)))

  df_final_bottom <- df_final_bottom %>%
    mutate(n = case_when(
      type == " " ~ as.character(n),
      type == "Protocol Deviations" ~ as.character(n),
      TRUE ~ format_count_percent(as.integer(n), as.integer(deviation_df_tot$n), decimals = 0)
    ))

  # Combine and compute indent positions directly from "level"
  df_final <- bind_rows(df_final_top, df_final_bottom)

  # Ensure one indent for the child rows directly under Not Completed / Not Expected
  indents_vec <- which(df_final$level == 1L)
  
  # These are used by your row_spec borders; keep as-is
  n_act  <- if (exists("not_completed_df")) nrow(not_completed_df) else 0
  n_disc <- if (exists("not_expected_df")) nrow(not_expected_df) else 0

  # Render
  if (include_ae) {
    vis <- kable(df_final %>% select(type, n),
                 format = "html", align = 'l',
                 col.names = c(" ", paste0("n=", total, ' <sub>(Enrolled)</sub>')),
                 escape = FALSE) %>%
      add_indent(indents_vec) %>%
      row_spec(0, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1 + 1, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1 + 1 + 1, extra_css = "border-bottom: 1px solid; font-weight: bold") %>%
      row_spec(nrow(df_final), extra_css = "border-bottom: 1px solid") %>%
      kable_styling("striped", full_width = F, position = "left")
  } else {
    vis <- kable(df_final %>% select(type, n),
                 format = "html", align = 'l',
                 col.names = c(" ", paste0("n=", total, ' <sub>(Enrolled)</sub>')),
                 escape = FALSE) %>%
      add_indent(indents_vec) %>%
      row_spec(0, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
      row_spec(1 + n_act + 1 + n_disc + 1 + 1, extra_css = "border-bottom: 1px solid; font-weight: bold") %>%
      row_spec(nrow(df_final), extra_css = "border-bottom: 1px solid") %>%
      kable_styling("striped", full_width = F, position = "left")
  }

  return(vis)
}


#' Number of Adjudications and Discontinuations by type
#'
#' @description 
#' This function visualizes the number of discontinuations, SAEs and Protocol Deviations by type.
#' This was originally made for NSAID.
#'
#' @param analytic This is the analytic data set that must include screened, inappropriate_enrollment, 
#' late_ineligible, late_refusal, withdrawn_patient, withdrawn_physician, adjudication_pending, 
#' dead, sae_count, protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative,
#' study_discontinuation
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' adjudications_and_discontinuations_by_type("Replace with Analytic Tibble")
#' 
adjudications_and_discontinuations_by_type <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('screened', 'inappropriate_enrollment', 'late_ineligible', 'late_refusal', 
                           'withdrawn_patient', 'withdrawn_physician', 'dead', 'sae_count', 'protocol_deviation_screen_consent', 
                           'protocol_deviation_procedural', 'protocol_deviation_administrative'), 
    example_types = c('Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Number-U5', 
                      'Category', 'Category', 'Category'))
  
  df <- analytic %>% 
    filter(screened == TRUE) %>% 
    select(inappropriate_enrollment, late_ineligible, late_refusal, withdrawn_patient, withdrawn_physician, #adjudication_pending, 
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
  
  df_final <- rbind(disc, study_discontinuation_df, sae_count_df, protocol_deviations, sc, protocol_deviation_screen_consent_df, 
                    dp, protocol_deviation_procedural_df, da, protocol_deviation_administrative_df) 
  
  n_disc <- nrow(study_discontinuation_df)
  n_dsc <- nrow(protocol_deviation_screen_consent_df)
  n_dp <- nrow(protocol_deviation_procedural_df)
  n_da <- nrow(protocol_deviation_administrative_df)
  
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
  
  
  vis <- kable(df_final, format="html", align='l', col.names = NULL) %>%
    add_header_above(header) %>%  
    add_indent(c(seq(n_disc) + 1, seq(1 + n_dsc + 1 + n_dp + 1 + n_da) + 1 + n_disc + 2, na.omit(c(dsc_indents, dp_indents, da_indents)))) %>% 
    row_spec(0, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1+ n_disc, extra_css = "border-bottom: 1px solid") %>% 
    row_spec(1 + n_disc + 1, extra_css = "border-bottom: 1px solid") %>%
    row_spec(1 + n_disc + 1 + 1 + 1 + n_dsc + 1 + n_dp + 1 + n_da, extra_css = "border-bottom: 1px solid") %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(vis)
}


#' Number of patients Ineligible by Top 5 reasons of Exclusion
#'
#' @description 
#' Visualizes the counts of ineligibility reasons (NOT NUMBER OF INELIGIBLE PARTICIPANTS) by site. The 
#' function will display a user-specified number of reasons broken down, and then collate the rest into
#' an "Other Reasons" column. Included are two columns depicting numbers of screened and ineligible
#' study participants.
#' 
#' Compare with ineligibility_reasons_info.
#'
#' @param analytic analytic data set that must include facilitycode, screened, ineligible, ineligibility_reasons
#' @param pre_screened when pre_screened is TRUE then uses pre-screening constructs
#' @param n_top_reasons is by default set to 5 but in case there are less than 5 reasons then as many 
#' columns would be reflected in the ineligibility table as reasons exist.
#' @param only_total hides site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' ineligibility_by_reasons("Replace with Analytic Tibble")
#' ineligibility_by_reasons("Replace with Analytic Tibble", n_top_reasons = 3, only_total = TRUE)
#' 
ineligibility_by_reasons <- function(analytic, pre_screened = FALSE, n_top_reasons = 5, only_total=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "screened", "ineligible", 'ineligibility_reasons'), 
    example_types = c('FacilityCode', 'Boolean', 'Boolean', 'Category-NS'))
  
  if (pre_screened) { 
    analytic <- analytic %>% 
      mutate(ineligibility_reasons = pre_ineligibility_reasons) %>% 
      mutate(screened = pre_screened) %>% 
      mutate(ineligible = pre_ineligible)
  }
  
  data <- analytic %>%
    select(study_id, facilitycode, screened, ineligible, ineligibility_reasons) %>%
    filter(screened == TRUE) 
  
  df <- data %>%
    select(study_id, facilitycode, ineligibility_reasons) %>%
    mutate(ineligibility_reasons = as.character(ineligibility_reasons)) %>%  
    column_unzipper('ineligibility_reasons', sep = '; ') %>%
    filter(!is.na(ineligibility_reasons))
  
  if (nrow(df) == 0) {
    output <- tibble(
      `Sites` = unique(analytic$facilitycode),  
      `No participant ineligible` = NA_character_
    )
    
    vis <- kable(output, format = "html", align = 'l') %>%
      kable_styling("striped", full_width = FALSE, position = "left")
  }
  
  if (nrow(df) > 0 ) {
    
    reason_data <- df %>%  
      boolean_column_counter() %>% 
      pivot_longer(everything()) %>% 
      arrange(desc(value)) %>% 
      filter(name != 'Other')
    
    n_reasons <- nrow(reason_data)
    n_top_reasons <- if (n_reasons >= n_top_reasons) n_top_reasons else max(1, n_reasons)
    
    reasons <- reason_data %>%
      slice(1:n_top_reasons) %>%
      pull(name)
    
    screened_total <- data %>% 
      select(study_id, screened, ineligible) %>% 
      boolean_column_counter() %>%
      mutate(Site = 'Total')
    
    total <- data %>%
      column_unzipper('ineligibility_reasons', sep = '; ') %>%
      boolean_column_counter() %>%
      mutate(otherreasons = rowSums(across(-c(all_of(reasons), screened, ineligible)))) %>% 
      select(-screened, -ineligible) %>%
      mutate(Site = 'Total') %>%
      left_join(screened_total) %>%
      select(Site, screened, ineligible, all_of(reasons), otherreasons)
    
    screened_total_sites <- data %>%
      select(facilitycode, screened, ineligible) %>%
      boolean_column_counter(groups = 'facilitycode') %>%
      rename(Site = facilitycode)
    
    sites <- data %>%
      column_unzipper('ineligibility_reasons', sep = '; ') %>%
      boolean_column_counter(groups = 'facilitycode') %>%
      mutate(otherreasons = rowSums(across(-c(all_of(reasons), screened, ineligible, facilitycode)))) %>%
      rename(Site = facilitycode) %>%
      select(-screened, -ineligible) %>%
      left_join(screened_total_sites) %>%
      select(Site, screened, ineligible, all_of(reasons), otherreasons)
    
    output <- bind_rows(total, sites) %>% 
      rename(Screened = screened,
             Ineligible = ineligible,
             `Other Reasons` = otherreasons) %>% 
      arrange(desc(Screened)) %>% 
      mutate(Ineligible = format_count_percent(Ineligible, Screened)) %>%
      mutate(across(4:(n_top_reasons+3), ~ format_count_percent(.x, Screened))) %>%
      mutate(`Other Reasons` = format_count_percent(`Other Reasons`, Screened))
    
    if(pre_screened){
      output <- output %>% 
        rename("Pre-Screened" = Screened,
               "Pre-Ineligible" = Ineligible)
    }
    
    top_n_header_text <- paste0("Top ", n_top_reasons, " Ineligibility Reasons")
    
    header_names <- c(" " = 3, top_n_header_text = n_top_reasons, " " = 1)
    
    names(header_names)[2] <- top_n_header_text
    
    if(only_total){
      output <- output %>% filter(Site=="Total")
    }
    
    vis <- kable(output, format = "html", align = 'l') %>%
      add_header_above(header_names) %>%
      kable_styling("striped", full_width = FALSE, position = "left")
  }
  
  return(vis)
}


#' Status of IRB approvals and certification by site
#'
#' @description 
#' Visualizes the sites for a given study and their dates of 
#' local, DOD, and METRC certifications. This function outputs 5 columns, Facility, Local or sIRB approval date, DoD approval date, 
#' certified by MCC to start screening, Number of days certified. To run this visualization a study needs qa site_certification_data long file.
#'
#' @param analytic This is the analytic data set that must include site_certification_data
#' @param exclude_local_irb whether Local (or iSRB) column is in output, defaults to false
#'
#' @return html table
#' @export
#'
#' @examples
#' certification_date_data("Replace with Analytic Tibble")
#' certification_date_data("Replace with Analytic Tibble", exclude_local_irb = TRUE)
#' 
certification_date_data <- function(analytic, exclude_local_irb=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = "site_certification_data",
    example_types = "FacilityCode;Date;Date;Date;NamedCategory['2 days' '3 days' '4 days' '40 days']") 
  
  df <- analytic %>% 
    select(site_certification_data) %>%
    unique()
  
  date_today <- Sys.Date()
  
  cols <- c('Facility', 'Local (or sIRB)  Approval Date', 'DoD Approval Date',
            'Certified by MCC to Start Screening', 
            paste0('Days Number of Days Certified (as of ', as.character(date_today), ')'))
  
  site_data <- df %>%
    separate(site_certification_data, cols, sep = ';') %>%
    filter(!is.na(Facility))
  
  site_data <- rbind(site_data %>% filter(.[[5]]!="NA days"),site_data %>% filter(.[[5]]=="NA days"))
  
  if(exclude_local_irb){
    cols <- cols[-2] 
    site_data <- site_data %>% 
      select(all_of(cols))
  }
  
  site_data <- site_data %>% 
    arrange(`Certified by MCC to Start Screening`)
  
  vis <- kable(site_data, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}



#' Complications by severity and relatedness
#'
#' @description 
#' Visualizes the complication_data long file. Data is shown for each grade and type of complication,
#' as well as the number of study participants who experienced this complication (in brackets). If a
#' study is documenting unique or obscure complications that are not in the example table, then an update 
#' to this function is necessary for the study. The complications shown in the example table will always
#' be present, even if the proposed study is not tracking them.
#' 
#' Grade is determined by the severity column, with 2,1 being Mild or Moderate and 3, 4 being Severe
#' and Life-threatening, respectively. Notably, a fatal complication results in a grade Unknown.
#'
#' @param analytic analytic data set that must include complication_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' complications_by_severity_relatedness("Replace with Analytic Tibble")
#' 
complications_by_severity_relatedness <- function(analytic){
  analytic <- if_needed_generate_example_data(
    "Replace with Analytic Tibble",
    example_constructs = "complication_data",
    example_types = "(';new_row: ', '|')FollowupPeriod|Date|
      NamedCategory['Superficial-infection' 'Deep-Infection' 'Deep-Infection, Not Involving Bone' 'Deep-Infection, Septic Joint' 'Non-Union' 'Malunion' 'Loss of limb/amputation' 'Fixation failure' 'Peri-implant Fracture' 'Reaction to Hardware' 'Wound Dehiscence' 'Wound Seroma/Hematoma' 'Flap failure' 'Tendon Injury' 'Delayed Wound Healing' 'Cellulitis' 'DVT/PE' 'Joint Arthritis' 'Other' 'Other' 'Other' 'Other' 'Moderate' 'Mild' 'Life-threatening or disabling' 'Severe and Undesirable' 'Fatal']|
      Character|Character|Date|
      NamedCategory['Definitely related' 'Probably related' 'Possibly related' 'Unlikely related' 'Unrelated' 'Don't know']|NamedCategory['Moderate' 'Mild' 'Life-threatening or disabling' 'Severe and Undesirable' 'Fatal']|NamedCategory['Operative' 'Non-operative' 'No treatment']|
      NamedCategory['New' 'Previous']|Character") 
  
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
              Unrelated_c = sum(Unrelated, na.rm = T), Unknown_c = sum(Unknown, na.rm = T), #TODO: Pretty sure this should be `Don't Know`, not Unknown
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
    arrange(severity, complications) %>%
    select(-severity)
  
  colnames(output)[1] <- " "
  
  index_vec <- c(" " = 1, "Grade 4" = 9, "Grade 3"= 9,"Grade 2,1"= 9, "Grade Unknown"= 9)
  subindex_vec <- c(" " = 1, "Infection" = 3, " " = 6, "Infection" = 3, " " = 6, "Infection" = 3, " " = 6,
                    "Infection" = 3, " " = 6)
  table_raw<- kable(output, format="html", align='l') %>%
    pack_rows(index = index_vec, label_row_css = "text-align:left") %>% 
    pack_rows(index = subindex_vec,label_row_css = "text-align:left;padding-left: 2em;", bold = FALSE) %>% 
    row_spec(1, extra_css = "border-bottom: 1px solid") %>% 
    kable_styling("striped", full_width = F, position="left") 
  
  return(table_raw)
}



#' Nonunion surgery outcome
#'
#' @description 
#' Visualizes the checks at 3 Months and 12 Months across the count of Non-Union at those timepoints.
#'
#' @param analytic This is the analytic data set that must include enrolled, 
#' followup_expected_3mo, followup_expected_12mo, nonunion_90days,  nonunion_1yr
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' nonunion_surgery_outcome("Replace with Analytic Tibble")
#' 
nonunion_surgery_outcome <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "followup_expected_3mo", "followup_expected_12mo",
                           "nonunion_90days", "nonunion_1yr"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean"))
  
  df <- analytic %>% 
    select(enrolled, followup_expected_3mo, nonunion_90days, followup_expected_12mo, nonunion_1yr) %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    filter(enrolled) %>% 
    boolean_column_counter() %>% 
    mutate(nonunion_90days = format_count_percent(nonunion_90days, followup_expected_3mo),
           nonunion_1yr = format_count_percent(nonunion_1yr, followup_expected_12mo))
  
  colname <- c("Enrolled", "Expected Three Month", "90 Day Non-Union", "Expected Twelve Month", "1 Year Non-Union")
  
  table<- kable(df, format="html", align='l', col.names = colname) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}




#' Injury Characteristics
#'
#' @description This function visualizes the certain injury characteristics for study participants study injuries, 
#' ross reference the potential usage of this visualization with injury_characteristics
#'
#' @param analytic This is the analytic data set that must include enrolled, injury_classification_ankle_ao, injury_at_work, injury_in_battle, 
#' injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type
#
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' injury_characteristics_by_alternate_constructs("Replace with Analytic Tibble")
#' 
injury_characteristics_by_alternate_constructs <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('injury_classification_ankle_ao', 'injury_at_work', 'injury_in_battle', 
                           'injury_in_blast', 'injury_date', 'injury_mechanism', 
                           'injury_side', 'injury_classification_tscherne', 'injury_type',
                           'enrolled'), 
    example_types = c('Category', 'Boolean', 'Boolean', "Boolean", 'Date', 'Category', 
                      'Category', 'Category', 'Category', 'Boolean'))
  
  df <- analytic %>% 
    select(enrolled, injury_classification_ankle_ao, injury_at_work, injury_in_battle, 
           injury_in_blast, injury_date, injury_mechanism, injury_side, injury_classification_tscherne, injury_type) %>% 
    filter(enrolled)
  
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
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  
  vis <- kable(df_final, format="html", align='l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Type of Injury' = nrow(type_df), 'Work Related Injury' = nrow(work_df), 'Battlefield Injury' = nrow(battle_df), 
                        'Blast Injury' = nrow(blast_df), 'Side of Study Injury' = nrow(side_df), 
                        'Tscherne Classification' = nrow(tscherne_df), 'AO Classification' = nrow(ao_df)), label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}


#' Generic Characteristics
#'
#' @description 
#' Visualize basic count statistics for a number of constructs. Missing values are 
#' given the value "Missing."
#' 
#' For other relevant characteristics counting visualizations, please see: baseline_characteristics_percent, baseline_characteristics_percent_nm
#'
#' @param analytic This is the analytic data set 
#' @param constructs The constructs to run statistics from
#' @param names_vec The names of the constructs in the final visualization. Pass NA to attach a construct to the previous group's header without creating a new top border.
#' @param filter_cols The columns to filter the the data by (for totals and missing counts)
#' @param titlecase Changes construct values to Title Case
#' @param splits Splits the constructs if they are lists like "test_one,test_two" into two rows then counts them
#' @param subcategory_constructs This allows a characteristic to have a construct as a sub category, 
#' must be empty or specify a subcategory construct (or NA) for each construct (length of constructs == length of subcategory_constructs)
#' @param bottom_order_levels A vector of category names (e.g., "Missing", "Refused") to force to the bottom of the table, maintaining their order. Defaults to "Missing".
#' @param mean_sd A vector of construct names. If a construct is included here, it will be displayed as "Mean [SD]" with its calculated values, instead of categorical counts.
#' @param include_overall When TRUE and subcategory_constructs is used, an "All Sites" block computed over every subcategory together is shown before the per-subcategory blocks.
#' @param collapse_other_entries single flag or per-construct vector; TRUE collapses
#' free-text Other entries for that construct before counting - as a whole value for
#' an unsplit construct, and inside the list for a split one
#'
#' @return html table
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' generic_characteristics("Replace with Analytic Tibble", constructs="stages", names_vec="Stages")
#' }
generic_characteristics <- function(analytic, constructs = c(), names_vec = c(), 
                                    filter_cols = c("enrolled"), titlecase = FALSE, splits=NULL,
                                    subcategory_constructs = c(), bottom_order_levels = c("Missing"),
                                    mean_sd = c(), include_overall = FALSE,
                                    collapse_other_entries = FALSE){
  
  out <- NULL
  index_vec <- c()
  sub_index_vec <- c()
  sub_bold_index_vec <- c()
  has_border <- c()
  
  if(is.null(splits)){
    splits <- rep(NA, length(constructs))
  } else{
    if(length(splits) == 1) {
      splits <- rep(splits, length(constructs))
    }
  }

  if(length(collapse_other_entries) == 1) {
    collapse_other_entries <- rep(collapse_other_entries, length(constructs))
  }
  for (coe_i in seq_along(constructs)) {
    # A construct absent from the data is left for the construct selection below,
    # which names the missing column in its error instead of a recycling failure.
    if (isTRUE(collapse_other_entries[coe_i]) && constructs[coe_i] %in% names(analytic)) {
      # An unsplit construct's Other free text may itself contain the split
      # character, so it is collapsed as a whole value; a split construct
      # collapses the Other terms inside its list.
      analytic[[constructs[coe_i]]] <- if (is.na(splits[coe_i])) {
        collapse_other(analytic[[constructs[coe_i]]])
      } else {
        collapse_other_multi(analytic[[constructs[coe_i]]])
      }
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
          select(study_id, all_of(c(constructs,subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]))
      } else {
        inner_analytic <- analytic %>%
          filter(!!sym(filter_cols[i])) %>%
          select(study_id, all_of(c(constructs,subcategory_constructs)[!is.na(c(constructs,subcategory_constructs))]))
      }
    }
    total <- nrow(inner_analytic)
    
    if (construct %in% mean_sd) {
      vec <- suppressWarnings(as.numeric(inner_analytic[[construct]]))
      inner <- tibble::tibble(temp = "Mean [SD]", percentage = format_mean_sd(vec), header = name_str)
      
      if (is.na(name_str) || name_str == "") {
        if (length(index_vec) > 0) {
          index_vec[length(index_vec)] <- index_vec[length(index_vec)] + 1
        } else {
          new <- 1; names(new) <- " "; index_vec <- c(index_vec, new); has_border <- c(has_border, FALSE)
        }
      } else {
        new <- 1; names(new) <- paste0(name_str, ' (n=', total, ')'); index_vec <- c(index_vec, new); has_border <- c(has_border, FALSE)
      }
      
      if (is.null(out)) out <- inner else out <- rbind(out, inner)
      next
    }
    
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
    
    non_bottom_temps <- sort(unique(inner$temp[!inner$temp %in% bottom_order_levels]))
    
    numeric_temps <- suppressWarnings(as.numeric(non_bottom_temps))
    is_numeric <- !is.na(numeric_temps)
    
    numeric_sort_list <- non_bottom_temps[is_numeric] %>% 
      as.numeric() %>% 
      sort() %>% 
      as.character()
    
    non_numeric_sort_list <- sort(non_bottom_temps[!is_numeric])
    
    custom_levels <- c(numeric_sort_list, non_numeric_sort_list, bottom_order_levels)
    
    if(!is.na(sub_construct)){
      if (include_overall) {
        inner <- bind_rows(inner %>% mutate(sub_temp = "All Sites"), inner)
      }
      sub_cats <- sort(unique(inner$sub_temp))
      sub_cats <- c(sub_cats[!sub_cats %in% bottom_order_levels], intersect(bottom_order_levels, sub_cats))
      if (include_overall) sub_cats <- c("All Sites", sub_cats[sub_cats != "All Sites"])
      row_count <- ifelse(is.null(out),0,nrow(out))
      new_row_count <- 0
      for(sub_cat in sub_cats){
        category_df <- inner %>% 
          filter(sub_temp==sub_cat) %>% 
          select(-sub_temp) %>% 
          group_by(temp) %>% 
          count(temp)
        
        category_tot <- sum(category_df$n)
        category_df <- category_df %>% 
          mutate(percentage = format_count_percent(n,category_tot))
        tot_df <- tibble(temp=sub_cat,percentage=format_count_percent(category_tot, total),header=name_str)
        
        category_df <- category_df  %>% 
          select(-n) %>%
          mutate(header = name_str) %>%
          mutate(temp = factor(temp, levels = custom_levels)) %>% 
          arrange(temp) %>%
          mutate(temp = as.character(temp))
        
        
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
      names(new) <- paste0(name_str, ' (n=', total, ')')
      index_vec <- c(index_vec, new)
      has_border <- c(has_border, TRUE)
    } else{
      inner <- inner %>% 
        group_by(temp) %>% 
        count(temp) %>% 
        mutate(percentage = format_count_percent(n, total)) %>% 
        select(-n) %>%
        mutate(header = name_str) %>%
        mutate(temp = factor(temp, levels = custom_levels)) %>% 
        arrange(temp) %>%
        mutate(temp = as.character(temp))
      
      if (titlecase) {
        inner <- inner %>%
          mutate(temp = str_to_title(temp))
      }
      
      new <- nrow(inner)
      names(new) <- paste0(name_str, ' (n=', total, ')')
      index_vec <- c(index_vec, new)
      has_border <- c(has_border, TRUE)
      
      if (is.null(out)) {
        out <- inner
      } else {
        out <- rbind(out, inner)
      }
    }
  }
  out <- out %>%
    select(-header)
  
  all_group_starts <- if(length(index_vec) > 1) c(1, cumsum(index_vec[1:(length(index_vec)-1)]) + 1) else c(1)
  border_rows <- all_group_starts[has_border]
  
  if(is_empty(sub_bold_index_vec)){
    vis <- kable(out, format="html", align='l', col.names = c('', '')) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      { if(length(border_rows) > 0) row_spec(., border_rows, extra_css = "border-top: 1px solid") else . } %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left", escape = FALSE) %>% 
      kable_styling("striped", full_width = F, position="left")
  } else{
    vis <- kable(out, format="html", align='l', col.names = c('', '')) %>%
      add_indent(c(seq(nrow(out)))) %>% 
      add_indent(sub_index_vec) %>% 
      row_spec(sub_bold_index_vec, bold = TRUE) %>% 
      { if(length(border_rows) > 0) row_spec(., border_rows, extra_css = "border-top: 1px solid") else . } %>%  
      pack_rows(index = index_vec, label_row_css = "text-align:left", escape = FALSE) %>% 
      kable_styling("striped", full_width = F, position="left")
  }
  return(vis)
}

#' Refusal reasons by each site
#'
#' @description This function visualizes the reasons of refusal, total refused and total screened by each site.
#'
#' @param analytic This is the analytic data set that must include facilitycode, screened, refused, refused_reason
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' refusal_reasons_by_site("Replace with Analytic Tibble")
#' 
refusal_reasons_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "screened", "refused", 'refused_reason'), 
    example_types = c('FacilityCode', 'Boolean', 'Boolean', 'Category'))

  df <- analytic %>% 
    select(facilitycode, screened, refused, refused_reason) %>% 
    filter(screened == TRUE) 
  
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
  
  totals_reasons <- reasons %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    mutate(facilitycode = "Total")
  
  totals_screened <- screened_df %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    mutate(facilitycode = "Total")
  
  totals_refused <- refused_df %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    mutate(facilitycode = "Total")
  
  totals <- left_join(totals_reasons, totals_screened) %>% left_join(totals_refused)
  
  exclude_columns <- c("facilitycode", "screen_n", "refuse_n")
  
  
  df_final <- left_join(reasons, screened_df) %>% left_join(refused_df) %>% 
    mutate_all(~ ifelse(is.na(.), 0, .)) %>% 
    rbind(totals) %>% 
    arrange(ifelse(facilitycode == "Total", 0, 1)) %>% 
    mutate(across(-one_of(exclude_columns),
                  ~ format_count_percent(.x, refuse_n),
                  .names = "{col}_percentage")) %>% 
    select(ends_with("_percentage"), one_of(exclude_columns)) %>% 
    rename_with(~ sub("_percentage$", "", .), ends_with("_percentage")) %>% 
    select(one_of(exclude_columns), everything()) %>%
    arrange(desc(refuse_n)) %>% 
    select(-contains("Other"), -contains("Unknown"), contains("Other"), contains("Unknown")) %>% 
    rename(`Screened, to date` = screen_n,
           `Refused, to date` = refuse_n, 
           `Clinical Site` = facilitycode) 
  
  output <- kable(df_final, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(row = 1, bold = TRUE)
  
  return(output)
}

#' Other reason of refusal by each site
#'
#' @description 
#' Returns a table of descriptions of all of the "Other" reasons given for refusal to participate.
#' 
#' See also the complementary table: refusal_reasons_by_site
#'
#' @param analytic analytic data set that must include study_id, facilitycode, screened_date, 
#' refused_reason_other
#'
#' @return html table
#' @export
#'
#' @examples
#' other_reason_refusal_by_site("Replace with Analytic Tibble")
#' 
other_reason_refusal_by_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened_date", "refused_reason_other", "facilitycode"),
    example_types = c("Date", "Character", "FacilityCode")) 
  
  df1 <- analytic %>%  select(study_id, facilitycode, screened_date, refused_reason_other) %>% 
    filter(!is.na(refused_reason_other)) %>% 
    rename(`Clinical Site` = facilitycode,
           `Screened Date` = screened_date,
           `"Other" reason of refusal` = refused_reason_other,
           `Study_ID` = study_id)
  
  output <- kable(df1, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}

#' Not enrolled for other reasons
#'
#' @description Visualizes the list of study_ids who were screened however were not enrolled for reasons labeled 'other'.
#' 
#' See also the complementary table: not_enrolled_reason
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, able_to_participate, 
#' nonparticipation_text_given, constraint_noconsent, constraint_admin, constraint_other, constraint_other_txt, screened
#'
#' @return html table
#' @export
#'
#' @examples
#' not_enrolled_for_other_reasons("Replace with Analytic Tibble")
#' 
not_enrolled_for_other_reasons <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "able_to_participate", "nonparticipation_text_given", 
                           "constraint_noconsent", "constraint_admin", "constraint_other", 
                           "constraint_other_txt", "screened"), 
    example_types = c("FacilityCode", "Boolean", "Character", 
                      "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean"))
  
  df1 <- analytic %>%  select(study_id, facilitycode, able_to_participate, nonparticipation_text_given, 
                              constraint_noconsent, constraint_admin, constraint_other, constraint_other_txt, screened) %>% 
    filter(screened) %>% 
    filter(constraint_admin == TRUE | constraint_noconsent == TRUE | constraint_other == TRUE | !is.na(nonparticipation_text_given)) %>% 
    select(-screened) %>% 
    mutate(constraint_noconsent = ifelse(constraint_noconsent, "Yes", "No")) %>% 
    mutate(constraint_admin = ifelse(constraint_admin, "Yes", "No")) %>% 
    mutate(constraint_other = ifelse(constraint_other, "Yes", "No")) %>% 
    rename(`Clinical Site` = facilitycode,
           `Able to participate` = able_to_participate,
           `Reason for nonparticpation` = nonparticipation_text_given,
           `Constraint: No consent given` = constraint_noconsent,
           `Constraint: Administrative reason` = constraint_admin,
           `Constraint: Other` = constraint_other,
           `Other constraint reason` = constraint_other_txt,
           `Study_ID` = study_id)
  
  
  output <- kable(df1, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}


#' Fracture characteristics
#'
#' @description 
#' This function visualizes fracture characteristics, broken down by tibial plateau or pilon, 
#' and then closed or open fracture with tscherne grades and gustilo types respectively. Percentages
#' are frome within each type of fracture.
#'
#' @param analytic analytic data set that must include study_id, enrolled, fracture_type, injury_gustilo, 
#' injury_classification_tscherne
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' fracture_characteristics("Replace with Analytic Tibble")
#' 
fracture_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "fracture_type","injury_gustilo", "injury_classification_tscherne"),
    example_types = c("Boolean", "Category", "Category", "Category")) 
  
  df <- analytic %>% select(study_id, enrolled, fracture_type, injury_gustilo, injury_classification_tscherne) %>% 
    filter(enrolled) %>% 
    mutate(closed = ifelse(!is.na(injury_classification_tscherne), TRUE, FALSE)) %>% 
    mutate(open = ifelse(!is.na(injury_gustilo), TRUE, FALSE))
  
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
  
  cnames <- c(' ', paste('n = ', total))
  header <- c(1,1)
  names(header)<-cnames
  
  n_closed <- nrow(closed)
  n_open <- nrow(open)
  n_frac <- nrow(fracture_type)
  n_tscherne <- nrow(tscherne)
  n_gustilo <- nrow(gustilo)
  
  vis <- kable(df_final, format="html", align='l', col.names = NULL) %>%
    add_header_above(header) %>%  
    pack_rows(index = c('Fractured Bone' = nrow(fracture_type), 'Fracture Type' = (nrow(closed) + nrow(tscherne) + nrow(open) + nrow(gustilo))), label_row_css = "text-align:left") %>%
    add_indent(c(seq(n_tscherne) + n_frac + n_closed, seq(n_gustilo) + n_frac + n_closed + n_open + n_tscherne)) %>% 
    kable_styling("striped", full_width = F, position="left") 
  return(vis)
}

#' enrollment_by_site tobra and sextant (var discontinued)
#'
#' @description 
#' Visualizes the number of subjects enrolled, not enrolled etc, with parameter specifications to include 
#' more columns. 
#' 
#' For other enrollment by site visualizations that may better fit your study, please look at: enrollment_by_site, 
#' enrollment_by_site_last_days_var_disc, enrollment_status_by_site, enrollment_status_by_site_var_discontinued
#'
#' @param analytic This is the analytic data set that must include screened, eligible, refused, not_consented, 
#' not_randomized, consented_and_randomized, enrolled, site_certification_date, facilitycode, screened_date, 
#' consented, randomized, consent_date, discontinued
#' @param days the number of last days to include in the last days summary section of the table
#' @param discontinued this is a meta construct where you can specify your discontinued construct like 
#' 'discontinued' or 'adjudicated_discontinued' (defaults to 'discontinued')
#' @param discontinued_colname this determines the label applied to the discontinued column of your choosing 
#' (defaults to 'Discontinued')
#' @param include_exclusive_safety_set this is a toggle that will include a exclusive_safety_set construct 
#' if you want it included (defaults to FALSE)
#' @param average if days argument is set to something other than 0, will return the average over the
#' time period specified for the length of the study
#' @param cumulative_data whether to include the final counts of the study statuses
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble")
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", days = 20, average = TRUE)
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", days = 20, average = FALSE)
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", discontinued_colname = 'HERE!')
#' enrollment_by_site_last_days_var_disc("Replace with Analytic Tibble", average = TRUE)
#' print("Note this call does not work, as average is set to true and no days are specified")
#' 
enrollment_by_site_last_days_var_disc <- function(analytic, days = 0, 
                                                  discontinued="discontinued", 
                                                  discontinued_colname="Discontinued", 
                                                  include_exclusive_safety_set=FALSE, 
                                                  average = FALSE, 
                                                  cumulative_data = TRUE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "refused", "consented", "enrolled", "randomized",
                           "not_consented", "site_certification_date", "facilitycode", "consent_date",
                           "not_randomized", "discontinued", "consented_and_randomized", "screened_date", "exclusive_safety_set"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Date", "FacilityCode", "Date", "Boolean", "Boolean",
                      "Boolean", "Date", "Boolean"))
  
  if(include_exclusive_safety_set){
    df <- analytic %>% 
      select(screened, eligible, refused, not_consented, not_randomized, consented_and_randomized, enrolled, site_certification_date, 
             facilitycode, all_of(discontinued), screened_date, exclusive_safety_set)
  } else{
    df <- analytic %>% 
      select(screened, eligible, refused, not_consented, not_randomized, consented_and_randomized, enrolled, site_certification_date, 
             facilitycode, all_of(discontinued), screened_date)
  }
  
  colnames(df)[10] <- "discontinued"
  
  last_days <- Sys.Date() - days
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility)) %>% 
    mutate(weeks_site_certified = site_certified_days/7)
  
  if(include_exclusive_safety_set){
    df_1st <- df %>% 
      group_by(Facility) %>% 
      summarize('Days Certified' = site_certified_days[1], 
                Screened = sum(screened), 
                Eligible = sum(eligible), 
                Refused = sum(refused[eligible == TRUE]), 
                'Not Consented' = sum(not_consented[eligible == TRUE]), 
                cnr = sum(consented_and_randomized[eligible == TRUE])) 
    
    df_2nd <- df %>% 
      group_by(Facility) %>% 
      summarize('Discontinued' = sum(discontinued[eligible == TRUE & consented_and_randomized == TRUE]), 
                "Enrolled" = sum(enrolled[eligible == TRUE & consented_and_randomized == TRUE]), 
                'Safety Set' = sum(exclusive_safety_set[eligible == TRUE & consented_and_randomized == TRUE])) %>% 
      select(Facility, Discontinued, Enrolled, `Safety Set`)
    
  } else{
    df_1st <- df %>% 
      group_by(Facility) %>% 
      summarize('Days Certified' = site_certified_days[1], 
                Screened = sum(screened), Eligible = sum(eligible), 
                Refused = sum(refused[eligible == TRUE]), 
                'Not Consented' = sum(not_consented[eligible == TRUE]), 
                cnr = sum(consented_and_randomized[eligible == TRUE])) 
    
    df_2nd <- df %>% 
      group_by(Facility) %>% 
      summarize('Discontinued' = sum(discontinued[eligible == TRUE & consented_and_randomized == TRUE]), 
                "Enrolled" = sum(enrolled[eligible == TRUE & consented_and_randomized == TRUE])) %>% 
      select(Facility, Discontinued, Enrolled)
  }
  
  table_raw <- left_join(df_1st, df_2nd, by = 'Facility')
  
  facilities <- df %>% 
    select(Facility) %>% 
    unique()
  
  last_day_df <- facilities
  
  for(last_day in last_days){
    new_last_day_df <- df %>% 
      mutate(screened_date = as.Date(screened_date)) %>% 
      mutate(screened_last = ifelse(screened_date > last_day, TRUE, FALSE)) %>% 
      mutate(eligible_last = ifelse(screened_last, eligible, FALSE)) %>% 
      mutate(enrolled_last = ifelse(screened_last, enrolled, FALSE)) %>% 
      select(Facility, screened_last, eligible_last, enrolled_last) %>% 
      group_by(Facility) %>% 
      summarize('last_days_Screened' = sum(screened_last, na.rm = T),
                'last_days_Eligible' = sum(eligible_last, na.rm = T),
                'last_days_Enrolled' = sum(enrolled_last, na.rm = T))
    
    last_day_df <- left_join(last_day_df, new_last_day_df, by = 'Facility')
  }
  
  by_week <- df %>%
    filter(!is.na(weeks_site_certified)) %>% 
    select(Facility, screened, enrolled, weeks_site_certified) %>% 
    group_by(Facility) %>% 
    summarize(
      Screened2 = round(sum(screened, na.rm = TRUE) / first(weeks_site_certified), 2),
      Enrolled2 = round(sum(enrolled, na.rm = TRUE) / first(weeks_site_certified), 2))
  
  weekly <- left_join(facilities, by_week, by = 'Facility')
  
  almost <- left_join(last_day_df, weekly, by = 'Facility')
  
  sum_days_certified <- sum(table_raw$`Days Certified`, na.rm=T)
  
  final <- left_join(almost, table_raw, by = 'Facility') %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,sum_days_certified,`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(across(starts_with(c("last_days_Eligible", "last_days_Enrolled")), 
      ~ format_count_percent(.x, 
                             get(str_replace(cur_column(), 
                                             "^(last_days_Eligible|last_days_Enrolled)(.*)$", 
                                             "last_days_Screened\\2"))))) %>% 
    mutate(`Discontinued (% randomized)` = format_count_percent(Discontinued, cnr)) %>% 
    mutate(`Eligible & Enrolled (% randomized)` = format_count_percent(Enrolled, cnr)) %>% 
    mutate(`Consented & Randomized (% eligible)` = format_count_percent(cnr, Eligible)) %>% 
    mutate(`Refused (% eligible)` = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Enrolled for 'Other' Reasons (% eligible)` = format_count_percent(`Not Consented`, Eligible)) %>% 
    mutate(`Eligible (% screened)` = format_count_percent(Eligible, Screened)) 
  
  if (include_exclusive_safety_set) {
    final <- final %>%
      mutate(`Safety Set` = format_count_percent(`Safety Set`, cnr))
  }
  
  total_row <- final %>% 
    slice_head(n=1)
  
  if(include_exclusive_safety_set){
    last <- bind_rows(final, total_row) %>% 
      slice_tail(n=-1) %>% 
      select(-Eligible, -Enrolled, -Refused, -`Not Consented`, -cnr, -Discontinued) %>% 
      select(Facility, starts_with('last_days'), Screened2, Enrolled2, Screened, `Eligible (% screened)`, `Refused (% eligible)`, `Not Enrolled for 'Other' Reasons (% eligible)`, 
             `Consented & Randomized (% eligible)`, `Discontinued (% randomized)`, `Safety Set`, `Eligible & Enrolled (% randomized)`)
    
    colnames(last) <- c('Facility', rep(c('Screened', 'Eligible (% screened)', 'Enrolled (% screened)'), length(days)), "Screened", 'Enrolled', 'Screened', 'Eligible (% screened)', 'Refused (% eligible)', 'Not Enrolled for `Other` Reasons (% eligible)', 
                        'Consented & Randomized (% eligible)', paste(discontinued_colname, '(% randomized)'), 'Not Enrolled Safety Set (% randomized)', 'Eligible & Enrolled (% randomized)')
    
    header_num <- c(1,rep(3, length(days)),2,8)
    header_names <- c(" ", paste("Last", days, " Days"), paste("Average per week"), paste("Cumulative", "to date"))
    names(header_num) <- header_names
  } else{
    last <- bind_rows(final, total_row) %>% 
      slice_tail(n=-1) %>% 
      select(-Eligible, -Enrolled, -Refused, -`Not Consented`, -cnr, -Discontinued) %>% 
      select(Facility, starts_with('last_days'), Screened2, Enrolled2, Screened, `Eligible (% screened)`, `Refused (% eligible)`, `Not Enrolled for 'Other' Reasons (% eligible)`, 
             `Consented & Randomized (% eligible)`, `Discontinued (% randomized)`, `Eligible & Enrolled (% randomized)`)
    
    colnames(last) <- c('Facility', rep(c('Screened', 'Eligible (% screened)', 'Enrolled (% screened)'), length(days)), "Screened", 'Enrolled', 'Screened', 'Eligible (% screened)', 'Refused (% eligible)', 'Not Enrolled for `Other` Reasons (% eligible)', 
                        'Consented & Randomized (% eligible)', paste(discontinued_colname, '(% randomized)'), 'Eligible & Enrolled (% randomized)')
    
    header_num <- c(1,rep(3, length(days)),2,7)
    header_names <- c(" ", paste("Last", days, " Days"), paste("Average per week"), paste("Cumulative", "to date"))
    names(header_num) <- header_names
  }
  
  if(length(days) == 1){
    
    if(days == 0){
      last <- last[, c(1, seq(from=5, to=ncol(last)))]
      
      if(average == FALSE){
        last <- last[, c(1, seq(from=4, to=ncol(last)))]
        header_num <- header_num[c(1, 4)]
      }
    } else {
      if(average == FALSE){
        last <- last[, c(1, 2, 3, 4, seq(from=7, to=ncol(last)))]
        header_num <- header_num[c(1, 2, 4)]
      }
    }
  } else {
    if(average == FALSE){
      last <- last[, c(seq(from = 1, to = 3*length(days)+1), seq(3*length(days)+4, to=ncol(last)))]
      header_num <- header_num[c(seq(from=1, to=length(days)+1), length(header_num))]
    }
  }
  
  if(cumulative_data == FALSE){
    if(average == TRUE){
      last <- last[, c(seq(from = 1, to = (ncol(last) - 7)))]
      header_num <- header_num[c(seq(from = 1, to = (length(days) + 2)))]
    }else{
      last <- last[, c(seq(from = 1, to = (ncol(last) - 7)))]
      header_num <- header_num[c(seq(from = 1, to = (length(days) + 1)))]
    }
  }
  
  table <- kable(last, format="html", align='l') %>%
    add_header_above(header_num) %>%
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(nrow(last), bold = TRUE)
  
  return(table)
}

#' enrollment_by_site_last_days_var_disc_i
#'
#' @description 
#' Visualizes the screening and eligibility status of subjects (Left half of original table).
#' Includes: Screened, Eligible, Refused, and Not Consented.
#'
#' @param analytic Analytic data set.
#' @param days Number of last days to include.
#' @param average Return average over the time period.
#' @param cumulative_data Include final counts.
#'
#' @return An HTML table.
#' @export
enrollment_by_site_last_days_var_disc_i <- function(analytic, days = 0, 
                                         average = FALSE, 
                                         cumulative_data = TRUE){
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "refused", 
                           "not_consented", "site_certification_date", 
                           "facilitycode", "screened_date"), 
    example_types = c("Boolean", "Boolean", "Boolean", 
                      "Boolean", "Date", 
                      "FacilityCode", "Date"))
  
  df <- analytic %>% 
    select(screened, eligible, refused, not_consented, site_certification_date, 
           facilitycode, screened_date) %>%
    arrange(facilitycode)
  
  last_days <- Sys.Date() - days
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility)) %>% 
    mutate(weeks_site_certified = site_certified_days/7)
  
  df_1st <- df %>% 
    group_by(Facility) %>% 
    summarize('Days Certified' = site_certified_days[1], 
              Screened = sum(screened), Eligible = sum(eligible), 
              Refused = sum(refused[eligible == TRUE]), 
              'Not Consented' = sum(not_consented[eligible == TRUE])) 
  
  table_raw <- df_1st
  
  facilities <- df %>% 
    select(Facility) %>% 
    unique()
  
  last_day_df <- facilities
  
  for(last_day in last_days){
    new_last_day_df <- df %>% 
      mutate(screened_date = as.Date(screened_date)) %>% 
      mutate(screened_last = ifelse(screened_date > last_day, TRUE, FALSE)) %>% 
      mutate(eligible_last = ifelse(screened_last, eligible, FALSE)) %>% 
      select(Facility, screened_last, eligible_last) %>% 
      group_by(Facility) %>% 
      summarize('last_days_Screened' = sum(screened_last, na.rm = T),
                'last_days_Eligible' = sum(eligible_last, na.rm = T))
    
    last_day_df <- left_join(last_day_df, new_last_day_df, by = 'Facility')
  }
  
  by_week <- df %>%
    filter(!is.na(weeks_site_certified)) %>% 
    select(Facility, screened, weeks_site_certified) %>% 
    group_by(Facility) %>% 
    summarize(
      Screened2 = round(sum(screened, na.rm = TRUE) / first(weeks_site_certified), 2))
  
  weekly <- left_join(facilities, by_week, by = 'Facility')
  
  almost <- left_join(last_day_df, weekly, by = 'Facility')
  
  sum_days_certified <- sum(table_raw$`Days Certified`, na.rm=T)
  
  final <- left_join(almost, table_raw, by = 'Facility') %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,sum_days_certified,`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(across(starts_with("last_days_Eligible"), 
                  ~ format_count_percent(.x, 
                                         get(str_replace(cur_column(), 
                                                         "^last_days_Eligible(.*)$", 
                                                         "last_days_Screened\\1"))))) %>% 
    mutate(`Refused (% eligible)` = format_count_percent(Refused, Eligible)) %>% 
    mutate(`Not Enrolled for Other Reasons (% eligible)` = format_count_percent(`Not Consented`, Eligible)) %>% 
    mutate(`Eligible (% screened)` = format_count_percent(Eligible, Screened)) 
  
  total_row <- final %>% 
    slice_head(n=1)
  
  last <- bind_rows(final, total_row) %>% 
    slice_tail(n=-1) %>% 
    select(-Eligible, -Refused, -`Not Consented`) %>% 
    select(Facility, starts_with('last_days'), Screened2, Screened, `Eligible (% screened)`, `Refused (% eligible)`, `Not Enrolled for Other Reasons (% eligible)`)
  
  colnames(last) <- c('Facility', rep(c('Screened', 'Eligible (% screened)'), length(days)), "Screened", 'Screened', 'Eligible (% screened)', 'Refused (% eligible)', 'Not Enrolled for Other Reasons (% eligible)')
  
  header_num <- c(1, rep(2, length(days)), 1, 4)
  header_names <- c(" ", paste("Last", days, " Days"), paste("Average per week"), paste("Cumulative", "to date"))
  names(header_num) <- header_names
  
  if(length(days) == 1){
    if(days == 0){
      last <- last[, c(1, seq(from=4, to=ncol(last)))]
      
      if(average == FALSE){
        last <- last[, c(1, seq(from=3, to=ncol(last)))]
        header_num <- header_num[c(1, 4)]
      }
    } else {
      if(average == FALSE){
        last <- last[, c(1, 2, 3, seq(from=5, to=ncol(last)))]
        header_num <- header_num[c(1, 2, 4)]
      }
    }
  } else {
    if(average == FALSE){
      last <- last[, c(seq(from = 1, to = 2*length(days)+1), seq(2*length(days)+3, to=ncol(last)))]
      header_num <- header_num[c(seq(from=1, to=length(days)+1), length(header_num))]
    }
  }
  
  if(cumulative_data == FALSE){
    last <- last[, c(seq(from = 1, to = (ncol(last) - 4)))]
    header_num <- header_num[1:(length(header_num)-1)]
  }
  
  table <- kable(last, format="html", align='l') %>%
    add_header_above(header_num) %>%
    kable_styling("striped", full_width = F, position="left") %>% 
    row_spec(nrow(last), bold = TRUE)
  
  return(table)
}

#' enrollment_by_site_last_days_var_disc_ii
#'
#' @description 
#' Visualizes the right half of the enrollment table: Consented & Randomized, Discontinued, Enrolled, and Safety Set.
#' Consented & Randomized is formatted as a percentage of Eligible.
#'
#' @param analytic Analytic data set.
#' @param discontinued Meta construct for discontinued.
#' @param discontinued_colname Label for the discontinued column.
#' @param include_exclusive_safety_set Toggle for exclusive_safety_set.
#' @param average Return average over the time period (Average Enrolled per week).
#' @param cumulative_data Include final counts.
#' @param days left side of table time period selection
#'
#' @return An HTML table.
#' @export
enrollment_by_site_last_days_var_disc_ii <- function(analytic,  
                                             discontinued="discontinued", 
                                             discontinued_colname="Discontinued", 
                                             include_exclusive_safety_set=FALSE, 
                                             average = FALSE, 
                                             cumulative_data = TRUE,
                                             days = NULL){

  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("consented_and_randomized", "discontinued", 
                           "enrolled", "exclusive_safety_set", 
                           "eligible", "screened_date",
                           "site_certification_date", "facilitycode"), 
    example_types = c("Boolean", "Boolean", 
                      "Boolean", "Boolean", 
                      "Boolean", "Date",
                      "Date", "FacilityCode"))
  
  if(include_exclusive_safety_set){
    df <- analytic %>% 
      select(consented_and_randomized, enrolled, exclusive_safety_set, 
             eligible, screened_date,
             site_certification_date, facilitycode, all_of(discontinued)) %>%
      arrange(facilitycode) 
  } else{
    df <- analytic %>% 
      select(consented_and_randomized, enrolled, 
             eligible, screened_date,
             site_certification_date, facilitycode, all_of(discontinued)) %>%
      arrange(facilitycode)
  }
  
  colnames(df)[which(colnames(df) == discontinued)] <- "discontinued"
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility)) %>% 
    mutate(weeks_site_certified = site_certified_days/7)
  
  if (length(days) == 1) {
    last_day <- Sys.Date() - days
    last_day_df <- df %>%
      mutate(screened_date = as.Date(screened_date)) %>%
      mutate(elig_last = screened_date > last_day) %>%
      mutate(cnr = consented_and_randomized & eligible) %>%
      mutate(consent_last = ifelse(elig_last, cnr, FALSE)) %>%
      select(Facility, consent_last, elig_last) %>%
      group_by(Facility) %>%
      summarize('last_days_Eligible' = sum(elig_last, na.rm = T),
                'last_days_Consented' = sum(consent_last, na.rm = T))
  }
  
  df_1st <- df %>% 
    group_by(Facility) %>% 
    summarize('Days Certified' = site_certified_days[1], 
              Eligible = sum(eligible),
              cnr = sum(consented_and_randomized[eligible == TRUE])) 
  
  if(include_exclusive_safety_set){
    df_2nd <- df %>% 
      group_by(Facility) %>% 
      summarize('Discontinued' = sum(discontinued[eligible == TRUE & consented_and_randomized == TRUE]), 
                "Enrolled" = sum(enrolled[eligible == TRUE & consented_and_randomized == TRUE]), 
                'Safety Set' = sum(exclusive_safety_set[eligible == TRUE & consented_and_randomized == TRUE])) %>% 
      select(Facility, Discontinued, Enrolled, `Safety Set`)
  } else{
    df_2nd <- df %>% 
      group_by(Facility) %>% 
      summarize('Discontinued' = sum(discontinued[eligible == TRUE & consented_and_randomized == TRUE]), 
                "Enrolled" = sum(enrolled[eligible == TRUE & consented_and_randomized == TRUE])) %>% 
      select(Facility, Discontinued, Enrolled)
  }
  
  table_raw <- left_join(df_1st, df_2nd, by = 'Facility')
  
  facilities <- df %>% 
    select(Facility) %>% 
    unique()
  
  by_week <- df %>%
    filter(!is.na(weeks_site_certified)) %>% 
    select(Facility, enrolled, weeks_site_certified) %>% 
    group_by(Facility) %>% 
    summarize(
      Enrolled2 = round(sum(enrolled, na.rm = TRUE) / first(weeks_site_certified), 2))
  weekly <- left_join(facilities, by_week, by = 'Facility')
  almost <- left_join(facilities, weekly, by = 'Facility')
  sum_days_certified <- sum(table_raw$`Days Certified`, na.rm=T)
  
  if (length(days) == 1) {
    final <- left_join(last_day_df, almost, by = 'Facility') %>%
      left_join(table_raw, by = 'Facility') 
  } else {
    final <- left_join(almost, table_raw, by = 'Facility')
  }
  
  final <- final %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,sum_days_certified,`Days Certified`)) %>% 
    select(-is_total) %>% 
    mutate(`Consented & Randomized (% eligible)` = format_count_percent(cnr, Eligible)) %>% 
    mutate(`Discontinued (% randomized)` = format_count_percent(Discontinued, cnr)) %>% 
    mutate(`Eligible & Enrolled (% randomized)` = format_count_percent(Enrolled, cnr))
  
  if (include_exclusive_safety_set) {
    final <- final %>%
      mutate(`Safety Set` = format_count_percent(`Safety Set`, cnr))
  }
  if (length(days) == 1) {
    colnames(final)[2:3] <- c('Eligible', 'Consented')
  }
  
  disc_col <- paste(discontinued_colname, "(% randomized)")                                                                                    
  names(final)[names(final) == "Discontinued (% randomized)"] <- disc_col                                                                      
  
  if (include_exclusive_safety_set) {
    final <- final %>%
      rename(`Not Enrolled Safety Set (% randomized)` = `Safety Set`)
  }
  
  # new way to build colnames and header with multiple arguments
  cols <- "Facility"
  header_num <- c(" " = 1)
  
  if (length(days) == 1) {
    cols <- c(cols, "Eligible", "Consented")
    header_num <- c(header_num, setNames(2, paste("Over last", days, "days")))
  }
  
  if (average) {
    cols <- c(cols, "Enrolled2")
    header_num <- c(header_num, "Average per week" = 1)
  }
  
  if (cumulative_data) {
    cum_cols <- c(
      "Consented & Randomized (% eligible)",
      disc_col,
      if (include_exclusive_safety_set) "Not Enrolled Safety Set (% randomized)",
      "Eligible & Enrolled (% randomized)"
    )
    cols <- c(cols, cum_cols)
    header_num <- c(header_num, setNames(length(cum_cols), "Cumulative to date"))
  }
  
  last <- final %>% select(all_of(cols))
  if ("Enrolled2" %in% names(last)) names(last)[names(last) == "Enrolled2"] <- "Enrolled"
  
  table <- kable(last, format = "html", align = "l") %>%
    add_header_above(header_num) %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    row_spec(nrow(last), bold = TRUE)
  
  return(table)
}

#' Weight Bearing Injury characteristics for Main paper
#'
#' @description This function outputs a table with various injury characteristics for enrolled patients with "Ankle"
#' injuries. This table is produced for Weight bearing main paper. 
#'
#' @param analytic injury_classification_weber, injury_classification_lauge_hansen, injury_gustilo, 
#' injury_type, injury_classification_ankle_ota, definitive_fixation_construct, 
#' definitive_fixation_type, soft_tissue_closure, enrolled
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' wbs_main_paper_injury_characteristics("Replace with Analytic Tibble")
#' 
wbs_main_paper_injury_characteristics <- function(analytic){
  df <- analytic %>% 
    select(injury_classification_weber, injury_classification_lauge_hansen, injury_gustilo, injury_type, 
           injury_classification_ankle_ota, definitive_fixation_construct, definitive_fixation_type, 
           soft_tissue_closure, enrolled) %>% 
    filter(enrolled & injury_type == 'ankle')
  
  total <- df %>% nrow()
  
  df_injury_ota <- df %>% 
    select(injury_classification_ankle_ota) %>% 
    mutate(ota_classification = ifelse(injury_classification_ankle_ota %in% c('44A2', '44A3'), "44 A2/A3", 
                                       ifelse(injury_classification_ankle_ota %in% c('44B2', '44B3'), '44 B2/B3',
                                              ifelse(injury_classification_ankle_ota %in% c('44C1', '44C2', '44C3'), '44 C1/C2/C3', injury_classification_ankle_ota)))) %>% 
    select(-injury_classification_ankle_ota) %>% 
    group_by(ota_classification) %>% 
    count() %>% 
    rename(heading = ota_classification) %>% 
    mutate(Category = "OTA") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_weber <- df %>% 
    select(injury_classification_weber) %>% 
    group_by(injury_classification_weber) %>% 
    count() %>% 
    rename(heading = injury_classification_weber) %>% 
    mutate(Category = "Weber") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_lauge_hansen <- df %>% 
    select(injury_classification_lauge_hansen) %>% 
    group_by(injury_classification_lauge_hansen) %>% 
    count() %>% 
    rename(heading = injury_classification_lauge_hansen) %>% 
    mutate(Category = "Lauge Hansen") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_gustilo <- df %>% 
    select(injury_gustilo) %>% 
    group_by(injury_gustilo) %>% 
    count() %>% 
    rename(heading = injury_gustilo) %>% 
    mutate(Category = "Gustilo")
  
  
  df_fixation_construct <- df %>% 
    select(definitive_fixation_construct) %>% 
    separate_rows(definitive_fixation_construct, sep = ";") %>% 
    group_by(definitive_fixation_construct) %>% 
    count() %>% 
    rename(heading = definitive_fixation_construct) %>% 
    mutate(Category = "Construct") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_fixation_medial <- df %>% 
    select(definitive_fixation_type) %>% 
    separate(definitive_fixation_type, into = c("medial", "lateral", "posterior"), sep='\\|') %>% 
    select(medial) %>% 
    mutate(medial = str_replace(medial, "^Medial-", "")) %>% 
    group_by(medial) %>% 
    count() %>% 
    rename(heading = medial) %>% 
    mutate(Category = "Medial")
  
  df_fixation_lateral <- df %>% 
    select(definitive_fixation_type) %>% 
    separate(definitive_fixation_type, into = c("medial", "lateral", "posterior"), sep='\\|') %>% 
    select(lateral) %>% 
    separate_rows(lateral, sep = ";") %>% 
    mutate(lateral = str_replace(lateral, "^Lateral-", "")) %>% 
    group_by(lateral) %>% 
    count() %>% 
    rename(heading = lateral) %>% 
    mutate(Category = "Lateral") 
  
  df_fixation_posterior <- df %>% 
    select(definitive_fixation_type) %>% 
    separate(definitive_fixation_type, into = c("medial", "lateral", "posterior"), sep='\\|') %>% 
    select(posterior) %>% 
    separate_rows(posterior, sep = ";") %>% 
    mutate(posterior = str_replace(posterior, "^Posterior-", "")) %>% 
    group_by(posterior) %>% 
    count() %>% 
    rename(heading = posterior) %>% 
    mutate(Category = "Posterior") 
  
  empty_df <- tibble(
    soft_tissue_closure = c("Primary closure", "Delayed primary closure", "STSG", "Flap(rotational or free)"),
    n = NA_real_
  )
  
  df_soft_tissue <- df %>% 
    select(soft_tissue_closure) %>% 
    separate_rows(soft_tissue_closure, sep = ";") %>% 
    mutate(soft_tissue_closure = recode(soft_tissue_closure, 
                                        "Primary" = "Primary closure")) %>% 
    full_join(empty_df, by = "soft_tissue_closure") %>% 
    group_by(soft_tissue_closure) %>% 
    count() %>% 
    rename(heading = soft_tissue_closure) %>% 
    mutate(Category = "Tissue") %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading))
  
  df_heading <- tibble(
    Category = "Fixation Types",
    n = NA_real_
  )
  
  bound_df <- bind_rows(df_injury_ota, df_weber, df_lauge_hansen, df_gustilo, df_fixation_construct, df_heading, df_fixation_medial, df_fixation_lateral, df_fixation_posterior,
                        df_soft_tissue) %>% 
    mutate(n = format_count_percent(n, total))
  
  
  df_table_raw <- reorder_rows(bound_df, list('Category'=c("OTA", "Weber", "Lauge Hansen", "Gustilo", "Construct", 
                                                           'Fixation Types', 'Medial', 'Lateral', 'Posterior', 
                                                           'Tissue'), 
                                              'heading'=c('44 A2/A3', '44 B2/B3', '44 C1/C2/C3', '44B1',
                                                          'Type B', 'Type C', 'Pronation-abduction (PA)',
                                                          'Pronation-external rotation (PER)', 'Supination-adduction (SA)',
                                                          'Supination-external rotation (SER)', 'Closed', 'Type I',
                                                          'Type II', 'Type IIIA', 'Medial only', 'Lateral and Posterior', 'Medial and Lateral',
                                                          'Medial, Lateral, and Posterior', 'No fixation', 'Ligament repair', 'Intramedullary Device',
                                                          'Screws', 'Screws Only', 'Screws and Plates', 'Delayed primary closure', 'Flap(rotational or free)',
                                                          'Primary closure', 'STSG', 'Missing'))) 
  
  index_vec_a <- c(
    "OTA Injury Classification" = df_table_raw %>% filter(Category=='OTA') %>% nrow(), 
    "Weber Classification" = df_table_raw %>% filter(Category=='Weber') %>% nrow(), 
    "Lauge-Hansen Classification" = df_table_raw %>% filter(Category=='Lauge Hansen') %>% nrow(),
    "Gustilo Type" = df_table_raw %>% filter(Category=='Gustilo') %>% nrow(),  
    "Fixation Constructs"= df_table_raw %>% filter(Category=='Construct') %>% nrow(), 
    "Fixation Types" = df_table_raw %>% filter(Category=='Medial'|Category=='Lateral'|Category=='Posterior') %>% nrow(), 
    "Soft Tissue Closure"= df_table_raw %>% filter(Category=='Tissue') %>% nrow()
    )
  index_vec_b <- c(
    " " = 21, 
    "Medial"= df_table_raw %>% filter(Category=='Medial') %>% nrow(), 
    "Lateral"= df_table_raw %>% filter(Category=='Lateral') %>% nrow(), 
    "Posterior"= df_table_raw %>% filter(Category=='Posterior') %>% nrow(),
    " "= 5
    )
  
  
  title <- paste("Total = ", total)
  
  df_for_table <- df_table_raw %>% 
    filter(!is.na(n)) %>% 
    select(-Category) %>% 
    filter(!is.na(heading)) %>% 
    rename(" " = heading) %>% 
    rename(!!title := n) 
  
  table_raw<- kable(df_for_table, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>% 
    pack_rows(index = index_vec_b, label_row_css = "text-align:left", bold = FALSE) %>% 
    kable_styling("striped", full_width = F, position='left') %>% 
    row_spec(c(0,4,7,12,16,21,34), extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}


#' Weight Bearing Patient Characteristics for Main paper
#'
#' @description Visualizes various patient characteristics/demographics for enrolled 
#' patients with "Ankle" injuries. 
#' 
#' NOTE: This table was originally produced for Weight bearing main paper, but may apply to your study, 
#' see the used constructs for more. 
#'
#' @param analytic enrolled, injury_type, sex, age, ethnicity_race, education_level, patient_reported_self_efficacy_6mo, 
#' patient_reported_self_efficacy_12mo, preinjury_productive_activity, preinjury_work_demand, 
#' preinjury_work_hours, tobacco_use, bmi, preinjury_health, insurance_type
#'
#' @return html table
#' @export
#'
#' @examples
#' wbs_main_paper_patient_characteristics(analytic)
#' 
wbs_main_paper_patient_characteristics <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("enrolled", "injury_type", "sex", "age", "ethnicity_race", "education_level",
                           "patient_reported_self_efficacy_6mo", "patient_reported_self_efficacy_12mo",
                           "preinjury_productive_activity", "preinjury_work_demand", "preinjury_work_hours",
                           "tobacco_use", "bmi", "preinjury_health", "insurance_type"), 
    example_types = c("Boolean", "NamedCategory['ankle' 'other']", "Category-U3", 
                      "Number-U100", "Category-U5", "Number-U10", "Number-U10", "Category-U4",
                      "Category-U4", "Category-U4", "Number-U60", "Boolean", "Number-U60", 
                      "Category-U4", "Category-U4"))
  
  df <- analytic %>% select(enrolled, injury_type, sex, age, ethnicity_race, education_level,
                            patient_reported_self_efficacy_6mo, patient_reported_self_efficacy_12mo,
                            preinjury_productive_activity, preinjury_work_demand, preinjury_work_hours,
                            tobacco_use, bmi, preinjury_health, insurance_type) %>% 
    filter(enrolled, injury_type == 'ankle')
  
  total <- df %>% nrow()
  
  df_age_missing <- df %>%  select(age) %>% mutate(age = ifelse(is.na(age), "Missing", age)) %>% 
    filter(age == "Missing") %>% count(age) %>% 
    rename(heading = age) %>% 
    mutate(Category = "Age") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_age <- df %>% 
    select(age) %>% 
    mutate(age = as.numeric(age)) %>%
    filter(!is.na(age)) %>% 
    summarise(age_mean = format_mean_sd(age)) %>% 
    mutate(heading = 'Mean age, (SD)') %>% 
    mutate(Category = "Age") %>% 
    rename(n = age_mean)
  
  df_age_final <- rbind(df_age, df_age_missing)
  
  df_sex <- df %>% select(sex) %>% 
    group_by(sex) %>% 
    count() %>% 
    mutate(Category = 'Sex') %>% 
    rename(heading = sex) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_race_ethnicity <- df %>% 
    select(ethnicity_race) %>% 
    group_by(ethnicity_race) %>% 
    count() %>% 
    mutate(Category = 'Race Ethnicity') %>% 
    rename(heading = ethnicity_race) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_education <- df %>% 
    select(education_level) %>% 
    group_by(education_level) %>% 
    count() %>% 
    mutate(Category = 'Education Level') %>% 
    rename(heading = education_level) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_self_efficacy_missing_6mo <- df %>%  
    select(patient_reported_self_efficacy_6mo) %>% 
    mutate(patient_reported_self_efficacy_6mo = ifelse(is.na(patient_reported_self_efficacy_6mo), "Missing 6 months Self Efficacy", patient_reported_self_efficacy_6mo)) %>% 
    filter(patient_reported_self_efficacy_6mo == "Missing 6 months Self Efficacy") %>% 
    count(patient_reported_self_efficacy_6mo) %>% 
    rename(heading = patient_reported_self_efficacy_6mo) %>% 
    mutate(Category = "Self Efficacy") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_self_efficacy_missing_12mo <- df %>%  
    select(patient_reported_self_efficacy_12mo) %>% 
    mutate(patient_reported_self_efficacy_12mo = ifelse(is.na(patient_reported_self_efficacy_12mo), "Missing 12 months Self Efficacy", patient_reported_self_efficacy_12mo)) %>% 
    filter(patient_reported_self_efficacy_12mo == "Missing 12 months Self Efficacy") %>% 
    count(patient_reported_self_efficacy_12mo) %>% 
    rename(heading = patient_reported_self_efficacy_12mo) %>% 
    mutate(Category = "Self Efficacy") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_self_efficacy_6mo <- df %>% 
    select(patient_reported_self_efficacy_6mo) %>% 
    filter(!is.na(patient_reported_self_efficacy_6mo)) %>% 
    mutate(patient_reported_self_efficacy_6mo = as.numeric(patient_reported_self_efficacy_6mo)) %>% 
    summarise(n = format_mean_sd(patient_reported_self_efficacy_6mo)) %>% 
    mutate(heading = 'Within 6 Months') %>% 
    mutate(Category = 'Self Efficacy')  
  
  df_self_efficacy_12mo <- df %>% 
    select(patient_reported_self_efficacy_12mo) %>% 
    filter(!is.na(patient_reported_self_efficacy_12mo)) %>% 
    mutate(patient_reported_self_efficacy_12mo = as.numeric(patient_reported_self_efficacy_12mo)) %>% 
    summarise(n = format_mean_sd(patient_reported_self_efficacy_12mo)) %>% 
    mutate(heading = 'Within 1 year') %>% 
    mutate(Category = 'Self Efficacy')  
  
  df_self_efficacy_final <- rbind(df_self_efficacy_6mo, df_self_efficacy_missing_6mo, df_self_efficacy_12mo, df_self_efficacy_missing_12mo)
  
  
  df_usual_major_activity <- df %>% 
    select(preinjury_productive_activity) %>% 
    group_by(preinjury_productive_activity) %>% 
    count() %>% 
    mutate(Category = 'Major Activity') %>% 
    rename(heading = preinjury_productive_activity) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_physical_demand <- df %>% 
    select(preinjury_work_demand) %>% 
    group_by(preinjury_work_demand) %>% 
    count() %>% 
    mutate(Category = 'Work Demand') %>% 
    rename(heading = preinjury_work_demand) %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_work_hours_missing <- df %>%  select(preinjury_work_hours) %>% 
    mutate(preinjury_work_hours = ifelse(is.na(preinjury_work_hours), "Missing", preinjury_work_hours)) %>% 
    filter(preinjury_work_hours == "Missing") %>% count(preinjury_work_hours) %>% 
    rename(heading = preinjury_work_hours) %>% 
    mutate(Category = "Work hours") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_work_hours <- df %>% 
    select(preinjury_work_hours) %>% 
    mutate(preinjury_work_hours = as.numeric(preinjury_work_hours)) %>% 
    filter(!is.na(preinjury_work_hours)) %>% 
    summarise(work_hours = format_mean_sd(preinjury_work_hours)) %>% 
    mutate(heading = 'Mean hours, (SD)') %>% 
    mutate(Category = "Work hours")  %>% 
    rename(n = work_hours)
  
  df_work_hours_final <- rbind(df_work_hours, df_work_hours_missing)
  
  
  df_tobacco <- df %>% 
    select(tobacco_use) %>% 
    group_by(tobacco_use) %>% 
    count() %>% 
    rename(heading = tobacco_use) %>% 
    mutate(Category = 'Tobacco') %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_bmi_missing <- df %>%  select(bmi) %>% mutate(bmi = ifelse(is.na(bmi), "Missing", bmi)) %>% 
    filter(bmi == "Missing") %>% count(bmi) %>% 
    rename(heading = bmi) %>% 
    mutate(Category = "BMI") %>% 
    mutate(n = format_count_percent(n, total))
  
  df_bmi <- df %>% 
    select(bmi) %>% 
    mutate(bmi = as.numeric(bmi)) %>% 
    summarise(bmi_mean = format_mean_sd(bmi)) %>% 
    mutate(heading = 'Mean, (SD)') %>% 
    mutate(Category = "BMI")  %>% 
    rename(n = bmi_mean)
  
  df_bmi_final <- rbind(df_bmi, df_bmi_missing)
  
  df_preinjury_health <- df %>% 
    select(preinjury_health) %>% 
    group_by(preinjury_health) %>%
    count() %>% 
    rename(heading = preinjury_health) %>% 
    mutate(Category = 'Health') %>% 
    mutate(heading = ifelse(is.na(heading), "Missing", heading)) %>% 
    mutate(n = format_count_percent(n, total))
  
  df_insurance <- df %>% 
    select(insurance_type) %>% 
    mutate(insurance_type = ifelse(str_detect(insurance_type, "Medicaid"), "Medicaid", 
                                   ifelse(!is.na(insurance_type), "Other Insurance", NA))) %>% 
    mutate(insurance_type = ifelse(!is.na(insurance_type), insurance_type, "Missing")) %>% 
    group_by(insurance_type) %>%
    count() %>% 
    rename(heading = insurance_type) %>% 
    mutate(Category = 'Insurance')  %>% 
    mutate(n = format_count_percent(n, total))
  
  df_final <- rbind(df_age_final, df_sex, df_race_ethnicity, df_education, df_self_efficacy_final, df_usual_major_activity,
                    df_physical_demand, df_work_hours_final, df_tobacco, df_bmi_final, df_preinjury_health, df_insurance) 
  
  
  index_vec_a <- c(
    "Age" = nrow(df_age_final),
    "Sex" = nrow(df_sex),
    "Race Ethnicity" = nrow(df_race_ethnicity),
    "Education" = nrow(df_education),
    "Self Efficacy for return to Usual Activities" = nrow(df_self_efficacy_final),
    "Preinjury Usual Major Activity" = nrow(df_usual_major_activity),
    "Physical Demand of Job" = nrow(df_physical_demand),
    "Hours worked per week" = nrow(df_work_hours_final),
    "Tobacco Use" = nrow(df_tobacco),
    "BMI" = nrow(df_bmi_final),
    "Preinjury Health" = nrow(df_preinjury_health),
    "Insurance Type" = nrow(df_insurance)
  )
  
  # Compute the cumulative row indices for adding bottom borders.
  # The first element (0) is used for the header row.
  border_rows <- c(0, cumsum(index_vec_a))
  
  title <- paste("Total = ", total)
  
  
  df_for_table <- df_final %>% 
    select(heading, n) %>% 
    rename("Enrolled" = heading) %>% 
    rename(!!title := n) 
  
  
  table_raw<- kable(df_for_table, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>% 
    kable_styling("striped", full_width = F, position='left') %>% 
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")
  
  
  return(table_raw)
} 

#' Expected visit status for Overall Followup
#'
#' @description 
#' Returns the counts of all the statuses of the Overall follow-up form at every follow-up periods. Notably,
#' this function does not separate the counts by site.
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' expected_and_followup_visit_overall("Replace with Analytic Tibble")
#' 
expected_and_followup_visit_overall <- function(analytic, pretty_cols = c()){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("followup_data"),
    example_types = c("(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date")) 

  df <- analytic %>% 
    select(study_id, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate(status = as.character(status)) %>% 
    mutate_all(na_if, 'NA')
  
  fu_levels <- df$followup_period %>% unique()
  fu_levels <- fu_levels[!is.na(fu_levels)]
  
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
  
  final_raw <- left_join(df_empty, combined, by = 'status') %>% 
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
  
  not_expected <- final_pre_pct %>%
    slice_head(n=2) %>%
    slice_tail(n=1)
  
  final_last <- rbind(not_expected, expected_row, top, middle, bottom) %>% 
    rename(Status = status)
  
  if (!is.null(pretty_cols)) {
    colnames(final_last) = c('Status', pretty_cols)
  }
  
  vis <- kable(final_last, format="html", align='l') %>%
    add_indent(c(4,5)) %>% 
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}



#' Followup Data Single Form and Timepoint By Site
#'
#' @description Returns the designated followup form status across all sites, 
#' for a single timepoint.
#' 
#' #' For other manipulations of the followup_data long file that may better fit your study, please see: followup_completion_time_stats, 
#' followup_form_all_timepoints_by_site, followup_form_at_timepoint_by_site, followup_forms_all_timepoints, followup_forms_at_timepoint_by_site
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data
#' @param timepoint the point in time to be considered in the visualization
#' @param form_selection the form to be considered in the visualization
#' @param name optional argument for changing the name of the followup form, for aesthetic use
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_form_at_timepoint_by_site("Replace with Analytic Tibble", "3 Month", "Form 3")
#' 
followup_form_at_timepoint_by_site <- function(analytic, timepoint, form_selection, name = NULL){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "followup_data"), 
    example_types = c("FacilityCode", "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
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
  
  header <- c(1,8)
  names(header) <- c(' ', ifelse(is.null(name),
                                 paste0(form_selection, ' Status at ', timepoint, ' Period'),
                                 paste0(name, ' Status at ', timepoint, ' Period')))
  
  vis <- kable(form_df, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Followup Data Single Form All Timepoints By Site
#'
#' @description 
#' Returns the counts of the specified form statuses of a given form for all follow-up periods where that
#' form is present, by site. Specifying "Overall" in the form_selection results in a slightly more streamlined
#' look, without a header above the table.
#'
#' @param analytic analytic data set that must include study_id, followup_data
#' @param form_selection form whose statuses are to be investigated
#' @param included_colmns statuses to include in the vis
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_form_all_timepoints_by_site("Replace with Analytic Tibble", form_selection = "Form 3")
#' followup_form_all_timepoints_by_site("Replace with Analytic Tibble", form_selection = "Form 3", included_columns = c("Expected", "Complete", "Incomplete"))
#' 
followup_form_all_timepoints_by_site <- function(
    analytic, form_selection = 'Overall', 
    included_columns=c("Not Expected", "Expected", "Complete", 
                       "Early", "Late", 'Missed', 'Not Started', 
                       'Incomplete')){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "followup_data"), 
    example_types = c("FacilityCode", "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
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
  
  timepoints <- df %>%
    filter(form == form_selection) %>%
    pull(followup_period) %>%
    unique()
  timepoints <- timepoints[!is.na(timepoints)]
  
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
    filter(!is.na(Facility)&Facility!='NA') %>% 
    select(matches(paste0("^",paste(c("Facility",included_columns),collapse="|^"))))
    
  colnames(form_df) <- c('Facility', rep(included_columns, times = length(timepoints)))
  
  header <- c(1,rep(length(included_columns), length(timepoints)))
  names(header) <- c(' ', timepoints)
  
  over_header <- c(1, length(included_columns)*length(timepoints))
  names(over_header) <- c(' ', paste(form_selection, 'Form Status'))
  
  if(form_selection=="Overall"){
    vis <- kable(form_df, format="html", align='l') %>%
      add_header_above(header) %>%
      kable_styling("striped", full_width = F, position='left')
  } else{
    vis <- kable(form_df, format="html", align='l') %>%
      add_header_above(header) %>%
      add_header_above(over_header) %>%
      kable_styling("striped", full_width = F, position='left')
  }
  return(vis)
}




#' Followup Data Multiple Forms and Single Timepoints By Site
#'
#' @description Visualizes the specified follow-up forms at a specific timeppoint, listed by site.
#' 
#' #' For other manipulations of this file that may better fit your study, please see: followup_completion_time_stats, 
#' followup_form_all_timepoints_by_site, followup_form_at_timepoint_by_site, followup_forms_all_timepoints, followup_forms_at_timepoint_by_site
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_forms_at_timepoint_by_site("Replace with Analytic Tibble", '3 Month', c('Form 3', 'Form 2'))
#' 
followup_forms_at_timepoint_by_site <- function(analytic, timepoint, forms, names = NULL){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "followup_data"), 
    example_types = c("FacilityCode", "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA')
  
  df <- df %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
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
  
  cols <- c('Facility', rep(c("Not Expected", "Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 
                              'Incomplete'), times = length(forms)))
  colnames(output) <- cols
  
  header <- c(1,rep(8, length(forms)))
  if (is.null(names)) {
    header_names <- c(' ', paste0(forms, ' Status at ', timepoint, ' Period'))
  } else {
    header_names <- c(' ', paste0(names, ' Status at ', timepoint, ' Period'))
  }
  
  names(header) <- header_names
  
  vis <- kable(output, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Followup Data Multiple Forms and All Timepoints
#'
#' @description 
#' Returns all of the statuses of the given follow-up forms at all timepoints by site. Not specifying forms results
#' in all follow-up forms being in the visualization.
#' 
#' NOTE: THIS VISUALIZATION CAN BE VERY LARGE!
#'
#' @param analytic analytic data set that must include study_id, followup_data, facilitycode
#' @param forms followup forms to output, as found in the followup_data construct
#' @param timepoints timepoints to output, as found in the followup_data construct
#' @param vertical whether to arrange the output vertically by form or horizontally.
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' followup_forms_all_timepoints("Replace with Analytic Tibble", forms = c('Form 3', 'Form 2'), timepoints = c('3 Month', '6 Month'))
#' followup_forms_all_timepoints("Replace with Analytic Tibble", vertical = FALSE)
#' 
followup_forms_all_timepoints <- function(analytic, forms = NULL, timepoints = NULL, vertical = TRUE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "followup_data"), 
    example_types = c('FacilityCode', "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"))
  
  df <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
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
  
  per_form <- function(form_name) {
    form_df <- df %>%
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
      
      if (nrow(result) != 0) {
        result_list[[i]] <- result      }
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
      slice(2)
    
    final_last <- rbind(not_expected_row, expected_row, top, middle, bottom) %>% 
      rename(Status = status)
    
    final_last
  }
  
  found_timepoints <- c()
  header <- c()
  out <- NULL
  for (form_name in forms) {
    res <- per_form(form_name)
    if (is.null(out)) {
      out <- res
      found_timepoints <- colnames(res)
      header <- rep(form_name, times = length(colnames(res))-1)
    } else {
      out <- full_join(out, per_form(form_name), by = 'Status')
      found_timepoints <- c(found_timepoints, colnames(res))
      header <- c(header, rep(form_name, times = length(colnames(res))-1))
    }
  }
  found_timepoints <- found_timepoints[found_timepoints!='Status']
  
  colnames(out) <- c('Status', found_timepoints)
  header <- c(' ', base::table(header))
  
  if (vertical) {
    out_long <- NULL
    i <- 2
    for (package in names(header[-1])) {
      colcount <- as.numeric(header[package]) - 1
      colindex <- i + colcount
      temp_df <- out[i:colindex]
      if (is.null(out_long)) {
        out_long <- temp_df
      } else {
        out_long <- bind_rows(out_long, temp_df)
      }
      i <- colindex + 1
    }
    
    out_long <- out_long %>%
      mutate(across(everything(), ~replace(., is.na(.), "."))) %>%
      mutate(Status = rep(c("Not Expected", "Expected", "Complete", "Early", "Late", 'Missed', 'Not Started', 'Incomplete'), 
                          length(forms))) %>%
      select(Status, everything())
    
      
    vis <- kable(out_long, format="html", align='l')  %>%
      kable_styling("striped", full_width = F, position='left')
    
    i <- 1
    for (package in names(header[-1])) {
      vis <- vis %>%
        pack_rows(package, i, i + 7)
      i <- i + 8
    }
  } else if (!vertical) {
    vis <- kable(out, format="html", align='l') %>%
      add_indent(c(3,4)) %>% 
      add_header_above(header) %>%
      kable_styling("striped", full_width = F, position='left')
  }
  
  return(vis)
}


#' Overview of enrollment and follow-up activities
#'
#' @description 
#' Returns the screened, overall, and follow-up data, separated by sites
#'
#' @param analytic analytic data set that must include study_id, followup_data,
#' facilitycode, screened, enrolled, eligible, screened_date
#' @param form_name The exact name (specified in followup_data) that you want
#' to find the data for
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_and_followup_activities_overview("Replace with Analytic Tibble")
#' 
enrollment_and_followup_activities_overview <- function(analytic, form_name = 'Overall'){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('facilitycode', "followup_data", "screened", "enrolled", "eligible", "screened_date"), 
    example_types = c('FacilityCode', "(';new_row: ', '|')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date",
                      "Boolean", "Boolean", "Boolean", "Date"))
  
   followups <- analytic %>%
    select(study_id, facilitycode, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 
                              'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA') %>%
    filter(form == form_name)
  
  followups <- followups %>%
    mutate(status = gsub('_', ' ', status)) %>%
    mutate(status = tools::toTitleCase(status))
  
  study_status <- analytic %>%
    select(study_id, facilitycode, screened, enrolled, eligible, screened_date)
  
  last30 <- Sys.Date() - 30
  
  per_site <- function(site = 'TOTAL') {
    if (site != 'TOTAL') {
      site_followups <-  followups %>%
        filter(facilitycode == site)
      site_study_status <- study_status %>%
        filter(facilitycode == site)
    } else {
      site_followups <- followups
      site_study_status <- study_status
    }
    
    last_month <- site_study_status %>%
      filter(screened_date > last30) %>%
      mutate(elig_not_enr = eligible & !enrolled) %>%
      reframe(Screened = sum(screened, na.rm = TRUE),
              Enrolled = sum(enrolled, na.rm = TRUE),
              `Eligible, Not Enrolled` = sum(elig_not_enr, na.rm = TRUE)) %>%
      mutate(Enrolled = format_count_percent(Enrolled, Screened),
             `Eligible, Not Enrolled` = format_count_percent(`Eligible, Not Enrolled`, Screened))
    
    historical <- site_study_status %>%
      mutate(elig_not_enr = eligible & !enrolled) %>%
      reframe(Screened = sum(screened, na.rm = TRUE),
              Enrolled = sum(enrolled, na.rm = TRUE),
              `Eligible, Not Enrolled` = sum(elig_not_enr, na.rm = TRUE)) %>%
      mutate(Enrolled = format_count_percent(Enrolled, Screened),
             `Eligible, Not Enrolled` = format_count_percent(`Eligible, Not Enrolled`, Screened))
    
    periods <- site_followups %>%
      pull(followup_period) %>%
      unique()
    
    followup_counts <- tibble()
    
    count_list <- list()
    
    for (period in periods) {
      complete_count <- site_followups %>%
        filter(followup_period == period, status == 'Complete') %>%
        nrow()
      
      count_list[[paste(period, "Follow-up")]] <- complete_count
    }
    
    followup_counts <- as_tibble(count_list)  
    
    site_combined <- cbind(last_month, historical, followup_counts)
    site_counts <- tibble(
      Site=site, site_combined, .name_repair = 'minimal'
    )
    
    site_counts
  }
  
  all_sites <- study_status$facilitycode %>% unique() %>% sort()
  all_sites_followup <- followups$facilitycode %>% unique() %>% sort()
  
  if (length(all_sites) != length(all_sites_followup)){
    sites_wo_followups <- all_sites[!all_sites %in% all_sites_followup]
    warning(paste("Site(s)", sites_wo_followups, "not found in followup_data, will not be included in final visualization"))
    all_sites <- all_sites[all_sites %in% all_sites_followup]
  }
  
  sites_combined <- tibble()
  for (site in c('TOTAL', all_sites)) {
    sites_combined <- rbind(sites_combined, per_site(site))
  }
  
  header <- c(1, 3, 3, ncol(sites_combined)-7)
  names(header) <- c(' ', 'Last 30 Days', 'Study Length', 'Follow-up Completion Status')
  
  vis <- kable(sites_combined, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Follow-up Forms Time to Complete
#'
#' @description 
#' Returns summary statistics on the number of days to complete various follow-up forms. Each study will have a 'unique' followup data long file, 
#' this visualization manipulates a part of that file to return that info for the desired forms.
#' 
#' For other manipulations of the followup_data long file that may better fit your study, please see: followup_completion_time_stats, 
#' followup_form_all_timepoints_by_site, followup_form_at_timepoint_by_site, followup_forms_all_timepoints, followup_forms_at_timepoint_by_site
#'
#' @param analytic This is the analytic data set that must include study_id, followup_data, event_time_zero,
#' and enrolled
#' @param timepoints the point in time to be considered in the visualization
#' @param form_selection the form to be considered in the visualization
#'
#' @return html table
#' @export
#'
#' @examples
#' followup_completion_time_stats("Replace with Analytic Tibble")
#' 
followup_completion_time_stats <- function(analytic, timepoints = c('6mo', '12mo'), ortho_timepoints = NULL, form_selection = 'Overall'){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('time_zero', "followup_data", "enrolled", 'followup_expected_12mo', 'followup_expected_6mo'), 
    example_types = c('Date', "(';new_row: ', '|')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date", 'Boolean', 'Boolean', 'Boolean'))
  
  if (is.null(ortho_timepoints)) {
    ortho_timepoints <- timepoints
  }
  
  df <- analytic %>%
    select(study_id, time_zero, followup_data,
           matches(paste0('^orthopaedic_last_date_(', paste(ortho_timepoints, collapse = '|'), ')$')),
           matches(paste0('^followup_expected_(', paste(timepoints, collapse = '|'), ')$')),
           enrolled) %>%
    mutate(across(matches("_date$|_date_"), ~ as.Date(., format = "%Y-%m-%d"))) %>%
    mutate(time_zero = as.Date(sapply(str_split(time_zero, ";"), `[`, 1))) %>%
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>%
    separate(status, c('status', 'timing'), sep = ':') %>%
    filter(form == form_selection) %>%
    select(-form, -redcap_event_name, -timing)
  
  df <- df %>%
    mutate(status = na_if(status, 'NA')) %>%
    mutate(form_dates = na_if(form_dates, 'NA')) 
  
  beautify_timepoint <- function(string) {
    newstr <- str_replace(string, 'wk', ' Week') %>%
      str_replace('mo', ' Month')
    return(newstr)
  }
  
  expected_counts <- df %>%
    select(study_id, starts_with('followup_expected')) %>%
    unique() %>%
    summarise(across(starts_with('followup_expected'), ~ sum(. == TRUE))) %>%
    pivot_longer(starts_with('followup_expected'))  %>%
    mutate(name = beautify_timepoint(str_remove(name, "followup_expected_"))) %>%
    rename(`Follow-up Period` = name)
  
  converted_timepoints <- beautify_timepoint(timepoints)
  converted_ortho_timepoints <- beautify_timepoint(ortho_timepoints)
  
  filtered_and_pivoted <- df %>%
    select(-starts_with('followup_expected')) %>%
    filter(followup_period %in% converted_timepoints) %>%
    mutate(form_dates = as.Date(form_dates)) %>%
    pivot_wider(values_from = form_dates, names_from = followup_period,
                names_prefix = 'Form; ')
  
  long_format <- filtered_and_pivoted %>%
    pivot_longer(
      cols = starts_with("orthopaedic_last_date") | starts_with("Form"),
      names_to = "timepoint",
      values_to = "date"
    ) %>%
    mutate(timepoint = beautify_timepoint(str_replace(timepoint, 'orthopaedic_last_date_', 'Ortho; ')))
  
  date_calc <- long_format %>%
    mutate(days = as.numeric(date - time_zero))
  
  inner_function <- function(inner_data) {
    inner_out <- tibble(
      `N (Number of Complete)` = nrow(inner_data),
      `Mean (Days)` = mean(inner_data$days, na.rm = TRUE) %>% round(2),
      `Standard Deviation` = sd(inner_data$days, na.rm = TRUE) %>% round(2),
      `Minimum (Days)` = min(inner_data$days, na.rm = TRUE),
      `24th Percentile (Days)` = quantile(inner_data$days, 0.24, na.rm = TRUE, type = 1),
      `Median (Days)` = quantile(inner_data$days, 0.5, na.rm = TRUE, type = 1),
      `75th Percentile (Days)` = quantile(inner_data$days, 0.75, na.rm = TRUE, type = 1),
      `Maximum (Days)` = max(inner_data$days, na.rm = TRUE)
    )
    if (inner_out$`N (Number of Complete)` == 0) {
      inner_out <- inner_out %>%
        mutate(across(-`N (Number of Complete)`, ~ "."))
    }
    inner_out
  }
  
  separated <- date_calc %>%
    separate(timepoint, into = c('kind', 'period'), sep = '; ')
  
  out <- NULL
  for(time in converted_timepoints) {
    temp <- separated %>%
      filter(period == time)
    
    if (time %in% converted_ortho_timepoints) {
      temp2 <- rbind(
        inner_function(temp %>% filter(kind=='Form'&enrolled&!is.na(days))) %>%
          mutate(kind = 'form completion', group = 'enrolled', 
                 Description = 'Days to Form Completion'),
        inner_function(temp %>% filter(kind=='Ortho'&enrolled&!is.na(days))) %>%
          mutate(kind = 'ortho visit', group = 'enrolled', 
                 Description = 'Days to Last Orthopaedic Visit')) %>%
        mutate(period = time)
    } else {
      temp2 <- rbind(
        inner_function(temp %>% filter(kind=='Form'&enrolled&!is.na(days))) %>%
          mutate(kind = 'form completion', group = 'enrolled', 
                 Description = 'Days to Form Completion')) %>%
        mutate(period = time)
    }
    
    if (is.null(out)) {
      out <- temp2
    } else {
      out <- rbind(out, temp2)
    }
  }
  
  output <- full_join(expected_counts, out %>% rename(`Follow-up Period` = period)) %>%
    select("Follow-up Period", "value", "N (Number of Complete)", 
           "Description", "Mean (Days)", "Standard Deviation", 
           "Minimum (Days)", "24th Percentile (Days)", "Median (Days)", 
           "75th Percentile (Days)", "Maximum (Days)") %>%
    arrange(factor(`Follow-up Period`, levels = converted_timepoints)) %>%
    rename(`N (Number of Expected)` = value)
  
  vis <- kable(output, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Not enrolled reason
#'
#' @description 
#' Visualizes list of study_ids who were are not enrolled, the reasons, and the screening notes reasons.
#' 
#' See also: not_enrolled_for_other_reasons, which examines the reasons categorized as 'other'
#'
#' @param analytic This is the analytic data set that must include study_id, facilitycode, study_id, not_enrolled_reason, pre_screened_notes
#' @param last_days This filters for ids who have been not_enrolled in the last x many days.
#'
#' @return An HTML table.
#' @export
#'
#' @examples                     
#' not_enrolled_reason("Replace with Analytic Tibble")
#' 
not_enrolled_reason <- function(analytic, last_days = NULL){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "not_enrolled_reason", "not_enrolled_date",
                           "pre_screened_notes"), 
    example_types = c("FacilityCode", "Character", "Date",
                      "Character"))
  
  df <- analytic %>%
    select(facilitycode, study_id, not_enrolled_reason, pre_screened_notes, not_enrolled_date) %>%
    filter(!is.na(not_enrolled_reason))
  
  if (!is.null(last_days)){
    df <- df %>%
      filter(not_enrolled_date >= Sys.Date() - last_days)
  }
  
  df %>%
    rename(Site = facilitycode,
           ID = study_id,
           `Reason for Not Enrolling` = not_enrolled_reason,
           `Screening Notes` = pre_screened_notes) %>% 
    select(-not_enrolled_date)
  
  output <- kable(df, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}
                       

#' Outcome by Site
#'
#' @description 
#' Returns summary statistics on the number of the time to event data of each site for a specified outcome.
#' Output column "Percent of Expected (excluding events)" comes from excluding events from the days sum calculation,
#' and "Percent of Expected" refers to dividing the average outcome_days with the average expected_days.
#'
#' @param analytic analytic data set that must include study_id, outcome_data, facilitycode, and enrolled
#' @param outcome_name name of the outcome to be considered in the visualization
#' @param days_since_dz optional numeric keyowrd argument to filter only for rows whose time_zero occured at leas that many days ago
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' outcome_by_site("Replace with Analytic Tibble", 'test_outcome')
#' 
outcome_by_site <- function(analytic, outcome_name, days_since_tz = 365) {
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('outcome_data', 'facilitycode', 'enrolled'), 
    example_types = c("(';', ',')NamedCategory['test_outcome']|Number|Number|Date|Date|NamedCategory['check' 'event']|Number|Number|Date", 'FacilityCode', 'Boolean'))
  
  # Extract the relevant outcome data
  outcome_data <- analytic %>%
    select(study_id, outcome_data, facilitycode, enrolled) %>%
    filter(enrolled) %>%
    # Split the outcome_data string
    separate_rows(outcome_data, sep=";") %>% 
    # Split each record into columns
    separate(
      outcome_data,
      c("outcome_name", "target_days", "expected_days", "time_zero", 
        "outcome_date_extended", "outcome_type", "outcome_days_extended", 
        "outcome_days", "outcome_date"),
      sep = ","
    ) %>%
    # Filter for the specific outcome
    filter(outcome_name == !!outcome_name) %>%
    filter((as.Date(time_zero)+days_since_tz)<Sys.Date()) %>% 
    # Convert numeric columns
    mutate(
      target_days = as.numeric(target_days),
      expected_days = as.numeric(expected_days),
      outcome_days_extended = as.numeric(outcome_days_extended),
      outcome_days = as.numeric(outcome_days)
    )
  
  # Calculate overall statistics
  overall_stats <- outcome_data %>%
    summarise(
      n_total = n(),
      n_missing = sum(is.na(outcome_days)),
      min_days = min(outcome_days, na.rm = TRUE),
      max_days = max(outcome_days, na.rm = TRUE),
      avg_days = format_mean_sd(outcome_days, decimals = 0),
      pct_expected = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%")
    ) %>%
    mutate(facilitycode = "Overall")
  
  overall_expected_non_event <- outcome_data %>%
    filter(outcome_type != 'event') %>%
    summarise(pct_expected_excluding_events = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%"))
  
  overall_stats <- overall_stats %>%
    cbind(overall_expected_non_event)
  
  # Calculate site-specific statistics
  site_stats <- outcome_data %>%
    group_by(facilitycode) %>%
    summarise(
      n_total = n(),
      n_missing = sum(is.na(outcome_days)),
      min_days = min(outcome_days, na.rm = TRUE),
      max_days = max(outcome_days, na.rm = TRUE),
      avg_days = format_mean_sd(outcome_days, decimals = 0),
      pct_expected = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%")
    ) %>%
    ungroup() %>%
    mutate(order_col = as.numeric(str_remove(pct_expected,"%"))) %>% 
    arrange(desc(order_col)) %>% 
    select(-order_col)
  
  site_expected_non_event <- outcome_data %>%
    group_by(facilitycode) %>%
    filter(outcome_type != 'event') %>%
    summarise(pct_expected_excluding_events = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%"))
  
  site_stats <- site_stats %>%
    left_join(site_expected_non_event)
  
  
  # Combine overall and site-specific statistics
  results <- bind_rows(overall_stats, site_stats)
  
  results <- results %>%
    rename(`N (Participants)` = n_total,
           `Missing Time to Event (Participants)` = n_missing,
           `Minimum (Days)` = min_days,
           `Maximum (Days)` = max_days,
           `Mean (Standard Deviation)` = avg_days,
           `Percent of Expected (non-event participants)` = pct_expected_excluding_events,
           `Percent of Expected` = pct_expected,
           `Site` = facilitycode) %>%
    select(`Site`, `N (Participants)`, `Missing Time to Event (Participants)`, `Minimum (Days)`, `Maximum (Days)`, 
           `Mean (Standard Deviation)`, `Percent of Expected (non-event participants)`, `Percent of Expected`)
  
  vis <- kable(results, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}

#' Outcome by Name Overall
#'
#' @description 
#' Returns summary statistics on the number of days to complete various follow-up forms.
#'
#' @param analytic This is the analytic data set that must include study_id, outcome_data, and enrolled
#' @param days_since_dz optional numeric keyowrd argument to filter only for rows whose time_zero occured at leas that many days ago
#' @param window_start optional numeric, days since time zero at which the follow-up
#'   window opens. NULL means day 0.
#' @param window_end optional numeric, days since time zero at which the follow-up
#'   window closes. NULL means no upper bound.
#'
#' When either window bound is supplied a final column is added, reporting percent of
#' expected counting only the follow-up that falls inside the window. Both sides of the
#' ratio are clipped, so a participant with 400 observed days against 500 expected
#' contributes 275 of 275 to a 90 to 365 day window rather than 400 of 500.
#'
#' Both bounds are days since time zero, matching the units of every other column in
#' this table. They are not calendar dates.
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' outcome_by_name_overall("Replace with Analytic Tibble")
#'
outcome_by_name_overall <- function(analytic, days_since_tz = 365, header = FALSE, opt_col_order = FALSE,
                                    window_start = NULL, window_end = NULL) {
  analytic <- if_needed_generate_example_data(analytic, 
                                              example_constructs = c('outcome_data', 'enrolled'), 
                                              example_types = c("(';', ',')NamedCategory['test_outcome']|Number|Number|Date|Date|NamedCategory['check' 'event']|Number|Number|Date", 'Boolean'))
  
  outcome_data <- analytic %>%
    select(study_id, outcome_data, enrolled) %>%
    filter(enrolled) %>%
    separate_rows(outcome_data, sep=";") %>%
    separate(outcome_data, c('outcome_name', 'target_days', 'expected_days', 'time_zero', 'outcome_date_extended', 'outcome_type', 'outcome_days_extended', 'outcome_days', 'outcome_date'), sep=",") %>% 
    filter((as.Date(time_zero)+days_since_tz)<Sys.Date()) 

  stats <- outcome_data %>%
    mutate(outcome_days = as.numeric(outcome_days)) %>%
    mutate(target_days = as.numeric(target_days)) %>%
    mutate(expected_days = as.numeric(expected_days)) %>%
    group_by(outcome_name) %>%
    summarise(
      n_total = n(),
      n_missing = sum(is.na(outcome_days)),
      min_days = min(outcome_days, na.rm = TRUE),
      max_days = max(outcome_days, na.rm = TRUE),
      avg_days = format_mean_sd(outcome_days, decimals = 0),
      pct_expected = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%")
    )
  
  expected_non_event <- outcome_data %>%
    filter(!str_detect(outcome_type, 'event')) %>%
    group_by(outcome_name) %>%
    mutate(expected_days = as.numeric(expected_days)) %>%
    mutate(outcome_days = as.numeric(outcome_days)) %>%
    summarise(pct_expected_excluding_events = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%"))
  
  stats <- stats %>%
    left_join(expected_non_event)

  # Windowed percent of expected. Only the part of each participant's follow-up that
  # falls inside the window counts, and it is clipped on both sides of the ratio so
  # the denominator shrinks with the numerator.
  windowed <- NULL
  window_label <- NULL
  if (!is.null(window_start) || !is.null(window_end)) {
    window_lo <- if (is.null(window_start)) 0 else as.numeric(window_start)
    window_hi <- if (is.null(window_end)) Inf else as.numeric(window_end)

    if (window_hi <= window_lo) {
      stop("window_end must be greater than window_start")
    }

    clip_to_window <- function(days) pmax(0, pmin(days, window_hi) - window_lo)

    windowed <- outcome_data %>%
      mutate(outcome_days = as.numeric(outcome_days),
             expected_days = as.numeric(expected_days)) %>%
      group_by(outcome_name) %>%
      summarise(observed_in_window = sum(clip_to_window(outcome_days), na.rm = TRUE),
                expected_in_window = sum(clip_to_window(expected_days), na.rm = TRUE),
                .groups = 'drop') %>%
      # No expected follow-up inside the window means there is nothing to be a
      # percent of, which is different from 0%.
      mutate(window_pct = ifelse(expected_in_window > 0,
                                 paste0(round(observed_in_window / expected_in_window * 100, 0), "%"),
                                 "-")) %>%
      select(outcome_name, window_pct)

    window_label <- paste0("Percent of Expected (",
                           if (is.infinite(window_hi)) paste0("days ", window_lo, "+")
                           else paste0("days ", window_lo, "-", window_hi), ")")
  }

  results <- stats %>%
    rename(`N (Participants)` = n_total,
           `Missing Time to Event (Participants)` = n_missing,
           `Minimum (Days)` = min_days,
           `Maximum (Days)` = max_days,
           `Mean (Standard Deviation)` = avg_days,
           `Percent of Expected (non-event participants)` = pct_expected_excluding_events,
           `Percent of Expected` = pct_expected,
           `Outcome` = outcome_name) %>%
    select(`Outcome`, `N (Participants)`, `Missing Time to Event (Participants)`, `Minimum (Days)`, 
           `Maximum (Days)`, `Mean (Standard Deviation)`, `Percent of Expected (non-event participants)`, 
           `Percent of Expected`)
  
  if (opt_col_order) {
    results <- results %>%
      select(`Outcome`, `N (Participants)`, `Missing Time to Event (Participants)`, `Minimum (Days)`, 
             `Maximum (Days)`, `Mean (Standard Deviation)`, `Percent of Expected`, 
             `Percent of Expected (non-event participants)`)
  }

  # cleanup the names of the outcomes by replacing the underscores with spaces and capitalizing the first letter
  results <- results %>%
    mutate(Outcome = str_replace_all(Outcome, "_", " "))

  # Appended last, so it lands after whichever column order was requested.
  if (!is.null(windowed)) {
    results <- results %>%
      left_join(windowed %>%
                  rename(Outcome = outcome_name) %>%
                  mutate(Outcome = str_replace_all(Outcome, "_", " ")),
                by = "Outcome")
    names(results)[ncol(results)] <- window_label
  }

  vis <- kable(results, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')

  if (header) {
    # The windowed column is more person-days of follow-up time, so it extends that
    # span rather than adding one - otherwise the header widths stop summing to ncol
    # and add_header_above errors.
    col_vec <- if (is.null(windowed)) c(3,5) else c(3,6)
    name_vec <- c(" ", "Person-days of follow-up time")
    names(col_vec) <- name_vec

    vis <- vis %>%
      add_header_above(col_vec)
  }

  return(vis)
}


#' Number of Subjects Screened, Eligible, and Enrolled, by Consented and with Pre-Screening (Variable Discontinued)
#'
#' @description 
#' Visualizes the totals of each include construct by site, split into among eligible and among consented,
#' with consented between the pre screening and the screening stage
#' 
#' For other enrollment by site visualizations that may better fit your study, refer to: enrollment_by_site, 
#' enrollment_by_site_last_days_var_disc, enrollment_status_by_site, enrollment_status_by_site_var_discontinued
#'
#' @param analytic This is the analytic data set that must include screened, 
#' eligible, refused, consented, enrolled, not_consented, site_certification_date, facilitycode,
#' consent_date, not_randomized
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' enrollment_status_by_site_consent_pre_screening("Replace with Analytic Tibble")
#' 
enrollment_status_by_site_consent_pre_screening <- function(analytic, discontinued="discontinued", 
                                                       discontinued_colname="Discontinued", only_total=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "ineligible", "consented", "enrolled", "randomized",
                          "site_certification_date", "facilitycode", 'pre_screened', 'pre_eligible', "discontinued"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Date", "FacilityCode", "Date", "Boolean", "Boolean"))
  
  df <- analytic %>%
    select(screened, eligible, ineligible, consented, randomized, enrolled,
           site_certification_date, facilitycode, pre_screened, pre_eligible, any_of(discontinued)) %>%
    arrange(facilitycode)
  
  colnames(df)[which(names(df) == discontinued)] <- "discontinued"
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  df_1st <- df %>%
    group_by(Facility) %>%
    summarize(
      `Days Certified` = site_certified_days[1],
      `Pre-Operative Screened` = sum(pre_screened),
      `Pre-Operative Screened Eligible` = sum(pre_eligible),
      Consented = sum(consented))
  
  df_2nd <- df %>% 
    filter(consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(`Intra-Operative Screened` = sum(screened), Ineligible = sum(ineligible), Eligible = sum(eligible))
  
  df_3rd <- df %>% 
    filter(eligible == TRUE & consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(Randomized = sum(randomized),
              !!discontinued_colname := sum(discontinued),
              Enrolled = sum(enrolled)) 
  
  table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
    left_join(df_3rd, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,"-",`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(!!discontinued_colname := format_count_percent(!!sym(discontinued_colname), Eligible)) %>% 
    mutate(Randomized = format_count_percent(Randomized, Eligible)) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Eligible)) %>% 
    mutate(`Intra-Operative Screened` = format_count_percent(`Intra-Operative Screened`, Consented)) %>% 
    mutate(Ineligible = format_count_percent(Ineligible, Consented)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Consented))
  
  if(only_total){
    table_raw <- table_raw %>% filter(Facility=="Total")
  }
  
  header <- c(" " = 5, "Among Consented" = 3, "Among Eligible" = 3)
  
  table <- kable(table_raw, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position="left")
  return(table)
}

#' enrollment_status_by_site_consent_pre_screening_i
#'
#' @description 
#' Visualizes the totals of pre-screening, consent, intra-operative screening, and eligibility by site.
#' Structure: Pre-Screen -> Consent -> Intra-Op Screen -> Eligible.
#'
#' @param analytic Analytic data set. Must include: screened, eligible, ineligible, 
#' consented, facilitycode, site_certification_date, pre_screened, pre_eligible.
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
enrollment_status_by_site_consent_pre_screening_i <- function(analytic, only_total=FALSE){
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "ineligible", "consented", 
                           "site_certification_date", "facilitycode", 'pre_screened', 'pre_eligible'), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", 
                      "Date", "FacilityCode", "Boolean", "Boolean"))
  
  df <- analytic %>%
    select(screened, eligible, ineligible, consented,
           site_certification_date, facilitycode, pre_screened, pre_eligible) %>%
    arrange(facilitycode)
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    mutate(site_certified_days = as.numeric(Sys.Date() - as.Date(site_certification_date))) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  df_1st <- df %>%
    group_by(Facility) %>%
    summarize(
      `Days Certified` = site_certified_days[1],
      `Pre-Operative Screened` = sum(pre_screened),
      `Pre-Operative Screened Eligible` = sum(pre_eligible),
      Consented = sum(consented))
  
  df_2nd <- df %>% 
    filter(consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(`Intra-Operative Screened` = sum(screened), 
              Ineligible = sum(ineligible), 
              Eligible = sum(eligible))
  
  table_raw <- full_join(df_1st, df_2nd, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    mutate(`Days Certified`=ifelse(is_total,"-",`Days Certified`)) %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(`Intra-Operative Screened` = format_count_percent(`Intra-Operative Screened`, Consented)) %>% 
    mutate(Ineligible = format_count_percent(Ineligible, Consented)) %>% 
    mutate(Eligible = format_count_percent(Eligible, Consented))
  
  if(only_total){
    table_raw <- table_raw %>% filter(Facility=="Total")
  }
  
  header <- c(" " = 5, "Among Consented" = 3)
  
  table <- kable(table_raw, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position="left")
  
  return(table)
}

#' enrollment_status_by_site_consent_pre_screening_ii
#'
#' @description 
#' Visualizes the totals of randomization, discontinuation, and enrollment by site.
#' Values are calculated as percentages of the "Eligible" population.
#'
#' @param analytic Analytic data set. Must include: eligible, consented, randomized, 
#' enrolled, facilitycode, discontinued (or custom name).
#' @param discontinued meta construct for discontinued
#' @param discontinued_colname column name for discontinued to appear in visualization like "Adjudicated Discontinued"
#' @param only_total hide all the site specific rows
#'
#' @return An HTML table.
#' @export
enrollment_status_by_site_consent_pre_screening_ii <- function(analytic, discontinued="discontinued", 
                                                                  discontinued_colname="Discontinued", only_total=FALSE){
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("eligible", "consented", "enrolled", "randomized",
                           "facilitycode", "discontinued"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean",
                      "FacilityCode", "Boolean"))
  
  df <- analytic %>%
    select(eligible, consented, randomized, enrolled, facilitycode, any_of(discontinued)) %>%
    arrange(facilitycode)
  
  colnames(df)[which(names(df) == discontinued)] <- "discontinued"
  
  df <- df %>% 
    mutate_if(is.logical, ~ifelse(is.na(.), FALSE, .)) %>% 
    rename(Facility = facilitycode) %>% 
    filter(!is.na(Facility))
  
  df_eligible_totals <- df %>%
    filter(consented == TRUE) %>% 
    group_by(Facility) %>%
    summarize(Eligible_Count = sum(eligible))
  
  df_main <- df %>% 
    filter(eligible == TRUE & consented == TRUE) %>% 
    group_by(Facility) %>% 
    summarize(Randomized = sum(randomized),
              !!discontinued_colname := sum(discontinued),
              Enrolled = sum(enrolled)) 
  
  table_raw <- full_join(df_eligible_totals, df_main, by = 'Facility') %>% 
    mutate_all(~ifelse(is.na(.), 0, .)) %>% 
    adorn_totals("row") %>% 
    mutate(is_total=Facility=="Total") %>% 
    arrange(desc(is_total), Facility) %>% 
    select(-is_total) %>% 
    mutate(!!discontinued_colname := format_count_percent(!!sym(discontinued_colname), Eligible_Count)) %>% 
    mutate(Randomized = format_count_percent(Randomized, Eligible_Count)) %>% 
    mutate(Enrolled = format_count_percent(Enrolled, Eligible_Count)) %>%
    select(-Eligible_Count)
  
  if(only_total){
    table_raw <- table_raw %>% filter(Facility=="Total")
  }
  
  header <- c(" " = 1, "Among Eligible" = 3)
  
 table <- kable(table_raw, format="html", align='l') %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = F, position="left")
 
 return(table)
}

#' Weight Bearing ALL characteristics for Main paper
#'
#' @description This function outputs a table with the specified injury characteristics and patient characteristics
#' for enrolled patients with "Ankle" injuries. This table is produced for Weight bearing main paper. 
#'
#' @param analytic This is the analytic dataset that must include enrolled, injury_type,
#' sex, age, ethnicity_race, education_level,
#' preinjury_productive_activity, preinjury_work_demand, preinjury_work_hours,
#' tobacco_use, bmi, preinjury_health, insurance,
#' injury_gustilo, injury_classification_ankle_ota, soft_tissue_closure,
#' injury_mechanism, injury_randomization_days, pre_randomization_immobilization_type
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' wbs_main_paper_all_characteristics("Replace with Analytic Tibble")
#' 
wbs_main_paper_all_characteristics <- function(analytic){
  df <- analytic %>%
    select(enrolled, injury_type,
      sex, age, ethnicity_race, education_level,
      preinjury_productive_activity, preinjury_work_demand, preinjury_work_hours,
      tobacco_use, bmi, preinjury_health, insurance,
      injury_gustilo, injury_classification_ankle_ota, soft_tissue_closure,
      injury_mechanism, injury_randomization_days, pre_randomization_immobilization_type) %>%
    filter(enrolled) %>% 
    filter(injury_type == 'ankle')
  
  total <- nrow(df)

  df_age_missing <- df %>%
    select(age) %>%
    mutate(age = ifelse(is.na(age), "Missing", age)) %>%
    filter(age == "Missing") %>%
    count(age) %>%
    rename(heading = age) %>%
    mutate(Category = "Age",
           n = format_count_percent(n, total))
  
  df_age_stats <- df %>%
    select(age) %>%
    filter(!is.na(age)) %>%
    mutate(age = as.numeric(age)) %>%
    summarise(n = format_mean_sd(age)) %>%
    mutate(Category = "Age",
           heading  = "Mean (SD)")
  
  df_age_final <- rbind(df_age_stats, df_age_missing)
  
  df_sex <- df %>%
    count(sex) %>%
    rename(heading = sex) %>%
    mutate(Category = "Sex",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_race_ethnicity <- df %>%
    count(ethnicity_race) %>%
    rename(heading = ethnicity_race) %>%
    mutate(Category = "Race Ethnicity",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_education <- df %>%
    count(education_level) %>%
    rename(heading = education_level) %>%
    mutate(Category = "Education Level",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_usual_major_activity <- df %>%
    mutate(preinjury_productive_activity = ifelse(is.na(preinjury_productive_activity), "Missing", preinjury_productive_activity)) %>% 
    count(preinjury_productive_activity) %>%
    rename(heading = preinjury_productive_activity) %>%
    mutate(Category = "Major Activity",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_physical_demand <- df %>%
    count(preinjury_work_demand) %>%
    rename(heading = preinjury_work_demand) %>%
    mutate(Category = "Work Demand",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_work_hours_missing <- df %>%
    select(preinjury_work_hours) %>%
    mutate(preinjury_work_hours = ifelse(is.na(preinjury_work_hours), "Missing", preinjury_work_hours)) %>%
    filter(preinjury_work_hours == "Missing") %>%
    count(preinjury_work_hours) %>%
    rename(heading = preinjury_work_hours) %>%
    mutate(Category = "Work Hours",
           n = format_count_percent(n, total))
  
  df_work_hours <- df %>%
    select(preinjury_work_hours) %>%
    filter(!is.na(preinjury_work_hours)) %>%
    mutate(preinjury_work_hours = as.numeric(preinjury_work_hours)) %>%
    summarise(n = format_mean_sd(preinjury_work_hours)) %>%
    mutate(Category = "Work Hours",
           heading = "Mean (SD)")
  
  df_work_hours_final <- rbind(df_work_hours, df_work_hours_missing)
  
  df_tobacco <- df %>%
    count(tobacco_use) %>%
    rename(heading = tobacco_use) %>%
    mutate(Category = "Tobacco Use",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_bmi_missing <- df %>%
    select(bmi) %>%
    mutate(bmi = ifelse(is.na(bmi), "Missing", bmi)) %>%
    filter(bmi == "Missing") %>%
    count(bmi) %>%
    rename(heading = bmi) %>%
    mutate(Category = "BMI",
           n = format_count_percent(n, total))
  
  df_bmi <- df %>%
    select(bmi) %>%
    filter(!is.na(bmi)) %>%
    summarise(n = format_mean_sd(bmi)) %>%
    mutate(Category = "BMI",
           heading = "Mean (SD)")
  
  df_bmi_final <- rbind(df_bmi, df_bmi_missing)
  
  df_preinjury_health <- df %>%
    count(preinjury_health) %>%
    rename(heading = preinjury_health) %>%
    mutate(Category = "Health",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_insurance <- df %>%
    mutate(insurance = ifelse(insurance == TRUE, 'Yes', 'No')) %>% 
    count(insurance) %>%
    rename(heading = insurance) %>%
    mutate(Category = "Insurance",
           n = format_count_percent(n, total))
  
  df_injury_ota <- df %>%
    mutate(ota_class = ifelse(injury_classification_ankle_ota %in% c('44A2','44A3'), "44 A2/A3",
                              ifelse(injury_classification_ankle_ota %in% c('44B2','44B3'), "44 B2/B3",
                                     ifelse(injury_classification_ankle_ota %in% c('44C1','44C2','44C3'), "44 C1/C2/C3",
                                            injury_classification_ankle_ota)))) %>%
    count(ota_class) %>%
    rename(heading = ota_class) %>%
    mutate(Category = "OTA Classification",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_gustilo <- df %>%
    count(injury_gustilo) %>%
    rename(heading = injury_gustilo) %>%
    mutate(Category = "Gustilo Type",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_soft_tissue <- df %>%
    separate_rows(soft_tissue_closure, sep = ";") %>%
    mutate(soft_tissue_closure = recode(soft_tissue_closure, "Primary" = "Primary closure")) %>%
    count(soft_tissue_closure) %>%
    rename(heading = soft_tissue_closure) %>%
    mutate(Category = "Tissue Closure",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_mechanism_raw <- df %>%
    mutate(injury_mechanism = ifelse(
      str_starts(injury_mechanism, "Other"), "Other", injury_mechanism)) %>%
    count(injury_mechanism) %>%
    rename(heading = injury_mechanism) %>%
    mutate(Category = "Mechanism",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_mechanism_rest  <- df_mechanism_raw %>% filter(heading != "Other")
  df_mechanism_other <- df_mechanism_raw %>% filter(heading == "Other")
  df_mechanism <- rbind(df_mechanism_rest, df_mechanism_other)
  
  df_days_missing <- df %>%
    mutate(injury_randomization_days = ifelse(is.na(injury_randomization_days), "Missing", injury_randomization_days)) %>%
    filter(injury_randomization_days == "Missing") %>%
    count(injury_randomization_days) %>%
    rename(heading = injury_randomization_days) %>%
    mutate(Category = "Days",
           n = format_count_percent(n, total))
  
  df_days_stats <- df %>%
    filter(!is.na(injury_randomization_days)) %>%
    summarise(n = format_mean_sd(injury_randomization_days)) %>%
    mutate(Category = "Days",
           heading = "Mean (SD)")
  
  df_days_final <- rbind(df_days_stats, df_days_missing)
  
  df_immobilization <- df %>%
    count(pre_randomization_immobilization_type) %>%
    rename(heading = pre_randomization_immobilization_type) %>%
    mutate(Category = "Immobilization",
           heading = ifelse(is.na(heading), "Missing", heading),
           n = format_count_percent(n, total))
  
  df_final <- rbind(
    df_age_final, df_sex, df_race_ethnicity, df_education,
    df_usual_major_activity, df_physical_demand, df_work_hours_final,
    df_tobacco, df_bmi_final, df_preinjury_health, df_insurance,
    df_injury_ota, df_gustilo, df_soft_tissue,
    df_mechanism, df_days_final, df_immobilization)
  
  index_vec_a <- c(
    "Age" = nrow(df_age_final),
    "Sex" = nrow(df_sex),
    "Race Ethnicity" = nrow(df_race_ethnicity),
    "Education Level" = nrow(df_education),
    "Major Activity" = nrow(df_usual_major_activity),
    "Work Demand" = nrow(df_physical_demand),
    "Work Hours" = nrow(df_work_hours_final),
    "Tobacco Use" = nrow(df_tobacco),
    "BMI" = nrow(df_bmi_final),
    "Health" = nrow(df_preinjury_health),
    "Insurance" = nrow(df_insurance),
    "OTA Classification" = nrow(df_injury_ota),
    "Gustilo Type" = nrow(df_gustilo),
    "Tissue Closure" = nrow(df_soft_tissue),
    "Mechanism of Injury" = nrow(df_mechanism),
    "Days from Injury to Randomization" = nrow(df_days_final),
    "Pre-Randomization Immobilization" = nrow(df_immobilization))
  
  border_rows <- c(0, cumsum(index_vec_a))
  
  title <- paste("Total =", total)
  df_for_table <- df_final %>%
    select(heading, n) %>%
    rename("Characteristic" = heading) %>%
    rename(!!title := n)
  
  table_raw <- kable(df_for_table, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}


#' Nerve BPI for Main Paper
#'
#' @description This function outputs a table with the Brief Pain Inventory scores for enrolled patients. This table is produced for Nerve main paper. 
#'
#' @param analytic enrolled, 
#' bpi_severity_score_6wk, bpi_severity_score_3mo, bpi_severity_score_6mo, bpi_severity_score_12mo, 
#' bpi_interference_score_6wk, bpi_interference_score_3mo, bpi_interference_score_6mo, bpi_interference_score_12mo
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' nerve_main_paper_bpi("Replace with Analytic Tibble")
#' 
nerve_main_paper_bpi <- function(analytic){
  df <- analytic %>%
    select(enrolled, bpi_severity_score_3mo, bpi_severity_score_6mo, bpi_severity_score_12mo, bpi_severity_score_18mo, bpi_severity_score_24mo, 
           bpi_interference_score_3mo, bpi_interference_score_6mo, bpi_interference_score_12mo, bpi_interference_score_18mo, bpi_interference_score_24mo) %>%
    filter(enrolled)
  
  severity <- df %>% 
    select(bpi_severity_score_3mo, bpi_severity_score_6mo, bpi_severity_score_12mo, , bpi_severity_score_18mo, bpi_severity_score_24mo) %>% 
    pivot_longer(cols = starts_with("bpi_severity_score"),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              bpi_severity_score_3mo  = "3 Months",
                              bpi_severity_score_6mo  = "6 Months",
                              bpi_severity_score_12mo = "12 Months",
                              bpi_severity_score_18mo = "18 Months",
                              bpi_severity_score_24mo = "24 Months"))
  
  severity_mean_sd <- severity %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  severity_counts <- severity %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  severity_final <- left_join(severity_counts, severity_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("3 Months", "6 Months", "12 Months", "18 Months", "24 Months"))) %>% 
    arrange(timepoint)
  
  interference <- df %>% 
    select(bpi_interference_score_3mo, bpi_interference_score_6mo, bpi_interference_score_12mo,, bpi_interference_score_18mo, bpi_interference_score_24mo) %>% 
    pivot_longer(cols = starts_with("bpi_interference_score"),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              bpi_interference_score_3mo  = "3 Months",
                              bpi_interference_score_6mo  = "6 Months",
                              bpi_interference_score_12mo = "12 Months",
                              bpi_interference_score_18mo = "18 Months",
                              bpi_interference_score_24mo = "24 Months"))
  
  interference_mean_sd <- interference %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  interference_counts <- interference %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  interference_final <- left_join(interference_counts, interference_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("3 Months", "6 Months", "12 Months", "18 Months", "24 Months"))) %>% 
    arrange(timepoint)
  
  final <- rbind(severity_final, interference_final)
  
  colnames(final) <- c('', 'n', 'Overall Scores, Mean (SD)')
  
  index_vec_a <- c(
    "Pain Severity" = nrow(severity_final),
    "Pain Interference" = nrow(interference_final))
  
  border_rows <- c(0, cumsum(index_vec_a))
  
  table_raw <- kable(final, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}


#' Weight Bearing BPI for Main Paper
#'
#' @description This function outputs a table with the Brief Pain Inventory scores for enrolled patients. This table is produced for Weight bearing main paper. 
#'
#' @param analytic enrolled, 
#' bpi_severity_score_6wk, bpi_severity_score_3mo, bpi_severity_score_6mo, bpi_severity_score_12mo, 
#' bpi_interference_score_6wk, bpi_interference_score_3mo, bpi_interference_score_6mo, bpi_interference_score_12mo
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' wbs_main_paper_bpi("Replace with Analytic Tibble")
#' 
wbs_main_paper_bpi <- function(analytic){
  df <- analytic %>%
    select(enrolled, 
           bpi_severity_score_6wk, bpi_severity_score_3mo, bpi_severity_score_6mo, bpi_severity_score_12mo, 
           bpi_interference_score_6wk, bpi_interference_score_3mo, bpi_interference_score_6mo, bpi_interference_score_12mo) %>%
    filter(enrolled)
  
  severity <- df %>% 
    select(bpi_severity_score_6wk, bpi_severity_score_3mo, bpi_severity_score_6mo, bpi_severity_score_12mo) %>% 
    pivot_longer(cols = starts_with("bpi_severity_score"),
      names_to = "timepoint",
      values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
      bpi_severity_score_6wk  = "6 Weeks",
      bpi_severity_score_3mo  = "3 Months",
      bpi_severity_score_6mo  = "6 Months",
      bpi_severity_score_12mo = "12 Months"))
  
  severity_mean_sd <- severity %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
    
  severity_counts <- severity %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  severity_final <- left_join(severity_counts, severity_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("6 Weeks", "3 Months", "6 Months", "12 Months"))) %>% 
    arrange(timepoint)
  
  interference <- df %>% 
    select(bpi_interference_score_6wk, bpi_interference_score_3mo, bpi_interference_score_6mo, bpi_interference_score_12mo) %>% 
    pivot_longer(cols = starts_with("bpi_interference_score"),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              bpi_interference_score_6wk  = "6 Weeks",
                              bpi_interference_score_3mo  = "3 Months",
                              bpi_interference_score_6mo  = "6 Months",
                              bpi_interference_score_12mo = "12 Months"))
  
  interference_mean_sd <- interference %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  interference_counts <- interference %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  interference_final <- left_join(interference_counts, interference_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("6 Weeks", "3 Months", "6 Months", "12 Months"))) %>% 
    arrange(timepoint)
  
  final <- rbind(severity_final, interference_final)
  
  colnames(final) <- c('', 'n', 'Overall Scores, Mean (SD)')
  
  index_vec_a <- c(
    "Pain Severity" = nrow(severity_final),
    "Pain Interference" = nrow(interference_final))
  
  border_rows <- c(0, cumsum(index_vec_a))
  
  table_raw <- kable(final, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}


#' Weight Bearing AOS for Main Paper
#'
#' @description This function outputs a table with the AOS scores for enrolled patients. This table is produced for Weight bearing main paper. 
#'
#' @param analytic enrolled, 
#' aos_disability_score_injured_leg_12mo, aos_disability_score_injured_leg_3mo, aos_disability_score_injured_leg_6mo, aos_disability_score_injured_leg_6wk, 
#' aos_pain_score_injured_leg_12mo, aos_pain_score_injured_leg_3mo, aos_pain_score_injured_leg_6mo, aos_pain_score_injured_leg_6wk, 
#' aos_score_injured_leg_12mo, aos_score_injured_leg_3mo, aos_score_injured_leg_6mo, aos_score_injured_leg_6wk
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' wbs_main_paper_aos("Replace with Analytic Tibble")
#' 
wbs_main_paper_aos <- function(analytic){
  df <- analytic %>%
    select(enrolled, 
           aos_disability_score_injured_leg_12mo, aos_disability_score_injured_leg_3mo, aos_disability_score_injured_leg_6mo, aos_disability_score_injured_leg_6wk, 
           aos_pain_score_injured_leg_12mo, aos_pain_score_injured_leg_3mo, aos_pain_score_injured_leg_6mo, aos_pain_score_injured_leg_6wk, 
           aos_score_injured_leg_12mo, aos_score_injured_leg_3mo, aos_score_injured_leg_6mo, aos_score_injured_leg_6wk) %>%
    filter(enrolled)
  
  overall <- df %>% 
    select(aos_score_injured_leg_12mo, aos_score_injured_leg_3mo, aos_score_injured_leg_6mo, aos_score_injured_leg_6wk) %>% 
    pivot_longer(cols = starts_with("aos_score"),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              aos_score_injured_leg_6wk  = "6 Weeks",
                              aos_score_injured_leg_3mo  = "3 Months",
                              aos_score_injured_leg_6mo  = "6 Months",
                              aos_score_injured_leg_12mo = "12 Months"))
  
  overall_mean_sd <- overall %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  overall_counts <- overall %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  overall_final <- left_join(overall_counts, overall_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("6 Weeks", "3 Months", "6 Months", "12 Months"))) %>% 
    arrange(timepoint)
  
  disability <- df %>% 
    select(aos_disability_score_injured_leg_12mo, aos_disability_score_injured_leg_3mo, aos_disability_score_injured_leg_6mo, aos_disability_score_injured_leg_6wk) %>% 
    pivot_longer(cols = starts_with("aos_disability"),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              aos_disability_score_injured_leg_6wk  = "6 Weeks",
                              aos_disability_score_injured_leg_3mo  = "3 Months",
                              aos_disability_score_injured_leg_6mo  = "6 Months",
                              aos_disability_score_injured_leg_12mo = "12 Months"))
  
  disability_mean_sd <- disability %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  disability_counts <- disability %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  disability_final <- left_join(disability_counts, disability_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("6 Weeks", "3 Months", "6 Months", "12 Months"))) %>% 
    arrange(timepoint)
  
  pain <- df %>% 
    select(aos_pain_score_injured_leg_12mo, aos_pain_score_injured_leg_3mo, aos_pain_score_injured_leg_6mo, aos_pain_score_injured_leg_6wk) %>% 
    pivot_longer(cols = starts_with("aos_pain"),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              aos_pain_score_injured_leg_6wk  = "6 Weeks",
                              aos_pain_score_injured_leg_3mo  = "3 Months",
                              aos_pain_score_injured_leg_6mo  = "6 Months",
                              aos_pain_score_injured_leg_12mo = "12 Months"))
  
  pain_mean_sd <- pain %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  pain_counts <- pain %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  pain_final <- left_join(pain_counts, pain_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("6 Weeks", "3 Months", "6 Months", "12 Months"))) %>% 
    arrange(timepoint)
  
  final <- rbind(overall_final, disability_final, pain_final)
  
  colnames(final) <- c('', 'n', 'Overall Scores, Mean (SD)')
  
  index_vec_a <- c(
    "Overall Score" = nrow(overall_final),
    "Disability Score" = nrow(disability_final),
    "Pain Score"= nrow(pain_final))
  
  border_rows <- c(0, cumsum(index_vec_a))
  
  table_raw <- kable(final, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}



#' PROMIS stats by time
#'
#' @description 
#' Returns stat data on promis scores, including sample size, mean, and standard deviation
#' 
#' @param analytic enrolled, promis_pf 6wk - 12mo constructs, promis_pain_interference 6wk - 12mo constructs
#' 
#' @return An HTML table.
#' @export
#'
#' @examples
#' wbs_main_paper_aos("Replace with Analytic Tibble")
#' 
promis_stats_by_time <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('enrolled', 
                           'promis_pf_6wk', 'promis_pf_3mo', 'promis_pf_6mo', 'promis_pf_12mo', 
                           'promis_pain_interference_6wk', 'promis_pain_interference_3mo', 'promis_pain_interference_6mo', 
                           'promis_pain_interference_12mo'), 
    example_types = c("Boolean", "Number","Number","Number","Number","Number","Number","Number","Number"))
  
  df <- analytic %>%
    select(enrolled, 
           promis_pf_6wk, promis_pf_3mo, promis_pf_6mo, promis_pf_12mo, 
           promis_pain_interference_6wk, promis_pain_interference_3mo, promis_pain_interference_6mo, 
           promis_pain_interference_12mo) %>%
    filter(enrolled)
  
  pf <- df %>% 
    select(promis_pf_6wk, promis_pf_3mo, promis_pf_6mo, promis_pf_12mo) %>% 
    pivot_longer(cols = everything(),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              promis_pf_6wk  = "6 Weeks",
                              promis_pf_3mo  = "3 Months",
                              promis_pf_6mo  = "6 Months",
                              promis_pf_12mo = "12 Months"))
  
  pf_mean_sd <- pf %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  pf_counts <- pf %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  pf_final <- left_join(pf_counts, pf_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("6 Weeks", "3 Months", "6 Months", "12 Months"))) %>% 
    arrange(timepoint)
  
  pi <- df %>% 
    select(promis_pain_interference_6wk, promis_pain_interference_3mo, promis_pain_interference_6mo, promis_pain_interference_12mo) %>% 
    pivot_longer(cols = everything(),
                 names_to = "timepoint",
                 values_to = "score") %>%
    mutate(timepoint = recode(timepoint,
                              promis_pain_interference_6wk  = "6 Weeks",
                              promis_pain_interference_3mo  = "3 Months",
                              promis_pain_interference_6mo  = "6 Months",
                              promis_pain_interference_12mo = "12 Months"))
  
  pi_mean_sd <- pi %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>%
    summarise(n = format_mean_sd(score))
  
  pi_counts <- pi %>% 
    group_by(timepoint) %>% 
    filter(!is.na(score)) %>% 
    count(timepoint)
  
  pi_final <- left_join(pi_counts, pi_mean_sd, by = 'timepoint') %>% 
    mutate(timepoint = factor(timepoint, c("6 Weeks", "3 Months", "6 Months", "12 Months"))) %>% 
    arrange(timepoint)
  
  final <- rbind(pf_final, pi_final)
  
  colnames(final) <- c('', 'n', 'Overall Scores, Mean (SD)')
  
  index_vec_a <- c(
    "PROMIS Physical Function" = nrow(pf_final),
    "PROMIS Pain Interference" = nrow(pi_final))
  
  border_rows <- c(0, cumsum(index_vec_a))
  
  table_raw <- kable(final, format="html", align='l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")
  
  return(table_raw)
}





#' Survival Analysis Kaplan-Meier
#'
#' @description This function outputs a table with the specified injury characteristics and patient characteristics
#' for enrolled patients with "Ankle" injuries. This table is produced for Weight bearing main paper. 
#'
#' @param analytic This is the analytic dataset that must include enrolled
#' @param type_construct the name of the column of the analytic dataset that must include whether the outcome for that participant was a check or event
#' @param days_construct the name of the column of the analytic dataset that must include the number of days till check or event
#' @param outcome_length number of days for this outcome
#' @param pre_filter_construct defaults to NULL but can be used to filter the participants using the name or names of other columns of the analytic dataset
#' @param remove_zero_day_events defaults to TRUE removes zero day events to match STATA behavior
#' @param non_inferiority defaults to FALSE lowers confidence interval on difference to 90 percent
#' @param hazard_ratio adds a coxph hazard ratio and p value
#' @param arm_labels named chr vec, c("0" = "Early Weight bearing","1" = "Restricted Weight bearing")
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' survival_analysis_kaplan_meier("Replace with Analytic Tibble")
#' 
survival_analysis_kaplan_meier <- function(analytic, type_construct, days_construct, outcome_length, pre_filter_constructs=NULL, remove_zero_day_events=TRUE, non_inferiority=FALSE, hazard_ratio=FALSE, arm_labels = c(`0` = "Control", `1` = "Treatment"), outcome_label="Outcome"){
  # ── Prep data ───────────────────────────────────────────────────────────
  df <- analytic %>%
    filter(enrolled == 1) %>%
    rename(type = !!sym(type_construct),
           days = !!sym(days_construct)) %>%
    mutate(
      outcome = case_when(
        type == "event"  ~ 1,
        type == "check"  ~ 0,
        TRUE             ~ NA_real_
      ),
      days   = as.numeric(days),
      trt    = as.numeric(study_id) %% 2         # simple placeholder randomization
    )
  
  # optional additional filters
  if (!is.null(pre_filter_constructs)) {
    for (pre_filter_col in pre_filter_constructs) {
      df <- df %>% filter(!!sym(pre_filter_col)==TRUE)
    }
  }
  
  if (remove_zero_day_events) {
    df <- df %>% filter(!(outcome == 1 & days == 0))
  }
  # ── Kaplan–Meier fit ────────────────────────────────────────────────────
  fit <- survfit(Surv(days, outcome) ~ trt, data = df, conf.type="log-log")
  
  if(hazard_ratio){
    coxph_fit <- coxph(Surv(days, outcome) ~ trt, data = df)
    summary_coxph_fit <- summary(coxph_fit)
    hz_ratio <- summary_coxph_fit$coefficients[2]
    hz_p <- summary_coxph_fit$coefficients[5]
    hz_low <- summary_coxph_fit$conf.int[3]
    hz_high <- summary_coxph_fit$conf.int[4]
  }
  # ── Extract per-arm KM estimates & n ─────────────────────────────────────
  summary_fit <- summary(fit, times = outcome_length, extend = TRUE)
  
  # Pull out the stratum label (e.g. "trt=0") and keep only the number after "="
  arm_code <- as.integer(sub(".*=", "", summary_fit$strata))
  
  # Sample size straight from the data
  n_counts <- df %>% count(trt)         # tibble: trt | n
  
  est <- tibble(
    trt      = arm_code,
    surv     = summary_fit$surv,
    std.err  = summary_fit$std.err
  ) %>%
    left_join(n_counts, by = "trt") %>%     # add n
    arrange(trt)       
  
  # difference & 90 % CI
  surv_diff  <- est$surv[2] - est$surv[1]             
  se_diff    <- sqrt(summary_fit$std.err[2]^2 + summary_fit$std.err[1]^2)
  if(non_inferiority){
    zValue        <- qnorm(0.95)
    diff_text <- "Difference (two-sided 90% CI)"
  } else{
    zValue        <- qnorm(0.975)
    diff_text <- "Difference (two-sided 95% CI)"
  }
  lower95    <- summary_fit$lower
  upper95    <- summary_fit$upper
  diff_low   <- surv_diff - zValue * se_diff
  diff_high  <- surv_diff + zValue * se_diff
  
  # ── Build table ─────────────────────────────────────────────────────────
  make_cell <- function(p, lo, hi) {
    sprintf("%.1f (%.1f, %.1f)", 100 * p, 100 * lo, 100 * hi)
  }
  
  zero_col <- make_cell(est$surv[1], lower95[1], upper95[1])
  one_col  <- make_cell(est$surv[2], lower95[2], upper95[2])
  diff_col  <- sprintf("%.1f (%.1f, %.1f)",
                       100 * surv_diff, 100 * diff_low, 100 * diff_high)
  
  hdr_zero <- sprintf("%s (n=%d) (%%)", arm_labels["0"], est$n[1])
  hdr_one  <- sprintf("%s (n=%d) (%%)", arm_labels["1"], est$n[2])
  
  
  out_tbl <- tibble(
    " " = outcome_label,
    !!hdr_one    := one_col,
    !!hdr_zero   := zero_col,
    !!diff_text  := diff_col
  )
  
  header <- c(" " = 1, "Kaplan-Meier Estimate (95% CI)" = 2, "FAKE! Treatment effect" = 1)
  
  if(hazard_ratio){
    hz_col  <- sprintf("%.1f (%.1f, %.1f)",
                       hz_ratio, hz_low, hz_high)
    p_col <- hz_p
    out_tbl <- tibble(
      " " = outcome_label,
      !!hdr_one    := one_col,
      !!hdr_zero   := zero_col,
      !!diff_text  := diff_col,
      "Hazard Ratio" := hz_col,
      "P value" := p_col
    )
    
    header <- c(" " = 1, "Kaplan-Meier Estimate (95% CI)" = 2, "FAKE! Treatment effect" = 2, " " = 1)
  } 
  
  table <- kable(out_tbl, format = "html", align = "l") %>%
    add_header_above(header) %>% 
    kable_styling("striped", full_width = FALSE, position = "left")
  
  return(table)
}


#' recruitment_source_statistics
#'
#' @description This function outputs a table with the baseline statistics organized by all possible combinations of recruitment source and clinic.
#' This table is produced for TBI DSMB. 
#'
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' recruitment_source_statistics("Replace with Analytic Tibble")
#' 
recruitment_source_statistics <- function(analytic){
  data <- analytic %>% 
    select(study_id, recruitment_source, recruitment_clinic, build_clinical_lead, 
                              consented, randomized, pre_screened_ineligibility_reasons, ineligible) %>% 
    mutate(pre_screened_ineligible = ifelse(!is.na(pre_screened_ineligibility_reasons), TRUE, FALSE)) 
    
  bcl_total <- sum(data$build_clinical_lead, na.rm = TRUE)
  consented_total <- sum(data$consented, na.rm = TRUE)
  randomized_total <- sum(data$randomized, na.rm = TRUE)
  psinelg_total <- sum(data$pre_screened_ineligible, na.rm = TRUE)
  inelg_total <- sum(data$ineligible, na.rm = TRUE)
  
  results <- data %>% 
    select(-pre_screened_ineligibility_reasons) %>% 
    group_by(
      Source = recruitment_source,
      Clinic = recruitment_clinic) %>%
    summarise(
      `Build Clinical Lead` = format_count_percent(sum(build_clinical_lead, na.rm = TRUE), bcl_total),
      Consented = format_count_percent(sum(consented, na.rm = TRUE), consented_total),
      Randomized = format_count_percent(sum(randomized, na.rm = TRUE), randomized_total), 
      `Prescreened Ineligible` = format_count_percent(sum(pre_screened_ineligible, na.rm = TRUE), psinelg_total),
      Ineligible = format_count_percent(sum(ineligible, na.rm = TRUE), inelg_total),
      .groups = "drop")
    
  
  vis <- kable(results, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position='left')
  
  return(vis)
}


#' Hardware duration statistics
#'
#' @description 
#' Returns a table of buckets for the duration hardware was applied to the patient.
#' 
#' Candidate for general visualization which interprets numeric constructs into buckets
#'
#' @param analytic analytic data set that must include study_id, hardware_duration, hardware_delta, facilitycode
#' @param by_site breaks down hardware times by site
#' @param delta uses dates instead of datetimes
#'
#' @return html table
#' @export
#'
#' @examples
#' hardware_duration_statistics("Replace with Analytic Tibble")
#' 
hardware_duration_statistics <- function(analytic, delta = FALSE){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("hardware_duration", "hardware_delta"),
    example_types = c("Number-U150", "Number-U6")) 
  
  df1 <- analytic %>%  
    select(hardware_duration, hardware_delta, facilitycode) 
  
  if (delta) {
    df1 <- df1 %>% mutate(target = hardware_delta)
  } else {
    df1 <- df1 %>% mutate(target = hardware_duration)
  }
  
  filtered <- df1 %>%
    filter(!is.na(target)) %>% 
    filter(target > 0) %>%
    mutate(target = as.numeric(target))
  
  if (delta) {
    buckets <- c(1, 2, 3, 4)
  } else {
    buckets <- c(24, 48, 72, 96)
  }
  unit <- ifelse(delta, ' Days', ' Hours')
  
  labels <- c(paste0('Total < ', buckets[1], unit, ' (Nonadherent)'), paste0('Total >= ', buckets, unit))
  
  table <- tibble(
    `VAC Time Thresholds` = c('Total', labels),
    N = c(
      nrow(filtered),
      format_count_percent(nrow(filtered %>% filter(target < buckets[1])), nrow(filtered)),
      format_count_percent(nrow(filtered %>% filter(target >= buckets[1])), nrow(filtered)),
      format_count_percent(nrow(filtered %>% filter(target >= buckets[2])), nrow(filtered)),
      format_count_percent(nrow(filtered %>% filter(target >= buckets[3])), nrow(filtered)),
      format_count_percent(nrow(filtered %>% filter(target >= buckets[4])), nrow(filtered)))
  )
  
  output <- kable(table, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}


#' Hardware duration statistics by site
#'
#' @description 
#' Returns a table of buckets for the duration hardware was applied to the patient, organized
#' by site.
#' 
#' Candidate for general visualization which interprets numeric constructs into buckets
#'
#' @param analytic analytic data set that must include study_id, hardware_duration, hardware_delta, facilitycode
#' @param by_site breaks down hardware times by site
#' @param delta uses dates instead of datetimes
#'
#' @return html table
#' @export
#'
#' @examples
#' \dontrun{
#' hardware_duration_statistics_by_site("Replace with Analytic Tibble")
#' }
hardware_duration_statistics_by_site <- function(analytic, delta = FALSE){
  if (delta) {
    df1 <- analytic %>%  
      select(hardware_application_date, hardware_application_datetime, hardware_removal_date, hardware_removal_datetime, 
             hardware_removal_date_missing, hardware_duration, hardware_delta, facilitycode, enrolled) 
    df1 <- df1 %>% mutate(start = hardware_application_date, end = hardware_removal_date, len = hardware_delta)
  } else {
    df1 <- analytic %>%  
      select(hardware_application_date, hardware_application_datetime, hardware_removal_date, hardware_removal_datetime, 
             hardware_removal_date_missing, hardware_duration, facilitycode, enrolled) 
    df1 <- df1 %>% mutate(start = hardware_application_datetime, end = hardware_removal_datetime, len = hardware_duration)
  }
  
  sites <- df1 %>%
    filter(enrolled %in% TRUE) %>%
    count(facilitycode, name = "n_enrolled") %>%
    filter(n_enrolled >= 5) %>%
    pull(facilitycode)
  
  df1 <- df1 %>% select(-enrolled) %>% filter(facilitycode %in% sites)
  
  sites <- c('Total', sites)
  
  site_list <- list()
  for (site in sites) {
    if (site != 'Total') {
      site_data <- df1 %>%
        filter(facilitycode == site)
    } else {
      site_data <- df1
    }

    app <- site_data %>%
      filter(!is.na(start)) %>%
      nrow()
    intreatment <- site_data %>%
      filter(!is.na(start)&is.na(end)&!hardware_removal_date_missing) %>%
      nrow()
    missing <- site_data %>%
      filter(!is.na(start)&hardware_removal_date_missing) %>%
      nrow()
    rem <- site_data %>%
      filter(!is.na(start)&!is.na(end)) %>%
      nrow()
    
    len_vec <- site_data %>%
      pull(len) %>%
      as.numeric
    len_vec <- len_vec[!is.na(len_vec)]
    len_vec <- len_vec[len_vec > 0]
    mean <- len_vec %>%
      mean() %>%
      round(2)
    med <- len_vec %>%
      median()
    
    site_list[[site]] <- c(app, intreatment, missing, rem, mean, med)
  }
  
  unit <- ifelse(delta, ' (Days)', ' (Hours)')
  
  table <- tibble(
    Site = sites,
    `N patients with VAC placed` = sapply(site_list, `[`, 1),
    `N assumed to be in treatment` = sapply(site_list, `[`, 2),
    `N patients with missing removal date, confirmed by RC` = sapply(site_list, `[`, 3),
    `N patients with calculable vac time means/medians` = sapply(site_list, `[`, 4),
    `Mean VAC time` = sapply(site_list, `[`, 5),
    `Median VAC time` = sapply(site_list, `[`, 6)
  )
  
  names(table)[6:7] <- paste0(c('Mean VAC time', 'Median VAC time'), unit)
  
  output <- kable(table, format="html", align='l') %>%
    kable_styling("striped", full_width = F, position="left") 
  
  return(output)
}

#' Overall complications
#'
#' @description 
#' Returns a table of the overall complications first ordered by complication alphabetically, then by relatedness (starting with most related), 
#' then by severity (starting with most severe), each row is a unique combination of those items
#'
#' @param analytic analytic data set that must include study_id, complication_data
#' @param relatedness includes that column
#' @param WB if the study is Weight Bearing
#' @param breakout_other If TRUE, replaces "Other" with "Other: [other_info]". Defaults to FALSE.
#' @param cols_spec List of column names and their replacements, valid names are Complication Relatedness and Severity
#'
#' @return html table
#' @export
#'
#' @examples
#' overall_complications("Replace with Analytic Tibble")
overall_complications <- function(analytic, relatedness = TRUE, WB = NULL, breakout_other = FALSE, cols_spec = NULL){

    analytic <- if_needed_generate_example_data(
      analytic,
      example_constructs = "complication_data",
      example_types = "(';new_row: ', '|')FollowupPeriod|Character|Character|NamedCategory['Superficial-infection' 'Deep-Infection' 'Deep-Infection, Not Involving Bone' 'Deep-Infection, Septic Joint' 'Non-Union' 'Malunion' 'Loss of limb/amputation' 'Fixation failure' 'Peri-implant Fracture' 'Reaction to Hardware' 'Wound Dehiscence' 'Wound Seroma/Hematoma' 'Flap failure' 'Tendon Injury' 'Delayed Wound Healing' 'Cellulitis' 'DVT/PE' 'Joint Arthritis' 'Other']|Character|Date|NamedCategory['Definitely related' 'Probably related' 'Possibly related' 'Unlikely related' 'Unrelated' \"Don't know\"]|NamedCategory['Mild' 'Moderate' 'Severe and Undesirable' 'Life-threatening or disabling' 'Fatal']|NamedCategory['Operative' 'Non-operative' 'No treatment']|Character"
    )

    if (is.null(WB)) {
      df <- analytic %>%
        select(study_id, complication_data) %>% 
        separate_rows(complication_data, sep = ';new_row: ') %>%
        separate(complication_data, into = c("redcap_event_name", "form_name", "event_type",
                                             "complication", "notes", "diagnosis_date", "relatedness_val",
                                             "severity_val", "treatment", "other_info"), sep = '\\|', fill = "right")
    } else {
      df <- analytic %>%
        select(study_id, complication_data) %>% 
        separate_rows(complication_data, sep = ';new_row: ') %>%
        separate(complication_data, into = c("redcap_event_name", "visit_date", "complication", "diagnosis_date", 
                                             "relatedness_val", "severity_val", "treatment_related", "new_or_previous_diagnosis", 
                                             "form_notes", "other_info"), sep = '\\|', fill = "right")
    }
    
    rel_levels <- c("Definitely related", 
                    "Probably related", 
                    "Possibly related", 
                    "Unlikely related", 
                    "Unrelated", 
                    "Don't know")
    
    sev_levels <- c("Mild", 
                    "Moderate", 
                    "Severe and Undesirable", 
                    "Life-threatening or disabling", 
                    "Fatal")
    
    clean_df <- df %>%
      filter(!is.na(complication)) %>% 
      mutate(complication = str_trim(complication),
             relatedness_val = str_trim(relatedness_val),
             severity_val = str_trim(severity_val),
             other_info = str_trim(other_info)) %>%
      mutate(across(c(relatedness_val, severity_val), ~na_if(., ""))) %>%
      mutate(relatedness_val = factor(relatedness_val, levels = rel_levels), 
             severity_val = factor(severity_val, levels = sev_levels))
    
    if (breakout_other) {
      clean_df <- clean_df %>%
        mutate(complication = case_when(
          complication == "Other" & !is.na(other_info) & other_info != "" ~ paste0("Other: ", other_info),
          TRUE ~ complication))
    }
    
    if (relatedness) {
      table_data <- clean_df %>%
        group_by(complication, relatedness_val, severity_val) %>%
        summarise(N = n(), 
                  PTs = n_distinct(study_id), 
                  .groups = 'drop') %>%
        arrange(str_detect(complication, "^Other"),
                complication,
                relatedness_val,
                desc(severity_val))
      
    } else {
      table_data <- clean_df %>%
        group_by(complication, severity_val) %>%
        summarise(N = n(), 
                  PTs = n_distinct(study_id),
                  .groups = 'drop') %>%
        arrange(str_detect(complication, "^Other"),
                complication, 
                desc(severity_val))
    }
    
    final_table <- table_data %>%
      mutate(`N[PTs]` = sprintf("%d[%d]", N, PTs)) %>%
      select(-N, -PTs) %>%
      rename(`Complication` = complication,
             `Severity` = severity_val)
    
    if(relatedness) {
      final_table <- final_table %>% rename(`Relatedness` = relatedness_val)
    }
    
    if (!is.null(cols_spec)) {
      final_table <- final_table %>%
        rename_with(~ unlist(cols_spec)[.x], .cols = names(cols_spec))
    }
    
    output <- kable(final_table, format = "html", align = 'l') %>%
      kable_styling("striped", full_width = F, position = "left") 
    
    return(output)
}


# Required packages: kableExtra, janitor, dplyr, tidyr, htmltools
# These are typically already loaded by VisualizationLibrary

#' iVAC Invoice Report Table
#'
#' @description Generates an invoice report table for the iVAC study showing
#' baseline and follow-up visit payments for each participant by site.
#' 
#' Baseline Visit: $417.50 when CRFs 03-07 are complete
#' Follow-up Visits: $104.38 per completed visit (2wk, 6wk, 3mo, 6mo), 
#' paid when AF01 is complete with fsf_agreement=1
#' 
#' @param analytic A data frame containing the required construct columns:
#'   - study_id
#'   - facilitycode
#'   - payment_baseline
#'   - payment_2wk
#'   - payment_6wk
#'   - payment_3mo
#'   - payment_6mo
#'   - enrolled (optional, for filtering)
#' @param facilitycodes Optional character vector of facility codes to filter by.
#'   If NULL, shows all facilities.
#' @param show_all_enrolled If TRUE, shows all enrolled participants even if no
#'   payments are due. If FALSE (default), only shows participants with at least
#'   one payment.
#'   
#' @return An HTML kable table suitable for reports.
#' @export
#'
#' @examples
#' \dontrun{
#' }
ivac_invoice_report <- function(analytic, 
                                facilitycodes = NULL, 
                                show_all_enrolled = FALSE) {
  
  # Validate required columns
  required_cols <- c("study_id", "facilitycode", "payment_baseline", 
                     "payment_2wk", "payment_6wk", "payment_3mo", "payment_6mo")
  missing_cols <- required_cols[!required_cols %in% names(analytic)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter by enrolled if column exists
  if ("enrolled" %in% names(analytic)) {
    analytic <- analytic %>% filter(enrolled == TRUE)
  }
  
  # Filter by facilitycode if specified
  if (!is.null(facilitycodes)) {
    analytic <- analytic %>% filter(facilitycode %in% facilitycodes)
  }
  
  # Helper function to parse payment string "amount;description" -> amount
  parse_payment_amount <- function(payment_string) {
    if (is.na(payment_string) || payment_string == "") {
      return(0)
    }
    parts <- strsplit(as.character(payment_string), ";")[[1]]
    return(as.numeric(parts[1]))
  }
  
  # Helper function to format payment status
  format_payment_status <- function(payment_string) {
    if (is.na(payment_string) || payment_string == "") {
      return("Incomplete")
    }
    return("Complete")
  }
  
  # Process the data
  invoice_df <- analytic %>%
    select(study_id, facilitycode, payment_baseline, 
           payment_2wk, payment_6wk, payment_3mo, payment_6mo) %>%
    filter(!is.na(facilitycode)) %>%
    mutate(
      # Parse amounts
      baseline_amount = sapply(payment_baseline, parse_payment_amount),
      fu_2wk_amount = sapply(payment_2wk, parse_payment_amount),
      fu_6wk_amount = sapply(payment_6wk, parse_payment_amount),
      fu_3mo_amount = sapply(payment_3mo, parse_payment_amount),
      fu_6mo_amount = sapply(payment_6mo, parse_payment_amount),
      
      # Parse statuses
      baseline_status = sapply(payment_baseline, format_payment_status),
      fu_2wk_status = sapply(payment_2wk, format_payment_status),
      fu_6wk_status = sapply(payment_6wk, format_payment_status),
      fu_3mo_status = sapply(payment_3mo, format_payment_status),
      fu_6mo_status = sapply(payment_6mo, format_payment_status),
      
      # Calculate totals
      followup_total = fu_2wk_amount + fu_6wk_amount + fu_3mo_amount + fu_6mo_amount,
      total_payment = baseline_amount + followup_total,
      
      # Count completed follow-ups
      completed_followups = (fu_2wk_status == "Complete") + 
        (fu_6wk_status == "Complete") + 
        (fu_3mo_status == "Complete") + 
        (fu_6mo_status == "Complete")
    )
  
  # Filter to only show participants with payments (unless show_all_enrolled)
  if (!show_all_enrolled) {
    invoice_df <- invoice_df %>% 
      filter(total_payment > 0)
  }
  
  # Create the detail table (one row per participant)
  detail_table <- invoice_df %>%
    mutate(
      Facility = facilitycode,
      `Study ID` = study_id,
      `Baseline Visit ($417.50)` = baseline_status,
      `2wk F/U` = fu_2wk_status,
      `6wk F/U` = fu_6wk_status,
      `3mo F/U` = fu_3mo_status,
      `6mo F/U` = fu_6mo_status,
      `F/U Count` = paste0(completed_followups, "/4"),
      `F/U Payment` = ifelse(followup_total > 0, 
                             paste0("$", format(followup_total, nsmall = 2)),
                             "-"),
      `Total Due` = ifelse(total_payment > 0,
                           paste0("$", format(total_payment, nsmall = 2)),
                           "-")
    ) %>%
    select(Facility, `Study ID`, `Baseline Visit ($417.50)`, 
           `2wk F/U`, `6wk F/U`, `3mo F/U`, `6mo F/U`,
           `F/U Count`, `F/U Payment`, `Total Due`) %>%
    arrange(Facility, `Study ID`)
  
  # Create summary by site
  summary_table <- invoice_df %>%
    group_by(facilitycode) %>%
    summarize(
      `Participants` = n(),
      `Baseline Complete` = sum(baseline_status == "Complete"),
      `Baseline Payment` = sum(baseline_amount),
      `F/U Complete` = sum(completed_followups),
      `F/U Payment` = sum(followup_total),
      `Total Payment` = sum(total_payment),
      .groups = 'drop'
    ) %>%
    rename(Facility = facilitycode) %>%
    adorn_totals("row") %>%
    mutate(
      `Baseline Payment` = paste0("$", format(`Baseline Payment`, nsmall = 2)),
      `F/U Payment` = paste0("$", format(`F/U Payment`, nsmall = 2)),
      `Total Payment` = paste0("$", format(`Total Payment`, nsmall = 2))
    )
  
  # Create the combined output
  # First the summary table
  summary_html <- kable(summary_table, format = "html", align = "l") %>%
    add_header_above(c(" " = 2, "Baseline" = 2, "Follow-up" = 2, " " = 1)) %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    row_spec(nrow(summary_table), bold = TRUE, background = "#f0f0f0")
  
  # Then the detail table  
  header <- c(1, 1, 1, 4, 3)
  names(header) <- c(" ", " ", "Baseline", "Follow-up Visits ($104.38 each)", "Payment")
  
  detail_html <- kable(detail_table, format = "html", align = "l") %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = FALSE, position = "left")
  
  # Title section
  title_html <- paste0(
    "<h3>iVAC Invoice Report</h3>",
    "<p><strong>Payment Schedule:</strong></p>",
    "<ul>",
    "<li>Baseline Visit: $417.50 (when CRFs 03-07 complete)</li>",
    "<li>Follow-up Visits: $104.38 per completed visit (2wk, 6wk, 3mo, 6mo)</li>",
    "<li>Follow-up payments triggered when AF01 complete with fsf_agreement=1</li>",
    "</ul>",
    "<h4>Summary by Site</h4>"
  )
  
  detail_title <- "<h4>Detail by Participant</h4>"
  
  # Combine all HTML
  full_html <- paste0(
    title_html,
    as.character(summary_html),
    detail_title,
    as.character(detail_html)
  )
  
  return(htmltools::HTML(full_html))
}


#' iVAC Invoice Summary Table
#'
#' @description Generates a simplified invoice summary table showing payment 
#' totals by site for the iVAC study. This is a more compact version suitable
#' for monthly reports.
#' 
#' @param analytic A data frame containing the required construct columns.
#' @param facilitycodes Optional character vector of facility codes to filter.
#'   
#' @return An HTML kable table suitable for reports.
#' @export
#'
#' @examples
#' \dontrun{
#' }
ivac_invoice_summary <- function(analytic, facilitycodes = NULL) {
  
  # Validate required columns
  required_cols <- c("study_id", "facilitycode", "payment_baseline", 
                     "payment_2wk", "payment_6wk", "payment_3mo", "payment_6mo")
  missing_cols <- required_cols[!required_cols %in% names(analytic)]
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Filter by enrolled if column exists
  if ("enrolled" %in% names(analytic)) {
    analytic <- analytic %>% filter(enrolled == TRUE)
  }
  
  # Filter by facilitycode if specified
  if (!is.null(facilitycodes)) {
    analytic <- analytic %>% filter(facilitycode %in% facilitycodes)
  }
  
  # Helper function to parse payment string
  parse_payment_amount <- function(payment_string) {
    if (is.na(payment_string) || payment_string == "") {
      return(0)
    }
    parts <- strsplit(as.character(payment_string), ";")[[1]]
    return(as.numeric(parts[1]))
  }
  
  # Process the data
  invoice_df <- analytic %>%
    filter(!is.na(facilitycode)) %>%
    mutate(
      baseline_amount = sapply(payment_baseline, parse_payment_amount),
      fu_2wk_amount = sapply(payment_2wk, parse_payment_amount),
      fu_6wk_amount = sapply(payment_6wk, parse_payment_amount),
      fu_3mo_amount = sapply(payment_3mo, parse_payment_amount),
      fu_6mo_amount = sapply(payment_6mo, parse_payment_amount),
      followup_total = fu_2wk_amount + fu_6wk_amount + fu_3mo_amount + fu_6mo_amount,
      total_payment = baseline_amount + followup_total
    )
  
  # Create summary by site
  summary_table <- invoice_df %>%
    group_by(facilitycode) %>%
    summarize(
      `Enrolled` = n(),
      `Baseline ($417.50)` = paste0(sum(baseline_amount > 0), " ($", 
                                    format(sum(baseline_amount), nsmall = 2), ")"),
      `2wk ($104.38)` = paste0(sum(fu_2wk_amount > 0), " ($", 
                               format(sum(fu_2wk_amount), nsmall = 2), ")"),
      `6wk ($104.38)` = paste0(sum(fu_6wk_amount > 0), " ($", 
                               format(sum(fu_6wk_amount), nsmall = 2), ")"),
      `3mo ($104.38)` = paste0(sum(fu_3mo_amount > 0), " ($", 
                               format(sum(fu_3mo_amount), nsmall = 2), ")"),
      `6mo ($104.38)` = paste0(sum(fu_6mo_amount > 0), " ($", 
                               format(sum(fu_6mo_amount), nsmall = 2), ")"),
      `Total Due` = paste0("$", format(sum(total_payment), nsmall = 2)),
      .groups = 'drop'
    ) %>%
    rename(Facility = facilitycode)
  
  # Add totals row
  totals <- invoice_df %>%
    summarize(
      Facility = "TOTAL",
      `Enrolled` = n(),
      `Baseline ($417.50)` = paste0(sum(baseline_amount > 0), " ($", 
                                    format(sum(baseline_amount), nsmall = 2), ")"),
      `2wk ($104.38)` = paste0(sum(fu_2wk_amount > 0), " ($", 
                               format(sum(fu_2wk_amount), nsmall = 2), ")"),
      `6wk ($104.38)` = paste0(sum(fu_6wk_amount > 0), " ($", 
                               format(sum(fu_6wk_amount), nsmall = 2), ")"),
      `3mo ($104.38)` = paste0(sum(fu_3mo_amount > 0), " ($", 
                               format(sum(fu_3mo_amount), nsmall = 2), ")"),
      `6mo ($104.38)` = paste0(sum(fu_6mo_amount > 0), " ($", 
                               format(sum(fu_6mo_amount), nsmall = 2), ")"),
      `Total Due` = paste0("$", format(sum(total_payment), nsmall = 2))
    )
  
  summary_table <- bind_rows(summary_table, totals)
  
  # Create kable output
  header <- c(1, 1, 1, 4, 1)
  names(header) <- c(" ", " ", "Baseline", "Follow-up Visits", " ")
  
  table <- kable(summary_table, format = "html", align = "l") %>%
    add_header_above(header) %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    row_spec(nrow(summary_table), bold = TRUE, background = "#f0f0f0")
  
  return(table)
}


#' Pathogen Characteristics
#'
#' @description 
#' Visualizes the breakdown of the dssi_data long file, uses helper constructs to get counts of everything.
#'
#' @param analytic This is the analytic data set that must include dssi_data
#'
#' @return An HTML table styled with kableExtra.
#' @export
#'
#' @examples
#' \dontrun{
#' }
pathogen_characteristics <- function(analytic){
  inner_analytic <- analytic %>% filter(enrolled == TRUE)
  enrolled_tot <- nrow(inner_analytic)
  
  long_dssi <- inner_analytic %>%
    select(study_id, dssi_data) %>%
    separate_rows(dssi_data, sep = ';;') %>%
    separate(dssi_data, into = c("redcap_repeat_instance", "date", "culture",
                                 "group", "id", "organism"), sep = ',,')
  
  cons <- inner_analytic %>%
    select(study_id, deep_ssi_any_gram_negative,
           deep_ssi_any_gram_positive, deep_ssi_polymicrobrial, deep_ssi_no_growth)
  
  ids <- tibble(
    `Pathogen Type` = c("Any gram-positive organism", "Any gram-negative organism", "Polymicrobrial infection",
                        "Culture negative", "Missing"),
    `N (% Enrolled)` = c(format_count_percent(sum(cons$deep_ssi_any_gram_positive, na.rm=TRUE), enrolled_tot),
                         format_count_percent(sum(cons$deep_ssi_any_gram_negative, na.rm=TRUE), enrolled_tot),
                         format_count_percent(sum(cons$deep_ssi_polymicrobrial, na.rm=TRUE), enrolled_tot),
                         format_count_percent(sum(cons$no_growth, na.rm=TRUE), enrolled_tot),
                         format_count_percent(nrow(long_dssi %>% filter(is.na(id))), enrolled_tot))
  )
  
  top_microbes <- long_dssi %>%
    filter(organism!="NA" & (id=='gram_positive'|id=='gram_negative'|id=='enterococci'|
                               id=='staphylococci'|id=='streptococci'|id == 'candida'))
  
  top_positive <- top_microbes %>%
    filter(id=='gram_positive'|id=='enterococci'|id=='staphylococci'|id=='streptococci'|
             id == 'candida') %>%
    mutate(id = (str_to_title(str_replace_all(id, '_', ' ')))) %>%
    mutate(organism = paste0(id, ': ', organism)) %>%
    count(organism) %>%
    arrange(desc(n)) %>%
    slice_head(n=5) %>%
    rename(`Top Pathogens Detected (Gram Positive)`=organism) %>%
    rename("(N)"=n)
  
  top_negative <- top_microbes %>%
    filter(id=='gram_negative') %>%
    mutate(id = (str_to_title(str_replace_all(id, '_', ' ')))) %>%
    mutate(organism = paste0(id, ': ', organism)) %>%
    count(organism) %>%
    arrange(desc(n)) %>%
    slice_head(n=5) %>%
    rename(`Top Pathogens Detected (Gram Negative)`=organism) %>%
    rename("(N)"=n)
  
  out <- cbind(ids, top_positive, top_negative)
  
  table <- kable(out, format = "html", align = 'l') %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>% 
    column_spec(3, extra_css = "border-left: 1px solid black;")
  return(table)
}

#' Persistent pain
#'
#' @description
#' Returns the persistent pain shell for a trial's secondary outcomes: BPI severity
#' and BPI interference at 3, 6 and 12 months, reported as n, mean (SD), and the
#' proportion with severe pain. Severe is a subscale score of 7 or above, per SAP
#' section 11.2, which classifies 0-6 as mild or moderate and 7-10 as severe.
#'
#' @param analytic enrolled, bpi_severity_score and bpi_interference_score 3mo - 12mo constructs
#' @param include_severe include the categorised Severe (7-10) column (defaults to TRUE).
#' Set FALSE for a trial whose SAP analyses BPI only as a continuous scale.
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' persistent_pain("Replace with Analytic Tibble")
#'
persistent_pain <- function(analytic, include_severe = TRUE){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c('enrolled',
                           'bpi_severity_score_3mo', 'bpi_severity_score_6mo', 'bpi_severity_score_12mo',
                           'bpi_interference_score_3mo', 'bpi_interference_score_6mo',
                           'bpi_interference_score_12mo'),
    example_types = c("Boolean", "Number", "Number", "Number", "Number", "Number", "Number"))

  df <- analytic %>%
    select(enrolled,
           bpi_severity_score_3mo, bpi_severity_score_6mo, bpi_severity_score_12mo,
           bpi_interference_score_3mo, bpi_interference_score_6mo, bpi_interference_score_12mo) %>%
    filter(enrolled)

  inner_data_extractor <- function(prefix, inner_df) {
    recode_map <- setNames(c("3 Months", "6 Months", "12 Months"),
                           paste0(prefix, c("3mo", "6mo", "12mo")))

    long <- inner_df %>%
      select(paste0(prefix, c("3mo", "6mo", "12mo"))) %>%
      pivot_longer(cols = everything(), names_to = "timepoint", values_to = "score") %>%
      mutate(timepoint = recode(timepoint, !!!recode_map),
             score = as.numeric(score)) %>%
      filter(!is.na(score))

    stats <- long %>%
      group_by(timepoint) %>%
      summarise(n = n(),
                mean_sd = format_mean_sd(score),
                # SAP 11.2: severe pain is a subscale score of 7 to 10.
                severe = paste0(sum(score >= 7), " (",
                                trimws(format(round(100 * mean(score >= 7), 1), nsmall = 1)), "%)"),
                .groups = 'drop')

    stats %>%
      mutate(timepoint = factor(timepoint, c("3 Months", "6 Months", "12 Months"))) %>%
      arrange(timepoint)
  }

  sev_final <- inner_data_extractor('bpi_severity_score_', df)
  int_final <- inner_data_extractor('bpi_interference_score_', df)

  final <- rbind(sev_final, int_final)

  if (!include_severe) {
    final <- final %>% select(-severe)
  }

  colnames(final) <- if (include_severe) {
    c('', 'n', 'Score, Mean (SD)', 'Severe (7-10), n (%)')
  } else {
    c('', 'n', 'Score, Mean (SD)')
  }

  index_vec_a <- c("BPI Severity" = nrow(sev_final),
                   "BPI Interference" = nrow(int_final))

  border_rows <- c(0, cumsum(index_vec_a))

  table_raw <- kable(final, format = "html", align = 'l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")

  return(table_raw)
}


#' Opioid days
#'
#' @description
#' Returns the opioid utilisation shell for a trial's secondary outcomes: total days
#' of reported opioid use at baseline, 3, 6 and 12 months, reported as n and
#' mean (SD). Opioid use is defined in SAP section 11.2 as the total days of
#' patient-reported opioid use.
#'
#' @param analytic enrolled, opioid_days baseline - 12mo constructs
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' opioid_days("Replace with Analytic Tibble")
#'
opioid_days <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c('enrolled', 'opioid_days_baseline', 'opioid_days_3mo',
                           'opioid_days_6mo', 'opioid_days_12mo'),
    example_types = c("Boolean", "Number", "Number", "Number", "Number"))

  df <- analytic %>%
    select(enrolled, opioid_days_baseline, opioid_days_3mo, opioid_days_6mo, opioid_days_12mo) %>%
    filter(enrolled)

  inner_data_extractor <- function(inner_df) {
    recode_map <- setNames(c("Baseline", "3 Months", "6 Months", "12 Months"),
                           paste0('opioid_days_', c("baseline", "3mo", "6mo", "12mo")))

    long <- inner_df %>%
      select(paste0('opioid_days_', c("baseline", "3mo", "6mo", "12mo"))) %>%
      pivot_longer(cols = everything(), names_to = "timepoint", values_to = "days") %>%
      mutate(timepoint = recode(timepoint, !!!recode_map),
             days = as.numeric(days)) %>%
      filter(!is.na(days))

    long %>%
      group_by(timepoint) %>%
      summarise(n = n(), mean_sd = format_mean_sd(days), .groups = 'drop') %>%
      mutate(timepoint = factor(timepoint, c("Baseline", "3 Months", "6 Months", "12 Months"))) %>%
      arrange(timepoint)
  }

  final <- inner_data_extractor(df)

  colnames(final) <- c('', 'n', 'Opioid Days, Mean (SD)')

  index_vec_a <- c("Days of Reported Opioid Use" = nrow(final))

  border_rows <- c(0, cumsum(index_vec_a))

  table_raw <- kable(final, format = "html", align = 'l') %>%
    pack_rows(index = index_vec_a, label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(border_rows, extra_css = "border-bottom: 1px solid;")

  return(table_raw)
}


#' Other drugs review
#'
#' @description
#' Lists every distinct free-text medication entry that is awaiting clinical
#' classification, so that the "Other" entries on the pain medication questions can
#' be reviewed and assigned an opioid status. Rows are drawn from the
#' opioid_days_data long file where other_review_status is "Pending review",
#' de-duplicated case-insensitively, with the number of times each entry was
#' reported and the forms it came from.
#'
#' Deliberately not split by treatment arm. Free-text medication names can identify
#' the assigned treatment, so an arm-level version of this table would unmask.
#'
#' @param analytic opioid_days_data construct
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' other_drugs_review("Replace with Analytic Tibble")
#' }
other_drugs_review <- function(analytic){
  df <- analytic %>%
    select(opioid_days_data) %>%
    separate_rows(opioid_days_data, sep = ";new_row: ") %>%
    separate(opioid_days_data,
             into = c('facilitycode', 'nominal_visit', 'assessment_date', 'days_from_time_zero',
                      'source_form', 'raw_drug_name', 'standardized_drug_name', 'opioid_yn',
                      'raw_frequency_response', 'assigned_days', 'other_drug_text',
                      'other_review_status', 'number_of_opioids_reported_at_visit'),
             sep = '\\|', fill = 'right') %>%
    mutate_all(na_if, 'NA') %>%
    filter(other_review_status == 'Pending review', !is.na(raw_drug_name)) %>%
    mutate(raw_drug_name = str_squish(raw_drug_name))

  review <- df %>%
    group_by(entry = str_to_lower(raw_drug_name)) %>%
    summarise(`Free-text entry` = first(raw_drug_name),
              `Times reported` = n(),
              `Source form(s)` = paste(sort(unique(source_form)), collapse = '; '),
              .groups = 'drop') %>%
    arrange(desc(`Times reported`), `Free-text entry`) %>%
    select(-entry)

  if (nrow(review) == 0) {
    review <- tibble(`Free-text entry` = 'No entries awaiting review',
                     `Times reported` = 0, `Source form(s)` = '-')
  }

  table_raw <- kable(review, format = "html", align = 'l') %>%
    kable_styling("striped", full_width = FALSE, position = 'left')

  return(table_raw)
}


#' Other events review
#'
#' @description
#' Lists every distinct clinical event awaiting category assignment, so that the
#' "Other" complication checklist entries can be reviewed and mapped to an analysis
#' grouping. Rows are drawn from the clinical_events_data long file where
#' complication_category_mapped is "NEEDS REVIEW" or "UNMAPPED", de-duplicated
#' case-insensitively, with the number of times each entry was reported and the
#' forms it came from.
#'
#' UNMAPPED rows are a different problem from NEEDS REVIEW rows: they are labels the
#' category mapping did not recognise at all, rather than free text expected to need
#' classification, so the reason is reported alongside each entry.
#'
#' @param analytic clinical_events_data construct
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' other_events_review("Replace with Analytic Tibble")
#' }
other_events_review <- function(analytic){
  df <- analytic %>%
    select(clinical_events_data) %>%
    separate_rows(clinical_events_data, sep = ";new_row: ") %>%
    separate(clinical_events_data,
             into = c('facilitycode', 'date_of_event', 'days_from_time_zero', 'source_form',
                      'event_type', 'complication_category_redcap', 'complication_category_mapped',
                      'other_text', 'other_review_status'),
             sep = '\\|', fill = 'right') %>%
    mutate_all(na_if, 'NA') %>%
    filter(complication_category_mapped %in% c('NEEDS REVIEW', 'UNMAPPED')) %>%
    # An Other box carries its detail in other_text; an unrecognised label has none,
    # so fall back to the REDCap category so the row is still identifiable.
    mutate(entry_text = str_squish(coalesce(other_text, complication_category_redcap))) %>%
    filter(!is.na(entry_text))

  review <- df %>%
    group_by(entry = str_to_lower(entry_text), Reason = complication_category_mapped) %>%
    summarise(`Free-text entry` = first(entry_text),
              `Times reported` = n(),
              `Source form(s)` = paste(sort(unique(source_form)), collapse = '; '),
              .groups = 'drop') %>%
    arrange(desc(`Times reported`), `Free-text entry`) %>%
    select(`Free-text entry`, `Times reported`, `Source form(s)`, Reason)

  if (nrow(review) == 0) {
    review <- tibble(`Free-text entry` = 'No entries awaiting review',
                     `Times reported` = 0, `Source form(s)` = '-', Reason = '-')
  }

  table_raw <- kable(review, format = "html", align = 'l') %>%
    kable_styling("striped", full_width = FALSE, position = 'left')

  return(table_raw)
}


#' Clinical event categories
#'
#' @description
#' Shared inner worker for the side effect and adverse event tables. Unpacks
#' the clinical_events_data long file, optionally windows it, and returns the number
#' and percentage of participants reporting an event in each analysis category.
#'
#' Counts participants, not events - a participant with three bleeding events
#' contributes once. The denominator is the number of enrolled participants passed
#' in, so percentages are of the arm being summarised rather than of the event count.
#'
#' Categories come from the values present in complication_category_mapped, but the
#' display order is fixed, so a category with no events still produces a 0 (0.0%)
#' row rather than disappearing from the table.
#'
#' @param events_df Unpacked clinical_events_data with study_id, days_from_time_zero
#'   and complication_category_mapped.
#' @param denominator Number of enrolled participants to divide by.
#' @param row_order Character vector of category rows, in display order.
#' @param composites Named list mapping a composite row label to the categories it
#'   sums over, e.g. list(`Major NSAID Related` = c('Renal', 'Gastric')).
#' @param other_label Label for the catch-all row.
#' @param other_categories Categories folded into the catch-all row.
#' @param max_days Optional day cutoff applied to days_from_time_zero.
#' @param none_label Optional label for a row counting participants with no event.
#'
#' @return A data frame of category, n and percentage.
#' @noRd
event_category_counts <- function(events_df, denominator, row_order,
                                        composites = list(), other_label = NULL,
                                        other_categories = character(0),
                                        max_days = NULL, none_label = NULL){
  fmt <- function(n) paste0(n, " (",
                            trimws(format(round(ifelse(denominator > 0, 100 * n / denominator, 0), 1),
                                          nsmall = 1)), "%)")

  windowed <- events_df
  if (!is.null(max_days)) {
    windowed <- windowed %>%
      mutate(days_from_time_zero = suppressWarnings(as.numeric(days_from_time_zero))) %>%
      filter(!is.na(days_from_time_zero), days_from_time_zero <= max_days)
  }

  ids_for <- function(cats) unique(windowed$study_id[windowed$complication_category_mapped %in% cats])

  rows <- lapply(row_order, function(lbl){
    if (!is.null(none_label) && lbl == none_label) {
      n <- max(0, denominator - length(unique(windowed$study_id)))
    } else if (lbl %in% names(composites)) {
      n <- length(ids_for(composites[[lbl]]))
    } else if (!is.null(other_label) && lbl == other_label) {
      n <- length(ids_for(other_categories))
    } else {
      n <- length(ids_for(lbl))
    }
    tibble(Category = lbl, n = n, pct = fmt(n))
  })

  return(bind_rows(rows))
}


#' Reported side effects
#'
#' @description
#' Returns the analgesic side effect burden shell: the number and
#' percentage of enrolled participants reporting each category of side effect through
#' three months. Per SAP Safety Outcomes, burden is summarised as no side effects, a
#' major side effect, and minor side effects, with major defined as renal impairment
#' and/or gastric ulcer.
#'
#' The composition of the major row is set in one place at the top of the function,
#' because whether bleeding belongs in it is still an open question - the SAP says
#' renal and gastric only, while the secondary outcomes table also lists bleeding.
#'
#' Events with no days_from_time_zero cannot be placed in the three month window and
#' are excluded from this table.
#'
#' @param analytic enrolled, clinical_events_data constructs
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' reported_side_effects("Replace with Analytic Tibble")
#' }
reported_side_effects <- function(analytic){
  # SAP Safety Outcomes: major is renal impairment and/or gastric ulcer. Add
  # 'Bleeding' here if the secondary outcomes table reading is adopted instead.
  major_categories <- c('Renal', 'Gastric')
  named_categories <- c('Bleeding', 'Renal', 'Thromboembolic', 'Gastric', 'Allergy', 'Minor Allergy')
  other_categories <- c('Surgical/Wound', 'NEEDS REVIEW', 'UNMAPPED')

  row_order <- c('None', 'Major NSAID Related', named_categories, 'All Others')

  events_df <- unpack_clinical_events(analytic)
  denominator <- analytic %>% filter(enrolled) %>% nrow()

  final <- event_category_counts(
    events_df, denominator, row_order,
    composites = list(`Major NSAID Related` = major_categories),
    other_label = 'All Others', other_categories = other_categories,
    max_days = 90, none_label = 'None') %>%
    select(Category, pct)

  colnames(final) <- c('', 'N (%)')

  table_raw <- kable(final, format = "html", align = 'l') %>%
    pack_rows(index = c("Reported Side Effects, through 3 Months" = nrow(final)),
              label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(c(0, nrow(final)), extra_css = "border-bottom: 1px solid;")

  return(table_raw)
}


#' Adverse events
#'
#' @description
#' Returns the adverse event shell: the number and percentage of
#' enrolled participants with an event in each category, across the whole study
#' period rather than windowed to three months. Deaths are taken from the dead
#' construct rather than from the complication checklists.
#'
#' @param analytic enrolled, dead, clinical_events_data constructs
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' \dontrun{
#' adverse_events("Replace with Analytic Tibble")
#' }
adverse_events <- function(analytic){
  named_categories <- c('Surgical/Wound', 'Thromboembolic', 'Renal', 'Gastric', 'Bleeding')
  other_categories <- c('Allergy', 'Minor Allergy', 'NEEDS REVIEW', 'UNMAPPED')

  row_order <- c(named_categories, 'Other Medical')

  events_df <- unpack_clinical_events(analytic)
  enrolled_df <- analytic %>% filter(enrolled)
  denominator <- nrow(enrolled_df)

  counts <- event_category_counts(
    events_df, denominator, row_order,
    other_label = 'Other Medical', other_categories = other_categories) %>%
    select(Category, pct)

  deaths_n <- sum(enrolled_df$dead %in% TRUE)
  deaths <- tibble(Category = 'Deaths',
                   pct = paste0(deaths_n, " (",
                                trimws(format(round(ifelse(denominator > 0, 100 * deaths_n / denominator, 0), 1),
                                              nsmall = 1)), "%)"))

  final <- bind_rows(deaths, counts)

  colnames(final) <- c('', 'N (%)')

  table_raw <- kable(final, format = "html", align = 'l') %>%
    pack_rows(index = c("Adverse Events, Whole Study" = nrow(final)),
              label_row_css = "text-align:left") %>%
    kable_styling("striped", full_width = FALSE, position = 'left') %>%
    row_spec(c(0, nrow(final)), extra_css = "border-bottom: 1px solid;")

  return(table_raw)
}


#' Unpack clinical events
#'
#' @description
#' Shared inner worker that unpacks the clinical_events_data long file into one row
#' per reported event, keeping study_id so that participants can be counted rather
#' than events.
#'
#' @param analytic A tibble containing study_id and clinical_events_data.
#'
#' @return A data frame of unpacked events.
#' @noRd
unpack_clinical_events <- function(analytic){
  return(analytic %>%
    select(study_id, clinical_events_data) %>%
    separate_rows(clinical_events_data, sep = ";new_row: ") %>%
    separate(clinical_events_data,
             into = c('facilitycode', 'date_of_event', 'days_from_time_zero', 'source_form',
                      'event_type', 'complication_category_redcap', 'complication_category_mapped',
                      'other_text', 'other_review_status'),
             sep = '\\|', fill = 'right') %>%
    mutate_all(na_if, 'NA') %>%
    filter(!is.na(complication_category_mapped)))
}


# ---- Packed-construct unpacking and repeated-measurement preparation ------------------------
#
# Shared helpers beneath the safety, repeated-measurement and patient-reported-outcome
# displays and analyses. They turn the one-row-per-study_id analytic contract into typed long
# tables without touching the participant file, so open and closed outputs agree on
# populations, denominators and definitions.

#' Treat packed literal missing tokens as missing
#' @noRd
packed_na <- function(x) {
  x <- str_trim(as.character(x))
  x[x %in% c("NA", "", "N/A", "NaN")] <- NA_character_
  x
}

#' Numeric conversion that keeps failed conversions visible
#' @noRd
parse_packed_number <- function(x) {
  raw <- packed_na(x)
  num <- suppressWarnings(as.numeric(raw))
  list(value = num, failed = !is.na(raw) & is.na(num))
}

#' Stop with the names of the constructs a function needs but the analytic lacks
#' @noRd
require_constructs <- function(analytic, constructs, what) {
  missing_constructs <- setdiff(constructs, names(analytic))
  if (length(missing_constructs) > 0) {
    stop(paste0(what, " requires constructs not present in the analytic data set: ",
                paste(missing_constructs, collapse = ", ")))
  }
  invisible(TRUE)
}

#' Sorted enrolled study_id values, enforcing the one-row-per-ID contract
#' @noRd
enrolled_study_ids <- function(analytic) {
  require_constructs(analytic, c("study_id", "enrolled"), "population selection")
  ids <- analytic %>% filter(enrolled %in% TRUE) %>% pull(study_id) %>% as.character()
  if (any(duplicated(ids))) {
    stop("analytic input violates the one-row-per-study_id contract: duplicated enrolled IDs ",
         paste(unique(ids[duplicated(ids)]), collapse = ", "))
  }
  sort(ids)
}

#' Unpack a packed long-file construct into typed rows
#' @noRd
unpack_packed_construct <- function(analytic, construct, fields, row_sep, field_sep,
                                    population = c("enrolled", "all")) {
  population <- match.arg(population)
  require_constructs(analytic, c("study_id", construct), paste0("unpacking ", construct))
  df <- analytic
  if (population == "enrolled") {
    require_constructs(analytic, "enrolled", paste0("unpacking ", construct))
    df <- df %>% filter(enrolled %in% TRUE)
  }
  df <- df %>%
    transmute(study_id = as.character(study_id), packed = packed_na(.data[[construct]])) %>%
    filter(!is.na(packed)) %>%
    separate_rows(packed, sep = row_sep) %>%
    mutate(packed = str_trim(packed)) %>%
    filter(!is.na(packed), packed != "")
  if (nrow(df) == 0) {
    out <- tibble(study_id = character())
    for (f in fields) out[[f]] <- character()
    return(out)
  }
  n_fields <- str_count(df$packed, field_sep) + 1
  if (any(n_fields != length(fields))) {
    bad <- df[n_fields != length(fields), ]
    stop(sprintf("%s has %d packed record(s) with %s fields instead of %d (first bad record for study_id %s: '%s')",
                 construct, nrow(bad), paste(unique(n_fields[n_fields != length(fields)]), collapse = "/"), length(fields),
                 bad$study_id[1], substr(bad$packed[1], 1, 80)))
  }
  df %>%
    separate(packed, into = fields, sep = field_sep) %>%
    mutate(across(all_of(fields), packed_na))
}

#' Unpack follow-up status records
#' @noRd
unpack_followup_data <- function(analytic, population = c("enrolled", "all")) {
  population <- match.arg(population)
  unpack_packed_construct(analytic, "followup_data",
                          c("redcap_event_name", "followup_period", "form", "status", "form_dates"),
                          row_sep = ";", field_sep = ",", population = population) %>%
    mutate(form_dates = suppressWarnings(as.Date(form_dates)),
           status_base = str_remove(status, ":.*$"),
           visit_complete = status_base %in% "Complete")
}

#' Complication category mapping for safety summaries
#' @noRd
complication_categories <- function(minor_expected = c("Local injection reactions"),
                                    minor_unexpected = c("Small hemorrhage", "Edema", "Nodules/papules",
                                                         "Irritation", "Dermatitis", "Pruritus", "Cellulitis"),
                                    serious_severities = c("Severe and Undesirable",
                                                           "Life-threatening or disabling", "Fatal"),
                                    related_levels = c("Definitely related", "Probably related",
                                                       "Possibly related"),
                                    labels = c(any = "Any complication",
                                               minor_expected = "Minor expected complication",
                                               minor_unexpected = "Minor unexpected complication",
                                               serious = "Serious complication (severe, life-threatening or fatal)",
                                               other = "Other complication",
                                               sae_any = "Any SAE (SAE form, all relatedness)",
                                               sae_related = "SAE related or possibly related to treatment")) {
  list(minor_expected = minor_expected, minor_unexpected = minor_unexpected,
       serious_severities = serious_severities, related_levels = related_levels, labels = labels)
}

#' Assign each complication record to a category
#' @noRd
assign_complication_category <- function(complication, severity, categories) {
  comp <- tolower(str_trim(as.character(complication)))
  sev <- tolower(str_trim(as.character(severity)))
  starts_with_any <- function(x, prefixes) {
    if (length(prefixes) == 0) return(rep(FALSE, length(x)))
    Reduce(`|`, lapply(tolower(prefixes), function(p) startsWith(x, p)))
  }
  case_when(
    sev %in% tolower(categories$serious_severities) ~ "serious",
    starts_with_any(comp, categories$minor_expected) ~ "minor_expected",
    starts_with_any(comp, categories$minor_unexpected) ~ "minor_unexpected",
    TRUE ~ "other"
  )
}

#' Unpack complication records with safety categories
#' @noRd
unpack_complication_data <- function(analytic, categories = complication_categories(),
                                     population = c("enrolled", "all")) {
  population <- match.arg(population)
  unpack_packed_construct(analytic, "complication_data",
                          c("redcap_event_name", "form_name", "event_type", "complication", "notes",
                            "diagnosis_date", "relatedness", "severity", "treatment", "other_info"),
                          row_sep = ";new_row: ", field_sep = "\\|", population = population) %>%
    filter(!is.na(complication)) %>%
    mutate(diagnosis_date = suppressWarnings(as.Date(diagnosis_date)),
           category = assign_complication_category(complication, severity, categories),
           serious = tolower(severity) %in% tolower(categories$serious_severities),
           related = tolower(relatedness) %in% tolower(categories$related_levels))
}

#' Unpack serious adverse event records
#' @noRd
unpack_sae_data <- function(analytic, population = c("enrolled", "all")) {
  population <- match.arg(population)
  unpack_packed_construct(analytic, "sae_data",
                          c("facilitycode", "treatment_arm_placeholder", "sae_treatment_received",
                            "consent_date", "sae_dt_event", "age", "sae_related",
                            "sae_relatedness_treatment", "sae_outcome", "sae_describe"),
                          row_sep = ";new_row: ", field_sep = "\\|", population = population) %>%
    select(-treatment_arm_placeholder) %>%
    mutate(sae_dt_event = suppressWarnings(as.Date(sae_dt_event)))
}

#' Unpack not-expected and not-completed administrative records
#' @noRd
unpack_not_expected_data <- function(analytic, population = c("enrolled", "all")) {
  population <- match.arg(population)
  unpack_packed_construct(analytic, "not_expected_data",
                          c("facilitycode", "treatment_arm_placeholder", "consent_date",
                            "not_expected_date", "age", "not_expected_reason"),
                          row_sep = ";new_row: ", field_sep = "\\|", population = population) %>%
    select(-treatment_arm_placeholder) %>%
    mutate(not_expected_date = suppressWarnings(as.Date(not_expected_date)))
}

#' @rdname unpack_not_expected_data
#' @noRd
unpack_not_completed_data <- function(analytic, population = c("enrolled", "all")) {
  population <- match.arg(population)
  unpack_packed_construct(analytic, "not_completed_data",
                          c("facilitycode", "treatment_arm_placeholder", "consent_date",
                            "not_completed_date", "age", "not_completed_reason"),
                          row_sep = ";new_row: ", field_sep = "\\|", population = population) %>%
    select(-treatment_arm_placeholder) %>%
    mutate(not_completed_date = suppressWarnings(as.Date(not_completed_date)))
}

#' Unpack repeated location measurements
#' @noRd
unpack_measurement_readings <- function(analytic, readings_constructs = "durometer_readings_set_1",
                                        fields = c("set", "event", "position", "injection", "reading"),
                                        value_field = "reading", row_sep = ";", field_sep = ",",
                                        population = c("enrolled", "all")) {
  population <- match.arg(population)
  if (!all(c("set", "event", "position") %in% fields)) {
    stop("fields must include set, event and position")
  }
  if (!value_field %in% fields) stop("value_field must be one of the packed fields")
  require_constructs(analytic, readings_constructs, "unpack_measurement_readings")
  out <- bind_rows(lapply(readings_constructs, function(construct) {
    unpack_packed_construct(analytic, construct, fields, row_sep = row_sep, field_sep = field_sep,
                            population = population) %>%
      mutate(source_construct = construct)
  }))
  parsed <- parse_packed_number(out[[value_field]])
  out %>%
    mutate(value_raw = .data[[value_field]], value = parsed$value, parse_failed = parsed$failed) %>%
    select(study_id, set, event, position, everything())
}

#' Location-level visit means of repeated readings
#' @noRd
measurement_visit_means <- function(long, min_valid = 1) {
  long %>%
    group_by(study_id, set, event, position) %>%
    summarise(n_total = n(),
              n_valid = sum(!is.na(value)),
              n_failed = sum(parse_failed),
              mean = if (sum(!is.na(value)) > 0) mean(value, na.rm = TRUE) else NA_real_,
              .groups = "drop") %>%
    mutate(available = n_valid >= min_valid & !is.na(mean),
           mean = ifelse(available, mean, NA_real_))
}

#' Paired change in location means between two visits
#' @noRd
measurement_visit_change <- function(visit_means, baseline_event = "injection_1",
                                     followup_event = "3_month") {
  base <- visit_means %>%
    filter(event == baseline_event) %>%
    select(study_id, set, position, baseline = mean, n_valid_baseline = n_valid)
  fu <- visit_means %>%
    filter(event == followup_event) %>%
    select(study_id, set, position, followup = mean, n_valid_followup = n_valid)
  full_join(base, fu, by = c("study_id", "set", "position")) %>%
    mutate(n_valid_baseline = replace_na(n_valid_baseline, 0L),
           n_valid_followup = replace_na(n_valid_followup, 0L),
           paired = !is.na(baseline) & !is.na(followup),
           change = ifelse(paired, followup - baseline, NA_real_)) %>%
    arrange(study_id, set, position)
}

#' Filter unpacked readings by packed field values
#' @noRd
keep_measurement_rows <- function(long, keep) {
  if (is.null(keep)) return(long)
  for (field in names(keep)) {
    if (!field %in% names(long)) stop("keep names a field not present in the readings: ", field)
    long <- long %>% filter(.data[[field]] %in% keep[[field]])
  }
  long
}

#' Default measurement endpoints for the amputation skin studies
#' @noRd
default_measurement_endpoints <- function() {
  list(
    durometer = list(label = "Skin firmness (durometer)",
                     readings_constructs = c("durometer_readings_set_1", "durometer_readings_set_2"),
                     fields = c("set", "event", "position", "injection", "reading"),
                     value_field = "reading", unit = "DU"),
    oct = list(label = "Skin thickness (OCT width)",
               readings_constructs = c("oct_readings_set_1", "oct_readings_set_2"),
               fields = c("set", "event", "position", "orientation", "area", "length", "width"),
               value_field = "width", unit = "OCT width units (unconfirmed)")
  )
}

#' Default patient-reported score families
#' @noRd
default_score_families <- function(extra = NULL) {
  fam <- function(prefix) c(baseline = paste0(prefix, "_baseline"), `1mo` = paste0(prefix, "_1mo"),
                            `2mo` = paste0(prefix, "_2mo"), `3mo` = paste0(prefix, "_3mo"))
  c(list("DLQI" = fam("dlqi"), "PEQ (Residual Limb Health)" = fam("peq_rl"), "PLUS-M" = fam("plus_m")),
    extra)
}

promis_domain_labels <- c(physical_function = "PROMIS-29 Physical Function (T-score)",
                          anxiety = "PROMIS-29 Anxiety (T-score)",
                          depression = "PROMIS-29 Depression (T-score)",
                          fatigue = "PROMIS-29 Fatigue (T-score)",
                          sleep_disturbance = "PROMIS-29 Sleep Disturbance (T-score)",
                          social_roles = "PROMIS-29 Social Roles (T-score)",
                          pain_interference = "PROMIS-29 Pain Interference (T-score)",
                          pain_intensity = "PROMIS-29 Pain Intensity (0-10)")

promis_data_example_type <- "(';', ',')NamedCategory['Baseline' '1 Month' '2 Month' '3 Month']|NamedCategory['physical_function' 'anxiety' 'depression' 'fatigue' 'sleep_disturbance' 'social_roles' 'pain_interference' 'pain_intensity']|Number-U4|Number|Number"

#' Unpack the packed PROMIS-29 scores (promis_data) to the score-family long shape
#'
#' Fields visit, domain, items_answered, raw_score, t_score; record separator ";", field
#' separator ",". The score is the T-score for the seven scored domains and the 0-10 rating
#' for pain intensity. A domain with fewer than four items answered has no T-score in the
#' export (PROMIS rule) and so counts as missing here; items_answered is kept.
#' @noRd
unpack_promis_data <- function(analytic, construct = "promis_data",
                               visit_labels = c("Baseline", "1 Month", "2 Month", "3 Month")) {
  long <- unpack_packed_construct(analytic, construct, c("visit", "domain", "items_answered", "raw_score", "t_score"),
                                  row_sep = ";", field_sep = ",")
  unknown_domain <- setdiff(unique(long$domain), names(promis_domain_labels))
  if (length(unknown_domain) > 0) stop(construct, " has unknown PROMIS domain(s): ", paste(unknown_domain, collapse = ", "))
  unknown_visit <- setdiff(unique(long$visit), visit_labels)
  if (length(unknown_visit) > 0) stop(construct, " has visit label(s) outside the score-family visits: ", paste(unknown_visit, collapse = ", "))
  score_field <- ifelse(long$domain %in% "pain_intensity", long$raw_score, long$t_score)
  parsed <- parse_packed_number(score_field)
  long %>%
    mutate(instrument = unname(promis_domain_labels[domain]),
           score_raw = score_field, score = parsed$value, parse_failed = parsed$failed,
           items_answered = parse_packed_number(items_answered)$value) %>%
    select(study_id, instrument, visit, score_raw, score, parse_failed, items_answered)
}

#' Reshape score families to participant/visit rows
#'
#' Wide score families (default_score_families) and, when promis_construct names a packed
#' PROMIS-29 construct present in the export, its domains as additional instruments.
#' @noRd
unpack_score_families <- function(analytic, score_families = default_score_families(),
                                  visit_labels = c(baseline = "Baseline", `1mo` = "1 Month",
                                                   `2mo` = "2 Month", `3mo` = "3 Month"),
                                  promis_construct = "promis_data") {
  require_constructs(analytic, c("study_id", "enrolled"), "unpack_score_families")
  df <- analytic %>% filter(enrolled %in% TRUE) %>% mutate(study_id = as.character(study_id))
  require_constructs(analytic, unlist(score_families, use.names = FALSE), "unpack_score_families")
  if (!is.null(promis_construct)) require_constructs(analytic, promis_construct, "unpack_score_families")
  rows <- list()
  for (inst in names(score_families)) {
    cols <- score_families[[inst]]
    for (v in names(cols)) {
      parsed <- parse_packed_number(df[[cols[[v]]]])
      rows[[length(rows) + 1]] <- tibble(study_id = df$study_id, instrument = inst,
                                         visit = visit_labels[[v]],
                                         score_raw = packed_na(df[[cols[[v]]]]),
                                         score = parsed$value, parse_failed = parsed$failed)
    }
  }
  out <- if (length(rows) == 0) {
    tibble(study_id = character(), instrument = character(), visit = character(),
           score_raw = character(), score = numeric(), parse_failed = logical())
  } else {
    bind_rows(rows)
  }
  out$items_answered <- NA_real_
  instrument_levels <- names(score_families)
  if (!is.null(promis_construct)) {
    promis <- unpack_promis_data(analytic, promis_construct, unname(visit_labels))
    out <- bind_rows(out, promis)
    instrument_levels <- c(instrument_levels, unname(promis_domain_labels)[unname(promis_domain_labels) %in% promis$instrument])
  }
  out %>%
    mutate(instrument = factor(instrument, levels = instrument_levels),
           visit = factor(visit, levels = unname(visit_labels)))
}

#' Participant-level safety inputs
#' @noRd
participant_event_summary <- function(analytic, categories = complication_categories(),
                                      count_construct = "complication_count",
                                      exposure_construct = "last_followup_days") {
  require_constructs(analytic, c("study_id", "enrolled", "complication_data", "sae_data", "followup_data",
                                 count_construct, exposure_construct), "participant_event_summary")
  ids <- enrolled_study_ids(analytic)
  base <- analytic %>%
    filter(enrolled %in% TRUE) %>%
    transmute(study_id = as.character(study_id),
              event_count = parse_packed_number(.data[[count_construct]])$value,
              exposure_days = parse_packed_number(.data[[exposure_construct]])$value,
              has_complication_record = !is.na(packed_na(complication_data)))

  events <- unpack_complication_data(analytic, categories)
  event_counts <- events %>%
    group_by(study_id) %>%
    summarise(n_records = n(),
              n_minor_expected = sum(category == "minor_expected"),
              n_minor_unexpected = sum(category == "minor_unexpected"),
              n_serious = sum(category == "serious"),
              n_other = sum(category == "other"), .groups = "drop")

  saes <- unpack_sae_data(analytic)
  sae_counts <- saes %>%
    group_by(study_id) %>%
    summarise(n_sae = n(),
              n_sae_related = sum(tolower(sae_related) %in% tolower(categories$related_levels) |
                                    tolower(sae_relatedness_treatment) %in% tolower(categories$related_levels)),
              .groups = "drop")

  fu_any <- unpack_followup_data(analytic) %>%
    group_by(study_id) %>%
    summarise(any_completed_form = any(visit_complete), .groups = "drop")

  out <- tibble(study_id = ids) %>%
    left_join(base, by = "study_id") %>%
    left_join(event_counts, by = "study_id") %>%
    left_join(sae_counts, by = "study_id") %>%
    left_join(fu_any, by = "study_id") %>%
    mutate(across(c(n_records, n_minor_expected, n_minor_unexpected, n_serious, n_other, n_sae, n_sae_related),
                  ~ replace_na(.x, 0L)),
           any_completed_form = replace_na(any_completed_form, FALSE),
           ascertained = has_complication_record | any_completed_form,
           ascertainment_unknown = !ascertained,
           event_count_source = case_when(!is.na(event_count) ~ "verified",
                                          ascertained ~ "no records (verified zero)",
                                          TRUE ~ "unknown"),
           event_count = ifelse(is.na(event_count) & ascertained, 0, event_count),
           any_complication = ifelse(ascertained, n_records > 0 | event_count > 0, NA),
           any_minor_expected = ifelse(ascertained, n_minor_expected > 0, NA),
           any_minor_unexpected = ifelse(ascertained, n_minor_unexpected > 0, NA),
           any_serious = ifelse(ascertained, n_serious > 0 | n_sae > 0, NA),
           any_other = ifelse(ascertained, n_other > 0, NA),
           any_sae = ifelse(ascertained, n_sae > 0, NA),
           any_sae_related = ifelse(ascertained, n_sae_related > 0, NA),
           exposure_valid = !is.na(exposure_days) & exposure_days > 0)

  inconsistent <- out %>% filter(is.na(event_count) & n_records > 0 | (!is.na(event_count) & event_count < n_records))
  if (nrow(inconsistent) > 0) {
    stop(count_construct, " is missing or below the number of packed complication records for study_id ",
         paste(inconsistent$study_id, collapse = ", "), "; reconcile the verified count with complication_data")
  }
  attr(out, "reconciliation") <- out %>%
    filter(event_count_source == "verified") %>%
    summarise(participants = n(), verified_total = sum(event_count, na.rm = TRUE),
              long_record_total = sum(n_records), participants_differing = sum(event_count != n_records))
  attr(out, "events") <- events
  attr(out, "saes") <- saes
  out
}

#' Category rows shared by the safety displays and analyses
#' @noRd
event_category_rows <- function(categories) {
  labels <- categories$labels
  keys <- c("any", "minor_expected", "minor_unexpected", "serious", "other", "sae_any", "sae_related")
  tibble(key = keys, category = unname(labels[keys]),
         flag = c("any_complication", "any_minor_expected", "any_minor_unexpected", "any_serious",
                  "any_other", "any_sae", "any_sae_related"),
         count = c("event_count", "n_minor_expected", "n_minor_unexpected", "n_serious", "n_other",
                   "n_sae", "n_sae_related"),
         count_source = c("verified count construct", "long complication records", "long complication records",
                          "long complication records", "long complication records", "SAE form records",
                          "SAE form records"))
}

#' Pooled participants-with-event counts by category
#' @noRd
pooled_event_risks <- function(participants, categories) {
  rows <- event_category_rows(categories)
  bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    flag <- participants[[rows$flag[i]]]
    tibble(key = rows$key[i], category = rows$category[i],
           participants_with_event = sum(flag %in% TRUE), denominator = sum(!is.na(flag)),
           unknown_ascertainment = sum(is.na(flag)), enrolled = nrow(participants))
  }))
}

# ---- Interval methods ------------------------------------------------------------------------

#' Risk difference with a small-sample confidence interval
#' @noRd
risk_difference_interval <- function(x1, n1, x2, n2, method = c("newcombe", "wald"), conf_level = 0.95) {
  method <- match.arg(method)
  if (any(c(n1, n2) == 0) || any(is.na(c(x1, n1, x2, n2)))) {
    return(tibble(p1 = NA_real_, p2 = NA_real_, estimate = NA_real_, lower = NA_real_, upper = NA_real_,
                  method = method, status = "not estimable: empty denominator"))
  }
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p1 <- x1 / n1
  p2 <- x2 / n2
  d <- p1 - p2
  if (method == "wald") {
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    lo <- d - z * se
    hi <- d + z * se
  } else {
    wilson <- function(x, n) {
      p <- x / n
      centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
      half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
      c(centre - half, centre + half)
    }
    w1 <- wilson(x1, n1)
    w2 <- wilson(x2, n2)
    lo <- d - sqrt((p1 - w1[1])^2 + (w2[2] - p2)^2)
    hi <- d + sqrt((w1[2] - p1)^2 + (p2 - w2[1])^2)
  }
  tibble(p1 = p1, p2 = p2, estimate = 100 * d, lower = 100 * max(-1, lo), upper = 100 * min(1, hi),
         method = method, status = "ok")
}

#' Exact incidence rate ratio for two Poisson counts with exposure
#' @noRd
exact_rate_ratio <- function(x1, t1, x2, t2, conf_level = 0.95) {
  method <- "exact conditional Poisson (binomial conditioning on total events; stats::poisson.test)"
  if (any(is.na(c(x1, t1, x2, t2))) || t1 <= 0 || t2 <= 0) {
    return(tibble(rate1 = NA_real_, rate2 = NA_real_, estimate = NA_real_, lower = NA_real_, upper = NA_real_,
                  p_value = NA_real_, method = method, status = "not estimable: missing or nonpositive exposure"))
  }
  if (x1 + x2 == 0) {
    return(tibble(rate1 = 0, rate2 = 0, estimate = NA_real_, lower = NA_real_, upper = NA_real_,
                  p_value = NA_real_, method = method, status = "not estimable: no events in either arm"))
  }
  fit <- tryCatch(stats::poisson.test(c(x1, x2), c(t1, t2), conf.level = conf_level), error = function(e) e)
  if (inherits(fit, "error")) {
    return(tibble(rate1 = x1 / t1, rate2 = x2 / t2, estimate = NA_real_, lower = NA_real_, upper = NA_real_,
                  p_value = NA_real_, method = method, status = paste("poisson.test failed:", conditionMessage(fit))))
  }
  tibble(rate1 = x1 / t1, rate2 = x2 / t2, estimate = unname(fit$estimate), lower = fit$conf.int[1],
         upper = fit$conf.int[2], p_value = fit$p.value, method = method,
         status = if (x1 == 0 || x2 == 0) "boundary estimate: zero events in one arm" else "ok")
}

# ---- Formatting helpers shared by the open and closed displays -------------------------------

#' Example type string for a packed construct with the given separators
#' @noRd
packed_example_type <- function(fields, row_sep, field_sep) {
  paste0("('", gsub("\\\\", "", row_sep), "', '", gsub("\\\\", "", field_sep), "')",
         paste(rep("Character", length(fields)), collapse = "|"))
}

#' Example constructs and types for the endpoint availability tables
#' @noRd
availability_example_constructs <- function(measurements, score_families, promis_construct = NULL) {
  c("enrolled", "followup_data", vapply(measurements, function(ep) ep$readings_constructs[1], character(1)),
    unlist(score_families, use.names = FALSE), promis_construct)
}

#' @noRd
availability_example_types <- function(measurements, score_families, promis_construct = NULL) {
  c("Boolean", followup_data_example_type,
    vapply(measurements, function(ep) measurement_example_type(ep$fields, ep$value_field), character(1)),
    rep("Number", length(unlist(score_families))), if (!is.null(promis_construct)) promis_data_example_type)
}

#' Example type string for a packed review index
#' @noRd
review_example_type <- function(fields, features, quality_field, visit_field) {
  types <- vapply(fields, function(f) {
    if (f == visit_field) "NamedCategory['1 Month' '2 Month' '3 Month']"
    else if (f == quality_field) "NamedCategory['Adequate' 'Poor']"
    else if (f %in% features) "NamedCategory['None' 'Mild' 'Moderate' 'Present']"
    else "Character"
  }, character(1))
  paste0("(';', ',')", paste(types, collapse = "|"))
}

#' Format an estimate with its interval
#' @noRd
fmt_estimate <- function(est, lower, upper, digits = 2) {
  f <- function(v) ifelse(is.na(v), "NA", ifelse(is.infinite(v), ifelse(v > 0, "Inf", "-Inf"),
                                                  trimws(format(round(v, digits), nsmall = digits))))
  ifelse(is.na(est), "Not estimable", paste0(f(est), " (", f(lower), " to ", f(upper), ")"))
}

#' Format a number or an em dash when missing
#' @noRd
fmt_number <- function(x, digits = 2) {
  ifelse(is.na(x), "-", trimws(format(round(x, digits), nsmall = digits)))
}

#' Mean (SD) that tolerates zero or one observation
#' @noRd
mean_sd_or_dash <- function(x, digits = 2) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("-")
  if (length(x) == 1) return(paste0(trimws(format(round(x, digits), nsmall = digits)), " (-)"))
  format_mean_sd(x, digits)
}

#' Format n; mean (SD) for a descriptive cell
#' @noRd
fmt_n_mean_sd <- function(n, m, s) {
  ifelse(n > 0, paste0(n, "; ", fmt_number(m), " (", fmt_number(s), ")"), "-")
}

#' Kable with bold header rows and indented detail rows
#' @noRd
kable_indented_rows <- function(table_raw, col_names, indent_flag = NULL, header_above = NULL) {
  is_header <- table_raw$Is_Header
  indent_rows <- if (is.null(indent_flag)) which(!is_header) else which(table_raw[[indent_flag]])
  table_print <- table_raw %>% select(-any_of(c("Is_Header", "Is_Subheader")))
  out <- kable(table_print, format = "html", col.names = col_names, align = "l", escape = FALSE) %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    column_spec(1, bold = is_header)
  if (length(indent_rows) > 0) out <- out %>% add_indent(indent_rows)
  if (!is.null(header_above)) out <- out %>% add_header_above(header_above)
  out
}

bottom_category_levels <- c("Refused", "Don't know", "Don't Know", "Unknown", "Other", "None", "Missing")

#' Category levels in display order, with unknown/refused/missing last
#' @noRd
category_levels <- function(x, sep = NULL, order = NULL) {
  x <- packed_na(x)
  if (!is.null(sep)) x <- unlist(str_split(x[!is.na(x)], sep))
  x <- str_trim(x)
  x[is.na(x)] <- "Missing"
  lev <- unique(x)
  if (!is.null(order)) {
    lev <- c(intersect(order, lev), sort(setdiff(lev, c(order, bottom_category_levels))),
             intersect(bottom_category_levels, setdiff(lev, order)))
  } else {
    lev <- c(sort(setdiff(lev, bottom_category_levels)), intersect(bottom_category_levels, lev))
  }
  unique(lev)
}

#' Header plus n (%) rows for one categorical construct
#' @noRd
category_count_rows <- function(df, construct, header, levels, sep = NULL, denominator = nrow(df)) {
  x <- packed_na(df[[construct]])
  if (!is.null(sep)) {
    x <- unlist(lapply(x, function(v) if (is.na(v)) "Missing" else str_trim(unlist(str_split(v, sep)))))
  } else {
    x[is.na(x)] <- "Missing"
  }
  counts <- base::table(factor(x, levels = levels))
  bind_rows(tibble(Construct = header, Value = "n (%)", Is_Header = TRUE),
            tibble(Construct = levels, Value = format_count_percent(as.integer(counts), denominator),
                   Is_Header = FALSE))
}

#' Mean (SD) row with observed / missing n for one numeric construct
#' @noRd
numeric_summary_rows <- function(df, construct, header, digits = 2, transform = identity) {
  vals <- if (!is.null(construct) && construct %in% names(df)) {
    transform(parse_packed_number(df[[construct]])$value)
  } else {
    rep(NA_real_, nrow(df))
  }
  bind_rows(tibble(Construct = header, Value = mean_sd_or_dash(vals, digits), Is_Header = TRUE),
            tibble(Construct = "Observed / missing, n", Value = paste0(sum(!is.na(vals)), " / ", sum(is.na(vals))),
                   Is_Header = FALSE))
}

# ---- Participant characteristics (open) ------------------------------------------------------

#' Category levels shared by the open and closed characteristics tables
#' @noRd
characteristics_levels <- function(df, constructs) {
  opt <- function(name) if (!is.null(constructs[[name]])) df[[constructs[[name]]]] else NULL
  list(
    sex = category_levels(df[[constructs$sex]], order = c("Male", "Female")),
    race = category_levels(df[[constructs$race]], order = c("Non-Hispanic White", "Non-Hispanic Black", "Hispanic")),
    education = category_levels(df[[constructs$education]],
                                order = c("8th grade or less", "9th to 12th grade, no diploma", "GED or high school graduate",
                                          "Some college, no degree", "Associates degree (2 year degree)",
                                          "Bachelors/college degree", "Some graduate work, no degree", "Graduate degree")),
    insurance = if (!is.null(opt("insurance"))) category_levels(opt("insurance")) else NULL,
    health = category_levels(df[[constructs$health]], order = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    tobacco = category_levels(df[[constructs$tobacco]], order = c("Never", "Former", "Current")),
    comorbidity = category_levels(df[[constructs$comorbidities]], sep = "; ")
  )
}

#' Rows of the characteristics table for one group
#' @noRd
characteristics_rows <- function(df, levels, constructs, health_label) {
  rows <- list(
    numeric_summary_rows(df, constructs$age, "Age, mean years (SD)", 1),
    category_count_rows(df, constructs$sex, "Sex", levels$sex),
    category_count_rows(df, constructs$race, "Race/Ethnicity", levels$race),
    category_count_rows(df, constructs$education, "Education", levels$education)
  )
  if (!is.null(constructs$insurance)) {
    rows[[length(rows) + 1]] <- category_count_rows(df, constructs$insurance, "Insurance", levels$insurance)
  }
  if (!is.null(constructs$bmi)) {
    rows[[length(rows) + 1]] <- numeric_summary_rows(df, constructs$bmi, "Body mass index, mean kg/m² (SD)", 1)
  }
  rows[[length(rows) + 1]] <- category_count_rows(df, constructs$comorbidities,
                                                  "Comorbidity (multiple selections possible)", levels$comorbidity, sep = "; ")
  rows[[length(rows) + 1]] <- category_count_rows(df, constructs$tobacco, "Tobacco use", levels$tobacco)
  rows[[length(rows) + 1]] <- category_count_rows(df, constructs$health, health_label, levels$health)
  bind_rows(rows)
}

#' Patient Characteristics Summary Table
#'
#' @description
#' Pooled participant characteristics for enrolled participants: age, sex, race/ethnicity,
#' education, insurance and body mass index, comorbidities (split on semicolon, so percentages may exceed 100%), tobacco use and a
#' single-item self-reported health measure. Numeric rows carry observed and missing n.
#' Unknown, refused and missing responses are separate categories and no baseline
#' significance tests are shown. The output does not depend on treatment assignment; see
#' closed_patient_characteristics_table for the by-arm version.
#'
#' @param analytic analytic data set that must include enrolled and the constructs named by
#' the other arguments
#' @param age,sex,race,education,comorbidities,tobacco,health construct names
#' @param insurance,bmi construct names for the insurance and body mass index rows; NULL omits a row
#' @param health_label row label for the self-reported health item
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' patient_characteristics_table("Replace with Analytic Tibble")
patient_characteristics_table <- function(analytic, age = "age", sex = "sex", race = "ethnicity_race",
                                          education = "education", insurance = "insurance", bmi = "bmi",
                                          comorbidities = "comorbidities_list", tobacco = "tobacco_use",
                                          health = "preinjury_health",
                                          health_label = "Self-reported general health (single item)") {
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", age, sex, race, education, insurance, bmi, comorbidities, tobacco, health),
    example_types = c("Boolean", "Number", "NamedCategory['Male' 'Female']", "Category", "Category", "Category",
                      "Number", "Category-NS", "NamedCategory['Never' 'Former' 'Current']",
                      "NamedCategory['Excellent' 'Very Good' 'Good' 'Fair' 'Poor']"))
  constructs <- list(age = age, sex = sex, race = race, education = education, insurance = insurance, bmi = bmi,
                     comorbidities = comorbidities, tobacco = tobacco, health = health)
  require_constructs(analytic, c("enrolled", unlist(constructs)), "patient_characteristics_table")
  df <- analytic %>% filter(enrolled %in% TRUE)
  rows <- characteristics_rows(df, characteristics_levels(df, constructs), constructs, health_label)
  kable_indented_rows(rows, c("Characteristic", paste0("Enrolled (n = ", nrow(df), ")")))
}

# ---- Amputation and prosthesis characteristics (open) ---------------------------------------

regular_use_levels <- c("1 per week", "Several times per week", "Daily", "Multiple times per day")

#' Count participants with regular use in a packed frequency list
#' @noRd
regular_use_count <- function(x, n_slots, regular_levels, construct = "frequency list") {
  x <- packed_na(x)
  n_parts <- ifelse(is.na(x), n_slots, str_count(x, ";") + 1)
  if (any(n_parts != n_slots)) {
    stop(construct, " has ", sum(n_parts != n_slots), " value(s) with ", paste(unique(n_parts[n_parts != n_slots]), collapse = "/"),
         " slots instead of ", n_slots, "; the exporter must preserve every slot position")
  }
  parsed <- lapply(x, function(v) {
    if (is.na(v)) return(rep(NA_character_, n_slots))
    str_trim(unlist(str_split(v, ";")))
  })
  mat <- do.call(rbind, parsed)
  any_regular <- apply(mat, 1, function(r) any(r %in% regular_levels))
  all_known <- apply(mat, 1, function(r) all(!is.na(packed_na(r))))
  list(regular = sum(any_regular), unknown = sum(!all_known & !any_regular))
}

#' Category levels shared by the open and closed amputation tables
#' @noRd
amputation_levels <- function(df, constructs) {
  opt <- function(name) if (!is.null(constructs[[name]])) df[[constructs[[name]]]] else NULL
  list(cause = category_levels(df[[constructs$cause]]),
       side = if (!is.null(opt("side"))) category_levels(opt("side")) else NULL,
       device = category_levels(df[[constructs$devices]], sep = "; ", order = c("Cane", "Crutches", "Walker", "Wheelchair")),
       ulcer = if (!is.null(opt("ulcer_stage"))) category_levels(opt("ulcer_stage")) else NULL,
       prosthesis_type = if (!is.null(opt("prosthesis_type"))) category_levels(opt("prosthesis_type")) else NULL)
}

#' Rows of the amputation table for one group
#' @noRd
amputation_rows <- function(df, levels, constructs, anchor, display_years, regular_levels) {
  n <- nrow(df)
  anchor_construct <- if (anchor == "first_injection") constructs$days_since_first_injection else constructs$days_since_consent
  if (is.null(anchor_construct) || !anchor_construct %in% names(df)) stop("the time-since-amputation construct for anchor '", anchor, "' is not in the export")
  anchor_label <- if (anchor == "first_injection") "first injection" else "consent"
  rows <- list(
    if (display_years) {
      numeric_summary_rows(df, anchor_construct, paste0("Years since amputation at ", anchor_label, ", mean (SD)"), 1,
                           transform = function(v) v / 365.25)
    } else {
      numeric_summary_rows(df, anchor_construct, paste0("Days since amputation at ", anchor_label, ", mean (SD)"), 0)
    },
    category_count_rows(df, constructs$cause, "Cause of amputation", levels$cause))
  if (!is.null(levels$side)) rows[[length(rows) + 1]] <- category_count_rows(df, constructs$side, "Side of amputation", levels$side)
  rows[[length(rows) + 1]] <- bind_rows(
    tibble(Construct = "Prosthesis use", Value = "", Is_Header = TRUE),
    numeric_summary_rows(df, constructs$days_per_week, "Days per week, mean (SD)", 1) %>% mutate(Is_Header = FALSE),
    numeric_summary_rows(df, constructs$hours_per_day, "Hours per day, mean (SD)", 1) %>% mutate(Is_Header = FALSE))
  rows[[length(rows) + 1]] <- category_count_rows(df, constructs$devices, "Ambulatory device use (multiple selections possible)",
                                                  levels$device, sep = "; ")
  if (!is.null(levels$ulcer)) rows[[length(rows) + 1]] <- category_count_rows(df, constructs$ulcer_stage, "Ulcer stage", levels$ulcer)
  if (!is.null(levels$prosthesis_type)) rows[[length(rows) + 1]] <- category_count_rows(df, constructs$prosthesis_type, "Prosthesis type", levels$prosthesis_type)
  rows[[length(rows) + 1]] <- bind_rows(
    tibble(Construct = "Socket comfort score", Value = "", Is_Header = TRUE),
    numeric_summary_rows(df, constructs$comfort_sit, "Sitting, mean (SD)", 1) %>% mutate(Is_Header = FALSE),
    numeric_summary_rows(df, constructs$comfort_stand, "Standing, mean (SD)", 1) %>% mutate(Is_Header = FALSE),
    numeric_summary_rows(df, constructs$comfort_walk, "Walking, mean (SD)", 1) %>% mutate(Is_Header = FALSE))
  meds <- regular_use_count(df[[constructs$medications]], constructs$medication_slots, regular_levels, constructs$medications)
  skin <- regular_use_count(df[[constructs$skin_treatments]], constructs$skin_treatment_slots, regular_levels, constructs$skin_treatments)
  rows[[length(rows) + 1]] <- bind_rows(
    tibble(Construct = "Regular use of pain medication for residual limb pain (daily or weekly)",
           Value = format_count_percent(meds$regular, n), Is_Header = TRUE),
    tibble(Construct = "Frequency unknown or missing", Value = as.character(meds$unknown), Is_Header = FALSE),
    tibble(Construct = "Regular use of skin treatments for residual limb problems (daily or weekly)",
           Value = format_count_percent(skin$regular, n), Is_Header = TRUE),
    tibble(Construct = "Frequency unknown or missing", Value = as.character(skin$unknown), Is_Header = FALSE))
  bind_rows(rows)
}

#' Amputation, Residual Limb and Prostheses Characteristics Summary Table
#'
#' @description
#' Pooled amputation and prosthesis characteristics for enrolled participants: time since
#' amputation at one labelled anchor (first injection or consent), cause, side, prosthesis
#' use, ambulatory devices split into individual selections, ulcer stage and prosthesis type,
#' socket comfort, and regular pain-medication and skin-treatment use. Regular
#' use applies the daily-or-weekly threshold in regular_levels rather than any answer other
#' than "Did not use", and parses the packed frequency lists by their slot count (six for
#' medications including cannabinoids, five for skin treatments).
#'
#' @param analytic analytic data set that must include enrolled and the constructs named by
#' the other arguments
#' @param anchor "first_injection" (uses days_since_first_injection) or "consent" (uses
#' days_since_consent)
#' @param display_years show years (days / 365.25) rather than days
#' @param regular_levels frequency labels counted as regular use
#' @param days_since_first_injection,days_since_consent,cause,side,days_per_week,hours_per_day,devices,ulcer_stage,prosthesis_type,comfort_sit,comfort_stand,comfort_walk,medications,skin_treatments construct names; NULL omits the side, ulcer stage or prosthesis type row
#' @param medication_slots,skin_treatment_slots number of packed slots in the frequency lists
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' amputation_characteristics_table("Replace with Analytic Tibble")
amputation_characteristics_table <- function(analytic, anchor = c("first_injection", "consent"), display_years = TRUE,
                                             regular_levels = regular_use_levels,
                                             days_since_first_injection = "amputation_days_since",
                                             days_since_consent = "amputation_days", cause = "amputation_cause",
                                             side = "amputation_side", days_per_week = "prosthesis_days_per_week",
                                             hours_per_day = "prosthesis_hours_per_day", devices = "ambulatory_device_list",
                                             ulcer_stage = "ulcer_stage", prosthesis_type = "prosthesis_type",
                                             comfort_sit = "socket_comfort_score_sit", comfort_stand = "socket_comfort_score_stand",
                                             comfort_walk = "socket_comfort_score_walk", medications = "medication_frequency_list",
                                             skin_treatments = "meds_skin_list", medication_slots = 6, skin_treatment_slots = 5) {
  anchor <- match.arg(anchor)
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", days_since_first_injection, days_since_consent, cause, side, days_per_week, hours_per_day,
                           devices, ulcer_stage, prosthesis_type, comfort_sit, comfort_stand, comfort_walk, medications, skin_treatments),
    example_types = c("Boolean", "Number", "Number", "Category", "NamedCategory['Left' 'Right']", "Number", "Number", "Category-NS",
                      "Category", "Category", "Number", "Number", "Number",
                      "NamedCategory['Daily; Did not use; Did not use; Did not use; Did not use; Did not use']",
                      "NamedCategory['Daily; Did not use; Did not use; Did not use; Did not use']"))
  constructs <- list(days_since_first_injection = days_since_first_injection, days_since_consent = days_since_consent,
                     cause = cause, side = side, days_per_week = days_per_week, hours_per_day = hours_per_day,
                     devices = devices, ulcer_stage = ulcer_stage, prosthesis_type = prosthesis_type,
                     comfort_sit = comfort_sit, comfort_stand = comfort_stand, comfort_walk = comfort_walk,
                     medications = medications, skin_treatments = skin_treatments,
                     medication_slots = medication_slots, skin_treatment_slots = skin_treatment_slots)
  require_constructs(analytic, c("enrolled", unlist(constructs[c("cause", "side", "days_per_week", "hours_per_day", "devices", "ulcer_stage",
                                                                "prosthesis_type", "comfort_sit", "comfort_stand", "comfort_walk",
                                                                "medications", "skin_treatments")])), "amputation_characteristics_table")
  df <- analytic %>% filter(enrolled %in% TRUE)
  rows <- amputation_rows(df, amputation_levels(df, constructs), constructs, anchor, display_years, regular_levels)
  kable_indented_rows(rows, c("Characteristic", paste0("Enrolled (n = ", nrow(df), ")")))
}

# ---- Safety displays (open) ------------------------------------------------------------------

complication_data_example_type <- "(';new_row: ', '|')FollowupPeriod|Character|Character|NamedCategory['Local injection reactions' 'Small hemorrhage' 'Edema' 'Nodules/papules' 'Irritation' 'Dermatitis' 'Pruritus' 'Cellulitis' 'Other']|Character|Date|NamedCategory['Definitely related' 'Probably related' 'Possibly related' 'Unlikely related' 'Unrelated']|NamedCategory['Mild' 'Moderate' 'Severe and Undesirable']|NamedCategory['Operative' 'Non-operative' 'No treatment']|Character"
sae_data_example_type <- "(';new_row: ', '|')FacilityCode|Character|Character|Date|Date|Number|NamedCategory['Possibly Related' 'Unlikely Related' 'Unrelated']|NamedCategory['Possibly Related' 'Probably Not Related']|Character|Character"
followup_data_example_type <- "(';', ',')FollowupPeriod|FollowupPeriod|Form|FollowupStatus|Date"

#' Participants with complications
#'
#' @description
#' Participants with one or more events in each safety category from
#' complication_categories(): category, participants with events, denominator with known
#' ascertainment, percent and the number with unknown ascertainment. Counting participants
#' is numerically different from counting complications, since one participant can have
#' several. The first row (any complication) uses the verified count construct together with
#' the long records; all-SAE coverage is shown separately from the narrower related SAE row.
#'
#' @param analytic analytic data set that must include study_id, enrolled, complication_data,
#' sae_data, followup_data and the count and exposure constructs
#' @param categories mapping from complication_categories()
#' @param count_construct verified total event count per participant
#' @param exposure_construct verified person-days of follow-up per participant
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' participants_w_complications("Replace with Analytic Tibble")
participants_w_complications <- function(analytic, categories = complication_categories(),
                                         count_construct = "complication_count",
                                         exposure_construct = "last_followup_days") {
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "complication_data", "sae_data", "followup_data", count_construct, exposure_construct),
    example_types = c("Boolean", complication_data_example_type, sae_data_example_type, followup_data_example_type,
                      "Number", "Number"))
  participants <- participant_event_summary(analytic, categories, count_construct, exposure_construct)
  pooled <- pooled_event_risks(participants, categories)
  out <- pooled %>%
    transmute(Category = category,
              `Participants with >=1 event` = participants_with_event,
              `Denominator (known ascertainment)` = denominator,
              Percent = ifelse(denominator > 0,
                               paste0(trimws(format(round(100 * participants_with_event / denominator, 1), nsmall = 1)), "%"),
                               ""),
              `Unknown ascertainment` = unknown_ascertainment)
  kable(out, format = "html", align = "l") %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    add_footnote(c(paste0("Enrolled participants: ", nrow(participants), ". Ascertainment is unknown when a participant ",
                          "has neither a complication record nor a completed follow-up form.")), notation = "number")
}

#' Total events and incidence rates
#'
#' @description
#' Pooled companion to closed_event_rate_analysis: for each safety category, the number of
#' participants with known ascertainment and positive exposure, the total events, the
#' person-days and the rate per rate_unit person-days, with no group comparison. The any
#' complication row uses the verified count construct; category rows use the long records
#' with the same verified exposure, and say so. Participants with missing or nonpositive
#' exposure are excluded and counted.
#'
#' @inheritParams participants_w_complications
#' @param rate_unit person-days per rate unit (100 gives events per 100 person-days)
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' event_rate_summary("Replace with Analytic Tibble")
event_rate_summary <- function(analytic, categories = complication_categories(),
                               count_construct = "complication_count",
                               exposure_construct = "last_followup_days", rate_unit = 100) {
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "complication_data", "sae_data", "followup_data", count_construct, exposure_construct),
    example_types = c("Boolean", complication_data_example_type, sae_data_example_type, followup_data_example_type,
                      "Number", "Number"))
  participants <- participant_event_summary(analytic, categories, count_construct, exposure_construct)
  usable <- participants %>% filter(exposure_valid, ascertained)
  rows <- event_category_rows(categories)
  out <- bind_rows(lapply(seq_len(nrow(rows)), function(i) {
    events <- sum(usable[[rows$count[i]]], na.rm = TRUE)
    days <- sum(usable$exposure_days)
    tibble(Category = rows$category[i], `Count source` = rows$count_source[i], Participants = nrow(usable),
           Events = events, `Person-days` = days, Rate = fmt_number(rate_unit * events / days))
  }))
  names(out)[names(out) == "Rate"] <- paste0("Rate per ", rate_unit, " person-days")
  kable(out, format = "html", align = "l") %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    add_footnote(c(paste0(nrow(participants) - nrow(usable), " participant(s) excluded for missing/nonpositive exposure ",
                          "or unknown ascertainment.")), notation = "number")
}

# ---- Repeated location measurements (open) ----------------------------------------------------

#' Descriptive summary of paired location changes by group
#' @noRd
measurement_descriptives <- function(change_grouped, positions, group_col) {
  by_loc <- change_grouped %>%
    group_by(.data[[group_col]], position) %>%
    summarise(n_baseline = sum(!is.na(baseline)), mean_baseline = mean(baseline, na.rm = TRUE),
              sd_baseline = stats::sd(baseline, na.rm = TRUE),
              n_followup = sum(!is.na(followup)), mean_followup = mean(followup, na.rm = TRUE),
              sd_followup = stats::sd(followup, na.rm = TRUE),
              n_paired = sum(paired), mean_change = mean(change, na.rm = TRUE),
              sd_change = stats::sd(change, na.rm = TRUE), .groups = "drop")
  participant <- change_grouped %>%
    group_by(.data[[group_col]], study_id) %>%
    summarise(baseline = if (any(!is.na(baseline))) mean(baseline, na.rm = TRUE) else NA_real_,
              followup = if (any(!is.na(followup))) mean(followup, na.rm = TRUE) else NA_real_,
              change = if (any(paired)) mean(change[paired]) else NA_real_,
              paired = any(paired), .groups = "drop")
  overall <- participant %>%
    group_by(.data[[group_col]]) %>%
    summarise(position = "All locations (participant mean)",
              n_baseline = sum(!is.na(baseline)), mean_baseline = mean(baseline, na.rm = TRUE),
              sd_baseline = stats::sd(baseline, na.rm = TRUE),
              n_followup = sum(!is.na(followup)), mean_followup = mean(followup, na.rm = TRUE),
              sd_followup = stats::sd(followup, na.rm = TRUE),
              n_paired = sum(paired), mean_change = mean(change, na.rm = TRUE),
              sd_change = stats::sd(change, na.rm = TRUE), .groups = "drop")
  desc <- bind_rows(by_loc, overall) %>%
    mutate(position = factor(position, levels = c(positions, "All locations (participant mean)"))) %>%
    arrange(.data[[group_col]], position) %>%
    mutate(across(c(mean_baseline, sd_baseline, mean_followup, sd_followup, mean_change, sd_change),
                  ~ ifelse(is.nan(.x), NA_real_, .x)))
  list(descriptive = desc, participant = participant)
}

#' Positions in display order: clock positions first, then any others
#' @noRd
measurement_positions <- function(x) {
  clock <- c("12 o'clock", "3 o'clock", "6 o'clock", "9 o'clock")
  c(intersect(clock, unique(x)), sort(setdiff(unique(x), clock)))
}

#' Example type string for a packed measurement construct
#' @noRd
measurement_example_type <- function(fields, value_field) {
  types <- vapply(fields, function(f) switch(f,
    set = "NamedCategory['set_1']",
    event = "NamedCategory['injection_1' '3_month' '1_month' '2_month']",
    position = "NamedCategory['12 oclock' '3 oclock' '6 oclock' '9 oclock']",
    if (f == value_field) "Number" else "Number-U6"), character(1))
  paste0("(';', ',')", paste(types, collapse = "|"))
}

#' Open descriptive rows for a measurement table
#' @noRd
measurement_table_rows <- function(change, positions) {
  desc <- measurement_descriptives(change %>% mutate(group = "all"), positions, "group")$descriptive
  desc %>% transmute(
    Construct = as.character(position),
    `Pre n` = n_baseline,
    `Pre, mean (SD)` = ifelse(n_baseline > 0, paste0(fmt_number(mean_baseline), " (", fmt_number(sd_baseline), ")"), "-"),
    `Follow-up n` = n_followup,
    `Follow-up, mean (SD)` = ifelse(n_followup > 0, paste0(fmt_number(mean_followup), " (", fmt_number(sd_followup), ")"), "-"),
    `Paired n` = n_paired,
    `Change, mean (SD)` = ifelse(n_paired > 0, paste0(fmt_number(mean_change), " (", fmt_number(sd_change), ")"), "-"),
    Is_Header = TRUE, Is_Subheader = FALSE)
}

#' Open measurement table from paired changes
#' @noRd
measurement_table_open <- function(change, followup_label, include_per_participant_values, unit) {
  positions <- measurement_positions(change$position)
  summary_rows <- measurement_table_rows(change, positions)
  if (include_per_participant_values) {
    indiv <- change %>%
      group_by(position) %>%
      slice_sample(prop = 1) %>%
      ungroup() %>%
      transmute(position, Construct = "",
                `Pre n` = n_valid_baseline,
                `Pre, mean (SD)` = ifelse(is.na(baseline), "-", fmt_number(baseline)),
                `Follow-up n` = n_valid_followup,
                `Follow-up, mean (SD)` = ifelse(is.na(followup), "-", fmt_number(followup)),
                `Paired n` = as.integer(paired),
                `Change, mean (SD)` = ifelse(paired, fmt_number(change), "-"),
                Is_Header = FALSE, Is_Subheader = FALSE, sort_key = 2)
    sub <- summary_rows %>% filter(Construct != "All locations (participant mean)") %>%
      transmute(position = Construct, Construct = "Per participant values (location mean; n = usable readings)",
                `Pre n` = NA_integer_, `Pre, mean (SD)` = "", `Follow-up n` = NA_integer_, `Follow-up, mean (SD)` = "",
                `Paired n` = NA_integer_, `Change, mean (SD)` = "", Is_Header = FALSE, Is_Subheader = TRUE, sort_key = 1)
    table_raw <- bind_rows(summary_rows %>% mutate(position = Construct, sort_key = 0), sub, indiv) %>%
      mutate(position = factor(position, levels = unique(summary_rows$Construct))) %>%
      arrange(position, sort_key) %>%
      select(-sort_key, -position) %>%
      mutate(across(c(`Pre n`, `Follow-up n`, `Paired n`), ~ ifelse(is.na(.x), "", as.character(.x))))
  } else {
    table_raw <- summary_rows
  }
  unit_label <- if (nzchar(unit)) paste0(" ", unit) else ""
  col_names <- c("Position", "Pre-injection n", paste0("Pre-injection", unit_label, ", mean (SD)"),
                 paste0(followup_label, " n"), paste0(followup_label, " post-injection", unit_label, ", mean (SD)"),
                 "Paired n", paste0("Change (", followup_label, " minus pre), mean (SD)"))
  kable_indented_rows(table_raw, col_names, indent_flag = "Is_Subheader")
}

#' Paired location changes from an analytic data set for one endpoint
#' @noRd
location_change_data <- function(analytic, readings_constructs, fields, value_field, baseline_event, followup_event,
                                 set, keep, min_valid) {
  long <- unpack_measurement_readings(analytic, readings_constructs, fields, value_field) %>%
    keep_measurement_rows(keep)
  if (!set %in% long$set) stop("set '", set, "' has no readings in ", paste(readings_constructs, collapse = ", "))
  vm <- measurement_visit_means(long, min_valid) %>% filter(set == !!set)
  list(long = long, visit_means = vm, change = measurement_visit_change(vm, baseline_event, followup_event),
       parse_failures = sum(long$parse_failed))
}

#' Footnote line for readings that failed numeric conversion
#' @noRd
parse_failure_note <- function(n_failed, value_field) {
  if (n_failed == 0) return(NULL)
  paste0(n_failed, " ", value_field, " value(s) could not be read as numbers and are treated as missing.")
}

#' Location Measurement Summary Table
#'
#' @description
#' Pooled pre-to-follow-up summary of a repeated location measurement (durometer readings,
#' OCT width, or any packed measurement family) for enrolled participants. At each location
#' the visit values are the mean of usable readings within participant, visit and location,
#' summarised as observed n and mean (SD), with the paired n and mean (SD) of the
#' within-participant change (follow-up minus baseline, calculated only when both visits are
#' available). An overall row uses the participant mean across locations. A missing
#' follow-up visit shows as unavailable rather than stopping the table. With
#' include_per_participant_values = TRUE, anonymised and shuffled per-participant location
#' means are listed beneath each location. durometer_readings_table and oct_readings_table
#' are convenience wrappers for the default endpoints.
#'
#' @param analytic analytic data set that must include enrolled, study_id and the readings
#' construct
#' @param readings_construct packed measurement construct
#' @param fields packed field names, including set, event and position
#' @param value_field the packed field holding the measurement
#' @param baseline_event,followup_event event labels of the two visits compared
#' @param followup_label display label for the follow-up visit
#' @param set measurement set summarised
#' @param unit unit label for the column headers
#' @param keep optional named list of packed-field values to keep (for example
#' list(orientation = "H")); NULL keeps every reading
#' @param include_per_participant_values list shuffled per-participant values
#' @param min_valid minimum usable readings for a location mean
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' location_measurement_table("Replace with Analytic Tibble")
location_measurement_table <- function(analytic, readings_construct = "durometer_readings_set_1",
                                       fields = c("set", "event", "position", "injection", "reading"),
                                       value_field = "reading", baseline_event = "injection_1",
                                       followup_event = "3_month", followup_label = "3 Month", set = "set_1",
                                       unit = "", keep = NULL, include_per_participant_values = FALSE,
                                       min_valid = 1) {
  analytic <- if_needed_generate_example_data(
    analytic, example_constructs = c("enrolled", readings_construct),
    example_types = c("Boolean", measurement_example_type(fields, value_field)))
  d <- location_change_data(analytic, readings_construct, fields, value_field, baseline_event, followup_event, set, keep, min_valid)
  out <- measurement_table_open(d$change, followup_label, include_per_participant_values, unit)
  note <- parse_failure_note(d$parse_failures, value_field)
  if (!is.null(note)) out <- out %>% add_footnote(note, notation = "symbol")
  out
}

#' Follow-up event and label for a readings table mode
#' @noRd
mode_followup <- function(mode) {
  switch(mode,
         "1mo" = c(event = "1_month", label = "1 Month"),
         "2mo" = c(event = "2_month", label = "2 Month"),
         "3mo" = c(event = "3_month", label = "3 Month"),
         stop("mode must be one of '1mo', '2mo', '3mo'"))
}

#' Durometer Readings Summary Table
#'
#' @description
#' location_measurement_table for the durometer endpoint (durometer_readings_set_1, fields
#' set, event, position, injection, reading; the injection field is the reading number).
#'
#' @inheritParams location_measurement_table
#' @param mode "1mo", "2mo" or "3mo": the follow-up visit compared with injection_1
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' durometer_readings_table("Replace with Analytic Tibble", mode = "3mo")
durometer_readings_table <- function(analytic, mode = "3mo", include_per_participant_values = FALSE, min_valid = 1) {
  ev <- mode_followup(mode)
  ep <- default_measurement_endpoints()$durometer
  location_measurement_table(analytic, ep$readings_constructs[1], ep$fields, ep$value_field, "injection_1",
                             ev[["event"]], ev[["label"]], "set_1", ep$unit, NULL, include_per_participant_values, min_valid)
}

#' OCT Readings Summary Table
#'
#' @description
#' location_measurement_table for the OCT width endpoint (oct_readings_set_1, fields set,
#' event, position, orientation, area, length, width). Each participant/location/visit value
#' is the mean of all usable images across orientations unless orientations restricts the
#' aggregation explicitly.
#'
#' @inheritParams durometer_readings_table
#' @param orientations optional orientations to include; NULL uses all images
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' oct_readings_table("Replace with Analytic Tibble", mode = "3mo")
oct_readings_table <- function(analytic, mode = "3mo", include_per_participant_values = FALSE, orientations = NULL,
                               min_valid = 1) {
  ev <- mode_followup(mode)
  ep <- default_measurement_endpoints()$oct
  keep <- if (is.null(orientations)) NULL else list(orientation = orientations)
  location_measurement_table(analytic, ep$readings_constructs[1], ep$fields, ep$value_field, "injection_1",
                             ev[["event"]], ev[["label"]], "set_1", "", keep, include_per_participant_values, min_valid)
}

# ---- Patient-reported outcomes (open) --------------------------------------------------------

#' Observed n, mean (SD) and missing n per instrument and visit
#' @noRd
score_family_rows <- function(long, n_total) {
  long %>%
    group_by(instrument, visit) %>%
    summarise(n = sum(!is.na(score)), msd = mean_sd_or_dash(score, 1), .groups = "drop") %>%
    mutate(missing = n_total - n)
}

#' Patient Reported Outcomes Summary Table
#'
#' @description
#' Each score family at baseline and one, two and three months for enrolled participants:
#' observed n, mean (SD) and missing n. Scores are used as exported. The PROMIS-29 domains
#' come from the packed promis_construct (T-scores for the seven scored domains, the 0-10
#' rating for pain intensity). Every named construct must be in the export; NULL omits the
#' PROMIS construct and a shorter score_families list omits a family.
#'
#' @param analytic analytic data set that must include enrolled, study_id and the score
#' constructs named in score_families
#' @param score_families named list from default_score_families
#' @param promis_construct packed PROMIS-29 construct (visit, domain, items_answered,
#' raw_score, t_score); NULL to omit
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' patient_reported_outcomes_table("Replace with Analytic Tibble")
patient_reported_outcomes_table <- function(analytic, score_families = default_score_families(),
                                            promis_construct = "promis_data") {
  constructs <- unlist(score_families, use.names = FALSE)
  analytic <- if_needed_generate_example_data(
    analytic, example_constructs = c("enrolled", constructs, promis_construct),
    example_types = c("Boolean", rep("Number", length(constructs)), if (!is.null(promis_construct)) promis_data_example_type))
  long <- unpack_score_families(analytic, score_families, promis_construct = promis_construct)
  n_total <- length(enrolled_study_ids(analytic))
  rows <- score_family_rows(long, n_total)
  table_raw <- bind_rows(lapply(levels(rows$instrument), function(inst) {
    r <- rows %>% filter(instrument == inst)
    bind_rows(tibble(Construct = inst, n = "", `Mean (SD)` = "", Missing = "", Is_Header = TRUE),
              r %>% transmute(Construct = paste0(as.character(visit), ", mean (SD)"), n = as.character(n),
                              `Mean (SD)` = msd, Missing = as.character(missing), Is_Header = FALSE))
  }))
  kable_indented_rows(table_raw, c("Instrument / visit", "Observed n", "Mean (SD)", "Missing n")) %>%
    add_footnote(c(paste0("Enrolled participants: ", n_total, ".")), notation = "number")
}

# ---- Endpoint availability (open) ------------------------------------------------------------

#' Availability rows for a set of study IDs
#' @noRd
endpoint_availability_rows <- function(analytic, ids, measurements, score_families, periods, followup_form,
                                       promis_construct = "promis_data") {
  n_ids <- length(ids)
  sub <- analytic %>% mutate(study_id = as.character(study_id)) %>% filter(study_id %in% ids)
  fu <- unpack_followup_data(sub)
  overall <- fu %>% filter(form == followup_form) %>%
    group_by(followup_period) %>%
    summarise(expected = sum(!status_base %in% "Not Expected"), visit_completed = sum(visit_complete),
              not_expected = sum(status_base %in% "Not Expected"), .groups = "drop")
  scores <- unpack_score_families(sub, score_families, promis_construct = promis_construct)
  measurement_row <- function(ep, label, period) {
    vm <- measurement_visit_means(unpack_measurement_readings(sub, ep$readings_constructs[1], ep$fields, ep$value_field)) %>%
      filter(set == "set_1")
    ev <- periods[[period]]
    present <- vm %>% filter(event == ev, available) %>% group_by(study_id) %>% summarise(locs = n_distinct(position), .groups = "drop")
    base <- vm %>% filter(event == "injection_1", available) %>% distinct(study_id, position)
    paired <- vm %>% filter(event == ev, available) %>% distinct(study_id, position) %>%
      inner_join(base, by = c("study_id", "position")) %>% distinct(study_id) %>% nrow()
    tibble(period = period, endpoint = label, outcome_available = nrow(present),
           locations_available = sum(present$locs), paired_with_baseline = paired)
  }
  score_row <- function(period) {
    p <- scores %>% filter(as.character(visit) == period) %>% group_by(instrument) %>%
      summarise(available = sum(!is.na(score)), .groups = "drop")
    b <- scores %>% filter(as.character(visit) == "Baseline", !is.na(score)) %>% select(study_id, instrument)
    paired <- scores %>% filter(as.character(visit) == period, !is.na(score)) %>%
      inner_join(b, by = c("study_id", "instrument")) %>% count(instrument, name = "paired")
    p %>% left_join(paired, by = "instrument") %>%
      transmute(period = period, endpoint = paste0("PRO: ", instrument), outcome_available = available,
                locations_available = NA_integer_, paired_with_baseline = replace_na(paired, 0L))
  }
  rows <- bind_rows(lapply(names(periods), function(p) {
    bind_rows(bind_rows(lapply(names(measurements), function(m) measurement_row(measurements[[m]], measurements[[m]]$label, p))),
              score_row(p))
  }))
  rows %>%
    left_join(overall, by = c("period" = "followup_period")) %>%
    mutate(expected = replace_na(expected, 0L), visit_completed = replace_na(visit_completed, 0L),
           not_expected = replace_na(not_expected, 0L), outcome_missing = pmax(expected - outcome_available, 0L),
           enrolled = n_ids)
}

#' Follow-up and Endpoint Availability Table
#'
#' @description
#' Rows per follow-up visit and endpoint (each measurement endpoint and each score family)
#' with the number of enrolled participants expected at the visit (the Overall follow-up form
#' not "Not Expected"), the number with the visit completed, the number with the outcome
#' available, the number expected but without the outcome, the number not expected, the
#' locations available and the number paired with a baseline value. Participant n is
#' distinct from observation n, and a completed visit is never equated with every endpoint
#' being available. Extends expected_and_followup_visit_overall rather than replacing it.
#'
#' @param analytic analytic data set that must include study_id, enrolled, followup_data and
#' the measurement and score constructs
#' @param measurements named list of endpoint definitions from default_measurement_endpoints
#' @param score_families named list from default_score_families
#' @param periods named character vector mapping follow-up period labels to measurement
#' event labels
#' @param followup_form the followup_data form whose status defines expected visits
#' @param promis_construct packed PROMIS-29 construct whose domains are listed as endpoints; NULL to omit
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' endpoint_availability_table("Replace with Analytic Tibble")
endpoint_availability_table <- function(analytic, measurements = default_measurement_endpoints(),
                                        score_families = default_score_families(),
                                        periods = c("1 Month" = "1_month", "2 Month" = "2_month", "3 Month" = "3_month"),
                                        followup_form = "Overall", promis_construct = "promis_data") {
  analytic <- if_needed_generate_example_data(analytic, example_constructs = availability_example_constructs(measurements, score_families, promis_construct),
      example_types = availability_example_types(measurements, score_families, promis_construct))
  ids <- enrolled_study_ids(analytic)
  rows <- endpoint_availability_rows(analytic, ids, measurements, score_families, periods, followup_form, promis_construct)
  out <- rows %>% transmute(Visit = period, Endpoint = endpoint, Enrolled = enrolled, Expected = expected,
                            `Visit completed` = visit_completed, `Outcome available (participants)` = outcome_available,
                            `Locations available` = ifelse(is.na(locations_available), "", as.character(locations_available)),
                            `Expected, outcome missing` = outcome_missing, `Not expected` = not_expected,
                            `Paired with baseline` = paired_with_baseline)
  kable(out, format = "html", align = "l") %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    collapse_rows(columns = 1, valign = "top")
}

# ---- Future-study power sensitivity (open) ---------------------------------------------------

#' Power of a two-sample t-test with unequal allocation
#' @noRd
power_two_sample <- function(delta, sd, n1, n2, alpha = 0.05) {
  df <- n1 + n2 - 2
  if (df < 1) return(NA_real_)
  ncp <- delta / (sd * sqrt(1 / n1 + 1 / n2))
  crit <- stats::qt(1 - alpha / 2, df)
  stats::pt(crit, df, ncp = ncp, lower.tail = FALSE) + stats::pt(-crit, df, ncp = ncp)
}

#' Future-Study Power Sensitivity Table
#'
#' @description
#' Required sample size for a future two-arm comparison of mean change under each supplied
#' scenario, using the noncentral t distribution with unequal allocation and inflating the
#' total for anticipated missingness. Inputs are explicit planning assumptions: the assumed
#' effect, the SD of participant change, the allocation ratio, the target power and the
#' missing fraction. Nothing is derived from a treatment-effect estimate; pooled_change_sd
#' can supply an arm-blind nuisance estimate from a location change analysis for explicitly
#' labelled blinded planning.
#'
#' @param scenarios data frame with columns scenario, effect, sd_change, allocation_ratio
#' (treatment per control), power and missing_fraction, and optionally alpha
#' @param alpha two-sided significance level used when a scenario has none
#' @param max_n_control search limit for the control-arm size
#' @param note footnote stating the provenance of the inputs
#' @param return_fit when TRUE, returns the scenario results as a tibble alongside the table
#' (as result_table)
#'
#' @return An HTML table, or a list when return_fit = TRUE.
#' @export
#'
#' @examples
#' power_sensitivity_table(data.frame(scenario = "Reference", effect = 6.4, sd_change = 8,
#'                                    allocation_ratio = 1, power = 0.8, missing_fraction = 0.1))
power_sensitivity_table <- function(scenarios, alpha = 0.05, max_n_control = 500,
                                    note = "Inputs are explicit planning assumptions; no treatment-effect estimate is used.",
                                    return_fit = FALSE) {
  needed <- c("scenario", "effect", "sd_change", "allocation_ratio", "power", "missing_fraction")
  if (!all(needed %in% names(scenarios))) stop("scenarios must have columns: ", paste(needed, collapse = ", "))
  if (!"alpha" %in% names(scenarios)) scenarios$alpha <- alpha
  results <- bind_rows(lapply(seq_len(nrow(scenarios)), function(i) {
    s <- scenarios[i, ]
    n_control <- NA_integer_
    achieved <- NA_real_
    for (n1 in 2:max_n_control) {
      n2 <- max(2, ceiling(n1 * s$allocation_ratio))
      p <- power_two_sample(s$effect, s$sd_change, n2, n1, s$alpha)
      if (!is.na(p) && p >= s$power) { n_control <- n1; achieved <- p; break }
    }
    n_treatment <- if (is.na(n_control)) NA_integer_ else max(2, ceiling(n_control * s$allocation_ratio))
    n_total <- n_control + n_treatment
    tibble(scenario = s$scenario, effect = s$effect, sd_change = s$sd_change, allocation_ratio = s$allocation_ratio,
           power = s$power, alpha = s$alpha, missing_fraction = s$missing_fraction, n_control = n_control,
           n_treatment = n_treatment, n_total = n_total, n_total_inflated = ceiling(n_total / (1 - s$missing_fraction)),
           achieved_power = achieved,
           status = if (is.na(n_control)) paste0("not reached within ", max_n_control, " per control arm") else "ok")
  }))
  out <- results %>% transmute(
    Scenario = scenario, `Assumed effect` = effect, `SD of change` = sd_change,
    `Allocation (treatment:control)` = paste0(allocation_ratio, ":1"), `Target power` = power, Alpha = alpha,
    `Anticipated missing` = paste0(round(100 * missing_fraction), "%"),
    `n control / n treatment` = paste0(n_control, " / ", n_treatment), `Total n` = n_total,
    `Total n inflated for missingness` = n_total_inflated, `Achieved power` = fmt_number(achieved_power, 3), Status = status)
  result_table <- kable(out, format = "html", align = "l") %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    add_footnote(c(note, "Two-sample t-test on participant change; total n inflated by 1 / (1 - missing fraction)."),
                 notation = "number")
  if (return_fit) return(list(result_table = result_table, results = results, scenarios = scenarios))
  result_table
}

#' Pooled (arm-blind) SD of participant change from a location change analysis
#' @noRd
pooled_change_sd <- function(result) {
  p <- result$participant %>% filter(paired)
  list(sd_change = stats::sd(p$change), n = nrow(p), endpoint = result$endpoint)
}

# ---- Qualitative review summary (open) -------------------------------------------------------

#' Summarise reviewer findings from a packed review index
#' @noRd
qualitative_review_data <- function(analytic, review_construct, fields, features, quality_field, visit_field,
                                    group_col = NULL) {
  long <- unpack_packed_construct(analytic, review_construct, fields, row_sep = ";", field_sep = ",")
  group_cols <- c(group_col, visit_field)
  summarise_feature <- function(feature) {
    long %>%
      mutate(level = replace_na(.data[[feature]], "Not recorded")) %>%
      group_by(across(all_of(group_cols)), level) %>%
      summarise(images = n(), participants = n_distinct(study_id), .groups = "drop") %>%
      mutate(feature = feature, .before = 1)
  }
  totals <- long %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(images_reviewed = n(), participants_reviewed = n_distinct(study_id),
              images_adequate = sum(.data[[quality_field]] %in% "Adequate"), .groups = "drop")
  list(long = long, table = bind_rows(lapply(features, summarise_feature)), totals = totals)
}

#' Format the review summary as a table
#' @noRd
qualitative_review_table <- function(review, visit_field, group_col = NULL, footnotes = NULL) {
  tb <- review$table %>%
    mutate(cell = paste0(images, " (", participants, ")")) %>%
    select(any_of(group_col), all_of(visit_field), feature, level, cell) %>%
    pivot_wider(names_from = all_of(visit_field), values_from = cell, values_fill = "0 (0)") %>%
    arrange(across(any_of(group_col)), feature, level)
  totals <- review$totals %>%
    mutate(feature = "Images reviewed (participants); adequate quality", level = "",
           cell = paste0(images_reviewed, " (", participants_reviewed, "); ", images_adequate)) %>%
    select(any_of(group_col), all_of(visit_field), feature, level, cell) %>%
    pivot_wider(names_from = all_of(visit_field), values_from = cell, values_fill = "0 (0)")
  out <- bind_rows(totals, tb)
  if (!is.null(group_col)) out <- out %>% rename(Arm = all_of(group_col))
  out <- out %>% rename(Feature = feature, Level = level)
  vis <- kable(out, format = "html", align = "l") %>%
    kable_styling("striped", full_width = FALSE, position = "left") %>%
    collapse_rows(columns = if (!is.null(group_col)) 1:2 else 1, valign = "top")
  if (!is.null(footnotes)) vis <- vis %>% add_footnote(footnotes, notation = "number")
  vis
}

#' Qualitative Review Summary Table
#'
#' @description
#' Counts reviewer findings from a packed review index (record separator ";", field
#' separator ","), for example the skin appearance review of redness, scaling and other
#' disturbances at each follow-up visit: images reviewed and participants per visit, and the
#' number of images (participants) at each recorded level of every feature. No score or
#' significance test is invented.
#'
#' @param analytic analytic data set that must include study_id, enrolled and the review
#' construct
#' @param review_construct name of the packed review index
#' @param fields packed field names in order
#' @param features fields whose levels are counted
#' @param quality_field field holding the image quality (counted when "Adequate")
#' @param visit_field field holding the visit label
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' qualitative_review_summary("Replace with Analytic Tibble")
qualitative_review_summary <- function(analytic, review_construct = "appearance_data",
                                       fields = c("period", "visit", "assessment_date", "location", "image_reference",
                                                  "quality", "redness", "scaling", "other_findings", "reviewer"),
                                       features = c("redness", "scaling", "other_findings"),
                                       quality_field = "quality", visit_field = "visit") {
  analytic <- if_needed_generate_example_data(analytic, example_constructs = c("enrolled", review_construct),
      example_types = c("Boolean", review_example_type(fields, features, quality_field, visit_field)))
  require_constructs(analytic, c("enrolled", review_construct), "qualitative_review_summary")
  review <- qualitative_review_data(analytic, review_construct, fields, features, quality_field, visit_field)
  qualitative_review_table(review, visit_field,
                           footnotes = c("Reviewer findings per image, counted by visit; participants may contribute several locations.",
                                         "Open summary: no treatment assignment used."))
}

# ---- Run manifest ----------------------------------------------------------------------------

#' Report Run Manifest
#'
#' @description
#' Records the analysis population, enrolled count, data cutoff, source SAP version,
#' assignment mode and package versions for a report header, so a knitted report states what
#' it was built from.
#'
#' @param analytic analytic data set that must include study_id and enrolled (and treatment_arm when
#' blinded = FALSE)
#' @param sap_version SAP version text
#' @param data_cutoff data cutoff or export date text
#' @param input_file name of the analytic export read by the report
#' @param blinded NULL for an open report; TRUE to describe the reproducible dummy assignment the closed
#' functions build from the sorted enrolled IDs and seed; FALSE to describe the actual assignment
#' @param seed seed of the dummy assignment
#' @param control_arm value of treatment_arm treated as the control group
#'
#' @return An HTML table.
#' @export
#'
#' @examples
#' report_manifest("Replace with Analytic Tibble", "v2.0", "2026-09-22", "analytic_dataset.csv")
#' report_manifest("Replace with Analytic Tibble", "v2.0", "2026-09-22", "analytic_dataset.csv", blinded = TRUE)
report_manifest <- function(analytic, sap_version, data_cutoff, input_file = NA_character_, blinded = NULL,
                            seed = 20260922, control_arm = "Group A") {
  analytic <- if_needed_generate_example_data(analytic, example_constructs = "enrolled", example_types = "Boolean")
  n_enrolled <- length(enrolled_study_ids(analytic))
  mode <- if (is.null(blinded)) "Open report: pooled, no treatment assignment used" else {
    assignment <- resolve_treatment_assignment(analytic, blinded, NULL, seed, control_arm)
    paste0(assignment$assignment_mode, " (", assignment$caption, "; contrast ", assignment$contrast,
           if (!is.null(assignment$seed)) paste0("; seed ", assignment$seed), ")")
  }
  df <- tibble(
    Item = c("Population", "Enrolled participants", "Data cutoff / export", "Source SAP", "Assignment mode",
             "Analytic input file", "VisualizationLibrary version", "VisualizationTools version", "R version"),
    Value = c("enrolled == TRUE", as.character(n_enrolled), as.character(data_cutoff), sap_version, mode,
              as.character(input_file), as.character(utils::packageVersion("VisualizationLibrary")),
              tryCatch(as.character(utils::packageVersion("VisualizationTools")), error = function(e) "not installed"),
              paste(R.version$major, R.version$minor, sep = ".")))
  kable(df, format = "html", align = "l") %>%
    kable_styling("striped", full_width = FALSE, position = "left")
}
