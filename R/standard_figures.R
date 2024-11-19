
#' DSMB Consort Diagram
#'
#' @description This function visualizes the categorical percentages of Study Status for any study, similar to the NSAID consort diagram, but with customization endpoints.
#'
#' @param analytic The analytic data set that must include the following columns: screened, eligible, consented, refused, discontinued_pre_randomization,
#'  randomized, late_ineligible, enrolled, completed, not_completed, not_expected, active, missed_final_followup
#' @param not_enrolled_other A column in the dataset for cases that are eligible but not enrolled for reasons other than refusal (optional).
#' @param completed_str A string specifying the label for the completion status box. Defaults to "Completed 12-month visit".
#' @param late_inelgible defaults to late_ineligble but can be any construct like "adjudicated_discontinued"
#' @param late_inelgible_str defaults to Late Ineligible but can be any construct like "Adjudicated Discontinued"
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' \dontrun{
#' dsmb_consort_diagram()
#' }
dsmb_consort_diagram <- function(analytic, not_enrolled_other=NULL, completed_str="Completed 12-month visit", late_ineligible="late_ineligible", late_ineligible_str="Late Ineligible", not_expected_adjudicated=FALSE){
  analytic <- analytic %>% 
  filter(screened == TRUE) 
  late_ineligible_var <- late_ineligible
  Screened <- sum(analytic$screened, na.rm=TRUE)
  Eligible <- sum(analytic$eligible, na.rm=TRUE)
  Consented <- sum(analytic %>% 
                     filter(eligible) %>% 
                     pull(consented), na.rm=TRUE)
  Refused <- sum(analytic %>% 
                   filter(eligible) %>% 
                   pull(refused), na.rm=TRUE)
  Disconintued_Pre <- sum(analytic %>% 
                            filter(eligible) %>% 
                            filter(consented) %>% 
                            pull(discontinued_pre_randomization), na.rm=TRUE)
  Randomized <- sum(analytic %>% 
                      filter(eligible) %>% 
                      filter(consented) %>% 
                      pull(randomized), na.rm=TRUE)
  Late_Ineligible <- sum(analytic %>% 
                           filter(eligible) %>% 
                           filter(consented) %>% 
                           pull(late_ineligible_var), na.rm=TRUE)
  Enrolled <- sum(analytic %>% 
                    filter(eligible) %>% 
                    filter(consented) %>% 
                    filter(randomized) %>% 
                    pull(enrolled), na.rm=TRUE)
  Ineligible <- Screened - Eligible
  if(is.null(not_enrolled_other)){
    Not_Enrolled_Other <- Eligible - Consented - Refused
  } else{
    temp <- analytic %>% 
      filter(eligible)
    Not_Enrolled_Other <- sum(temp[[not_enrolled_other]], na.rm=TRUE)
  }
  
  en_df <- analytic %>% 
    filter(eligible) %>% 
    filter(consented) %>% 
    filter(randomized) %>%
    filter(enrolled)
  
  complete <- sum(en_df$completed, na.rm = TRUE)
  not_complete <- sum(en_df$not_completed, na.rm = TRUE)
  missed <- sum(en_df$missed_final_followup, na.rm = TRUE)
  active <- sum(en_df$active, na.rm = TRUE)
  not_expected <- sum(en_df$not_expected, na.rm = TRUE)
  if(not_expected_adjudicated){
    not_expected_str= "Adjudicated Not Expected"
  } else{
    not_expected_str= "Not Expected"
  }
  
  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      start [style="rounded,filled", fillcolor="#ccccff", pos="6,12!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      elig [style="rounded,filled", fillcolor="#ccccff", pos="6,10!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      cons [style="rounded,filled", fillcolor="#ccccff", pos="6,8!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];
      pre_rand [style="rounded,filled", fillcolor="#ccccff", pos="10,8!", shape = box, width=2.4, height=1, label = "Discontinued (n=',Disconintued_Pre,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="6,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      late_inelig [style="rounded,filled", fillcolor="#ccccff", pos="10,6!", shape = box, width=2.4, height=1, label = "',late_ineligible_str,', (n=',Late_Ineligible,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="6,4!", shape = box, width=2.4, height=1, label = "Enrolled (n=',Enrolled,')"];
      
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Refused (n=',Refused,')"];
      
      not_enrolled [style="rounded,filled", fillcolor="#ccccff", pos="2,10!", shape = box, width=2.4, height=1, label = "Not Enrolled Other (n=',Not_Enrolled_Other,')"];

      active [style="rounded,filled", fillcolor="#ccccff", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_complete [style="rounded,filled", fillcolor="#ccccff", pos="4,0!", shape = box, width=2.4, height=1, label = "Not Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="8,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="12,0!", shape = box, width=2.4, height=1, label = "',completed_str,' (n=',complete,')"];
      
      # Relationships
      start -> elig
      start -> ineligible
      elig -> cons
      elig -> refused
      elig -> not_enrolled
      cons -> rand
      cons -> pre_rand
      rand -> enrolled
      rand -> late_inelig
      enrolled -> active
      enrolled -> not_complete
      enrolled -> not_expected
      enrolled -> fu_complete
      
    }
  '))
  svg_content <- DiagrammeRsvg::export_svg(consort_diagram)
  temp_svg_path <- tempfile(fileext = ".svg")
  writeLines(svg_content, temp_svg_path)
  temp_png_path <- tempfile(fileext = ".png")
  rsvg::rsvg_png(temp_svg_path, temp_png_path, width = 1200, height = 1200)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Consort Diagram" style="max-width: 100%%; width: 1200px;">', image_data)
  file.remove(c(temp_svg_path, temp_png_path))
  return(img_tag)
}


#' DSMB NSAID Consort Diagram
#'
#' @description This function visualizes the categorical percentages of Study Status
#' for the NSAID study
#'
#' @param analytic This is the analytic data set that must include screened, eligible, 
#' consented, not_consented, randomized, enrolled, refused, df_surg_completed, completed, not_completed, not_expected, active
#' @param final_period Defaults to 12 Month
#' @param definitive_event Event either DF or DWC
#' @param not_expected_adjudicated whether to note that the Not Expected was adjudicated
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' dsmb_nsaid_consort_diagram()
#' }
dsmb_nsaid_consort_diagram <- function(analytic, final_period="12 Month", not_expected_adjudicated=TRUE){
  analytic <- analytic %>% 
    filter(screened == TRUE) 
  
  Screened <- sum(analytic$screened, na.rm=TRUE)
  Eligible <- sum(analytic$eligible, na.rm=TRUE)
  Ineligible <- Screened - Eligible
  
  Consented <- sum(analytic %>% 
                     filter(eligible) %>% 
                     pull(consented), na.rm=TRUE)
  Refused <- sum(analytic %>% 
                   filter(eligible) %>% 
                   pull(refused), na.rm=TRUE)
  Not_Consented <- sum(analytic %>% 
                         filter(eligible) %>% 
                         pull(not_consented), na.rm=TRUE)
  Consented <- sum(analytic %>% 
                      filter(eligible) %>% 
                      pull(consented), na.rm=TRUE)
  
  Randomized <- sum(analytic %>% 
                      filter(eligible) %>% 
                      filter(consented) %>% 
                      pull(randomized), na.rm=TRUE)
  
  Enrolled <- sum(analytic %>% 
                    filter(eligible) %>% 
                    filter(consented) %>% 
                    filter(randomized) %>% 
                    pull(enrolled), na.rm=TRUE)
  Adjudicated_Discontinuation <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>%
                        pull(adjudicated_discontinued), na.rm=TRUE)
  Definitive_Fixation_Complete <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>%
                        filter(enrolled) %>% 
                        pull(df_surg_completed), na.rm=TRUE)
  
  fu_df <- analytic %>% 
    filter(eligible) %>% 
    filter(consented) %>% 
    filter(randomized) %>%
    filter(enrolled) %>% 
    filter(df_surg_completed)
  
  complete <- sum(fu_df$completed, na.rm = TRUE)
  not_complete <- sum(fu_df$not_completed, na.rm = TRUE)
  missed <- sum(fu_df$missed_final_followup, na.rm = TRUE)
  active <- sum(fu_df$active, na.rm = TRUE)
  not_expected <- sum(fu_df$not_expected, na.rm = TRUE)
  if(not_expected_adjudicated){
    not_expected_str= "Adjudicated Not Expected"
  } else{
    not_expected_str= "Not Expected"
  }
  
  NSAID_consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="6,12!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#ccccff", pos="6,10!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Not Consented (n=',Not_Consented,')\nRefused (n=',Refused,')"];

      cons [style="rounded,filled", fillcolor="#ccccff", pos="6,8!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="6,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="6,4!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',Enrolled,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="10,6!", shape = box, width=2.4, height=1, label = "Adjudicated Discontinued (n=',Adjudicated_Discontinuation,')"];

      compl [style="rounded,filled", fillcolor="#ccccff", pos="6,2!", shape = box, width=2.4, height=1, label = "Definitive Fixation Complete (n=',Definitive_Fixation_Complete,')"];
      
      active [style="rounded,filled", fillcolor="#ccccff", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_complete [style="rounded,filled", fillcolor="#ccccff", pos="4,0!", shape = box, width=2.4, height=1, label = "Not Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="8,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="12,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')"];
      
      # Relationships
      screened -> eligible
      screened -> ineligible
      eligible -> cons
      cons -> rand
      rand -> discon
      eligible -> refused
      rand -> enrolled
      enrolled -> compl
      compl -> active
      compl -> not_complete
      compl -> not_expected
      compl -> fu_complete
      
    }
  '))
  svg_content <- DiagrammeRsvg::export_svg(NSAID_consort_diagram)
  temp_svg_path <- tempfile(fileext = ".svg")
  writeLines(svg_content, temp_svg_path)
  temp_png_path <- tempfile(fileext = ".png")
  rsvg::rsvg_png(temp_svg_path, temp_png_path, width = 1200, height = 1200)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Consort Diagram" style="max-width: 100%%; width: 1200px;">', image_data)
  file.remove(c(temp_svg_path, temp_png_path))
  return(img_tag)
}


#' Cumulative percentage for ankle injuries
#'
#' @description This function visualizes the Cumulative percentage for number of patients with ankle injuries over the period
#' of Year-Months(YYYY-MM) out of 526
#'
#' @param analytic This is the analytic data set that must include study_id, injury_type, enrolled
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' cumulative_percentage_ankle_injuries()
#' }
cumulative_percentage_ankle_injuries <- function(analytic){

  df <- analytic %>%  select(study_id, injury_type, enrolled, consent_date) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(injury_type)) %>% 
    filter(!is.na(consent_date)) %>% 
    filter(injury_type == "ankle")
  
  df$consent_date <- ymd(df$consent_date)
  
  yyyy_mm <- df %>% mutate(year_month = str_remove(consent_date, '...$')) %>% 
    group_by(year_month) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    arrange(year_month) %>%
    mutate(
      cumulative_value = cumsum(Total),
      cumulative_percentage = (cumulative_value / 526))
  
  
  g <- ggplot(yyyy_mm, aes(x = factor(year_month), y = cumulative_percentage)) +
    geom_bar(stat = "identity", fill = "yellow", color = "black", size = 0.3) +
    labs(x = "Month", y = "Cumulative Percent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +  # Rotate labels vertically
    scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0,1))
  
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Percentage Enrollment for Ankle injury" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}


#' Cumulative percentage for Tibial Plateau injuries
#'
#' @description This function visualizes the Cumulative percentage for number of patients with Plateau injuries 
#' over the period of Year-Months(YYYY-MM, consent_date) out of 100
#'
#' @param analytic This is the analytic data set that must include study_id, injury_type, enrolled
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' cumulative_percentage_plateau_injuries()
#' }
cumulative_percentage_plateau_injuries <- function(analytic){

  df <- analytic %>%  select(study_id, injury_type, enrolled, consent_date) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(injury_type)) %>%
    filter(!is.na(consent_date)) %>% 
    filter(injury_type == "plateau")
  
  df$consent_date <- ymd(df$consent_date)
  
  yyyy_mm <- df %>% 
    mutate(year_month = str_remove(consent_date, '...$')) %>% 
    group_by(year_month) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    arrange(year_month) %>%
    mutate(
      cumulative_value = cumsum(Total),
      cumulative_percentage = (cumulative_value / 100))
  
  
  g <- ggplot(yyyy_mm, aes(x = factor(year_month), y = cumulative_percentage)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", size = 0.3) +
    labs(x = "Month", y = "Cumulative Percent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +  # Rotate labels vertically
    scale_y_continuous(labels = scales::percent_format(), limits = c(0,1))
  
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Percentage Enrollment for Plateau injury" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
   return(img_tag)
   
}

#' Enrollment of subjects for ankle and plateau injuries by each site
#'
#' @description This function visualizes the enrollment by each site for each injury_type
#'
#' @param analytic This is the analytic data set that must include study_id, injury_type, enrolled, facilitycode
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' enrollment_by_injury_and_site()
#' }
enrollment_by_injury_and_site <- function(analytic){
  
  df <- analytic %>%  select(study_id, injury_type, enrolled, facilitycode, consent_date) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(injury_type)) %>% 
    filter(!is.na(consent_date)) %>% 
    group_by(facilitycode, injury_type) %>%
    summarise(EnrolledPatients = n()) 
  
  g <- ggplot(df, aes(x = facilitycode, y = EnrolledPatients, fill = injury_type)) +
    geom_bar(stat = "identity", color = "black", size = 0.5, width = 0.8) +
    labs(title = "Number of patients enrolled by site and fracture type", x = "Site", y = "Number enrolled") +
    scale_fill_manual(values = c( "ankle" = "yellow", "plateau" = "blue")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",  # Center the legend at the top
          legend.title = element_blank())
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Enrollment by each injury type and site" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}


#' Enrollment of subjects by each site
#'
#' @description This function visualizes the enrollment by each site for each patient
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, facilitycode
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' enrollment_by_site()
#' }
enrollment_by_site <- function(analytic, number_order = FALSE){
  
  df <- analytic %>%  select(study_id, enrolled, facilitycode, consent_date) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(consent_date)) %>% 
    group_by(facilitycode) %>%
    summarise(EnrolledPatients = n()) %>%
    arrange(facilitycode)
  
  if (number_order) {
    df <- df %>%
      arrange(desc(EnrolledPatients))
  }
  
  g <- ggplot(df, aes(x = factor(facilitycode,  levels = facilitycode), y = EnrolledPatients)) +
    geom_bar(stat = "identity", fill = 'blue3', color = 'black', size = 0.5, width = 0.8) +
    labs(title = "Number of patients enrolled by site", x = "Site", y = "Number enrolled") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",  # Center the legend at the top
          legend.title = element_blank())
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Enrollment by site" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}

#' Cumulative enrollment
#'
#' @description This function visualizes the Cumulative number of patients enrolled
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, consent_date 
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' cumulative_enrolled()
#' }
cumulative_enrolled <- function(analytic, add_discrete=TRUE){
  
  df <- analytic %>%  select(study_id, enrolled, consent_date) %>% 
    filter(!is.na(consent_date)) %>% 
    filter(enrolled == TRUE) 
  
  df$consent_date <- ymd(df$consent_date)
  
  yyyy_mm <- df %>% 
    mutate(year_month = str_remove(consent_date, '...$')) %>% 
    group_by(year_month) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    arrange(year_month) %>%
    mutate(cumulative_value = cumsum(Total))
  
  
  if(add_discrete){
    g <- ggplot(yyyy_mm) +
      geom_bar(aes(x = factor(year_month), y = Total, group = 1), stat = "identity", fill = "blue3", color = "black", size = 0.3) +
      geom_line(aes(x = factor(year_month), y = cumulative_value), data = yyyy_mm, stat = "identity", group = 1) +  # Add the 'data' argument
      labs(title = "Cumulative Enrollment with Discrete Enrollment by Month", x = "Month", y = "Enrolled") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  } else{
    g <- ggplot(yyyy_mm) +
      geom_line(aes(x = factor(year_month), y = cumulative_value), data = yyyy_mm, stat = "identity", group = 1) +  # Add the 'data' argument
      labs(title = "Cumulative Enrollment by Month", x = "Month", y = "Enrolled") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  }

  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Enrollment with Discrete Enrollment by Month" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}

#' Monthly Discrete Enrollment
#'
#' @description This function visualizes the discrete number of patients enrolled by month
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, consent_date 
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' discrete_enrolled()
#' }
discrete_enrolled <- function(analytic){
  
  df <- analytic %>%  select(study_id, enrolled, consent_date) %>% 
    filter(!is.na(consent_date)) %>% 
    filter(enrolled == TRUE) 
  
  df$consent_date <- ymd(df$consent_date)
  
  yyyy_mm <- df %>% 
    mutate(year_month = str_remove(consent_date, '...$')) %>% 
    group_by(year_month) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    arrange(year_month) %>%
    mutate(cumulative_value = cumsum(Total))
  
  g <- ggplot(yyyy_mm) +
    geom_bar(aes(x = factor(year_month), y = Total, group = 1), stat = "identity", fill = "blue3", color = "black", size = 0.3) +
    labs(title = "Discrete Enrollment by Month", x = "Month", y = "Enrolled") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Enrollment with Discrete Enrollment by Month" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}



#' Cumulative enrollment for Length of Stay for NSAID
#'
#' @description This function visualizes the Cumulative number of patients enrolled for each bucket of number
#' of days stay during the admission
#'
#' @param analytic This is the analytic data set that must include study_id, ih_los_days
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' cumulative_enrolled_los()
#' }
cumulative_enrolled_los <- function(analytic){
  
  df <- analytic %>%  select(study_id, ih_los_days) %>% 
    filter(ih_los_days != 'Missing' & !is.na(ih_los_days))
  
  count_data <- df %>% 
    group_by(ih_los_days) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))
  
  df$ih_los_days <- factor(df$ih_los_days, levels = count_data$ih_los_days)
  
  g <- ggplot(df, aes(x = ih_los_days)) +
    geom_bar(fill = "blue", color = "white") +
    geom_text(stat = "count", aes(label = paste("N =", ..count..)), vjust = -0.5,size = 2) +
    labs(title = "Histogram of LOS Days",
         x = "Hospital Length of Stay(Number of Days)",
         y = "Cumulative Enrollment, N") +
    theme_minimal() +
    scale_x_discrete(limits = count_data$ih_los_days)
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Enrollment with Discrete Enrollment by Month" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}




#' Cumulative Enrollment with Goals
#'
#' @description This function visualizes the cumulative number of patients enrolled, accompanied by 
#' a participant goal curve which demonstrates the goal relative to the actual cumulative participant enrollment process, 
#' and a second curve which demonstrates the necessary growth in order to meet that goal, all within a specified start and end date..
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, consent_date.
#' @param start_date The start date for the analysis.
#' @param end_date The end date for the analysis.
#' @param participant_goal The goal number of participants for the study.
#'
#' @return Nothing (intended for plotting)
#' @export
#'
#' @examples
#' \dontrun{
#' cumulative_enrollment_goals()
#' }
cumulative_enrollment_goals <- function(analytic, start_date, end_date, participant_goal){
  df <- analytic %>% 
    select(study_id, enrolled, consent_date)
  
  df <- analytic %>%  select(study_id, enrolled, consent_date) %>% 
    filter(!is.na(consent_date)) %>% 
    filter(enrolled == TRUE) 
  
  yyyy_mm <- df %>% 
    mutate(year_month = as.Date(str_replace(consent_date, '...$', '-01'))) %>% 
    group_by(year_month) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    arrange(year_month) %>%
    mutate(cumulative_value = cumsum(Total))
  
  df$consent_date <- ymd(df$consent_date)
  
  most_recent_row <- yyyy_mm %>% slice_tail(n = 1)
  
  current_participants <- most_recent_row$cumulative_value
  
  most_recent_entry <- most_recent_row$year_month
  
  g <- ggplot(yyyy_mm) +
    geom_line(aes(x = year_month, y = cumulative_value), data = yyyy_mm, stat = "identity", group = 1, linewidth = 1) + 
    geom_segment(aes(x = as.Date(start_date), xend = as.Date(end_date), y = 0, yend = participant_goal), color = 'red', linetype = 2) +
    geom_segment(aes(x = as.Date(most_recent_entry), xend = as.Date(end_date), y = current_participants, yend = participant_goal), color = 'green', linetype = 2) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
    labs(title = "Cumulative Enrollment with Projections and Targets", x = "Month", y = "Enrolled") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5))
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Enrollment with Projections and Targets" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}  

#' Consort Diagram
#'
#' @description This function visualizes the categorical percentages of study status as well as followup completions
#'
#' @param analytic This is the analytic data set that must include study_id, screened, ineligible, eligible,
#' refused, consented, randomized, enrolled, time_zero, adjudicated_discontinued, completed, 
#' safety_set, exclusive_safety_set, not_completed, not_expected, active, missed_final_followup
#' @param final_period Defaults to 12 Month
#' @param definitive_event Event either DF or DWC
#' @param not_expected_adjudicated whether to note that the Not Expected was adjudicated
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' consort_diagram()
#' }
consort_diagram <- function(analytic, final_period="12 Month", definitive_event = "Definitive Fixation Complete" , not_expected_adjudicated=TRUE){
  
  df <- analytic %>% 
    select(study_id, screened, ineligible, eligible, refused, consented, randomized, enrolled, time_zero, 
           adjudicated_discontinued, completed, safety_set, exclusive_safety_set, not_completed, not_expected, active, missed_final_followup) %>% 
    mutate(time_zero = ifelse(!is.na(time_zero), TRUE, FALSE))
  
  screened <- sum(analytic$screened, na.rm = TRUE)
  
  eligible_df <- df %>% 
    filter(screened)
  
  safety <- sum(df$safety_set, na.rm = TRUE)
  ex_safety <- sum(df$exclusive_safety_set, na.rm = TRUE)
  
  eligible <- sum(eligible_df$eligible, na.rm = TRUE)
  
  ineligible <- sum(eligible_df$ineligible, na.rm = TRUE)
  
  eligble_df <- eligible_df %>% 
    filter(eligible)
  
  refused <- sum(eligble_df$refused, na.rm = TRUE)
  
  not_refused_df <- eligble_df %>% 
    filter(refused == FALSE | is.na(refused))
  
  consented <- sum(not_refused_df$consented, na.rm = TRUE)
  
  # First identity (no construct used for not consented)
  not_consented <- eligible - (consented + refused)
  
  consented_df <- eligible_df %>% 
    filter(consented)
  
  randomized <- sum(consented_df$randomized, na.rm = TRUE)
  
  not_randomized_df <- consented_df %>% 
    filter(!randomized | is.na(randomized))
  
  ed_consented <- sum(not_randomized_df$adjudicated_discontinued, na.rm = TRUE)
  
  randomized_df <- consented_df %>% 
    filter(randomized)
  
  ed_randomized <- sum(randomized_df$adjudicated_discontinued, na.rm = TRUE)
  
  enrolled_df <- randomized_df %>% 
    filter(enrolled)
  
  enrolled <- sum(enrolled_df$enrolled, na.rm = TRUE)
  df_complete <- sum(!is.na(enrolled_df$time_zero), na.rm = TRUE)
  
  fu_df <- enrolled_df %>% 
    filter(!is.na(time_zero))
  
  complete <- sum(fu_df$completed, na.rm = TRUE)
  not_complete <- sum(fu_df$not_completed, na.rm = TRUE)
  missed <- sum(fu_df$missed_final_followup, na.rm = TRUE)
  active <- sum(fu_df$active, na.rm = TRUE)
  not_expected <- sum(fu_df$not_expected, na.rm = TRUE)
  if(not_expected_adjudicated){
    not_expected_str= "Adjudicated Not Expected"
  } else{
    not_expected_str= "Not Expected"
  }
  
  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      screened [style="rounded,filled", fillcolor="#a4d3ee", pos="6,12!", shape = box, width=2.4, height=1, label = "Screened (n=',screened,')"];
      ineligible [style="rounded,filled", fillcolor="#a4d3ee", pos="10,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#a4d3ee", pos="6,10!", shape = box, width=2.4, height=1, label = "Eligible (n=',eligible,')"];
      
      refused [style="rounded,filled", fillcolor="#a4d3ee", pos="10,10!", shape = box, width=2.4, height=1, label = "Not Consented (n=',not_consented,')\nRefused (n=',refused,')"];

      consented [style="rounded,filled", fillcolor="#a4d3ee", pos="6,8!", shape = box, width=2.4, height=1, label = "Consented (n=',consented,')"];
      
      randomized [style="rounded,filled", fillcolor="#a4d3ee", pos="6,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',randomized,')"];

      ed_consented [style="rounded,filled", fillcolor="#a4d3ee", pos="10,8!", shape = box, width=2.4, height=1, label = "Adjudicated Discontinued (Consented) (n=',ed_consented,')"];
      
      ed_randomized [style="rounded,filled", fillcolor="#a4d3ee", pos="10,6!", shape = box, width=2.4, height=1, label = "Adjudicated Discontinued (Randomized) (n=',ed_randomized,')"];
      
      safety [style="rounded,filled", fillcolor="#a4d3ee", pos="2,6!", shape = box, width=2.4, height=1, label = "Full Safety Set (n=',safety,')\nSafety Set & Not Enrolled (n=',ex_safety,')"];
      
      enrolled [style="rounded,filled", fillcolor="#a4d3ee", pos="6,4!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',enrolled,')"];
      df_complete [style="rounded,filled", fillcolor="#a4d3ee", pos="6,2!", shape = box, width=2.4, height=1, label = "',definitive_event,' (n=',df_complete,')"];

      active [style="rounded,filled", fillcolor="#a4d3ee", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_complete [style="rounded,filled", fillcolor="#a4d3ee", pos="4,0!", shape = box, width=2.4, height=1, label = "Not Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      not_expected [style="rounded,filled", fillcolor="#a4d3ee", pos="8,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#a4d3ee", pos="12,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')"];

      # Relationships
      screened -> eligible
      screened -> ineligible
      eligible -> refused
      eligible -> consented
      consented -> randomized
      consented -> ed_consented
      randomized -> enrolled
      randomized -> ed_randomized
      enrolled -> df_complete
      df_complete -> active
      df_complete -> not_complete
      df_complete -> not_expected
      df_complete -> fu_complete
    }
  '))
  svg_content <- DiagrammeRsvg::export_svg(consort_diagram)
  temp_svg_path <- tempfile(fileext = ".svg")
  writeLines(svg_content, temp_svg_path)
  temp_png_path <- tempfile(fileext = ".png")
  rsvg::rsvg_png(temp_svg_path, temp_png_path, width = 1200, height = 1200)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Consort Diagram" style="max-width: 100%%; width: 1200px;">', image_data)
  file.remove(c(temp_svg_path, temp_png_path))
  return(img_tag)
}



#' Visualization Library: Issues per site (Basic)
#'
#' @description Visualizes the number of open and untouched issues per site,
#' determined by the status column in the query_database being set to "Deteected".
#'
#'
#' @return table of data quality confirmation forms
#' @export
#'
#' @examples
#' \dontrun{
#' vislib_query_issues_per_site_basic()
#' }
vislib_query_issues_per_site_basic <- function(analytic) {
  
  queries <- analytic %>%
    select(analytic_query_database) %>%
    separate_rows(analytic_query_database, sep = 'NEWROW:') %>%
    separate(analytic_query_database, into = c("ID", "facilitycode", "construct", "Message", "ADDRESS", 
                                               "Field", "Value", "updated_value", "status", "detected_date", 
                                               "changed_date", "recent", "modified_date", "confirmed_date", 
                                               "confirmed_modified_date", "closed_date", "warning", "note"),
             sep = 'NEWCOLUMN:') %>%
    filter(!is.na(status) & status != 'NA')
  
  if (nrow(queries)==0){
    return("No Queries in Database.")
  }
  
  queries<- queries %>% 
    mutate(status = recode(status,
                               "Closed" = "Resolved Issue",
                               "Updated Form Value Unchanged" = "Open Issue",
                               "Dashboard Changed" = "Open Issue",
                               "Follow-up Requested" = "Open Issue",
                               "Changed & Confirmed" = "Resolved Issue",
                               "Confirmed" = "Resolved Issue",
                               "Changed" = "Resolved Issue",
                               "Detected" = "Open Issue",
                               "Indicated Data Change" = "Open Issue",
                               "Update Form & Indicated Data Change" = "Open Issue")) %>% 
    group_by(facilitycode) %>% 
    summarise(open = sum(status=="Open Issue"), closed=sum(status=="Resolved Issue")) %>%
    ungroup() %>% 
    arrange(desc(open))
  
  queries_long <- queries %>%
    pivot_longer(cols = c(open, closed), names_to = "status", values_to = "count")
  
  # Update the ggplot to use the long format data
  g <- ggplot(queries_long, aes(x = factor(facilitycode, levels=queries$facilitycode), y = count, fill = status)) +
    geom_bar(stat = "identity", position = "stack", color = 'black', size = 0.5, width = 0.8) +
    scale_fill_manual(values = c("open" = "red", "closed" = "blue3"),
                      labels = c("open" = "Open Issue", "closed" = "Resolved Issue")) +
    labs(title = "Number of Issues by Facility",
         x = "Site",
         y = "Issue Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",  # Center the legend at the top
          legend.title = element_blank())
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 2500, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Enrollment by site" style="max-width: 100%%; width: 80%%;">', image_data)
  
  return(img_tag)
}


#' Visualization Library: Issues per site
#'
#' @description Visualizes the number of open and untouched issues per site,
#' determined by the status column in the query_database being set to "Deteected".
#'
#'
#' @return table of data quality confirmation forms
#' @export
#'
#' @examples
#' \dontrun{
#' vislib_query_issues_per_site()
#' }
vislib_query_issues_per_site <- function(analytic) {
  
  queries_full <- analytic %>%
    select(analytic_query_database) %>%
    separate_rows(analytic_query_database, sep = 'NEWROW:') %>%
    separate(analytic_query_database, into = c("ID", "facilitycode", "construct", "Message", "ADDRESS", 
                                               "Field", "Value", "updated_value", "status", "detected_date", 
                                               "changed_date", "recent", "modified_date", "confirmed_date", 
                                               "confirmed_modified_date", "closed_date", "warning", "note"),
             sep = 'NEWCOLUMN:') %>%
    filter(!is.na(status) & status != 'NA')
  
  weeks <- 2
  
  if (nrow(queries_full)==0){
    return("No Queries in Database.")
  }
  
  queries_full['recent'] <- as.logical(queries_full$recent)
  queries_full['ID'] <- as.character(queries_full$ID)
  
  fixed <- Sys.Date()
  today <- fixed
  indexes <- seq(1, weeks)
  t_cols <- c("Detected","Changed","Confirmed","Changed & Confirmed","Follow-up Requested","Dashboard Changed","Updated Form Value Unchanged","Closed","Indicated Data Change", "Update Form & Indicated Data Change")
  
  for (i in indexes){
    start <- Sys.time()
    queries <- queries_full
    
    queries['closed_fixed'] <- replace_na(as.Date(queries$closed_date, "%m/%d/%Y") <= fixed,FALSE)
    queries['changed_fixed'] <- replace_na(as.Date(queries$changed_date, "%m/%d/%Y") <= fixed,FALSE)
    queries['confirmed_fixed'] <- replace_na(as.Date(queries$confirmed_date, "%m/%d/%Y") <= fixed,FALSE)
    queries['detected_fixed'] <- replace_na(as.Date(queries$detected_date, "%m/%d/%Y") <= fixed,FALSE)
    
    queries['closed_dated'] <- as.Date(ifelse(queries$closed_fixed, queries$closed_date, NA), "%m/%d/%Y")
    queries['changed_dated'] <- as.Date(ifelse(queries$changed_fixed, queries$changed_date, NA), "%m/%d/%Y")
    queries['confirmed_dated'] <- as.Date(ifelse(queries$confirmed_fixed, queries$confirmed_date, NA), "%m/%d/%Y")
    queries['detected_dated'] <- as.Date(ifelse(queries$detected_fixed, queries$detected_date, NA), "%m/%d/%Y")
    
    queries <- queries %>% rowwise() %>%
      mutate(max_date= max(na.omit(c(detected_dated, changed_dated, confirmed_dated, changed_dated, closed_dated))))
    
    changed_confirmed <- c("Changed", "Confirmed")
    
    queries <- queries %>%
      mutate(status= NA) %>%
      mutate(status= ifelse(replace_na(detected_dated==max_date,FALSE), "Detected", status)) %>%
      mutate(status= ifelse(replace_na(changed_dated==max_date,FALSE), "Changed", status)) %>%
      mutate(status= ifelse(replace_na(confirmed_dated==max_date,FALSE), "Confirmed", status)) %>%
      mutate(status= ifelse(is.na(status)==FALSE & is.na(confirmed_dated)==FALSE & is.na(changed_dated)==FALSE & status %in% changed_confirmed,"Changed & Confirmed", status)) %>%
      mutate(status= ifelse(replace_na(closed_dated==max_date,FALSE), "Closed", status))
    
    queries <- queries %>%
      mutate(recent= replace_na(recent,FALSE)) %>%
      mutate(warning= ifelse(is.na(changed_date),NA, ifelse(as.Date(changed_date, "%m/%d/%Y") > as.Date(detected_date, "%m/%d/%Y"), ifelse(recent==TRUE, "WARNING: None Modified after Changed", NA), "WARNING: Detected after Changed"))) %>%
      select(colnames(queries))
    
    queries_count <- queries %>% group_by(facilitycode) %>% count(status)
    
    processed_data <- tibble('facilitycode'=unique(queries$facilitycode))
    
    for(new_col in t_cols){
      counts <- queries_count %>% filter(status==new_col)
      processed_data <- processed_data %>% rowwise() %>% mutate(!!new_col := ifelse(length(counts[counts$facilitycode==facilitycode,]$n)==0,0,counts[counts$facilitycode==facilitycode,]$n))
    }
    processed_data <- processed_data %>% rename(Site=facilitycode) %>% mutate(Site = paste(Site,format(fixed,"%b-%d"),sep=", "))
    
    if (i==1){
      processed_data_full <- processed_data
    } else{
      processed_data_full <- rbind(processed_data, processed_data_full)
    }
    fixed <- fixed - 7
  }
  
  
  
  fixed_names <- list("Closed"="MCC Closed Issue:\nClosed", "Updated Form Value Unchanged"="Open Issue:\nUpdated Form\nValue Unchanged", "Dashboard Changed"="Open Issue:\nDashboard Changed",
                      "Follow-up Requested"="Open Issue:\nFollow-up Requested", "Changed & Confirmed"="Site Addressed Issue:\nChanged & Confirmed", "Confirmed"="Site Addressed Issue:\nConfirmed",
                      "Changed"="Site Addressed Issue:\nChanged", "Detected"="Open Issue:\nUntouched", "Indicated Data Change"="Open Issue:\nIndicated Data Change",
                      "Update Form & Indicated Data Change"="Open Issue:\nUpdate Form &\nIndicated Data Change")
  
  sort_sites <- TRUE
  
  if(sort_sites==TRUE){
    sites_ordered <- processed_data_full %>% ungroup() %>% arrange(desc(Detected), Closed, Changed)
    sites_sorted <- str_sub(sites_ordered[str_detect(sites_ordered$Site,format(today,"%b-%d")),]$Site,end=3)
    dates_suffix <- rep(c(format(today-7,"%b-%d"),format(today,"%b-%d")),length(sites_sorted))
    site_order <- paste(rep(sites_sorted,each=2),dates_suffix,sep=", ")
    processed_data_full <- processed_data_full[match(site_order, processed_data_full$Site),]
  }
  
  p <- plotly::plot_ly(data = processed_data_full, x=~Site, y=~Detected, type = 'bar', name = unname(unlist(fixed_names['Detected']))) %>%
    add_trace(y = ~Changed, name = unname(unlist(fixed_names['Changed'])), marker = list(color = '#ff9933')) %>%
    add_trace(y = ~Confirmed, name = unname(unlist(fixed_names['Confirmed'])), marker = list(color = '#669933')) %>%
    add_trace(y = ~`Changed & Confirmed`, name = unname(unlist(fixed_names['Changed & Confirmed'])), marker = list(color = '#cc3333')) %>%
    add_trace(y = ~`Follow-up Requested`, name = unname(unlist(fixed_names['Follow-up Requested'])), marker = list(color = '#9966cc')) %>%
    add_trace(y = ~`Dashboard Changed`, name = unname(unlist(fixed_names['Dashboard Changed'])), marker = list(color = '#996666')) %>%
    add_trace(y = ~`Updated Form Value Unchanged`, name = unname(unlist(fixed_names['Updated Form Value Unchanged'])), marker = list(color = '#cc66cc')) %>%
    add_trace(y = ~`Indicated Data Change`, name = unname(unlist(fixed_names['Indicated Data Change'])), marker = list(color = '#cc66a0')) %>%
    add_trace(y = ~Closed, name = unname(unlist(fixed_names['Closed'])), marker = list(color = '#666666')) %>%
    add_trace(y = ~`Update Form & Indicated Data Change`, name = unname(unlist(fixed_names['Update Form & Indicated Data Change'])), marker = list(color = '#f0e690')) %>%
    plotly::layout(xaxis = list(title = "",categoryorder = "array", categoryarray = ~Site), yaxis = list(title = 'Count'), barmode = 'stack')
  
  html_page <- export_plotly(p)
  
  if(nrow(processed_data_full)==0){
    processed_data_full<-NA
  }
  
  return(html_page)
  
}

