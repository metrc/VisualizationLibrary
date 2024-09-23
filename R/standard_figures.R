
#' DSMB Consort Diagram
#'
#' @description This function visualizes the categorical percentages of Study Status for any study, similar to the NSAID consort diagram, but with customization endpoints.
#'
#' @param analytic The analytic data set that must include the following columns: screened, eligible, consented, refused, discontinued_pre_randomization, randomized, late_ineligible, enrolled, patient_status_active, censored, and completed.
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
dsmb_consort_diagram <- function(analytic, not_enrolled_other=NULL, completed_str="Completed 12-month visit", late_inelgible="late_inelgible", late_inelgible_str="Late Inelgible"){
  analytic <- analytic %>% 
    filter(screened == TRUE) 
  late_inelgible_var <- late_inelgible
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
  Active <- sum(analytic %>% 
                  filter(eligible) %>% 
                  filter(consented) %>% 
                  filter(randomized) %>% 
                  filter(enrolled) %>% 
                  pull(patient_status_active), na.rm=TRUE)
  Discontinued <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>%
                        filter(enrolled) %>% 
                        pull(censored), na.rm=TRUE)
  Completed <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized)  %>% 
                        filter(enrolled) %>% 
                        pull(completed), na.rm=TRUE)
  Ineligible <- Screened - Eligible
  if(is.null(not_enrolled_other)){
    Not_Enrolled_Other <- Eligible - Consented - Refused
  } else{
    temp <- analytic %>% 
      filter(eligible)
    Not_Enrolled_Other <- sum(temp[[not_enrolled_other]], na.rm=TRUE)
  }
  
  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      start [style="rounded,filled", fillcolor="#ccccff", pos="6,12!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      elig [style="rounded,filled", fillcolor="#ccccff", pos="6,10!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      cons [style="rounded,filled", fillcolor="#ccccff", pos="6,8!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];
      pre_rand [style="rounded,filled", fillcolor="#ccccff", pos="10,8!", shape = box, width=2.4, height=1, label = "Discontinued\nPre-Randomization\n(n=',Disconintued_Pre,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="6,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      late_inelig [style="rounded,filled", fillcolor="#ccccff", pos="10,6!", shape = box, width=2.4, height=1, label = "',late_inelgible_str,', (n=',Late_Ineligible,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="6,4!", shape = box, width=2.4, height=1, label = "Enrolled (n=',Enrolled,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="6,1!", shape = box, width=2.4, height=1, label = "Discontinued (n=',Discontinued,')"];

      active [style="rounded,filled", fillcolor="#ccccff", pos="2,1!", shape = box, width=2.4, height=1, label = "Active (n=',Active,')"];
      compl [style="rounded,filled", fillcolor="#ccccff", pos="10,1!", shape = box, width=2.4, height=1, label = "',completed_str,' (n=',Completed,')"];
      
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Refused (n=',Refused,')"];
      
      not_enrolled [style="rounded,filled", fillcolor="#ccccff", pos="2,10!", shape = box, width=2.4, height=1, label = "Not Enrolled Other (n=',Not_Enrolled_Other,')"];
      
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
      enrolled -> discon
      enrolled -> compl
      
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
#' consented, not_consented, randomized, enrolled, censored, refused, df_surg_completed, and the meta construct column
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' dsmb_nsaid_consort_diagram()
#' }
dsmb_nsaid_consort_diagram <- function(analytic){
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

      compl [style="rounded,filled", fillcolor="#ccccff", pos="6,1!", shape = box, width=2.4, height=1, label = "Definitive Fixation Complete (n=',Definitive_Fixation_Complete,')"];
      
      
      # Relationships
      screened -> eligible
      screened -> ineligible
      eligible -> cons
      cons -> rand
      rand -> discon
      eligible -> refused
      rand -> enrolled
      enrolled -> compl
      
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
enrollment_by_site <- function(analytic){
  
  df <- analytic %>%  select(study_id, enrolled, facilitycode, consent_date) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(consent_date)) %>% 
    group_by(facilitycode) %>%
    summarise(EnrolledPatients = n()) 
  
  g <- ggplot(df, aes(x = facilitycode, y = EnrolledPatients)) +
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
cumulative_enrolled <- function(analytic){
  
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
    geom_line(aes(x = factor(year_month), y = cumulative_value), data = yyyy_mm, stat = "identity", group = 1) +  # Add the 'data' argument
    labs(title = "Cumulative Enrollment with Discrete Enrollment by Month", x = "Month", y = "Enrolled") +
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
#' refused, consented, randomized, enrolled, time_zero, adjudicated_early_discontinued, followup_data, 
#' safety_set
#'  
#' @param definitive_event Event either DF or DWC
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' consort_diagram()
#' }
consort_diagram <- function(analytic, definitive_event = "Definitive Fixation Complete"){
  
  df <- analytic %>% 
    select(study_id, screened, ineligible, eligible, refused, consented, randomized, enrolled, time_zero, 
           adjudicated_early_discontinued, followup_data, safety_set) %>% 
    mutate(time_zero = ifelse(!is.na(time_zero), TRUE, FALSE))
  
  screened <- sum(analytic$screened, na.rm = TRUE)
  
  eligible_df <- df %>% 
    filter(screened)
  
  safety <- sum(eligible_df$safety_set, na.rm = TRUE)
  
  eligible <- sum(eligible_df$eligible, na.rm = TRUE)
  
  ineligible <- screened - eligible
  
  refused_df <- eligible_df %>% 
    filter(eligible)
  
  refused <- sum(refused_df$refused, na.rm = TRUE)
  
  consented_df <- refused_df %>% 
    filter(refused == FALSE | is.na(refused))
  
  consented <- sum(consented_df$consented, na.rm = TRUE)
  not_consented <- eligible - (consented + refused)
  
  randomized_df <- consented_df %>% 
    filter(consented)
  
  randomized <- sum(randomized_df$randomized, na.rm = TRUE)
  
  ed_df <- randomized_df %>% 
    filter(randomized)
  
  early_discontinuation <- sum(ed_df$adjudicated_early_discontinued, na.rm = TRUE)
  
  enrolled_df <- ed_df %>% 
    filter(adjudicated_early_discontinued == FALSE | is.na(adjudicated_early_discontinued))
  
  enrolled <- sum(enrolled_df$enrolled, na.rm = TRUE)
  df_complete <- sum(enrolled_df$time_zero, na.rm = TRUE)
  
  ed_consented <- consented - randomized
  ed_randomized <- randomized - enrolled
  
  fu_df <- enrolled_df %>% 
    filter(time_zero)
  
  
  fu_complete_12mo_df <- fu_df %>% 
    select(study_id, followup_data) %>% 
    separate_rows(followup_data, sep=";") %>% 
    separate(followup_data, c('redcap_event_name', 'followup_period', 'form', 'status', 'form_dates'), sep=",") %>% 
    mutate_all(na_if, 'NA') %>% 
    filter(followup_period == '12 Month' & form == 'CFU') %>% 
    mutate(followup_complete_12mo = ifelse(status %in% c('complete', 'complete: early', 'complete: late'), TRUE, FALSE)) 
  
  fu_complete_12mo <- sum(fu_complete_12mo_df$followup_complete_12mo, na.rm = TRUE)
  
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
      
      enrolled [style="rounded,filled", fillcolor="#a4d3ee", pos="6,4!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',enrolled,')"];
      df_complete [style="rounded,filled", fillcolor="#a4d3ee", pos="6,2!", shape = box, width=2.4, height=1, label = "',definitive_event,' (n=',df_complete,')"];

      fu_complete [style="rounded,filled", fillcolor="#a4d3ee", pos="6,0!", shape = box, width=2.4, height=1, label = "12 Month Follow-Up Complete (n=',fu_complete_12mo,')"];
      
      safety [style="rounded,filled", fillcolor="#a4d3ee", pos="10,2!", shape = box, width=2.4, height=1, label = "Safety Set (n=',safety,')"];
      
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
      df_complete -> fu_complete
      df_complete -> safety
      
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