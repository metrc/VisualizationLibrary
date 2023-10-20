
#' DSMB Consort Diagram
#'
#' @description This function visualizes the categorical percentages of baseline characteristics sex, age, race, education, and military
#'
#' @param analytic This is the analytic data set that must include screened, eligible, 
#' consented, randomized, enrolled, active, enrolled_discontinuation, refused, late_ineligible, and the meta construct column
#' @param not_enrolled_other is a meta construct that is NULL by default
#' @param completed_str is the text for the completed box that defaults to 'Completed 12-month visit'
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' dsmb_consort_diagram()
#' }
dsmb_consort_diagram <- function(analytic, not_enrolled_other=NULL, completed_str="Completed 12-month visit"){
  analytic <- analytic %>% 
    filter(screened == TRUE) 
  
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
                           pull(late_ineligible), na.rm=TRUE)
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
                  pull(active), na.rm=TRUE)
  Discontinued <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>%
                        filter(enrolled) %>% 
                        pull(enrolled_discontinuation), na.rm=TRUE)
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
      late_inelig [style="rounded,filled", fillcolor="#ccccff", pos="10,6!", shape = box, width=2.4, height=1, label = "Late Ineligible (n=',Late_Ineligible,')"];
      
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
  rsvg::rsvg_png(temp_svg_path, temp_png_path, width = 2000, height = 2000)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Consort Diagram" style="max-width: 100%%; width: 80%%;">', image_data)
  file.remove(c(temp_svg_path, temp_png_path))
  return(img_tag)
}


#' DSMB NSAID Consort Diagram
#'
#' @description This function visualizes the categorical percentages of Study Status
#' for the NSAID study
#'
#' @param analytic This is the analytic data set that must include screened, eligible, 
#' consented, randomized, enrolled, active, enrolled_discontinuation, refused, late_ineligible, and the meta construct column
#' @param not_enrolled_other is a meta construct that is NULL by default
#' @param completed_str is the text for the completed box that defaults to 'Completed 12-month visit'
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' dsmb_nsaid_consort_diagram()
#' }
dsmb_nsaid_consort_diagram <- function(analytic, not_enrolled_other=NULL){
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
  if(is.null(not_enrolled_other)){
    Not_Enrolled_Other <- Eligible - Consented - Refused
  } else{
    temp <- analytic %>% 
      filter(eligible)
    Not_Enrolled_Other <- sum(temp[[not_enrolled_other]], na.rm=TRUE)
  }

  
  Randomized <- sum(analytic %>% 
                      filter(eligible) %>% 
                      filter(consented) %>% 
                      pull(randomized), na.rm=TRUE)
  
  Enrolled <- sum(analytic %>% 
                    filter(eligible) %>% 
                    filter(consented) %>% 
                    filter(randomized) %>% 
                    pull(enrolled), na.rm=TRUE)
  Early_Discontinuation <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>%
                        filter(enrolled) %>% 
                        pull(enrolled_discontinuation), na.rm=TRUE)
  Definitive_Fixation_Complete <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>%
                        filter(enrolled) %>% 
                        pull(dfsurg_completed), na.rm=TRUE)
  

  
  NSAID_consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="6,12!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#ccccff", pos="6,10!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Not Enrolled\nRefused (n=',Refused,')\nNot Enrolled: Other (n=',Not_Enrolled_Other,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="6,6!", shape = box, width=2.4, height=1, label = "Consented and Randomized (n=',Randomized,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="6,4!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',Enrolled,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="10,4!", shape = box, width=2.4, height=1, label = "Adjudicated Discontinued (n=',Early_Discontinuation,')"];

      compl [style="rounded,filled", fillcolor="#ccccff", pos="10,1!", shape = box, width=2.4, height=1, label = "Definitive Fixation Complete (n=',Definitive_Fixation_Complete,')"];
      
      
      # Relationships
      screened -> eligible
      screened -> ineligible
      eligible -> rand
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
  rsvg::rsvg_png(temp_svg_path, temp_png_path, width = 2000, height = 2000)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Consort Diagram" style="max-width: 100%%; width: 80%%;">', image_data)
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
    labs(title = "Cumulative Percentage Enrollment for Ankles fracture type (of 526)", x = "Month", y = "Cumulative Percent") +
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
    labs(title = "Cumulative Percentage Enrollment for Ankles fracture type (of 526)", x = "Month", y = "Cumulative Percent") +
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