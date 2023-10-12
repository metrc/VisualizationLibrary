
#' DSMB Consort Diagram
#'
#' @description This function visualizes the categorical percentages of baseline characteristics sex, age, race, education, and military
#'
#' @param analytic This is the analytic data set that must include screened, eligible, 
#' consented, randomized, discontinued, refused, late_ineligible, and the meta construct columns
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
  Late_Ineligible <- sum(analytic %>% 
                           filter(eligible) %>% 
                           filter(consented) %>% 
                           pull(late_ineligible), na.rm=TRUE)
  Randomized <- sum(analytic %>% 
                      filter(eligible) %>% 
                      filter(consented) %>% 
                      pull(randomized), na.rm=TRUE)
  Active <- sum(analytic %>% 
                  filter(eligible) %>% 
                  filter(consented) %>% 
                  filter(randomized) %>% 
                  pull(active), na.rm=TRUE)
  Discontinued <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>% 
                        pull(discontinued), na.rm=TRUE)
  Completed <- sum(analytic %>% 
                        filter(eligible) %>% 
                        filter(consented) %>% 
                        filter(randomized) %>% 
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
      rand [style="rounded,filled", fillcolor="#ccccff", pos="6,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      
      active [style="rounded,filled", fillcolor="#ccccff", pos="2,2!", shape = box, width=2.4, height=1, label = "Active (n=',Active,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="6,2!", shape = box, width=2.4, height=1, label = "Discontinued (n=',Discontinued,')"];
      compl [style="rounded,filled", fillcolor="#ccccff", pos="10,2!", shape = box, width=2.4, height=1, label = "',completed_str,' (n=',Completed,')"];
      
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Refused (n=',Refused,')"];
      late_inelig [style="rounded,filled", fillcolor="#ccccff", pos="10,8!", shape = box, width=2.4, height=1, label = "Late Ineligible (n=',Late_Ineligible,')"];
      
      not_enrolled [style="rounded,filled", fillcolor="#ccccff", pos="2,10!", shape = box, width=2.4, height=1, label = "Not Enrolled Other (n=',Not_Enrolled_Other,')"];
      
      # Relationships
      start -> elig
      start -> ineligible
      elig -> cons
      elig -> refused
      elig -> not_enrolled
      cons -> rand
      cons -> late_inelig
      rand -> active
      rand -> discon
      rand -> compl
      
    }
  '))
  svg_content <- DiagrammeRsvg::export_svg(consort_diagram)
  temp_svg_path <- tempfile(fileext = ".svg")
  writeLines(svg_content, temp_svg_path)
  temp_png_path <- tempfile(fileext = ".png")
  rsvg::rsvg_png(temp_svg_path, temp_png_path, width = 1000, height = 1000)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Consort Diagram">', image_data)
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
  
  crf_dt <- get_data(c('study_id', 'crf02_dt'))
  
  df <- analytic %>%  select(study_id, injury_type, enrolled) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(injury_type)) %>% 
    left_join(crf_dt, by = 'study_id') %>% 
    filter(!is.na(crf02_dt)) %>% 
    filter(injury_type == "ankle")
  
  df$crf02_dt <- ymd(df$crf02_dt)
  
  yyyy_mm <- df %>% mutate(
    year = year(crf02_dt),
    month = month(crf02_dt),
    year_month = format(crf02_dt, "%Y-%m")
  ) %>% 
    group_by(year_month) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    arrange(year_month) %>%
    mutate(
      cumulative_value = cumsum(Total),
      cumulative_percentage = (cumulative_value / 526) * 100)
  
  
  g <- ggplot(yyyy_mm, aes(x = factor(year_month), y = cumulative_percentage / 100)) +
    geom_bar(stat = "identity", fill = "yellow", color = "black", size = 0.3) +
    labs(title = "Cumulative Percentage Enrollment for Ankles fracture type(of 526)", x = "Month", y = "Cumulative Percent") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +  # Rotate labels vertically
    scale_y_continuous(labels = scales::percent_format(scale = 1))
  
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 1000, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Percentage Enrollment for Ankle injury">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}


#' Cumulative percentage for Tibial Plateau injuries
#'
#' @description This function visualizes the Cumulative percentage for number of patients with Plateau injuries 
#' over the period of Year-Months(YYYY-MM, crf02_dt) out of 526
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
  
  crf_dt <- get_data(c('study_id', 'crf02_dt'))
  
  df <- analytic %>%  select(study_id, injury_type, enrolled) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(injury_type)) %>% 
    left_join(crf_dt, by = 'study_id') %>% 
    filter(!is.na(crf02_dt)) %>% 
    filter(injury_type == "plateau")
  
  
  
  df$crf02_dt <- ymd(df$crf02_dt)
  
  yyyy_mm <- df %>% mutate(
    year = year(crf02_dt),
    month = month(crf02_dt),
    year_month = format(crf02_dt, "%Y-%m")
  ) %>% 
    group_by(year_month) %>%
    summarise(Total = n()) %>%
    ungroup() %>% 
    arrange(year_month) %>%
    mutate(
      cumulative_value = cumsum(Total),
      cumulative_percentage = (cumulative_value / 100) * 100)
  
  
  g <- ggplot(yyyy_mm, aes(x = factor(year_month), y = cumulative_percentage / 100)) +
    geom_bar(stat = "identity", fill = "blue", color = "black", size = 0.3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, vjust = 1)) +
    scale_y_continuous(labels = scales::percent_format(scale = 1) +
                         scale_y_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)))
                       
   temp_png_path <- tempfile(fileext = ".png")
   ggsave(temp_png_path, plot = g, width = 1000, height = 1000, units = 'px')
   image_data <- base64enc::base64encode(temp_png_path)
   img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Cumulative Percentage Enrollment for Plateau injury">', image_data)
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
  
  crf_dt <- get_data(c('study_id', 'crf02_dt'))
  
  df <- analytic %>%  select(study_id, injury_type, enrolled, facilitycode) %>% 
    filter(enrolled = TRUE) %>% 
    filter(!is.na(injury_type)) %>% 
    left_join(crf_dt, by = 'study_id') %>% 
    filter(!is.na(crf02_dt)) %>% 
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
  ggsave(temp_png_path, plot = g, width = 1000, height = 1000, units = 'px')
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Enrollment by each injury type and site">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}