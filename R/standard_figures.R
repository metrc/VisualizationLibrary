#' DSMB Consort Diagram
#'
#' @description This function visualizes the categorical percentages of Study Status for any study, similar to the NSAID consort diagram, but with customization endpoints.
#' 
#' For other consort diagrams that may better fit your study, refer to: consort_diagram, consort_diagram_no_definitive_event, 
#' dsmb_consort_diagram, dsmb_consort_diagram_pre_no_def, dsmb_consort_diagram_pre_no_def_shifted_consent, 
#' dsmb_consort_diagram_pre_shifted_consent, dsmb_nsaid_consort_diagram. 
#'
#' @param analytic analytic data set that must include the following columns: screened, eligible, 
#' consented, refused, discontinued_pre_randomization, randomized, late_ineligible, enrolled, completed, 
#' not_completed, not_expected, active, missed_final_followup, incomplete_final_followup
#' @param not_enrolled_other a column in the dataset for cases that are eligible but not enrolled for 
#' reasons other than refusal (optional, if not given is the number of eligible minus consented minus
#' refused).
#' @param final_period string specifying the label for the completion status box. Defaults to "12-month".
#' @param late_inelgible construct used for the count in the late ineligible box
#' @param late_inelgible_str labels the not expected box as adjudicated
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' dsmb_consort_diagram("Replace with Analytic Tibble")
#' dsmb_consort_diagram("Replace with Analytic Tibble", late_ineligible = 'test', late_ineligible_str = "Test Column", 
#'   not_expected_adjudicated = TRUE)
#' 
dsmb_consort_diagram <- function(analytic, not_enrolled_other=NULL, final_period = '12 Month', late_ineligible="late_ineligible", 
                                 late_ineligible_str="Late Ineligible", not_expected_adjudicated=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c('screened', 'eligible', 'consented', 'refused', 'discontinued_pre_randomization', 
                           'randomized', 'late_ineligible',
                           'enrolled', 'completed', 'not_completed', 'not_expected', 
                           'active', 'missed_final_followup', 'incomplete_final_followup', 'test'),
    example_types = c('Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 
                      'Boolean', 'Boolean',
                      'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean'))

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
  incomplete <- sum(en_df$incomplete_final_followup, na.rm = TRUE)
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
      
      start [style="rounded,filled", fillcolor="#ccccff", pos="5,12!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      elig [style="rounded,filled", fillcolor="#ccccff", pos="5,10!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      cons [style="rounded,filled", fillcolor="#ccccff", pos="5,8!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];
      pre_rand [style="rounded,filled", fillcolor="#ccccff", pos="9,8!", shape = box, width=2.4, height=1, label = "Discontinued (n=',Disconintued_Pre,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="5,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      late_inelig [style="rounded,filled", fillcolor="#ccccff", pos="9,6!", shape = box, width=2.4, height=1, label = "',late_ineligible_str,', (n=',Late_Ineligible,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="5,4!", shape = box, width=2.4, height=1, label = "Enrolled (n=',Enrolled,')"];
      
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="9,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      refused [style="rounded,filled", fillcolor="#ccccff", pos="9,10!", shape = box, width=2.4, height=1, label = "Refused (n=',Refused,')"];
      
      not_enrolled [style="rounded,filled", fillcolor="#ccccff", pos="1,10!", shape = box, width=2.4, height=1, label = "Not Enrolled Other (n=',Not_Enrolled_Other,')"];

      active [style="rounded,filled", fillcolor="#ccccff", pos="1,2!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="5,2!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="9,2!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      
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
#' For other consort diagrams that may better fit your study, refer to: consort_diagram, consort_diagram_no_definitive_event, 
#' dsmb_consort_diagram, dsmb_consort_diagram_pre_no_def, dsmb_consort_diagram_pre_no_def_shifted_consent, 
#' dsmb_consort_diagram_pre_shifted_consent, dsmb_nsaid_consort_diagram. 
#'
#' @param analytic This is the analytic data set that must include screened, eligible, 
#' consented, not_consented, randomized, enrolled, refused, df_surg_completed, completed, 
#' not_completed, not_expected, active, missed_final_followup, incomplete_final_followup,
#' adjudicated_discontinued
#' @param final_period Defaults to 12 Month
#' @param definitive_event Event either DF or DWC
#' @param not_expected_adjudicated whether to note that the Not Expected was adjudicated
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' dsmb_nsaid_consort_diagram("Replace with Analytic Tibble")
#' 
dsmb_nsaid_consort_diagram <- function(analytic, final_period="12 Month", not_expected_adjudicated=TRUE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "eligible", "adjudicated_discontinued",
                           "consented", "not_consented", "randomized", "enrolled", "refused", 
                           "df_surg_completed", "completed", 
                           "not_completed", "not_expected", "active", "missed_final_followup", 
                           "incomplete_final_followup"),
    example_types = c("Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean"))
  
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
  incomplete <- sum(fu_df$incomplete_final_followup, na.rm = TRUE)
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
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="5,12!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,10!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Not Consented (n=',Not_Consented,')\nRefused (n=',Refused,')"];

      cons [style="rounded,filled", fillcolor="#ccccff", pos="5,8!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="5,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="5,4!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',Enrolled,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="10,6!", shape = box, width=2.4, height=1, label = "Adjudicated Discontinued (n=',Adjudicated_Discontinuation,')"];

      compl [style="rounded,filled", fillcolor="#ccccff", pos="5,2!", shape = box, width=2.4, height=1, label = "Definitive Fixation Complete (n=',Definitive_Fixation_Complete,')"];
      
      active [style="rounded,filled", fillcolor="#ccccff", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="5,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="10,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      
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


#' DSMB consort diagram with pre-screening and no definitive event
#'
#' @description 
#' Visualizes the counts of different study statuses, including prescreening statuses. The diagram works
#' from the final_followup constructs rather than any df ones.
#' 
#' See dsmb_consort_diagram and consort_diagram for similar options.
#'
#' @param analytic This is the analytic data set that must include pre_screened, pre_eligible, pre_ineligible, 
#' screened, eligible, ineligible, consented, not_consented, randomized, enrolled, refused, completed, 
#' not_completed, not_expected, active, missed_final_followup, incomplete_final_followup
#' @param final_period label of the final period
#' @param adjudicated visual option to say that discontinuation was adjudicated, defaults to false
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' dsmb_consort_diagram_pre_no_def("Replace with Analytic Tibble")
#' dsmb_consort_diagram_pre_no_def("Replace with Analytic Tibble", final_period = '3 Month', adjudicated = TRUE)
#' 
dsmb_consort_diagram_pre_no_def <- function(analytic, final_period="12 Month", adjudicated=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "ineligible", "eligible", "refused", "consented", 
                           "randomized", "enrolled", "adjudicated_discontinued", "not_consented",
                           "completed", "safety_set", "exclusive_safety_set", "not_completed", 
                           "not_expected", "active", "missed_final_followup", "incomplete_final_followup", 
                           "pre_screened", "pre_eligible", "pre_ineligible",
                           "discontinued"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean"))
  
  pre_analytic <- analytic
  
  pre_Screened <- sum(pre_analytic$pre_screened, na.rm=TRUE)
  pre_Eligible <- sum(pre_analytic$pre_eligible, na.rm=TRUE)
  pre_Ineligible <- sum(pre_analytic$pre_ineligible, na.rm=TRUE)
  
  analytic <- analytic %>% 
    filter(screened == TRUE) 
  
  Screened <- sum(analytic$screened, na.rm=TRUE)
  Eligible <- sum(analytic$eligible, na.rm=TRUE)
  Ineligible <- sum(analytic$ineligible, na.rm=TRUE)
  
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
  if(adjudicated){
    Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(randomized) %>%
                             pull(adjudicated_discontinued), na.rm=TRUE)
  } else{
    Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(randomized) %>%
                             pull(discontinued), na.rm=TRUE)
  }
  
  
  fu_df <- analytic %>% 
    filter(eligible) %>% 
    filter(consented) %>% 
    filter(randomized) %>%
    filter(enrolled)
  
  complete <- sum(fu_df$completed, na.rm = TRUE)
  not_complete <- sum(fu_df$not_completed, na.rm = TRUE)
  missed <- sum(fu_df$missed_final_followup, na.rm = TRUE)
  incomplete <- sum(fu_df$incomplete_final_followup, na.rm = TRUE)
  active <- sum(fu_df$active, na.rm = TRUE)
  not_expected <- sum(fu_df$not_expected, na.rm = TRUE)
  if(adjudicated){
    not_expected_str= "Adjudicated Not Expected"
  } else{
    not_expected_str= "Not Expected"
  }
  if(adjudicated){
    disc_str= "Adjudicated Discontinued"
  } else{
    disc_str= "Discontinued"
  }
  
  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      pre_screened [style="rounded,filled", fillcolor="#ccccff", pos="5,14!", shape = box, width=2.4, height=1, label = "Pre-Screened (n=',pre_Screened,')"];
      pre_ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,14!", shape = box, width=2.4, height=1, label = "Pre-Ineligible (n=',pre_Ineligible,')"];
      pre_eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,12!", shape = box, width=2.4, height=1, label = "Pre-Eligible (n=',pre_Eligible,')"];
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="5,10!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,8!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,8!", shape = box, width=2.4, height=1, label = "Not Consented (n=',Not_Consented,')\nRefused (n=',Refused,')"];

      cons [style="rounded,filled", fillcolor="#ccccff", pos="5,6!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="5,4!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="5,2!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',Enrolled,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="10,4!", shape = box, width=2.4, height=1, label = "',disc_str,' (n=',Discontinuation,')"];

      active [style="rounded,filled", fillcolor="#ccccff", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="5,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="10,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      
      # Relationships
      pre_screened -> pre_eligible
      pre_screened -> pre_ineligible
      pre_eligible -> screened
      screened -> eligible
      screened -> ineligible
      eligible -> cons
      cons -> rand
      rand -> discon
      eligible -> refused
      rand -> enrolled
      enrolled -> active
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


#' DSMB Consort Diagram With Pre Screened and No Definitive Event
#'
#' @description This function visualizes the categorical percentages of Study Status
#' 
#' For other consort diagrams that may better fit your study, refer to: consort_diagram, consort_diagram_no_definitive_event, 
#' dsmb_consort_diagram, dsmb_consort_diagram_pre_no_def, dsmb_consort_diagram_pre_no_def_shifted_consent, 
#' dsmb_consort_diagram_pre_shifted_consent, dsmb_nsaid_consort_diagram. 
#'
#' @param analytic analytic data set that must include pre_screened, pre_eligible, screened, eligible,
#' consented, not_consented, randomized, enrolled, refused, completed, not_completed, not_expected, active, missed_final_followup, incomplete_final_followup
#' @param final_period labels the final follow-up period box, defaults to "12 Month"
#' @param adjudicated whether to use construct adjudicated_discontinued instead of discontinued and 
#' labels it as such
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' dsmb_consort_diagram_pre_no_def_shifted_consent("Replace with Analytic Tibble")
#' 
dsmb_consort_diagram_pre_no_def_shifted_consent <- function(analytic, final_period="12 Month", adjudicated=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "ineligible", "eligible", "refused", "consented", 
                           "randomized", "enrolled", "adjudicated_discontinued", "not_consented",
                           "completed", "safety_set", "exclusive_safety_set", "not_completed", 
                           "not_expected", "active", "missed_final_followup", "incomplete_final_followup", 
                           "time_zero", "pre_screened", "pre_eligible", "pre_ineligible",
                           "discontinued"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Boolean", "Date", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean"))
  
  pre_analytic <- analytic %>% 
    filter(pre_screened == TRUE)
  
  pre_Screened <- sum(pre_analytic$pre_screened, na.rm=TRUE)
  pre_Eligible <- sum(pre_analytic$pre_eligible, na.rm=TRUE)
  pre_Ineligible <- pre_Screened - pre_Eligible
  
  Consented <- sum(pre_analytic %>% 
                     filter(pre_eligible) %>% 
                     pull(consented), na.rm=TRUE)
  
  Not_Consented <- sum(pre_analytic %>% 
                         filter(pre_eligible) %>% 
                         pull(not_consented), na.rm=TRUE)
  
  Refused <- sum(pre_analytic %>% 
                   filter(eligible) %>% 
                   pull(refused), na.rm=TRUE)
  
  analytic <- analytic %>% 
    filter(screened == TRUE & consented==TRUE) 
  
  Screened <- sum(analytic$screened, na.rm=TRUE)
  Eligible <- sum(analytic$eligible, na.rm=TRUE)
  Ineligible <- Screened - Eligible
  
  Randomized <- sum(analytic %>% 
                      filter(eligible) %>% 
                      filter(consented) %>% 
                      pull(randomized), na.rm=TRUE)
  
  Enrolled <- sum(analytic %>% 
                    filter(eligible) %>% 
                    filter(consented) %>% 
                    filter(randomized) %>% 
                    pull(enrolled), na.rm=TRUE)
  if(adjudicated){
    Discontinuation <- sum(analytic %>% 
                                         filter(eligible) %>% 
                                         filter(consented) %>% 
                                         filter(randomized) %>%
                                         pull(adjudicated_discontinued), na.rm=TRUE)
  } else{
    Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(randomized) %>%
                             pull(discontinued), na.rm=TRUE)
  }

  
  fu_df <- analytic %>% 
    filter(eligible) %>% 
    filter(consented) %>% 
    filter(randomized) %>%
    filter(enrolled)
  
  complete <- sum(fu_df$completed, na.rm = TRUE)
  not_complete <- sum(fu_df$not_completed, na.rm = TRUE)
  missed <- sum(fu_df$missed_final_followup, na.rm = TRUE)
  incomplete <- sum(fu_df$incomplete_final_followup, na.rm = TRUE)
  active <- sum(fu_df$active, na.rm = TRUE)
  not_expected <- sum(fu_df$not_expected, na.rm = TRUE)
  if(adjudicated){
    not_expected_str= "Adjudicated Not Expected"
  } else{
    not_expected_str= "Not Expected"
  }
  if(adjudicated){
    disc_str= "Adjudicated Discontinued"
  } else{
    disc_str= "Discontinued"
  }
  
  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      pre_screened [style="rounded,filled", fillcolor="#ccccff", pos="5,14!", shape = box, width=2.4, height=1, label = "Pre-Screened (n=',pre_Screened,')"];
      pre_ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,14!", shape = box, width=2.4, height=1, label = "Pre-Ineligible (n=',pre_Ineligible,')"];
      pre_eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,12!", shape = box, width=2.4, height=1, label = "Pre-Eligible (n=',pre_Eligible,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Not Consented (n=',Not_Consented,')\nRefused (n=',Refused,')"];

      cons [style="rounded,filled", fillcolor="#ccccff", pos="5,10!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="5,8!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,8!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,6!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      
      rand [style="rounded,filled", fillcolor="#ccccff", pos="5,4!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="5,2!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',Enrolled,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="10,4!", shape = box, width=2.4, height=1, label = "',disc_str,' (n=',Discontinuation,')"];

      active [style="rounded,filled", fillcolor="#ccccff", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="5,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="10,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      
      # Relationships
      pre_screened -> pre_eligible
      pre_screened -> pre_ineligible
      pre_eligible -> cons
      pre_eligible -> refused
      cons -> screened
      screened -> eligible
      screened -> ineligible
      eligible -> rand
      rand -> enrolled
      rand -> discon
      enrolled -> active
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


#' DSMB Consort Diagram With Pre Screened and No Definitive Event and with the Consented Group moved up
#'
#' @description 
#' Visualizes all counts of study statuses.
#' 
#' Very similar to dsmb_consort_diagram_pre_no_def except the consented group is before screened in the
#' chain of study statuses.
#'
#' @param analytic analytic data set that must include pre_screened, pre_eligible, screened, eligible,
#' consented, not_consented, randomized, enrolled, refused, completed, not_completed, not_expected, active, missed_final_followup, incomplete_final_followup
#' @param final_period visual option to name the last followup period
#' @param adjudicated visual option to show that discontinuation was adjudicated
#' @param definitive_event label for the definitive event
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' dsmb_consort_diagram_pre_shifted_consent("Replace with Analytic Tibble")
#' dsmb_consort_diagram_pre_shifted_consent("Replace with Analytic Tibble", definitive_event = 'TEST')
#' 
dsmb_consort_diagram_pre_shifted_consent <- function(analytic, final_period="12 Month", adjudicated=FALSE, definitive_event = "Nerve Surgery"){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c('pre_screened', 'pre_eligible', 'screened', 'eligible', 'consented', 'not_consented', 
                           'randomized', 'enrolled', 'refused', 'completed', 'not_completed', 'not_expected', 
                           'active', 'missed_final_followup', 'incomplete_final_followup', 'discontinued',
                           'time_zero', 'incomplete'),
    example_types = c('Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean',
                      'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean', 'Boolean',
                      'Date', 'Boolean'))

  pre_analytic <- analytic %>% 
    filter(pre_screened == TRUE)
  
  pre_Screened <- sum(pre_analytic$pre_screened, na.rm=TRUE)
  pre_Eligible <- sum(pre_analytic$pre_eligible, na.rm=TRUE)
  pre_Ineligible <- pre_Screened - pre_Eligible
  
  Consented <- sum(pre_analytic %>% 
                     filter(pre_eligible) %>% 
                     pull(consented), na.rm=TRUE)
  
  Not_Consented <- sum(pre_analytic %>% 
                         filter(pre_eligible) %>% 
                         pull(not_consented), na.rm=TRUE)
  
  Refused <- sum(pre_analytic %>% 
                   filter(eligible) %>% 
                   pull(refused), na.rm=TRUE)
  
  analytic <- analytic %>% 
    filter(screened == TRUE) 
  
  Screened <- sum(analytic$screened, na.rm=TRUE)
  Eligible <- sum(analytic$eligible, na.rm=TRUE)
  Ineligible <- Screened - Eligible
  
  
  Randomized <- sum(analytic %>% 
                      filter(eligible) %>% 
                      filter(consented) %>% 
                      pull(randomized), na.rm=TRUE)
  
  Enrolled <- sum(analytic %>% 
                    filter(eligible) %>% 
                    filter(consented) %>% 
                    filter(randomized) %>% 
                    pull(enrolled), na.rm=TRUE)
  if(adjudicated){
    Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(randomized) %>%
                             pull(adjudicated_discontinued), na.rm=TRUE)
  } else{
    Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(randomized) %>%
                             pull(discontinued), na.rm=TRUE)
  }
  
  
  fu_df <- analytic %>% 
    filter(eligible) %>% 
    filter(consented) %>% 
    filter(randomized) %>%
    filter(enrolled) %>% 
    filter(!is.na(time_zero))
  
  Definitive <- nrow(fu_df)
  
  complete <- sum(fu_df$completed, na.rm = TRUE)
  not_complete <- sum(fu_df$not_completed, na.rm = TRUE)
  missed <- sum(fu_df$missed_final_followup, na.rm = TRUE)
  incomplete <- sum(fu_df$incomplete, na.rm = TRUE)
  active <- sum(fu_df$active, na.rm = TRUE)
  not_expected <- sum(fu_df$not_expected, na.rm = TRUE)
  if(adjudicated){
    not_expected_str= "Adjudicated Not Expected"
  } else{
    not_expected_str= "Not Expected"
  }
  if(adjudicated){
    disc_str= "Adjudicated Discontinued"
  } else{
    disc_str= "Discontinued"
  }
  
  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      pre_screened [style="rounded,filled", fillcolor="#ccccff", pos="5,16!", shape = box, width=2.4, height=1, label = "Pre-Screened (n=',pre_Screened,')"];
      pre_ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,16!", shape = box, width=2.4, height=1, label = "Pre-Ineligible (n=',pre_Ineligible,')"];
      pre_eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,14!", shape = box, width=2.4, height=1, label = "Pre-Eligible (n=',pre_Eligible,')"];
      
      cons [style="rounded,filled", fillcolor="#ccccff", pos="5,12!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,14!", shape = box, width=2.4, height=1, label = "Not Consented (n=',Not_Consented,')\nRefused (n=',Refused,')"];
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="5,10!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,8!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];

      rand [style="rounded,filled", fillcolor="#ccccff", pos="5,6!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="5,4!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',Enrolled,')"];
      discon [style="rounded,filled", fillcolor="#ccccff", pos="10,6!", shape = box, width=2.4, height=1, label = "',disc_str,' (n=',Discontinuation,')"];
      
      df_complete [style="rounded,filled", fillcolor="#ccccff", pos="5,2!", shape = box, width=2.4, height=1, label = "',definitive_event,' (n=',Definitive,')"];

      active [style="rounded,filled", fillcolor="#ccccff", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="5,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="10,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      
      # Relationships
      pre_screened -> pre_eligible
      pre_screened -> pre_ineligible
      pre_eligible -> cons
      pre_eligible -> refused
      cons -> screened
      screened -> eligible
      screened -> ineligible
      eligible -> rand
      rand -> enrolled
      rand -> discon
      enrolled -> df_complete
      df_complete -> active
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


#' Cumulative percentage for ankle injuries
#'
#' @description 
#' Visualizes the percentage of study participant enrollment over time, using the consent_date construct. 
#' Only ankle injuries are accounted for.
#'
#' @param analytic analytic data set that must include study_id, injury_type (with an ankle value), 
#' enrolled
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' cumulative_percentage_ankle_injuries("Replace with Analytic Tibble")
#' 
cumulative_percentage_ankle_injuries <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('injury_type', 'enrolled', "consent_date"), 
    example_types = c('NamedCategory[\'ankle\']', 'Boolean', "Date"))

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
#' @description 
#' Visualizes the percentage of study participant enrollment over time, using the consent_date construct. 
#' Only plateau injuries are accounted for.
#'
#' @param analytic analytic data set that must include study_id, injury_type (with a plateau 
#' value), enrolled
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' cumulative_percentage_plateau_injuries("Replace with Analytic Tibble")
#' 
cumulative_percentage_plateau_injuries <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("injury_type", "enrolled", "consent_date"), 
    example_types = c("NamedCategory[\'plateau\']", "Boolean", "Date"))
  
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
#' @description 
#' Visualizes the enrollment by each site for each injury_type, in split bar chart with
#' each section of the bar indicating injury type.
#' 
#' NOTE: Currently, this function only works if injury_type only includes plateau and ankle injuries
#'
#' @param analytic This is the analytic data set that must include study_id, injury_type, enrolled, facilitycode
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' enrollment_by_injury_and_site("Replace with Analytic Tibble")
#' 
enrollment_by_injury_and_site <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('injury_type', "enrolled", "facilitycode", 'consent_date'), 
    example_types = c("NamedCategory[\'ankle\' \'plateau\']", "Boolean", 'FacilityCode', 'Date'))

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
#' @description 
#' This function visualizes the count of enrollment for each site.
#'
#' @param analytic analytic data set that must include study_id, enrolled, facilitycode, consent_date
#' @param number_order arranges output by number of enrolled
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' enrollment_by_site("Replace with Analytic Tibble")
#' enrollment_by_site("Replace with Analytic Tibble", number_order = TRUE)
#' 
enrollment_by_site <- function(analytic, number_order = FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("facilitycode", "enrolled", "consent_date"), 
    example_types = c("FacilityCode", "Boolean", "Date"))
  
  df <- analytic %>%  select(study_id, enrolled, facilitycode, consent_date) %>% 
    filter(enrolled == TRUE) %>% 
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
          legend.position = "top",
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
#' @description This function visualizes the cumulative number of patients enrolled, by month.
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, consent_date
#' @param bar_mode if false, uses a line to indicate total enrollment, and bars to indicate enrollment
#' change
#' @param goal number, the goal of enrollment
#' @param goal_percent if goal is supplied then sets the y axis label to percent
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' cumulative_enrolled("Replace with Analytic Tibble")
#' cumulative_enrolled("Replace with Analytic Tibble", bar_mode=TRUE)
#' cumulative_enrolled("Replace with Analytic Tibble", goal = 1000)
#' cumulative_enrolled("Replace with Analytic Tibble", goal = 1000, goal_percent = TRUE)
#' 
cumulative_enrolled <- function(analytic, bar_mode=FALSE, goal=NULL, goal_percent=FALSE){
  analytic <- if_needed_generate_example_data(analytic, 
                                              example_constructs = c('enrolled', "consent_date"), 
                                              example_types = c('Boolean', "Date"))
  
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
  
  if(!is.null(goal) && goal_percent) {
    yyyy_mm <- yyyy_mm %>%
      mutate(
        Total = (Total/goal) * 100,
        cumulative_value = (cumulative_value/goal) * 100
      )
    y_lab <- "Cumulative Percent"
    y_max <- max(100, max(yyyy_mm$cumulative_value))
    y_scale <- scale_y_continuous(labels = function(x) paste0(x, "%"))
  } else if(!is.null(goal)) {
    y_lab <- "Enrolled"
    y_max <- goal
    y_scale <- scale_y_continuous()
  } else {
    y_lab <- "Enrolled"
    y_max <- max(yyyy_mm$cumulative_value)
    y_scale <- scale_y_continuous()
  }
  
  if(!bar_mode){
    g <- ggplot(yyyy_mm) +
      geom_bar(aes(x = factor(year_month), y = Total, group = 1), stat = "identity", fill = "blue3", color = "black", size = 0.3) +
      geom_line(aes(x = factor(year_month), y = cumulative_value), data = yyyy_mm, stat = "identity", group = 1) +
      labs(title = "Cumulative Enrollment with Discrete Enrollment by Month", x = "Month", y = y_lab) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      coord_cartesian(ylim = c(0, y_max)) +
      y_scale
  } else {
    g <- ggplot(yyyy_mm) +
      geom_col(aes(x = factor(year_month), y = cumulative_value), fill = "blue3") +
      labs(title = "Cumulative Enrollment by Month", x = "Month", y = y_lab) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      coord_cartesian(ylim = c(0, y_max)) +
      y_scale
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
#' @description 
#' Visualizes the discrete number of participants enrolled by month using the consent_date construct. 
#' Notably, once a participant has been disenrolled, they are removed from this visualization.
#'
#' @param analytic analytic data set that must include enrolled, consent_date 
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' discrete_enrolled("Replace with Analytic Tibble")
#' 
discrete_enrolled <- function(analytic){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("enrolled", "consent_date"),
    example_types = c("Boolean", "Date")) 
  
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



#' Cumulative enrollment for Length of Stay
#'
#' @description 
#' Visualizes the distribution of the number of days recorded across the study in the ih_los_days construct.
#'
#' @param analytic analytic data set that must include study_id, ih_los_days
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' cumulative_enrolled_los("Replace with Analytic Tibble")
#' 
cumulative_enrolled_los <- function(analytic){
  
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("ih_los_days"), 
    example_types = c("Number-U30"))
  
  df <- analytic %>%  select(study_id, ih_los_days) %>% 
    filter(ih_los_days != 'Missing' & !is.na(ih_los_days))
  
  count_data <- df %>% 
    group_by(ih_los_days) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count), as.numeric(as.character(ih_los_days)))
  
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
#' @description Visualizes the cumulative number of patients enrolled, accompanied by 
#' a participant goal curve which demonstrates the goal relative to the actual cumulative participant enrollment process, 
#' and a second curve which demonstrates the necessary growth in order to meet that goal, all within a specified start and end date.
#'
#' @param analytic This is the analytic data set that must include study_id, enrolled, consent_date.
#' @param start_date The start date for the analysis.
#' @param end_date The end date for the analysis.
#' @param participant_goal The goal number of participants for the study.
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format. (intended for plotting)
#' @export
#'
#' @examples
#' cumulative_enrollment_goals("Replace with Analytic Tibble", start_date = "01-01-2025", end_date = "12-31-2026", 
#'   participant_goal = 500)
#' 
cumulative_enrollment_goals <- function(analytic, start_date, end_date, participant_goal){
  analytic <- if_needed_generate_example_data(analytic, 
                                              example_constructs = c('enrolled', 'consent_date'), 
                                              example_types = c('Boolean', 'Date'))
  
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
    mutate(cumulative_value = cumsum(Total)) %>% 
    slice(-1)
  
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
#' @description 
#' Visualizes the categorical percentages of study status as well as followup completions. 
#' Consort diagrams are almost fully customizable in their implementation. 
#' 
#' For other consort diagrams that may better fit your study, refer to: consort_diagram_no_definitive_event, 
#' dsmb_consort_diagram, dsmb_consort_diagram_pre_no_def, dsmb_consort_diagram_pre_no_def_shifted_consent, 
#' dsmb_consort_diagram_pre_shifted_consent, dsmb_nsaid_consort_diagram. 
#'
#' @param analytic analytic data set that must include study_id, screened, ineligible, eligible,
#' refused, consented, randomized, enrolled, time_zero, adjudicated_discontinued, completed, 
#' safety_set, exclusive_safety_set, not_completed, not_expected, active, missed_final_followup, incomplete_final_followup
#' @param final_period visual label of period of study completion, defaults to "12 Month"
#' @param definitive_event visual label of definitive event, defaults to "Definitive Fixation Complete" 
#' (attached to the count of the df_complete field)
#' @param not_expected_adjudicated whether to note that the Not Expected was adjudicated, purely visual
#' change
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' consort_diagram("Replace with Analytic Tibble")
#' 
consort_diagram <- function(analytic, final_period="12 Month", definitive_event = "Definitive Fixation Complete" , 
                            not_expected_adjudicated=TRUE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "ineligible", "eligible", "refused", "consented", 
                           "randomized", "enrolled", "adjudicated_discontinued", 
                           "completed", "safety_set", "exclusive_safety_set", "not_completed", 
                           "not_expected", "active", "missed_final_followup", "incomplete_final_followup", 
                           "time_zero"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Boolean", "Date"))
  df <- analytic %>% 
    select(study_id, screened, ineligible, eligible, refused, consented, randomized, enrolled, time_zero, 
           adjudicated_discontinued, completed, safety_set, exclusive_safety_set, not_completed, not_expected, 
           active, missed_final_followup, incomplete_final_followup) %>% 
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
  df_complete <- sum(enrolled_df$time_zero, na.rm = TRUE)
  
  fu_df <- enrolled_df %>% 
    filter(time_zero)
  
  complete <- sum(fu_df$completed, na.rm = TRUE)
  not_complete <- sum(fu_df$not_completed, na.rm = TRUE)
  missed <- sum(fu_df$missed_final_followup, na.rm = TRUE)
  incomplete <- sum(fu_df$incomplete_final_followup, na.rm = TRUE)
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

      active [style="rounded,filled", fillcolor="#a4d3ee", pos="2,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#a4d3ee", pos="6,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#a4d3ee", pos="10,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];

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
#' determined by the status column in the query_database being set to "Detected".
#' NOTE: this is not a dsmb visualization
#' 
#' See also: vislib_query_issues_per_site
#'
#'
#' @return html table
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
#' @description 
#' Returns interactive HTML of the query status of all the sites. The data displays the current and two
#' week previous status of the queries to show a sense of progress. To turn on this function, one must
#' not only set up queries but also talk to the Analytic Team to move the queries into the Analytic 
#' Codebase.
#'
#' @return HTML graph.
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



#' Consort Diagram No Definitive Event
#'
#' @description 
#' Visualizes study status data for studies without a definitive event. See functions consort_diagram,
#' dsmb_consort_diagram (all types) for related functions. The constructs completed, not_completed, 
#' missed_final_followup, and incomplete_final_followup are used in the last stage of the study, which
#' can be labelled by the final_period function parameter.
#' 
#' @param analytic analytic data set that must include study_id, screened, ineligible, eligible,
#' refused, consented, randomized, enrolled, adjudicated_discontinued, completed, 
#' safety_set, exclusive_safety_set, not_completed, not_expected, active, missed_final_followup, incomplete_final_followup
#' @param final_period labels the final follow-up period
#' @param not_expected_adjudicated whether to note that the Not Expected was adjudicated
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' consort_diagram_no_definitive_event("Replace with Analytic Tibble")
#' 
consort_diagram_no_definitive_event <- function(analytic, final_period="12 Month", not_expected_adjudicated=TRUE){
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened", "ineligible", "eligible", "refused", "consented", 
                           "randomized", "enrolled", 
                           "adjudicated_discontinued", "completed", "safety_set", 
                           "exclusive_safety_set", "not_completed", 
                           "not_expected", "active", "missed_final_followup", "incomplete_final_followup"),
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean")) 
  
  df <- analytic %>% 
    select(study_id, screened, ineligible, eligible, refused, consented, randomized, enrolled, 
           adjudicated_discontinued, completed, safety_set, exclusive_safety_set, not_completed, not_expected, active, missed_final_followup, incomplete_final_followup)
  
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
  
  complete <- sum(enrolled_df$completed, na.rm = TRUE)
  not_complete <- sum(enrolled_df$not_completed, na.rm = TRUE)
  missed <- sum(enrolled_df$missed_final_followup, na.rm = TRUE)
  incomplete <- sum(enrolled_df$incomplete_final_followup, na.rm = TRUE)
  active <- sum(enrolled_df$active, na.rm = TRUE)
  not_expected <- sum(enrolled_df$not_expected, na.rm = TRUE)
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

      active [style="rounded,filled", fillcolor="#a4d3ee", pos="2,2!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#a4d3ee", pos="6,2!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#a4d3ee", pos="10,2!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];

      # Relationships
      screened -> eligible
      screened -> ineligible
      eligible -> refused
      eligible -> consented
      consented -> randomized
      consented -> ed_consented
      randomized -> enrolled
      randomized -> ed_randomized
      enrolled -> active
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

#' Visualize patient outcomes by ID over time
#'
#' @description 
#' Creates a timeline visualization for each patient showing events relative to time zero for SINGLE event outcomes.
#'
#' @param analytic analytic data set that must include study_id, facilitycode, events_data, outcome_data 
#' and time_zero
#' @param event_name specific event to track (will mark first occurrence specially)
#' @param random_sample optional integer to limit to a random sample of IDs
#' @param facilitycodes optional character vector to limit to a certain facilities
#'
#' @return An HTML string containing an image tag with the base64-encoded timeline visualization in PNG format.
#' @export
#'
#' @examples
#' outcome_by_id("Replace with Analytic Tibble", "test_outcome")
#' outcome_by_id("Replace with Analytic Tibble", "test_outcome", random_sample = TRUE, facilitycodes = c('AAA', 'AAB'))
#' 
outcome_by_id <- function(analytic, event_name, random_sample = NULL, facilitycodes = NULL) {
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('outcome_data', 'enrolled', 'time_zero', 'facilitycode', 'events_data'), 
    example_types = c("(';', ',')NamedCategory['test_outcome']|Number|Number|Date|Date|NamedCategory['check' 'event']|Number|Number|Date",
                      'Boolean','Date','FacilityCode',
                      "(';', ',')Period|NamedCategory['test_outcome']|Form|NamedCategory['check' 'event']|Date"))
  
  
  # Check if required columns exist
  required_cols <- c("study_id", "facilitycode", "events_data", "outcome_data", "time_zero" , "enrolled")
  missing_cols <- required_cols[!required_cols %in% names(analytic)]
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  analytic <- analytic %>% filter(enrolled == TRUE)

  if (!is.null(random_sample)) {
    sample_ids <- sample(unique(analytic$study_id), random_sample)
    analytic <- analytic %>% filter(study_id %in% sample_ids)
  }
  
  if (!is.null(facilitycodes)) {
    analytic <- analytic %>% filter(facilitycode %in% facilitycodes)
  }

  # Process the events_data column
  events_df <- analytic %>%
    select(study_id, facilitycode, events_data, outcome_data, time_zero) %>%
    separate_rows(events_data, sep = ";") %>%
    separate(events_data, into = c("period", "name", "form", "type", "date"), sep = ",") %>% 
    filter(name == event_name)
  
  # Parse the outcome_data
  outcome_parsed <- analytic %>%
    select(study_id, outcome_data) %>%
    separate_rows(outcome_data, sep = ";") %>%
    separate(outcome_data, into = c("outcome_name", "target_days", "expected_days", 
                                   "time_zero", "outcome_date_extended", "outcome_type", 
                                   "outcome_days_extended", "outcome_days", "outcome_date"), 
             sep = ",") %>%
    mutate(target_days = as.numeric(target_days),
           expected_days = as.numeric(expected_days),
           outcome_days = as.numeric(outcome_days),
           outcome_days_extended = as.numeric(outcome_days_extended),
           time_zero = as.Date(time_zero),
           outcome_date = as.Date(outcome_date),
           outcome_date_extended = as.Date(outcome_date_extended)) %>% 
    filter(outcome_name == event_name)
  
  # Extract the information for the event of interest
  event_outcomes <- outcome_parsed %>% 
    filter(outcome_name == event_name) %>%
    select(study_id, target_days, expected_days, outcome_days, outcome_days_extended)
  
  # Convert date to proper format and join with outcome info
  events_df <- events_df %>%
    mutate(date = as.Date(date),
           time_zero = as.Date(time_zero),
           days_from_zero = as.numeric(difftime(date, time_zero, units = "days")),
           patient_label = paste(facilitycode, study_id, sep = "-")) %>%
    # Filter out events before time_zero
    filter(days_from_zero >= 0) %>% 
    left_join(event_outcomes, by = "study_id") %>% 
    arrange(patient_label) %>% 
    filter((time_zero+365)<Sys.Date()) %>% 
    select(-study_id, -facilitycode, -period) 
  
  patients_df <- events_df %>%
    select(patient_label, outcome_days, outcome_days_extended, expected_days) %>% 
    distinct()
  
  # Get the global target_days (should be same for all patients)
  target_days <- unique(event_outcomes$target_days)[1]
  
  # Create the plot
  g <- ggplot() +
    geom_segment(data = patients_df, 
                aes(x = 0, y = patient_label, 
                   xend = outcome_days, 
                   yend = patient_label),
                size = 1) +
    
    # Dotted line after first event until outcome_days
    geom_segment(data = patients_df %>% filter(outcome_days < expected_days), 
                 aes(x = outcome_days, 
                     y = patient_label, 
                     xend = expected_days, yend = patient_label),
                 linetype = "dotted", size = 1, color = "red") +
    
    # Dotted line after first event until outcome_days
    geom_segment(data = patients_df %>% filter(outcome_days < outcome_days_extended), 
                aes(x = outcome_days, 
                    y = patient_label, 
                    xend = outcome_days_extended, yend = patient_label),
                linetype = "dotted", size = 1) +
    
    # Vertical line at target_days
    geom_vline(xintercept = target_days, linetype = "dashed", color = "red") +
    
    # Event points - now with size mapping for "event" type
    geom_point(data = events_df, 
              aes(x = days_from_zero, y = patient_label, color = form, shape = type,
                  size = type == "event")) +
    
    # Formatting with classic paper theme
    scale_size_manual(values = c("TRUE" = 5, "FALSE" = 2), guide = "none") +
    scale_color_brewer(palette = "Set1", direction = -1) +  # More muted color palette
    labs(title = paste("Patient outcomes tracking:", str_replace_all(event_name, "_"," ")),
         subtitle = "Solid line until first event or expected follow-up date. Red line at target date.",
         x = "Days from time zero",
         y = "Study ID") +
    theme_minimal() +
    theme(text = element_text(family = "serif"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95"),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          plot.subtitle = element_text(size = 10, face = "italic", margin = margin(b = 20)),  # Add bottom margin to subtitle
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20))  # Add overall plot margins
  
  # Add annotation for target days mark
  g <- g + annotate("text", x = target_days, y = 0, 
                   label = paste0("Target (", target_days, " days)"), 
                   vjust = 2, color = "red")  # Changed y to 0 and vjust to 2 to position below
  
  # Save and convert to base64 image
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 10, height = max(8, nrow(patients_df) * 0.2), units = 'in', dpi = 200, limitsize = FALSE)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Patient outcomes timeline" style="max-width: 100%%; width: 100%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}


#' Weight Dearing adherence by ID
#'
#' @description 
#' Creates a timeline visualization for each patient showing adherence recording in text and call logs.
#' Will most likely work only for Weight Bearing, but potential changes to the adherence_data could make
#' this work for your study, so please contact an ADS member if you're interested in this visualization.
#'
#' @param analytic analytic data set that must include study_id, facilitycode, adherence_data (adherence_data must
#' be a long file with four columns: week, redcap_pt_call_status, text_logs_status, combined_status)
#' @param random_sample optional integer to limit to a random sample of IDs
#' @param facilitycodes optional character vector to limit to a certain facilities
#'
#' @return An HTML string containing an image tag with the base64-encoded timeline visualization in PNG format.
#' @export
#'
#' @examples
#' adherence_by_id("Replace with Analytic Tibble")
#' adherence_by_id("Replace with Analytic Tibble", random_sample = 10, facilitycodes = c('AAA', 'AAB'))
#' 
adherence_by_id <- function(analytic, random_sample = NULL, facilitycodes = NULL) {
  cached_arg <- analytic
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c('adherence_data', 'facilitycode'), 
    example_types = c("(';', ',')Number-U4|Boolean|Boolean|Boolean", 'FacilityCode'))
  
  if (!is.null(random_sample)) {
    sample_ids <- sample(unique(analytic$study_id), random_sample)
    analytic <- analytic %>% filter(study_id %in% sample_ids)
  }
  
  if (!is.null(facilitycodes)) {
    analytic <- analytic %>% filter(facilitycode %in% facilitycodes)
  }
  
  adherence_df <- analytic %>%
    select(study_id, facilitycode, adherence_data) %>%
    separate_rows(adherence_data, sep = ";") %>%
    separate(adherence_data, into = c("week", "redcap_pt_call_status", "text_logs_status", "combined_status"), sep = ",") %>%
    mutate(week = as.numeric(week))
  
  adherence_df <- adherence_df %>%
    mutate(patient_label = paste(facilitycode, study_id, sep = "-")) %>%
    arrange(patient_label) %>% 
    select(-study_id, -facilitycode) 
  
  if (cached_arg == 'Replace with Analytic Tibble') {
    adherence_df <- adherence_df %>%
      group_by(patient_label, week) %>%
      slice(1) %>%
      ungroup()
  }
  
  adherence_df <- adherence_df %>%
    mutate(combined_status = factor(combined_status, levels = c("TRUE", "FALSE")))
  
  first_nonadherent_or_last_adherent <- adherence_df %>%
    group_by(patient_label) %>%
    summarize(first_false_week = case_when(
        first(na.omit(combined_status)) == "TRUE" & any(combined_status == "FALSE", na.rm = TRUE) ~ 
          min(week[combined_status == "FALSE"], na.rm = TRUE),
        first(na.omit(combined_status)) == "TRUE" ~ max(week[combined_status == "TRUE"], na.rm = TRUE),
        TRUE ~ 0)
      ) %>%
    ungroup()
        
  g <- ggplot(adherence_df, aes(x = week, y = patient_label)) +
    geom_segment(
      data = first_nonadherent_or_last_adherent,
      aes(x = 0, y = patient_label, xend = first_false_week, yend = patient_label),
      inherit.aes = FALSE,
      color = "black",
      size = 1
    ) +
    geom_point(aes(color = combined_status), size = 3) +
    scale_color_manual(
      values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
      name = "Adherence Status",
      labels = c("Adherent", "Non-Adherent")
    ) +
    labs(
      title = "Patient Adherence by Week",
      x = "Week",
      y = "Patient",
      color = "Status"
    ) +
    theme_minimal() +
    theme(
      text = element_text(family = "serif"),
      plot.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(vjust = -1),
      axis.title.y = element_text(vjust = 1), 
      axis.text.y = element_text(size = 6),
      legend.position = "top", 
      legend.box = "horizontal",
      plot.margin = margin(t = 40, r = 20, b = 20, l = 20) 
    )
  
  temp_png_path <- tempfile(fileext = ".png")
  ggsave(temp_png_path, plot = g, width = 10, height = max(8, nrow(adherence_df %>% select(patient_label) %>% unique()) * 0.2), units = 'in', dpi = 200, limitsize = FALSE)
  image_data <- base64enc::base64encode(temp_png_path)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" alt="Patient adherence timeline" style="max-width: 100%%; width: 100%%;">', image_data)
  file.remove(temp_png_path)
  
  return(img_tag)
}

