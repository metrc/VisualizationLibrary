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
#' @param missing_post_consent_check construct name for missing post event check
#' @param missing_post_consent_check_label label for post missing event check
#' @param post_consent_check construct name for post event check
#' @param post_consent_check_label label for post event check
#' @param post_consent_eligible construct name for post event eligible
#' @param post_consent_eligible_label label for post event eligible
#' @param post_consent_ineligible construct name for post event ineligible
#' @param post_consent_ineligible_label label for post event ineligible
#' @param final_period label of the final period
#' @param adjudicated visual option to say that discontinuation was adjudicated, defaults to false
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' dsmb_consort_diagram_pre_post_consent_check("Replace with Analytic Tibble", "pre_eligible_not_continue", "Pre Eligible Not Continuing", "missing_double_check", "Missing Double Check", "not_started_post_consent_check", "Not Started Double Check", "late_post_consent_check", "Late Double Check", "post_consent_check", "Post Consent Check", "post_consent_eligible", "Post Consent Eligible", "post_consent_ineligible", "Post Consent Ineligible", final_period = '3 Month', adjudicated = TRUE)
#' 
dsmb_consort_diagram_pre_post_consent_check <- function(analytic, pre_eligible_not_continuing, pre_eligible_not_continuing_label,
 missing_post_consent_check, missing_post_consent_check_label, not_started_post_consent_check, not_started_post_consent_check_label,
 late_post_consent_check, late_post_consent_check_label, post_consent_check, post_consent_check_label,
 post_consent_eligible, post_consent_eligible_label, post_consent_ineligible, post_consent_ineligible_label, final_period="12 Month", adjudicated=FALSE){
  analytic <- if_needed_generate_example_data(
    analytic, 
    example_constructs = c("screened", "ineligible", "eligible",
                           "refused", "consented", "pre_eligible_not_continue", "not_started_double_check", "late_double_check",
                           "randomized", "enrolled", "adjudicated_discontinued", "not_consented",
                           "completed", "safety_set", "exclusive_safety_set", "not_completed", 
                           "not_expected", "active", "missed_final_followup", "incomplete_final_followup", 
                           "pre_screened", "pre_eligible", "pre_ineligible",
                           "discontinued", "double_check", "double_check_eligible", "double_check_ineligible", "missing_double_check"), 
    example_types = c("Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean", "Boolean"))

  # rename constructs by variable column names (won't necessarily be called that in the data so we need to rename them using the variable column names in dplyr)
  analytic <- analytic %>% 
    rename(missing_double_check = {{missing_post_consent_check}},
           double_check = {{post_consent_check}},
           double_check_eligible = {{post_consent_eligible}},
           double_check_ineligible = {{post_consent_ineligible}},
           pre_eligible_not_continue = {{pre_eligible_not_continuing}},
           not_started_double_check = {{not_started_post_consent_check}},
           late_double_check = {{late_post_consent_check}}) 
  
  pre_analytic <- analytic
  
  pre_Screened <- sum(pre_analytic$pre_screened, na.rm=TRUE)
  pre_Eligible <- sum(pre_analytic$pre_eligible, na.rm=TRUE)
  pre_Ineligible <- sum(pre_analytic$pre_ineligible, na.rm=TRUE)
  pre_eligible_not_continue <- sum(pre_analytic %>% filter(pre_eligible) %>% pull(pre_eligible_not_continue), na.rm=TRUE)
  
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

  missing_post_consent_check <- sum(analytic %>% 
                                      filter(eligible) %>% 
                                      filter(consented) %>% 
                                      pull(missing_double_check), na.rm=TRUE)

  not_started_post_consent_check <- sum(analytic %>% 
                                      filter(eligible) %>% 
                                      filter(consented) %>% 
                                      pull(not_started_double_check), na.rm=TRUE)

  late_post_consent_check <- sum(analytic %>% 
                                      filter(eligible) %>% 
                                      filter(consented) %>% 
                                      pull(late_double_check), na.rm=TRUE)                         

  post_consent_check <- sum(analytic %>% 
                              filter(eligible) %>% 
                              filter(consented) %>% 
                              pull(double_check), na.rm=TRUE)
  post_consent_eligible <- sum(analytic %>% 
                                 filter(eligible) %>% 
                                 filter(consented) %>% 
                                 filter(double_check) %>% 
                                 pull(double_check_eligible), na.rm=TRUE)
  Randomized <- sum(analytic %>% 
                      filter(eligible) %>% 
                      filter(consented) %>% 
                      filter(double_check) %>% 
                      filter(double_check_eligible) %>% 
                      pull(randomized), na.rm=TRUE)
  
  Enrolled <- sum(analytic %>% 
                    filter(eligible) %>% 
                    filter(consented) %>% 
                    filter(double_check) %>% 
                    filter(double_check_eligible) %>% 
                    filter(randomized) %>% 
                    pull(enrolled), na.rm=TRUE)
  if(adjudicated){
    Post_Randomization_Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(double_check) %>% 
                             filter(double_check_eligible) %>% 
                             filter(randomized) %>%
                             pull(adjudicated_discontinued), na.rm=TRUE)

    Pre_Randomization_Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(!randomized|is.na(randomized)) %>%
                             pull(adjudicated_discontinued), na.rm=TRUE)
    
    post_consent_ineligible <- sum(analytic %>% 
                                     filter(eligible) %>% 
                                     filter(consented) %>% 
                                     filter(double_check) %>% 
                                     filter(!adjudicated_discontinued|is.na(adjudicated_discontinued)) %>% 
                                     pull(double_check_ineligible), na.rm=TRUE)
  } else{
    Post_Randomization_Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(double_check) %>% 
                             filter(double_check_eligible) %>% 
                             filter(randomized) %>%
                             pull(discontinued), na.rm=TRUE)

    Pre_Randomization_Discontinuation <- sum(analytic %>% 
                             filter(eligible) %>% 
                             filter(consented) %>% 
                             filter(!randomized|is.na(randomized)) %>%
                             pull(discontinued), na.rm=TRUE)
    
    post_consent_ineligible <- sum(analytic %>% 
                                     filter(eligible) %>% 
                                     filter(consented) %>% 
                                     filter(double_check) %>% 
                                     filter(!discontinued|is.na(discontinued)) %>% 
                                     pull(double_check_ineligible), na.rm=TRUE)
  }
  
  
  fu_df <- analytic %>% 
    filter(eligible) %>% 
    filter(consented) %>% 
    filter(double_check) %>% 
    filter(double_check_eligible) %>% 
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
      
      pre_screened [style="rounded,filled", fillcolor="#ccccff", pos="5,18!", shape = box, width=2.4, height=1, label = "Pre-Screened (n=',pre_Screened,')"];
      pre_ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,18!", shape = box, width=2.4, height=1, label = "Pre-Ineligible (n=',pre_Ineligible,')"];
      pre_eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,16!", shape = box, width=2.4, height=1, label = "Pre-Eligible (n=',pre_Eligible,')"];
      pre_eligible_not_continue [style="rounded,filled", fillcolor="#ccccff", pos="10,16!", shape = box, width=2.4, height=1, label = "',pre_eligible_not_continuing_label,' (n=',pre_eligible_not_continue,')"];
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="5,14!", shape = box, width=2.4, height=1, label = "Screened (n=',Screened,')"];
      ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,14!", shape = box, width=2.4, height=1, label = "Ineligible (n=',Ineligible,')"];
      eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,12!", shape = box, width=2.4, height=1, label = "Eligible (n=',Eligible,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,12!", shape = box, width=2.4, height=1, label = "Not Consented (n=',Not_Consented,')\nRefused (n=',Refused,')"];

      cons [style="rounded,filled", fillcolor="#ccccff", pos="5,10!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];

      missing_post_consent_check [style="rounded,filled", fillcolor="#ccccff", pos="10,10!", shape = box, width=2.4, height=1, label = "Pre Randomization\n',disc_str,' (n=',Pre_Randomization_Discontinuation,')\n',missing_post_consent_check_label,' (n=',missing_post_consent_check,')\n',not_started_post_consent_check_label,' (n=',not_started_post_consent_check,')\n',late_post_consent_check_label,' (n=',late_post_consent_check,')"];

      post_consent_check [style="rounded,filled", fillcolor="#ccccff", pos="5,8!", shape = box, width=2.4, height=1, label = "',post_consent_check_label,' (n=',post_consent_check,')"];
      post_consent_eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,6!", shape = box, width=2.4, height=1, label = "',post_consent_eligible_label,' (n=',post_consent_eligible,')"];
      
      post_consent_ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,8!", shape = box, width=2.4, height=1, label = "',post_consent_ineligible_label,' (n=',post_consent_ineligible,')"];
      
      rand [style="rounded,filled", fillcolor="#ccccff", pos="5,4!", shape = box, width=2.4, height=1, label = "Randomized (n=',Randomized,')"];
      
      enrolled [style="rounded,filled", fillcolor="#ccccff", pos="5,2!", shape = box, width=2.4, height=1, label = "Eligible and Enrolled (n=',Enrolled,')"];
      post_randomization_discontinuation [style="rounded,filled", fillcolor="#ccccff", pos="10,4!", shape = box, width=2.4, height=1, label = "',disc_str,' (n=',Post_Randomization_Discontinuation,')"];

      active [style="rounded,filled", fillcolor="#ccccff", pos="0,0!", shape = box, width=2.4, height=1, label = "Active (n=',active,')"];
      not_expected [style="rounded,filled", fillcolor="#ccccff", pos="5,0!", shape = box, width=2.4, height=1, label = "',not_expected_str,' (n=',not_expected,')"];
      fu_complete [style="rounded,filled", fillcolor="#ccccff", pos="10,0!", shape = box, width=2.4, height=1, label = "',final_period,' Follow-Up Complete (n=',complete,')\n',final_period,' Follow-Up Incomplete (n=',incomplete,')\nNot Completed (n=',not_complete,')\nMissed (n=',missed,')"];
      
      # Relationships
      pre_screened -> pre_eligible
      pre_screened -> pre_ineligible
      pre_eligible -> pre_eligible_not_continue
      pre_eligible -> screened
      screened -> eligible
      screened -> ineligible
      eligible -> cons
      eligible -> refused
      cons -> missing_post_consent_check
      cons -> post_consent_check
      post_consent_check -> post_consent_eligible
      post_consent_check -> post_consent_ineligible
      post_consent_eligible -> rand
      rand -> post_randomization_discontinuation
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
#' @param pre_screened_name label for pre-screened
#' @param screened_name label for screened
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' dsmb_consort_diagram_pre_shifted_consent("Replace with Analytic Tibble")
#' dsmb_consort_diagram_pre_shifted_consent("Replace with Analytic Tibble", definitive_event = 'TEST')
#' 
dsmb_consort_diagram_pre_shifted_consent <- function(analytic, final_period="12 Month", adjudicated=FALSE, definitive_event = "Nerve Surgery", pre_screened_name = NULL, screened_name = NULL){
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
                   filter(pre_eligible) %>% 
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
      
      pre_screened [style="rounded,filled", fillcolor="#ccccff", pos="5,16!", shape = box, width=2.4, height=1, label = "',pre_screened_name,' (n=',pre_Screened,')"];
      pre_ineligible [style="rounded,filled", fillcolor="#ccccff", pos="10,16!", shape = box, width=2.4, height=1, label = "Pre-Ineligible (n=',pre_Ineligible,')"];
      pre_eligible [style="rounded,filled", fillcolor="#ccccff", pos="5,14!", shape = box, width=2.4, height=1, label = "Pre-Eligible (n=',pre_Eligible,')"];
      
      cons [style="rounded,filled", fillcolor="#ccccff", pos="5,12!", shape = box, width=2.4, height=1, label = "Consented (n=',Consented,')"];
      
      refused [style="rounded,filled", fillcolor="#ccccff", pos="10,14!", shape = box, width=2.4, height=1, label = "Not Consented (n=',Not_Consented,')\nRefused (n=',Refused,')"];
      
      screened [style="rounded,filled", fillcolor="#ccccff", pos="5,10!", shape = box, width=2.4, height=1, label = "',screened_name,' (n=',Screened,')"];
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
#' @param not_enrolled_missing_df_reason adds reasons for not enrollment beyond adjudication to consort
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' consort_diagram("Replace with Analytic Tibble")
#' 
consort_diagram <- function(analytic, final_period="12 Month", definitive_event = "Definitive Fixation Complete" , 
                            not_expected_adjudicated=TRUE, not_enrolled_missing_df_reason=FALSE){
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
  
  if(not_enrolled_missing_df_reason){
    reasons <- analytic %>% filter(!is.na(not_enrolled_missing_df_reason)) %>% pull(not_enrolled_missing_df_reason)
    reasons <- c(sort(reasons[reasons!="Missing Definitive Fixation"]), reasons[reasons=="Missing Definitive Fixation"])
    missing_not_enrolled_str <- ""
    for(reason in unique(reasons)){
      missing_not_enrolled_str <- paste0(missing_not_enrolled_str, "\n", ifelse(reason=="Missing Definitive Fixation","","Missing Definitive Fixation: "), reason," (n=",length(reasons[reasons==reason]),")")
    }
  } else{
    missing_not_enrolled_str <- ""
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
      
      ed_randomized [style="rounded,filled", fillcolor="#a4d3ee", pos="10,6!", shape = box, width=2.4, height=1, label = "Adjudicated Discontinued (Randomized) (n=',ed_randomized,')', missing_not_enrolled_str,'"];
      
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


#' Consort Diagram: Weight Bearing publication
#'
#' @description 
#' Visualizes the categorical percentages of study status as well as followup completions. 
#' Consort diagrams are almost fully customizable in their implementation. 
#' 
#' This consort diagram was made for the Weight Bearing study, and so is unlikely to work for yours.
#' 
#' For other consort diagrams that may better fit your study, refer to: consort_diagram_no_definitive_event, 
#' dsmb_consort_diagram, dsmb_consort_diagram_pre_no_def, dsmb_consort_diagram_pre_no_def_shifted_consent, 
#' dsmb_consort_diagram_pre_shifted_consent, dsmb_nsaid_consort_diagram. 
#'
#' @param analytic analytic data set that must include 
#' study_id, screened, ineligible, ineligibility_reasons, refused, constraint_other, constraint_other_txt, consented, 
#' discontinued_pre_randomization, injury_type (must have ankle and plateau values), randomized, 
#' late_ineligible, per_protocol_sample, enrolled, consent_date, death_date, withdraw_date,
#' preinjury_work_status, followup_expected_12mo, completed, outcome_data
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' consort_diagram_wb_publication("Replace with Analytic Tibble")
#' 
consort_diagram_wb_publication <- function(analytic){
  
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened", "ineligible", "ineligibility_reasons", "refused", "constraint_unavailable",
                           "constraint_other", "constraint_issue", "constraint_other_txt", "constraint_unavailable", "constraint_surgeon_unwilling",
                           "consented", "discontinued_pre_randomization", "received_treatment",
                           "injury_type", "randomized", "late_ineligible", "per_protocol_sample", "enrolled", 
                           "consent_date", "death_date", "not_consented", "withdraw_date", "preinjury_work_status", "followup_expected_12mo",
                           "completed", "outcome_data"),
    example_types = c("Boolean", "Boolean", "Category-NS", "Boolean", "Boolean", "Boolean", "Boolean", "Character",
                      "Boolean", "Boolean", "Boolean", "NamedCategory['ankle' 'plateau']", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Date", "Date", "Date", "Boolean","Boolean", "Boolean", "Boolean",
                      "(';', ',')NamedCategory['returned_to_work' 'admission_for_complication']|Number|Number|Date|NamedCategory['event' 'check']|Number|Number|Date"))
  
  df <- analytic %>% 
    select(study_id, screened, ineligible, ineligibility_reasons, refused, constraint_other, constraint_issue, constraint_other_txt, 
           constraint_unavailable, constraint_surgeon_unwilling, consented, discontinued_pre_randomization, received_treatment,
           injury_type, randomized, late_ineligible, per_protocol_sample, enrolled, consent_date, death_date, not_consented,
           withdraw_date, preinjury_work_status, followup_expected_12mo, completed, outcome_data) %>% 
    filter(screened)
  
  ir_count <- df %>%
    select(study_id, ineligibility_reasons) %>%
    filter(!is.na(ineligibility_reasons)) %>%
    separate_rows(ineligibility_reasons, sep = '; ') %>%
    count(ineligibility_reasons) %>%
    arrange(desc(n))
  
  top_reasons <- ir_count %>%
    pull(ineligibility_reasons)
  top_reasons <- top_reasons[1:6]
  
  ir_count_raw <- df %>%
    select(study_id, ineligibility_reasons) %>%
    filter(!is.na(ineligibility_reasons)) %>%
    count(ineligibility_reasons)
  
  top_reasons_count <- ir_count_raw %>%
    filter(ineligibility_reasons %in% top_reasons) %>%
    arrange(desc(n))
  
  total_count <- sum(ir_count_raw$n)
  other_count <- sum(ir_count_raw$n) - sum(top_reasons_count$n)
  
  other_row <- tibble(
    ineligibility_reasons = 'Other reason/Multiple reasons',
    n = other_count
  )
  
  top_reasons_count <- rbind(top_reasons_count, other_row)
  
  screened <- sum(df$screened, na.rm = TRUE)
  ineligible <- sum(df$ineligible, na.rm = TRUE)
  
  refused <- sum(df$refused, na.rm = TRUE)
  constraint <- sum(df$constraint_other, na.rm = TRUE) + sum(df$constraint_issue, na.rm = TRUE)
  constraint_unavailable <- sum(df$constraint_unavailable & (is.na(df$constraint_other)|!df$constraint_other), na.rm = TRUE)
  constraint_surgeon_unwilling <- sum(df$constraint_surgeon_unwilling & (is.na(df$constraint_other)|!df$constraint_other)& (is.na(df$constraint_unavailable)|!df$constraint_unavailable), na.rm = TRUE)
  
  late_discontinuation <- sum(df$discontinued_pre_randomization & 
                                df$consented, na.rm = TRUE)
  
  plateau_injuries <- sum(df$injury_type=='plateau', na.rm = TRUE)
  
  accounted_ids <- df %>% filter(ineligible|refused|constraint_other|constraint_issue|constraint_unavailable|constraint_surgeon_unwilling|(discontinued_pre_randomization & consented)|injury_type=='plateau'|(injury_type=='ankle' & randomized)) %>% pull(study_id)
  
  not_consented <- sum(df %>% filter(!study_id %in% accounted_ids) %>% pull(not_consented), na.rm = TRUE)
  
  randomized <- sum((df$injury_type=='ankle'|is.na(df$injury_type)) & df$randomized, na.rm = TRUE)
  
  late_ineligible <- sum((df$injury_type=='ankle'|is.na(df$injury_type)) & df$randomized & df$late_ineligible, na.rm = TRUE)
  diverging_review <- sum((df$injury_type=='ankle'|is.na(df$injury_type)) & df$randomized & (!df$late_ineligible|is.na(df$late_ineligible))&!df$per_protocol_sample, na.rm = TRUE)
  
  died <- sum(as.Date(df$death_date)-as.Date(df$consent_date)<365, na.rm = TRUE)
  withdrew <- sum(as.Date(df$withdraw_date)-as.Date(df$consent_date)<365, na.rm = TRUE)
  
  extract_outcome_expected <- function(inner_df) {
    long_outcomes <- inner_df %>%
      select(outcome_data) %>%
      separate_rows(outcome_data, sep = ';') %>%
      separate(outcome_data, into = c("outcome_name", "target_days", "expected_days",
                                      "time_zero", "outcome_date_extended", "outcome_type",
                                      "outcome_days_extended", "outcome_days", "outcome_date"), sep = ',') %>%
      filter((as.Date(time_zero)+365)<Sys.Date()) %>% 
      mutate(
        target_days = as.numeric(target_days),
        expected_days = as.numeric(expected_days),
        outcome_days_extended = as.numeric(outcome_days_extended),
        outcome_days = as.numeric(outcome_days)
      ) %>%
      group_by(outcome_name) %>%
      summarise(
        pct_expected = paste0(round(sum(outcome_days, na.rm = TRUE)/ sum(expected_days, na.rm = TRUE) *100, 0), "%")
      )
    long_outcomes
  }
  
  outcome_extracted <- extract_outcome_expected(df)
  afc_expected <- outcome_extracted %>% filter(outcome_name == 'admission_for_complication') %>% pull(pct_expected)
  rtw_expected <- outcome_extracted %>% filter(outcome_name == 'returned_to_work') %>% pull(pct_expected)
  
  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline]
      
      title [style="rounded,filled", fillcolor="#a4d3ee", pos="2,5.5!", shape = box, width=2.4, height=.5, 
        label = "', screened, ' Patients screened for eligibility"];
        
      box1 [style="rounded,filled", fillcolor="#a4d3ee", pos="4.5,3.25!", shape = box, width=2.4, height=.5, 
      labeljust=l,
      label = <
        <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
          <TR><TD ALIGN="LEFT">', ineligible, ' Did not meet eligibility criteria</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', top_reasons_count$n[1], ' ', top_reasons_count$ineligibility_reasons[1], '</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', top_reasons_count$n[2], ' ', top_reasons_count$ineligibility_reasons[2], '</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', top_reasons_count$n[3], ' ', top_reasons_count$ineligibility_reasons[3], '</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', top_reasons_count$n[4], ' ', top_reasons_count$ineligibility_reasons[4], '</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', top_reasons_count$n[5], ' ', top_reasons_count$ineligibility_reasons[5], '</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', top_reasons_count$n[6], ' ', top_reasons_count$ineligibility_reasons[6], '</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', top_reasons_count$n[7], ' ', top_reasons_count$ineligibility_reasons[7], '</TD></TR>
          <TR><TD ALIGN="LEFT">', refused, ' Declined consent</TD></TR>
          <TR><TD ALIGN="LEFT">', constraint_unavailable, ' Patient not available for consent</TD></TR>            
          <TR><TD ALIGN="LEFT">', constraint_surgeon_unwilling, ' Had surgeon unwilling to randomize</TD></TR>            
          <TR><TD ALIGN="LEFT">', constraint, ' Had other reasons not enrolled</TD></TR>
          <TR><TD ALIGN="LEFT">', late_discontinuation, ' Discontinued after consent, prior to randomization</TD></TR>
          <TR><TD ALIGN="LEFT">', plateau_injuries, ' Enrolled patients with tibial plateau fractures</TD></TR>
        </TABLE>
      >];
        
      title2 [style="rounded,filled", fillcolor="#a4d3ee", pos="2,1!", shape = box, width=2.4, height=.5, 
        label = "', randomized, ' Underwent randomization"];
        
      box2 [style="rounded,filled", fillcolor="#a4d3ee", pos="2,-0.5!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', late_ineligible, ' Late ineligible</TD></TR>
            <TR><TD ALIGN="LEFT">', diverging_review, ' Weight bearing instructions review diverged from protocol</TD></TR>
            <TR><TD ALIGN="LEFT">', randomized-late_ineligible-diverging_review, ' Included in primary analysis</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', died, ' Died prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', withdrew, ' Withdrew prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">', afc_expected, ' Admitted for complication out of expected</TD></TR>
            <TR><TD ALIGN="LEFT">', rtw_expected, ' Returned to work out of expected</TD></TR>
          </TABLE>
        >]
        
      midpoint [style=invis, pos="1.34,3.125!, width=0, height=0"]
        
      # Relationships
      title -> title2
      midpoint -> box1
      title2 -> box2
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
#' @param days_since_dz optional numeric keyowrd argument to filter only for rows whose time_zero occured 
#' at least that many days ago
#' @param sort_by_fu_days optional boolean that sorts the html by the span from time_zero of the most
#' available data we have
#'
#' @return An HTML string containing an image tag with the base64-encoded timeline visualization in PNG format.
#' @export
#'
#' @examples
#' outcome_by_id("Replace with Analytic Tibble", "test_outcome")
#' outcome_by_id("Replace with Analytic Tibble", "test_outcome", sort_by_followup_days = TRUE)
#' outcome_by_id("Replace with Analytic Tibble", "test_outcome", random_sample = 50, facilitycodes = c('AAA', 'AAB'))
#' 
outcome_by_id <- function(analytic, event_name, random_sample = NULL, facilitycodes = NULL, 
                          days_since_tz = 365, sort_by_fu_days = FALSE) {
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
  
  analytic <- analytic %>% filter(enrolled == TRUE & !is.na(time_zero))

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
  outcome_raw <- analytic %>%
    select(study_id, outcome_data) %>%
    separate_rows(outcome_data, sep = ";") %>%
    separate(outcome_data, into = c("outcome_name", "target_days", "expected_days", 
                                   "time_zero", "outcome_date_extended", "outcome_type", 
                                   "outcome_days_extended", "outcome_days", "outcome_date"), 
             sep = ",") 

  outcome_parsed <- outcome_raw %>%
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
  dates_df <- events_df %>%
    mutate(date = as.Date(date),
           time_zero = as.Date(time_zero),
           days_from_zero = as.numeric(difftime(date, time_zero, units = "days")),
           patient_label = paste(facilitycode, study_id, sep = "-")) %>%
    # Filter out events before time_zero
    filter(days_from_zero >= 0) %>% 
    left_join(event_outcomes, by = "study_id") %>% 
    arrange(patient_label) %>% 
    filter((time_zero+days_since_tz)<Sys.Date()) %>% 
    select(-study_id, -facilitycode, -period) 
  
  patients_df <- dates_df %>%
    select(patient_label, outcome_days, outcome_days_extended, expected_days) %>% 
    distinct()
  
  # Get the global target_days (should be same for all patients)
  target_days <- unique(event_outcomes$target_days)[1]
  
  favorable_events_present <- nrow(events_df %>% filter(type == 'favorable_event')) > 0
  events_present <- nrow(events_df %>% filter(str_detect(type, 'event'))) > 0
  shape_breaks <- 16  # Circle for checks (always present)
  shape_labels <- "Check"
  if (events_present) {
    shape_breaks <- c(shape_breaks, 17)  
    shape_labels <- c(shape_labels, if(any(dates_df$type == "unfavorable_event")) "Unfavorable Event" else "Event")
  }
  if (favorable_events_present) {
    shape_breaks <- c(shape_breaks, 15)  
    shape_labels <- c(shape_labels, "Favorable Event")
  }
  
  # Create the plot
  g <- ggplot() +
    #solid black until outcome_days
    geom_segment(data = patients_df,
                aes(x = 0, y = patient_label,
                   xend = outcome_days,
                   yend = patient_label),
                size = 1) +
    
    geom_segment(data = dates_df %>% filter(type == 'favorable_event' & days_from_zero < outcome_days), 
                 aes(x = days_from_zero, 
                     y = patient_label, 
                     xend = outcome_days, yend = patient_label),
                 linetype = "dotted", size = 1, color = "green") +
    
    #if event occurs before expected days, then red dot line until expected days
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
    geom_point(data = dates_df, 
              aes(x = days_from_zero, y = patient_label, color = form, 
                  shape = case_when(type == "check" ~ 16,
                                    type %in% c('event', 'unfavorable_event') ~ 17,
                                    type == 'favorable_event' ~ 15,
                                    TRUE ~ 16),
                  size = str_detect(type, "event"))) +
    scale_shape_identity(
      name = "Event type",
      guide = "legend",
      breaks = shape_breaks,
      labels = shape_labels) +
  
    # Formatting with classic paper theme
    scale_size_manual(values = c("TRUE" = 5, "FALSE" = 2), guide = "none") +
    scale_color_brewer(palette = "Set1", direction = -1) + 
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
          plot.subtitle = element_text(size = 10, face = "italic", margin = margin(b = 20)),  
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20))  
  
  # Add annotation for target days mark
  g <- g + annotate("text", x = target_days, y = 0, 
                   label = paste0("Target (", target_days, " days)"), 
                   vjust = 2, color = "red")
  
  # section for arranging final output
  if (sort_by_fu_days) {
    order_df <- dates_df %>% 
      group_by(patient_label) %>%
      summarize(days_of_followup = max(days_from_zero)) %>%
      ungroup() %>%
      arrange(days_of_followup)
  } else {
    order_df <- dates_df %>% arrange(desc(patient_label))
  }
  order <- order_df %>%
    pull(patient_label) %>%
    unique()
  g <- g + scale_y_discrete(limits = order)
  
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
#' Data is filtered for enrolled participants.
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
    example_constructs = c('adherence_data', 'facilitycode', 'enrolled'), 
    example_types = c("(';', ',')Number-U4|Boolean|Boolean|Boolean", 'FacilityCode', 'Boolean'))
  
  analytic <- analytic %>%
    filter(enrolled)
  
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
  
  if(length(cached_arg) == 1) {
    if (cached_arg == 'Replace with Analytic Tibble') {
      adherence_df <- adherence_df %>%
        group_by(patient_label, week) %>%
        slice(1) %>%
        ungroup()
      }
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


library(tidyverse)
library(ggplot2)
library(gridExtra)

#' Progress Tracker
#'
#' @description 
#' Generates a progress bar/visual to display a percentage and item based tracker of what things for a given study must be done.
#' 
#' Currently only rendering for the IVAC data set
#'
#' @param analytic analytic data set that must include study_id, and the associated data set called progress_tracker (dummy column with all values set to TRUE)
#' @param style integers 1-5 indicating the type of style preffered for the progress indicator
#' @param showCheckPoints boolean to render the names of the tasks next to the progress bar
#' @param trackers a comma separated string of particular tracker names, use "ALL" for all trackers, full name doesnt need to be given
#' @param numeratorConstruct the name of the construct you want to use as the numerator for your construct progress bar, 
#' if denominator is NA this construct must be only TRUE or FALSE so it can be its own denominator
#' @param denominatorConstruct optional denominator construct to allow the ratioing of one construct to another
#' @param constructTrackerName name of the tracker for the given construct params
#' @param constructUnits the "units" that are being mesured within the given tracker
#'
#' @return An HTML string that renders the progress bar with the associated settings
#' @export
#'
#' @examples
#' progress_tracker(1, TRUE, "ALL")
#' progress_tracker(5, FALSE, "FirstTracker, Sec")
#' 
progress_tracker <- function(analytic, style, showCheckPoints, trackers, numeratorConstruct, denominatorConstruct, constructTrackerName, constructUnits) {
  
  master_list <- list()
  
  addProgressTracker <- function(master_list, name, tasks, statuses, done_values = "DONE", style = NA) {
    
    if (length(tasks) != length(statuses)) {
      stop(paste0("addProgressTracker: 'tasks' and 'statuses' must be the same length (got ",
                  length(tasks), " tasks and ", length(statuses), " statuses) for tracker '", name, "'"))
    }
    
    if (name %in% names(master_list)) {
      warning(paste0("addProgressTracker: a tracker named '", name, "' already exists and is being overwritten"))
    }
    
    normalized_status <- ifelse(statuses %in% done_values, "DONE", "PENDING")
    
    tracker_df <- data.frame(task = tasks, status = normalized_status, raw_status = statuses,
                             stringsAsFactors = FALSE)
    
    attr(tracker_df, "style") <- style   # reserved for future per-tracker styling
    
    master_list[[name]] <- tracker_df
    master_list
  }
  
  ##if we have a progress tracker we can load for the analytic we load it and pass it through our new function to get the data the same way
  if (file.exists("progress_tracker.csv")) {
    csv_data <- read_csv("progress_tracker.csv")
    
    csv_total_cols <- ncol(csv_data)
    if (csv_total_cols %% 2 != 0) {
      stop("Must have an even number of colums, check to see if each tracker has a task column and status column")
    }
    csv_status_count <- sum(grepl("status", colnames(csv_data), ignore.case = TRUE))
    if (csv_status_count != csv_total_cols / 2) {
      stop("Not enough 'status' columns! Please check that for every task column there is a column that has status next to it")
    }
    
    for (col_i in seq(1, csv_total_cols, by = 2)) {
      master_list <- addProgressTracker(
        master_list,
        name        = names(csv_data)[col_i],
        tasks       = csv_data[[col_i]],
        statuses    = csv_data[[col_i + 1]],
        done_values = "DONE"
      )
    }
  }
  
  
  
  target_trackers <- trimws(unlist(strsplit(trackers, ",")))
  global_style <- style   # the style this whole call was given; per-tracker overrides fall back to this
  global_showNames <- showCheckPoints #need a secondary variable so things dont get messed up when switching for the 10/7 cases
  
  for (curr_tracker_name in names(master_list)) {
    showCheckPoints <- global_showNames
    
    if(trackers != "ALL"){
      if(!any(startsWith(curr_tracker_name, target_trackers))){
        next
      }
    }
    
    #create simple standardized dataframe for each tracker that splits it into tasks and statuses
    tracker_df  <- master_list[[curr_tracker_name]]
    tasks       <- tracker_df$task
    completions <- tracker_df$status
    raw_statuses <- tracker_df$raw_status   # pre-normalization values, used only by the ratio check below
    
    ##this logic is for future specific styling, we fall back on global style if this value is null
    tracker_style <- attr(tracker_df, "style")
    style <- if (!is.null(tracker_style) && !is.na(tracker_style)) tracker_style else global_style
    
    total_tasks <- sum(tasks != "" & !is.na(tasks)) # recalculated below once the empty rows are actually filtered out
    completed_tasks <- 0
    
    #data organizing and cleaning, marks all tasks as a locally defined completed or pending for consistency
    # Filter out empty rows
    valid_rows <- tasks != "" & !is.na(tasks)
    df <- data.frame(
      task = tasks[valid_rows],
      status = ifelse(completions[valid_rows] %in% "DONE", "Completed", "Pending"),
      stringsAsFactors = FALSE
    )
    
    total_tasks <- nrow(df)
    if(total_tasks == 0) next #skip if no tasks
    
    #sort so Completed tasks are always filled first, then Pending
    df$status <- factor(df$status, levels = c("Completed", "Pending"))
    df <- df[order(df$status), ]
    df$id <- 1:total_tasks
    
    #wrap text cleanly so it fits into the visual segments (approx 15 chars wide)
    df$task_wrap <- stringr::str_wrap(df$task, width = 15)
    
    completed_tasks <- sum(df$status == "Completed", na.rm = TRUE)
    percent_complete <- (completed_tasks / total_tasks) * 100
    
    #ratio format check
    looks_numeric <- function(x) !is.na(suppressWarnings(as.numeric(x)))
    
    if (total_tasks == 1 && looks_numeric(df$task[1]) && looks_numeric(raw_statuses[valid_rows][1])) {
      ratio_total     <- round(as.numeric(df$task[1]))
      ratio_completed <- round(as.numeric(raw_statuses[valid_rows][1]))
      
      if (ratio_completed > ratio_total) {
        stop(paste0("progress_tracker: tracker '", curr_tracker_name, "' looks like a ratio (one numeric row), ",
                    "but its status value (", ratio_completed, ") is bigger than its task value (", ratio_total, "). ",
                    "Expected the task column to hold the total and the status column to hold the completed count."))
      }
      
      df <- data.frame(
        task   = paste0("Task ", seq_len(ratio_total)),
        status = factor(ifelse(seq_len(ratio_total) <= ratio_completed, "Completed", "Pending"),
                        levels = c("Completed", "Pending")),
        stringsAsFactors = FALSE
      )
      
      df <- df[order(df$status), ]
      df$id        <- 1:ratio_total
      df$task_wrap <- stringr::str_wrap(df$task, width = 15)
      
      total_tasks      <- ratio_total
      completed_tasks  <- ratio_completed
      percent_complete <- (completed_tasks / total_tasks) * 100
      showCheckPoints  <- FALSE
    }
    
    
    if(style == 1){
      #Subway Style Bar
      track_color   <- "#e5e7eb"
      fill_color    <- "#10b981"
      fill_color_dk <- "#059669"
      
      p <- ggplot(df, aes(x = id, y = 0)) +
        # Background empty track
        annotate("segment", x = 0.5, xend = total_tasks + 0.5, y = 0, yend = 0,
                 color = track_color, linewidth = 14, lineend = "round")
      
      #rendering for empty bar
      if (completed_tasks > 0) {
        p <- p + annotate("segment", x = 0.5, xend = completed_tasks + 0.5, y = 0, yend = 0,
                          color = fill_color, linewidth = 14, lineend = "round")
      }
      
      p <- p +
        #rendered circles "checkpoints"
        geom_point(aes(fill = status), shape = 21, size = 7, color = "white", stroke = 2.2)
        
        if(showCheckPoints) {
          #grey pointer line
          p <- p + geom_segment(aes(x = id, xend = id, y = ifelse(id %% 2 == 1, 0.15, -0.15), yend = ifelse(id %% 2 == 1, 0.265, -0.265)),
                       color = "#9ca3af", linewidth = 0.6) +
            
            #using if else to stagger the text to be above and under bar 
            geom_text(aes(y = ifelse(id %% 2 == 1, 0.38, -0.38), label = task_wrap, color = status),
                      fontface = "bold", size = 4, vjust = 0.5, lineheight = 0.9)
        }
        
        
        #rendering percent based off total tasks to dynamically push it away from the bar itself
        p <- p + annotate("point", x = ifelse(total_tasks > 4, total_tasks * 1.25, total_tasks * 1.4), y = 0, size = 24, color = fill_color, alpha = 0.12) +
        annotate("text", x = ifelse(total_tasks > 4, total_tasks * 1.25, total_tasks * 1.4), y = 0, label = paste0(round(percent_complete), "%"),
                 fontface = "bold", size = 6, color = fill_color_dk) +
        
        scale_fill_manual(values = c("Completed" = fill_color, "Pending" = "white")) +
        scale_color_manual(values = c("Completed" = fill_color_dk, "Pending" = "#9ca3af")) +
        
        # xlim padding to keep things centered
        #consistent theme addition to keep all progress bars contained within their own consistent boxes
        coord_cartesian(ylim = c(-1, 1), xlim = c(-0.1, total_tasks + 2.7)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        
        labs(title = curr_tracker_name)
      
      
    } 
    else if(style == 2){
      #Segmented Donut Style
      
      df$val <- 1 # each task takes up an equal 1/n slice of the donut
      
      # Each task's slice spans [cum_start, cum_end] on a 0..total_tasks scale,
      # in the SAME order as the rows already appear in `df` (Completed tasks
      # first, then Pending -- this is what makes the ring's completed portion
      # one contiguous arc). Every layer below (ring, leader line, label) reads
      # its position from these two columns, so they can never disagree.
      df$cum_end   <- cumsum(df$val)
      df$cum_start <- df$cum_end - df$val
      df$mid_y     <- (df$cum_start + df$cum_end) / 2
      
      # frac_mid = how far around the donut (0 to 1) the MIDDLE of each task's
      # slice sits, purely to decide which side of the circle a label is on.
      df$frac_mid    <- df$mid_y / total_tasks
      
      # Labels on one half of the ring are left-aligned and on the other half
      # right-aligned, so the text always points outward away from the ring
      # instead of overlapping it.
      df$label_hjust <- ifelse(df$frac_mid < 0.5, 0, ifelse(df$frac_mid > 0.5, 1, 0.5))
      
      # --- Radius layout -----------------------------------------------------
      # Everything below is defined as a radius, working outward from the
      # center: ring -> small gap -> leader line -> label. When there are a
      # lot of tasks, adjacent slices can end up very close together in angle,
      # and if every label sat at the same radius those neighbors would
      # collide -- so in that case we alternate labels between a "near" and
      # "far" radius (see needs_stagger below), the same trick used for the
      # node labels in Style 1. With fewer tasks there's no crowding to solve,
      # so every label just uses the same consistent "near" radius.
      ring_center <- 2.6    # radius the ring itself is drawn at (bigger = bigger donut)
      ring_width  <- 1.15   # thickness of the ring
      ring_outer  <- ring_center + ring_width / 2
      
      leader_start <- ring_outer + 0.1    # connector line starts just outside the ring
      leader_end   <- leader_start + 0.4  # ...and points back to here
      
      label_near <- leader_end + 0.2   # "near" label radius
      label_far  <- label_near + 0.75  # "far" label radius (only used when staggering, see below)
      
     
      STAGGER_THRESHOLD <- 6
      needs_stagger <- total_tasks > STAGGER_THRESHOLD
      df$label_x <- if (needs_stagger) {
        ifelse(df$id %% 2 == 1, label_near, label_far)
      } else {
        label_near
      }
      
      outer_radius <- label_far + 1.3  # leaves room for the label text itself
      
      p <- ggplot(df) +
        # The ring itself, drawn with explicit ymin/ymax (= cum_start/cum_end)
        # rather than geom_col()+position_stack(), so every slice sits exactly
        # where mid_y says it does -- no separate stacking calculation that
        # could disagree with the leader lines or labels below.
        geom_rect(aes(xmin = ring_center - ring_width / 2, xmax = ring_center + ring_width / 2,
                      ymin = cum_start, ymax = cum_end, fill = status),
                  color = "white", linewidth = 2) +
        coord_polar(theta = "y", start = 0) +
        # This is a radius limit, not a left/right limit (it's a polar plot) --
        # sized to just clear the outermost ("far") labels.
        xlim(c(0, outer_radius)) +
        scale_fill_manual(values = c("Completed" = "#3b82f6", "Pending" = "#e5e7eb"))
        
        if(showCheckPoints){
          # Thin leader line pointing from the edge of the ring back out to each
          # label, drawn at that task's own mid_y -- guaranteed to point at the
          # correct slice since it's the same value used to draw the ring above.
          p <- p + geom_segment(aes(x = leader_start, xend = leader_end, y = mid_y, yend = mid_y),
                       color = "#9ca3af", linewidth = 0.6) +
            geom_text(aes(x = label_x, y = mid_y, label = task_wrap, color = status, hjust = label_hjust),
                      size = 3.3, fontface = "bold", lineheight = 0.9) +
            scale_color_manual(values = c("Completed" = "#1d4ed8", "Pending" = "#6b7280"))
        }
        
        
        
        # Center percentage text (x = 0 is the pole/center of the donut hole)
        p <- p + annotate("text", x = 0, y = 0, label = paste0(round(percent_complete), "%"),
                 size = 9, fontface = "bold", color = "#1f2937") +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
      
    } 
    else if(style == 3){
      #Thermometer Style
      tube_x <- 1.6
      
      p <- ggplot(df) +
        # Glass outline
        annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = total_tasks,
                 color = "#cbd5e1", linewidth = 9, lineend = "round") +
        # Empty tube interior
        annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = total_tasks,
                 color = "#f8fafc", linewidth = 6.4, lineend = "round")
      
      if (completed_tasks > 0) {
        p <- p + annotate("segment", x = tube_x, xend = tube_x, y = 0, yend = completed_tasks,
                          color = "#ef4444", linewidth = 6.4, lineend = "round")
      }
      
      p <- p +
        # Glass shine streak
        annotate("segment", x = tube_x - 0.13, xend = tube_x - 0.13, y = 0.5, yend = total_tasks - 0.3,
                 color = "white", alpha = 0.55, linewidth = 1.2, lineend = "round") +
        # Bulb
        annotate("point", x = tube_x, y = 0, size = 19, color = "#b91c1c") +
        annotate("point", x = tube_x, y = 0, size = 16, color = "#ef4444")
        
        if(showCheckPoints){
        # Tick marks
        p <- p + geom_segment(aes(x = tube_x + 0.55, xend = tube_x + 0.85, y = id - 0.5, yend = id - 0.5),
                     color = "#111827", linewidth = 0.9) +
        # Task labels
        geom_text(aes(x = tube_x + 1.0, y = id - 0.5, label = task_wrap, color = status),
                  hjust = 0, size = 3.8, fontface = "bold", lineheight = 0.9)
        }
      
        # Percent readout
        p <- p + annotate("point", x = tube_x - 1.15, y = total_tasks / 2, size = 28, color = "#ef4444", alpha = 0.10) +
        annotate("text", x = tube_x - 1.15, y = total_tasks / 2,
                 label = paste0(round(percent_complete), "%"),
                 fontface = "bold", size = 6, color = "#b91c1c") +
        scale_color_manual(values = c("Completed" = "#b91c1c", "Pending" = "#9ca3af")) +
        
        #xlim used to keep things centered via left and right padding
        coord_cartesian(xlim = c(tube_x - 2.1, tube_x + 4.7), ylim = c(-0.8, total_tasks + 0.8)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
      
    } 
    else if(style == 4) {
      #Battery Progress bar
      
      #rennders the colors for better
      battery_fill <- if (percent_complete <= 33) "#ef4444" else if (percent_complete <= 66) "#f59e0b" else "#10b981"
      battery_text <- if (percent_complete <= 33) "#b91c1c" else if (percent_complete <= 66) "#b45309" else "#047857"
      
      p <- ggplot(df) +
        # Outer Battery Case
        geom_rect(aes(xmin = 0.2, xmax = total_tasks + 0.8, ymin = -0.6, ymax = 0.6), 
                  fill = NA, color = "#374151", linewidth = 1.2) +
        # Battery Terminal Nub
        geom_rect(aes(xmin = total_tasks + 0.8, xmax = total_tasks + 1.1, ymin = -0.25, ymax = 0.25), 
                  fill = "#374151", color = "#374151") +
        # Internal Charge Segments
        geom_rect(aes(xmin = id - 0.6, xmax = id + 0.4, ymin = -0.48, ymax = 0.48, fill = status), 
                  color = "white", linewidth = 1)
      
        if(showCheckPoints){
        # Staggered Connecting Lines (odd segments connect upward, even ones downward)
        p <- p + geom_segment(aes(x = id - 0.1, xend = id - 0.1, 
                         y = ifelse(id %% 2 == 1, 0.7, -0.7), 
                         yend = ifelse(id %% 2 == 1, 1.2, -1.2)), color = "#9ca3af") +
        # Staggered Task Labels
        geom_text(aes(x = id - 0.1, y = ifelse(id %% 2 == 1, 1.55, -1.55), 
                      label = task_wrap, color = status), 
                  size = 4, fontface = "bold", lineheight = 0.9)
        }
      
        # Charge readout
        p <- p + annotate("text", x = total_tasks + 2.0, y = 0.55, label = "⚡", size = 5) +
        annotate("text", x = total_tasks + 2.0, y = 0, label = paste0(round(percent_complete), "%"), 
                 size = 6, fontface = "bold", color = battery_text) +
        scale_fill_manual(values = c("Completed" = battery_fill, "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = battery_text, "Pending" = "#6b7280")) +
        
        #more centering logic for left right padding
        coord_cartesian(ylim = c(-2.2, 2.2), xlim = c(-0.2, total_tasks + 2.8)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
    } 
    else if(style == 5){
      #Stepping Progress bar
      
      p <- ggplot(df) +
        # Pending Path
        geom_step(aes(x = id - 0.5, y = id), color = "#e5e7eb", linewidth = 2, direction = "vh") +
        # Completed Path
        geom_step(data = subset(df, id <= completed_tasks), 
                  aes(x = id - 0.5, y = id), color = "#f59e0b", linewidth = 2.5, direction = "vh") +
        # Step Nodes
        geom_point(aes(x = id - 0.5, y = id, fill = status), 
                   shape = 21, size = 4.5, color = "white", stroke = 1.5) +
        # Summit Flag
        annotate("text", x = total_tasks + 0.1, y = total_tasks + 0.6, label = "🚩", size = 6)
        
        if(showCheckPoints){
        # Dotted Line Connectors
        p <- p +geom_segment(aes(x = id - 0.5, xend = id + 0.45, y = id, yend = id), 
                     color = "#9ca3af", linewidth = 0.9, linetype = "dotted") +
        # Task Labels
        geom_text(aes(x = id + 0.55, y = id, label = task_wrap, color = status), 
                  hjust = 0, size = 6, fontface = "bold", lineheight = 0.9)
        }
      
        # Percent complete
        p <- p + annotate("point", x = 1, y = total_tasks , size = 26, color = "#f59e0b", alpha = 0.12) +
        annotate("text", x = 1, y = total_tasks, 
                 label = paste0(round(percent_complete), "%"),
                 size = 6, fontface = "bold", color = "#b45309") +
        scale_fill_manual(values = c("Completed" = "#f59e0b", "Pending" = "#d1d5db")) +
        scale_color_manual(values = c("Completed" = "#b45309", "Pending" = "#9ca3af")) +
        
        # Right-hand padding bumped slightly (3 -> 3.3) so it mirrors the 0.5-unit
        # gap left in front of the first step, instead of hugging the labels
        coord_cartesian(xlim = c(0, total_tasks + 3.3), ylim = c(0.5, total_tasks + 1.2)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
      
    } 
    else if(style == 6) {
      #Style 6, Half Throttle
      df$seg_start   <- df$id - 1
      df$seg_end     <- df$id
      
      #same idea as the donut labels above: point each task's label outward
      # away from the dial instead of letting it overlap the ring.
      df$label_hjust <- ifelse(df$id / total_tasks < 0.5, 1, ifelse(df$id / total_tasks > 0.5, 0, 0.5))
      
      # The needle rests at the boundary between the last completed task and
      # the first pending one; if nothing is done yet it just sits at 0.
      needle_y  <- if (completed_tasks > 0) completed_tasks - 0.5 else 0
      percent_y <- 1.5 * total_tasks
      
      
      p <- ggplot(df) +
        # Task slices
        geom_rect(aes(xmin = 1.0, xmax = 3.0, ymin = seg_start, ymax = seg_end, fill = status), 
                  color = "white", linewidth = 1)
      
        if(showCheckPoints){
        # Task labels
        p <- p + geom_text(aes(x = 3.3, y = id - 0.5, label = task_wrap, color = status, hjust = label_hjust), 
                  size = 3, fontface = "bold", lineheight = 0.9)
        }
        
        # Needle hub + pointer
        p <- p + annotate("point", x = 0, y = 0, size = 9, color = "#1f2937") +
        annotate("segment", x = 0.15, xend = 2.3, y = needle_y, yend = needle_y, 
                 color = "#1f2937", linewidth = 1.4, lineend = "round") +
        
        # Percent readout
        annotate("text", x = 0.9, y = percent_y, label = paste0(round(percent_complete), "%"), 
                 size = 8, fontface = "bold", color = "#c2410c") +
        scale_fill_manual(values = c("Completed" = "#f97316", "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = "#c2410c", "Pending" = "#6b7280")) +
        coord_polar(theta = "y", start = -pi / 2) +
        ylim(c(0, 2 * total_tasks)) +
        
        #sizing options
        xlim(c(0, 4.7)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(2, 8, 0, 8),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        labs(title = curr_tracker_name)
    }
    else if(style == 7) {
      #Spider spoke wheel (non linear style)
      
      set.seed(sum(utf8ToInt(curr_tracker_name))) #seeding to keep the same ordering of nodes for every run of the code for each tracker
      
      spoke_full  <- 2.9   # how far a Completed spoke reaches
      spoke_short <- 1.5   # how far a Pending spoke reaches
      
      
      cone_half_width_deg <- 18 #cone deadzone
      allowed_start_deg   <- 270 + cone_half_width_deg   # just past the cone's right edge, where to start rendering
      allowed_span_deg    <- 360 - 2 * cone_half_width_deg #allowed radius with cone deadzone
      
      # spoke_order[k] = which of the n evenly-spaced slots (1..n) row k's task
      # gets. Since slot numbers increase in angular order by construction, this
      # SAME vector doubles as an angular-adjacency index for the label
      # staggering below -- no separate column needed.
      spoke_order <- sample(seq_len(total_tasks))
      df$spoke_angle <- (allowed_start_deg + (spoke_order - 0.5) / total_tasks * allowed_span_deg) * pi / 180
      
      df$spoke_len <- ifelse(df$status == "Completed", spoke_full, spoke_short)
      df$node_x <- df$spoke_len * cos(df$spoke_angle)
      df$node_y <- df$spoke_len * sin(df$spoke_angle)
      df$label_hjust <- ifelse(df$node_x < 0, 1, 0)
      
      #dyanmic rendering for completed spokes being thicker
      df$spoke_width <- ifelse(df$status == "Completed", 1.9, 1)
      
  
      STAGGER_THRESHOLD <- 8   # past this many tasks, alternate near/far so tight angular neighbors don't collide
      label_gap_near <- 0.42 #displacment for near nodes
      label_gap_far  <- label_gap_near + 0.55 #displacement for far nodes
      
      #if we have more than stagger_trheshold nodes, we start the staggering process
      df$label_gap <- if (total_tasks > STAGGER_THRESHOLD) {
        ifelse(spoke_order %% 2 == 0, label_gap_far, label_gap_near)
      } else {
        label_gap_near
      }
      
      #spoke setting logic
      df$label_x <- df$node_x + cos(df$spoke_angle) * df$label_gap
      df$label_y <- df$node_y + sin(df$spoke_angle) * df$label_gap
      
      #adjustable distance for rendering the %complete graphic, percent_extra pushes it that amount below the last filled spoke for no chance of collision
      percent_extra <- 1.3
      percent_y <- -(spoke_full + percent_extra)
      
      p <- ggplot(df) +
        geom_segment(aes(x = 0, y = 0, xend = node_x, yend = node_y, color = status,
                         linewidth = I(spoke_width))) + 
        geom_point(aes(x = node_x, y = node_y, fill = status), shape = 21, size = 8,
                   color = "white", stroke = 1.6)
      
        if(showCheckPoints){
        p <- p  +  geom_text(aes(x = label_x, y = label_y, label = task_wrap,
                      color = status, hjust = label_hjust),
                  size = 3.2, fontface = "bold", lineheight = 0.85)
        }
      
        #HUB
        p <- p + annotate("point", x = 0, y = 0, size = 13, color = "#1f2937") +
        annotate("text", x = 0, y = percent_y, label = paste0(round(percent_complete), "%"),
                 size = 8, fontface = "bold", color = "#111827") +
        scale_fill_manual(values = c("Completed" = "#6366f1", "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = "#4338ca", "Pending" = "#9ca3af")) +
        
        #bottom limited tied to percent_y so there will never be a chance of collision assuming regular length of task names
        coord_fixed(ratio = 1, xlim = c(-spoke_full - 2.3, spoke_full + 2.3),
                    ylim = c(percent_y - 0.6, spoke_full + 1.6)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        
        labs(title = curr_tracker_name)
      
    }
    else{
      #Orbit Ring Non-Linear Style
      
      set.seed(sum(utf8ToInt(curr_tracker_name))) #seeding again for consistency across different code runs
      
      ring_inner_r <- ifelse(total_tasks > 7, 2.6, 1.8)   #completed inner ring
      ring_outer_r <- ifelse(total_tasks > 7, 4.0, 2.7)   #pending outer ring
      
      #master function logic for placing the dots on the orbits, radius is either the inner ring our outer ring, and start angle is adjusted so there will be no inner
      #outer collision
      #logic is just reused with different variables to save code and allow for a more modifyable placment sequence
      place_on_ring <- function(sub_df, radius, start_angle) {
        if (nrow(sub_df) == 0) return(sub_df)
        sub_df <- sub_df[sample(nrow(sub_df)), ]
        n <- nrow(sub_df)
        sub_df$ring_radius <- radius
        sub_df$ring_order  <- seq_len(n)   # this ring's own 1..n index, used for label staggering below
        sub_df$angle        <- start_angle + seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
        sub_df$orbit_x <- radius * cos(sub_df$angle)
        sub_df$orbit_y <- radius * sin(sub_df$angle)
        sub_df
      }
      
      #actual placing using completed and pending constructs as previously mentione
      df <- rbind(
        place_on_ring(df[df$status == "Completed", ], ring_inner_r, start_angle = 0),
        place_on_ring(df[df$status == "Pending", ],   ring_outer_r, start_angle = pi / 2)
      )
      
      #each label rendered away from the center based on where it is located ring wise
      df$label_hjust <- ifelse(df$orbit_x < 0, 1, 0)
      
      #completes are bolded and pends are just regular text (small change)
      df$label_face <- ifelse(df$status == "Completed", "bold", "plain")
      
      #adding classic stagger threshold logic to provide collisons between nodes on the SAME radius, many variables here to allow for easy edits
      STAGGER_THRESHOLD <- 5
      tick_gap   <- 0.18   #gap from the ring out to where the leader line starts
      leader_len <- 0.35   #length of the leader line itself
      label_near <- tick_gap + leader_len + 0.15
      label_far  <- label_near + 0.55
      
      #actual logic of the labels to apply thresholding idea
      ring_counts <- table(df$status)
      df$ring_task_count <- as.integer(ring_counts[df$status])
      df$label_offset <- ifelse(df$ring_task_count > STAGGER_THRESHOLD & df$ring_order %% 2 == 0,
                                label_far, label_near)
      
      #logic to keep the nodes exactly on their rings by having an imaginary "spoke" going from the center
      df$leader_end_x <- (df$ring_radius + tick_gap) * cos(df$angle)
      df$leader_end_y <- (df$ring_radius + tick_gap) * sin(df$angle)
      df$label_x <- (df$ring_radius + df$label_offset) * cos(df$angle)
      df$label_y <- (df$ring_radius + df$label_offset) * sin(df$angle)
      
      #thin reference circle for each ring, traced out as a 100-point path because theres no way to draw a circle with one command in ggplot2
      ring_path <- function(radius) {
        a <- seq(0, 2 * pi, length.out = 100)
        data.frame(x = radius * cos(a), y = radius * sin(a))
      }
      
      p <- ggplot(df, aes(x = orbit_x, y = orbit_y)) +
        geom_path(data = ring_path(ring_inner_r), aes(x = x, y = y), inherit.aes = FALSE,
                  color = "#e5e7eb", linewidth = 0.6) +
        geom_path(data = ring_path(ring_outer_r), aes(x = x, y = y), inherit.aes = FALSE,
                  color = "#e5e7eb", linewidth = 0.6) +
        
        #leader line from each node out to its label + the ball itself
        geom_segment(aes(xend = leader_end_x, yend = leader_end_y), color = "#9ca3af", linewidth = 0.5)+ 
        geom_point(aes(fill = status), shape = 21, size = 6, color = "white", stroke = 1.6)
      
        if(showCheckPoints) {
        p <- p +  geom_text(aes(x = label_x, y = label_y, label = task_wrap, color = status,
                      hjust = label_hjust, fontface = I(label_face)),
                  size = 3.2, lineheight = 0.85)
        }
        
        # Percent readout
        p <- p + annotate("point", x = 0, y = 0, size = 22, color = "#f97316") +
        annotate("text", x = 0, y = 0, label = paste0(round(percent_complete), "%"),
                 size = 5.5, fontface = "bold", color = "white") +
        scale_fill_manual(values = c("Completed" = "#ec4899", "Pending" = "#e5e7eb")) +
        scale_color_manual(values = c("Completed" = "#be185d", "Pending" = "#6b7280")) +
        
        #padding sized generously for the bigger label font + the "far" stagger distance, so long wrapped labels don't clip at the panel edge.
        coord_fixed(ratio = 1, xlim = c(-ring_outer_r - 2.6, ring_outer_r + 2.6),
                    ylim = c(-ring_outer_r - 1.8, ring_outer_r + 1.8)) +
        theme_void(base_size = 13) +
        theme(
          plot.background  = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          plot.margin      = margin(24, 30, 22, 30),
          plot.title       = element_text(face = "bold", size = 17, hjust = 0.5,
                                          color = "#111827", margin = margin(b = 6)),
          legend.position  = "none"
        ) +
        
        labs(title = curr_tracker_name)
    }
    
    #print and then use ggplot to draw a thin rectangle below to seperate
    print(p)
    grid::grid.rect(
      x = 0.5, y = 0.012, width = .94, height = 0.007,
      gp = grid::gpar(fill = "black", col = NA)
    )
    
  }
}

