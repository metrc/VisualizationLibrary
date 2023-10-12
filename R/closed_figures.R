
#' Closed DSMB Consort Diagram
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
#' closed_dsmb_consort_diagram()
#' }
closed_dsmb_consort_diagram <- function(analytic, not_enrolled_other=NULL, completed_str="Completed 12-month visit"){
  
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