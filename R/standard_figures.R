
#' DSMB Consort Diagram
#'
#' @description This function visualizes the categorical percentages of baseline characteristics sex, age, race, education, and military
#'
#' @param analytic This is the analytic data set that must include screened, eligible, 
#' consented, randomized, discontinued, ineligible, refused, late_ineligible, and the meta construct columns
#' @param active is a meta construct that is required
#' @param completed is a meta construct that is required
#' @param not_enrolled_other is a meta construct that is required
#' @param completed_str is the text for the completed box that defaults to 'Completed 12-month visit'
#'
#' @return nothing
#' @export
#'
#' @examples
#' \dontrun{
#' dsmb_consort_diagram()
#' }
dsmb_consort_diagram <- function(analytic, active, completed, not_enrolled_other, completed_str="Completed 12-month visit"){
  
  analytic <- analytic %>% 
    filter(screened == TRUE) 
  
  Screened <- sum(analytic$screened, na.rm=TRUE)
  Eligible <- sum(analytic$eligible, na.rm=TRUE)
  Consented <- sum(analytic$consented, na.rm=TRUE)
  Randomized <- sum(analytic$randomized, na.rm=TRUE)
  Active <- sum(analytic[[active]], na.rm=TRUE)
  Discontinued <- sum(analytic$discontinued, na.rm=TRUE)
  Completed <- sum(analytic[[completed]], na.rm=TRUE)
  Ineligible <- sum(analytic$ineligible, na.rm=TRUE)
  Refused <- sum(analytic$refused, na.rm=TRUE)
  Late_Ineligible <- sum(analytic$late_ineligible, na.rm=TRUE)
  Not_Enrolled_Other <- sum(analytic[[not_enrolled_other]], na.rm=TRUE)
  
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
  
  return(htmltools::HTML(export_svg(consort_diagram)))
}