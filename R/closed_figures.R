#' Consort Diagram: publication
#'
#' @description 
#' The closed version of consort_diagram_wb_publication, breaking down study cancellations by treatment_arm.
#' 
#' This consort diagram was made for the Weight Bearing study, and so is unlikely to work for yours.
#' 
#' @param analytic analytic data set that must include 
#' study_id, screened, ineligible, ineligibility_reasons, refused, constraint_other, constraint_other_txt, consented, 
#' discontinued_pre_randomization, injury_type, randomized, 
#' late_ineligible, per_protocol_sample, enrolled, consent_date, death_date, withdraw_date,
#' preinjury_work_status, treatment_arm
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' 
closed_consort_diagram_wb_publication <- function(analytic){
  
  confirm_stability_of_related_visual('consort_diagram_wb_publication', '93bd45f86181f8f3c18967dac324b6fd')
  
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened", "ineligible", "ineligibility_reasons", "refused", "constraint_unavailable",
                           "constraint_other", "constraint_other_txt", "constraint_unavailable", "constraint_surgeon_unwilling",
                           "consented", "discontinued_pre_randomization", "received_treatment",
                           "injury_type", "randomized", "late_ineligible", "per_protocol_sample", "enrolled", 
                           "consent_date", "death_date", "withdraw_date", "preinjury_work_status", "followup_expected_12mo",
                           "completed", "outcome_data", "treatment_arm"),
    example_types = c("Boolean", "Boolean", "Category-NS", "Boolean", "Boolean", "Boolean", "Character",
                      "Boolean", "Boolean", "NamedCategory['ankle' 'plateau']", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Date", "Date", "Date", "Boolean", "Boolean", "Boolean",
                      "(';', ',')NamedCategory['returned_to_work' 'admission_for_complication']|Number|Number|Date|NamedCategory['event' 'check']|Number|Number|Date",
                      "TreatmentArm"))
  
  df <- analytic %>% 
    select(study_id, screened, ineligible, ineligibility_reasons, refused, constraint_other, constraint_other_txt, 
           constraint_unavailable, constraint_surgeon_unwilling, consented, discontinued_pre_randomization, received_treatment,
           injury_type, randomized, late_ineligible, per_protocol_sample, enrolled, consent_date, death_date, 
           withdraw_date, preinjury_work_status, followup_expected_12mo, completed, outcome_data, treatment_arm)
  
  
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
  constraint <- sum(df$constraint_other, na.rm = TRUE)
  constraint_unavailable <- sum(analytic$constraint_unavailable, na.rm = TRUE)
  constraint_surgeon_unwilling <- sum(analytic$constraint_unavailable, na.rm = TRUE)
  
  late_discontinuation <- sum(df$discontinued_pre_randomization & 
                                df$consented, na.rm = TRUE)
  
  plateau_injuries <- sum(df$injury_type=='plateau', na.rm = TRUE)
  randomized <- sum(df$injury_type=='ankle', na.rm = TRUE)
  
  randomized_a <- sum(df$treatment_arm == 'Group A' & df$injury_type=='ankle', na.rm = TRUE)
  randomized_b <- sum(df$treatment_arm == 'Group B' & df$injury_type=='ankle', na.rm = TRUE)
  
  df_a <- df %>% filter(treatment_arm == 'Group A')
  df_b <- df %>% filter(treatment_arm == 'Group B')
  
  dnr_treatment_df_a <- df_a %>% filter(injury_type == 'ankle')
  dnr_treatment_a <- sum(!dnr_treatment_df_a$received_treatment, na.rm = TRUE)
  dnr_treatment_df_b <- df_b %>% filter(injury_type == 'ankle')
  dnr_treatment_b <- sum(!dnr_treatment_df_b$received_treatment, na.rm = TRUE)
  
  late_ineligible_a <- sum(df_a$late_ineligible, na.rm = TRUE)
  late_ineligible_b <- sum(df_b$late_ineligible, na.rm = TRUE)
  
  diverging_review_a <- sum(!df_a$per_protocol_sample, na.rm = TRUE)
  diverging_review_b <- sum(!df_b$per_protocol_sample, na.rm = TRUE)
  
  died_a <- sum(as.Date(df_a$death_date)-as.Date(df_a$consent_date)<365, na.rm = TRUE)
  died_b <- sum(as.Date(df_b$death_date)-as.Date(df_b$consent_date)<365, na.rm = TRUE)
  
  withdrew_a <- sum(as.Date(df_a$withdraw_date)-as.Date(df_a$consent_date)<365, na.rm = TRUE)
  withdrew_b <- sum(as.Date(df_b$withdraw_date)-as.Date(df_b$consent_date)<365, na.rm = TRUE)
  
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
  
  outcome_extracted_a <- extract_outcome_expected(df_a)
  outcome_extracted_b <- extract_outcome_expected(df_b)
  
  afc_expected_a <- outcome_extracted_a %>% filter(outcome_name == 'admission_for_complication') %>% pull(pct_expected)
  rtw_expected_a <- outcome_extracted_a %>% filter(outcome_name == 'returned_to_work') %>% pull(pct_expected)
  
  afc_expected_b <- outcome_extracted_b %>% filter(outcome_name == 'admission_for_complication') %>% pull(pct_expected)
  rtw_expected_b <- outcome_extracted_b %>% filter(outcome_name == 'returned_to_work') %>% pull(pct_expected)

  #adding spaces to variables themselves
  top_reasons_count <- top_reasons_count %>%
    mutate(n = as.character(n)) %>%
    mutate(n = paste0('   ', n))
  died_a <- paste0('   ', as.character(died_a))
  withdrew_a <- paste0('   ', as.character(withdrew_a))  
  died_b <- paste0('   ', as.character(died_b))
  withdrew_b <- paste0('   ', as.character(withdrew_b))  
  
  dnr_treatment_a_chr <- paste0('   ', as.character(dnr_treatment_a))
  late_ineligible_a_chr <- paste0('   ', as.character(late_ineligible_a))  
  diverging_review_a_chr <- paste0('   ', as.character(diverging_review_a))
  
  dnr_treatment_b_chr <- paste0('   ', as.character(dnr_treatment_b))
  late_ineligible_b_chr <- paste0('   ', as.character(late_ineligible_b))  
  diverging_review_b_chr <- paste0('   ', as.character(diverging_review_b))
  
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
          <TR><TD ALIGN="LEFT">', top_reasons_count$n[1], ' ', top_reasons_count$ineligibility_reasons[1], '</TD></TR>
          <TR><TD ALIGN="LEFT">', top_reasons_count$n[2], ' ', top_reasons_count$ineligibility_reasons[2], '</TD></TR>
          <TR><TD ALIGN="LEFT">', top_reasons_count$n[3], ' ', top_reasons_count$ineligibility_reasons[3], '</TD></TR>
          <TR><TD ALIGN="LEFT">', top_reasons_count$n[4], ' ', top_reasons_count$ineligibility_reasons[4], '</TD></TR>
          <TR><TD ALIGN="LEFT">', top_reasons_count$n[5], ' ', top_reasons_count$ineligibility_reasons[5], '</TD></TR>
          <TR><TD ALIGN="LEFT">', top_reasons_count$n[6], ' ', top_reasons_count$ineligibility_reasons[6], '</TD></TR>
          <TR><TD ALIGN="LEFT">', top_reasons_count$n[7], ' ', top_reasons_count$ineligibility_reasons[7], '</TD></TR>          
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
        
      box2 [style="rounded,filled", fillcolor="#a4d3ee", pos="-0.25,-0.75!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', randomized_a, ' Assigned to early weight bearing</TD></TR>
            <TR><TD ALIGN="LEFT">', dnr_treatment_a_chr, ' Randomized, did not receive treatment</TD></TR>
            <TR><TD ALIGN="LEFT">', late_ineligible_a_chr, ' Late ineligible</TD></TR>
            <TR><TD ALIGN="LEFT">', diverging_review_a_chr, ' Weight bearing instructions review diverged</TD></TR>
            <TR><TD ALIGN="LEFT">from protocol</TD></TR>
            <TR><TD ALIGN="LEFT">', randomized_a-dnr_treatment_a-late_ineligible_a-diverging_review_a, ' Included in primary analysis</TD></TR>
            <TR><TD ALIGN="LEFT">', died_a, ' Died prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">', withdrew_a, ' Withdrew prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">', afc_expected_a, ' Admitted for complication out of expected</TD></TR>
            <TR><TD ALIGN="LEFT">', rtw_expected_a, ' Returned to work out of expected</TD></TR>
          </TABLE>
        >];
          
      box3 [style="rounded,filled", fillcolor="#a4d3ee", pos="4.25,-0.75!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', randomized_b, ' Assigned to delayed weight bearing</TD></TR>
            <TR><TD ALIGN="LEFT">', dnr_treatment_b_chr, ' Randomized, did not receive treatment</TD></TR>
            <TR><TD ALIGN="LEFT">', late_ineligible_b_chr, ' Late ineligible</TD></TR>
            <TR><TD ALIGN="LEFT">', diverging_review_b_chr, ' Weight bearing instructions review diverged</TD></TR>
            <TR><TD ALIGN="LEFT">from protocol</TD></TR>
            <TR><TD ALIGN="LEFT">', randomized_b-dnr_treatment_b-late_ineligible_b-diverging_review_b, ' Included in primary analysis</TD></TR>
            <TR><TD ALIGN="LEFT">', died_b, ' Died prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">', withdrew_b, ' Withdrew prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">', afc_expected_b, ' Admitted for complication out of expected</TD></TR>
            <TR><TD ALIGN="LEFT">', rtw_expected_b, ' Returned to work out of expected</TD></TR>
          </TABLE>
        >]
        
      midpoint [style=invis, pos="1.35,3.125!, width=0, height=0"]
      
      # Relationships
      title -> title2
      midpoint -> box1
      title2 -> box2
      title2 -> box3
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