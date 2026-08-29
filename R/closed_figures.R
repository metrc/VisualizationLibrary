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
#' closed_consort_diagram_wb_publication("Replace with Analytic Tibble")
#' 
closed_consort_diagram_wb_publication <- function(analytic){
  
  confirm_stability_of_related_visual('consort_diagram_wb_publication', '6074dfb751632d6f30b09e682ff34ce5')
  
  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened", "ineligible", "ineligibility_reasons", "refused", "constraint_unavailable", "constraint_issue",
                           "constraint_other", "constraint_other_txt", "constraint_unavailable", "constraint_surgeon_unwilling",
                           "consented", "discontinued_pre_randomization", "received_treatment",
                           "injury_type", "randomized", "late_ineligible", "per_protocol_sample", "enrolled", "not_consented",
                           "consent_date", "death_date", "withdraw_date", "preinjury_work_status", "followup_expected_12mo",
                           "completed", "outcome_data", "treatment_arm"),
    example_types = c("Boolean", "Boolean", "Category-NS", "Boolean", "Boolean", "Boolean", "Boolean", "Character",
                      "Boolean", "Boolean", "NamedCategory['ankle' 'plateau']", "Boolean", "Boolean", 
                      "Boolean", "Boolean", "Boolean", "Date", "Date", "Date", "Boolean", "Boolean", "Boolean",
                      "(';', ',')NamedCategory['returned_to_work' 'admission_for_complication']|Number|Number|Date|NamedCategory['event' 'check']|Number|Number|Date",
                      "TreatmentArm"))
  
  df <- analytic %>% 
    select(study_id, screened, ineligible, ineligibility_reasons, refused, constraint_issue, constraint_other, constraint_other_txt, 
           constraint_unavailable, constraint_surgeon_unwilling, consented, discontinued_pre_randomization, received_treatment,
           injury_type, randomized, late_ineligible, per_protocol_sample, enrolled, consent_date, death_date, not_consented,
           withdraw_date, preinjury_work_status, followup_expected_12mo, completed, outcome_data, treatment_arm) %>%
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
  constraint <- sum(df$constraint_other,  na.rm = TRUE) +
    sum(df$constraint_issue, na.rm = TRUE)
  constraint_unavailable <- sum(df$constraint_unavailable & (is.na(df$constraint_other)|!df$constraint_other), na.rm = TRUE)
  constraint_surgeon_unwilling <- sum(df$constraint_surgeon_unwilling & (is.na(df$constraint_other)|!df$constraint_other)& (is.na(df$constraint_unavailable)|!df$constraint_unavailable), na.rm = TRUE)
  
  late_discontinuation <- sum(df$discontinued_pre_randomization & 
                                df$consented, na.rm = TRUE)
  
  plateau_injuries <- sum(df$injury_type=='plateau', na.rm = TRUE)
  randomized <- sum((df$injury_type == "ankle" | is.na(df$injury_type)) &
                      df$randomized,
                    na.rm = TRUE)
  
  accounted_ids <- df %>% filter(ineligible|refused|constraint_other|constraint_unavailable|
                                   constraint_surgeon_unwilling|(discontinued_pre_randomization & consented)|
                                   injury_type=='plateau'|(injury_type=='ankle' & randomized)) %>% pull(study_id)
  
  not_consented <- sum(df %>% filter(!study_id %in% accounted_ids) %>% pull(not_consented), na.rm = TRUE)
  
  
  
  randomized_a <- sum(df$treatment_arm == "Group A" &
                        (df$injury_type == "ankle" | is.na(df$injury_type)) &
                        df$randomized,
                      na.rm = TRUE)
  
  randomized_b <- sum(df$treatment_arm == "Group B" &
                        (df$injury_type == "ankle" | is.na(df$injury_type)) &
                        df$randomized,
                      na.rm = TRUE)
  
  df_a <- df %>% filter(treatment_arm == 'Group A')
  df_b <- df %>% filter(treatment_arm == 'Group B')
  
  dnr_treatment_df_a <- df_a %>% filter(injury_type == 'ankle'|is.na(injury_type)) %>% filter(randomized)
  dnr_treatment_a <- sum(!dnr_treatment_df_a$received_treatment, na.rm = TRUE)
  dnr_treatment_df_b <- df_b %>% filter(injury_type == 'ankle'|is.na(injury_type)) %>% filter(randomized)
  dnr_treatment_b <- sum(!dnr_treatment_df_b$received_treatment, na.rm = TRUE)
  
  late_ineligible_a <- sum(df_a$late_ineligible &
                             (df_a$injury_type == "ankle" | is.na(df_a$injury_type)) &
                             df_a$randomized,
                           na.rm = TRUE)
  
  late_ineligible_b <- sum(df_b$late_ineligible &
                             (df_b$injury_type == "ankle" | is.na(df_b$injury_type)) &
                             df_b$randomized,
                           na.rm = TRUE)
  
  diverging_review_a <- sum((df_a$injury_type == "ankle" | is.na(df_a$injury_type)) &
                              df_a$randomized &
                              (!df_a$late_ineligible | is.na(df_a$late_ineligible)) &
                              !df_a$per_protocol_sample,
                            na.rm = TRUE)
  
  diverging_review_b <- sum((df_b$injury_type == "ankle" | is.na(df_b$injury_type)) &
                              df_b$randomized &
                              (!df_b$late_ineligible | is.na(df_b$late_ineligible)) &
                              !df_b$per_protocol_sample,
                            na.rm = TRUE)
  
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
        
      box2 [style="rounded,filled", fillcolor="#a4d3ee", pos="-0.25,-0.75!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', randomized_a, ' Assigned to early weight bearing</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', late_ineligible_a, ' Late ineligible</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', diverging_review_a, ' Weight bearing instructions review diverged</TD></TR>
            <TR><TD ALIGN="LEFT">from protocol</TD></TR>
            <TR><TD ALIGN="LEFT">', randomized_a-late_ineligible_a-diverging_review_a, ' Included in primary analysis</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', died_a, ' Died prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', withdrew_a, ' Withdrew prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">', afc_expected_a, ' Admitted for complication out of expected</TD></TR>
            <TR><TD ALIGN="LEFT">', rtw_expected_a, ' Returned to work out of expected</TD></TR>
          </TABLE>
        >];
          
      box3 [style="rounded,filled", fillcolor="#a4d3ee", pos="4.25,-0.75!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', randomized_b, ' Assigned to delayed weight bearing</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', late_ineligible_b, ' Late ineligible</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', diverging_review_b, ' Weight bearing instructions review diverged</TD></TR>
            <TR><TD ALIGN="LEFT">from protocol</TD></TR>
            <TR><TD ALIGN="LEFT">', randomized_b-late_ineligible_b-diverging_review_b, ' Included in primary analysis</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', died_b, ' Died prior to 365 days</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', withdrew_b, ' Withdrew prior to 365 days</TD></TR>
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

#' NSAID Consort Diagram: publication
#'
#' @description
#' The closed version of consort_diagram_nsaid_publication, breaking down the flow of
#' patients after randomization by treatment_arm.
#'
#' This consort diagram was made for the NSAID study, and so is unlikely to work for yours.
#'
#' @param analytic analytic data set that must include
#' study_id, screened, eligible, ineligibility_reasons, refused, not_consented, consented,
#' constraint_48hrs, constraint_admin, constraint_noconsent, constraint_other,
#' nonparticipation_other_study_coenrolled, nonparticipation_other_reason, randomized,
#' adjudicated_inappropriate_enrollment, adjudicated_late_ineligible, adjudicated_late_refusal,
#' adjudicated_physician_withdrawn, df_surg_start_date, surgery_or_healed_type, surgery_or_healed_days,
#' crossover, treatment_arm
#' @param outcome_day day at which the primary outcome status is assessed, defaults to 365
#' @param arm_a_str label for the Group A treatment arm, defaults to "Group A"
#' @param arm_b_str label for the Group B treatment arm, defaults to "Group B"
#'
#' @return An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
#' @export
#'
#' @examples
#' closed_consort_diagram_nsaid_publication("Replace with Analytic Tibble")
#'
closed_consort_diagram_nsaid_publication <- function(analytic, outcome_day=365, arm_a_str="Group A", arm_b_str="Group B"){

  confirm_stability_of_related_visual('consort_diagram_nsaid_publication', '6072312f625d86a6a45c170bd0b9426e')

  analytic <- if_needed_generate_example_data(
    analytic,
    example_constructs = c("screened", "eligible", "ineligibility_reasons", "refused", "not_consented",
                           "consented", "constraint_48hrs", "constraint_admin", "constraint_noconsent",
                           "constraint_other", "nonparticipation_other_study_coenrolled", "nonparticipation_other_reason",
                           "randomized", "adjudicated_inappropriate_enrollment", "adjudicated_late_ineligible",
                           "adjudicated_late_refusal", "adjudicated_physician_withdrawn", "df_surg_start_date",
                           "surgery_or_healed_type", "surgery_or_healed_days", "crossover", "adherent", "treatment_arm",
                           "primary_entry_day",
                           "dead", "withdrawn_consent", "not_completed_reason",
                           "bpi_severity_score_3mo", "bpi_interference_score_3mo",
                           "opioid_days_baseline", "opioid_days_3mo", "opioid_days_6mo", "opioid_days_12mo"),
    example_types = c("Boolean", "Boolean", "Category-NS", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Boolean",
                      "Boolean", "Boolean", "Date",
                      "NamedCategory['check' 'favorable_event' 'unfavorable_event']", "Number-U365", "Boolean", "Boolean", "TreatmentArm",
                      "Number",
                      "Boolean", "Boolean", "NamedCategory['Unreachable' 'Other']",
                      "Number", "Number",
                      "Number", "Number", "Number", "Number"))

  df <- analytic %>%
    select(study_id, screened, eligible, ineligibility_reasons, refused, not_consented, consented,
           constraint_48hrs, constraint_admin, constraint_noconsent, constraint_other,
           nonparticipation_other_study_coenrolled, nonparticipation_other_reason,
           randomized, adjudicated_inappropriate_enrollment, adjudicated_late_ineligible,
           adjudicated_late_refusal, adjudicated_physician_withdrawn, df_surg_start_date,
           surgery_or_healed_type, surgery_or_healed_days, crossover, any_of("adherent"), treatment_arm,
           primary_entry_day,
           dead, withdrawn_consent, not_completed_reason,
           bpi_severity_score_3mo, bpi_interference_score_3mo,
           opioid_days_baseline, opioid_days_3mo, opioid_days_6mo, opioid_days_12mo) %>%
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

  other_count <- sum(df$screened, na.rm = TRUE) - sum(df$eligible, na.rm = TRUE) - sum(top_reasons_count$n)

  other_row <- tibble(
    ineligibility_reasons = 'Had other reasons',
    n = other_count
  )

  top_reasons_count <- rbind(top_reasons_count, other_row) %>%
    filter(!is.na(ineligibility_reasons))

  reason_rows <- paste0('<TR><TD ALIGN="LEFT">&#8203;        ', top_reasons_count$n, ' ',
                        top_reasons_count$ineligibility_reasons, '</TD></TR>', collapse = '')

  screened <- sum(df$screened, na.rm = TRUE)
  eligible <- sum(df$eligible, na.rm = TRUE)
  ineligible <- screened - eligible

  eligible_df <- df %>% filter(eligible)
  refused <- sum(eligible_df$refused, na.rm = TRUE)
  not_enrolled_other <- sum(eligible_df$not_consented, na.rm = TRUE)

  ne_reasons <- eligible_df %>%
    filter(not_consented) %>%
    mutate(ne_reason = case_when(
      constraint_admin %in% TRUE ~ 'admin',
      constraint_noconsent %in% TRUE ~ 'noconsent',
      constraint_48hrs %in% TRUE ~ '48hrs',
      constraint_other %in% TRUE ~ 'other_constraint',
      nonparticipation_other_study_coenrolled %in% TRUE ~ 'coenrolled',
      nonparticipation_other_reason %in% TRUE ~ 'other_nonparticipation',
      TRUE ~ 'unknown')) %>%
    pull(ne_reason)
  constraint_admin <- sum(ne_reasons == 'admin')
  constraint_noconsent <- sum(ne_reasons == 'noconsent')
  constraint_48hrs <- sum(ne_reasons == '48hrs')
  constraint_other <- sum(ne_reasons == 'other_constraint')
  coenrolled <- sum(ne_reasons == 'coenrolled')
  other_nonparticipation <- sum(ne_reasons == 'other_nonparticipation')
  ne_unknown <- sum(ne_reasons == 'unknown')

  rand_df <- eligible_df %>%
    filter(consented) %>%
    filter(randomized)
  randomized <- nrow(rand_df)
  excluded <- screened - randomized

  arm_counts <- function(inner_df) {
    assigned <- nrow(inner_df)
    inappropriately_enrolled <- sum(inner_df$adjudicated_inappropriate_enrollment, na.rm = TRUE)
    late_ineligible <- sum(inner_df$adjudicated_late_ineligible, na.rm = TRUE)
    late_refusal <- sum(inner_df$adjudicated_late_refusal, na.rm = TRUE)
    physician_withdrawn <- sum(inner_df$adjudicated_physician_withdrawn, na.rm = TRUE)

    itt_df <- inner_df %>%
      filter(!adjudicated_inappropriate_enrollment | is.na(adjudicated_inappropriate_enrollment)) %>%
      filter(!adjudicated_late_ineligible | is.na(adjudicated_late_ineligible)) %>%
      filter(!adjudicated_late_refusal | is.na(adjudicated_late_refusal)) %>%
      filter(!adjudicated_physician_withdrawn | is.na(adjudicated_physician_withdrawn))

    no_definitive_fixation <- sum(is.na(itt_df$df_surg_start_date))
    itt_df <- itt_df %>%
      filter(!is.na(df_surg_start_date))

    itt <- nrow(itt_df)
    surgery_or_healed_days_num <- suppressWarnings(as.numeric(itt_df$surgery_or_healed_days))
    full_follow_up <- !is.na(surgery_or_healed_days_num) & surgery_or_healed_days_num >= 365
    known_outcome <- sum(itt_df$surgery_or_healed_type %in% 'favorable_event' |
                           (itt_df$surgery_or_healed_type %in% 'check' & full_follow_up))
    adjudicated_healed <- sum(itt_df$surgery_or_healed_type %in% 'favorable_event')
    unfavorable_event <- sum(itt_df$surgery_or_healed_type %in% 'unfavorable_event')
    if ("adherent" %in% names(itt_df)) {
      pp_n <- sum(itt_df$adherent %in% TRUE)
      non_adherent <- itt - pp_n
    } else {
      non_adherent <- sum(itt_df$crossover, na.rm = TRUE)
    }
    crossover_n <- sum(itt_df$crossover %in% TRUE)

    # Amended SAP section 3 elements, per arm. Risk-set entry and person-time
    # describe the primary analysis, so they run on the intention-to-treat set;
    # the day-180/365 statuses and the dispositions run on all randomized in the
    # arm. Days count from Time Zero; the SAP phrases the status days as
    # following discharge, recorded in SAP_Issues_and_Questions.md.
    # Participant-specific primary risk entry, revised SAP; missing entry falls
    # back to day 90. An event before entry never enters the risk set.
    entry_num <- suppressWarnings(as.numeric(itt_df$primary_entry_day))
    entry_num <- ifelse(is.na(entry_num), 90, entry_num)
    entry_boundary <- entry_num - 1
    itt_event <- itt_df$surgery_or_healed_type %in% 'unfavorable_event'
    in_risk <- !is.na(surgery_or_healed_days_num) & surgery_or_healed_days_num > entry_boundary & !(itt_event & surgery_or_healed_days_num < entry_num)
    risk_set_n <- sum(in_risk)
    person_days <- sum(pmax(0, pmin(surgery_or_healed_days_num[in_risk], 365) - entry_boundary[in_risk]), na.rm = TRUE)

    rand_days <- suppressWarnings(as.numeric(inner_df$surgery_or_healed_days))
    status_at <- function(d) {
      event_by <- inner_df$surgery_or_healed_type %in% 'unfavorable_event' & !is.na(rand_days) & rand_days <= d
      free_through <- !event_by & !is.na(rand_days) & rand_days >= d
      c(event = sum(event_by), free = sum(free_through),
        unknown = nrow(inner_df) - sum(event_by) - sum(free_through))
    }

    list(
      s180 = status_at(180),
      s365 = status_at(365),
      risk_set_n = risk_set_n,
      person_days = person_days,
      deaths_n = sum(inner_df$dead %in% TRUE),
      withdrew_n = sum(inner_df$withdrawn_consent %in% TRUE),
      ltfu_n = sum(inner_df$not_completed_reason %in% 'Unreachable'),
      bpi_sev_n = sum(!is.na(suppressWarnings(as.numeric(itt_df$bpi_severity_score_3mo)))),
      bpi_int_n = sum(!is.na(suppressWarnings(as.numeric(itt_df$bpi_interference_score_3mo)))),
      opioid_n = sum(!is.na(suppressWarnings(as.numeric(itt_df$opioid_days_baseline))) |
                       !is.na(suppressWarnings(as.numeric(itt_df$opioid_days_3mo))) |
                       !is.na(suppressWarnings(as.numeric(itt_df$opioid_days_6mo))) |
                       !is.na(suppressWarnings(as.numeric(itt_df$opioid_days_12mo)))),
      assigned = assigned,
      inappropriately_enrolled = inappropriately_enrolled,
      late_ineligible = late_ineligible,
      late_refusal = late_refusal,
      physician_withdrawn = physician_withdrawn,
      no_definitive_fixation = no_definitive_fixation,
      itt = itt,
      known_outcome = known_outcome,
      unfavorable_event = unfavorable_event,
      unknown_outcome = itt - known_outcome - unfavorable_event,
      adjudicated_healed = adjudicated_healed,
      per_protocol = itt - non_adherent,
      non_adherent = non_adherent,
      crossover_n = crossover_n
    )
  }

  a <- arm_counts(rand_df %>% filter(treatment_arm == 'Group A'))
  b <- arm_counts(rand_df %>% filter(treatment_arm == 'Group B'))

  show_no_df <- (a$no_definitive_fixation + b$no_definitive_fixation) > 0

  arm_column <- function(arm, arm_str, x, suffix) {
    no_df_row <- ifelse(show_no_df,
                        paste0('<TR><TD ALIGN="LEFT">&#8203;    ', arm$no_definitive_fixation, ' Did not complete definitive fixation</TD></TR>'),
                        '')
    paste0('
      assigned', suffix, ' [style="rounded,filled", fillcolor="#DDE9F5", color="#2E5F8A", penwidth=1.5, pos="', x, ',-1.5!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', arm$assigned, ' Were assigned to receive ', arm_str, '</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$inappropriately_enrolled, ' Were determined to be inappropriately</TD></TR>
            <TR><TD ALIGN="LEFT">enrolled by blinded adjudication committee</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$late_ineligible, ' Were determined to be late ineligible by</TD></TR>
            <TR><TD ALIGN="LEFT">blinded adjudication committee</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$late_refusal, ' Were determined to be late refusals by</TD></TR>
            <TR><TD ALIGN="LEFT">blinded adjudication committee</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$physician_withdrawn, ' Were determined to be physician</TD></TR>
            <TR><TD ALIGN="LEFT">withdrawn by blinded adjudication committee</TD></TR>
            ', no_df_row, '
          </TABLE>
        >];

      itt', suffix, ' [style="rounded,filled", fillcolor="#DDE9F5", color="#2E5F8A", penwidth=1.5, pos="', x, ',-3.4!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', arm$itt, ' Were included in the intention-to-treat</TD></TR>
            <TR><TD ALIGN="LEFT">analysis</TD></TR>
          </TABLE>
        >];

      outcome', suffix, ' [style="rounded,filled", fillcolor="#E3F1E7", color="#2E7D4F", penwidth=1.5, pos="', x, ',-5.3!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', arm$known_outcome, ' Had ', outcome_day, ' days of follow-up without</TD></TR>
            <TR><TD ALIGN="LEFT">a surgery to promote union</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$adjudicated_healed, ' Were adjudicated as healed at the</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    final follow-up x ray and are counted as</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    having 365 days of event-free follow-up</TD></TR>
            <TR><TD ALIGN="LEFT">', arm$unfavorable_event, ' Had a surgery to promote union</TD></TR>
            <TR><TD ALIGN="LEFT">', arm$unknown_outcome, ' Had less than ', outcome_day, ' days of follow-up</TD></TR>
            <TR><TD ALIGN="LEFT">without a surgery to promote union and</TD></TR>
            <TR><TD ALIGN="LEFT">were censored at last contact</TD></TR>
          </TABLE>
        >];

      pp', suffix, ' [style="rounded,filled", fillcolor="#E3F1E7", color="#2E7D4F", penwidth=1.5, pos="', x, ',-7.5!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">', arm$per_protocol, ' Were included in the per-protocol</TD></TR>
            <TR><TD ALIGN="LEFT">analysis</TD></TR>
            <TR><TD ALIGN="LEFT">', arm$non_adherent, ' Were excluded from the per-protocol</TD></TR>
            <TR><TD ALIGN="LEFT">analysis due to non-adherence</TD></TR>
            <TR><TD ALIGN="LEFT">', arm$crossover_n, ' Met the opposite arm&#39;s adherence</TD></TR>
            <TR><TD ALIGN="LEFT">criteria (crossover, revised definition)</TD></TR>
          </TABLE>
        >];

      sap1', suffix, ' [style="rounded,filled", fillcolor="#FAF3DF", color="#B08A2E", penwidth=1.2, pos="', x, ',-10.1!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">Primary analysis accounting</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$risk_set_n, ' Entered the primary risk set</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', format(arm$person_days, big.mark = ","), ' Primary likelihood person-days</TD></TR>
            <TR><TD ALIGN="LEFT">Status at day 180:</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$s180['event'], ' Surgery to promote union</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$s180['free'], ' Known event-free</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$s180['unknown'], ' Status unknown</TD></TR>
            <TR><TD ALIGN="LEFT">Status at day 365:</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$s365['event'], ' Surgery to promote union</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$s365['free'], ' Known event-free</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$s365['unknown'], ' Status unknown</TD></TR>
          </TABLE>
        >];

      sap2', suffix, ' [style="rounded,filled", fillcolor="#FAF3DF", color="#B08A2E", penwidth=1.2, pos="', x, ',-12.7!", shape = box, width=2.4, height=.5, labeljust=l,
        label = <
          <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
            <TR><TD ALIGN="LEFT">Dispositions among randomized</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$deaths_n, ' Died</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$withdrew_n, ' Withdrew consent</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$ltfu_n, ' Lost to follow-up</TD></TR>
            <TR><TD ALIGN="LEFT">Included in secondary analyses:</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$bpi_sev_n, ' Day-90 BPI pain intensity</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$bpi_int_n, ' Day-90 BPI pain interference</TD></TR>
            <TR><TD ALIGN="LEFT">&#8203;    ', arm$opioid_n, ' Reported opioid use</TD></TR>
          </TABLE>
        >];
    ')
  }

  consort_diagram <- grViz(paste0('
    digraph g {
      graph [layout=fdp, overlap = true, fontsize=1, splines=polyline, bgcolor="white"]
      node [fontname="Helvetica", fontsize=12, margin="0.14,0.08"]
      edge [color="#5B6B7C", penwidth=1.1, arrowsize=0.7]

      title [style="rounded,filled", fillcolor="#DDE9F5", color="#2E5F8A", penwidth=1.5, pos="2,7.2!", shape = box, width=2.4, height=.5,
        label = "', screened, ' Patients were assessed for eligibility"];

      box1 [style="rounded,filled", fillcolor="#EEF1F5", color="#8A93A0", penwidth=1.2, pos="5.4,3.8!", shape = box, width=2.4, height=.5,
      labeljust=l,
      label = <
        <TABLE BORDER="0" CELLBORDER="0" CELLPADDING="0">
          <TR><TD ALIGN="LEFT">', excluded, ' Were excluded</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', ineligible, ' Did not meet eligibility criteria</TD></TR>
          ', reason_rows, '
          <TR><TD ALIGN="LEFT">&#8203;    ', refused, ' Declined consent</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;    ', not_enrolled_other, ' Were not enrolled for other reasons</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;        ', constraint_admin, ' Had administrative reasons</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;        ', constraint_noconsent, ' Had no one there to consent them</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;        ', constraint_48hrs, ' Were more than 48 hours post</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;            definitive fixation surgery</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;        ', constraint_other, ' Had other constraints</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;        ', coenrolled, ' Were enrolled in a study that does</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;            not allow co-enrollment</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;        ', other_nonparticipation, ' Were not able to participate for</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;            other reasons</TD></TR>
          <TR><TD ALIGN="LEFT">&#8203;        ', ne_unknown, ' Had other or unknown reasons</TD></TR>
        </TABLE>
      >];

      title2 [style="rounded,filled", fillcolor="#DDE9F5", color="#2E5F8A", penwidth=1.5, pos="2,0.4!", shape = box, width=2.4, height=.5,
        label = "', randomized, ' Underwent randomization"];
    ',
    arm_column(a, arm_a_str, '-0.85', '_a'),
    arm_column(b, arm_b_str, '4.85', '_b'),
    '
      midpoint [style=invis, pos="2,3.8!", width=0, height=0, fixedsize=true]

      # Relationships
      title -> midpoint [arrowhead=none]
      midpoint -> title2
      midpoint -> box1
      title2 -> assigned_a
      title2 -> assigned_b
      assigned_a -> itt_a
      itt_a -> outcome_a
      outcome_a -> pp_a
      assigned_b -> itt_b
      itt_b -> outcome_b
      outcome_b -> pp_b
      pp_a -> sap1_a [style=dashed, arrowhead=none]
      sap1_a -> sap2_a [style=dashed, arrowhead=none]
      pp_b -> sap1_b [style=dashed, arrowhead=none]
      sap1_b -> sap2_b [style=dashed, arrowhead=none]
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


#' Posterior figure of the risk difference behind a noninferiority table
#'
#' @description
#' Renders the exact risk-difference posterior behind a return_fit = TRUE result of
#' closed_survival_analysis_bayes_poisson: the same draws as the table it accompanies,
#' no additional fit. Density over the risk-difference draws with the median, the 95%
#' credible bounds, the zero line, and the noninferiority margin with the region past
#' it shaded, the margin read from the fit's settings. The caption belongs to the
#' calling report via VisualizationTools::figure().
#'
#' @param survival_result the list returned by closed_survival_analysis_bayes_poisson
#' with return_fit = TRUE
#'
#' @return An HTML img tag with the figure embedded as a data URI.
#' @export
#'
#' @examples
#' \dontrun{
#' closed_risk_difference_posterior_figure(survival_result)
#' }
closed_risk_difference_posterior_figure <- function(survival_result) {
  rd     <- 100 * survival_result$posterior$risk_difference
  rd_med <- median(rd)
  rd_lo  <- unname(quantile(rd, 0.025))
  rd_hi  <- unname(quantile(rd, 0.975))
  margin_pct <- 100 * survival_result$settings$ni_margin
  p_fig <- ggplot2::ggplot(data.frame(rd = rd), ggplot2::aes(x = rd)) +
    ggplot2::annotate("rect", xmin = margin_pct, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = "#D62828", alpha = 0.08) +
    ggplot2::geom_density(fill = "#DDE9F5", color = "#17365D", linewidth = 0.9, adjust = 1.2) +
    ggplot2::geom_vline(xintercept = 0, color = "#8A93A0", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = c(rd_lo, rd_hi), color = "#2E5F8A", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = rd_med, color = "#17365D", linewidth = 0.8) +
    ggplot2::geom_vline(xintercept = margin_pct, color = "#D62828", linewidth = 0.9) +
    ggplot2::annotate("text", x = margin_pct, y = Inf,
                      label = sprintf("+%.0f point noninferiority margin", margin_pct),
                      hjust = 1.05, vjust = 2, color = "#D62828", size = 3.4, fontface = "bold") +
    ggplot2::labs(x = "Absolute risk difference, treatment minus control (percentage points)", y = NULL,
                  subtitle = sprintf("Median %+.1f  |  95%% credible interval %+.1f to %+.1f  |  97.5th percentile %+.1f",
                                     rd_med, rd_lo, rd_hi, rd_hi)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::theme_minimal(base_family = "Helvetica", base_size = 12) +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   plot.subtitle = ggplot2::element_text(color = "#17365D", face = "bold", size = 10))
  fig_path <- tempfile(fileext = ".png")
  ggplot2::ggsave(fig_path, p_fig, width = 8.5, height = 3.4, dpi = 150, bg = "white")
  img_tag <- sprintf('<img src="data:image/png;base64,%s" style="max-width:100%%" alt="Posterior distribution of the risk difference"/>',
                     base64enc::base64encode(fig_path))
  file.remove(fig_path)
  return(img_tag)
}


#' Summary forest of noninferiority analyses
#'
#' @description
#' Forest figure of the median and 95% credible interval of the risk-difference
#' posterior for a set of noninferiority fits, read against the noninferiority margin
#' and color-coded by verdict. Each entry carries a return_fit = TRUE result of
#' closed_survival_analysis_bayes_poisson, so the forest is built from the same fits
#' as the tables it summarizes; no additional model runs. The margin is read from the
#' first entry's settings. The caption belongs to the calling report via
#' VisualizationTools::figure().
#'
#' @param analyses list of entries, each a list with number (the analysis's table
#' number), label (its display name), and result (its return_fit = TRUE result of
#' closed_survival_analysis_bayes_poisson)
#'
#' @return An HTML img tag with the figure embedded as a data URI, or invisible NULL
#' when analyses is empty.
#' @export
#'
#' @examples
#' \dontrun{
#' closed_noninferiority_forest(list(list(number = "8.3", label = "Primary Analysis", result = survival_result)))
#' }
closed_noninferiority_forest <- function(analyses) {
  if (length(analyses) == 0) {
    return(invisible(NULL))
  }
  fr <- do.call(rbind, lapply(analyses, function(a) {
    rd <- 100 * a$result$posterior$risk_difference
    data.frame(number = a$number, label = a$label,
               med = median(rd),
               lo  = unname(quantile(rd, 0.025)),
               hi  = unname(quantile(rd, 0.975)),
               verdict = ifelse(a$result$posterior$noninferior, "Yes", "No"))
  }))
  margin_pct <- 100 * analyses[[1]]$result$settings$ni_margin
  fr$axis_label <- sprintf("%s   (%s)", fr$label, fr$number)
  fr$axis_label <- factor(fr$axis_label, levels = rev(fr$axis_label))
  p_forest <- ggplot2::ggplot(fr, ggplot2::aes(x = med, y = axis_label)) +
    ggplot2::annotate("rect", xmin = margin_pct, xmax = Inf, ymin = -Inf, ymax = Inf,
                      fill = "#D62828", alpha = 0.08) +
    ggplot2::geom_vline(xintercept = 0, color = "#8A93A0", linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = margin_pct, color = "#D62828", linewidth = 0.9) +
    ggplot2::geom_segment(ggplot2::aes(x = lo, xend = hi, yend = axis_label, color = verdict), linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(color = verdict), size = 2.4) +
    ggplot2::scale_color_manual(values = c(Yes = "#17365D", No = "#D62828"), name = "Noninferior") +
    ggplot2::labs(x = "Absolute risk difference, treatment minus control (percentage points)", y = NULL) +
    ggplot2::theme_minimal(base_family = "Helvetica", base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "top",
                   axis.text.y = ggplot2::element_text(color = "#17365D"))
  fig_path <- tempfile(fileext = ".png")
  ggplot2::ggsave(fig_path, p_forest, width = 9.5, height = 1.4 + 0.34 * nrow(fr),
                  dpi = 150, bg = "white", limitsize = FALSE)
  img_tag <- sprintf('<img src="data:image/png;base64,%s" style="max-width:100%%" alt="Forest plot of all noninferiority analyses"/>',
                     base64enc::base64encode(fig_path))
  file.remove(fig_path)
  return(img_tag)
}
