% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/closed_tables.R
\name{closed_followup_3mo_status_by_site_sextant}
\alias{closed_followup_3mo_status_by_site_sextant}
\title{Closed Followup 3 month status by site for Sextant}
\usage{
closed_followup_3mo_status_by_site_sextant(analytic)
}
\arguments{
\item{analytic}{This is the analytic data set that must include study_id, eligible, enrolled, facilitycode, followup_expected_3mo, time_zero,
followup_complete_crf12_3mo, followup_incomplete_crf12_3mo, followup_early_crf12_3mo, followup_late_crf12_3mo,
followup_missing_crf12_3mo, followup_not_started_crf12_3mo, followup_status_crf14_crf15_3mo, treatment_arm}
}
\value{
A kable table
}
\description{
This function visualizes 3 month followup status by site for Clinical followup form(crf12) and patient
reported outcome forms(CRF 14 & 15) for Sextant weekly report by treatment arm
}
\examples{
\dontrun{
closed_followup_3mo_status_by_site_sextant(analytic)
}
}
