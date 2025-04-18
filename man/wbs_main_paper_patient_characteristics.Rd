% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard_tables.R
\name{wbs_main_paper_patient_characteristics}
\alias{wbs_main_paper_patient_characteristics}
\title{Weight Bearing Patient Characteristics for Main paper}
\usage{
wbs_main_paper_patient_characteristics(analytic)
}
\arguments{
\item{analytic}{enrolled, injury_type, sex, age, ethnicity_race, education_level, patient_reported_self_efficacy_6mo, 
patient_reported_self_efficacy_12mo, preinjury_productive_activity, preinjury_work_demand, 
preinjury_work_hours, tobacco_use, bmi, preinjury_health, insurance_type}
}
\value{
html table
}
\description{
Visualizes various patient characteristics/demographics for enrolled 
patients with "Ankle" injuries. 

NOTE: This table was originally produced for Weight bearing main paper, but may apply to your study, 
see the used constructs for more.
}
\examples{
wbs_main_paper_patient_characteristics(analytic)

}
