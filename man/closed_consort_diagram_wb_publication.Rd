% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/closed_figures.R
\name{closed_consort_diagram_wb_publication}
\alias{closed_consort_diagram_wb_publication}
\title{Consort Diagram: publication}
\usage{
closed_consort_diagram_wb_publication(analytic)
}
\arguments{
\item{analytic}{analytic data set that must include 
study_id, screened, ineligible, ineligibility_reasons, refused, constraint_other, constraint_other_txt, consented, 
discontinued_pre_randomization, injury_type, randomized, 
late_ineligible, per_protocol_sample, enrolled, consent_date, death_date, withdraw_date,
preinjury_work_status, treatment_arm}
}
\value{
An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
}
\description{
The closed version of consort_diagram_wb_publication, breaking down study cancellations by treatment_arm.

This consort diagram was made for the Weight Bearing study, and so is unlikely to work for yours.
}
\examples{
closed_consort_diagram_wb_publication("Replace with Analytic Tibble")

}
