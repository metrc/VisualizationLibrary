% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard_tables.R
\name{not_complete_sae_deviation_by_type}
\alias{not_complete_sae_deviation_by_type}
\title{Number of Non-Completing Participants, SAEs, and Protocol Deviations by type}
\usage{
not_complete_sae_deviation_by_type(analytic)
}
\arguments{
\item{analytic}{This is the analytic data set that must include enrolled, not_expected_reason, not_completed_reason,
protocol_deviation_screen_consent, protocol_deviation_procedural, protocol_deviation_administrative, sae_count, not_completed, sae_count}
}
\value{
html table
}
\description{
This function visualizes the number of non-completions, not expected, and SAEs for only 
"enrolled" participants and Protocol Deviations by type for all the "consented" participants. 

Refer to not_complete_sae_deviation_by_type_auto_categories.
}
\examples{
not_complete_sae_deviation_by_type("Replace with Analytic Tibble")

}
