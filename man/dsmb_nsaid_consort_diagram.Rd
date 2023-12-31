% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard_figures.R
\name{dsmb_nsaid_consort_diagram}
\alias{dsmb_nsaid_consort_diagram}
\title{DSMB NSAID Consort Diagram}
\usage{
dsmb_nsaid_consort_diagram(analytic)
}
\arguments{
\item{analytic}{This is the analytic data set that must include screened, eligible, 
consented, not_consented, randomized, enrolled, censored, refused, df_surg_completed, and the meta construct column}
}
\value{
nothing
}
\description{
This function visualizes the categorical percentages of Study Status
for the NSAID study
}
\examples{
\dontrun{
dsmb_nsaid_consort_diagram()
}
}
