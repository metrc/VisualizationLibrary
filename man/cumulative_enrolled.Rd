% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard_figures.R
\name{cumulative_enrolled}
\alias{cumulative_enrolled}
\title{Cumulative enrollment}
\usage{
cumulative_enrolled(
  analytic,
  bar_mode = FALSE,
  goal = NULL,
  goal_percent = FALSE
)
}
\arguments{
\item{analytic}{This is the analytic data set that must include study_id, enrolled, consent_date}

\item{bar_mode}{if false, uses a line to indicate total enrollment, and bars to indicate enrollment
change}

\item{goal}{number, the goal of enrollment}

\item{goal_percent}{if goal is supplied then sets the y axis label to percent}
}
\value{
An HTML string containing an image tag with the base64-encoded consort diagram in PNG format.
}
\description{
This function visualizes the cumulative number of patients enrolled, by month.
}
\examples{
cumulative_enrolled("Replace with Analytic Tibble")
cumulative_enrolled("Replace with Analytic Tibble", bar_mode=TRUE)
cumulative_enrolled("Replace with Analytic Tibble", goal = 1000)
cumulative_enrolled("Replace with Analytic Tibble", goal = 1000, goal_percent = TRUE)

}
