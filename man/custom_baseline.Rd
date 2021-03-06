% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customizable_functions.R
\name{custom_baseline}
\alias{custom_baseline}
\title{Pull smoothed Chicago NMMAPS health outcome data}
\usage{
custom_baseline(n, df = dlnm::chicagoNMMAPS, outcome_type = "cvd",
  start.date = "2000-01-01")
}
\arguments{
\item{n}{A numeric value specifying the number of days for which to obtain an
exposure value.}

\item{df}{Data frame from which to pull exposure values.}

\item{outcome_type}{A character string specifying the desired health outcome metric.
   Options are:
   \itemize{
     \item"death"
     \item"cvd"
     \item"resp"}
(Note: These are the column names for outcome counts in the observed data.)}

\item{start.date}{A date of the format "yyyy-mm-dd" from which to begin
pulling exposure values. Dates in the Chicago NMMAPS data set are from
1987-01-01 to 2000-12-31.}
}
\value{
A data frame with one column for date and one column for baseline
   outcome values.
}
\description{
Example of a custom baseline function that can be passed to \code{\link{eesim}} or
\code{\link{power_calc}}.
By default, this function pulls smoothed data from the \code{chicagoNMMAPS} data set
in the \code{dlnm} package.  The user may also input a different data set from which
to pull data. The function uses a smoothed function of this observed data as the
underlying baseline outcome trend in simulating data.
}
\examples{
custom_baseline(n = 5)
custom_baseline(n = 5, outcome_type = "death")

}
