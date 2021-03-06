% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assess_performance.R
\name{power_beta}
\alias{power_beta}
\title{Estimate power}
\usage{
power_beta(df)
}
\arguments{
\item{df}{A data frame of replicated simulations which must include columns
titled \code{lower_ci} and \code{upper_ci}.}
}
\value{
A data frame with one row with the estimated power of the analysis
   at the 5\% significance level.
}
\description{
Calculates the estimated power of a hypothesis test that the log relative risk
equals 0 at a 5\% significance level across all simulated data.
}
\examples{
sims <- create_sims(n_reps = 10, n = 600, central = 100,
                    sd = 10, exposure_type = "continuous",
                    exposure_trend = "cos1",
                    exposure_amp = 0.6,
                    average_outcome = 20,
                    outcome_trend = "no trend",
                    rr = 1.01)
fits <- fit_mods(data = sims, custom_model = spline_mod,
                 custom_model_args = list(df_year = 1))
power_beta(fits)

}
