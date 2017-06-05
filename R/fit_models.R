#' Fit a generalized linear model
#'
#' Fits a model to estimate the log relative risk between an exposure and outcome
#' by using a natural cubic spline to control for long-term and seasonal trends and
#' assuming a quasi-Poisson distribution for the outcome. This function provides an
#' example of a function that can be input to \code{\link{eesim}} or \code{\link{power_calc}}.
#'
#' @param df A data frame with columns for daily exposure and outcome called "x" and "outcome",
#'    respectively
#' @param df_year A numeric value specifying the degrees of freedom per year to
#'    use in the spline to control for long-term and seasonal trends in the outcome.
#'
#' @return A numeric vector of length six with summary measures from fitting this model
#'    to simulated data.
#'
#' @examples
#' exp <- sim_exposure(n = 500, central = 100, sd = 10, trend = "cos1",
#'                     amp = .6, exposure_type = "continuous")
#' out <- sim_outcome(exposure = exp, average_outcome = 22, rr = 1.01)
#' spline_mod(df = out)
#'
#' @export
spline_mod <- function(df, df_year = 7){
  dgrs_free <- df_year * as.numeric(diff(df[c(1, nrow(df)), "date"])) / 365.4
  df$time <- scale(df$date, center = TRUE, scale = FALSE)
  mod <- stats::glm(outcome ~ x + splines::ns(time, round(dgrs_free)),
                   data = df,
                   family = stats::quasipoisson(link = "log"))

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- stats::confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}
