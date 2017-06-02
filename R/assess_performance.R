#' Average Estimated Coefficient
#'
#' This function gives the mean value of the estimated log relative risks (\eqn{\hat{\beta}}s)
#' and the mean of the estimated relative risk values over the \code{n} simulations.
#'
#' @param df A data frame of replicated simulations which must include a column
#'    titled "Estimate" with the effect estimate from the fitted model.
#'
#' @return A data frame with the mean estimated log relative risk and mean estimated
#'    relative risk. The mean estimated risk is based on first calculating the
#'    mean log relative risk and then exponentiating this mean value.
#'
#' @examples
#' sims <- create_sims(n_reps=10, n=50, central = 100, sd = 10,
#'             exposure_type="continuous", exposure_trend = "cos1",
#'             exposure_amp = .6, average_outcome = 22,
#'             outcome_trend = "no trend", outcome_amp = .6, rr = 1.01)
#' fits <- fit_mods(data = sims, custom_model = spline_mod,
#'                  custom_model_args = list(df_year = 1))
#' mean_beta(df=fits)
#'
#' @export
mean_beta <- function(df){
  if (!is.data.frame(df)){
    stop("Input must be a data frame")
  }
  beta_hat <- mean(df$Estimate)
  rr_hat <- mean(beta_hat)
  out <- data.frame(beta_hat, rr_hat)
  return(out)
}

#' Standard Deviation of Estimated Coefficients
#'
#' Measures the variance of the point estimates of the estimated log relative risk
#' (\eqn{\hat{beta}}{b}) over the \code{n_rep} simulations and the mean of
#' the variances of each \eqn{\hat{\beta}}{b}.
#'
#' @param df A data frame of replicated simulations which must include columns
#'    titled "Estimate" and "Std.Error".
#'
#' @return A data frame of the variance across all values of beta hat
#'    and the mean variance of the beta hats
#'
#' @examples
#' sims <- create_sims(n_reps = 10, n = 600, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(data = sims, custom_model = spline_mod,
#'                  custom_model_args  = list(df_year = 1))
#' beta_var(fits)
#'
#' @export
beta_var <- function(df){
  var_across_betas <- stats::var(df$Estimate)
  mean_beta_var <- mean(df$Std.Error^2)
  out <- data.frame(var_across_betas, mean_beta_var)
  return(out)
}

#' Percent Bias of Estimated Coefficient
#'
#' This function returns the relative bias of the mean of the estimated coefficients.
#'
#'
#' @details This function estimates the percent bias in the estimated log relative risk
#'    (\eqn{b}) as:
#' \deqn{100 * \frac{\beta - \hat{\beta}}{\beta}}{100 ((\beta - b) / \beta)}
#' where \eqn{\hat{\beta}}{b} is the mean of the estimated log relative risk values from all
#' simulations and \eqn{\beta} is the true log relative risk used to simulate the data.
#'
#' @inheritParams mean_beta
#' @param true_rr The true relative risk used to simulate the data.
#'
#' @return A data frame with a single value: the percent bias of the mean of the estimated
#'    coefficients over \code{n_reps} simulations.
#'
#' @examples
#' sims <- create_sims(n_reps = 10, n = 600, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(data = sims, custom_model = spline_mod,
#'                  custom_model_args = list(df_year = 1))
#' beta_bias(fits, true_rr = 1.02)
#'
#' @export
beta_bias <- function(df, true_rr){
  percent_bias <- 100 * (log(true_rr) - mean(df$Estimate)) / log(true_rr)
  out <- data.frame(percent_bias)
  return(out)
}

#' Empirical coverage of confidence intervals
#'
#' Calculates the percent of simulations in which the estimated 95\% confidence
#' interval for the log relative risk includes the true value of the log
#' relative risk.
#'
#' @param df A data frame of replicated simulations which must include columns
#'    titled \code{lower_ci} and \code{upper_ci}.
#' @param true_rr The true relative risk used to simulate the data.
#'
#' @return A data frame with the percent of confidence intervals for the
#'    estimated log relative risk over \code{n_reps} simulations which include
#'    the true log relative risk.
#'
#' @examples
#' sims <- create_sims(n_reps = 10, n = 600, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1", exposure_slope=1,
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(data = sims, custom_model = spline_mod,
#'                  custom_model_args = list(df_year = 1))
#' coverage_beta(df=fits, true_rr = 1.02)
#'
#' @export
coverage_beta <- function(df, true_rr){
  true_beta <- log(true_rr)
  coverage <- df$lower_ci <= true_beta & df$upper_ci >= true_beta
  out <- data.frame(coverage = sum(coverage) / nrow(df))
  return(out)
}

#' Estimate power
#'
#' Calculates the estimated power of a hypothesis test that the log relative risk
#' equals 0 at a 5\% significance level across all simulated data.
#'
#' @inheritParams coverage_beta
#'
#' @return A data frame with one row with the estimated power of the analysis
#'    at the 5\% significance level.
#'
#' @examples
#' sims <- create_sims(n_reps = 10, n = 600, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(data = sims, custom_model = spline_mod,
#'                  custom_model_args = list(df_year = 1))
#' power_beta(fits)
#'
#' @export
power_beta <- function(df){
  no_zero <- df$lower_ci >= 0 | df$upper_ci <= 0
  out <- data.frame(power = sum(no_zero) / nrow(df))
  return(out)
}

#' Assess model performance
#'
#' Calculates several measures of model performance, based on results of fitting
#' a model to all simulated datasets.
#'
#' @inheritParams beta_bias
#'
#' @return A dataframe with one row with model assessment across all simulations.
#'   Includes values for:
#'   \itemize{
#'     \item{\code{beta_hat}: Mean of the estimated log relative risk across all simulations.}
#'     \item{\code{rr_hat}: Mean value of the estimated relative risk across all simulations.}
#'     \item{\code{var_across_betas}: Variance of the estimated log relative risk across all
#'           simulations}
#'     \item{\code{mean_beta_var}: The mean of the estimated variances of the estimated log
#'           relative risks across all simulations.}
#'     \item{\code{percent_bias}: The relative bias of the estimated log relative risks compared
#'           to the true log relative risk.}
#'     \item{\code{coverage}: Percent of simulations for which the estimated 95\% confidence
#'           interval for log relative risk includes the true log relative risk.}
#'     \item{\code{power}: Percent of simulations for which the null hypothesis that the log
#'           relative risk equals zero is rejected based on a p-value of 0.05.}
#'   }
#'
#' @seealso The following functions are used to calculate these measurements:
#'    \code{\link{beta_bias}}, \code{\link{beta_var}}, \code{\link{coverage_beta}},
#'    \code{\link{mean_beta}}, \code{\link{power_beta}}
#'
#' @examples
#' sims <- create_sims(n_reps = 100, n = 1000, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.02)
#' fits <- fit_mods(data = sims, custom_model = spline_mod,
#'                  custom_model_args = list(df_year = 1))
#' check_sims(df = fits, true_rr = 1.02)
#'
#' @export
#'
check_sims <- function(df, true_rr){
  a <- mean_beta(df)
  b <- beta_var(df)
  c <- beta_bias(df, true_rr = true_rr)
  d <- coverage_beta(df, true_rr = true_rr)
  e <- power_beta(df)

  out <- cbind(a, b, c, d, e)
  return(out)
}

#' Power Calculations
#'
#' Calculates the expected power of an environmental epidemiology time series analysis based
#' on simulated datasets. This function uses the simulation provided by \code{eesim} to
#' simulate multiple environmental epidemiology datasets under different scenarios (e.g.,
#' total days in study, size of association between exposure and outcome, or baseline
#' average daily count of the outcome in the study) and estimates the power of a specified
#' analysis to detect the hypothesized association.
#'
#' @param varying A character string specifying the parameter to be varied.  Choices are
#'    \code{'n'} (which varies the number of days in each dataset of simulated data),
#'    \code{'rr'} (which varies the relative rate per unit increase in exposure that is used
#'    to simulate the data), or \code{'average_outcome'} (which varies the average value
#'    of the outcomes in each dataset). For whichever of these three values is not set to vary in this
#'    argument, the user must specify a constant value to this function through the
#'    \code{n}, \code{rr}, or \code{average_outcome} arguments.
#' @param values A numeric vector with the values you would like to test for the varying
#'    parameters. For example, \code{values = c(1.05, 1.10, 1.15)} would produce power
#'    estimates for the four specified values of relative risk if the user has specified
#'    \code{varying = 'rr'}.
#' @param plot "TRUE" or "FALSE" for whether to produce a plot
#' @inheritParams power_beta
#' @inheritParams create_sims
#' @inheritParams fit_mods
#'
#' @return Data frame with the values of the varying parameter and the estimated power
#' for each. If the \code{plot} argument is set to \code{TRUE}, it also returns a power
#' curve plot as a side effect. Because these estimates are based on simulations, there
#' will be some random variation in estimates of power. Estimates will be more stable
#' if a higher value is used for \code{n_reps}, although this will increase the time it
#' takes the function to run.
#'
#' @examples
#'
#' # Calculate power for studies that vary in the total length of the study period
#' # (between one and twenty-one years of data) for the association between a continuous
#' # exposure with a seasonal trend (mean = 100, sd from seasonal baseline = 10) and a count
#' # outcome (e.g., daily number of deaths, mean daily value across the study period of 22).
#' # The alternative hypothesis is that there is a relative rate of the outcome of 1.001 for
#' # every one-unit increase in exposure. The null hypothesis is that there is no association
#' # between the exposure and the outcome. The model used to test for an association is a
#' # case-crossover model
#' \dontrun{
#' pow <- power_calc(varying = "n", values = floor(365.25 * seq(1, 21, by = 5)), n_reps = 20,
#'            central = 100, sd = 10, rr = 1.001, exposure_type = "continuous",
#'            exposure_trend = "cos1", exposure_amp = .6, average_outcome = 22,
#'            outcome_trend = "no trend", outcome_amp = .6,
#'            custom_model = spline_mod, plot = TRUE)
#'}
#'
#' @export
power_calc <- function(varying, values, n_reps, custom_model, central, exposure_type,
                       n = NULL, sd = NULL, exposure_trend = "no trend",
                       exposure_amp = NULL, average_outcome = NULL,
                       outcome_trend = "no trend", outcome_amp = NULL, rr = NULL,
                       start.date = "2000-01-01",
                       cust_exp_func = NULL, cust_exp_args = NULL,
                       cust_base_func = NULL, cust_lambda_func = NULL,
                       cust_base_args = NULL, cust_lambda_args = NULL,
                       custom_model_args = NULL, plot = FALSE){
  msg <- paste("This function may take a minute or two to run, especially with lots of",
               "replications (`n_reps`) or options for `values`.")
  msg <- paste(strwrap(msg), collapse = "\n")
  message(msg)

  if(varying == "n"){
    rep_df <- values %>% purrr::map(create_sims, n_reps = n_reps, central = central, sd = sd,
                                    exposure_type = exposure_type,
                                    exposure_trend = exposure_trend,exposure_amp = exposure_amp,
                                    average_outcome = average_outcome, outcome_trend = outcome_trend,
                                    outcome_amp = outcome_amp, rr = rr, start.date = start.date)
    fits <- rep_df %>% purrr::map(fit_mods, custom_model = custom_model,
                                  custom_model_args = custom_model_args)
    power <- fits %>% purrr::map(power_beta) #makes a list, want to extract the values of power and put in a data frame with values of n.
  }
  else if(varying == "rr"){
    rep_df <- values %>% purrr::map(create_sims, n = n, n_reps = n_reps, central = central, sd = sd,                         exposure_type = exposure_type,
                                    exposure_trend = exposure_trend, exposure_amp = exposure_amp,
                                    average_outcome = average_outcome, outcome_trend = outcome_trend,
                                    outcome_amp = outcome_amp, start.date = start.date)
    fits <- rep_df %>% purrr::map(fit_mods, custom_model = custom_model,
                                  custom_model_args = custom_model_args)
    power <- fits %>% purrr::map(power_beta) #makes a list, want to extract the values of power and put in a data frame with values of rr.
  }
  else if(varying == "average_outcome"){
    rep_df <- values %>% purrr::map(create_sims, n = n, n_reps = n_reps,
                                    central = central, sd = sd, exposure_type = exposure_type,
                                    exposure_trend = exposure_trend,
                                    exposure_amp = exposure_amp, outcome_trend = outcome_trend,
                                    outcome_amp = outcome_amp, rr = rr, start.date = start.date)
    fits <- rep_df %>% purrr::map(fit_mods, custom_model = custom_model,
                                  custom_model_args = custom_model_args)
    power <- fits %>% purrr::map(power_beta)
  }

  powervec <- rep(0, length(power))
  for (i in 1:length(power)){
    powervec[i] <- power[[i]][ , 1]
  }

  dat <- data.frame(values = values, power = powervec)

  if(plot == TRUE){
    my_plot <- ggplot2::ggplot(dat, ggplot2::aes_(x = ~ dat$values, y = ~ dat$power)) +
      ggplot2::geom_line() + ggplot2::theme_minimal() +
      ggplot2::xlab(varying) + ggplot2::ylab("power") +
      ggplot2::ylim(0, 1)
    print(my_plot)
  }

  return(dat)
}
