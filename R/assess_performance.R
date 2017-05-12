#' Average Estimated Coefficient
#'
#' This function gives the mean value of the \eqn{\hat{\beta}}s and the mean
#' estimated relative risk over the n simulations.
#'
#' @param df A data frame of replicated simulations which must include a column
#'    titled "Estimate"
#'
#' @return A data frame with the mean estimated coefficient and mean estimated
#'    relative risk
#'
#' @examples
#' sims <- create_sims(n_reps=10, n=50, central = 100, sd = 10,
#'             exposure_type="continuous", exposure_trend = "cos1",
#'             exposure_amp = .6, average_outcome = 22,
#'             outcome_trend = "no trend", outcome_amp = .6, rr = 1.01)
#' fits <- fit_mods(outcome = sims, model = "spline")
#' mean_beta(df=fits)
#'
#' @export
mean_beta <- function(df){
  if (!is.data.frame(df)){
    stop("Input must be a data frame")
  }
  beta_hat <- mean(df$Estimate)
  rr_hat <- mean(exp(df$Estimate))
  out <- data.frame(beta_hat, rr_hat)
  return(out)
}

#' Standard Deviation of Estimated Coefficients
#'
#' This function gives the variance of the point estimates of beta hat over
#' the n simulations and the mean of the variances of each beta hat.
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
#' fits <- fit_mods(outcome = sims, model = "spline", df_year = 1)
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
#' @inheritParams mean_beta
#' @param true_rr The true relative risk used to simulate your data
#'
#' @return The percent bias of the mean of the estimated coefficients over n
#'    simulations
#'
#' @examples
#' sims <- create_sims(n_reps = 10, n = 600, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(data = sims, model = "spline", df_year = 1)
#' beta_bias(fits, true_rr = 1.02)
#'
#' @export
beta_bias <- function(df, true_rr){
  percent_bias <- 100 * (true_rr - mean(exp(df$Estimate))) / true_rr
  out <- data.frame(percent_bias)
  return(out)
}
#'
#' Percent Coverage of Estimated Coefficients
#'
#' This function gives the percent coverage of the true coefficient.
#'
#' @param df A data frame of replicated simulations which must include columns
#'    titled "lower_ci" and "upper_ci"
#' @param true_rr The true relative risk used to simulate your data
#'
#' @return The percent of confidence intervals for the estimated relative risk
#'    over n simulations which include the true relative risk
#'
#' @examples
#' sims <- create_sims(n_reps = 10, n = 600, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1", exposure_slope=1,
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(data = sims, model = "spline", df_year = 1)
#' coverage_beta(df=fits, true_rr = 1.02)
#'
#' @export
coverage_beta <- function(df, true_rr){
  true_beta <- log(true_rr)
  coverage <- df$lower_ci <= true_beta & df$upper_ci >= true_beta
  out <- data.frame(coverage = sum(coverage) / nrow(df))
  return(out)
}

#' Power
#'
#' This function gives the power of the test at a 5% siginificance level.
#'
#' @inheritParams coverage_beta
#'
#' @return Power at the 5% significance level
#'
#' @examples
#' sims <- create_sims(n_reps = 10, n = 600, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(data = sims, model = "spline", df_year = 1)
#' power_beta(fits)
#'
#' @export
power_beta <- function(df){
  no_zero <- df$lower_ci >= 0 | df$upper_ci <= 0
  out <- data.frame(power = sum(no_zero) / nrow(df))
  return(out)
}

#' Model Performance Assessment
#'
#' This function gives several measures of model performance.
#'
#' @inheritParams beta_bias
#'
#' @return Mean beta estimate, mean relative risk estimate, variance across
#'    betas, mean variance of the estimates, percent bias, coverage, and power.
#'
#' @examples
#' sims <- create_sims(n_reps = 100, n = 1000, central = 100,
#'                     sd = 10, exposure_type = "continuous",
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.02)
#' fits <- fit_mods(data = sims, model = "spline", df_year = 1)
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
#' Calculate the expected power of an enviromental epidemiology time series study based
#' on simulated datasets. This function uses the simulation provided by \code{eesim} to
#' simulate multiple environmental epidemiology datasets under different scenarios (e.g.,
#' total days in study, size of association between exposure and outcome, or baseline
#' average daily count of the outcome in the study) and estimates the power of a specified
#' model to detect the hypothesized association.
#'
#' @param varying A character string of the parameter to be varied.  Choices are
#'    "n" (which varies the number of days in each dataset of simulated data) or
#'    "rr" (which varies the relative rate per unit increase in exposure that is used
#'    to simulate the data). For whichever of these two values is not set to vary in this
#'    argument, the user must specify a constant value to this funciton through either the
#'    \code{n} or the \code{rr} argument.
#' @param values A numeric vector with the values you would like to test for the varying
#'    parameters. For example, \code{values = c(1.05, 1.10, 1.15)} would
#' @param plot "TRUE" or "FALSE" for whether to produce a plot
#' @inheritParams power_beta
#' @inheritParams create_sims
#' @inheritParams fit_mods
#'
#' @return Data frame with the values of the varying parameter and their
#'    corresponding power. If the \code{plot} argument is set to \code{TRUE},
#'    it also returns a power curve plot as a side effect.
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
#' pow <- power_calc(varying = "n", values = floor(365.25 * seq(1, 21, by = 5)), n_reps = 20,
#'            central = 100, sd = 10, rr = 1.001, exposure_type = "continuous",
#'            exposure_trend = "cos1", exposure_amp = .6, average_outcome = 22,
#'            outcome_trend = "no trend", outcome_amp = .6,
#'            model = "casecrossover", plot=TRUE)
#'
#' @export
power_calc <- function(varying, values, n_reps, n = NULL, central, sd = NULL, exposure_type,
                       exposure_trend = "no trend", exposure_amp, average_outcome,
                       outcome_trend = "no trend", outcome_amp, rr = NULL,
                       start.date = "2000-01-01",
                       cust_exp_func = NULL, cust_exp_args = NULL,
                       cust_base_func = NULL, cust_lambda_func = NULL,
                       cust_base_args = NULL, cust_lambda_args = NULL,
                       model, df_year = 7, plot = FALSE){

  msg <- paste("This function may take a minute or two to run, especially with lots of",
               "replications (`n_reps`) or options for `values`.")
  msg <- paste(strwrap(msg), collapse="\n")
  message(msg)

  if(varying == "n"){
    rep_df <- values %>% purrr::map(create_sims, n_reps=n_reps, central=central, sd=sd,
                                    exposure_type = exposure_type,
                                    exposure_trend=exposure_trend,exposure_amp=exposure_amp,
                                    average_outcome=average_outcome,outcome_trend=outcome_trend,
                                    outcome_amp = outcome_amp, rr=rr, start.date = start.date)
    fits <- rep_df %>% purrr::map(fit_mods, model=model, df_year=df_year)
    power <- fits %>% purrr::map(power_beta) #makes a list, want to extract the values of power and put in a data frame with values of n.
  }
  else if(varying == "rr"){
    rep_df <- values %>% purrr::map(create_sims, n=n, n_reps=n_reps, central=central, sd=sd,                         exposure_type = exposure_type,
                                    exposure_trend=exposure_trend,exposure_amp=exposure_amp,
                                    average_outcome=average_outcome,outcome_trend=outcome_trend,
                                    outcome_amp = outcome_amp, start.date = start.date)
    fits <- rep_df %>% purrr::map(fit_mods, model=model, df_year=df_year)
    power <- fits %>% purrr::map(power_beta) #makes a list, want to extract the values of power and put in a data frame with values of rr.
  }
  else if(varying=="average_outcome"){
    rep_df <- values %>% purrr::map(create_sims, n=n, n_reps=n_reps,
                                    central=central,
                                    sd=sd, exposure_type = exposure_type, exposure_trend=exposure_trend,
                                    exposure_amp=exposure_amp, outcome_trend=outcome_trend,
                                    outcome_amp = outcome_amp, rr=rr, start.date = start.date)
    fits <- rep_df %>% purrr::map(fit_mods, model=model, df_year=df_year)
    power <- fits %>% purrr::map(power_beta)
  }
  powervec <- rep(0, length(power))
  for (i in 1:length(power)){
  powervec[i] <- power[[i]][,1]
  }
  dat <- data.frame(values=values, power=powervec)
  if(plot == TRUE){
    my_plot <- ggplot2::ggplot(dat, ggplot2::aes_(x = ~ dat$values, y = ~ dat$power)) +
      ggplot2::geom_line() + ggplot2::theme_minimal() +
      ggplot2::xlab(varying) + ggplot2::ylab("power") +
      ggplot2::ylim(0, 1)
    print(my_plot)
  }
  return(dat)
}
