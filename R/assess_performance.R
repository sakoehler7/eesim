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
#' fits <- fit_mods(outcome = sims, model = "spline", df_year = 1)
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
#'                     exposure_trend = "cos1",
#'                     exposure_amp = 0.6,
#'                     average_outcome = 20,
#'                     outcome_trend = "no trend",
#'                     rr = 1.01)
#' fits <- fit_mods(outcome = sims, model = "spline", df_year = 1)
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
#' fits <- fit_mods(outcome = sims, model = "spline", df_year = 1)
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
#' fits <- fit_mods(outcome = sims, model = "spline", df_year = 1)
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
#' This function gives the power for a model with varying parameters.
#'
#' @param varying A character string of the parameter to be varied.  Choices are
#'    "n" or "rr"
#' @param values A numeric vector of the chosen values of the varying parameters
#' @param plot "TRUE" or "FALSE" for whether to produce a plot
#' @inheritParams power_beta
#'
#' @return Data frame with the value of the varying parameter and its
#'    corresponding power
#'
#' @examples
#' power_calc(varying = "n", values = c(50 * (1:5)), n_reps = 50,
#'            model = "spline_mod", rr = 1.02)
#' power_calc(varying = "rr", values = c(1.002, 1.005, 1.01, 1.02, 1.03, 1.05,
#'            1.1), n_sims = 100, model = "spline_mod", n = 365, plot = TRUE)
#'
#' @export
power_calc <- function(varying, values, plot = FALSE, ...){
  out <- data.frame(x = values, power = NA)
  if(varying == "n"){
    for(i in 1:nrow(out)){
      rep_df <- create_sims(n = out$x[i], ...)
      fits <- fit_mods(outcome = rep_df, ...)
      out$power[i] <- power_beta(fits)[1,1]
    }
  } else if(varying == "rr"){
    for(i in 1:nrow(out)){
      rep_df <- create_sims(rr = out$x[i], ...)
      fits <- fit_mods(outcome = rep_df, ...)
      out$power[i] <- power_beta(fits)[1,1]
    }
  }
  if(plot == TRUE){
    my_plot <- ggplot2::ggplot(out, ggplot2::aes_(x = ~ x, y = ~ power)) +
      ggplot2::geom_line() + ggplot2::theme_minimal() +
      ggplot2::xlab(varying)
    print(my_plot)
  }

  colnames(out)[1] <- varying
  return(out)
}
