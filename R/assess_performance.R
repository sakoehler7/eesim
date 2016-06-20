#' Average Estimated Coefficient
#'
#' This function gives the mean value of the $\hat{\beta}$s and the mean estimated
#' relative risk over the n simulations.
#'
#' @param df A data frame of replicated simulations which must include a column titled "est"
#'
#' @return A data frame with the mean estimated coefficient and mean estimated relative risk
#'
#' @examples
#' mean_beta(df)
#'
#' @export
#'
mean_beta <- function(df){
  beta_hat <- mean(df$est)
  rr_hat <- mean(exp(df$est))
  out <- data.frame(beta_hat, rr_hat)
  return(out)
}
#'
#' Standard Deviation of Estimated Coefficients
#'
#' This function gives the variance of the point estimates of $\hat{\beta}$ over the n simulations
#' and the mean of the variances of each $\hat{\beta}$.
#'
#' @param df A data frame of replicated simulations which must include columns titled "est" and "se".
#'
#' @return A data frame of the variance across $\hat{\beta}$s and the mean variance of the $\hat{\beta}$s
#'
#' @examples
#' beta_var(df)
#'
#' @export
#'
beta_var <- function(df){
  var_across_betas <- var(df$est)
  mean_beta_var <- mean(df$se^2)
  out <- data.frame(var_across_betas, mean_beta_var)
  return(out)
}
#'
#' Percent Bias of Estimated Coefficient
#'
#' This function returns the relative bias of the mean of the estimated coefficients.
#'
#' @inheritParams mean_beta
#' @param true_rr The true relative risk used to simulate your data
#'
#' @return The percent bias of the mean of the estimated coefficients over n simulations
#'
#' @examples
#' beta_bias(df, true_rr = 1.02)
#'
beta_bias <- function(df, true_rr){
  percent_bias <- 100 * (true_rr - mean(exp(df$est))) / true_rr
  out <- data.frame(percent_bias)
  return(out)
}
#'
#' Percent Coverage of Estimated Coefficients
#'
#' This function gives the percent coverage of the true coefficient.
#'
#' @param df A data frame of replicated simulations which must include columns titled "lower_ci" and "upper_ci"
#' @param true_rr The true relative risk used to simulate your data
#'
#' @return The percent of confidence intervals for the estimated relative risk over n simulations which include the true relative risk
#'
#' @examples
#' coverage_beta(df, true_rr = 1.02)
#'
coverage_beta <- function(df, true_rr){
  true_beta <- log(true_rr)
  coverage <- df$lower_ci <= true_beta & df$upper_ci >= true_beta
  out <- data.frame(coverage = sum(coverage) / nrow(df))
  return(out)
}
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
