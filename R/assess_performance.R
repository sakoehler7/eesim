#' Average Estimated Coefficient
#'
#' This function gives the mean value of the $\hat{\beta}$s and the mean estimated
#' relative risk over the n simulations.
#'
#' @param df A data frame of replicated simulations which must include a column titled "est"
#'
#' @return A data frame with the mean estimated coefficient and mean estimated relative risk
#'
#' @example
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
#' @param df A data frame of replicated simulations which must include columns titles "est" and "se".
#'
#' @return A data frame of the variance across $\hat{\beta}$s and the mean variance of the $\hat{\beta}$s
#'
#' @example
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
#'
#'
#'
