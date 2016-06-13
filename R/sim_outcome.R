#' Expected baseline outcomes for constant hazard rate
#'
#' This is a function to compute the expected baseline outcomes when the hazard
#' rate for exposure is constant.
#'
#' @param n A numeric vector specifying the number of days for which to simulate
#'    data
#' @param lambda A numeric value specifying the mean for the expected outcomes
#' @param start.date A date in the format "yyyy-mm-dd" specifying the first day
#'    for which to simulate data
#'
#' @return A data frame with the date and expected baseline value of outcomes
#'    for each day of simulated data
#'
#' @examples
#' constant_baseline(n = 3, lambda = 100)
#'
#' @export
#'
constant_baseline <- function(n, lambda, start.date = "2000-01-01", ...){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  exp_base_y <- rep(lambda, n)
  df <- data.frame(date = date,
                   exp_base_y = exp_base_y)
  return(df)
}

#' Expected baseline outcomes for seasonal hazard rate
#'
#' This function generates expected baseline outcomes when the hazard rate for
#' exposure is seasonal.
#'
#' @inheritParams constant_baseline
#' @inheritParams calc_t
#'
#' @return A data frame with the date and expected baseline outcomes for each
#'    day of simulated data
#'
#' @examples
#' seasonal_baseline(n = 3, lambda = 100, trend = "cos1")
#'
seasonal_baseline <- function(n, lambda, start.date = "2000-01-01",
                              trend = "no trend"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- calc_t(n = n, trend = trend)
  exp_base_y <- lambda * t
  df <- data.frame(date = date, exp_base_y = exp_base_y)
  return(df)
}

#' Simulate outcome data for binary exposure with a seasonal trend
#'
#' This function simulates outcome counts for binary exposure with a seasonal
#' trend.
#'
#' @inheritParams constant_baseline
#' @param lambda A numeric value specifying the mean outcome count per day
#' @param t A numeric vector for the trend variable resulting from the
#'    \code{calc_t} function
#' @param exposure A numeric vector of exposure values
#' @param rr A numeric value specifying the relative risk
#'
#' @return A data frame with the expected outcomes and simulated outcomes for
#'    each day
#'
#' @examples
#' exposure <- season_contexp(n = 5, mu = 100, sd = 10, trend = "cos1")
#' t <- calc_t(n = 5, trend = "no trend")
#' sim_binout(n = 5, lambda = 22, t = t, exposure = exposure, rr = 1.1)
#'
sim_binout <- function(n, lambda, t, exposure, rr){
  day <- c(1:n)
  rr <- ifelse(exposure == 1, rr, 1)
  exp_y <- log(lambda * t) + log(rr) * exposure
  y <- rpois(n, exp_y)
  df <- data.frame(exp_y = exp_y, y = y)
  return(df)
}

#' Simulate outcomes for continuous exposure with a seasonal trend
#'
#' This function simulates outcome counts for continuous exposure with a
#' seasonal trend.
#'
#' @inheritParams sim_binout
#' @param mu A numeric value specifying the mean
#'
#' @return A data frame with the expected outcome and simulated outcome for each
#'    day of simulated data
#'
sim_contout<- function(n, mu, t, exposure, rr){
  day <- c(1:n)
  exp_y <- mu * t * rr * (exposure / sd(exposure))
  y <- rpois(n, exp_y)
  df <- data.frame(exp_y = exp_y, y = åy)
  return(df)
}
