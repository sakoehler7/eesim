#' Constant Baseline
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
#'
#' Simulated Exposure and Outcome Data
#'
#' This function simulates data for exposure levels and health outcomes by date.
#'
#' @inheritParams constant_baseline
#' @inheritParams calc_t
#' @inheritParams season_binexp
#' @inheritParams season_contexp
#' @param rr A numeric value specifying the relative risk of the outcome for each 1-unit increase in exposure
#' @param x_type A character string specifying the type of exposure data.  Options are "binary" and "continuous".
#'
#' @return A data frame with the date, exposure value, expected baseline outcome,
#' expected outcome, and simulated outcome for each day of simulated data.
#'
#' @examples
#' sim_data(n=10, rr = 1.1, lambda = 100, p = .5)
#'
#' @export
#'
sim_data <- function(n, rr, lambda, x_type = "binary", trend = "constant",
                     start.date = "2000-01-01", ...){

  require(dplyr)

  if(trend=="constant"){
    if(x_type == "binary"){
      x <- binary_exposure(n=n, ...)
    }
    else if(x_type == "continuous"){
      x <- continuous_exposure(n=n, ...)
    }
    exp_base_y <- constant_baseline(n=n, lambda = lambda, start.date = start.date)
    df <- full_join(x, exp_base_y, by="date") %>% mutate(exp_y =
                                                           exp(log(exp_base_y)+log(rr)*x))
    df$y <- sapply(df$exp_y, FUN = function(x) rpois(1,x))
  }

  else if(trend != "constant"){
    if(x_type == "binary"){
      x <- season_binexp(n = n, ...)
    }
    else if(x_type == "continuous"){
      x <- season_contexp(n = n, trend = trend, ...)
    }
    exp_base_y <- seasonal_baseline(n = n, lambda = lambda, start.date = start.date,
                                    trend = trend)
    df <- full_join(x, exp_base_y, by="date") %>% mutate(exp_y =
                                                           exp(log(exp_base_y)+log(rr)*x))
    df$y <- sapply(df$exp_y, FUN = function(x) rpois(1,x))
  }
  return(df)
}

#'
#'
#'
#'
#'
#'
#'
#'
