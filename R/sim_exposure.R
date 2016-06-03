#' Simulate binary exposure data
#'
#' This function simulated binary exposure data.
#'
#' @param n A numeric value giving the number of days to simulate.
#' @param p A numeric value giving the baseline probability of exposure for
#'    a binary exposure.
#'
#' @return A numeric vector with simulated exposure of length n.
#'
#' @examples
#' season_binexp(5, 0.25)
#'
#' @export
season_binexp <- function(n, p){
  p <- p #Change this later to reflect probability varying by season using trends
  binexp <- sample(c(0,1), size = n, replace = T, prob = c(1-p, p))
  return(binexp)
}

#' Simulate continuous exposure data
#'
#' This function simulates a time series of continuous exposure data.
#'
#' @param mu A numeric vector giving the average of the exposure distribution.
#' @param sd A numeric vector giving the standard deviation of the exposure
#'    distribution.
#' @param trend A character string that gives the trend function to use. Options
#'    include ...
#' @inheritParams season_binexp
#'
#' @return A numeric vector with simulated exposure of length n.
#'
#' @examples
#' season_contexp(n = 5, mu = 100, sd = 10, trend = "cos1")
#'
#' @export
season_contexp <- function(n, mu, sd, trend){
  day <- c(1:n)
  if (trend=="cos1"){
    mu <- mu*(1+.6*cos(2*pi*(day/365)))
  }
  if (trend == "cos2"){
    mu <- mu*(1+.6*cos(2*pi*(day/365)) + ifelse(day<639 & day>274, .4*cos(2*(pi*(day/365))), 0))
  }
  if (trend == "cos3"){
    mu <- mu*(1+.75^(day/365)*.6*cos(2*pi*(day/365)))
  }
  if (trend == "linear"){
    mu <- mu* (1+ (day/n))
  }
  if (trend == "curvilinear"){
    mu <- mu*(1+day*(2/n)+day^2*(-1/n^2))
  }
  if (trend == "cos1linear"){
    mu <- mu*((1+(day/n)) *(1+.6*cos(2*pi*(day/365))))
  }
  if (trend == "cvd"){ #Figure this out later.
  } #else{
  #stop("Specify a trend variable from cos1, cos2, cos3, linear, curvilinear, cos1linear, or cvd."
  #)}
  contexp <- rnorm(n, mean=mu, sd=sd)
  return(contexp)
}
