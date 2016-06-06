#' Simulate binary exposure data
#'
#' This function simulates binary exposure data.
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
  t <- calc_t(n, trend)
  mu <- mu*t
  contexp <- rnorm(n, mean=mu, sd=sd)
  return(contexp)
}
