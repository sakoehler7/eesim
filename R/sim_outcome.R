#' Expected baseline health outcomes
#'
#' Generates expected baseline health outcomes.
#'
#' @param n A numeric value specifying the number of days for which to simulate
#'    data
#' @param lambda A numeric value specifying the mean for the expected outcomes
#' @param start.date A date in the format "yyyy-mm-dd" specifying the first day
#'    for which to simulate data
#' @inheritParams std_exposure
#'
#' @return A data frame with the date and expected baseline outcomes for each
#'    day of simulated data
#'
#' @examples
#' sim_baseline(n = 5, lambda = 100, trend = "cos1")
#'
sim_baseline <- function(n, lambda, trend = "no trend", amp = .6, start.date = "2000-01-01"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- calc_t(n = n, trend = trend, amp = amp)
  exp_base_y <- lambda * t
  df <- data.frame(date, exp_base_y)
  return(df)
}

