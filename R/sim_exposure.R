#' Simulate binary exposure data without a seasonal trend
#'
#' This function generates binary exposure data without a seasonal trend.
#'
#' @inheritParams season_binexp
#' @param p A numeric value between 0 and 1 specifying the probability of
#'    exposure.
#' @param start.date A date in the format "yyyy-mm-dd" specifying the first day
#'    of simulated measurements.
#'
#' @return A data frame with the date and a 0 or 1 exposure value for each day.
#'
#' @examples
#' binary_exposure(n = 5, p = 0.25)
#'
#' @export
binary_exposure <- function(n, p, start.date = "2000-01-01", ...){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  x <- rbinom(n, 1, p)
  df <- data.frame(date, x)
  return(df)
}

#' Simulate continuous exposure data without a seasonal trend
#'
#' This function simulates continuous exposure data without a seasonal trend.
#'
#' @inheritParams season_binexp
#' @inheritParams binary_exposure
#' @inheritParams season_contexp
#'
#' @return A data frame with date and exposure value for each day.
#'
#' @examples
#' continuous_exposure(n = 5, mu = 10, sd = 10)
#'
#' @export
#'
continuous_exposure <- function(n, mu, sd, start.date = "2000-01-01", ...){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  x <- rnorm(n, mean = mu, sd = sd)
  df <- data.frame(date, x)
  return(df)
}

#' Create a trend variable
#'
#' This function creates a trend variable.
#'
#' @param n A numeric value giving the number of days to simulate.
#' @param trend A character string that gives the trend function to use. Options
#'    include:
#'    \itemize{
#'      \item{"cos1"}
#'      \item{"cos2"}
#'      \item{"cos3"}
#'      \item{"linear"}
#'      \item{"curvilinear"}
#'      \item{"cos1linear"}
#'      \item{"no trend"}
#'    }
#'
#' @param amp A numeric value giving the amplitude of the seasonal trend. Must be between 0 and 1.
#'
#' @return A numeric vector used to generate data with seasonal trends.
#'
#' @examples
#' calc_t(5, "cos3")
#'
#' @export
calc_t <- function(n, trend = "no trend", amp = .6, custom_func = NULL, ...){
  day <- c(1:n)
  if (trend == "cos1"){
    seasont <- 1 + amp * cos(2 * pi * (day / 365))
    } else if (trend == "cos2"){
    seasont <- 1 + amp * cos(2 * pi * (day / 365)) +
      ifelse(day < 639 & day > 274, .4 * cos(2 * (pi * (day / 365))), 0)
    } else if (trend == "cos3"){
      seasont <- 1 + .75 ^ (day / 365) * amp * cos(2 * pi * (day / 365))
    } else if (trend == "linear"){
      seasont <- 1 + (day / n)
    } else if (trend == "curvilinear"){
      seasont <- 1+ day * (2 / n) + day^2 * (-1 / n^2)
    } else if (trend == "cos1linear"){
      seasont <- (1 + (day / n)) * (1 + amp * cos(2 * pi * (day / 365)))
    } else if (trend == "no trend"){
      seasont <- 1
    } else if (trend == "custom" & !is.null(custom_func)) {
      arguments <- list(...)
      arguments$n <- n
      seasont <- do.call(custom_func, arguments)
    } else {
      stop(paste0("`trend` value is not a valid choice. Please check the",
                  " function documentation to select a valid option."))
    }
  seasont <- seasont / mean(seasont)
  return(seasont)
}
#'
#' Create a trend variable for binary exposure data
#'
#' This function creates a trend variable for binary exposure data which is centered at p and
#' restricts the probability of exposure between 0 and 1.
#'
#' @inheritParams calc_t
#' @param p A numeric value giving the mean probability of exposure
#'
#' @return A numeric vector used to generate binary exposure data with seasonal trends
#'
#' @examples
#' bin_t(n = 5, p = .3, trend = "cos1", amp = .3)
#'
#' @export
#'
bin_t <- function(n, p, trend = "no trend", amp = .2, custom_func = NULL, ...){
  day <- c(1:n)
  if (p > .5 & amp >1-p){
    stop(paste0("For p>.5, amp must be between 0 and 1-p."))
  }
  if (p < .5 & amp>p){
    stop(paste0("For p<.5, amp must be between 0 and p."))
  }
  if (trend == "cos1"){
    seasont <- p + amp * cos(2 * pi * (day / 365))
  } else if (trend == "cos2"){
    seasont <- p + amp/2 * cos(2 * pi * (day / 365)) +
      ifelse(day < 639 & day > 274, amp/2 * cos(2 * (pi * (day / 365))), 0)
  } else if (trend == "cos3"){
    seasont <- p + .75 ^ (day / 365) * amp * cos(2 * pi * (day / 365))
  } else if (trend == "linear"){
    seasont <- p+1/(day/n)
  } else if (trend == "curvilinear"){
    seasont <- p + day * (2 / n) + day^2 * (-1 / n^2)
  } else if (trend == "cos1linear"){
    seasont <- (p + (day / n)) * (p + amp * cos(2 * pi * (day / 365)))
  } else if (trend == "no trend"){
    seasont <- p
  } else if (trend == "custom" & !is.null(custom_func)) {
    arguments <- list(...)
    arguments$n <- n
    seasont <- do.call(custom_func, arguments)
  } else {
    stop(paste0("`trend` value is not a valid choice. Please check the",
                " function documentation to select a valid option."))
  }
  return(seasont)
}
#'
#' Simulate binary exposure data with a seasonal trend
#'
#' This function simulates binary exposure data with a seasonal trend.
#'
#' @param n A numeric value giving the number of days to simulate.
#' @param p A numeric value giving the baseline probability of exposure for
#'    a binary exposure.
#'
#' @return A numeric vector with simulated exposure of length n.
#'
#' @examples
#' season_binexp(n = 5, p = 0.25)
#'
#' @export
#'
season_binexp <- function(n, p, trend, amp=.2, start.date = "2000-01-01", ...){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- bin_t(n, p, trend, amp)
  for (i in 1:n){
    x <- sample(c(0, 1), size = 1, replace = T, prob = c(1 - t[i], t[i]))
  }
  df <- data.frame(date, x)
  return(df)
}

#' Simulate continuous exposure data with a seasonal trend
#'
#' This function simulates a time series of continuous exposure data with a
#' seasonal trend.
#'
#' @param mu A numeric vector giving the average of the exposure distribution.
#' @param sd A numeric vector giving the standard deviation of the exposure
#'    distribution.
#' @inheritParams calc_t
#' @inheritParams season_binexp
#'
#' @return A numeric vector with simulated exposure of length n.
#'
#' @examples
#' season_contexp(n = 5, mu = 100, sd = 10, trend = "cos1")
#'
#' @export
season_contexp <- function(n, mu, sd, trend, amp, start.date = "2000-01-01", ...){
  day <- c(1:n)
  t <- calc_t(n, trend, amp)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  mu <- mu * t
  x <- rnorm(n, mean = mu, sd = sd)
  df <- data.frame(date, x)
  return(df)
}
