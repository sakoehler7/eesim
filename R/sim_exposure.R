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
#' @return A numeric vector used to generate data with seasonal trends.
#'
#' @examples
#' calc_t(5, "cos3")
#'
#' @export
calc_t <- function(n, trend = "no trend", custom_func = NULL, ...){
  day <- c(1:n)
  if (trend == "cos1"){
    seasont <- 1 + .6 * cos(2 * pi * (day / 365))
    } else if (trend == "cos2"){
    seasont <- 1 + .6 * cos(2 * pi * (day / 365)) +
      ifelse(day < 639 & day > 274, .4 * cos(2 * (pi * (day / 365))), 0)
    } else if (trend == "cos3"){
      seasont <- 1 + .75 ^ (day / 365) * .6 * cos(2 * pi * (day / 365))
    } else if (trend == "linear"){
      seasont <- 1 + (day / n)
    } else if (trend == "curvilinear"){
      seasont <- 1+ day * (2 / n) + day^2 * (-1 / n^2)
    } else if (trend == "cos1linear"){
      seasont <- (1 + (day / n)) * (1 + .6 * cos(2 * pi * (day / 365)))
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
season_binexp <- function(n, p, start.date = "2000-01-01", ...){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  p <- p #Change this later to reflect probability varying by season using trends
  x <- sample(c(0, 1), size = n, replace = T, prob = c(1 - p, p))
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
season_contexp <- function(n, mu, sd, trend, start.date = "2000-01-01", ...){
  day <- c(1:n)
  t <- calc_t(n, trend)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  mu <- mu * t
  x <- rnorm(n, mean = mu, sd = sd)
  df <- data.frame(date, x)
  return(df)
}
