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
      seasont <- rep(1, n)
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
#' @param trend A character string that gives the trend function to use. Options
#'    include:
#'    \itemize{
#'      \item{"cos1"}
#'      \item{"cos2"}
#'      \item{"cos3"}
#'      \item{"linear"}
#'      \item{"monthly"}
#'      \item{"no trend"}
#'    }
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
bin_t <- function(n, p, trend = "no trend", amp = .01, start.date = "2000-01-01", custom_func = NULL, ...){
  day <- c(1:n)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  if (trend == "monthly"){

  }
  else if (p > .5 & amp >1-p){
    stop(paste0("For p>.5, amp must be between 0 and 1-p."))
  }
  else if (p < .5 & amp>p){
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
    seasont <- ifelse(p*(1 + day/n) <1, p*(1+day/n), 1)
  } else if (trend == "monthly"){
    require(lubridate)
    months <- month(date)
    seasont <- p[months]
  } else if (trend == "no trend"){
    seasont <- rep(p, n)
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
#' Simulate binary exposure data
#'
#' This function simulates binary exposure data with or without seasonal trends.
#'
#' @inheritParams bin_t
#'
#' @return A numeric vector with simulated exposure of length n.
#'
#' @examples
#' binary_exposure(n = 5, p = 0.25, trend = "cos1")
#'
#' @export
#'
binary_exposure <- function(n, p, trend = "no trend", amp, start.date = "2000-01-01"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- bin_t(n, p, trend, amp)
  x <- rbinom(length(t), size = 1, prob = t)
  df <- data.frame(date, x)
  return(df)
}

#' Simulate continuous exposure data
#'
#' This function simulates a time series of continuous exposure data with or without a
#' seasonal trend.
#'
#' @param mu A numeric vector giving the average of the exposure distribution.
#' @param sd A numeric vector giving the standard deviation of the exposure
#'    values around their expected values.
#' @inheritParams calc_t
#' @inheritParams season_binexp
#'
#' @return A numeric vector with simulated exposure of length n.
#'
#' @examples
#' continuous_exposure(n = 5, mu = 100, sd = 10, trend = "cos1")
#'
#' @export
continuous_exposure <- function(n, mu, sd, trend = "no trend", amp, start.date = "2000-01-01", ...){
  day <- c(1:n)
  t <- calc_t(n, trend, amp)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  mu <- mu * t
  x <- rnorm(n, mean = mu, sd = sd)
  df <- data.frame(date, x)
  return(df)
}
