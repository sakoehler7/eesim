#' Create a trend variable
#'
#' Creates a trend variable for continuous exposures.
#'
#' @param n A numeric value specifying the number of days to simulate.
#' @param trend A character string that specifies the desired trend function.
#'    Options are:
#'    \itemize{
#'      \item{"cos1"}
#'      \item{"cos2"}
#'      \item{"cos3"}
#'      \item{"linear"}
#'      \item{"curvilinear"}
#'      \item{"cos1linear"}
#'      \item{"no trend"}
#'      \item{"custom"}
#'    }
#'
#' @param amp A numeric value specifying the amplitude of the seasonal trend.
#'    Must be between 0 and 1.
#' @param custom_func A character string specifying a customized function from
#'    which to create a trend variable
#' @param ... optional arguments to a custom trend function
#'
#' @return A numeric vector used to generate data with seasonal trends.
#'
#' @examples
#' calc_t(5, "cos3", amp = .5)
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

#' Create a trend variable
#'
#' Creates a trend variable for binary exposure data which is centered at p.
#'
#' @param trend A character string that gives the trend function to use. Options
#'    are:
#'    \itemize{
#'      \item{"cos1"}
#'      \item{"cos2"}
#'      \item{"cos3"}
#'      \item{"linear"}
#'      \item{"monthly"}
#'      \item{"no trend"}
#'      \item{"custom"}
#'    }
#'
#' @param p A numeric value giving the mean probability of exposure
#' @param amp A numeric value specifying the amplitude of the seasonal trend.
#'    Must be between 0 and .5.
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin
#'    simulating values
#' @inheritParams calc_t
#'
#' @return A numeric vector used to generate binary exposure data with seasonal
#'    trends.
#'
#' @examples
#' bin_t(n = 5, p = .3, trend = "cos1", amp = .3)
#'
#' @export
bin_t <- function(n, p, trend = "no trend", amp = .01,
                  start.date = "2000-01-01", custom_func = NULL,...){
  day <- c(1:n)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  if (trend == "monthly"){
  }
  else if (p > .5 & amp >1-p){
    stop(paste0("For p>.5, amp must be between 0 and 1-p."))
  }
  else if (p<.5 & amp >p){
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
    months <- lubridate::month(date)
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

#' Simulate binary exposure data
#'
#' Simulates a time series of binary exposure values with or without seasonal
#' trends.
#'
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin
#'    simulating daily exposures
#' @inheritParams bin_t
#'
#' @return A data frame with the dates and daily exposure values from n days
#'
#' @examples
#' binary_exposure(n = 5, p = 0.1, trend = "cos1", amp = .02,
#'                 start.date = "2001-02-01")
#'
#'
#' @export
binary_exposure <- function(n, p, trend = "no trend", amp,
                            start.date = "2000-01-01", custom_func = NULL, ...){
  t <- bin_t(n=n, p=p, trend = trend, amp = amp, start.date = start.date,
             custom_func = custom_func)
  x <- stats::rbinom(length(t), size = 1, prob = t)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  df <- data.frame(date, x)
  return(df)
}

#' Simulate continuous exposure data
#'
#' Simulates a time series of continuous exposure values with or without a
#' seasonal trend.
#'
#' @param mu A numeric value giving the mean exposure.
#' @param sd A numeric value giving the standard deviation of the exposure
#'    values around the trend line.
#' @inheritParams calc_t
#'
#' @return A data frame with the dates and daily exposure values from n days
#'
#' @examples
#' continuous_exposure(n = 5, mu = 100, sd = 10, trend = "cos1")
#'
#' @export
continuous_exposure <- function(n, mu, sd=1, trend = "no trend", amp = .6,
                                start.date = "2000-01-01", ...){
  day <- c(1:n)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- calc_t(n, trend, amp, start.date, ...)
  newmu <- mu * t
  x <- stats::rnorm(n, newmu, sd)
  df <- data.frame(date, x)
  return(df)
}

#' Simulate exposure data
#'
#' Simulates binary or continuous exposure data with or without seasonal trends.
#'
#' @param central A numeric value specifying the mean probability of exposure
#'    (for binary data) or the mean exposure value (for continuous data)
#' @param exposure_type A character string specifying the type of exposure.
#'    Choices are "binary" or "continuous".
#'
#' @inheritParams continuous_exposure
#'
#' @return A data frame with the dates and daily exposure values from n days
#'
#' @examples
#' std_exposure(n=5, central = .1, trend = "cos1", amp = .02)
#' std_exposure(n = 5, central = 50, sd = 5, trend = "cos3", amp = .6,
#'              exposure_type = "continuous", start.date = "2001-04-01")
#'
#' @export
std_exposure <- function(n, central, sd = NULL, trend = "no trend",
                         exposure_type = "binary", amp,
                         start.date = "2000-01-01"){
  if(exposure_type=="binary"){
    p <- central
    df <- binary_exposure(n=n, p=p, trend=trend, amp=amp, start.date=start.date)
  }
  else if(exposure_type == "continuous"){
    mu <- central
    df <- continuous_exposure(n=n, mu=mu, sd =sd, trend=trend, amp=amp,
                              start.date=start.date)
  }
  return(df)
}

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
#' @export
sim_baseline <- function(n, lambda, trend = "no trend", amp = .6,
                         start.date = "2000-01-01"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- calc_t(n = n, trend = trend, amp = amp)
  baseline <- lambda * t
  df <- data.frame(date, baseline)
  return(df)
}


