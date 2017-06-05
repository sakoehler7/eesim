#' Create a continuous exposure trend vector
#'
#' Creates a trend vector for a continuous exposure.
#'
#' @param n A non-negative integer specifying the number of days to simulate.
#' @param trend A character string that specifies the desired trend function.
#'    Options are:
#'    \itemize{
#'      \item{"no trend": No trend, either seasonal or long-term (default).}
#'      \item{"cos1": A seasonal trend only.}
#'      \item{"cos2": A seasonal trend with variable amplitude across years.}
#'      \item{"cos3": A seasonal trend with steadily decreasing amplitude over time.}
#'      \item{"linear": A linear long-term trend with no seasonal trend.}
#'      \item{"curvilinear": A curved long-term trend with no seasonal trend.}
#'      \item{"cos1linear": A seasonal trend plus a linear long-term trend.}
#'      }
#'    See the package vignette for examples of the shapes of these trends.
#' @param slope A numeric value specifying the slope of the trend, to be used
#'    with \code{trend = "linear"} or \code{trend = "cos1linear"}.
#' @param amp A numeric value specifying the amplitude of the seasonal trend.
#'    Must be between -1 and 1.
#' @param custom_func An R object specifying a customized function from
#'    which to create a trend variable. Must accept the arguments \code{n}
#'    and \code{mean}.
#' @param ... Optional arguments to a custom trend function
#'
#' @return A numeric vector of simulated exposure values for each study day, to be
#'    used to generate data with seasonal trends.
#'
#' @examples
#' calc_t(5, "cos3", amp = .5)
#'
#' @export
calc_t <- function(n, trend = "no trend", slope=1, amp = .6, custom_func = NULL, ...){
  day <- c(1:n)
  if (!is.null(custom_func)) {
    arguments <- list(...)
    arguments$n <- n
    seasont <- do.call(custom_func, arguments)
  }
  else if (trend == "cos1"){
    seasont <- 1 + amp * cos(2 * pi * (day / 365))
    } else if (trend == "cos2"){
    seasont <- 1 + amp * cos(2 * pi * (day / 365)) +
      ifelse(day < 639 & day > 274, .4 * cos(2 * (pi * (day / 365))), 0)
    } else if (trend == "cos3"){
      seasont <- 1 + .75 ^ (day / 365) * amp * cos(2 * pi * (day / 365))
    } else if (trend == "linear"){
      seasont <- 1 + slope*(day / n)
    } else if (trend == "curvilinear"){
      seasont <- 1 + day * (2 / n) + day^2 * (-1 / n^2)
    } else if (trend == "cos1linear"){
      seasont <- (1 + slope*(day / n)) * (1 + amp * cos(2 * pi * (day / 365)))
    } else if (trend == "no trend"){
      seasont <- rep(1, n)
    } else {
      stop(paste0("`trend` value is not a valid choice. Please check the",
                  " function documentation to select a valid option."))
    }
  seasont <- seasont / mean(seasont)
  return(seasont)
}

#' Create a binary exposure trend vector
#'
#' Creates a trend vector for binary exposure data, centered at a probability \code{p}.
#'
#' @param trend A character string that gives the trend function to use. Options
#'    are:
#'   \itemize{
#'      \item{"no trend": No trend, either seasonal or long-term (default).}
#'      \item{"cos1": A seasonal trend only.}
#'      \item{"cos2": A seasonal trend with variable amplitude across years.}
#'      \item{"cos3": A seasonal trend with steadily decreasing amplitude over time.}
#'      \item{"linear": A linear long-term trend with no seasonal trend.}
#'      \item{"monthly": Uses a user-specified probability of exposure for each month.}
#'    }
#'
#' @param p A numeric value between 0 and 1 giving the mean probability of exposure
#'    across study days.
#' @param slope A numeric value specifying the slope of the trend, to be used
#'    with \code{trend = "linear"} or \code{trend = "cos1linear"}.
#' @param amp A numeric value specifying the amplitude of the seasonal trend.
#'    Must be between -.5 and .5.
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin
#'    simulating values.
#' @param custom_func An R object specifying a customized function from
#'    which to create a trend variable. Must accept arguments \code{n} and
#'    \code{p}.
#' @inheritParams calc_t
#'
#' @return A numeric vector of daily expected probability of exposure, to be used
#'    to generate binary exposure data with seasonal trends.
#'
#' @examples
#' bin_t(n = 5, p = .3, trend = "cos1", amp = .3)
#'
#' @export
bin_t <- function(n, p, trend = "no trend", slope = 1, amp = .01,
                  start.date = "2000-01-01", custom_func = NULL,...){
  day <- c(1:n)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  if (!is.null(custom_func)) {
    arguments <- list(...)
    arguments$n <- n
    arguments$p <- p
    seasont <- do.call(custom_func, arguments)
    return(seasont)
  }
  else if (trend == "monthly"){
  }
  else if (abs(p) > .5 & abs(amp) >1-p & !(trend == "no trend")){
    stop(paste0("For abs(p)>.5, amp must be between -(1-p) and 1-p."))
  }
  else if (abs(p)<.5 & abs(amp) >p & !(trend == "no trend")){
    stop(paste0("For abs(p)<.5, amp must be between -p and p."))
  }
  if (trend == "cos1"){
    seasont <- p + amp * cos(2 * pi * (day / 365))
  } else if (trend == "cos2"){
    seasont <- p + amp/2 * cos(2 * pi * (day / 365)) +
      ifelse(day < 639 & day > 274, amp/2 * cos(2 * (pi * (day / 365))), 0)
  } else if (trend == "cos3"){
    seasont <- p + .75 ^ (day / 365) * amp * cos(2 * pi * (day / 365))
  } else if (trend == "linear"){
    seasont <- ifelse(p*(1 + slope*day/n) <1, p*(1+slope*day/n), 1)
  } else if (trend == "monthly"){
    months <- lubridate::month(date)
    seasont <- p[months]
  } else if (trend == "no trend"){
    seasont <- rep(p, n)
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
#' @param cust_expdraw An R object name specifying a user-created function
#'    which determines the distribution of random noise off of the trend line.
#'    This function must have inputs "n" and "prob" and output a vector of simulated
#'    exposure values.
#' @param cust_expdraw_args A list of arguments other than \code{n} required
#'  by the \code{cust_expdraw} function.
#' @inheritParams bin_t
#'
#' @return A data frame with columns for the dates and daily exposure values for
#'  \code{n} days.
#'
#' @examples
#' binary_exposure(n = 5, p = 0.1, trend = "cos1", amp = .02,
#'                 start.date = "2001-02-01")
#' binary_exposure(n=10, p=.1, cust_expdraw=rnbinom,
#'                 cust_expdraw_args=list(size=10))
#'
#' @export
binary_exposure <- function(n, p, trend = "no trend", slope, amp=.05,
                            start.date = "2000-01-01", cust_expdraw=NULL,
                            cust_expdraw_args = list(), custom_func = NULL, ...){
  t <- bin_t(n=n, p=p, trend = trend, slope=slope, amp = amp, start.date = start.date,
             custom_func = custom_func)
  if(!is.null(cust_expdraw)){
    cust_expdraw_args$prob <- t
    cust_expdraw_args$n <- n
    x <- do.call(cust_expdraw, cust_expdraw_args)
  }
  else{
  x <- stats::rbinom(length(t), size = 1, prob = t)
  }
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  df <- data.frame(date, x)
  return(df)
}

#' Simulate continuous exposure data
#'
#' Simulates a time series of continuous exposure values with or without a
#' seasonal and / or long-term trend.
#'
#' @param mu A numeric value giving the mean exposure across all study days.
#' @param sd A numeric value giving the standard deviation of the exposure
#'    values from the exposure trend line.
#' @param cust_expdraw A character string specifying a user-created function
#'    which determines the distribution of random noise off of the trend line.
#'    This function must have inputs "n" and "mean" and output a vector of simulated
#'    exposure values.
#' @param cust_expdraw_args A list of arguments other than "n" and "mean" required
#'  by the cust_expdraw function.
#' @inheritParams calc_t
#' @inheritParams binary_exposure
#'
#' @return A data frame with the dates and simulated daily exposure values from \code{n} days.
#'
#' @examples
#' continuous_exposure(n = 5, mu = 100, sd = 10, trend = "cos1")
#' continuous_exposure(n=10, mu=3, trend="linear", slope = 2,
#'                     cust_expdraw=rnorm, cust_expdraw_args = list(sd=.5))
#'
#' @export
continuous_exposure <- function(n, mu, sd = 1, trend = "no trend", slope, amp = .6,
                                cust_expdraw = NULL, cust_expdraw_args=list(),
                                start.date = "2000-01-01", ...){
  day <- c(1:n)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- calc_t(n=n, trend=trend, slope=slope, amp=amp, ...)
  newmu <- mu * t
  if(!is.null(cust_expdraw)){
    cust_expdraw_args$mean <- newmu
    cust_expdraw_args$n <-n
    x <- do.call(cust_expdraw, cust_expdraw_args)
  }
  else {
  x <- stats::rnorm(n, newmu, sd)
  }
  df <- data.frame(date, x)
  return(df)
}

#' Simulate exposure data using default methods
#'
#' Simulates binary or continuous exposure data with or without seasonal trends
#' using default functions.
#'
#' @param central A numeric value specifying the mean probability of exposure
#'    (for binary data) or the mean exposure value (for continuous data).
#' @param exposure_type A character string specifying the type of exposure.
#'    Choices are "binary" or "continuous".
#' @inheritParams continuous_exposure
#' @inheritParams binary_exposure
#'
#' @return A data frame with two columns: date (\code{date}) and simulated
#'    exposure values (\code{x}).
#'
#' @examples
#' std_exposure(n = 5, central = .1, trend = "cos1", amp = .02)
#' std_exposure(n = 5, central = 50, sd = 5, trend = "cos3", amp = .6,
#'              exposure_type = "continuous", start.date = "2001-04-01")
#' std_exposure(n=50, central=.1, amp=.05,cust_expdraw=rnbinom,
#'              cust_expdraw_args=list(size=10))
#'
#' @export
std_exposure <- function(n, central, sd = NULL, trend = "no trend",
                         exposure_type = "binary", slope, amp,
                         start.date = "2000-01-01", ...){
  if(exposure_type=="binary"){
    p <- central
    df <- binary_exposure(n=n, p=p, trend=trend, slope=slope, amp=amp,
                          start.date=start.date, ...)
  }
  else if(exposure_type == "continuous"){
    mu <- central
    df <- continuous_exposure(n=n, mu=mu, sd =sd, trend=trend, slope=slope,
                              amp=amp, start.date=start.date, ...)
  }
  return(df)
}

#' Expected baseline health outcomes
#'
#' Generates expected baseline health outcome counts based on average outcome and
#' desired seasonal and / or long-term trends.
#'
#' @param n A numeric value specifying the number of days for which to simulate
#'    data
#' @param lambda A numeric value specifying the mean for the expected outcomes
#' @param start.date A date in the format "yyyy-mm-dd" specifying the first day
#'    for which to simulate data
#' @inheritParams std_exposure
#'
#' @return A data frame with the date and expected baseline outcome count for each
#'    day of simulated data.
#'
#' @examples
#' sim_baseline(n = 5, lambda = 100, trend = "cos1")
#'
#' @export
sim_baseline <- function(n, lambda, trend = "no trend", slope=1,  amp = .6,
                         start.date = "2000-01-01"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  t <- calc_t(n = n, trend = trend, slope=slope, amp = amp)
  baseline <- lambda * t
  df <- data.frame(date, baseline)
  return(df)
}


