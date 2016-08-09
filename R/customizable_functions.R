#' Pulls exposure series from Chicago NMMAPS data set
#'
#' @examples
#' custom_exposure(n = 5, metric = "temp")
custom_exposure <- function(n, df = dlnm::chicagoNMMAPS, central = NA,
                            metric = "temp"){
  exposure <- df[1:n, metric]
  return(exposure)
}

#' Simulate random series of exposure values
#'
#' @examples
#' sim_exposure(n = 5, central = 0.25, exposure_type = "binary", amp = .02)
#' sim_exposure(n = 5, central = 100, sd = 10, amp = .6,
#'              exposure_type = "continuous")
#' sim_exposure(n = 5, central = NA, custom_func = "custom_exposure",
#'                     metric = "temp")
#'
#' @export
sim_exposure <- function(n, central = NULL, trend = "no trend", amp = .6,
                         exposure_type = NULL,
                         start.date = "2001-01-01", custom_func = NULL, ...){
  arguments <- list(...)
  arguments$n <- n
  arguments$central <- central
  if(is.null(custom_func)){
    arguments$trend <- trend
    arguments$amp <- amp
    arguments$exposure_type <- exposure_type
    exposure <- do.call("std_exposure", arguments)
  } else if (!(is.null(custom_func))){
    start.date <- as.Date(start.date)
    date <- seq(from = start.date, by = 1, length.out = n)
    x <- do.call(custom_func, arguments)
    exposure <- data.frame(date, x)
  } else {
    stop(paste0("If a custom function is not used to simulate randomness in the
                exposure variable, then the parameters central and
                exposure_type must be specified."))
  }
  return(exposure)
}


#' Pull smoothed Chicago NMMAPS mortality data
#'
#' @examples
#' custom_baseline(n = 5)
#' custom_baseline(n = 5, outcome_type = "death")
custom_baseline <- function(n, df = dlnm::chicagoNMMAPS, average_outcome = NA,
                            trend = NA, outcome_type = "cvd",
                            start.date = "2000-01-01"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  df$outcome <- df[ , outcome_type]
  smooth_mod <- glm(outcome ~ splines::ns(time, 7 * 14), data = df)
  baseline <- predict(smooth_mod)[1:n]
  df2 <- data.frame(date, baseline)
  return(df2)
}

#' Create a series of baseline outcomes
#'
#' @examples
#' create_baseline(n = 5, average_outcome = 22, trend = "linear")
#' create_baseline(n = 5, average_outcome = NA, trend = NA, amp = NA,
#'                 custom_func = "custom_baseline", outcome_type = "death")
#'
create_baseline <- function(n, average_outcome, trend, amp, custom_func = NULL,
                            ...){
  if(is.null(custom_func)){
    lambda <- average_outcome
    baseline <- sim_baseline(n=n, lambda=lambda, trend=trend, amp=amp)
  } else {
    arguments <- list(...)
    arguments$n <- n
    arguments$average_outcome <- average_outcome
    arguments$trend <- trend
    baseline <- do.call(custom_func, arguments)
  }
  return(baseline)
}

create_lambda <- function(baseline, exposure, rr, custom_func = NULL, ...){
  if(is.null(custom_func)){
    log_lambda <- log(baseline) + log(rr) * exposure
    lambda <- exp(log_lambda)
  } else {
    arguments <- list(...)
    arguments$baseline <- baseline
    arguments$exposure <- exposure
    arguments$rr <- rr
    lambda <- do.call(custom_func, arguments)
  }
  return(lambda)
}

#' @export

sim_outcome <- function(exposure, average_outcome = NULL, trend = "no trend",
                        amp = .6, rr = NULL, start.date="2000-01-01",
                        custom_func = NULL, ...){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = nrow(exposure))
  if(is.null(custom_func)){
    baseline <- create_baseline(n = nrow(exposure),
                                average_outcome = average_outcome,
                                trend = trend,
                                amp = amp)
    lambda <- create_lambda(baseline = baseline$exp_base_y,
                            exposure = exposure$x,
                            rr = rr)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  else {
    arguments <- list(...)
    arguments$n <- n
    arguments$average_outcome <- average_outcome
    arguments$trend <- trend
    arguments$baseline <- baseline
    arguments$exposure <- exposure
    arguments$lambda <- lambda
    outcome <- do.call(custom_func, arguments)
  }
  df <- data.frame(date, x = exposure$x, outcome)
  return(df)
}



