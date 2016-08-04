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
sim_exposure <- function(n, central, trend = "no trend", amp = .6, exposure_type = NA,
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
    exposure <- do.call(custom_func, arguments)
  } else {
    stop(paste0("If a custom function is not used to simulate randomness in the",
                "exposure variable, then `outcome_type` must be specified as",
                "either `binary` or `continuous`."))
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
    baseline <- sim_baseline(n, lambda, trend, amp, ...)
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

sim_random_outcome <- function(lambda, custom_func = NULL, ...){
  if(is.null(custom_func)){
    outcome <- rpois(n = length(lambda), lambda = lambda)
  } else {
    arguments <- list(...)
    arguments$lambda <- lambda
    outcome <- do.call(custom_func, arguments)
  }
  return(outcome)
}


sim_outcome2 <- function(exposure, average_outcome, trend = "no trend", amp = .6,
                         rr = 1.1, start.date = "2000-01-01", custom_func = NULL, ...){
  n <- nrow(exposure)
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  if (is.null(custom_func)){
    baseline <- sim_baseline(n, lambda = average_outcome, trend, amp, start.date, ...)
    log_lambda <- log(baseline$exp_base_y) + log(rr) * exposure$x
    lambda2 <- exp(log_lambda)
    outcome <- rpois(n, lambda2)
  }
  else {

  }
  df <- data.frame(date, outcome)
  return(df)
}

sim_outcome <- function(exposure, average_outcome, trend = "no trend",
                        amp = .6, rr, start.date="2000-01-01",
                        custom_func = NULL, ...){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = nrow(exposure))
  if(is.null(custom_func)){
    arguments <- list(...)
    arguments$n <- nrow(exposure)
    arguments$average_outcome <- average_outcome
    arguments$amp <- amp
    arguments$rr <- rr
    arguments$trend <- trend
    arguments$exposure <- exposure
    baseline <- do.call(create_baseline, arguments)
    arguments$baseline <- baseline
    lambda <- do.call(create_lambda, arguments)
    arguments$lambda <- lambda
    outcome <- rpois(n=nrow(exposure), lamba = lambda)
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
  df <- data.frame(date, outcome)
  return(df)
}

sim_df <- function(n, central, amp, exposure_type, average_outcome, trend, rr, start.date = "2000-01-01"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  df <- data.frame(date) %>%
    dplyr::mutate(exposure = sim_exposure(n = n, amp = amp, central = central,
                                          exposure_type = exposure_type),
           baseline = create_baseline(n = n, average_outcome = average_outcome,
                                      trend = trend, amp = amp),
           lambda = create_lambda(baseline = baseline, exposure = exposure,
                                  rr = rr),
           outcome = sim_random_outcome(lambda = lambda)
           )
  return(df)
}


