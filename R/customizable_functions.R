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
#' @param cust_exp_args A list of arguments used in the user-specified custom function
#'
#' @examples
#' sim_exposure(n = 5, central = 0.25, exposure_type = "binary", amp = .02)
#' sim_exposure(n = 5, central = 100, sd = 10, amp = .6,
#'              exposure_type = "continuous")
#' sim_exposure(n = 5, cust_exp_func = "custom_exposure",
#'                     cust_exp_args = metric = "temp")
#'
#' @export
sim_exposure <- function(n, central = NULL, sd=NULL, trend = "no trend", amp = .6,
                         exposure_type = NULL,
                         start.date = "2001-01-01", cust_exp_func = NULL,
                         cust_exp_args = NULL){
  if(is.null(cust_exp_args)){
    arguments <- vector(mode = "list")
  } else {
    arguments <- cust_exp_args
  }
  arguments$n <- n

  if(is.null(cust_exp_func)){
    if(is.null(central)){
      stop(paste0("If a custom function is not used to generate exposure values, a central value must be specified."))
    }
    arguments$central <- central
    arguments$trend <- trend
    arguments$amp <- amp
    arguments$exposure_type <- exposure_type
    arguments$sd <- sd
    exposure <- do.call("std_exposure", arguments)
  } else if (!(is.null(cust_exp_func))){
    if (!is.null(central)){
      arguments$central <- central
    }
    start.date <- as.Date(start.date)
    date <- seq(from = start.date, by = 1, length.out = n)
    x <- do.call(cust_exp_func, arguments)
    exposure <- data.frame(date, x)
  } else {
    stop(paste0("If a custom function is not used to simulate randomness in the
                exposure variable, then central and
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
create_baseline <- function(n, average_outcome, trend, amp, cust_base_func = NULL,
                            ...){
  if(is.null(cust_base_func)){
    lambda <- average_outcome
    baseline <- sim_baseline(n=n, lambda=lambda, trend=trend, amp=amp)
  } else {
    arguments <- list(...)
    arguments$n <- n
    arguments$average_outcome <- average_outcome
    arguments$trend <- trend
    baseline <- do.call(cust_base_func, arguments)
  }
  return(baseline)
}

create_lambda <- function(baseline, exposure, rr, cust_lambda_func = NULL, ...){
  if(is.null(cust_lambda_func)){
    log_lambda <- log(baseline) + log(rr) * exposure
    lambda <- exp(log_lambda)
  } else {
    arguments <- list(...)
    arguments$baseline <- baseline
    arguments$exposure <- exposure
    arguments$rr <- rr
    lambda <- do.call(cust_lambda_func, arguments)
  }
  return(lambda)
}
#' @param cust_args A list of arguments and their values used in the user-specified custom functions
#' @export

sim_outcome <- function(exposure, average_outcome = NULL, trend = "no trend",
                        amp = .6, rr = 1.01, start.date="2000-01-01",
                        cust_base_func = NULL, cust_lambda_func = NULL,
                        cust_args = NULL){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = nrow(exposure))
  if (is.null(cust_args)){
    arguments <- vector(mode = "list")
  } else {
    arguments <- cust_args
  }
  if(is.null(cust_base_func) & is.null(cust_lambda_func)){
    if(is.null(average_outcome)){
      stop(paste0("If custom functions are not used to generate outcomes,
                  a value for average_outcome must be specified."))
    }
    baseline <- create_baseline(n = nrow(exposure),
                                average_outcome = average_outcome,
                                trend = trend,
                                amp = amp)
    lambda <- create_lambda(baseline = baseline$exp_base_y,
                            exposure = exposure$x,
                            rr = rr)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  else if (is.null(cust_lambda_func) & !is.null(cust_base_func)){
    arguments$n <- nrow(exposure)
    arguments$average_outcome <- average_outcome
    arguments$exposure <- exposure
    arguments$rr <- rr
    baseline <- do.call(cust_base_func, arguments)
    lambda <- create_lambda(baseline = baseline$exp_base_y,
                            exposure = exposure$x,
                            rr = rr)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  else if (is.null(cust_base_func) & !is.null(cust_lambda_func)){
    if(!is.null(average_outcome)){
      arguments$average_outcome <- average_outcome
    }
    if(!is.null(exposure)){
      arguments$exposure <- exposure
    }
    if(!is.null(rr)){
      arguments$rr <- rr
    }
    if(!is.null(trend)){
      arguments$trend <- trend
    }
    if(!is.null(amp)){
      arguments$amp <- amp
    }
    baseline <- create_baseline(n = nrow(exposure),
                                average_outcome = average_outcome,
                                trend = trend,
                                amp = amp)
    lambda <- do.call(cust_lambda_func, arguments)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  else {
    arguments$average_outcome <- average_outcome
    arguments$exposure <- exposure
    arguments$rr <- rr
    baseline <- do.call(cust_base_func, arguments)
    arguments$baseline <- baseline
    lambda <- do.call(cust_lambda_func, arguments)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  df <- data.frame(date, x = exposure$x, outcome)
  return(df)
}

#' One Function to Rule Them All
#'
#' This function takes all the other functions and in the darkness binds them.
#'
#' @return A list resulting from repetitions of simulations with data frames
#' for date, exposure, and outcomes, and estimates from fitting models
#'
eesim <- function(n_reps, n, central, sd, exposure_type, exposure_trend, exposure_amp,
                  average_outcome, outcome_trend, outcome_amp, rr, start.date,
                  cust_exp_func, cust_exp_args, cust_base_func, cust_lambda_func,
                  cust_out_args){
  exposure <- lapply(rep(n, times = n_reps), sim_exposure, central = central, sd = sd,
                     exposure_type = exposure_type, amp = exposure_amp, trend = exposure_trend,
                     start.date = start.date, cust_exp_func = cust_exp_func,
                     cust_exp_args = cust_exp_args)
  outcome <- lapply(exposure=exposure, sim_outcome, average_outcome = average_outcome,
                    )
}



