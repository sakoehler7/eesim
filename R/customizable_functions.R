#' Pull exposure series from data set
#'
#' By default, this function pulls exposure data from the Chicago NMMAPS data set in the dlnm
#' package.  The user may specify a different data set from which to pull exposure
#' values.
#'
#' @param n A numeric value specifying the number of days for which to obtain an exposure value.
#' @param df Data frame from which to pull exposure values.
#' @param metric A character string specifying the desired exposure metric.
#'    Options are:
#'    \itemize{
#'      \item"temp"
#'      \item"dptp"
#'      \item"rhum"
#'      \item"pm10"
#'      \item"o3"}
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin pulling exposure
#' values.  Dates in the Chicago NMMAPS data set are from 1987-01-01 to 2000-12-31.
#'
#' @return A numeric vector of exposure values
#'
#' @examples
#' custom_exposure(n = 5, metric = "temp", start.date = "2000-01-01")
#'
#' @export
custom_exposure <- function(n, df = dlnm::chicagoNMMAPS, metric = "temp", start.date = NULL){
  if(!is.null(start.date)){
    date1 <- which(df$date==start.date)
    enddate <- date1+n
    exposure <- df[date1:enddate, metric]
  }
  else{
  exposure <- df[1:n, metric]
  }
  return(exposure)
}

#' Simulate random series of exposure values
#'
#' This function simulates binary or continuous exposure values with or without seasonal trends.
#' It also allows for a custom function for exposure trend.
#'
#' @param cust_exp_function The name of a function from which to generate custom exposure values
#' @param cust_exp_args A list of arguments used in the user-specified custom function
#' @inheritParams std_exposure
#'
#' @return A data frame with two columns: date and exposure values
#'
#' @examples
#' sim_exposure(n = 5, central = 0.25, exposure_type = "binary", amp = .02)
#' sim_exposure(n = 5, central = 100, sd = 10, amp = .6,
#'              exposure_type = "continuous")
#' sim_exposure(n = 5, cust_exp_func = "custom_exposure",
#'                     cust_exp_args = list(metric = "temp"))
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


#' Pull smoothed Chicago NMMAPS health outcome data
#'
#' By default, this function pulls smoothed data from the chicagoNMMAPS data set
#' in the dlnm package.  The user may also input a different data set from which to pull
#' data.
#'
#' @inheritParams custom_exposure
#' @inheritParams std_exposure
#' @param metric A character string specifying the desired health outcome metric.
#'    Options are:
#'    \itemize{
#'      \item"death"
#'      \item"cvd"
#'      \item"resp"}
#'
#'
#' @examples
#' custom_baseline(n = 5)
#' custom_baseline(n = 5, outcome_type = "death")
#'
#' @export
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
#'
#' @examples
#' sim_outcome(exposure, cust_base_func = custombase,
#' cust_base_args = list(n=nrow(exposure), slope = .2, intercept = 55))
#' sim_outcome(exposure, p, average_outcome = 22, cust_lambda_func = customlambda,
#' cust_lambda_args = list(exposure = testexp$x, rr=1.02, constant = 4))
#'
#' @export

sim_outcome <- function(exposure, average_outcome = NULL, trend = "no trend",
                        amp = .6, rr = 1.01, start.date="2000-01-01",
                        cust_base_func = NULL, cust_lambda_func = NULL,
                        cust_base_args = list(), cust_lambda_args = list()){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = nrow(exposure))
  if(is.null(cust_base_func) & is.null(cust_lambda_func)){
    if(is.null(average_outcome)){
      stop(paste0("If custom functions are not used to generate outcomes,
                  a value for average_outcome must be specified."))
    }
    baseline <- create_baseline(n = nrow(exposure),
                                average_outcome = average_outcome,
                                trend = trend,
                                amp = amp)
    lambda <- create_lambda(baseline,
                            exposure = exposure$x,
                            rr = rr)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  else if (is.null(cust_lambda_func) & !is.null(cust_base_func)){
    baseline <- do.call(cust_base_func, cust_base_args)
    lambda <- create_lambda(baseline = baseline,
                            exposure = exposure$x,
                            rr = rr)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  else if (is.null(cust_base_func) & !is.null(cust_lambda_func)){
    baseline <- create_baseline(n = nrow(exposure),
                                average_outcome = average_outcome,
                                trend = trend,
                                amp = amp)
    cust_lambda_args$baseline <- baseline
    lambda <- do.call(cust_lambda_func, cust_lambda_args)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  else {
    baseline <- do.call(cust_base_func, cust_base_args)
    cust_lambda_args$baseline <- baseline
    lambda <- do.call(cust_lambda_func, cust_lambda_args)
    outcome <- rpois(n = nrow(exposure), lambda = lambda)
  }
  df <- data.frame(date, x = exposure$x, outcome)
  return(df)
}

#' Create simulated data for many repetitions
#'
#' @param A character string specifying the model to be used.  Options are "spline" and "casecrossover"
#'
#' @return A list resulting from repetitions of simulations with data frames
#' for date, exposure, and outcomes, and estimates from fitting models
#'
#' @examples
#' create_sims(n_reps=10, n=10, central = 100, sd = 10, exposure_type="continuous",
#' exposure_trend = "cos1", exposure_amp = .6, average_outcome = 22, outcome_trend = "no trend",
#' outcome_amp = .6, rr = 1.01)
#'
#'
create_sims <- function(n_reps, n, central, sd, exposure_type, exposure_trend, exposure_amp,
                  average_outcome, outcome_trend, outcome_amp, rr, start.date = "2000-01-01",
                  cust_exp_func = NULL, cust_exp_args = NULL, cust_base_func = NULL,
                  cust_lambda_func = NULL, cust_base_args = NULL, cust_lambda_args = NULL){
  exposure <- lapply(rep(n, times = n_reps), sim_exposure, central = central, sd = sd,
                     exposure_type = exposure_type, amp = exposure_amp, trend = exposure_trend,
                     start.date = start.date, cust_exp_func = cust_exp_func,
                     cust_exp_args = cust_exp_args)
  outcome <- lapply(exposure, sim_outcome, average_outcome = average_outcome,
                    trend = outcome_trend, amp = outcome_amp, rr = rr, start.date = start.date,
                    cust_base_func = cust_base_func, cust_lambda_func = cust_lambda_func,
                    cust_base_args = cust_base_args, cust_lambda_args = cust_lambda_args)
  return(outcome)
}

#' Fit models
#'
#' @param outcome A list of simulated data sets which each include columns called "x" and "outcome"
#' @param model A character string specifying model to be used.  Choices are "spline" and "casecrossover"
#'
#' @export
fit_mods <- function(outcome, model, df_year = 7){
  if(model == "spline"){
    mods <- lapply(outcome, spline_mod, df_year = df_year)
  }
  else if(model == "casecrossover"){
    mods <- lapply(outcome, casecross_mod)
  }
  datframe <- data.frame(do.call("rbind", mods))
  names(datframe) <- c("Estimate", "Std.Error", "t.value", "p.value", "lower_ci", "upper_ci")
  return(datframe)
}

#' Wrapper to do Everything
#'
#' This function generates exposures and outcomes, fits models, and evaluates them for many repetitions
#'
#' @examples
#' eesim(n_reps = 3, n = 50, central = 100, sd = 10, exposure_type = "continuous",
#' exposure_trend = "cos3", exposure_amp = .6, average_outcome = 22, rr = 1.01,
#' model = "spline", df_year = 5)
#'
#' @export
#'
eesim <- function(n_reps, n, central, sd, exposure_type, exposure_trend, exposure_amp,
                  average_outcome, outcome_trend = "no trend", outcome_amp, rr, start.date = "2000-01-01",
                  cust_exp_func = NULL, cust_exp_args = NULL, cust_base_func = NULL,
                  cust_lambda_func = NULL, cust_base_args = NULL, cust_lambda_args = NULL,
                  model, df_year = NULL){
  datasims <- create_sims(n_reps=n_reps, n=n, central=central, sd=sd, exposure_type=exposure_type, exposure_trend=exposure_trend, exposure_amp=exposure_amp,
                          average_outcome=average_outcome, outcome_trend=outcome_trend, outcome_amp=outcome_amp, rr=rr, start.date = "2000-01-01",
                          cust_exp_func = NULL, cust_exp_args = NULL, cust_base_func = NULL,
                          cust_lambda_func = NULL, cust_base_args = NULL, cust_lambda_args = NULL)
  mods <- fit_mods(datasims, model, df_year)
  check <- check_sims(df = mods, true_rr = rr)
  totalsims <- list(datasims, mods, check)
  return(totalsims)
}

