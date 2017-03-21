#' Pull exposure series from data set
#'
#' By default, this function pulls exposure data from the Chicago NMMAPS data
#' set in the dlnm package.  The user may specify a different data set from
#' which to pull exposure values.
#'
#' @param n A numeric value specifying the number of days for which to obtain an
#'    exposure value.
#' @param df Data frame from which to pull exposure values.
#' @param metric A character string specifying the desired exposure metric.
#'    Options are:
#'    \itemize{
#'      \item"temp"
#'      \item"dptp"
#'      \item"rhum"
#'      \item"pm10"
#'      \item"o3"}
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin
#'    pulling exposure values. Dates in the Chicago NMMAPS data set are from
#'    1987-01-01 to 2000-12-31.
#'
#' @return A numeric vector of exposure values
#'
#' @examples
#' custom_exposure(n = 5, metric = "temp", start.date = "2000-01-01")
#'
#' @export
custom_exposure <- function(n, df = dlnm::chicagoNMMAPS, metric = "temp",
                            start.date = NULL){
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

#' Simulate time series of exposure values
#'
#' Simulates a time series of binary or continuous exposure values with or
#' without seasonal trends. It also allows the user to use a custom function for
#' the shape of the exposure trend.
#'
#' @param cust_exp_func The name of a function to use to generate custom
#'    exposure values.
#' @param cust_exp_args A list of arguments used in the user-specified custom
#'    function.
#' @inheritParams std_exposure
#' @inheritParams continuous_exposure
#' @inheritParams binary_exposure
#' @inheritParams calc_t
#'
#' @return A data frame with two columns: date (\code{date}) and simulated
#'    exposure values (\code{x}).
#'
#' @examples
#' sim_exposure(n = 5, central = 0.25, exposure_type = "binary")
#' sim_exposure(n = 5, central = 100, sd = 10, exposure_type = "continuous")
#' library(ggplot2)
#' x_cont <- sim_exposure(n = 1000, central = 100, sd = 10,
#'                        exposure_type = "continuous",
#'                        trend = "cos1linear", amp = 0.6)
#' ggplot(x_cont, aes(x = date, y = x)) + geom_point()
#' x_cust <- sim_exposure(n = 1000, cust_exp_func = "custom_exposure",
#'                        cust_exp_args = list(metric = "temp"))
#' ggplot(x_cust, aes(x = date, y = x)) + geom_point()
#' @export
sim_exposure <- function(n, central = NULL, sd = NULL, trend = "no trend",
                         amp = .6, exposure_type = NULL,
                         start.date = "2001-01-01", cust_exp_func = NULL,
                         cust_exp_args = NULL){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  if(is.null(cust_exp_args)){
    arguments <- vector(mode = "list")
  } else {
    arguments <- cust_exp_args
  }
  arguments$n <- n

  if(is.null(cust_exp_func)){
    if(is.null(central)){
      stop(paste0("If a custom function is not used to generate exposure values,
                  a central value must be specified."))
    }
    arguments$central <- central
    arguments$trend <- trend
    arguments$amp <- amp
    arguments$exposure_type <- exposure_type
    arguments$sd <- sd
    arguments$start.date <- start.date
    exposure <- do.call("std_exposure", arguments)
  } else if (!(is.null(cust_exp_func))){
    if (!is.null(central)){
      arguments$central <- central
    }
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
#' in the dlnm package.  The user may also input a different data set from which
#'    to pull data.
#'
#' @inheritParams custom_exposure
#' @inheritParams std_exposure
#' @param outcome_type A character string specifying the desired health outcome metric.
#'    Options are:
#'    \itemize{
#'      \item"death"
#'      \item"cvd"
#'      \item"resp"}
#'
#' @return A data frame with one column for date and one column for baseline
#'    outcome values
#'
#' @examples
#' custom_baseline(n = 5)
#' custom_baseline(n = 5, outcome_type = "death")
#'
#' @export
custom_baseline <- function(n, df = dlnm::chicagoNMMAPS, outcome_type = "cvd",
                            start.date = "2000-01-01"){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = n)
  df$outcome <- df[ , outcome_type]
  smooth_mod <- stats::glm(outcome ~ splines::ns(time, 7 * 14), data = df)
  baseline <- stats::predict(smooth_mod)[1:n]
  df2 <- data.frame(date, baseline)
  return(df2)
}

#' Create a series of baseline outcomes
#'
#' This function creates a time series of baseline outcome values and allows the
#' user to input a custom function if desired to specify outcome trend.
#'
#' @inheritParams sim_baseline
#' @param trend outcome_trend A character string specifying the seasonal trend in
#'        health outcomes.  Options are Options are:
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
#' @param amp A numeric value specifying the amplitude of the seasonal trend.
#'    Must be between 0 and 1.
#' @param ... optional arguments to a custom baseline function
#' @param average_outcome A numeric value specifying the average outcome value
#' @param cust_base_func A character string specifying a user-made custom
#'    function for baseline trend
#'
#' @return A numeric vector of baseline outcome values
#'
#' @examples
#' create_baseline(n = 5, average_outcome = 22, trend = "linear")
#' create_baseline(n = 5, average_outcome = NULL, trend = NULL,
#'                 custom_func = "custom_baseline", outcome_type = "death")
#'
#' @export
#'
create_baseline <- function(n, average_outcome, trend, amp,
                            cust_base_func = NULL, ...){
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

#' Create a series of mean outcome values
#'
#' This function relates exposure to baseline outcome values with the function
#' lambda = log(baseline) + log(relative risk)*exposure to create a series
#' of mean outcome values with or without incorporating a seasonal trend.  The
#' user may input a custom function to relate exposure, relative risk, and
#' baseline.
#'
#' @param baseline A numeric vector of baseline outcome values
#' @param exposure A numeric vector of exposure values
#' @param rr A numeric value specifying the relative risk
#' @param cust_lambda_func A character string specifying a user-made custom
#'    function for relating baseline, relative risk, and exposure
#' @param ... optional arguments for a custom lambda function
#'
#' @return A numeric vector of mean outcome values
#'
#' @examples
#' base <- create_baseline(n = 5, average_outcome = 22, trend = "linear")
#' exp <- sim_exposure(n = 5, central = 100, sd = 10, amp = .6,
#'                     exposure_type = "continuous")
#' create_lambda(baseline = base, exposure = exp, rr = 1.01)
#'
#' @export

create_lambda <- function(baseline, exposure, rr, cust_lambda_func = NULL, ...){
  if(is.null(cust_lambda_func)){
    if("data.frame" %in% class(baseline)){ baseline <- baseline$baseline }
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

#' Simulate outcome
#'
#' @param cust_lambda_args A list of arguments and their values used in the
#'    user-specified custom lambda function
#' @param cust_base_args A list of arguments and their values used in the
#'    user-specified custom baseline function
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin
#'    simulating values
#' @inheritParams create_baseline
#' @inheritParams create_lambda
#'
#' @return A dataframe with a simulated time series, with columns for
#'    \code{date}, \code{x} (exposure), and \code{outcome}.
#'
#' @examples
#' exp <- sim_exposure(n = 5, central = 100, sd = 10, amp = .6,
#'                     exposure_type = "continuous")
#' sim_outcome(exposure = exp, average_outcome = 22, trend = "linear")
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
    lambda <- create_lambda(baseline = baseline,
                            exposure = exposure$x,
                            rr = rr)
    outcome <- stats::rpois(n = nrow(exposure), lambda = lambda)
  } else if (is.null(cust_lambda_func) & !is.null(cust_base_func)){
    baseline <- do.call(cust_base_func, cust_base_args)
    lambda <- create_lambda(baseline = baseline,
                            exposure = exposure$x,
                            rr = rr)
    outcome <- stats::rpois(n = nrow(exposure), lambda = lambda)
  } else if (is.null(cust_base_func) & !is.null(cust_lambda_func)){
    baseline <- create_baseline(n = nrow(exposure),
                                average_outcome = average_outcome,
                                trend = trend,
                                amp = amp)
    cust_lambda_args$baseline <- baseline$baseline
    lambda <- do.call(cust_lambda_func, cust_lambda_args)
    outcome <- stats::rpois(n = nrow(exposure), lambda = lambda)
  } else {
    baseline <- do.call(cust_base_func, cust_base_args)
    cust_lambda_args$baseline <- baseline
    lambda <- do.call(cust_lambda_func, cust_lambda_args)
    outcome <- stats::rpois(n = nrow(exposure), lambda = lambda)
  }
  df <- data.frame(date, x = exposure$x, outcome)
  return(df)
}

#' Create simulated data for many repetitions
#'
#' @param n_reps A numeric value specifying the number of simulation repititions
#'        to be performed
#' @param n A numeric value specifying the number of days to simulate.
#' @param sd A numeric value giving the standard deviation of the exposure
#'    values around the trend line.
#' @param exposure_trend A character string specifying the seasonal trend for
#'        exposure. Options for continuous exposure are:
#'        \itemize{
#'      \item{"cos1"}
#'      \item{"cos2"}
#'      \item{"cos3"}
#'      \item{"linear"}
#'      \item{"curvilinear"}
#'      \item{"cos1linear"}
#'      \item{"no trend"}
#'      \item{"custom"}
#'      }
#'       Options for binary exposure are:
#'       \itemize{
#'      \item{"cos1"}
#'      \item{"cos2"}
#'      \item{"cos3"}
#'      \item{"linear"}
#'      \item{"monthly"}
#'      \item{"no trend"}
#'      \item{"custom"}
#'    }
#' @param exposure_amp A numeric value specifying the amplitude of the exposure
#'        trend. Must be between 0 and 1 for continuous exposure or between 0
#'        and .5 for binary exposure.
#' @param outcome_trend A character string specifying the seasonal trend in
#'        health outcomes.  Options are the same as for continuous exposure
#'        data.
#' @param outcome_amp A numeric value specifying the amplitude of the outcome
#'        trend.  Must be between 0 and 1.
#' @param cust_exp_func A character string specifying the name of a custom
#'        trend function to generate exposure data
#' @param cust_exp_args A list of arguments and their values for the
#'        user-specified custom exposure function.
#' @param cust_lambda_args A list of arguments and their values used in the
#'    user-specified custom lambda function
#' @inheritParams std_exposure
#' @inheritParams sim_outcome
#'
#' @return A list resulting from repetitions of simulations with data frames
#'    for date, exposure, and outcomes, and estimates from fitting models
#'
#' @examples
#' create_sims(n_reps=3, n=100, central = 100, sd = 10,
#'             exposure_type="continuous", exposure_trend = "cos1",
#'             exposure_amp = .6, average_outcome = 22,
#'             outcome_trend = "no trend", outcome_amp = .6, rr = 1.01)
#'
#' @export
#'
create_sims <- function(n_reps, n, central, sd, exposure_type, exposure_trend,
                        exposure_amp, average_outcome, outcome_trend,
                        outcome_amp, rr, start.date = "2000-01-01",
                        cust_exp_func = NULL, cust_exp_args = NULL,
                        cust_base_func = NULL, cust_lambda_func = NULL,
                        cust_base_args = NULL, cust_lambda_args = NULL){

  exposure <- lapply(rep(n, times = n_reps), sim_exposure, central = central,
                     sd = sd, exposure_type = exposure_type, amp = exposure_amp,
                     trend = exposure_trend, start.date = start.date,
                     cust_exp_func = cust_exp_func,
                     cust_exp_args = cust_exp_args)
  outcome <- lapply(exposure, sim_outcome, average_outcome = average_outcome,
                    trend = outcome_trend, amp = outcome_amp, rr = rr,
                    start.date = start.date, cust_base_func = cust_base_func,
                    cust_lambda_func = cust_lambda_func,
                    cust_base_args = cust_base_args,
                    cust_lambda_args = cust_lambda_args)
  return(outcome)
}

#' Fit models
#'
#' @param outcome A list of simulated data sets which each include columns
#'    called "x" and "outcome"
#' @param model A character string specifying model to be used. Choices are
#'    "spline" and "casecrossover"
#' @inheritParams spline_mod
#'
#' @return A data frame in which each row includes an estimate of beta hat,
#'    standard error, t-value, p-value, and 2.5% and 97.5% confidence bounds
#'    for each repetition of the simulation
#'
#' @examples
#' sims <- create_sims(n_reps=10, n=50, central = 100, sd = 10,
#'             exposure_type="continuous", exposure_trend = "cos1",
#'             exposure_amp = .6, average_outcome = 22,
#'             outcome_trend = "no trend", outcome_amp = .6, rr = 1.01)
#' fit_mods(outcome = sims, model = "spline")
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
  names(datframe) <- c("Estimate", "Std.Error", "t.value", "p.value",
                       "lower_ci", "upper_ci")
  return(datframe)
}

#' Simulate data, fit models, and assess models
#'
#' This function generates exposures and outcomes, fits models, and evaluates
#' them for many simulation repetitions
#'
#' @inheritParams create_sims
#' @inheritParams sim_exposure
#' @inheritParams sim_outcome
#' @inheritParams std_exposure
#' @inheritParams fit_mods
#'
#' @return A list object with summaries of each model fitted on the simulated
#'    data sets and measures of model evaluation including coverage and power
#'
#' @examples
#' eesim(n_reps = 3, n = 50, central = 100, sd = 10,
#'       exposure_type = "continuous", exposure_trend = "cos3",
#'       exposure_amp = .6, average_outcome = 22, rr = 1.01, model = "spline",
#'       df_year = 5)
#'
#' @export
eesim <- function(n_reps, n, central = NULL, sd = NULL, exposure_type, exposure_trend = NULL,
                  exposure_amp = NULL, average_outcome = NULL,
                  outcome_trend = "no trend",
                  outcome_amp = NULL, rr, start.date = "2000-01-01",
                  cust_exp_func = NULL, cust_exp_args = NULL,
                  cust_base_func = NULL, cust_lambda_func = NULL,
                  cust_base_args = NULL, cust_lambda_args = NULL, model,
                  df_year = NULL){
  datasims <- create_sims(n_reps=n_reps, n=n, central=central, sd=sd,
                          exposure_type=exposure_type,
                          exposure_trend=exposure_trend,
                          exposure_amp=exposure_amp,
                          average_outcome=average_outcome,
                          outcome_trend=outcome_trend, outcome_amp=outcome_amp,
                          rr=rr, start.date = "2000-01-01",
                          cust_exp_func = cust_exp_func, cust_exp_args = cust_exp_args,
                          cust_base_func = cust_base_func, cust_lambda_func = cust_lambda_func,
                          cust_base_args = cust_base_args, cust_lambda_args = cust_lambda_args)
  mods <- fit_mods(datasims, model, df_year)
  check <- check_sims(df = mods, true_rr = rr)
  totalsims <- list(mods, check)
  return(totalsims)
}

