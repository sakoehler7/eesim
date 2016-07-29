#' Pulls exposure series from Chicago NMMAPS data set
#'
#' @examples
#' custom_exposure(n = 5, metric = "temp")
custom_exposure <- function(n, df = dlnm::chicagoNMMAPS, central = NA, metric = "temp"){
  exposure <- df[1:n, metric]
  return(exposure)
}

#' Simulate random series of exposure values
#'
#' @examples
#' sim_random_exposure(n = 5, central = 0.25, exposure_type = "binary")
#' sim_random_exposure(n = 5, central = 100, sd = 10,
#'                     exposure_type = "continuous")
#' sim_random_exposure(n = 5, central = NA, custom_func = "custom_exposure",
#'                     metric = "temp")
#'
#' @export
sim_random_exposure <- function(n, central, custom_func = NULL,
                         exposure_type = NA, ...){
  if(is.null(custom_func) & exposure_type == "binary"){
    exposure <- stats::rbinom(n = n, size = 1, prob = central)
  } else if (is.null(custom_func) & exposure_type == "continuous"){
    exposure <- stats::rnorm(n = n, mean = central, ...)
  } else if (!(is.null(custom_func))){
    arguments <- list(...)
    arguments$n <- n
    arguments$central <- central
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
custom_baseline <- function(n, df = dlnm::chicagoNMMAPS, average_outcome = NA, trend = NA,
                            outcome_type = "cvd", start.date = "2000-01-01"){
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
#' create_baseline(n = 5, average_outcome = NA, trend = NA,
#'                 custom_func = "custom_baseline", outcome_type = "death")
create_baseline <- function(n, average_outcome, trend, custom_func = NULL, ...){
  if(is.null(custom_func)){
    season_t <- calc_t(n = n, trend = trend, ...)
    baseline <- rep(average_outcome, n) * season_t
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

sim_df <- function(n, central, exposure_type, average_outcome, trend, rr){
  df <- data.frame(day = 1:n) %>%
    dplyr::mutate(exposure = sim_random_exposure(n = n, central = central,
                                          exposure_type = exposure_type),
           baseline = create_baseline(n = n, average_outcome = average_outcome,
                                      trend = trend),
           lambda = create_lambda(baseline = baseline, exposure = exposure,
                                  rr = rr),
           outcome = sim_random_outcome(lambda = lambda)
           )
  return(df)
}
