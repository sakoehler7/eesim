sim_random_exposure <- function(n, central, custom_func = NULL,
                         exposure_type, ...){
  if(is.null(custom_func) & exposure_type == "binary"){
    exposure <- stats::rbinom(n = n, size = 1, prob = central)
  } else if (is.null(custom_func) & exposure_type == "continuous"){
    x <- stats::rnorm(n = n, mean = central, ...)
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

create_baseline <- function(n, average_outcome, trend, custom_func = NULL, ...){
  season_t <- calc_t(n = n, trend = trend, ...)
  baseline <- rep(average_outcome, n) * season_t
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
    mutate(exposure = sim_random_exposure(n = n, central = central,
                                          exposure_type = exposure_type),
           baseline = create_baseline(n = n, average_outcome = average_outcome,
                                      trend = trend),
           lambda = create_lambda(baseline = baseline, exposure = exposure,
                                  rr = rr),
           outcome = sim_random_outcome(lambda = lambda)
           )
  return(df)
}
