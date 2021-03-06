#' Pull exposure series from data set
#'
#' Example of a custom exposure function that can be passed to \code{\link{eesim}} or
#' \code{\link{power_calc}}.
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
#' (Note: These are the column names for exposure measurements in the observed data.)
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin
#'    pulling exposure values. Dates in the Chicago NMMAPS data set are from
#'    1987-01-01 to 2000-12-31.
#'
#' @return A numeric vector of length \code{n} giving exposure values.
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
#' @param cust_exp_func An R object specifying the function to use to generate custom
#'    exposure values.
#' @param cust_exp_args A list of arguments used in the user-specified custom
#'    function.
#' @param cust_expdraw An R object specifying a user-created function
#'    which determines the distribution of random noise off of the trend line.
#'    This function must have inputs \code{n} and \code{prob} for a binary exposure
#'    function and inputs \code{n} and \code{mean} for a continuous exposure function.
#'    The custom function must output a vector of simulated exposure values.
#' @param cust_expdraw_args A list of arguments other than \code{n} required by the
#'    \code{cust_expdraw} function.
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
                         slope = 1, amp = .6, exposure_type = NULL,
                         start.date = "2001-01-01", cust_exp_func = NULL,
                         cust_exp_args = NULL, cust_expdraw = NULL,
                         cust_expdraw_args = list()){
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
    arguments$slope <- slope
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
#' Example of a custom baseline function that can be passed to \code{\link{eesim}} or
#' \code{\link{power_calc}}.
#' By default, this function pulls smoothed data from the \code{chicagoNMMAPS} data set
#' in the \code{dlnm} package.  The user may also input a different data set from which
#' to pull data. The function uses a smoothed function of this observed data as the
#' underlying baseline outcome trend in simulating data.
#'
#' @inheritParams custom_exposure
#' @inheritParams std_exposure
#' @param outcome_type A character string specifying the desired health outcome metric.
#'    Options are:
#'    \itemize{
#'      \item"death"
#'      \item"cvd"
#'      \item"resp"}
#' (Note: These are the column names for outcome counts in the observed data.)
#'
#' @return A data frame with one column for date and one column for baseline
#'    outcome values.
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
#' Creates a time series of baseline outcome values. This function allows the
#' user to input a custom function if desired to specify outcome trend.
#'
#' @inheritParams sim_baseline
#' @inheritParams continuous_exposure
#' @param average_baseline A non-negative numeric value specifying the average outcome
#'    value over all simulated days.
#' @param cust_base_func A R object name specifying a user-made custom
#'    function for baseline trend.
#' @param ... Optional arguments to a custom baseline function
#'
#' @return A numeric vector of baseline outcome values
#'
#' @examples
#' create_baseline(n = 5, average_baseline = 22, trend = "linear")
#'
#' @export
#'
create_baseline <- function(n, average_baseline = NULL, trend = "no trend",
                            slope = 1, amp = 0.6,
                            cust_base_func = NULL, ...){
  if(is.null(cust_base_func)){
    lambda <- average_baseline
    baseline <- sim_baseline(n=n, lambda=lambda, trend=trend, slope=slope,
                             amp=amp)
  } else {
    arguments <- list(...)
    arguments$n <- n
    arguments$average_outcome <- average_baseline
    arguments$trend <- trend
    baseline <- do.call(cust_base_func, arguments)
  }
  return(baseline)
}

#' Create a series of mean outcome values
#'
#' Creates a vector of expected daily outcome count by relating exposure to baseline
#' outcome values with the function:
#' \deqn{log(\lambda_t) = log(B_t) + log(RR)*X_t}{log(\lambda) = log(B) + log(RR)*X}
#' where \eqn{\lambda_t}{\lambda} is the expected outcome count on day \eqn{t},
#' \eqn{B} is the expected base outcome count on day \eqn{t} (incorporating long-term
#' and seasonal trends, but not the influence of the exposure), \eqn{RR} is the relative
#' risk of the outcome for a one-unit increase in exposure, and \eqn{X_t}{X} is the
#' simulated exposure on day \eqn{t}.
#' The user may input a custom function to relate exposure, relative risk, and
#' baseline.
#'
#' @param baseline A non-negative numeric vector of baseline outcome values,
#'    typically the output of \code{\link{create_baseline}}.
#' @param exposure A numeric vector of exposure values, typically the output
#'    of \code{\link{sim_exposure}}.
#' @param rr A non-negative numeric value specifying the relative risk (i.e., the
#'    relative risk per unit increase in the exposure).
#' @param cust_lambda_func An R object name specifying a user-made custom
#'    function for relating baseline, relative risk, and exposure
#' @param ... Optional arguments for a custom lambda function
#'
#' @return A numeric vector of mean outcome values for each day in the simulation.
#'
#' @examples
#' base <- create_baseline(n = 10, average_baseline = 22, trend = "linear",
#'                         slope = .4)
#' exp <- sim_exposure(n = 5, central = 100, sd = 10, amp = .6,
#'                     exposure_type = "continuous")
#' create_lambda(baseline = base, exposure = exp$x, rr = 1.01)
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
#' Simulates daily outcome counts for each study day based on user specifications
#' for average outcome count, any underlying trends in expected outcome counts, and
#' the association between exposure and outcome. This function starts from a vector of
#' expected outcome count on each study day and simulates through a draw from a Poisson
#' distribution based on this expected daily value. If desired, a user can also use
#' a custom function to customize this stage of the simulation; see the vignette for
#' \code{eesim} for more details and examples.
#'
#' @param cust_lambda_args A list of arguments and their values used in the
#'    user-specified custom lambda function
#' @param cust_base_args A list of arguments and their values used in the
#'    user-specified custom baseline function
#' @param cust_outdraw An R object name specifying a user-created function to
#'    randomize the outcome values off of the baseline for outcome values. This
#'    function must take inputs \code{n} and \code{lambda} and output a vector of outcome
#'    values.
#' @param cust_outdraw_args A list of arguments besides \code{n} passed
#'    to the user-created custom outcome draw function.
#' @param start.date A date of the format "yyyy-mm-dd" from which to begin
#'    simulating values
#' @inheritParams create_baseline
#' @inheritParams create_lambda
#' @inheritParams create_sims
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
                        slope = 1, amp = .6, rr = 1.01, start.date="2000-01-01",
                        cust_base_func = NULL, cust_lambda_func = NULL,
                        cust_base_args = list(), cust_lambda_args = list(),
                        cust_outdraw = NULL, cust_outdraw_args = list()){
  start.date <- as.Date(start.date)
  date <- seq(from = start.date, by = 1, length.out = nrow(exposure))
  average_baseline <- average_outcome / exp(log(rr) * mean(exposure$x))
  if(is.null(cust_base_func) & is.null(cust_lambda_func)){
    if(is.null(average_outcome)){
      stop(paste0("If custom functions are not used to generate outcomes,
                  a value for average_outcome must be specified."))
    }
    baseline <- create_baseline(n = nrow(exposure),
                                average_baseline = average_baseline,
                                trend = trend, slope=slope,
                                amp = amp)
    lambda <- create_lambda(baseline = baseline,
                            exposure = exposure$x,
                            rr = rr)
  } else if (is.null(cust_lambda_func) & !is.null(cust_base_func)){
    baseline <- do.call(cust_base_func, cust_base_args)
    lambda <- create_lambda(baseline = baseline,
                            exposure = exposure$x,
                            rr = rr)
  } else if (is.null(cust_base_func) & !is.null(cust_lambda_func)){
    baseline <- create_baseline(n = nrow(exposure),
                                average_baseline = average_baseline,
                                trend = trend, slope=slope,
                                amp = amp)
    cust_lambda_args$baseline <- baseline$baseline
    cust_lambda_args$rr <- rr
    cust_lambda_args$exposure <- exposure$x
    lambda <- do.call(cust_lambda_func, cust_lambda_args)
  } else {
    baseline <- do.call(cust_base_func, cust_base_args)
    cust_lambda_args$baseline <- baseline
    cust_lambda_args$rr <- rr
    cust_lambda_args$exposure <- exposure$x
    lambda <- do.call(cust_lambda_func, cust_lambda_args)
  }

  lambda <- lambda * average_outcome / mean(lambda) # Make sure we have desired mean

  if (is.null(cust_outdraw)){
  outcome <- stats::rpois(n = nrow(exposure), lambda = lambda)
  } else {
    cust_outdraw_args$lambda <- lambda
    cust_outdraw_args$n <- nrow(exposure)
    outcome <- do.call(cust_outdraw, cust_outdraw_args)
  }
  df <- data.frame(date, x = exposure$x, outcome)
  return(df)
}

#' Create simulated data for many repetitions
#'
#' Creates a collection of synthetic datasets that follow a set of user-specified
#' conditions (e.g., exposure mean and variance, average daily outcome count,
#' long-term and seasonal trends in exposure and outcome, association between exposure
#' and outcome). These synthetic datasets can be used to investigate performance of a specific
#' model or to estimate power or required sample size for a hypothetical study.
#'
#' @param n_reps An integer specifying the number of datasets to simulate (e.g.,
#'        \code{n_reps = 1000} would simulate one thousand time series datasets with the
#'        specified characteristics, which can be used for a power analysis or to investigate
#'        the performance of a proposed model).
#' @param n An integer specifying the number of days to simulate (e.g., \code{n = 365}
#'        would simulate a dataset with a year's worth of data).
#' @param sd A non-negative numeric value giving the standard deviation of the exposure
#'        values from the exposure trend line (not the total standard deviation of
#'        the exposure values).
#' @param exposure_trend A character string specifying a seasonal and / or long-term trend for
#'        expected mean exposure. See the vignette for \code{eesim} for examples of each option.
#'        The shapes are based on those used in Bateson and Schwartz (1999).
#'        For trends with a seasonal component, the amplitude of the seasonal trend can be
#'        customized using the \code{exposure\_amp} argument. For trends with a long-term
#'        pattern, the slope of the long-term trend can be set using the \code{exposure\_slope}
#'        argument.
#'        If using the "monthly" option for a binary exposure, you must input a numeric
#'        vector of length 12 for the \code{central} argument that gives the probability of
#'        exposure for each month, starting in January and ending in December.
#'        Options for continuous exposure are:
#'    \itemize{
#'      \item{"no trend": No trend, either seasonal or long-term (default).}
#'      \item{"cos1": A seasonal trend only.}
#'      \item{"cos2": A seasonal trend with variable amplitude across years.}
#'      \item{"cos3": A seasonal trend with steadily decreasing amplitude over time.}
#'      \item{"linear": A linear long-term trend with no seasonal trend.}
#'      \item{"curvilinear": A curved long-term trend with no seasonal trend.}
#'      \item{"cos1linear": A seasonal trend plus a linear long-term trend.}
#'      }
#'       Options for binary exposure are:
#'       \itemize{
#'      \item{"no trend": No trend, either seasonal or long-term (default).}
#'      \item{"cos1": A seasonal trend only.}
#'      \item{"cos2": A seasonal trend with variable amplitude across years.}
#'      \item{"cos3": A seasonal trend with steadily decreasing amplitude over time.}
#'      \item{"linear": A linear long-term trend with no seasonal trend.}
#'      \item{"monthly": Uses a user-specified probability of exposure for each month.}
#'    }
#' @param exposure_slope A numeric value specifying the linear slope of the
#'        exposure, to be used with \code{exposure_trend = "linear"} or
#'        \code{exposure_trend = "cos1linear"}.
#'        The default value is 1. Positive values will generate data with an
#'        increasing expected value over the years while negative values will
#'        generate data with decreasing expected value over the years.
#' @param exposure_amp A numeric value specifying the amplitude of the exposure
#'        trend. Must be between -1 and 1 for continuous exposure or between -0.5
#'        and 0.5 for binary exposure. Positive values will simulate a pattern
#'        with higher values at the time of the year of the start of the dataset
#'        (typically January) and lowest values six months following that (typically
#'        July). Negative values can be used to simulate a trend with lower values
#'        at the time of year of the start of the dataset and higher values in the
#'        opposite season.
#' @param average_outcome A non-negative numeric value specifying the average
#'        daily outcome count.
#' @param outcome_trend A character string specifying the seasonal trend in
#'        health outcomes.  Options are the same as for continuous exposure
#'        data.
#' @param outcome_slope A numeric value specifying the linear slope of the
#'        outcome trend, to be used with \code{outcome_trend = "linear"} or
#'        \code{outcome_trend = "cos1linear"}. The default value is 1.
#'        Positive values will generate data with an
#'        increasing expected value over the years while negative values will
#'        generate data with decreasing expected value over the years.
#' @param outcome_amp A numeric value specifying the amplitude of the outcome
#'        trend.  Must be between -1 and 1.
#' @param cust_exp_func An R object name specifying the name of a custom
#'        trend function to generate exposure data
#' @param cust_exp_args A list of arguments and their values for the
#'        user-specified custom exposure function.
#' @param cust_lambda_args A list of arguments and their values used in the
#'    user-specified custom lambda function
#' @inheritParams std_exposure
#' @inheritParams sim_exposure
#' @inheritParams sim_outcome
#'
#' @return A list object of length \code{n_rep}, in which each list element is one of the
#'    synthetic datasets simulated under the input conditions. Each synthetic dataset includes
#'    columns for for date (\code{date}), daily exposure (\code{x}), and daily outcome count
#'    (\code{outcome}).
#'
#' @references
#'
#' Bateson TF, Schwartz J. 1999. Control for seasonal variation and time trend in
#'     case-crossover studies of acute effects of environmental exposures. Epidemiology
#'     10(4):539-544.
#'
#' @examples
#' create_sims(n_reps=3, n=100, central = 100, sd = 10,
#'             exposure_type="continuous", exposure_trend = "cos1",
#'             exposure_amp = .6, average_outcome = 22,
#'             outcome_trend = "no trend", outcome_amp = .6, rr = 1.01)
#'
#' @export
#'
create_sims <- function(n_reps, n, rr, central, average_outcome, sd = NULL, exposure_type,
                        exposure_trend,
                        exposure_slope = 1, exposure_amp = NULL,
                        outcome_trend = NULL, outcome_slope = 1,
                        outcome_amp = NULL, start.date = "2000-01-01",
                        cust_exp_func = NULL, cust_exp_args = NULL,
                        cust_expdraw = NULL, cust_expdraw_args = NULL,
                        cust_base_func = NULL, cust_lambda_func = NULL,
                        cust_base_args = NULL, cust_lambda_args = NULL,
                        cust_outdraw = NULL, cust_outdraw_args = NULL){

  exposure <- lapply(rep(n, times = n_reps), sim_exposure, central = central,
                     sd = sd, exposure_type = exposure_type,
                     slope = exposure_slope, amp = exposure_amp,
                     trend = exposure_trend, start.date = start.date,
                     cust_exp_func = cust_exp_func,
                     cust_exp_args = cust_exp_args,
                     cust_expdraw = cust_expdraw,
                     cust_expdraw_args = cust_expdraw_args)
  outcome <- lapply(exposure, sim_outcome, average_outcome = average_outcome,
                    trend = outcome_trend, slope = outcome_slope,
                    amp = outcome_amp, rr = rr,
                    start.date = start.date, cust_base_func = cust_base_func,
                    cust_lambda_func = cust_lambda_func,
                    cust_base_args = cust_base_args,
                    cust_lambda_args = cust_lambda_args,
                    cust_outdraw=cust_outdraw,
                    cust_outdraw_args=cust_outdraw_args)
  return(outcome)
}

#' Fit a model to simulated datasets
#'
#' Fits a specified model to each of the simulated datasets and returns a dataframe
#' summarizing results from fitting the model to each dataset, including the estimated
#' effect and the estimated standard error for that estimated effect. The model is specified
#' through a user-created R function, which must take specific input and return
#' output in a specific format. For more details, see the parameter definitions,
#' the Details section, and the vignette for the \code{eesim} package.
#'
#' @param data A list of simulated data sets. Each simulated dataset must include a
#'    column called \code{x} with daily exposure values and a column called \code{outcome} with
#'    daily outcome values. Typically, this will be the outcome from \code{\link{create_sims}}.
#' @param custom_model The object name of an R function that defines the code that will be used
#'     to fit the model. This object name should not be in quotations. See Details for more.
#' @param custom_model_args A list of arguments and their values for a custom
#'    model. These arguments are passed through to the function specified with \code{custom_model}.
#'
#' @details The function specified by the \code{custom_model} argument should be
#'    a user-created function that inputs a data frame with columns named "x" for
#'    exposure values and "outcome" for outcome values. The function must output a data
#'    frame with columns called \code{Estimate}, \code{Std. Error}, \code{t value},
#'    \code{Pr(>|t|)}, \code{2.5\%}, and \code{97.5\%}. Note that these columns are the output
#'    from \code{summary} and \code{confint} for models fit using a \code{glm} call. You may
#'    use the function \code{format_out} from eesim within your function to produce output
#'    with these columns if this model is fit using \code{glm} or something similar.
#'    For more details and examples, see the vignette for \code{eesim}.
#'
#' @return A data frame in which each row gives the results from the model-fitting function run
#'   on one of the simulated datasets input to the function as the \code{data} object. The returned
#'   data frame has one row per simulated dataset and the following columns:
#'   \itemize{
#'     \item{\code{Estimate}: The estimated \eqn{\beta} (log relative risk) as estimated by
#'       the model specified with \code{custom_model}.}
#'     \item{\code{Std.Error}: The standard error for the estimated \eqn{\beta}.}
#'     \item{\code{t.value}: The test statistic for a test of the null hypothesis \eqn{\beta = 0}.}
#'     \item{\code{p.value}: The p-value for a test of the null hypothesis \eqn{\beta = 0}.}
#'     \item{\code{lower_ci}: The lower value in the 95\% confidence interval estimated for
#'       \eqn{\beta}.}
#'     \item{\code{upper_ci}: The upper value in the 95\% confidence interval estimated for
#'       \eqn{\beta}.}
#'   }
#'
#' @examples
#' # Create a set of simulated datasets and then fit the model defined in `spline_mod` to
#' # all datasets, using the argument `df_year = 7` in the call to `spline_mod`. The `spline_mod`
#' # function is included in the `eesim` package and can be investigating by calling the function
#' # name without parentheses (i.e., `spline_mod`).
#' sims <- create_sims(n_reps = 10, n = 5 * 365, central = 100, sd = 10,
#'             exposure_type = "continuous", exposure_trend = "cos1",
#'             exposure_amp = .6, average_outcome = 22,
#'             outcome_trend = "no trend", outcome_amp = .6, rr = 1.01)
#' fit_mods(data = sims, custom_model = spline_mod, custom_model_args = list(df_year = 7))
#'
#' @export
fit_mods <- function(data, custom_model = NULL, custom_model_args = list()){
  mods <- lapply(data, function(x){
      args <- custom_model_args
      args$df <- x
      do.call(custom_model, args = args)
    })

  datframe <- data.frame(do.call("rbind", mods))
  names(datframe) <- c("Estimate", "Std.Error", "t.value", "p.value",
                       "lower_ci", "upper_ci")
  return(datframe)
}

#' Format output for custom model to use in eesim
#'
#' Formats the output within a modeling function to be used in a call to
#' \code{\link{eesim}} when the model is fit using \code{glm} or something
#' similar.
#'
#' @param mod A model object from lm, glm, etc.
#'
#' @return Output with the correct values and column names needed for a
#'    modeling function to pass to \code{\link{eesim}}.
#'
#' @examples
#' dat <- data.frame(x=rnorm(1000, 0, 1), outcome = rnorm(1000, 5, 1))
#' lin_mod <- lm(outcome~x, data=dat)
#' format_out(lin_mod)
#'
#' @export
format_out <- function(mod){
  out_1 <- summary(mod)$coef[2, ]
  out_2 <- stats::confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
  }

#' Simulate data, fit models, and assess models
#'
#' Generates synthetic time series datasets relevant for environmental epidemiology
#' studies and tests performance of a model on that simulated data.
#' Datasets can be generated with seasonal and long-term trends in either
#' exposure or outcome. Binary or continuous outcomes can be simulated or incorporated
#' from observed datasets. The function includes extensive options for customizing each
#' step of the simulation process; see the \code{eesim} vignette for more details and
#' examples.
#'
#' @inheritParams create_sims
#' @inheritParams sim_exposure
#' @inheritParams sim_outcome
#' @inheritParams std_exposure
#' @inheritParams fit_mods
#'
#' @return A list object with three elements:
#' \itemize{
#'   \item{\code{simulated_datasets}: }{A list of length \code{n_reps}, in which each element is
#'     a data frame with
#'     one of the simulated time series datasets, created according to the specifications
#'     set by the user.}
#'   \item{\code{indiv_performance}: }{A dataframe with one row per simulated dataset (i.e.,
#'     total number of rows equal to \code{n_reps}). Each row gives the results of fitting the
#'     specified model to one of the simulated datasets. See \code{\link{fit_mods}} for more on
#'     this output.}
#'   \item{\code{overall_performance}: }{A one-row dataframe with overall performance summaries
#'     from fitting the specified model to the synthetic datasets. See \code{\link{check_sims}}
#'     for more on this output.}
#' }
#'
#' @references
#'
#' Bateson TF, Schwartz J. 1999. Control for seasonal variation and time trend in
#'     case-crossover studies of acute effects of environmental exposures. Epidemiology
#'     10(4):539-544.
#'
#' @examples
#' # Run a simulation for a continuous exposure (mean = 100, standard
#' # deviation after long-term and seasonal trends = 10) that increases
#' # risk of a count outcome by 0.1% per unit increase, where the average
#' # daily outcome is 22 per day. The exposure outcome has a seasonal trend,
#' # with higher values in the winter, while the outcome has no seasonal
#' # or long-term trends beyond those introduced through effects from the
#' # exposure. The simulated data are fit with a model defined by the `spline_mod`
#' # function (also in the `eesim` package), with its `df_year` argument set to 7.
#'
#' sims <- eesim(n_reps = 3, n = 5 * 365, central = 100, sd = 10,
#'       exposure_type = "continuous", exposure_trend = "cos3",
#'       exposure_amp = .6, average_outcome = 22, rr = 1.001,
#'       custom_model = spline_mod, custom_model_args = list(df_year = 7))
#' names(sims)
#' sims[[2]]
#' sims[[3]]
#'
#' @export
eesim <- function(n_reps, n, rr, exposure_type, custom_model,
                  central = NULL, sd = NULL,
                  exposure_trend = "no trend", exposure_slope = NULL,
                  exposure_amp = NULL, average_outcome = NULL,
                  outcome_trend = "no trend", outcome_slope = NULL,
                  outcome_amp = NULL, start.date = "2000-01-01",
                  cust_exp_func = NULL, cust_exp_args = NULL,
                  cust_expdraw = NULL, cust_expdraw_args = NULL,
                  cust_base_func = NULL, cust_lambda_func = NULL,
                  cust_base_args = NULL, cust_lambda_args = NULL,
                  cust_outdraw = NULL, cust_outdraw_args = NULL,
                  custom_model_args = NULL){

  msg <- paste("This function may take a minute or two to run, especially if you are creating lots of",
               "replications (`n_reps`).")
  msg <- paste(strwrap(msg), collapse = "\n")
  message(msg)

  totalsims <- vector("list", 1)
  totalsims[[1]] <- create_sims(n_reps = n_reps, n = n, central = central, sd = sd,
                          exposure_type = exposure_type, exposure_trend = exposure_trend,
                          exposure_slope = exposure_slope, exposure_amp = exposure_amp,
                          average_outcome = average_outcome, outcome_trend = outcome_trend,
                          outcome_slope = outcome_slope, outcome_amp = outcome_amp,
                          rr = rr, start.date = "2000-01-01", cust_exp_func = cust_exp_func,
                          cust_exp_args = cust_exp_args, cust_expdraw=cust_expdraw,
                          cust_expdraw_args = cust_expdraw_args,
                          cust_base_func = cust_base_func,
                          cust_lambda_func = cust_lambda_func,
                          cust_base_args = cust_base_args,
                          cust_lambda_args = cust_lambda_args,
                          cust_outdraw=cust_outdraw,
                          cust_outdraw_args=cust_outdraw_args)
  totalsims[[2]] <- fit_mods(totalsims[[1]], custom_model = custom_model,
                             custom_model_args = custom_model_args)
  totalsims[[3]] <- check_sims(df = totalsims[[2]], true_rr = rr)
  names(totalsims) <- c("simulated_datasets", "indiv_performance", "overall_performance")
  return(totalsims)
}

