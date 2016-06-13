#' Fit a generalized linear model
#'
#' This function fits a GLM with splines to your data.
#'
#' @param df A data frame with exposure and outcome data
#' @param df_year A numeric value specifying the degrees of freedom per year
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in exposure
#'
#' @example
#' spline_mod(df)
#'
#' @export
#'
spline_mod <- function(df, df_year = 7){
  require(splines)

  dgrs_free <- df_year * as.numeric(diff(df[c(1, nrow(df)), "date"])) / 365.4
  df$time <- scale(df$date, center = TRUE, scale = FALSE)
  mod <- glm(y ~ x + ns(time, round(dgrs_free)),
             data = df,
             family = quasipoisson(link = "log"))

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}

#' Fit a case-crossover model
#'
#' This function fits a case-crossover model to your data.
#'
#' @inheritParams spline_mod
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in exposure
#'
#' @example
#' casecross_mod(df)
#'
#' @export
#'
casecross_mod <- function(df){
  df$stratum <- factor(format(df$date, "%Y.%m"))

  if (sum(df$x == 0 | df$x == 1) == length(df$x)){
    event.check <- as.matrix(table(df$stratum, df$x))
    informative.strata <- rownames(event.check)[apply(event.check,
                                                      1, prod) > 0]
    df <- subset(df, stratum %in% informative.strata)

    if(length(informative.strata) > 1){
      mod <- glm(y ~ x + stratum,
                 data = df,
                 family = quasipoisson(link = "log"))
    } else {
      mod <- glm(y ~ x,
                 data = df,
                 family = quasipoisson(link = "log"))
    }
  } else {
    mod <- glm(y ~ x + stratum,
               data = df,
               family = quasipoisson(link = "log"))
  }

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}

#' Fit a cross-year model
#'
#' This function fits a cross-year model to your data.
#'
#' @inheritParams spline_mod
#'
#' @return Summary of the estimated log relative risk for a 1-unit increase in exposure
#'
#' @example
#' crossyear_mod(df)
#'
#' @export
#'
crossyear_mod <- function(df){
  df$stratum <- factor(format(df$date, "%j"))
  df$year <- factor(format(df$date, "%Y"))

  if (sum(df$x == 0 | df$x == 1) == length(df$x)){
    event.check <- as.matrix(table(df$stratum, df$x))
    informative.strata <- rownames(event.check)[apply(event.check,
                                                      1, prod) > 0]
    df <- subset(df, stratum %in% informative.strata)

    if(length(informative.strata) > 1){
      mod <- glm(y ~ x + stratum,
                 data = df,
                 family = quasipoisson(link = "log"))
    } else {
      mod <- glm(y ~ x,
                 data = df,
                 family = quasipoisson(link = "log"))
    }
  } else {
    mod <- glm(y ~ x + stratum,
               data = df,
               family = quasipoisson(link = "log"))
  }

  out_1 <- summary(mod)$coef[2, ]
  out_2 <- confint.default(mod)[2, ]
  out <- c(out_1, out_2)
  return(out)
}

#' Run simulations
#'
#' This function runs many simulations for a given dataset and model.
#'
#' @param n_sims A numeric value specifying the number of repetitions of the simulation to run
#' @param model A character string specifying which model to use.  Options include "spline_mod" and "casecross_mod"
#' @param n A numeric value specifying the number of days for which to simulate data
#' @param rr A numeric value specifying the relative risk for each 1-unit increase in exposure
#' @param x_type A character string specifying the type of exposure data.  Options are "binary" and "continuous".
#' @inheritParams continuous_exposure
#' @inheritParams binary_exposure
#'
#' @return A data frame with the summaries of the estimate of the log relative risk for each simulation run
#'
#' @example
#' rep_sims(n_sims = 3, model = "spline_mod")
#'
#' @export
#'
rep_sims <- function(n_sims, model, n = 5 * 365, rr = 1.01,
                     x_type = "continuous", mu = 10, sd = 2, lambda = 100){
  library(dplyr)
  out <- replicate(n_sims, eval(call(model,
                                     sim_data(n = n, rr = rr,
                                              x_type = x_type,
                                              mu = mu, sd = sd,
                                              lambda = lambda))))
  out <- as.data.frame(t(out))
  names(out) <- c("est", "se", "t", "p", "lower_ci", "upper_ci")
  return(out)
}
